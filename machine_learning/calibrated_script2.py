import os
import argparse
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import DataLoader, random_split
from torchvision import datasets, transforms
from transformers import AutoImageProcessor, AutoModelForImageClassification, TrainingArguments, Trainer
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.isotonic import IsotonicRegression
from sklearn.linear_model import LogisticRegression
from sklearn.utils.class_weight import compute_class_weight
import numpy as np
from PIL import Image
import json
from datetime import datetime
import pickle
import random
import sys
from collections import Counter

class TeeOutput:
    """Class to redirect output to both console and file"""
    def __init__(self, file_path=None):
        self.terminal = sys.stdout
        self.log_file = open(file_path, 'w') if file_path else None
    
    def write(self, message):
        self.terminal.write(message)
        if self.log_file:
            self.log_file.write(message)
            self.log_file.flush()
    
    def flush(self):
        self.terminal.flush()
        if self.log_file:
            self.log_file.flush()
    
    def close(self):
        if self.log_file:
            self.log_file.close()

class TemperatureScaling(nn.Module):
    """
    Temperature scaling calibration method
    """
    def __init__(self):
        super().__init__()
        self.temperature = nn.Parameter(torch.ones(1) * 1.5)

    def forward(self, logits):
        return logits / self.temperature

    def temperature_scale(self, logits, labels, max_iter=50, lr=0.01):
        """
        Tune the temperature parameter using validation data
        """
        criterion = nn.CrossEntropyLoss()
        optimizer = torch.optim.LBFGS([self.temperature], lr=lr, max_iter=max_iter)
        
        def eval():
            optimizer.zero_grad()
            scaled_logits = self.forward(logits)
            loss = criterion(scaled_logits, labels)
            loss.backward()
            return loss
        
        optimizer.step(eval)
        return self.temperature.item()

class CalibratedModel(nn.Module):
    """
    Wrapper that combines the base model with calibration
    """
    def __init__(self, base_model, calibration_method='temperature'):
        super().__init__()
        self.base_model = base_model
        self.calibration_method = calibration_method
        
        if calibration_method == 'temperature':
            self.calibrator = TemperatureScaling()
        elif calibration_method == 'platt':
            self.calibrator = None  # Will be set during calibration
        elif calibration_method == 'isotonic':
            self.calibrator = None  # Will be set during calibration
    
    def forward(self, **inputs):
        outputs = self.base_model(**inputs)
        
        if hasattr(self, 'calibrator') and self.calibrator is not None:
            if self.calibration_method == 'temperature':
                outputs.logits = self.calibrator(outputs.logits)
        
        return outputs

class EnsembleModel(nn.Module):
    """
    Simple ensemble of multiple models for better calibration
    """
    def __init__(self, models):
        super().__init__()
        self.models = nn.ModuleList(models)
    
    def forward(self, **inputs):
        outputs = []
        for model in self.models:
            output = model(**inputs)
            outputs.append(F.softmax(output.logits, dim=-1))
        
        # Average predictions
        avg_probs = torch.stack(outputs).mean(dim=0)
        
        # Convert back to logits format for compatibility
        class EnsembleOutput:
            def __init__(self, logits):
                self.logits = logits
        
        return EnsembleOutput(torch.log(avg_probs + 1e-8))

class ConfidenceBasedSampler:
    """
    Sample training data based on confidence to reduce overconfident training
    """
    def __init__(self, model, processor, device, confidence_threshold=0.9):
        self.model = model
        self.processor = processor  
        self.device = device
        self.confidence_threshold = confidence_threshold
    
    def get_confident_samples(self, dataset, max_samples=None):
        """
        Get samples where model is confident but potentially wrong
        """
        self.model.eval()
        confident_indices = []
        
        with torch.no_grad():
            for idx in range(len(dataset)):
                if max_samples and len(confident_indices) >= max_samples:
                    break
                    
                sample = dataset[idx]
                inputs = {k: v.unsqueeze(0).to(self.device) for k, v in sample.items() 
                         if k != 'labels'}
                
                outputs = self.model(**inputs)
                probs = F.softmax(outputs.logits, dim=-1)
                max_prob = probs.max().item()
                
                if max_prob > self.confidence_threshold:
                    confident_indices.append(idx)
        
        return confident_indices

class WeightedFocalLoss(nn.Module):
    """
    Focal Loss with class weights to handle class imbalance and reduce overconfidence
    """
    def __init__(self, class_weights=None, alpha=1, gamma=2, reduction='mean'):
        super().__init__()
        self.alpha = alpha
        self.gamma = gamma
        self.reduction = reduction
        self.class_weights = class_weights
    
    def forward(self, inputs, targets):
        ce_loss = F.cross_entropy(inputs, targets, weight=self.class_weights, reduction='none')
        pt = torch.exp(-ce_loss)
        focal_loss = self.alpha * (1-pt)**self.gamma * ce_loss
        
        if self.reduction == 'mean':
            return focal_loss.mean()
        elif self.reduction == 'sum':
            return focal_loss.sum()
        else:
            return focal_loss

class WeightedLabelSmoothingLoss(nn.Module):
    """
    Label smoothing with class weights to reduce overconfidence
    """
    def __init__(self, class_weights=None, smoothing=0.1):
        super().__init__()
        self.smoothing = smoothing
        self.class_weights = class_weights
    
    def forward(self, inputs, targets):
        num_classes = inputs.size(-1)
        log_probs = F.log_softmax(inputs, dim=-1)
        
        # Create smoothed targets
        targets_one_hot = torch.zeros_like(inputs).scatter(1, targets.unsqueeze(1), 1)
        targets_smooth = (1 - self.smoothing) * targets_one_hot + \
                        self.smoothing / num_classes
        
        # Apply class weights if provided
        if self.class_weights is not None:
            # Apply weights to the smooth targets
            weight_matrix = self.class_weights.unsqueeze(0).expand_as(targets_smooth)
            targets_smooth = targets_smooth * weight_matrix
            # Normalize to maintain probability distribution
            targets_smooth = targets_smooth / targets_smooth.sum(dim=-1, keepdim=True)
        
        loss = -(targets_smooth * log_probs).sum(dim=-1)
        
        # Apply class weights to final loss if provided
        if self.class_weights is not None:
            weights = self.class_weights[targets]
            loss = loss * weights
        
        return loss.mean()

class ImageDataset(torch.utils.data.Dataset):
    def __init__(self, dataset, processor, transform=None):
        self.dataset = dataset
        self.processor = processor
        self.transform = transform

    def __len__(self):
        return len(self.dataset)
    
    def __getitem__(self, idx):
        image, label = self.dataset[idx]

        # Ensure image is RGB (Hugging Face processor expects RGB)
        if image.mode != 'RGB':
            image = image.convert('RGB')

        # Apply data augmentation transforms (still a PIL image)
        if self.transform:
            image = self.transform(image)

        # Processor does resizing, normalization, ToTensor, etc.
        inputs = self.processor(image, return_tensors="pt")

        # Remove batch dimension from processor output
        inputs = {k: v.squeeze(0) for k, v in inputs.items()}

        # Add label
        inputs['labels'] = torch.tensor(label, dtype=torch.long)
        return inputs

def collate_fn(batch):
    return {
        'pixel_values': torch.stack([x['pixel_values'] for x in batch]),
        'labels': torch.stack([x['labels'] for x in batch])
    }

def compute_metrics(eval_pred):
    predictions, labels = eval_pred
    predictions = np.argmax(predictions, axis=1)
    
    accuracy = accuracy_score(labels, predictions)
    
    return {
        'accuracy': accuracy,
        'eval_loss': 0.0  # Will be computed by trainer
    }

def calculate_class_weights(dataset, class_names, weight_method='balanced'):
    """
    Calculate class weights based on the dataset distribution
    """
    # Get all labels from the dataset
    if hasattr(dataset, 'dataset'):
        # Handle wrapped datasets (like from random_split)
        labels = []
        for i in range(len(dataset)):
            _, label = dataset.dataset[dataset.indices[i]] if hasattr(dataset, 'indices') else dataset.dataset[i]
            labels.append(label)
    else:
        # Handle regular ImageFolder datasets
        labels = [label for _, label in dataset]
    
    labels = np.array(labels)
    
    if weight_method == 'balanced':
        # Use sklearn's compute_class_weight with 'balanced' strategy
        class_weights = compute_class_weight('balanced', classes=np.arange(len(class_names)), y=labels)
    elif weight_method == 'inverse':
        # Simple inverse frequency weighting
        label_counts = Counter(labels)
        total_samples = len(labels)
        class_weights = []
        for i in range(len(class_names)):
            count = label_counts.get(i, 1)  # Avoid division by zero
            class_weights.append(total_samples / (len(class_names) * count))
        class_weights = np.array(class_weights)
    elif weight_method == 'sqrt_inverse':
        # Square root of inverse frequency (less aggressive)
        label_counts = Counter(labels)
        total_samples = len(labels)
        class_weights = []
        for i in range(len(class_names)):
            count = label_counts.get(i, 1)
            weight = total_samples / (len(class_names) * count)
            class_weights.append(np.sqrt(weight))
        class_weights = np.array(class_weights)
    else:
        # No weighting (uniform weights)
        class_weights = np.ones(len(class_names))
    
    # Print class distribution and weights
    label_counts = Counter(labels)
    print(f"\nClass distribution and weights ({weight_method}):")
    print("-" * 60)
    print(f"{'Class':<15} {'Count':<8} {'Percentage':<12} {'Weight':<10}")
    print("-" * 60)
    
    for i, class_name in enumerate(class_names):
        count = label_counts.get(i, 0)
        percentage = (count / len(labels)) * 100
        weight = class_weights[i]
        print(f"{class_name:<15} {count:<8} {percentage:<12.2f} {weight:<10.4f}")
    print("-" * 60)
    
    return torch.tensor(class_weights, dtype=torch.float32)

def calibrate_model(model, val_loader, device, method='temperature'):
    """
    Calibrate model using validation data
    """
    model.eval()
    all_logits = []
    all_labels = []
    
    # Collect predictions on validation set
    with torch.no_grad():
        for batch in val_loader:
            inputs = {k: v.to(device) for k, v in batch.items()}
            outputs = model(**inputs)
            all_logits.append(outputs.logits.cpu())
            all_labels.append(inputs['labels'].cpu())
    
    logits = torch.cat(all_logits, dim=0)
    labels = torch.cat(all_labels, dim=0)
    
    if method == 'temperature':
        temp_scaler = TemperatureScaling()
        optimal_temp = temp_scaler.temperature_scale(logits, labels)
        print(f"Optimal temperature: {optimal_temp:.3f}")
        # Move calibrator to the same device as the model
        temp_scaler = temp_scaler.to(device)
        return temp_scaler
    
    elif method == 'platt':
        # Platt scaling (logistic regression on max logit)
        max_logits = logits.max(dim=1)[0].numpy().reshape(-1, 1)
        platt_scaler = LogisticRegression()
        platt_scaler.fit(max_logits, labels.numpy())
        return platt_scaler
    
    elif method == 'isotonic':
        # Isotonic regression on max probability
        probs = F.softmax(logits, dim=-1)
        max_probs = probs.max(dim=1)[0].numpy()
        
        # Create binary problem: correct vs incorrect
        correct = (logits.argmax(dim=1) == labels).numpy().astype(float)
        
        isotonic_scaler = IsotonicRegression(out_of_bounds='clip')
        isotonic_scaler.fit(max_probs, correct)
        return isotonic_scaler

def evaluate_model_with_calibration(model, processor, test_loader, class_names, device, calibrator=None, calibration_method='temperature'):
    """
    Enhanced evaluation with calibration metrics
    """
    model.eval()
    all_preds = []
    all_labels = []
    all_probs = []
    all_max_probs = []
    
    with torch.no_grad():
        for batch in test_loader:
            inputs = {k: v.to(device) for k, v in batch.items()}
            outputs = model(**inputs)
            
            logits = outputs.logits
            
            # Apply calibration if available
            if calibrator is not None:
                if calibration_method == 'temperature':
                    logits = calibrator(logits)
                elif calibration_method == 'platt':
                    max_logits = logits.max(dim=1)[0].cpu().numpy().reshape(-1, 1)
                    calibrated_probs = calibrator.predict_proba(max_logits)[:, 1]
                    # This is simplified - you'd need more complex logic for multi-class
                elif calibration_method == 'isotonic':
                    probs = F.softmax(logits, dim=-1)
                    max_probs = probs.max(dim=1)[0].cpu().numpy()
                    calibrated_confidence = calibrator.predict(max_probs)
            
            predictions = F.softmax(logits, dim=-1)
            predicted_class_ids = predictions.argmax(dim=-1)
            max_probs = predictions.max(dim=-1)[0]
            
            all_preds.extend(predicted_class_ids.cpu().numpy())
            all_labels.extend(inputs['labels'].cpu().numpy())
            all_probs.extend(predictions.cpu().numpy())
            all_max_probs.extend(max_probs.cpu().numpy())
    
    # Calculate metrics
    accuracy = accuracy_score(all_labels, all_preds)
    report = classification_report(all_labels, all_preds, 
                                 target_names=class_names, 
                                 output_dict=True)
    cm = confusion_matrix(all_labels, all_preds)
    
    # Calculate calibration metrics
    all_probs = np.array(all_probs)
    all_max_probs = np.array(all_max_probs)
    correct = (np.array(all_preds) == np.array(all_labels))
    
    # Expected Calibration Error (ECE)
    ece = calculate_ece(all_max_probs, correct)
    
    # Reliability diagram data
    bin_boundaries = np.linspace(0, 1, 11)
    bin_lowers = bin_boundaries[:-1]
    bin_uppers = bin_boundaries[1:]
    
    accuracies = []
    confidences = []
    
    for bin_lower, bin_upper in zip(bin_lowers, bin_uppers):
        in_bin = (all_max_probs > bin_lower) & (all_max_probs <= bin_upper)
        prop_in_bin = in_bin.mean()
        
        if prop_in_bin > 0:
            accuracy_in_bin = correct[in_bin].mean()
            avg_confidence_in_bin = all_max_probs[in_bin].mean()
            accuracies.append(accuracy_in_bin)
            confidences.append(avg_confidence_in_bin)
        else:
            accuracies.append(0)
            confidences.append(0)
    
    calibration_data = {
        'ece': ece,
        'bin_accuracies': accuracies,
        'bin_confidences': confidences,
        'bin_boundaries': bin_boundaries.tolist()
    }
    
    return accuracy, report, cm, calibration_data

def calculate_ece(confidences, correct, n_bins=10):
    """
    Calculate Expected Calibration Error
    """
    bin_boundaries = np.linspace(0, 1, n_bins + 1)
    bin_lowers = bin_boundaries[:-1]
    bin_uppers = bin_boundaries[1:]
    
    ece = 0
    total_samples = len(confidences)
    
    for bin_lower, bin_upper in zip(bin_lowers, bin_uppers):
        in_bin = (confidences > bin_lower) & (confidences <= bin_upper)
        prop_in_bin = in_bin.mean()
        
        if prop_in_bin > 0:
            accuracy_in_bin = correct[in_bin].mean()
            avg_confidence_in_bin = confidences[in_bin].mean()
            ece += np.abs(avg_confidence_in_bin - accuracy_in_bin) * prop_in_bin
    
    return ece

class CalibratedTrainer(Trainer):
    """
    Custom trainer with different loss functions and class weights
    """
    def __init__(self, loss_type='cross_entropy', class_weights=None, label_smoothing=0.1, 
                 focal_alpha=1, focal_gamma=2, **kwargs):
        super().__init__(**kwargs)
        self.loss_type = loss_type
        self.class_weights = class_weights
        
        # Move class weights to appropriate device if provided
        if self.class_weights is not None and torch.cuda.is_available():
            self.class_weights = self.class_weights.cuda()
        
        if loss_type == 'label_smoothing':
            self.loss_fn = WeightedLabelSmoothingLoss(class_weights=self.class_weights, 
                                                     smoothing=label_smoothing)
        elif loss_type == 'focal':
            self.loss_fn = WeightedFocalLoss(class_weights=self.class_weights, 
                                           alpha=focal_alpha, gamma=focal_gamma)
        else:
            self.loss_fn = nn.CrossEntropyLoss(weight=self.class_weights)
    
    def compute_loss(self, model, inputs, return_outputs=False):
        labels = inputs.pop("labels")
        outputs = model(**inputs)
        
        if self.loss_type != 'cross_entropy':
            loss = self.loss_fn(outputs.logits, labels)
        else:
            if self.class_weights is not None:
                loss = self.loss_fn(outputs.logits, labels)
            else:
                loss = outputs.loss
        
        inputs["labels"] = labels  # Put back for other uses
        return (loss, outputs) if return_outputs else loss

def set_seed(seed):
    """Set all random seeds for reproducibility"""
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    # Make CUDNN deterministic (optional, may affect performance)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

def main():
    parser = argparse.ArgumentParser(description='Train calibrated image classifier with class weights')
    parser.add_argument('--data_dir', type=str, required=True, 
                       help='Directory containing image data in class subfolders')
    parser.add_argument('--model_name', type=str, default='microsoft/resnet-50',
                       help='Pretrained model name from Hugging Face')
    parser.add_argument('--output_dir', type=str, default='./results',
                       help='Directory to save model and results')
    parser.add_argument('--epochs', type=int, default=10,
                       help='Number of training epochs')
    parser.add_argument('--batch_size', type=int, default=32,
                       help='Batch size for training')
    parser.add_argument('--learning_rate', type=float, default=2e-5,
                       help='Learning rate')
    parser.add_argument('--test_split', type=float, default=0.15,
                       help='Fraction of data to use for testing')
    parser.add_argument('--val_split', type=float, default=0.15,
                       help='Fraction of data to use for validation')
    
    # Calibration arguments
    parser.add_argument('--calibration_method', type=str, default='temperature',
                       choices=['temperature', 'platt', 'isotonic', 'none'],
                       help='Calibration method to use')
    parser.add_argument('--loss_type', type=str, default='cross_entropy',
                       choices=['cross_entropy', 'label_smoothing', 'focal'],
                       help='Loss function to use during training')
    parser.add_argument('--label_smoothing', type=float, default=0.1,
                       help='Label smoothing parameter')
    parser.add_argument('--focal_alpha', type=float, default=1.0,
                       help='Focal loss alpha parameter')
    parser.add_argument('--focal_gamma', type=float, default=2.0,
                       help='Focal loss gamma parameter')
    parser.add_argument('--ensemble_size', type=int, default=1,
                       help='Number of models in ensemble (1 = no ensemble)')
    
    # Class weights arguments
    parser.add_argument('--use_class_weights', action='store_true',
                       help='Use class weights to handle imbalanced datasets')
    parser.add_argument('--class_weight_method', type=str, default='balanced',
                       choices=['balanced', 'inverse', 'sqrt_inverse', 'none'],
                       help='Method to calculate class weights')
    
    # Base seed argument
    parser.add_argument('--base_seed', type=int, default=42,
                       help='Base seed for reproducibility (ensemble models will use base_seed + model_index)')
    
    # Output file argument
    parser.add_argument('--output_file', type=str, default=None,
                       help='File path to save console output (optional)')
    
    args = parser.parse_args()
    
    # Setup output redirection
    tee_output = None
    if args.output_file:
        # Create output directory if it doesn't exist (only if there's a directory path)
        output_dir = os.path.dirname(args.output_file)
        if output_dir:  # Only create directory if there's actually a directory path
            os.makedirs(output_dir, exist_ok=True)
        tee_output = TeeOutput(args.output_file)
        sys.stdout = tee_output
        print(f"Output will be saved to: {args.output_file}")
    
    try:
        # Setup device
        device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        print(f"Using device: {device}")
        print(f"Training started at: {datetime.now().isoformat()}")
        print(f"Arguments: {vars(args)}")
        
        # Load processor and create transforms
        processor = AutoImageProcessor.from_pretrained(args.model_name)
        
        # Enhanced augmentation for better generalization
        transform = transforms.Compose([
            transforms.Resize((256, 256)),
            transforms.RandomResizedCrop(224, scale=(0.7, 1.0)),
            transforms.RandomHorizontalFlip(p=0.5),
            transforms.RandomRotation(degrees=20),
            transforms.ColorJitter(brightness=0.3, contrast=0.3, saturation=0.3, hue=0.1),
            transforms.RandomApply([transforms.GaussianBlur(3, sigma=(0.1, 2.0))], p=0.1),
        ])
        
        # Load datasets (same logic as original)
        train_dir = os.path.join(args.data_dir, 'train')
        val_dir = os.path.join(args.data_dir, 'val') 
        test_dir = os.path.join(args.data_dir, 'test')
        
        if all(os.path.exists(d) for d in [train_dir, val_dir, test_dir]):
            print("Using pre-split datasets...")
            train_dataset = datasets.ImageFolder(train_dir)
            val_dataset = datasets.ImageFolder(val_dir)
            test_dataset = datasets.ImageFolder(test_dir)
            class_names = train_dataset.classes
            num_classes = len(class_names)
            
            print(f"Pre-split datasets loaded:")
            print(f"  Train: {len(train_dataset)} images")
            print(f"  Val: {len(val_dataset)} images")  
            print(f"  Test: {len(test_dataset)} images")
            print(f"  Classes ({num_classes}): {class_names}")
        else:
            print("Pre-split directories not found, creating splits automatically...")
            full_dataset = datasets.ImageFolder(args.data_dir, transform=transform)
            class_names = full_dataset.classes
            num_classes = len(class_names)
            
            print(f"Found {len(full_dataset)} images in {num_classes} classes: {class_names}")
            
            # Split dataset
            total_size = len(full_dataset)
            test_size = int(args.test_split * total_size)
            val_size = int(args.val_split * total_size)
            train_size = total_size - test_size - val_size
            
            train_dataset, val_dataset, test_dataset = random_split(
                full_dataset, [train_size, val_size, test_size],
                generator=torch.Generator().manual_seed(args.base_seed)
            )
            
            print(f"Dataset splits - Train: {len(train_dataset)}, Val: {len(val_dataset)}, Test: {len(test_dataset)}")
        
        # Calculate class weights if requested
        class_weights = None
        if args.use_class_weights and args.class_weight_method != 'none':
            print("Calculating class weights...")
            class_weights = calculate_class_weights(train_dataset, class_names, args.class_weight_method)
        elif args.use_class_weights:
            print("Using uniform class weights (no weighting)")
            class_weights = torch.ones(num_classes, dtype=torch.float32)
        else:
            print("Not using class weights")
        
        # Wrap datasets
        train_dataset = ImageDataset(train_dataset, processor, transform=transform)
        val_dataset = ImageDataset(val_dataset, processor)
        test_dataset = ImageDataset(test_dataset, processor)
       
        # Create label mappings for proper class names
        id2label = {i: class_name for i, class_name in enumerate(class_names)}
        label2id = {class_name: i for i, class_name in enumerate(class_names)}

        print(f"\nLabel mappings:")
        print(f"  id2label: {id2label}")

        # Train ensemble or single model
        models = []
        for i in range(args.ensemble_size):
            print(f"\nTraining model {i+1}/{args.ensemble_size}")
            
            # Set unique seed for each ensemble member
            model_seed = args.base_seed + i
            set_seed(model_seed)
            print(f"Using seed {model_seed} for model {i+1}")
            
            # Load model with proper class labels
            model = AutoModelForImageClassification.from_pretrained(
                args.model_name,
                num_labels=num_classes,
                id2label=id2label,
                label2id=label2id,
                ignore_mismatched_sizes=True
            )
            
            # Create unique training arguments for each model with different seed
            training_args = TrainingArguments(
                output_dir=f"{args.output_dir}/model_{i}",
                num_train_epochs=args.epochs,
                per_device_train_batch_size=args.batch_size,
                per_device_eval_batch_size=args.batch_size,
                learning_rate=args.learning_rate,
                warmup_steps=100,
                logging_dir=f'{args.output_dir}/model_{i}/logs',
                logging_steps=50,
                evaluation_strategy="epoch",
                save_strategy="epoch",
                load_best_model_at_end=True,
                metric_for_best_model="accuracy",
                greater_is_better=True,
                save_total_limit=5,
                seed=model_seed,
                fp16=torch.cuda.is_available(),
                dataloader_num_workers=0,
            )
            
            # Create trainer with calibrated loss and class weights
            trainer = CalibratedTrainer(
                model=model,
                args=training_args,
                train_dataset=train_dataset,
                eval_dataset=val_dataset,
                data_collator=collate_fn,
                compute_metrics=compute_metrics,
                loss_type=args.loss_type,
                class_weights=class_weights,
                label_smoothing=args.label_smoothing,
                focal_alpha=args.focal_alpha,
                focal_gamma=args.focal_gamma,
            )
            
            # Train model
            trainer.train()
            models.append(model)
        
        # Create final model (ensemble or single)
        if args.ensemble_size > 1:
            final_model = EnsembleModel(models)
            print(f"Created ensemble of {args.ensemble_size} models")
        else:
            final_model = models[0]
        
        # Save processor
        print("Saving processor...")
        processor.save_pretrained(args.output_dir)
        
        # Calibrate model if requested
        calibrator = None
        if args.calibration_method != 'none':
            print(f"Calibrating model using {args.calibration_method}...")
            val_loader = DataLoader(val_dataset, batch_size=args.batch_size, 
                                   shuffle=False, collate_fn=collate_fn)
            calibrator = calibrate_model(final_model, val_loader, device, args.calibration_method)
            
            # Save calibrator
            calibrator_path = os.path.join(args.output_dir, f'calibrator_{args.calibration_method}.pkl')
            with open(calibrator_path, 'wb') as f:
                pickle.dump(calibrator, f)
            print(f"Calibrator saved to {calibrator_path}")
            
            # Move final model to device for evaluation
            final_model = final_model.to(device)
        
        # Save final model
        if args.ensemble_size == 1:
            final_model.save_pretrained(args.output_dir)
        else:
            # Save ensemble components
            for i, model in enumerate(models):
                model.save_pretrained(os.path.join(args.output_dir, f'model_{i}'))
        
        # Save class weights if used
        if class_weights is not None:
            class_weights_path = os.path.join(args.output_dir, 'class_weights.pkl')
            with open(class_weights_path, 'wb') as f:
                pickle.dump({
                    'class_weights': class_weights.cpu().numpy(),
                    'class_names': class_names,
                    'weight_method': args.class_weight_method
                }, f)
            print(f"Class weights saved to {class_weights_path}")
        
        # Evaluate on test set
        print("Evaluating on test set...")
        test_loader = DataLoader(test_dataset, batch_size=args.batch_size, 
                               shuffle=False, collate_fn=collate_fn)
        
        accuracy, report, cm, calibration_data = evaluate_model_with_calibration(
            final_model, processor, test_loader, class_names, device, 
            calibrator, args.calibration_method
        )
        
        # Print results
        print(f"\nTest Accuracy: {accuracy:.4f}")
        print(f"Expected Calibration Error (ECE): {calibration_data['ece']:.4f}")
        
        print("\nPer-class metrics:")
        for class_name in class_names:
            metrics = report[class_name]
            print(f"{class_name}: Precision={metrics['precision']:.3f}, "
                  f"Recall={metrics['recall']:.3f}, F1={metrics['f1-score']:.3f}")
        
        print(f"\nConfusion Matrix:")
        print("Predicted ->")
        print("Actual ↓  ", end="")
        for name in class_names:
            print(f"{name[:8]:>8}", end="")
        print()
        
        for i, name in enumerate(class_names):
            print(f"{name[:8]:8}", end="")
            for j in range(len(class_names)):
                print(f"{cm[i][j]:8d}", end="")
            print()
        
        # Helper function to convert numpy types to native Python types for JSON serialization
        def convert_numpy_types(obj):
            if isinstance(obj, dict):
                return {key: convert_numpy_types(value) for key, value in obj.items()}
            elif isinstance(obj, list):
                return [convert_numpy_types(item) for item in obj]
            elif isinstance(obj, np.integer):
                return int(obj)
            elif isinstance(obj, np.floating):
                return float(obj)
            elif isinstance(obj, np.ndarray):
                return obj.tolist()
            else:
                return obj
        
        # Save results
        results = {
            'model_name': args.model_name,
            'test_accuracy': float(accuracy),
            'expected_calibration_error': float(calibration_data['ece']),
            'class_names': class_names,
            'classification_report': convert_numpy_types(report),
            'confusion_matrix': cm.tolist(),
            'calibration_data': convert_numpy_types(calibration_data),
            'training_args': vars(args),
            'class_weights_used': class_weights is not None,
            'class_weight_method': args.class_weight_method if class_weights is not None else None,
            'class_weights': class_weights.cpu().numpy().tolist() if class_weights is not None else None,
            'timestamp': datetime.now().isoformat()
        }
        
        results_path = os.path.join(args.output_dir, 'evaluation_results.json')
        with open(results_path, 'w') as f:
            json.dump(results, f, indent=2)
        
        print(f"\nResults saved to {results_path}")
        print(f"Model saved to {args.output_dir}")
        print(f"Training completed at: {datetime.now().isoformat()}")
        
        # Print final summary
        print("\n" + "="*60)
        print("TRAINING SUMMARY")
        print("="*60)
        print(f"Model: {args.model_name}")
        print(f"Classes: {num_classes} ({', '.join(class_names)})")
        print(f"Loss function: {args.loss_type}")
        print(f"Class weights: {'Yes' if class_weights is not None else 'No'}")
        if class_weights is not None:
            print(f"Weight method: {args.class_weight_method}")
        print(f"Calibration: {args.calibration_method}")
        print(f"Ensemble size: {args.ensemble_size}")
        print(f"Final accuracy: {accuracy:.4f}")
        print(f"ECE: {calibration_data['ece']:.4f}")
        print("="*60)
        
    except Exception as e:
        print(f"Error during training: {str(e)}")
        import traceback
        traceback.print_exc()
        raise
    finally:
        # Clean up output redirection
        if tee_output:
            sys.stdout = tee_output.terminal
            tee_output.close()
            print(f"Output saved to: {args.output_file}")

if __name__ == "__main__":
    main()
