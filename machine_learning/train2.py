import os
import argparse
import torch
import torch.nn as nn
from torch.utils.data import DataLoader, random_split
from torchvision import datasets, transforms
from transformers import AutoImageProcessor, AutoModelForImageClassification, TrainingArguments, Trainer
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.utils.class_weight import compute_class_weight
import numpy as np
from PIL import Image
import json
from datetime import datetime

class WeightedTrainer(Trainer):
    def __init__(self, class_weights=None, **kwargs):
        super().__init__(**kwargs)
        self.class_weights = class_weights
    
    def compute_loss(self, model, inputs, return_outputs=False):
        labels = inputs.get("labels")
        outputs = model(**inputs)
        logits = outputs.get("logits")
        
        if self.class_weights is not None:
            # Use weighted cross entropy loss
            loss_fn = nn.CrossEntropyLoss(weight=self.class_weights)
            loss = loss_fn(logits, labels)
        else:
            loss = outputs.loss
            
        return (loss, outputs) if return_outputs else loss

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

def calculate_class_weights(dataset, num_classes):
    """Calculate class weights for imbalanced dataset"""
    # Get all labels from the dataset
    labels = []
    for i in range(len(dataset)):
        _, label = dataset[i]
        labels.append(label)
    
    # Calculate class weights using sklearn
    class_weights = compute_class_weight(
        'balanced',
        classes=np.arange(num_classes),
        y=labels
    )
    
    return torch.FloatTensor(class_weights)

def print_class_distribution(dataset, class_names):
    """Print the distribution of classes in the dataset"""
    class_counts = {}
    for i in range(len(dataset)):
        _, label = dataset[i]
        class_name = class_names[label]
        class_counts[class_name] = class_counts.get(class_name, 0) + 1
    
    print("Class distribution:")
    for class_name, count in class_counts.items():
        print(f"  {class_name}: {count} images")
    
    return class_counts

def evaluate_model(model, processor, test_loader, class_names, device):
    model.eval()
    all_preds = []
    all_labels = []
    
    with torch.no_grad():
        for batch in test_loader:
            inputs = {k: v.to(device) for k, v in batch.items()}
            outputs = model(**inputs)
            
            predictions = torch.nn.functional.softmax(outputs.logits, dim=-1)
            predicted_class_ids = predictions.argmax(dim=-1)
            
            all_preds.extend(predicted_class_ids.cpu().numpy())
            all_labels.extend(inputs['labels'].cpu().numpy())
    
    # Calculate metrics
    accuracy = accuracy_score(all_labels, all_preds)
    report = classification_report(all_labels, all_preds, 
                                 target_names=class_names, 
                                 output_dict=True)
    cm = confusion_matrix(all_labels, all_preds)
    
    return accuracy, report, cm

def main():
    parser = argparse.ArgumentParser(description='Train image classifier')
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
    
    args = parser.parse_args()
    
    # Setup device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")
    
    # Load processor and create transforms
    processor = AutoImageProcessor.from_pretrained(args.model_name)
    
    # Create transforms for data augmentation
    transform = transforms.Compose([
        transforms.Resize((256, 256)),
        transforms.RandomResizedCrop(224, scale=(0.8, 1.0)),
        transforms.RandomHorizontalFlip(p=0.5),
        transforms.RandomRotation(degrees=15),
        transforms.ColorJitter(brightness=0.2, contrast=0.2, saturation=0.2, hue=0.1),
    ])
    
    # Load pre-split datasets
    train_dir = os.path.join(args.data_dir, 'train')
    val_dir = os.path.join(args.data_dir, 'val') 
    test_dir = os.path.join(args.data_dir, 'test')
    
    # Check if pre-split directories exist
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
        
        # Print class distribution for training set
        train_class_counts = print_class_distribution(train_dataset, class_names)
        
    else:
        print("Pre-split directories not found, creating splits automatically...")
        # Original splitting code as fallback
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
            generator=torch.Generator().manual_seed(42)
        )
        
        print(f"Dataset splits - Train: {len(train_dataset)}, Val: {len(val_dataset)}, Test: {len(test_dataset)}")
    
    # Calculate class weights for imbalanced dataset
    print("\nCalculating class weights for imbalanced dataset...")
    class_weights = calculate_class_weights(train_dataset, num_classes)
    class_weights = class_weights.to(device)
    
    print("Class weights:")
    for i, (class_name, weight) in enumerate(zip(class_names, class_weights)):
        print(f"  {class_name}: {weight:.4f}")
    
    # Wrap datasets
    train_dataset = ImageDataset(train_dataset, processor, transform=transform)
    val_dataset = ImageDataset(val_dataset, processor)
    test_dataset = ImageDataset(test_dataset, processor)
   
    # Create label mappings for proper class names
    id2label = {i: class_name for i, class_name in enumerate(class_names)}
    label2id = {class_name: i for i, class_name in enumerate(class_names)}

    print(f"\nLabel mappings:")
    print(f"  id2label: {id2label}")

    # Load model with proper class labels
    model = AutoModelForImageClassification.from_pretrained(
        args.model_name,
        num_labels=num_classes,
        id2label=id2label,
        label2id=label2id,
        ignore_mismatched_sizes=True
    )

    # Training arguments
    training_args = TrainingArguments(
        output_dir=args.output_dir,
        num_train_epochs=args.epochs,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=args.batch_size,
        learning_rate=args.learning_rate,
        warmup_steps=100,
        logging_dir=f'{args.output_dir}/logs',
        logging_steps=50,
        evaluation_strategy="epoch",
        save_strategy="epoch",
        load_best_model_at_end=True,
        metric_for_best_model="accuracy",
        greater_is_better=True,
        save_total_limit=5,
        seed=42,
        fp16=torch.cuda.is_available(),  # Use mixed precision if GPU available
    )
    
    # Create trainer with class weights
    trainer = WeightedTrainer(
        class_weights=class_weights,
        model=model,
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=val_dataset,
        data_collator=collate_fn,
        compute_metrics=compute_metrics,
    )

    # IMPORTANT: Save the processor along with the model
    print("Saving processor...")
    processor.save_pretrained(args.output_dir)

    # Train model
    print("Starting training...")
    trainer.train()
  
    # IMPORTANT: Save the final model to the main output directory
    print("Saving final model and processor...")
    trainer.save_model()  # This should save to args.output_dir
    # If the above doesn't work, use this alternative:
    # trainer.model.save_pretrained(args.output_dir)

    # Evaluate on test set
    print("Evaluating on test set...")
    test_loader = DataLoader(test_dataset, batch_size=args.batch_size, 
                           shuffle=False, collate_fn=collate_fn)
    
    accuracy, report, cm = evaluate_model(model, processor, test_loader, class_names, device)
    
    # Print results
    print(f"\nTest Accuracy: {accuracy:.4f}")
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
    
    # Save results
    results = {
        'model_name': args.model_name,
        'test_accuracy': accuracy,
        'class_names': class_names,
        'classification_report': report,
        'confusion_matrix': cm.tolist(),
        'training_args': vars(args),
        'class_weights': class_weights.cpu().tolist(),
        'timestamp': datetime.now().isoformat()
    }
    
    results_path = os.path.join(args.output_dir, 'evaluation_results.json')
    with open(results_path, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"\nResults saved to {results_path}")
    print(f"Model saved to {args.output_dir}")

if __name__ == "__main__":
    main()
