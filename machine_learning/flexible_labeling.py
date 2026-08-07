import os
import argparse
import torch
from torch.utils.data import DataLoader
from transformers import AutoImageProcessor, AutoModelForImageClassification
from PIL import Image
import json
import shutil
from tqdm import tqdm
import numpy as np
from pathlib import Path

# [Previous classes remain the same - UnlabeledImageDataset, collate_fn, EnsemblePredictor]
class UnlabeledImageDataset(torch.utils.data.Dataset):
    def __init__(self, image_paths, processor):
        self.image_paths = image_paths
        self.processor = processor
    
    def __len__(self):
        return len(self.image_paths)
    
    def __getitem__(self, idx):
        image_path = self.image_paths[idx]
        image = Image.open(image_path).convert('RGB')
        
        # Process image
        inputs = self.processor(image, return_tensors="pt")
        inputs = {k: v.squeeze() for k, v in inputs.items()}
        
        return inputs, image_path

def collate_fn(batch):
    inputs_list = [item[0] for item in batch]
    paths = [item[1] for item in batch]
    
    return {
        'pixel_values': torch.stack([x['pixel_values'] for x in inputs_list]),
        'paths': paths
    }

class EnsemblePredictor:
    def __init__(self, model_base_path, device='cpu'):
        """
        Load ensemble models from the base path
        """
        self.device = device
        self.models = []
        self.processor = None
        
        # Check if this is an ensemble directory or single model
        model_path = Path(model_base_path)
        
        # Look for individual model directories (model_0, model_1, etc.)
        ensemble_dirs = sorted([d for d in model_path.iterdir() 
                               if d.is_dir() and d.name.startswith('model_')])
        
        if ensemble_dirs:
            print(f"Found ensemble with {len(ensemble_dirs)} models:")
            for model_dir in ensemble_dirs:
                print(f"  Loading {model_dir.name}...")
                try:
                    model = AutoModelForImageClassification.from_pretrained(str(model_dir))
                    model.to(device)
                    model.eval()
                    self.models.append(model)
                    
                    # Load processor from first model (they should all be the same)
                    if self.processor is None:
                        self.processor = AutoImageProcessor.from_pretrained(str(model_dir))
                        
                except Exception as e:
                    print(f"    Error loading {model_dir}: {e}")
                    continue
            
            if not self.models:
                raise ValueError(f"No valid ensemble models found in {model_base_path}")
                
        else:
            # Single model
            print("Loading single model...")
            model = AutoModelForImageClassification.from_pretrained(str(model_path))
            model.to(device)
            model.eval()
            self.models.append(model)
            self.processor = AutoImageProcessor.from_pretrained(str(model_path))
        
        print(f"Loaded {len(self.models)} model(s) successfully")
        
        # Get class names from the first model
        first_model = self.models[0]
        if hasattr(first_model.config, 'id2label') and first_model.config.id2label:
            self.class_names = list(first_model.config.id2label.values())
        else:
            self.class_names = [f"Class_{i}" for i in range(first_model.config.num_labels)]
    
    def predict_batch(self, batch):
        """
        Get ensemble predictions for a batch of images
        """
        inputs = {k: v.to(self.device) if k != 'paths' else v for k, v in batch.items()}
        paths = inputs.pop('paths')
        
        all_predictions = []
        
        # Get predictions from each model
        with torch.no_grad():
            for model in self.models:
                outputs = model(**inputs)
                probabilities = torch.nn.functional.softmax(outputs.logits, dim=-1)
                all_predictions.append(probabilities.cpu())
        
        # Average predictions across models
        if len(all_predictions) > 1:
            ensemble_probs = torch.stack(all_predictions).mean(dim=0)
        else:
            ensemble_probs = all_predictions[0]
        
        # Get final predictions
        max_probs, predicted_classes = torch.max(ensemble_probs, dim=1)
        
        return ensemble_probs, predicted_classes, max_probs, paths, all_predictions

def get_image_paths(directory):
    """Get all image file paths from directory"""
    image_extensions = {'.jpg', '.jpeg', '.png', '.bmp', '.tiff', '.webp', '.JPG', '.JPEG', '.PNG'}
    image_paths = []
    
    for root, dirs, files in os.walk(directory):
        for file in files:
            if any(file.lower().endswith(ext.lower()) for ext in image_extensions):
                image_paths.append(os.path.join(root, file))
    
    return image_paths

def generate_all_predictions(ensemble_predictor, image_paths, output_dir, batch_size=32, save_individual_predictions=False):
    """
    Generate predictions for all images and save to JSON - NO ORGANIZING YET
    """
    # Create output directory
    os.makedirs(output_dir, exist_ok=True)
    
    # Create dataset and dataloader
    dataset = UnlabeledImageDataset(image_paths, ensemble_predictor.processor)
    dataloader = DataLoader(dataset, batch_size=batch_size, shuffle=False, collate_fn=collate_fn)
    
    all_predictions = []
    
    print(f"Generating predictions for {len(image_paths)} images with {len(ensemble_predictor.models)} model(s)...")
    
    for batch in tqdm(dataloader, desc="Generating predictions"):
        ensemble_probs, predicted_classes, max_probs, paths, individual_preds = \
            ensemble_predictor.predict_batch(batch)
        
        for i, (path, pred_class, confidence) in enumerate(zip(paths, predicted_classes, max_probs)):
            pred_class_name = ensemble_predictor.class_names[pred_class.item()]
            confidence_score = confidence.item()
            
            # Calculate model agreement for ensemble
            model_disagreement = 0.0
            if len(individual_preds) > 1:
                individual_max_probs = [torch.max(pred[i]).item() for pred in individual_preds]
                model_disagreement = np.std(individual_max_probs)
            
            # Individual model predictions for logging
            individual_predictions = {}
            if save_individual_predictions and len(individual_preds) > 1:
                for model_idx, pred in enumerate(individual_preds):
                    model_max_prob, model_pred_class = torch.max(pred[i], dim=0)
                    individual_predictions[f'model_{model_idx}'] = {
                        'predicted_class': ensemble_predictor.class_names[model_pred_class.item()],
                        'confidence': model_max_prob.item(),
                        'probabilities': pred[i].numpy().tolist()
                    }
            
            # Store all prediction data
            prediction_data = {
                'image_path': path,
                'filename': os.path.basename(path),
                'ensemble_predicted_class': pred_class_name,
                'ensemble_confidence': confidence_score,
                'ensemble_probabilities': ensemble_probs[i].numpy().tolist(),
                'model_disagreement': model_disagreement,
                'num_models': len(ensemble_predictor.models)
            }
            
            if save_individual_predictions and individual_predictions:
                prediction_data['individual_models'] = individual_predictions
            
            all_predictions.append(prediction_data)
    
    return all_predictions

def organize_images_by_threshold(predictions_data, output_dir, confidence_threshold=0.9, disagreement_threshold=0.15):
    """
    Organize images based on predictions and confidence threshold
    """
    print(f"Organizing images with confidence threshold: {confidence_threshold}")
    
    # Get class names from first prediction
    class_names = list(set([pred['ensemble_predicted_class'] for pred in predictions_data]))
    
    # Create output directories
    threshold_dir = os.path.join(output_dir, f"threshold_{confidence_threshold}")
    os.makedirs(threshold_dir, exist_ok=True)
    
    for class_name in class_names:
        os.makedirs(os.path.join(threshold_dir, class_name), exist_ok=True)
    
    # Create special directories
    low_conf_dir = os.path.join(threshold_dir, 'low_confidence')
    os.makedirs(low_conf_dir, exist_ok=True)
    
    disagreement_dir = os.path.join(threshold_dir, 'model_disagreement')
    os.makedirs(disagreement_dir, exist_ok=True)
    
    # Organize images
    high_conf_count = 0
    low_conf_count = 0
    disagreement_count = 0
    
    for pred in tqdm(predictions_data, desc="Organizing images"):
        image_path = pred['image_path']
        filename = pred['filename']
        confidence = pred['ensemble_confidence']
        pred_class = pred['ensemble_predicted_class']
        disagreement = pred['model_disagreement']
        
        # Determine destination
        if pred['num_models'] > 1 and disagreement > disagreement_threshold:
            dest_path = os.path.join(disagreement_dir, filename)
            disagreement_count += 1
        elif confidence >= confidence_threshold:
            dest_path = os.path.join(threshold_dir, pred_class, filename)
            high_conf_count += 1
        else:
            dest_path = os.path.join(low_conf_dir, filename)
            low_conf_count += 1
        
        # Copy image
        try:
            if not os.path.exists(dest_path):  # Avoid overwriting
                shutil.copy2(image_path, dest_path)
        except Exception as e:
            print(f"Error copying {image_path}: {e}")
    
    # Save threshold-specific summary
    summary = {
        'confidence_threshold': confidence_threshold,
        'disagreement_threshold': disagreement_threshold,
        'high_confidence_count': high_conf_count,
        'low_confidence_count': low_conf_count,
        'disagreement_count': disagreement_count,
        'class_distribution': {}
    }
    
    # Count per class
    for class_name in class_names:
        class_dir = os.path.join(threshold_dir, class_name)
        if os.path.exists(class_dir):
            count = len([f for f in os.listdir(class_dir) if os.path.isfile(os.path.join(class_dir, f))])
            summary['class_distribution'][class_name] = count
    
    summary_path = os.path.join(threshold_dir, 'organization_summary.json')
    with open(summary_path, 'w') as f:
        json.dump(summary, f, indent=2)
    
    print(f"  High confidence: {high_conf_count}")
    print(f"  Low confidence: {low_conf_count}")
    print(f"  Model disagreement: {disagreement_count}")
    
    return summary

def main():
    parser = argparse.ArgumentParser(description='Generate predictions and organize by threshold')
    parser.add_argument('--model_path', type=str, required=True,
                       help='Path to trained model directory')
    parser.add_argument('--input_dir', type=str, required=True,
                       help='Directory containing unlabeled images')
    parser.add_argument('--output_dir', type=str, required=True,
                       help='Directory to save results')
    parser.add_argument('--batch_size', type=int, default=32,
                       help='Batch size for inference')
    parser.add_argument('--save_individual_predictions', action='store_true',
                       help='Save individual model predictions')
    parser.add_argument('--class_names', type=str, nargs='+',
                       help='List of class names (if not in model config)')
    
    # Options for organizing
    parser.add_argument('--predict_only', action='store_true',
                       help='Only generate predictions, do not organize images')
    parser.add_argument('--confidence_thresholds', type=float, nargs='+',
                       default=[0.7, 0.8, 0.9, 0.95],
                       help='List of confidence thresholds to test')
    
    args = parser.parse_args()
    
    # Setup device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}")
    
    # Load ensemble predictor
    try:
        ensemble_predictor = EnsemblePredictor(args.model_path, device)
    except Exception as e:
        print(f"Error loading models: {e}")
        return
    
    # Override class names if provided
    if args.class_names:
        ensemble_predictor.class_names = args.class_names
    
    # Get image paths
    image_paths = get_image_paths(args.input_dir)
    print(f"Found {len(image_paths)} images")
    
    if len(image_paths) == 0:
        print("No images found!")
        return
    
    # Generate all predictions
    predictions_data = generate_all_predictions(
        ensemble_predictor, image_paths, args.output_dir, 
        args.batch_size, args.save_individual_predictions
    )
    
    # Save all predictions to JSON
    predictions_file = os.path.join(args.output_dir, 'all_predictions.json')
    with open(predictions_file, 'w') as f:
        json.dump({
            'model_path': args.model_path,
            'num_models': len(ensemble_predictor.models),
            'total_images': len(image_paths),
            'class_names': ensemble_predictor.class_names,
            'predictions': predictions_data
        }, f, indent=2)
    
    print(f"Saved all predictions to: {predictions_file}")
    
    if not args.predict_only:
        # Organize images at different thresholds
        print(f"\nOrganizing images at different confidence thresholds...")
        
        for threshold in args.confidence_thresholds:
            print(f"\nThreshold: {threshold}")
            organize_images_by_threshold(predictions_data, args.output_dir, threshold)
        
        print(f"\nAll organizations complete! Check subdirectories in: {args.output_dir}")
    else:
        print("Prediction complete. Use the reorganize script to organize at different thresholds.")

if __name__ == "__main__":
    main()

# SEPARATE REORGANIZATION SCRIPT
"""
Save this as 'reorganize_by_threshold.py':

import json
import argparse
import os
import shutil
from tqdm import tqdm

def reorganize_predictions(predictions_file, output_dir, confidence_threshold=0.9, disagreement_threshold=0.15):
    '''Reorganize images based on existing predictions'''
    
    # Load predictions
    with open(predictions_file, 'r') as f:
        data = json.load(f)
    
    predictions_data = data['predictions']
    class_names = data['class_names']
    
    print(f"Reorganizing {len(predictions_data)} predictions with threshold {confidence_threshold}")
    
    # Create directories
    threshold_dir = os.path.join(output_dir, f"threshold_{confidence_threshold}")
    os.makedirs(threshold_dir, exist_ok=True)
    
    for class_name in class_names:
        os.makedirs(os.path.join(threshold_dir, class_name), exist_ok=True)
    
    low_conf_dir = os.path.join(threshold_dir, 'low_confidence')
    disagreement_dir = os.path.join(threshold_dir, 'model_disagreement')
    os.makedirs(low_conf_dir, exist_ok=True)
    os.makedirs(disagreement_dir, exist_ok=True)
    
    # Organize
    counts = {'high': 0, 'low': 0, 'disagreement': 0}
    
    for pred in tqdm(predictions_data):
        image_path = pred['image_path']
        filename = pred['filename']
        confidence = pred['ensemble_confidence']
        pred_class = pred['ensemble_predicted_class']
        disagreement = pred['model_disagreement']
        
        if pred['num_models'] > 1 and disagreement > disagreement_threshold:
            dest_path = os.path.join(disagreement_dir, filename)
            counts['disagreement'] += 1
        elif confidence >= confidence_threshold:
            dest_path = os.path.join(threshold_dir, pred_class, filename)
            counts['high'] += 1
        else:
            dest_path = os.path.join(low_conf_dir, filename)
            counts['low'] += 1
        
        if os.path.exists(image_path) and not os.path.exists(dest_path):
            shutil.copy2(image_path, dest_path)
    
    print(f"High confidence: {counts['high']}")
    print(f"Low confidence: {counts['low']}")
    print(f"Disagreement: {counts['disagreement']}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--predictions_file', required=True)
    parser.add_argument('--output_dir', required=True)
    parser.add_argument('--confidence_threshold', type=float, default=0.9)
    args = parser.parse_args()
    
    reorganize_predictions(args.predictions_file, args.output_dir, args.confidence_threshold)
"""
