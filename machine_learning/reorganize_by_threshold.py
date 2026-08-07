import json
import argparse
import os
import shutil
from tqdm import tqdm
from pathlib import Path

def reorganize_predictions(predictions_file, output_dir, confidence_threshold=0.9, disagreement_threshold=0.15, copy_mode='copy'):
    """
    Reorganize images based on existing predictions at a new confidence threshold
    
    Args:
        predictions_file: Path to all_predictions.json
        output_dir: Base output directory
        confidence_threshold: New confidence threshold to apply
        disagreement_threshold: Threshold for model disagreement
        copy_mode: 'copy', 'symlink', or 'move'
    """
    
    print(f"Loading predictions from: {predictions_file}")
    
    # Load predictions
    if not os.path.exists(predictions_file):
        print(f"Error: Predictions file not found: {predictions_file}")
        return
    
    with open(predictions_file, 'r') as f:
        data = json.load(f)
    
    predictions_data = data['predictions']
    class_names = data['class_names']
    total_images = len(predictions_data)
    
    print(f"Loaded predictions for {total_images} images")
    print(f"Classes: {class_names}")
    print(f"Reorganizing with confidence threshold: {confidence_threshold}")
    print(f"Model disagreement threshold: {disagreement_threshold}")
    print(f"File operation mode: {copy_mode}")
    
    # Create new threshold directory
    threshold_dir = os.path.join(output_dir, f"threshold_{confidence_threshold}")
    os.makedirs(threshold_dir, exist_ok=True)
    
    # Create class directories
    for class_name in class_names:
        os.makedirs(os.path.join(threshold_dir, class_name), exist_ok=True)
    
    # Create special directories
    low_conf_dir = os.path.join(threshold_dir, 'low_confidence')
    disagreement_dir = os.path.join(threshold_dir, 'model_disagreement')
    os.makedirs(low_conf_dir, exist_ok=True)
    os.makedirs(disagreement_dir, exist_ok=True)
    
    # Track statistics
    counts = {
        'high_confidence': 0,
        'low_confidence': 0,
        'model_disagreement': 0,
        'file_not_found': 0,
        'file_exists': 0,
        'processed': 0
    }
    
    class_distribution = {class_name: 0 for class_name in class_names}
    
    # Organize images
    print("Organizing images...")
    for pred in tqdm(predictions_data, desc="Processing images"):
        image_path = pred['image_path']
        filename = pred['filename']
        confidence = pred['ensemble_confidence']
        pred_class = pred['ensemble_predicted_class']
        disagreement = pred['model_disagreement']
        num_models = pred['num_models']
        
        # Check if source file exists
        if not os.path.exists(image_path):
            counts['file_not_found'] += 1
            continue
        
        # Determine destination based on new threshold
        if num_models > 1 and disagreement > disagreement_threshold:
            dest_path = os.path.join(disagreement_dir, filename)
            category = 'model_disagreement'
        elif confidence >= confidence_threshold:
            dest_path = os.path.join(threshold_dir, pred_class, filename)
            category = 'high_confidence'
            class_distribution[pred_class] += 1
        else:
            dest_path = os.path.join(low_conf_dir, filename)
            category = 'low_confidence'
        
        counts[category] += 1
        
        # Skip if destination already exists
        if os.path.exists(dest_path):
            counts['file_exists'] += 1
            continue
        
        # Perform file operation
        try:
            if copy_mode == 'copy':
                shutil.copy2(image_path, dest_path)
            elif copy_mode == 'symlink':
                os.symlink(os.path.abspath(image_path), dest_path)
            elif copy_mode == 'move':
                shutil.move(image_path, dest_path)
            else:
                print(f"Unknown copy mode: {copy_mode}")
                return
            
            counts['processed'] += 1
            
        except Exception as e:
            print(f"Error processing {image_path}: {e}")
            continue
    
    # Create summary
    summary = {
        'reorganization_info': {
            'source_predictions_file': predictions_file,
            'confidence_threshold': confidence_threshold,
            'disagreement_threshold': disagreement_threshold,
            'copy_mode': copy_mode,
            'total_images_in_predictions': total_images
        },
        'counts': counts,
        'class_distribution': class_distribution,
        'percentages': {
            'high_confidence': (counts['high_confidence'] / total_images) * 100,
            'low_confidence': (counts['low_confidence'] / total_images) * 100,
            'model_disagreement': (counts['model_disagreement'] / total_images) * 100
        }
    }
    
    # Save summary
    summary_path = os.path.join(threshold_dir, 'reorganization_summary.json')
    with open(summary_path, 'w') as f:
        json.dump(summary, f, indent=2)
    
    # Print results
    print(f"\nReorganization complete!")
    print(f"Results saved to: {threshold_dir}")
    print(f"\nStatistics:")
    print(f"  High confidence (≥{confidence_threshold}): {counts['high_confidence']} ({summary['percentages']['high_confidence']:.1f}%)")
    print(f"  Low confidence: {counts['low_confidence']} ({summary['percentages']['low_confidence']:.1f}%)")
    print(f"  Model disagreement: {counts['model_disagreement']} ({summary['percentages']['model_disagreement']:.1f}%)")
    print(f"  Files processed: {counts['processed']}")
    print(f"  Files not found: {counts['file_not_found']}")
    print(f"  Files already existed: {counts['file_exists']}")
    
    print(f"\nClass distribution (high confidence only):")
    for class_name, count in class_distribution.items():
        percentage = (count / counts['high_confidence']) * 100 if counts['high_confidence'] > 0 else 0
        print(f"  {class_name}: {count} ({percentage:.1f}%)")
    
    print(f"\nSummary saved to: {summary_path}")
    
    return summary

def compare_thresholds(predictions_file, output_dir, thresholds):
    """
    Compare multiple thresholds and generate a comparison report
    """
    print(f"Comparing thresholds: {thresholds}")
    
    comparison_results = {}
    
    for threshold in thresholds:
        print(f"\nProcessing threshold: {threshold}")
        summary = reorganize_predictions(predictions_file, output_dir, threshold, copy_mode='symlink')
        if summary:
            comparison_results[threshold] = summary
    
    # Create comparison report
    comparison_path = os.path.join(output_dir, 'threshold_comparison.json')
    with open(comparison_path, 'w') as f:
        json.dump(comparison_results, f, indent=2)
    
    # Print comparison table
    print(f"\n{'='*60}")
    print("THRESHOLD COMPARISON")
    print(f"{'='*60}")
    print(f"{'Threshold':<12} {'High Conf':<10} {'Low Conf':<10} {'Disagreement':<12}")
    print(f"{'-'*60}")
    
    for threshold in sorted(thresholds):
        if threshold in comparison_results:
            counts = comparison_results[threshold]['counts']
            print(f"{threshold:<12} {counts['high_confidence']:<10} {counts['low_confidence']:<10} {counts['model_disagreement']:<12}")
    
    print(f"\nComparison report saved to: {comparison_path}")

def main():
    parser = argparse.ArgumentParser(description='Reorganize images based on existing predictions with new confidence threshold')
    parser.add_argument('--predictions_file', type=str, required=True,
                       help='Path to all_predictions.json file')
    parser.add_argument('--output_dir', type=str, required=True,
                       help='Base output directory')
    parser.add_argument('--confidence_threshold', type=float, default=0.9,
                       help='New confidence threshold to apply')
    parser.add_argument('--disagreement_threshold', type=float, default=0.15,
                       help='Model disagreement threshold')
    parser.add_argument('--copy_mode', type=str, default='copy',
                       choices=['copy', 'symlink', 'move'],
                       help='How to handle files: copy, symlink, or move')
    
    # Multiple threshold comparison
    parser.add_argument('--compare_thresholds', type=float, nargs='+',
                       help='Compare multiple thresholds (uses symlinks)')
    
    args = parser.parse_args()
    
    # Validate inputs
    if not os.path.exists(args.predictions_file):
        print(f"Error: Predictions file not found: {args.predictions_file}")
        return
    
    os.makedirs(args.output_dir, exist_ok=True)
    
    if args.compare_thresholds:
        # Compare multiple thresholds
        compare_thresholds(args.predictions_file, args.output_dir, args.compare_thresholds)
    else:
        # Single threshold reorganization
        reorganize_predictions(
            args.predictions_file, 
            args.output_dir, 
            args.confidence_threshold,
            args.disagreement_threshold,
            args.copy_mode
        )

if __name__ == "__main__":
    main()
