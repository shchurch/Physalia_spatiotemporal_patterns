import torch
import torch.nn.functional as F
from transformers import AutoImageProcessor, AutoModelForImageClassification
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image
import cv2
import os
import argparse
from pathlib import Path
import json

class AttentionVisualizer:
    def __init__(self, model_path, device='cuda' if torch.cuda.is_available() else 'cpu'):
        """
        Initialize with your trained model
        """
        self.device = device
        print(f"Loading model from: {model_path}")
        
        try:
            self.model = AutoModelForImageClassification.from_pretrained(model_path).to(device)
            self.processor = AutoImageProcessor.from_pretrained(model_path)
            self.model.eval()
            
            # Get class names from model config
            if hasattr(self.model.config, 'id2label'):
                self.class_names = list(self.model.config.id2label.values())
            else:
                self.class_names = [f"Class_{i}" for i in range(self.model.config.num_labels)]
            
            print(f"Model loaded successfully")
            print(f"Device: {self.device}")
            print(f"Classes: {self.class_names}")
            
        except Exception as e:
            print(f"Error loading model: {e}")
            raise
    
    def get_attention_map(self, image_path):
        """
        Extract attention map from ViT model
        """
        try:
            # Load and preprocess image
            image = Image.open(image_path).convert('RGB')
            inputs = self.processor(image, return_tensors="pt").to(self.device)
            
            print(f"Input shape: {inputs['pixel_values'].shape}")
            
            # Forward pass with attention output
            with torch.no_grad():
                outputs = self.model(**inputs, output_attentions=True)
                predictions = F.softmax(outputs.logits, dim=-1)
                predicted_class = torch.argmax(predictions, dim=-1).item()
                confidence = torch.max(predictions).item()
                
                # Get attention weights
                if hasattr(outputs, 'attentions') and outputs.attentions is not None:
                    print(f"Found {len(outputs.attentions)} attention layers")
                    
                    # Use the last layer's attention (most refined)
                    last_attention = outputs.attentions[-1]  # Shape: [batch, heads, seq_len, seq_len]
                    print(f"Attention shape: {last_attention.shape}")
                    
                    # Average across attention heads
                    attention = last_attention.mean(dim=1)  # Shape: [batch, seq_len, seq_len]
                    
                    # Get attention from CLS token (first token) to all patch tokens
                    cls_attention = attention[0, 0, 1:]  # Skip CLS token itself
                    
                    # Calculate grid size for spatial arrangement
                    num_patches = len(cls_attention)
                    grid_size = int(np.sqrt(num_patches))
                    
                    print(f"Number of patches: {num_patches}, Grid size: {grid_size}x{grid_size}")
                    
                    if grid_size * grid_size == num_patches:
                        # Reshape to spatial grid
                        attention_map = cls_attention.cpu().numpy().reshape(grid_size, grid_size)
                        return attention_map, predicted_class, confidence, image
                    else:
                        print(f"Cannot reshape {num_patches} patches into square grid")
                        return None, predicted_class, confidence, image
                else:
                    print("No attention weights found in model output")
                    return None, predicted_class, confidence, image
                    
        except Exception as e:
            print(f"Error processing {image_path}: {e}")
            return None, None, None, None
    
    def visualize_single_image(self, image_path, output_dir, overlay_alpha=0.6, min_confidence=0.8):
        """
        Create individual plots for a single image (only if confidence >= min_confidence)
        """
        print(f"Processing: {image_path}")
        
        attention_map, pred_class, confidence, original_image = self.get_attention_map(image_path)
        
        # Skip if confidence is too low
        if confidence < min_confidence:
            print(f"  Skipping - confidence {confidence:.3f} below threshold {min_confidence}")
            return False
        
        if attention_map is None:
            print(f"  No attention map available for {image_path}")
            return False
        
        # Create output filename base
        image_name = Path(image_path).stem
        true_class = Path(image_path).parent.name  # Assume folder name is true class
        
        # Resize attention map to match image size
        img_array = np.array(original_image)
        attention_resized = cv2.resize(attention_map, (img_array.shape[1], img_array.shape[0]))
        
        # Normalize attention map
        attention_normalized = (attention_resized - attention_resized.min()) / \
                              (attention_resized.max() - attention_resized.min())
        
        predicted_class_name = self.class_names[pred_class] if pred_class < len(self.class_names) else f"Class_{pred_class}"
        
        # 1. Original image
        plt.figure(figsize=(8, 6))
        plt.imshow(original_image)
        plt.title(f'Original Image\nTrue: {true_class}', fontsize=14)
        plt.axis('off')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/{image_name}_original.png", dpi=300, bbox_inches='tight')
        plt.close()
        
        # 2. Attention heatmap
        plt.figure(figsize=(8, 6))
        plt.imshow(attention_normalized, cmap='hot', interpolation='bilinear')
        plt.title(f'Attention Map\nPred: {predicted_class_name} ({confidence:.3f})', fontsize=14)
        plt.colorbar(label='Attention Weight')
        plt.axis('off')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/{image_name}_attention.png", dpi=300, bbox_inches='tight')
        plt.close()
        
        # 3. Overlay
        plt.figure(figsize=(8, 6))
        plt.imshow(original_image)
        plt.imshow(attention_normalized, cmap='hot', alpha=overlay_alpha, interpolation='bilinear')
        plt.title(f'Attention Overlay\nPred: {predicted_class_name} ({confidence:.3f})', fontsize=14)
        plt.axis('off')
        plt.tight_layout()
        plt.savefig(f"{output_dir}/{image_name}_overlay.png", dpi=300, bbox_inches='tight')
        plt.close()
        
        print(f"  Saved visualizations for {image_name}")
        print(f"  True: {true_class}, Predicted: {predicted_class_name}, Confidence: {confidence:.3f}")
        
        return True

def collect_images(data_dir, num_per_class=5):
    """
    Collect sample images from each class directory
    """
    print(f"Collecting images from: {data_dir}")
    
    data_path = Path(data_dir)
    if not data_path.exists():
        print(f"Error: Data directory does not exist: {data_dir}")
        return []
    
    image_extensions = {'.jpg', '.jpeg', '.png', '.JPG', '.JPEG', '.PNG'}
    collected_images = []
    
    # Look for class directories
    class_dirs = [d for d in data_path.iterdir() if d.is_dir()]
    
    if not class_dirs:
        # If no subdirectories, treat the directory as containing all images
        print("No class subdirectories found, collecting from main directory")
        for img_path in data_path.iterdir():
            if img_path.suffix in image_extensions:
                collected_images.append(img_path)
        collected_images = collected_images[:num_per_class * 4]  # Assume 4 classes max
    else:
        # Collect from each class directory
        for class_dir in sorted(class_dirs):
            print(f"  Class: {class_dir.name}")
            class_images = []
            
            for img_path in class_dir.iterdir():
                if img_path.suffix in image_extensions:
                    class_images.append(img_path)
            
            # Take up to num_per_class images
            selected = class_images[:num_per_class]
            collected_images.extend(selected)
            print(f"    Selected {len(selected)} images")
    
    print(f"Total images collected: {len(collected_images)}")
    return collected_images

def main():
    parser = argparse.ArgumentParser(description='Visualize ViT attention maps')
    parser.add_argument('--model_path', type=str, required=True,
                       help='Path to trained model directory')
    parser.add_argument('--data_dir', type=str, required=True,
                       help='Directory containing test images (with class subdirectories)')
    parser.add_argument('--output_dir', type=str, default='attention_visualizations',
                       help='Directory to save attention visualizations')
    parser.add_argument('--num_per_class', type=int, default=3,
                       help='Number of images to process per class')
    parser.add_argument('--overlay_alpha', type=float, default=0.6,
                       help='Alpha transparency for attention overlay (0-1)')
    parser.add_argument('--min_confidence', type=float, default=0.8,
                       help='Minimum prediction confidence to generate visualizations')
    
    args = parser.parse_args()
    
    print("=== ViT Attention Visualizer ===")
    print(f"Model path: {args.model_path}")
    print(f"Data directory: {args.data_dir}")
    print(f"Output directory: {args.output_dir}")
    print(f"Images per class: {args.num_per_class}")
    
    # Validate paths
    if not os.path.exists(args.model_path):
        print(f"Error: Model path does not exist: {args.model_path}")
        return
    
    if not os.path.exists(args.data_dir):
        print(f"Error: Data directory does not exist: {args.data_dir}")
        return
    
    # Create output directory
    os.makedirs(args.output_dir, exist_ok=True)
    
    # Initialize visualizer
    try:
        visualizer = AttentionVisualizer(args.model_path)
    except Exception as e:
        print(f"Failed to initialize visualizer: {e}")
        return
    
    # Collect images to process
    images_to_process = collect_images(args.data_dir, args.num_per_class)
    
    if not images_to_process:
        print("No images found to process")
        return
    
    # Process each image
    successful_count = 0
    failed_count = 0
    results = []
    
    for i, image_path in enumerate(images_to_process):
        print(f"\n[{i+1}/{len(images_to_process)}]", end=" ")
        
        try:
            success = visualizer.visualize_single_image(
                str(image_path), 
                args.output_dir, 
                args.overlay_alpha,
                args.min_confidence
            )
            
            if success:
                successful_count += 1
                results.append({
                    'image_path': str(image_path),
                    'true_class': image_path.parent.name,
                    'status': 'success'
                })
            else:
                failed_count += 1
                results.append({
                    'image_path': str(image_path),
                    'true_class': image_path.parent.name,
                    'status': 'failed'
                })
                
        except Exception as e:
            print(f"  Error processing {image_path}: {e}")
            failed_count += 1
            results.append({
                'image_path': str(image_path),
                'true_class': image_path.parent.name,
                'status': 'error',
                'error': str(e)
            })
    
    # Save summary
    summary = {
        'model_path': args.model_path,
        'data_dir': args.data_dir,
        'total_processed': len(images_to_process),
        'successful': successful_count,
        'failed': failed_count,
        'class_names': visualizer.class_names,
        'results': results
    }
    
    summary_path = os.path.join(args.output_dir, 'processing_summary.json')
    with open(summary_path, 'w') as f:
        json.dump(summary, f, indent=2)
    
    # Print final summary
    print(f"\n=== PROCESSING COMPLETE ===")
    print(f"Total images: {len(images_to_process)}")
    print(f"Successful: {successful_count}")
    print(f"Failed: {failed_count}")
    print(f"Success rate: {100*successful_count/len(images_to_process):.1f}%")
    print(f"Results saved to: {args.output_dir}")
    print(f"Summary saved to: {summary_path}")
    
    # List generated files
    generated_files = list(Path(args.output_dir).glob("*.png"))
    print(f"Generated {len(generated_files)} visualization files")

if __name__ == "__main__":
    main()

