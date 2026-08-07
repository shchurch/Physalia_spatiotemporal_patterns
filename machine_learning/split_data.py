import os
import shutil
import argparse
from sklearn.model_selection import train_test_split
import random
from collections import defaultdict, Counter
import json

def get_image_paths_by_class(data_dir):
    """
    Get all image paths organized by class from a directory structure like:
    data_dir/
    ├── class1/
    │   ├── img1.jpg
    │   └── img2.jpg
    ├── class2/
    │   ├── img3.jpg
    │   └── img4.jpg
    """
    image_extensions = {'.jpg', '.jpeg', '.png', '.bmp', '.tiff', '.webp', '.JPG', '.JPEG', '.PNG'}
    class_images = defaultdict(list)
    
    for class_name in os.listdir(data_dir):
        class_path = os.path.join(data_dir, class_name)
        if os.path.isdir(class_path):
            for filename in os.listdir(class_path):
                if any(filename.endswith(ext) for ext in image_extensions):
                    full_path = os.path.join(class_path, filename)
                    class_images[class_name].append(full_path)
    
    return dict(class_images)

def stratified_split(class_images, train_ratio=0.7, val_ratio=0.15, test_ratio=0.15, random_seed=42):
    """
    Split images maintaining class distribution across train/val/test sets
    """
    if abs(train_ratio + val_ratio + test_ratio - 1.0) > 1e-6:
        raise ValueError("train_ratio + val_ratio + test_ratio must equal 1.0")
    
    random.seed(random_seed)
    
    splits = {'train': {}, 'val': {}, 'test': {}}
    
    for class_name, image_paths in class_images.items():
        n_images = len(image_paths)
        
        if n_images < 3:
            print(f"Warning: Class '{class_name}' has only {n_images} images. "
                  f"All will go to training set.")
            splits['train'][class_name] = image_paths
            splits['val'][class_name] = []
            splits['test'][class_name] = []
            continue
        
        # Shuffle images
        shuffled_images = image_paths.copy()
        random.shuffle(shuffled_images)
        
        # Calculate split indices
        n_train = max(1, int(n_images * train_ratio))
        n_val = max(1, int(n_images * val_ratio)) if val_ratio > 0 else 0
        n_test = n_images - n_train - n_val
        
        # Ensure we have at least 1 image in test if test_ratio > 0
        if test_ratio > 0 and n_test == 0:
            n_test = 1
            n_train -= 1
        
        # Split the images
        train_images = shuffled_images[:n_train]
        val_images = shuffled_images[n_train:n_train + n_val]
        test_images = shuffled_images[n_train + n_val:]
        
        splits['train'][class_name] = train_images
        splits['val'][class_name] = val_images
        splits['test'][class_name] = test_images
        
        print(f"Class '{class_name}': {len(train_images)} train, "
              f"{len(val_images)} val, {len(test_images)} test")
    
    return splits

def copy_files_to_splits(splits, output_dir):
    """
    Copy files to train/val/test directory structure
    """
    split_dirs = {}
    
    for split_name in ['train', 'val', 'test']:
        split_dir = os.path.join(output_dir, split_name)
        os.makedirs(split_dir, exist_ok=True)
        split_dirs[split_name] = split_dir
    
    # Create class directories and copy files
    for split_name, class_images in splits.items():
        for class_name, image_paths in class_images.items():
            if not image_paths:  # Skip empty classes
                continue
                
            class_dir = os.path.join(split_dirs[split_name], class_name)
            os.makedirs(class_dir, exist_ok=True)
            
            for image_path in image_paths:
                filename = os.path.basename(image_path)
                dest_path = os.path.join(class_dir, filename)
                shutil.copy2(image_path, dest_path)

def print_split_summary(splits):
    """Print summary statistics of the split"""
    print("\n" + "="*50)
    print("SPLIT SUMMARY")
    print("="*50)
    
    total_counts = Counter()
    class_names = set()
    
    for split_name, class_images in splits.items():
        split_total = 0
        print(f"\n{split_name.upper()} SET:")
        for class_name, image_paths in class_images.items():
            count = len(image_paths)
            split_total += count
            total_counts[split_name] += count
            class_names.add(class_name)
            print(f"  {class_name}: {count} images")
        print(f"  Total: {split_total} images")
    
    print(f"\nOVERALL TOTALS:")
    grand_total = sum(total_counts.values())
    for split_name, count in total_counts.items():
        percentage = (count / grand_total) * 100 if grand_total > 0 else 0
        print(f"  {split_name}: {count} images ({percentage:.1f}%)")
    print(f"  Grand total: {grand_total} images")
    print(f"  Number of classes: {len(class_names)}")

def save_split_info(splits, output_dir, args):
    """Save split information to JSON file"""
    split_info = {
        'split_ratios': {
            'train': args.train_ratio,
            'val': args.val_ratio,
            'test': args.test_ratio
        },
        'random_seed': args.random_seed,
        'source_directory': args.input_dir,
        'output_directory': args.output_dir,
        'class_counts': {}
    }
    
    for split_name, class_images in splits.items():
        split_info['class_counts'][split_name] = {}
        for class_name, image_paths in class_images.items():
            split_info['class_counts'][split_name][class_name] = len(image_paths)
    
    info_path = os.path.join(output_dir, 'split_info.json')
    with open(info_path, 'w') as f:
        json.dump(split_info, f, indent=2)
    
    print(f"\nSplit information saved to: {info_path}")

def main():
    parser = argparse.ArgumentParser(description='Split labeled image dataset into train/val/test sets')
    parser.add_argument('--input_dir', type=str, required=True,
                       help='Input directory with class subdirectories')
    parser.add_argument('--output_dir', type=str, required=True,
                       help='Output directory for split datasets')
    parser.add_argument('--train_ratio', type=float, default=0.7,
                       help='Fraction of data for training (default: 0.7)')
    parser.add_argument('--val_ratio', type=float, default=0.15,
                       help='Fraction of data for validation (default: 0.15)')
    parser.add_argument('--test_ratio', type=float, default=0.15,
                       help='Fraction of data for testing (default: 0.15)')
    parser.add_argument('--random_seed', type=int, default=42,
                       help='Random seed for reproducible splits (default: 42)')
    parser.add_argument('--dry_run', action='store_true',
                       help='Show split statistics without copying files')
    
    args = parser.parse_args()
    
    # Validate ratios
    total_ratio = args.train_ratio + args.val_ratio + args.test_ratio
    if abs(total_ratio - 1.0) > 1e-6:
        raise ValueError(f"Ratios must sum to 1.0, got {total_ratio}")
    
    # Check input directory exists
    if not os.path.exists(args.input_dir):
        raise FileNotFoundError(f"Input directory does not exist: {args.input_dir}")
    
    print(f"Reading images from: {args.input_dir}")
    print(f"Split ratios - Train: {args.train_ratio}, Val: {args.val_ratio}, Test: {args.test_ratio}")
    print(f"Random seed: {args.random_seed}")
    
    # Get images by class
    class_images = get_image_paths_by_class(args.input_dir)
    
    if not class_images:
        raise ValueError("No images found in input directory. "
                        "Make sure images are organized in class subdirectories.")
    
    print(f"\nFound {len(class_images)} classes:")
    total_images = 0
    for class_name, images in class_images.items():
        print(f"  {class_name}: {len(images)} images")
        total_images += len(images)
    print(f"Total images: {total_images}")
    
    # Create splits
    print(f"\nCreating splits...")
    splits = stratified_split(class_images, args.train_ratio, args.val_ratio, 
                            args.test_ratio, args.random_seed)
    
    # Print summary
    print_split_summary(splits)
    
    if args.dry_run:
        print("\nDry run completed. No files were copied.")
        return
    
    # Copy files
    print(f"\nCopying files to: {args.output_dir}")
    os.makedirs(args.output_dir, exist_ok=True)
    copy_files_to_splits(splits, args.output_dir)
    
    # Save split information
    save_split_info(splits, args.output_dir, args)
    
    print(f"\nDataset splitting completed!")
    print(f"Files organized in: {args.output_dir}")
    print(f"  - {args.output_dir}/train/")
    print(f"  - {args.output_dir}/val/")
    print(f"  - {args.output_dir}/test/")

if __name__ == "__main__":
    main()
