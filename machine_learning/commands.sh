salloc --partition=gpu_devel --gpus=2
ml scikit-learn
ml CUDA
ml PyTorch
ml torchvision

DIR="/vast/palmer/scratch/dunn/sc2962/inat_download_Aug14_2025"

# 1. Split your labeled data (2k images)
python split_data.py --input_dir ${DIR}/labeled_data --output_dir ${DIR}/data_splits

python train2.py   --data_dir ${DIR}/data_splits   --model_name google/vit-base-patch16-224-in21k   --output_dir ${DIR}/results_vit/iteration_0   --epochs 15   
--batch_size 32   --learning_rate 2e-5



python calibrated_script2.py \
    --data_dir ${DIR}/data_splits \
    --output_dir ${DIR}/calibrated1 \
    --model_name google/vit-base-patch16-224-in21k \
    --calibration_method temperature \
    --ensemble_size 3 \
    --loss_type focal \
    --focal_gamma 2.0


ml OpenCV
module unload CUDA; ml CUDA

python attention_visualizer.py \
    --model_path ${DIR}/calibrated1/model_2 \
    --data_dir ${DIR}/data_splits/test \
    --output_dir ./attention_results \
    --num_per_class 25 \
    --overlay_alpha 0.5 \
    --min_confidence 0.5

python flexible_labeling.py \
    --model_path ${DIR}/calibrated1 \
    --input_dir ${DIR}/unlabeled_data \
    --output_dir ${DIR}/prediction_results \
    --predict_only

python reorganize_by_threshold.py     --predictions_file ${DIR}/prediction_results/all_predictions.json    --output_dir ${DIR}/labels_thresholds     --compare_thresholds 0.5 0.6 0.65 0.7 0.75 0.8
