# machine_learning

Image classification of iNaturalist photographs to *Physalia* species, as
described in the Methods.

**These scripts cannot be re-run from this repository.** They were run on the
McCleary HPC against a local copy of the downloaded image corpus, which is not
distributed here (the images belong to their iNaturalist contributors). They are
included so the procedure can be followed and the parameters checked, not so it
can be reproduced end to end. Nothing in this directory has been edited since it
was run.

`commands.sh` records the invocations in order, with the working directory as it
was on the cluster.

| script | role |
|---|---|
| `split_data.py` | partition the labelled images into train / validation / test |
| `train2.py` | fine-tune `google/vit-base-patch16-224-in21k` |
| `calibrated_script2.py` | the ensemble with temperature scaling and focal loss |
| `flexible_labeling.py` | apply the trained model to the unlabelled images |
| `reorganize_by_threshold.py` | re-sort predictions at a different confidence threshold, without re-running inference |
| `attention_visualizer.py` | attention maps for individual images; produces the panels in the classification figure |

## Outputs

`reorganize_by_threshold.py` reads `results/all_predictions.json`, so a different
confidence threshold can be explored from the committed predictions without the
image corpus.

`attention_visualizer.py` writes to `results/attention_results/`, which is
**deliberately not tracked** — see the note in `.gitignore`. It is ~190 MB, and
the `_original.png` files in it are iNaturalist contributors' photographs whose
licences vary per observation.
