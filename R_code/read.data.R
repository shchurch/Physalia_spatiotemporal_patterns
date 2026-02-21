# This file is the main “data wiring” for the project:
# - Reads iNaturalist exports, manual labels/corrections, and model predictions
# - Builds staged classification tables (`inr_0` ... `inr_5`) showing how IDs change over time
# - Produces `final_results`, the canonical record-level table used by plotting scripts

library(rinat)
library(dplyr)
library(jsonlite)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(viridis)
library(ggrepel)
library(lubridate)
library(circular)
library(dggridR)

theme_set(theme_minimal())

# Shared helper functions used by multiple plotting scripts.
source("R_code/functions.R")

# Project color palette used across figures.
cols <- c("purple","dodgerblue","dark orange","dark cyan","gray","dark gray")
names(cols) <- c("megalista","minuta","utriculus","physalis","beach_sp","A/B")

# Global map basemap preparation.
# Uses a Robinson projection, with antimeridian handling for cleaner global plotting.
LeftBound = -90
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_set_crs(4326)
robinson = paste("+proj=robin +lon_0=",LeftBound," +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",sep="")
world2 = world %>% st_break_antimeridian(lon_0 = LeftBound) %>% st_transform(crs = robinson)
 
# Primary iNaturalist export for Physalia observations.
inat_results <- read.delim("data/observations-604981.csv",sep=",",header=T,stringsAsFactors=F) 
inat_results$image_url_large <- gsub("medium","large",inat_results$image_url)

# Baseline/effort (“normalization”) iNaturalist exports.
# These are combined and assigned `species = "beach_sp"` to represent background reporting effort.
norm_results1 <- read.delim("data/observations-607577.csv",sep=",",header=T,stringsAsFactors=F)
norm_results2 <- read.delim("data/observations-607598.csv",sep=",",header=T,stringsAsFactors=F)
norm_results <- rbind(norm_results1,norm_results2) %>% 
    mutate(species="beach_sp", yd = yday(observed_on), ymd = ymd(observed_on), mon = month(observed_on), year = year(observed_on)) 

# Manual expert labels from first pass identification.
# File format: two columns with (id, labeled_species).
labeled_data <- read.delim("data/labels.tsv",sep="\t",header=F,stringsAsFactors=F) %>%
    rename(labeled_species = V2, id = V1) %>% mutate(status = "labeled")

# Machine-learning predictions.
# Reads the full prediction JSON and extracts the ensemble class + confidence per image.
learned_json <- fromJSON("results/all_predictions.json")
learned_data <- learned_json$predictions %>% rename(learned_species = ensemble_predicted_class, confidence = ensemble_confidence) %>% 
    mutate(id = as.integer(gsub(".*unlabeled_data/(.*).jpg","\\1",image_path))) %>% select(id, learned_species, confidence)
    
# Confidence threshold used to accept model predictions into the curation pipeline.
confidence_threshold <- 0.65

# High-confidence predictions only.
predicted_data <- learned_data %>% filter(confidence > confidence_threshold) %>% 
    rename(predicted_species = learned_species) %>% mutate(status = "predicted")

# Manual correction passes.
# `corrected.tsv` is an initial correction round; filtered to IDs still present in `inat_results`.
corrected_data_1 <- read.delim("data/corrected.tsv",sep="\t",header=T,stringsAsFactors=F) %>% 
    mutate(status = "corrected_initial") %>% filter(id %in% inat_results$id) # remove any that were dropped from inat during the correction period

# `corrected_final.tsv` is a later/final correction round.
corrected_data_2 <- read.delim("data/corrected_final.tsv",sep="\t",header=T,stringsAsFactors=F) %>% 
    mutate(status = "corrected_final")

# Community IDs from iNat when the observation has been identified beyond the generic "Physalia".
# These are used as one stage in the staged curation pipeline.
community_data <- inat_results %>% filter(scientific_name != "Physalia") %>%
    mutate(community_species = gsub("Physalia ","",scientific_name), status = "community") %>%
    select(id, community_species, status)

# Staged curation pipeline.
# Each stage starts from the previous stage and “overlays” a data source by:
# - keeping records not mentioned in the new source, and
# - replacing/adding records that are mentioned in the new source
#
# This makes it easy to visualize how individual IDs change labels over time.
inr_0 <- inat_results %>% mutate(species = "", status = "unidentified") %>% mutate(stage = 0) %>% select(id,species,status,stage)
inr_1 <- inr_0 %>% filter(!id %in% labeled_data$id) %>% bind_rows(labeled_data %>% rename(species = labeled_species)) %>% mutate(stage = 1)
inr_2 <- inr_1 %>% filter(!id %in% corrected_data_1$id) %>% bind_rows(corrected_data_1 %>% rename(species = corrected_species)) %>% mutate(stage = 2)
inr_3 <- inr_2 %>% filter(!id %in% predicted_data$id) %>% bind_rows(predicted_data %>% rename(species = predicted_species)) %>% mutate(stage = 3)
inr_4 <- inr_3 %>% filter(!id %in% community_data$id) %>% bind_rows(community_data %>% rename(species = community_species)) %>% mutate(stage = 4)
inr_5 <- inr_4 %>% filter(!id %in% corrected_data_2$id) %>% bind_rows(corrected_data_2 %>% rename(species = corrected_species)) %>% mutate(stage = 5)

# Canonical plotting order for species categories.
species_order <- c("physalis", "megalista", "minuta", "utriculus", "unidentified")

# Final per-observation dataset.
# - One row per iNat observation (`id`)
# - Final curated `species` and `status` from the staged pipeline
# - All original iNat metadata columns (joined from `inat_results`)
# - Adds convenient date-derived columns (`yd`, `ymd`, `mon`, `year`)
# - Drops rows lacking coordinates (needed for mapping)
final_results <- inr_5 %>% select(id,species,status) %>% left_join(.,inat_results,by="id") %>%
	mutate(yd = yday(observed_on), ymd = ymd(observed_on),  mon = month(observed_on), year = year(observed_on)) %>% 
	filter(!is.na(longitude)) %>% mutate(species = factor(species, levels=species_order))

