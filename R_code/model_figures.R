library(ggalluvial)
source("R_code/read.data.R")

inr_final <- bind_rows(inr_0,inr_1,inr_2,inr_3,inr_4,inr_5) %>% select(id, stage, species)

# start from your long table: inr_final (id, stage, species)
df <- inr_final %>%  mutate(stage = as.integer(stage))
stages <- sort(unique(df$stage))

# 2) widen, then collapse identical full paths into counts
df_wide <- df %>%
  tidyr::pivot_wider(
    names_from = stage, values_from = species, names_prefix = "stage"
  )

path_counts <- df_wide %>%
  select(-id) %>%
  group_by(across(everything())) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    path_id = row_number(),
    start_species = .data[[paste0("stage", stages[1])]]  # color by starting group (optional)
  )

stage_cols <- paste0("stage", stages)

# 3) convert to lodes (long) with weights = counts, preserving "" as a category
lodes <- ggalluvial::to_lodes_form(
  path_counts, axes = stage_cols, key = "stage", value = "species", id = "path_id"
) %>% mutate(species = factor(species, levels=species_order))

alluvial <- ggplot(lodes, aes(x = stage, stratum = species, alluvium = path_id, y = n)) +
  geom_flow(aes(fill = species), stat = "alluvium", lode.guidance = "forward", knot.pos = 0.3, color = "transparent") + 
  geom_stratum(aes(fill = species), width = 0.25, color = "transparent") +
  scale_fill_manual(values = cols) +
  theme(
    axis.title = element_blank(),       # remove axis labels
    axis.text = element_blank(),        # remove axis text
    axis.ticks = element_blank(),       # remove axis ticks
    panel.grid = element_blank(),       # remove grid
    panel.background = element_blank(), # remove panel background
    plot.background = element_blank(),  # remove plot background
    legend.position = "none"
  )

pdf("figures/panels/alluvial_plot.pdf", width = 10, height = 6)
print(alluvial)
dev.off()

# Read the TSV file
cm <- read.table("results/confusion_matrix.txt", sep = "\t", header = TRUE, row.names = 1)
cm_order <- c("utriculus","minuta","megalista","physalis","unidentified")

# Convert to long format using base R + dplyr
cm_melted <- expand.grid(Actual = factor(rownames(cm), levels=rev(species_order)), Predicted = factor(colnames(cm), levels=species_order)) %>%
  mutate(Proportion = as.vector(as.matrix(cm)))

# Create the heatmap
cm_plot1 <- ggplot(cm_melted, aes(x = Predicted, y = Actual, fill = Proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Proportion, 3)), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Predicted class",
       y = "Actual class",
       fill = "Proportion") +
  coord_fixed()

pdf(file="figures/panels/confusion_matrix_raw.pdf", width=3.5, height=3.5)
print(cm_plot1)
dev.off()

# Normalize by rows (each actual class sums to 1)
cm_normalized <- cm / rowSums(cm)

# Convert to long format using base R + dplyr
cm_melted <- expand.grid(Actual = factor(rownames(cm_normalized), levels=rev(species_order)), Predicted = factor(colnames(cm_normalized), levels=species_order)) %>%
  mutate(Proportion = as.vector(as.matrix(cm_normalized)))

# Create the heatmap
cm_plot2 <- ggplot(cm_melted, aes(x = Predicted, y = Actual, fill = Proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Proportion, 2)), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Predicted class",
       y = "Actual class",
       fill = "Proportion") +
  coord_fixed()

pdf(file="figures/panels/confusion_matrix_norm.pdf", width=3.5, height=3.5)
print(cm_plot2)
dev.off()

inr_predicted <- inr_0  %>% mutate(predicted_species = "unidentified") %>% filter(!id %in% predicted_data$id) %>% bind_rows(predicted_data)
inr_community <- inr_0  %>% mutate(community_species = "unidentified") %>% filter(!id %in% community_data$id) %>% bind_rows(community_data)

matched_data <- inr_predicted %>%
  inner_join(inr_community, by = "id") %>%
  select(predicted_species, community_species)

matched_data$community_species <- matched_data$community_species
matched_data$predicted_species <- matched_data$predicted_species

cm <- table(Actual = matched_data$community_species, 
            Predicted = matched_data$predicted_species)

# Convert to long format using base R + dplyr
cm_melted <- expand.grid(Actual = factor(rownames(cm), levels = rev(species_order)), 
                        Predicted = factor(colnames(cm), levels = species_order)) %>%
  mutate(Proportion = as.vector(as.matrix(cm)))

# Create the heatmap
cm_plot3 <- ggplot(cm_melted, aes(x = Predicted, y = Actual, fill = Proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Proportion, 3)), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Predicted species",
       y = "Community species",
       fill = "Proportion") +
  coord_fixed()

pdf(file="figures/panels/community_predicted_matrix.pdf", width=4, height=4)
print(cm_plot3)
dev.off()


transinat <- st_as_sf(final_results,coords=c("longitude","latitude"),crs=4326)
itran <- st_transform(transinat,robinson)
itran <- itran %>% mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]) %>% add_count(species) %>% arrange(desc(n))

global_map <- ggplot(data=world2) + geom_sf(fill = "#D3D3D3", colour = NA) +
  coord_sf(datum=NA) + 
  geom_point(data=itran,aes(x=x,y=y,color=species), size = 0.75, alpha=0.5,pch=16) + 
  scale_color_manual(values = cols) +
  scale_x_continuous(breaks = seq(-180, 180, by = 20)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 20)) + 
  theme(legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank())

pdf(file="figures/panels/global_map.pdf", width=5, height=3)
print(global_map)
dev.off()
 
fr <- final_results %>% filter(year > 2015) %>% filter(species != "") 

year_plot <- ggplot(fr,aes(x = ymd(observed_on), fill = species)) + 
  geom_histogram(binwidth=7) +
  facet_wrap(~ species, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = cols) + 
  ylab("") + xlab("year") + 
  scale_y_continuous(breaks = conditional_breaks) +
	theme(strip.text = element_text(size = 0), legend.position="none",
    text = element_text(size = 8))

pdf(file="figures/panels/year_species.pdf", width=4.5, height=3)
print(year_plot)
dev.off()
