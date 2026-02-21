# plot inaturalist records

setwd("~/Downloads/Physalia_spatiotemporal/")
source("R_code/read.data.R")

results <- final_results %>% filter(!is.na(longitude) & !is.na(latitude)) %>%
    mutate(species = ifelse(is.na(species),"",species))

LeftBound = -90
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_set_crs(4326)
robinson = paste("+proj=robin +lon_0=",LeftBound," +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",sep="")
world2 = world %>% st_break_antimeridian(lon_0 = LeftBound) %>% st_transform(crs = robinson)

plot_map <- function(data, title, cols) {

    transinat <- st_as_sf(data,coords=c("longitude","latitude"),crs=4326)
    itran <- st_transform(transinat,robinson)

    gmon <- ggplot(data=world2) + geom_sf(fill = "light gray", colour = NA) +
    coord_sf() +
    geom_point(data=itran,aes(geometry=geometry), fill = "white", color = "black", size = 2, stroke=0.01, alpha=1,pch=21 ,stat="sf_coordinates") + 
    geom_point(data=itran,aes(geometry=geometry,color=species), size = 2, alpha=1,pch=16 ,stat="sf_coordinates") + 
    scale_color_manual(values = cols) +
    theme(legend.position = "none") +
    theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())


    pdf(file=paste0(title,"_predictions.pdf"),width=24,height=8,useDingbats=F)
    print(gmon)
    dev.off()
}


cls <- c("black")
names(cls) <- c("Physalia")
plot_map(results %>% mutate(species = "Physalia"), "all_unlabeled", cls)

library(ggalluvial)
df_counts <- results %>%
  count(species, scientific_name) %>% 
  mutate(
    scientific_name = gsub(" ", "", gsub("Physalia", "", scientific_name))
  ) %>%
  mutate(across(everything(), ~tidyr::replace_na(., ""))) %>%


ggplot(df_counts,
       aes(axis1 = species, axis2 = scientific_name, y = n)) +
  geom_alluvium(aes(fill = species), width = 1/12, knot.pos = 0.4) +
  geom_stratum(aes(fill = species), width = 1/12, color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Species", "Scientific Name"), expand = c(.05, .05)) +
  scale_fill_manual(values = cols) + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


df_ids <- bind_rows(
  results %>%
    select(id, species) %>%
    mutate(type = "semisupervised"),
  results %>%
    mutate(species = gsub(" ", "", gsub("Physalia", "", scientific_name))) %>%
    select(id, species) %>%
    mutate(type = "community")
)

df_ids <-   df_ids %>% distinct(id, species, type)
df_ids$type <- factor(df_ids$type, levels = c("semisupervised", "community")) 

ggplot(df_ids,
       aes(x = type, stratum = species, alluvium = id,
           y = 1, fill = species, label = species)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom")