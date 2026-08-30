#!/usr/bin/env Rscript
# The open-ocean sampling map, the single panel assembled into
# figures/open_ocean_sampling.png.
#
# Extracted from the superseded R_code/PCA.qmd, which was dropped from the
# repository with the population-genomic analyses. Only the map is kept here:
# it reads no popgen output and shares nothing with the PCA and admixture
# sections of that notebook beyond the cluster assignments in data/subset.txt.
#
# Run from the repository root:  Rscript analysis/open_ocean_map.R
#
# Note on reproducibility: geom_label_repel places labels stochastically, so
# the seed below is what pins the output. Changing it moves the labels without
# changing the data.
#
# Samples are restricted to those that fall inside the plotted frame. The
# original chunk passed all 199 non-excluded samples to geom_label_repel and
# relied on ggrepel dropping the ones outside it; current ggrepel repels them
# to the frame edge instead, crowding the top of the plot with more than a
# hundred labels for specimens the figure is not about. Clipping to the frame
# explicitly makes the panel independent of that behaviour.
#
# 92 of the 199 samples fall inside the frame: meg 27, min 52, utr 13. All 92
# carry a cluster assignment, and none were collected in 2026, so the clipped
# panel has neither the NA assignment level nor the extra shape level that the
# unclipped version picks up. The three species left are exactly the three the
# figure caption names. See issue #54.

library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

set.seed(10012)

# Species assignment colours. Carried over verbatim from PCA.qmd, including the
# two trailing values that no name is assigned to.
cols <- c("dark cyan","dark orange","dark red","#9B59B6","#6DCFF6","dark blue","dodgerblue2")
names(cols) <- c("phy","utr","B2","meg","min")

# Sequenced specimens, with collection date and coordinates.
sample_info <- read.delim("data/sample_ids.tsv", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(year = as.numeric(gsub(".*/(.*)", "\\1", date_collected)),
         month = as.numeric(gsub("(.*)/.*/.*", "\\1", date_collected))) %>%
  separate(lat_long, into = c("latitude", "longitude"), sep = ", ") %>%
  mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
  filter(status != "excluded")

# Genomic cluster assignments, recoded to species abbreviations.
assignments <- read.delim("data/subset.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE) %>%
  rename(ID = V1, cluster = V2) %>%
  mutate(cluster = recode(cluster, "A" = "phy", "B1" = "utr", "C1" = "meg", "C2" = "min"))

sample_info <- sample_info %>% left_join(., assignments, by = "ID")

# Robinson projection centred on the South Pacific, with the antimeridian broken
# so the basemap does not smear across the frame.
LeftBound <- 110
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_set_crs(4326)
robinson <- paste("+proj=robin +lon_0=", LeftBound, " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", sep = "")
world2 <- world %>% st_break_antimeridian(lon_0 = LeftBound) %>% st_transform(crs = robinson)

# One point per rounded coordinate, cluster and collection period.
sample_info_tally <- sample_info %>%
  mutate(latitude = round(latitude, 0), longitude = round(longitude, 0),
         year2 = ifelse(year < 2023, "<2023", year)) %>%
  group_by(latitude, longitude, cluster, year2) %>% tally()

transpoint <- st_as_sf(sample_info_tally, coords = c("longitude", "latitude"), crs = 4326)
dtran <- st_transform(transpoint, robinson)

spac_lon_range <- c(140, -140)
spac_lat_range <- c(-50, -20)

bbox_ll <- st_bbox(c(
  xmin = spac_lon_range[1],
  xmax = spac_lon_range[2],
  ymin = spac_lat_range[1],
  ymax = spac_lat_range[2]
), crs = st_crs(4326)) %>% st_as_sfc()
bbox_proj <- st_transform(bbox_ll, crs = robinson)
bbox_coords <- st_bbox(bbox_proj)
xlims <- c(bbox_coords$xmin, bbox_coords$xmax)
ylims <- c(bbox_coords$ymin, bbox_coords$ymax)

# Clip to the plotted frame. Doing this on the projected coordinates rather
# than on raw latitude/longitude means the kept points are exactly the ones
# coord_sf will show, including across the antimeridian.
frame_xy <- st_coordinates(dtran)
inside <- frame_xy[, 1] >= xlims[1] & frame_xy[, 1] <= xlims[2] &
          frame_xy[, 2] >= ylims[1] & frame_xy[, 2] <= ylims[2]
cat(sprintf("%d of %d sampling points fall inside the frame; %d clipped\n",
            sum(inside), nrow(dtran), sum(!inside)))
dtran <- dtran[inside, ]

theme_set(theme_minimal())
g1 <- ggplot(data = world2) + geom_sf(fill = "light gray", colour = NA) +
  coord_sf(xlim = xlims, ylim = ylims) +
  geom_label_repel(data = dtran, aes(geometry = geometry, label = cluster, fill = cluster),
                   stat = "sf_coordinates", color = "white",
                   size = 1.5, max.overlaps = 100, box.padding = 0.35,
                   segment.color = "dark gray", segment.size = 0.25) +
  geom_point(data = dtran, aes(geometry = geometry, color = cluster, shape = as.character(year2)),
             size = 1.5, alpha = 0.9, stat = "sf_coordinates") +
  scale_x_continuous(breaks = c(120,130,140,150,160,170,180,-170,-160,-150,-140,-130,-120,-110,-100)) +
  scale_fill_manual(values = cols, "assignment") +
  scale_color_manual(values = cols, "assignment") +
  xlab("") + ylab("")

dir.create("figures/panels", showWarnings = FALSE, recursive = TRUE)
output_file <- "figures/panels/SPacific_sampling_assignments.pdf"
pdf(file = output_file, height = 4, width = 8)
print(g1)
dev.off()
cat("Plot saved to:", output_file, "\n")
