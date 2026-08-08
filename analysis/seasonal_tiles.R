source("analysis/read.data.R")

set.seed(10012)

# Regions, with a short tag used in the output filenames. The tags match the
# ones seasonal_panels.R uses, so panels for the same region sort together
# whichever script produced them.
REGIONS <- tibble::tribble(
  ~tag,        ~x0,   ~x1,  ~y0,  ~y1,  ~label,
  "ESAm",      -60,   -20,  -40,    0,  "E South America",
  "CaribNAm", -100,   -60,    0,   40,  "Caribbean, N America",
  "SAfrica",     5,    45,  -40,    0,  "Southern Africa",
  "WAus",      100,   140,  -50,  -10,  "W Australia",
  "EAusNZ",    140,   180,  -50,  -10,  "E Australia, New Zealand"
)
# unname: build_world does c(xmin = longitude_range[1], ...), and a name carried
# over from the tibble turns that into "xmin.x0", which st_bbox reads as NA.
ranges <- lapply(seq_len(nrow(REGIONS)), function(k) unname(unlist(REGIONS[k, c("x0","x1","y0","y1")])))

#i <- 1
# for(i in 1:5){source("analysis/seasonal_tiles.R")}

# A hexagon is coloured by its circular median day of year only if it holds at
# least MIN_N records. Below that a median is not a median: with two records it
# is simply one of the two dates. Half the hexes in some panels sit at one or
# two records, so without this the gradient reads as better supported than it
# is. Thin hexes are kept rather than dropped, so the reader still sees that the
# species was recorded there -- drawn near-white with an outline, because the
# land fill is #D3D3D3 and a plain light-grey hex vanishes wherever it overlaps
# land, which is most of them in the Southern Africa panels.
MIN_N        <- 3
THIN         <- "grey93"
THIN_OUTLINE <- "grey60"

# One folder per figure family, so panels are easy to find and swap. Filenames
# are <type>_<region>_<species>, not the lat/lon strings the old flat layout
# used, which sorted by species and buried the region in a number soup.
OUT <- "figures/panels/gradients"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
DPI <- 500

species_of_interest <- c("utriculus","physalis","megalista","minuta")
longitude_range <- ranges[[i]][c(1,2)]
latitude_range  <- ranges[[i]][c(3,4)]
LeftBound = (longitude_range[2] + longitude_range[1]) / 2
TAG  <- REGIONS$tag[i]


name <- paste0(longitude_range[1],"_",longitude_range[2],"_",latitude_range[1],"_",latitude_range[2])
world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

# Frames are the full region box, as in the original panels. The three-month
# maps in the space figure are cropped to the records they contain, but these
# are not: a gradient panel is read as a picture of the whole region, and a
# tight frame both slices hexes at the edges and makes neighbouring panels look
# like different areas.
#
# Limits stay in lon/lat and go to coord_sf with default_crs below. Computing
# them in projected metres invites a subtle failure: coord_sf reads xlim/ylim in
# the coord's own CRS, so numbers derived from any other projection silently
# shift the window.
# A little wider than the region box itself: a hexagon centred on the boundary
# extends past it, so a frame drawn exactly at the box slices the outermost row.
FRAME_MARGIN <- 0.05
CROP_X <- longitude_range + c(-1, 1) * FRAME_MARGIN * diff(longitude_range)
CROP_Y <- latitude_range  + c(-1, 1) * FRAME_MARGIN * diff(latitude_range)
ASPECT <- diff(CROP_X) * cos(mean(CROP_Y) * pi / 180) / diff(CROP_Y)
# Size each file to its frame so no panel carries dead margin, as in the maps.
PANEL_H <- 2.1
PANEL_W <- max(1.25, min(3.4, PANEL_H * ASPECT)) + 0.35

# Shared styling: cropped frame, few enough axis breaks that the degree labels
# do not collide, and the grey border the space-figure panels carry.
panel_style <- function(p) {
  p +
    coord_sf(xlim = CROP_X, ylim = CROP_Y, expand = FALSE,
             default_crs = sf::st_crs(4326)) +
    scale_x_continuous(breaks = seq(-180, 180, by = 20)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 20)) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none",
          panel.grid = element_line(colour = "grey92", linewidth = 0.2),
          panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 0.3),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text = element_text(size = 5.5),
          text = element_text(size = 8))
}
save_panel <- function(p, stem) {
  ggplot2::ggsave(file.path(OUT, paste0(stem, "_", TAG, ".png")), p,
                  width = PANEL_W, height = PANEL_H, dpi = DPI)
}

plot_seasonal_point <- function(species_of_interest){
	world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

	if (nrow(world[['data']]) == 0) {
		return(invisible(NULL))
	}

	seasonal <- panel_style(
		ggplot(data=world[['world2']]) + geom_sf(fill = "#D3D3D3", colour = NA) +
		geom_point(data=world[['itran']],aes(geometry=geometry,color=yd), size = 1.5, alpha=0.75,pch=16 ,stat="sf_coordinates") +
		scale_color_gradientn(colors = pals::kovesi.cyclic_mrybm_35_75_c68_s25(12), limits = c(0,365)))

	save_panel(seasonal, paste0("points_", species_of_interest))
}

lapply(species_of_interest,plot_seasonal_point)

plot_seasonal_grid <- function(species_of_interest){
	world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

	if (nrow(world[['data']]) == 0) {
		return(invisible(NULL))
	}
	dggs <- dgconstruct(res=7)

	generate_tiled_map_count <- function(subset){
		subset$cell <- dgGEO_to_SEQNUM(dggs,subset$longitude,subset$latitude)$seqnum

		subcounts   <- subset %>% group_by(cell) %>% summarise(records=n(), .groups="drop")
		grid          <- dgcellstogrid(dggs,subcounts$cell)
		grid          <- merge(grid,subcounts,by.x="seqnum",by.y="cell")

		wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=-50"), quiet = TRUE)
		transformed_grid <- st_transform(wrapped_grid,robinson)

		panel_style(
			ggplot(data=world[['world2']]) + geom_sf(fill = "#D3D3D3", colour = NA) +
			geom_sf(data=transformed_grid, aes(fill=records), color=alpha("white", 0.1)) +
			scale_fill_gradient(low="dodgerblue2", high = "black",
				trans="log",limits = c(1,50),breaks=c(1,5,10,20,40), name="count"))
	}

	save_panel(generate_tiled_map_count(world[['data']]),
	           paste0("count_", species_of_interest))

	generate_tiled_map_season <- function(subset){
		subset$cell <- dgGEO_to_SEQNUM(dggs,subset$longitude,subset$latitude)$seqnum

		subcounts   <- subset %>% group_by(cell) %>%
			summarise(records = n(), median_day = circular_median_day(yd), .groups = "drop") %>%
			mutate(shown = ifelse(records >= MIN_N, median_day, NA_real_))
		grid          <- dgcellstogrid(dggs,subcounts$cell)
		grid          <- merge(grid,subcounts,by.x="seqnum",by.y="cell")

		wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=-100"), quiet = TRUE)
		transformed_grid <- st_transform(wrapped_grid,robinson)

		panel_style(
			ggplot(data=world[['world2']]) + geom_sf(fill = "#D3D3D3", colour = NA) +
			geom_sf(data=subset(transformed_grid, records <  MIN_N), fill = THIN,
				colour = THIN_OUTLINE, linewidth = 0.15) +
			geom_sf(data=subset(transformed_grid, records >= MIN_N), aes(fill=shown),
				color=alpha("white", 0)) +
			scale_fill_gradientn(colors = pals::kovesi.cyclic_mrybm_35_75_c68_s25(12),
				limits = c(0,365), na.value = THIN, name = "median day of year"))
	}

	save_panel(generate_tiled_map_season(world[['data']]),
	           paste0("gradient_", species_of_interest))

	n_hex <- world[['data']] %>%
		mutate(cell = dgGEO_to_SEQNUM(dggs, longitude, latitude)$seqnum) %>%
		count(cell)
	cat(sprintf("%-9s %-10s n = %5d, hexes %3d, coloured %3d at n >= %d\n",
	            TAG, species_of_interest, nrow(world[['data']]),
	            nrow(n_hex), sum(n_hex$n >= MIN_N), MIN_N))
}

lapply(species_of_interest,plot_seasonal_grid)

# Reference wheel for the cyclic day-of-year palette. One per region is
# redundant -- they are identical -- but it is written per region so each
# figure's folder is self-contained.
df <- data.frame(
  day = 0:365,
  value = 0:365
)

color_map <- ggplot(df, aes(x = day, y = 1, fill = value)) +
  geom_tile(width = 1) +
  scale_fill_gradientn(colors = pals::kovesi.cyclic_mrybm_35_75_c68_s25(12), limits = c(0,365)) +
  coord_polar(theta = "x") +
  theme_void() +
  theme(legend.position = "right")

ggplot2::ggsave(file.path(OUT, paste0("calendar_wheel_", TAG, ".png")), color_map,
                width = 3, height = 3, dpi = DPI)
