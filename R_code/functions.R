sf_use_s2(FALSE)

conditional_breaks <- function(limit) {
  # The `limit` argument is the y-axis range for a single facet, e.g., c(0, 1.05)
  # If the max value for a facet's y-axis is less than 2 (i.e., the max count is 1),
  # then set specific breaks. ggplot expands the axis slightly, so we check for < 2.
  if (max(limit) < 2) {
    return(c(0, 1, 2))
  } else {
    # Otherwise, use a standard "pretty" breaks algorithm.
    # We use n=4 to suggest 4 break points, but ggplot will adjust as needed.
    return(scales::pretty_breaks(n = 3)(limit))
  }
}

circular_median_day <- function(days) {
  deg <- (days %% 365) / 365 * 360
  circ <- circular(deg, units = "degrees", modulo = "2pi")
  med_deg <- median(circ)
  round((as.numeric(med_deg) %% 360) / 360 * 365)
}

build_world <- function(LeftBound,latitude_range,longitude_range,species_of_interest) {
	data <- final_results %>%
	filter(species %in% species_of_interest) %>%
	filter(latitude >= latitude_range[1], latitude <= latitude_range[2]) %>%
	filter(longitude >= longitude_range[1], longitude <= longitude_range[2])

	world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_set_crs(4326)
	robinson = paste("+proj=robin +lon_0=",LeftBound," +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",sep="")
	world2 = world %>% st_break_antimeridian(lon_0 = LeftBound) %>% st_transform(crs = robinson)

	transinat <- st_as_sf(data ,coords=c("longitude","latitude"),crs=4326)
	itran <- st_transform(transinat,robinson)
	itran <- itran %>% mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2])
 
  bbox_ll <- st_bbox(c(
    xmin = longitude_range[1],
    xmax = longitude_range[2],
    ymin = latitude_range[1],
    ymax = latitude_range[2]
  ), crs = st_crs(4326)) %>% st_as_sfc()

  bbox_proj <- st_transform(bbox_ll, crs = robinson)
  bbox_coords <- st_bbox(bbox_proj)

  xlims <- c(bbox_coords$xmin, bbox_coords$xmax)
  ylims <- c(bbox_coords$ymin, bbox_coords$ymax)

	return(list(data = data, world2 = world2, itran = itran, xlims = xlims, ylims = ylims))
}
