source("R_code/read.data.R")
library(circular)
circular_median_day <- function(days) {
  deg <- (days %% 365) / 365 * 360
  circ <- circular(deg, units = "degrees", modulo = "2pi")
  med_deg <- median(circ)
  round((as.numeric(med_deg) %% 360) / 360 * 365)
}
library(dggridR)

species_of_interest <- c("utriculus","minuta","megalista","physalis")
longitude_range <- c(145,180) #c(-60,40) #c(-100,0) #
latitude_range <- c(-50,-15) #c(-40,0) #c(0,60) #
LeftBound = 150 #-50 

build_world <- function(LeftBound,latitude_range,longitude_range,species_of_interest){
	data <- final_results %>% 
	mutate(yd = yday(observed_on), mon = month(observed_on)) %>% 
	filter(!is.na(longitude)) %>% 
	filter(species %in% species_of_interest) %>%
	filter(latitude >= latitude_range[1], latitude <= latitude_range[2]) %>%
	filter(longitude >= longitude_range[1], longitude <= longitude_range[2])


	world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_set_crs(4326)
	robinson = paste("+proj=robin +lon_0=",LeftBound," +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",sep="")
	world2 = world %>% st_break_antimeridian(lon_0 = LeftBound) %>% st_transform(crs = robinson)

	transinat <- st_as_sf(data ,coords=c("longitude","latitude"),crs=4326)
	itran <- st_transform(transinat,robinson)
	itran <- itran %>% mutate(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2])
 
  # --- Add this block for xlim/ylim based on the bounding box ---
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

name <- paste0(species_of_interest,"_",longitude_range[1],"_",longitude_range[2],"_",latitude_range[1],"_",latitude_range[2])
world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

month1_data <- world[['itran']] %>% filter(mon %in% c(1,2,3)) %>%  st_drop_geometry() %>% select(x, y, species) %>% slice_sample(prop = 1) 
month2_data <- world[['itran']] %>% filter(mon %in% c(4,5,6)) %>%  st_drop_geometry() %>% select(x, y, species) %>% slice_sample(prop = 1) 
month3_data <- world[['itran']] %>% filter(mon %in% c(7,8,9)) %>%  st_drop_geometry() %>% select(x, y, species) %>% slice_sample(prop = 1) 
month4_data <- world[['itran']] %>% filter(mon %in% c(10,11,12)) %>%  st_drop_geometry() %>% select(x, y, species) %>% slice_sample(prop = 1) 

month_plot <- ggplot(data=world[['world2']]) + geom_sf(fill = "light gray", colour = NA) +
  coord_sf(xlim = world[['xlims']], ylim = world[['ylims']]) + 
  theme(legend.position = "none",
  axis.title.x=element_blank(),
  axis.title.y=element_blank()) +
	scale_x_continuous(breaks = seq(-180, 180, by = 20), name = "Longitude") +
	scale_y_continuous(breaks = seq(-90, 90, by = 20), name = "Latitude")
 
month1 <-  month_plot +
  geom_point(data=month1_data,aes(x=x,y=y,color=species), size = 1.5, alpha=0.75,pch=16) + 
  scale_color_manual(values = cols) 
month2 <-  month_plot +
  geom_point(data=month2_data,aes(x=x,y=y,color=species), size = 1.5, alpha=0.75,pch=16) + 
  scale_color_manual(values = cols)
month3 <-  month_plot +
  geom_point(data=month3_data,aes(x=x,y=y,color=species), size = 1.5, alpha=0.75,pch=16) + 
  scale_color_manual(values = cols)
month4 <-  month_plot +
  geom_point(data=month4_data,aes(x=x,y=y,color=species), size = 1.5, alpha=0.75,pch=16) + 
  scale_color_manual(values = cols)

pdf(file=paste0("figures/panels/months_",name,".pdf"),width=4,height=8,useDingbats=F)
gridExtra::grid.arrange(month1, month2, month3, month4, ncol = 1)
dev.off()

print_seasonal_point <- function(species_of_interest){
	name <- paste0(species_of_interest,"_",longitude_range[1],"_",longitude_range[2],"_",latitude_range[1],"_",latitude_range[2])
	world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

	seasonal <- ggplot(data=world[['world2']]) + geom_sf(fill = "light gray", colour = NA) +
	coord_sf(xlim = world[['xlims']], ylim = world[['ylims']]) +
	geom_point(data=world[['itran']],aes(geometry=geometry,color=yd), size = 1.5, alpha=0.75,pch=16 ,stat="sf_coordinates") + 
	scale_color_gradientn(colors = pals::kovesi.cyclic_mrybm_35_75_c68_s25(12), limits = c(0,365)) + 
	theme(legend.position = "none", 
	axis.title.x=element_blank(),
	axis.title.y=element_blank()) +
		scale_x_continuous(breaks = seq(-180, 180, by = 20), name = "Longitude") +
		scale_y_continuous(breaks = seq(-90, 90, by = 20), name = "Latitude")
	

	pdf(file=paste0("figures/panels/seasonal_",name,".pdf"),width=3,height=2,useDingbats=F)
	print(seasonal)
	dev.off()
}

for (species in species_of_interest) {
	print_seasonal_point(species)
}

print_seasonal_grid <- function(species_of_interest){
	name <- paste0(species_of_interest,"_",longitude_range[1],"_",longitude_range[2],"_",latitude_range[1],"_",latitude_range[2])
	world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

	dggs <- dgconstruct(res=8)

	generate_tiled_map_count <- function(subset){
		subset$cell <- dgGEO_to_SEQNUM(dggs,subset$longitude,subset$latitude)$seqnum

		subcounts   <- subset %>% group_by(cell) %>% summarise(count=n())
		grid          <- dgcellstogrid(dggs,subcounts$cell)
		grid          <- merge(grid,subcounts,by.x="seqnum",by.y="cell")
		grid$records  <- grid$count
		grid$count    <- log(grid$count)
		cutoff        <- quantile(grid$count,0.9)
		grid          <- grid %>% mutate(count=ifelse(count>cutoff,cutoff,count))

		wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=-50"), quiet = TRUE)
		transformed_grid <- st_transform(wrapped_grid,robinson)

		tiled_map <- ggplot(data=world[['world2']]) + geom_sf(fill = "#ededed", colour = NA) + 
			geom_sf(data=transformed_grid, aes(fill=records), color=alpha("white", 0.1)) +
			scale_fill_gradient(low="dodgerblue2", high = "black", 
				trans="log",limits = c(1,50),breaks=c(1,5,10,20,40), name="count") + 
			xlab("") + ylab("")  + 
			coord_sf(xlim = world[['xlims']], ylim = world[['ylims']])
			theme(legend.position = "none", 
			axis.title.x=element_blank(),
			axis.title.y=element_blank())
			return(tiled_map)
		

	}

	tiled_count <- generate_tiled_map_count(world[['data']]) 
	pdf(file=paste0("figures/panels/seasonal_tiled_count_",name,".pdf"),width=3,height=3,useDingbats=F)
	print(tiled_count)
	dev.off()

	generate_tiled_map_season <- function(subset){
		subset$cell <- dgGEO_to_SEQNUM(dggs,subset$longitude,subset$latitude)$seqnum

		subcounts   <- subset %>% group_by(cell) %>% summarise(count=circular_median_day(yd))
		grid          <- dgcellstogrid(dggs,subcounts$cell)
		grid          <- merge(grid,subcounts,by.x="seqnum",by.y="cell")
		grid$records  <- grid$count
		grid$count    <- log(grid$count)
		cutoff        <- quantile(grid$count,0.9)
		grid          <- grid %>% mutate(count=ifelse(count>cutoff,cutoff,count))

		wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=-50"), quiet = TRUE)
		transformed_grid <- st_transform(wrapped_grid,robinson)

		tiled_map <- ggplot(data=world[['world2']]) + geom_sf(fill = "#ededed", colour = NA) + 
			geom_sf(data=transformed_grid, aes(fill=records), color=alpha("white", 0)) +
			scale_fill_gradientn(colors = pals::kovesi.cyclic_mrybm_35_75_c68_s25(12), limits = c(0,365), name = "median calendar day") + 
			xlab("") + ylab("")  + 
			coord_sf(xlim = world[['xlims']], ylim = world[['ylims']]) + 
			theme(legend.position = "none", 
			axis.title.x=element_blank(),
			axis.title.y=element_blank())
			return(tiled_map)
	}

	tiled_season <- generate_tiled_map_season(world[['data']]) 
	pdf(file=paste0("figures/panels/seasonal_tiled_season_",name,".pdf"),width=3,height=2,useDingbats=F)
	print(tiled_season)
	dev.off()
}

for (species in species_of_interest) {
	print_seasonal_grid(species)
}


