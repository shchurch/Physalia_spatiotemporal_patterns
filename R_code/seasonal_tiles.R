source("R_code/read.data.R")

set.seed(10012)

ranges <- list(
  c(-60, -20, -40, 0), 
  c(-100, -60, 0, 40), 
  c(5, 45, -40, 0),
  c(100, 140, -50, -10),
  c(140, 180, -50,-10)
)

#i <- 1
# for(i in 1:5){source("R_code/seasonal_tiles.R")}

species_of_interest <- c("utriculus","physalis","megalista","minuta")
longitude_range <- ranges[[i]][c(1,2)] 
latitude_range <- ranges[[i]][c(3,4)] 
LeftBound = (longitude_range[2] + longitude_range[1]) / 2 

name <- paste0(longitude_range[1],"_",longitude_range[2],"_",latitude_range[1],"_",latitude_range[2])
world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

plot_seasonal_point <- function(species_of_interest){
	world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

	if (nrow(world[['data']]) == 0) {
		return(invisible(NULL))
	}

	seasonal <- ggplot(data=world[['world2']]) + geom_sf(fill = "#D3D3D3", colour = NA) +
	coord_sf(xlim = world[['xlims']], ylim = world[['ylims']]) +
	geom_point(data=world[['itran']],aes(geometry=geometry,color=yd), size = 1.5, alpha=0.75,pch=16 ,stat="sf_coordinates") + 
	scale_color_gradientn(colors = pals::kovesi.cyclic_mrybm_35_75_c68_s25(12), limits = c(0,365)) + 
	theme(legend.position = "none", 
		axis.title.x=element_blank(),
		axis.title.y=element_blank(),
  	text = element_text(size = 8)) +
	scale_x_continuous(breaks = seq(-180, 180, by = 20)) +
	scale_y_continuous(breaks = seq(-90, 90, by = 20))
	

	pdf(file=paste0("figures/panels/seasonal_",species_of_interest,"_",name,".pdf"),width=3,height=2,useDingbats=F)
	print(seasonal)
	dev.off()
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

		subcounts   <- subset %>% group_by(cell) %>% summarise(count=n())
		grid          <- dgcellstogrid(dggs,subcounts$cell)
		grid          <- merge(grid,subcounts,by.x="seqnum",by.y="cell")
		grid$records  <- grid$count
		grid$count    <- log(grid$count)
		cutoff        <- quantile(grid$count,0.9)
		grid          <- grid %>% mutate(count=ifelse(count>cutoff,cutoff,count))

		wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=-50"), quiet = TRUE)
		transformed_grid <- st_transform(wrapped_grid,robinson)

		tiled_map <- ggplot(data=world[['world2']]) + geom_sf(fill = "#D3D3D3", colour = NA) + 
			geom_sf(data=transformed_grid, aes(fill=records), color=alpha("white", 0.1)) +
			scale_fill_gradient(low="dodgerblue2", high = "black", 
				trans="log",limits = c(1,50),breaks=c(1,5,10,20,40), name="count") + 
			xlab("") + ylab("")  + 
			coord_sf(xlim = world[['xlims']], ylim = world[['ylims']]) + 
			theme(legend.position = "none", 
				axis.title.x=element_blank(),
				axis.title.y=element_blank(),
				text = element_text(size = 8)) +
			scale_x_continuous(breaks = seq(-180, 180, by = 20)) +
			scale_y_continuous(breaks = seq(-90, 90, by = 20))
		
		return(tiled_map)
		

	}

	tiled_count <- generate_tiled_map_count(world[['data']]) 
	pdf(file=paste0("figures/panels/seasonal_tiled_count_",species_of_interest,"_",name,".pdf"),width=3,height=3,useDingbats=F)
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

		wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=-100"), quiet = TRUE)
		transformed_grid <- st_transform(wrapped_grid,robinson)

		tiled_map <- ggplot(data=world[['world2']]) + geom_sf(fill = "#D3D3D3", colour = NA) + 
			geom_sf(data=transformed_grid, aes(fill=records), color=alpha("white", 0)) +
			scale_fill_gradientn(colors = pals::kovesi.cyclic_mrybm_35_75_c68_s25(12), limits = c(0,365), name = "median day of year") + 
			xlab("") + ylab("")  + 
			coord_sf(xlim = world[['xlims']], ylim = world[['ylims']]) + 
			theme(legend.position = "none", 
				axis.title.x=element_blank(),
				axis.title.y=element_blank(),
				text = element_text(size = 8)) +
			scale_x_continuous(breaks = seq(-180, 180, by = 20)) +
			scale_y_continuous(breaks = seq(-90, 90, by = 20))
		
		return(tiled_map)
	}

	tiled_season <- generate_tiled_map_season(world[['data']]) 
	pdf(file=paste0("figures/panels/seasonal_tiled_",species_of_interest,"_",name,".pdf"),width=3,height=2,useDingbats=F)
	print(tiled_season)
	dev.off()
}

lapply(species_of_interest,plot_seasonal_grid)

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

pdf(file = paste0("figures/panels/polar_calendar_", name, ".pdf"), width = 3, height = 3, useDingbats = FALSE)
print(color_map)
dev.off()	
