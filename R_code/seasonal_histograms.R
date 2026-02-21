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
# for(i in 1:5){source("R_code/seasonal_histograms.R")}

species_of_interest <- c("utriculus","physalis","megalista","minuta")
longitude_range <- ranges[[i]][c(1,2)] 
latitude_range <- ranges[[i]][c(3,4)] 
LeftBound = (longitude_range[2] + longitude_range[1]) / 2 

name <- paste0(longitude_range[1],"_",longitude_range[2],"_",latitude_range[1],"_",latitude_range[2])
world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

month1_data <- world[['itran']] %>% filter(mon %in% c(1,2,3)) %>%  st_drop_geometry() %>% select(x, y, species) %>% add_count(species) %>% arrange((species))
month2_data <- world[['itran']] %>% filter(mon %in% c(4,5,6)) %>%  st_drop_geometry() %>% select(x, y, species)  %>% add_count(species) %>% arrange((species))
month3_data <- world[['itran']] %>% filter(mon %in% c(7,8,9)) %>%  st_drop_geometry() %>% select(x, y, species)  %>% add_count(species) %>% arrange((species))
month4_data <- world[['itran']] %>% filter(mon %in% c(10,11,12)) %>%  st_drop_geometry() %>% select(x, y, species)  %>% add_count(species) %>% arrange((species))

month_plot <- ggplot(data=world[['world2']]) + geom_sf(fill = "#D3D3D3", colour = NA) +
  	coord_sf(xlim = world[['xlims']], ylim = world[['ylims']]) + 
  	theme(legend.position = "none",
  	axis.title.x=element_blank(),
  	axis.title.y=element_blank(),
		text = element_text(size = 8)) +
	scale_x_continuous(breaks = seq(-180, 180, by = 20)) +
	scale_y_continuous(breaks = seq(-90, 90, by = 20))
 
month1 <-  month_plot +
  geom_point(data=month1_data,aes(x=x,y=y,color=species), size = 0.75, alpha=0.5,pch=16) + 
  scale_color_manual(values = cols) 
month2 <-  month_plot +
  geom_point(data=month2_data,aes(x=x,y=y,color=species), size = 0.75, alpha=0.5,pch=16) + 
  scale_color_manual(values = cols)
month3 <-  month_plot +
  geom_point(data=month3_data,aes(x=x,y=y,color=species), size = 0.75, alpha=0.5,pch=16) + 
  scale_color_manual(values = cols)
month4 <-  month_plot +
  geom_point(data=month4_data,aes(x=x,y=y,color=species), size = 0.75, alpha=0.5,pch=16) + 
  scale_color_manual(values = cols)

pdf(file=paste0("figures/panels/months_",name,".pdf"),width=3,height=3,useDingbats=F)
gridExtra::grid.arrange(month1, month2, month3, month4, ncol = 2)
dev.off()

facet_hist_stacked <- ggplot(world[["data"]], aes(x = yd, fill = species)) +
	geom_histogram(binwidth = 7, alpha = 1) +
	scale_fill_manual(values = cols) +
	facet_wrap(~ species, ncol = 1, scales = "free_y") +
	ylab("") + xlab("day of year") + 
    scale_y_continuous(breaks = conditional_breaks) +
	theme(strip.text = element_text(size = 0),
		text = element_text(size = 8),
		legend.position="none")

pdf(file = paste0("figures/panels/facet_hist_", name, ".pdf"), width = 4, height = 3, useDingbats = FALSE)
print(facet_hist_stacked)
dev.off()

print_yearly_plot <- function(species_of_interest){
	ydata <- world[["data"]] %>% filter(species == species_of_interest) %>% filter(year > 2014, year < 2025)

	if (nrow(ydata) == 0) {
		return(invisible(NULL))
	}

	yearly_hist <- ggplot(ydata, aes(x = yd, fill = species)) +
	geom_histogram(binwidth = 7, alpha = 1) +
	scale_fill_manual(values = cols) +
	facet_grid(rows = vars(year), switch = "y") + 
	ylab("") + 
	xlab("day of year") + 
	scale_y_continuous(breaks = conditional_breaks) +
	theme(
		legend.position = "none",
		strip.placement = "outside",   # put them outside y-axis
		strip.background = element_blank(),
        panel.spacing.y = unit(0.5, "lines")
	)

	pdf(file = paste0("figures/panels/yearly_hist_", species_of_interest,"_",name,".pdf"), width = 7, height = 7, useDingbats = FALSE)
	print(yearly_hist)
	dev.off()
}

lapply(species_of_interest, print_yearly_plot)
