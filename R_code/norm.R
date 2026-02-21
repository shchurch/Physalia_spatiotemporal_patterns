source("R_code/read.data.R")
library(mgcv)

set.seed(10012)

ranges <- list(
  c(-60, -20, -40, 0), 
  c(-100, -60, 0, 40), 
  c(5, 45, -40, 0),
  c(100, 140, -50, -10),
  c(140, 180, -50,-10)
)

#i <- 1
# for(i in 1:5){source("R_code/norm.R")}

species_of_interest <- c("utriculus","physalis","megalista","minuta")
longitude_range <- ranges[[i]][c(1,2)] 
latitude_range <- ranges[[i]][c(3,4)] 
LeftBound = (longitude_range[2] + longitude_range[1]) / 2 

name <- paste0(longitude_range[1],"_",longitude_range[2],"_",latitude_range[1],"_",latitude_range[2])
world <- build_world(LeftBound,latitude_range,longitude_range,species_of_interest)

ndata <- norm_results %>%
	filter(latitude >= latitude_range[1], latitude <= latitude_range[2]) %>%
	filter(longitude >= longitude_range[1], longitude <= longitude_range[2])

plot_norm_hist <- function(species_of_interest){
	ydata <- world[["data"]] %>% filter(species == species_of_interest)
	
	if (nrow(ydata) == 0) {
		return(invisible(NULL))
	}
	
	daily_counts <- ndata %>%
		count(yd)
	
	model <- gam(n ~ s(yd, bs = "cc", k = 20), data = daily_counts, family = poisson())

	effort_pred <- predict(model, newdata = ydata, type = "response")
	ydata$norm_weight <- 1 / effort_pred

	ydata$daily_weight <- 1 / daily_counts$n[match(ydata$yd, daily_counts$yd)]
	ydata$daily_weight[is.na(ydata$daily_weight)] <- 1

	ydata_plot <- ydata %>%
		mutate(unweighted = 1,  
					seasonally = norm_weight,
					daily = daily_weight) %>%
		tidyr::pivot_longer(cols = c(unweighted, seasonally, daily), names_to = "type", values_to = "weight")

	baseline_plot <- tidyr::expand_grid(
		daily_counts,
		species = "beach_sp") %>%
		mutate(type = "baseline",
					weight = n) %>%
		select(yd, species, type, weight)

	combined_plot_data <- bind_rows(ydata_plot, baseline_plot)

	norm_plot <- ggplot(combined_plot_data, aes(x = yd, fill = species, weight = weight)) +
		geom_histogram(binwidth = 7) +
		scale_fill_manual(values = cols) + 
		facet_wrap(~ factor(type,levels=c("baseline","unweighted","daily","seasonally")), ncol = 1, scales = "free_y", 
							strip.position = "left") +
		xlab("day of year") + ylab("") +
		theme(strip.placement = "outside",
			legend.position = "none")
		
	pdf(file = paste0("figures/panels/norm_hist_", species_of_interest,"_",name,".pdf"), width = 7, height = 4, useDingbats = FALSE)
	print(norm_plot)
	dev.off()
}

lapply(species_of_interest,plot_norm_hist)
