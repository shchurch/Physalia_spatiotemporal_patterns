setwd("~/Downloads/Physalia_spatiotemporal/")
source("R_code/read.data.R")

utr <- final_results %>% filter(species == "utriculus")
meg <- final_results %>% filter(species == "megalista")
phy <- final_results %>% filter(species == "physalis")
min <- final_results %>% filter(species == "minuta")

utr_natl <- utr %>%
  filter(
    longitude > -100, longitude < 20,
    latitude > 15, latitude < 70
  )

utr_nz <- utr %>%
  filter(
    latitude > -49, latitude < -33,
    longitude > 165, longitude < 180
  )

meg_north <- meg %>%
  filter(latitude > -15)

phy_oceans <- phy %>%
  filter(
    (longitude > 30 & longitude < 180) |  # Indian & Pacific
    (longitude < -100 & latitude > 0) | # NE Pacific
    (longitude < -60 & latitude < 0)  # SE Pacific
  )

min_filtered <- min %>%
  filter(
    latitude > -15 | 
    longitude < 110 | 
    longitude > 180
  )
LeftBound = -90
world <- ne_countries(scale = "medium", returnclass = "sf") %>% st_set_crs(4326)
robinson = paste("+proj=robin +lon_0=",LeftBound," +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",sep="")
world2 = world %>% st_break_antimeridian(lon_0 = LeftBound) %>% st_transform(crs = robinson)

plot_map <- function(data, title) {

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

plot_map(utr_natl, "utr_natl")
write.table(utr_natl %>% select(id,image_url,place_guess,status), "utr_natl.tsv", sep="\t", row.names=F, quote=F)
plot_map(utr_nz, "utr_nz")
write.table(utr_nz %>% select(id,image_url,place_guess,status), "utr_nz.tsv", sep="\t", row.names=F, quote=F)
plot_map(meg_north, "meg_north")
write.table(meg_north %>% select(id,image_url,place_guess,status), "meg_north.tsv", sep="\t", row.names=F, quote=F)
plot_map(phy_oceans, "phy_oceans")
write.table(phy_oceans %>% select(id,image_url,place_guess,status), "phy_oceans.tsv", sep="\t", row.names=F, quote=F)
plot_map(min_filtered, "min_north")
write.table(min_filtered %>% select(id,image_url,place_guess,status), "min_north.tsv", sep="\t", row.names=F, quote=F)

cols <- c("purple","dodgerblue","dark orange","dark cyan")
names(cols) <- c("Physalia megalista","Physalia minuta","Physalia utriculus","Physalia physalis")


plot_map(final_results %>% mutate(species = scientific_name) %>% filter(!is.na(longitude)), "all")

