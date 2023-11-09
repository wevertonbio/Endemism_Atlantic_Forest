#### Plot hotspots ####

library(ggplot2)
library(geobr)
library(tidyterra)
library(ggnewscale)
library(patchwork)
library(terra)
library(tidyr)

#Import Brasil map
br <- geobr::read_state() %>% vect()

# #Import Atlantic Forest map
# af <- vect("https://github.com/wevertonbio/Get_and_Filter_Points/raw/main/Vectors/AF_dissolved..gpkg")

###Load lifeforms####
lf <- list.files("Data/Priorization/Priority_areas/", pattern = ".tiff",
                 full.names = F, recursive = F) %>% 
  gsub("\\.tiff", "", .)
#Get urban areas and protected areas
ua <- rast("Data/Priorization/Variables/Urban_areas.tiff")
#Convert to dataframe
ua_df <- as.data.frame(ua, xy = TRUE, na.rm = TRUE)
#Get protected areas
pa <- vect("Data/Vectors/Protected_areas_AF_WDPA.gpkg")
plot(pa)

#Initiale looping
#Test looping
i = lf[6]
data <- pblapply(lf, function(i){
  #Import hotspots
  r <- paste0("Data/Priorization/Priority_areas/", i, ".tiff") %>%
    rast()
  r[[1]][r[[1]] > 0] <- 1
  r[[2]][r[[2]] > 0] <- 1
  #set limits
  bb_af <- ext(r)

  #Convert to dataframe
  d <- as.data.frame(r, xy = TRUE, na.rm = T)
  #Gather values and Add lifeform
  df <- d %>%
      pivot_longer(cols = starts_with("Equal") | starts_with("Restauration"),
                 names_to = "Cost", values_to = "Values") %>% 
    mutate(lifeform = i, .before = 1) %>% 
    filter(Values > 0)    #Remove 0 values
  return(df)
})
data <- bind_rows(data)
head(data)
#Map
map1 <- ggplot() +
    borders(colour = "black", size = 0.5, fill = "#FEE9B6") +
    geom_tile(data = data, aes(x = x, y = y, fill = "Values")) +
    geom_tile(data = ua_df, aes(x = x, y = y, fill = "Urban_areas")) +
    borders(colour = "black", size = 0.5, fill = NA) +
    geom_sf(data = pa, linewidth = 0.1, alpha = 0, aes(colour = "PA")) +
    geom_sf(data = br, fill = NA, size = 0.1, colour = "grey30") +
    geom_sf(data = af, fill = NA, linewidth = 0.6, colour = "grey10") +
    scale_fill_manual(values = c("Values" = "#f03b20",
                                 "Urban_areas" = "grey50"),
                      labels = c("Urban areas", "Hotspots")) +
    scale_colour_manual(values = c("PA" = "#238B45"),
                        labels = "Protected areas") +
    coord_sf(xlim = c(bb_af[1] - 0.5, xmax=bb_af[3] + 0.25),
             ylim = c(bb_af[2] - 0.5, ymax=bb_af[4] + 0.5),
             expand = T) + 
    xlab("Longitude") + ylab("Latitude") +
    metR::scale_x_longitude(ticks = 5) + metR::scale_y_latitude(ticks = 5) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title=element_blank(),
          text = element_text(family = "Arial"),
          legend.text = element_text(size = 10),
          legend.key.size = unit(0.75, 'cm'), #change legend key size
          legend.key.height = unit(0.87, 'cm'), #change legend key height
          legend.key.width = unit(1.3, 'cm'),
          legend.box = "horizontal",
          legend.background = element_rect(fill = "white", size = 0.5, colour = "black"),
          panel.background = element_rect(fill = 'aliceblue', colour = NA),
          panel.border = element_rect(colour = "black", size = 2, fill = NA)) +
    facet_wrap(vars(lifeform), vars(Cost))
map1
#Save
ggsave("Data/Figures/Hotspots.png",
         map1, dpi = 600, units = "px", width = 2500,
         height = 2500, scale = 5)
