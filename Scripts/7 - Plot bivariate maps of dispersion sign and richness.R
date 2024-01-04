#### Plot bivariate maps of dispersion sign and richness ####

#See: 10.17161/bi.v16i1.14782
library(terra)
library(pbapply)
library(biscale)
library(ggplot2)
library(ggpubr)
library(mapview)
library(data.table)
library(dplyr)
library(cowplot)
library(geobr)
library(sf)
library(patchwork)
library(ggspatial)
library(tidyterra)

#Import data to plot#
lf <- list.files("Data/Dispersion_sign/", full.names = T)
lf <- lf[!grepl("Other", lf)] %>% as.character()
#Reorder lifeform
lf
lf <- lf[c(1,6,3:5,2)]
lf
#Get br map
br <- read_state()
bb_af <- c(-57.89255434, -34.82285576, -33.75435434, -2.80832455)

custom_pal <- c(
  "1-1" = "#E69F00", 
  "2-1" = "#D55E00", 
  "3-1" = "#cd2626", 
  "1-2" = "#D5D5D5", 
  "2-2" = "#858F94", 
  "3-2" = "#4D5662", 
  "1-3" = "#DBE8B4", 
  "2-3"= "#8DC967", 
  "3-3" = "#228B22")

#Initiate looping
  #Test looping
# i <- 2

maps <- pblapply(seq_along(lf), function(i){
  lf_i <- lf[i]
  #Get lifeform name
  lf_name <- gsub("Data/Dispersion_sign/|\\.RDS", "", lf_i)
  print(lf_name)
  #Get label to plot
  my_label <- paste0("(", letters[i], ") ", lf_name)
  
  #Read data with dispersion sign and richness
  disp_sign <- readRDS(lf_i)
  disp_sign$DispersionSign <- as.factor(disp_sign$DispersionSign)
  
  #Create breaks manually
  break_vals <- list(bi_x = quantile(disp_sign$Richness,
                                      probs = c(0, 0.25, 0.5, 0.75, 1))[c(2,4)] %>% 
                       as.numeric(),
                     bi_y = c(1, 0, -1))
  
  
  #Create breaks dataframe Manually
  df.bi <- disp_sign %>%
    #Create class of Richness
    mutate(by_x = ifelse(Richness <= break_vals$bi_x[1], 1, ifelse(
      Richness > break_vals$bi_x[1] & Richness <= break_vals$bi_x[2], 2, ifelse(
        Richness > break_vals$bi_x[2], 3, NA)))) %>% 
    #Create class of dispersion sign (Endemism: - 1 is higher endemism, 1 is lower endemism)
    mutate(by_y = ifelse(DispersionSign == -1, 3, ifelse( #High endemism
      DispersionSign == 0, 2, ifelse( #Random
        DispersionSign == 1, 1, NA #Low endemism
      )
    ))) %>% 
    #Create columns with class
    mutate(bi_class = paste0(by_x, "-", by_y))
  
  #Get beta-diversity
  ind_i <- readRDS(paste0("Data/PAM_indices/Indices_", lf_name, ".rds"))
  betadiv <- round(ind_i$One_value_indices["Beta_Whittaker",], 2)
  #Get number of species
  S_tot <- ind_i$One_value_indices["Species",]
  
  #Bivariate Raster Mapping
  map <- ggplot() +
    geom_raster(data = df.bi , aes(x = x, y = y, fill = bi_class)) +
    bi_scale_fill(pal = custom_pal, dim = 3) +
    geom_sf(data = br, fill = NA, size = 0.1, colour = "grey40") +
    theme_bw() +
    theme(text = element_text(size = 10, colour = "black")) +
    borders(colour = "black", size = 0.5) +
    coord_sf(xlim = c(bb_af[1] - 0.5, xmax=bb_af[3] + 0.25),
             ylim = c(bb_af[2] - 0.5, ymax=bb_af[4] + 0.5),
             expand = T) +
    annotate("text", x = -48, y = -30,
             label = paste("??-diversity =",betadiv), hjust = 0) +
    annotate("text", x = -48, y = -27,
             label = paste("Species total =",S_tot), hjust = 0) +
    theme(legend.position = "none",
          plot.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 9.5, colour = "black"),
          axis.title = element_text(size = 9.5, colour = "black"),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    labs(x = "Longitude",
         y = "Latitude") +
    geom_hline(yintercept = -19, color = "black", linetype="dashed") +
    annotate("text", x = -33, y=-1.5, label = my_label, size = unit(4.4, "pt"),
             hjust = 1) +
    annotation_scale(pad_x = unit(2.5, "cm"), plot_unit = "km")
  
    return(map)
})
names(maps) <- gsub("Data/Dispersion_sign/|\\.RDS", "", lf)

#Get legend
break_vals2 <- list()
break_vals2$bi_y <- c("Low", "Random\n(Non-significant)", "High")
break_vals2$bi_x <- c("Low\n(Q1)", "Medium\n(Q2-Q3)", "High\n(Q4)")
legend <- bi_legend(pal = custom_pal,
                      xlab = "Richness",
                      ylab = "Rarity",
                      size = 12,
                      dim = 3,
                      breaks = break_vals2) +
#Deixar background transparente
theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA),
          plot.background = element_rect(fill = "transparent",
                                         colour = NA))

#Arrange plot
p <- (maps$All + maps$Tree + maps$Liana) / (maps$Shrub + maps$Subshrub + maps$Herb) / 
  (plot_spacer() + legend + plot_spacer()) +
  plot_layout(widths = c(2, 2, 1.4), heights = c(2, 2, 1.4))
p
#Save
ggsave("Data/Figures/Dispersion_maps3.png",
       p, dpi = 600, units = "px", width = 2500,
       height = 1700, scale = 4.5)


# ####Representations of diversity and dispersion indices####
# ####Plot richness vs dispersion field####
# # ds_legeng <- factor(df.bi$DispersionSign,
# #                     levels = c(-1, 0, 1),
# #                     labels = c("Below 5%", "Non-significant", "Above 95%"))
# 
# g1 <- ggplot(df.bi, aes(x = NormalizedRichness,
#                         y = DispersedFieldNormalized,
#                         colour = bi_class)) +
#   geom_point() + 
#   scale_colour_manual(name = "Significance",
#                       values = custom_pal) +
#   theme_bw() +
#   xlab("Normalized Richness") + ylab("Normalized Dispersion Field/S") +
#   theme(legend.position = "none")
# 
# # g1
# g2 <- map + g1/legend + plot_layout(widths = c(4, 3, 1)) +
#   plot_annotation(title = lf_i) & 
#   theme(plot.title = element_text(hjust = 0.5, face = "bold"))
# # g2
# ggsave(paste0("PAM_indices/Dispersion_maps/", lf_i, ".png"), g2, width = 14, height = 8, dpi = 300)
# })
