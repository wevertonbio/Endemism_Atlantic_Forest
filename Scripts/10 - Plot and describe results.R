#### Plot and describe results ####

#Load packages
library(dplyr)
library(ggplot2)
library(pbapply)
library(tidyr)
library(scales)
library(terra)

####PROTECTED AREAS####

#Import data of protected area
m <- readRDS("Data/Metrics/ProtectionSignificance.RDS")
lf <- names(m)

#% of hotspot protected 
# #test iteration
# i <- "Tree"
  #Extract data
d1 <- pblapply(lf, function(i){
  d_i <- m[[i]] %>% 
  #Create new columns indicating area protected and non protected
  mutate(Protected = round(Area_km * (Portion_protected/100)),
         Non_protected = round(Area_km - Protected)) %>% 
  #Reshape dataframe
    pivot_longer(cols = c(Protected, Non_protected),
                 names_to = "Status",
                 values_to = "Value")
  }) %>% bind_rows()

#Fix factors
unique(d1$lifeform) %>% dput()
d1$lifeform <- factor(d1$lifeform, levels = c("All", "Tree", "Liana", "Shrub",
                                              "Subshrub", "Herb"))
d1$Hotspot <- factor(d1$Hotspot, levels = c("Rich_rare", "Poor_rare"),
                    labels = c("Hotspots", "Coldspots"))
d1$Status <- factor(d1$Status, levels = c("Protected", "Non_protected"),
                    labels = c("Protected", "Non-protected"))



#Plot
g_pa <- ggplot(d1, aes(x = lifeform, y = Value, fill=Status,
                       label = paste0(round(Value/1000, 0), "K"))) +
  geom_bar(stat = 'identity', color = "black") +
  # geom_text(position = position_stack(vjust = 0.5), size = 3, angle = 90) +
  geom_text(aes(label = Protection_sig, color = Protection_sig),
            position = position_stack(), size = 4, hjust = 0.5, vjust = -2.25,
            alpha = rep(c(0, 1), 12),
            fontface = "bold") +
  scale_color_manual(values = c("darkgreen", "gray", "firebrick"), guide = "none") +
  scale_fill_manual(values=c('darkgreen', "gray"),
                    guide = guide_legend(reverse = TRUE)) +
  ylab(bquote('Area '(km^2))) +
  xlab("Life form") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                     limits = c(0, 600000)) +
  facet_wrap(Hotspot ~ ., nrow = 2) +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16))
g_pa
ggsave("Data/Figures/Protection.png", g_pa,
       units = "px",
       dpi = 600, width = 2500,
       height = 2300, scale = 2)

####SIGNIFICANCE OF EFFICIENCY OF PROTECTED AREAS - HISTOGRAM####
#Load data
ps <- readRDS("Data/Metrics/ProtectionSignificance.RDS")
random_protection <- readRDS("Data/Metrics/Random_protection.RDS")
#Get lifeforms
lf_names <- names(random_protection)
#Convert to dataframe and longer format
ps_df <- pblapply(ps, function(x) as.data.frame(x)) %>% bind_rows()
rp_df <- pblapply(lf_names, function(x) {
  as.data.frame(random_protection[[x]]) %>% mutate(lifeform = x, .before = 1)
    }) %>% bind_rows()
#Change colnames
colnames(rp_df) <-c("lifeform", "Rich_rare" , "Poor_rare")
  #Longer format
rp_df <- pivot_longer(
  rp_df,
  cols = c(Rich_rare, Poor_rare),
  names_to = "Hotspot",
  values_to = "Value"
)
#Convert to %
rp_df$Value <- rp_df$Value * 100

#Get quartis
rp_df <- rp_df %>% group_by(lifeform, Hotspot) %>%
  mutate(quartil_2.5 = quantile(Value, probs = 0.025),
         quartil_97.5 = quantile(Value, probs = 0.975))

#Organize factors
rp_df$lifeform <- factor(rp_df$lifeform, levels = c("All", "Tree", "Liana", "Shrub",
                                              "Subshrub", "Herb"))
rp_df$Hotspot <- factor(rp_df$Hotspot, levels = c("Rich_rare", "Poor_rare"),
                     labels = c("Hotspots", "Coldspots"))

ps_df$lifeform <- factor(ps_df$lifeform, levels = c("All", "Tree", "Liana", "Shrub",
                                                    "Subshrub", "Herb"))
ps_df$Hotspot <- factor(ps_df$Hotspot, levels = c("Rich_rare", "Poor_rare"),
                        labels = c("Hotspots", "Coldspots"))
#plot
gh <- ggplot(data = rp_df, aes(Value)) +
   geom_histogram(bins = 50, col = "black", fill = "gray", center = 0.1) +
   geom_vline(aes(xintercept = quartil_2.5), col = "red", linewidth = 1.25) +
   geom_vline(aes(xintercept = quartil_97.5), col = "darkgreen", linewidth = 1.25) +
   facet_wrap(Hotspot ~ lifeform, scales = "free") +
  ylab("Frequency") + xlab("% Protected") +
  ggpubr::theme_pubclean()
gh
gh <- gh + geom_vline(data = ps_df, aes(xintercept = Portion_protected),
                      col = "yellow", linewidth = 1.25) +
  facet_wrap(Hotspot ~ lifeform, scales = "free", ncol = 3) +
  theme(strip.text = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16))
gh
ggsave("Data/Figures/Histogram_Random_Protection.png", gh,
       units = "px",
       dpi = 600, width = 2500,
       height = 2000, scale = 4)


#### LAND USE AND FRAGMENTS ####
#Import data with metrics
m <- readRDS("Data/Metrics/Metrics.rds")
lf <- names(m)

#Test looping
#i <- "Tree"

#Land use
data_luc <- pblapply(lf, function(i){
  luc_i <- m[[i]]$LUC_data
  #Calculate area in km2 using portion
  pa_i <- m[[i]]$PA_data %>% dplyr::select(lifeform, layer = Hotspot, Area_km)
  #Join data and calculate area
  luc_i <- left_join(luc_i, pa_i) %>% mutate(Area_luc = Area_km * (portion/100))
  return(luc_i)
}) %>% bind_rows() %>% 
  #Remove classes NA from AF in other countries
  na.omit()

#Plot
head(data_luc)
#Define factors
data_luc$layer <- factor(data_luc$layer, levels = c("Rich_rare", "Poor_rare"),
                         labels = c("Hotspots", "Coldspots"))
data_luc$LUC <- factor(data_luc$LUC,
                       levels = c("Natural\nforest", "Other natural\nvegetation", "Forest\nplantation", 
                                  "Pasture", "Temporary\ncrop", "Perennial\ncrop", "Urban\narea", 
                                  "Mining", "Water"))
data_luc$lifeform <- factor(data_luc$lifeform, levels = c("All", "Tree", "Liana", "Shrub",
                                                  "Subshrub", "Herb"))
#Create colors (based on Mapbiomas
#https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/EN__Codigos_da_legenda_Colecao_7.pdf
my_c <- c("#129912", "#bbfcac", "#935132", "#ffd966", "#660066", "#f3b4f1",
          "#af2a2a", "#8a2be2", "#0000ff")

g_luc <- ggplot(data_luc, aes(x = lifeform, y = Area_luc, fill=LUC, group = LUC)) +
  geom_bar(stat = 'identity', position = "stack", color = "black") +
  # scale_fill_manual(values=c('gray', 'darkgreen'),
  #                   guide = guide_legend(reverse = TRUE)) +
  # coord_flip() +
  ylab(bquote('Area '(km^2))) +
  xlab("Life form") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = my_c, name = "Land Use Cover") +
  facet_grid(.~layer) +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16))
g_luc
# ggsave("Data/Figures/LUC.png", g_luc,
#        units = "px",
#        dpi = 600, width = 2500,
#        height = 1500, scale = 3)

####FRAGMENT SIZE####
data_f <- pblapply(lf, function(i){
  f_i <- m[[i]]$Frag_data %>% 
  #Calculate area in km2 (1 pixel = 0.0009km2)
  mutate(Area_km = count * 0.0009) %>% 
  #Calculare area in ha (1km = 100ha)
  mutate(Area_ha = Area_km * 100)
  return(f_i)
}) %>% bind_rows()
quantile(data_f$Area_ha)

#Classify fragments by size
data_f <- data_f %>% 
  mutate(Size = ifelse(Area_ha < 1, "Micro", ifelse(
      between(Area_ha, 1, 50), "Small", ifelse(
        between(Area_ha, 50, 250), "Medium", ifelse(
          Area_ha > 250, "Large", NA
        ))
    )
  ))
table(data_f$Size)
#Count data by group
df_f <- data_f %>% group_by(lifeform, layer, Size) %>% 
  summarise(count = n())

#Get area of hotspot to get number of fragments/area
df_area <- data_luc %>% dplyr::select(layer, lifeform, Area_km) %>% distinct()
df_area$layer <- as.character(df_area$layer)
df_area$layer[which(df_area$layer == "Hotspots")] <- "Rich_rare"
df_area$layer[which(df_area$layer == "Coldspots")] <- "Poor_rare"

df_f <- left_join(df_f, df_area)

#Define factors
df_f$layer <- factor(df_f$layer, levels = c("Rich_rare", "Poor_rare"),
                         labels = c("Hotspots", "Coldspots"))
df_f$lifeform <- factor(df_f$lifeform, levels = c("All", "Tree", "Liana", "Shrub",
                                                          "Subshrub", "Herb"))
df_f$Size <- factor(df_f$Size, levels = c("Large", "Medium", "Small", "Micro"),
                    labels = c("Large (>250ha)", "Medium (50-250ha)",
                               "Small (1-50ha)", "Micro (<1ha)"))



#Plot
g_f <- ggplot(df_f, aes(x = lifeform, y = count, fill=Size, group = Size)) +
  geom_bar(stat = 'identity', position = "stack", color = "black") +
  # scale_fill_manual(values=c('gray', 'darkgreen'),
  #                   guide = guide_legend(reverse = TRUE)) +
  # coord_flip() +
  ylab("Number of fragments") +
  xlab("Life form") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = c("firebrick", "#FDAE61", "yellow2", "#1A9641"),
                    breaks =  c("Micro (<1ha)", "Small (1-50ha)", "Medium (50-250ha)",
                                "Large (>250ha)"),
                    name = "Fragment size") +
  facet_grid(.~layer) +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 16))
g_f
# ggsave("Data/Figures/Fragment.png", g_f,
#        units = "px",
#        dpi = 600, width = 2500,
#        height = 1000, scale = 3)

#Join land use and fragment maps
luc_f <- cowplot::plot_grid(g_luc, g_f, nrow = 2, labels = c("(a)", "(b)"))
luc_f
ggsave("Data/Figures/LUC_and_Fragment.png", luc_f,
       units = "px",
       dpi = 600, width = 2800,
       height = 2600, scale = 2.4)
####RANGE-DIVERSITY PLOTS####
#Import data to plot#
lf <- list.files("Data/Dispersion_sign/", full.names = T)
lf <- lf[!grepl("Other", lf)] %>% as.character()
#Reorder lifeform
lf
lf <- lf[c(1,6,3:5,2)]
lf

data <- pblapply(seq_along(lf), function(i){
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
    mutate(bi_class = paste0(by_x, "-", by_y)) %>% 
    mutate(lifeform = lf_name)
  return(df.bi)
}) %>% bind_rows()

#RDP Plot
#Colors
custom_pal <- c(
  "1-1" = "#DBE8B4", 
  "2-1" = "#8DC967", 
  "3-1" = "#228B22",
  "1-2" = "#D5D5D5", 
  "2-2" = "#858F94", 
  "3-2" = "#4D5662", 
  "1-3" = "#56B4E9",
  "2-3"= "#0072B2", 
  "3-3" = "#cd2626")

#Define factors
data$lifeform <- factor(data$lifeform, levels = c("All", "Tree", "Liana", "Shrub",
                                                  "Subshrub", "Herb"))
#Get plots
rdp <- ggplot(data, aes(x = NormalizedRichness,
                        y = DispersedFieldNormalized,
                        colour = bi_class)) +
  geom_point() +
  scale_colour_manual(name = "Significance",
                      values = custom_pal) +
  theme_bw() +
  xlab("Richness (Normalized)") + ylab("Dispersion Field (Normalized)") +
  facet_wrap(.~ lifeform, scales = "free_x") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "none")
rdp
#Get biplot legend
#Get legend
break_vals2 <- list()
break_vals2$bi_y <- c("Low", "Random\n(Non-significant)", "High")
break_vals2$bi_x <- c("Low\n(Q1)", "Medium\n(Q2-Q3)", "High\n(Q4)")
legend <- bi_legend(pal = custom_pal,
                    xlab = "Richness",
                    ylab = "Rarity",
                    size = 17,
                    dim = 3,
                    breaks = break_vals2) +
  #Deixar background transparente
  theme(panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA),
        legend.title = element_text(size=8))
legend
#Save legend
ggsave("Data/Figures/LegendRDP.png", legend,
       units = "px",
       dpi = 600, width = 1500,
       height = 1500, scale = 1.75)
#Join data
p <- rdp + legend + plot_layout(widths = c(2, 0.5))
p
ggsave("Data/Figures/RangeDiversityPlots.png", p,
       units = "px",
       dpi = 600, width = 2500,
       height = 1500, scale = 3)

#Single range-diversity plot
data_single <- data %>% filter(lifeform == "Tree")
srdp <- ggplot(data, aes(x = NormalizedRichness,
                          y = DispersedFieldNormalized,
                          colour = bi_class)) +
  geom_point() +
  scale_colour_manual(name = "Significance",
                      values = custom_pal) +
  theme_bw() +
  xlab("Richness (Normalized)") + ylab("Dispersion Field (Normalized)") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "none",
        axis.title=element_text(size=18),
        axis.text = element_text(size = 14))
srdp
ggsave("Data/Figures/Single_RangeDiversityPlot.png", srdp,
       units = "px",
       dpi = 600, width = 2200,
       height = 1500, scale = 1.75)

#Single plot of range diversity in map
#Get br map
br <- read_state()
bb_af <- c(-57.89255434, -34.82285576, -33.75435434, -2.80832455)
#Bivariate Raster Mapping
map <- ggplot() +
  geom_raster(data = data_single, aes(x = x, y = y, fill = bi_class)) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  geom_sf(data = br, fill = NA, size = 0.1, colour = "grey40") +
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.5) +
  coord_sf(xlim = c(bb_af[1] - 0.5, xmax=bb_af[3] + 0.25),
           ylim = c(bb_af[2] - 0.5, ymax=bb_af[4] + 0.5),
           expand = T) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 9.5, colour = "black"),
        axis.title = element_text(size = 17, colour = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(x = "Longitude",
       y = "Latitude") +
  annotation_scale(pad_x = unit(2.5, "cm"), plot_unit = "km")
map
ggsave("Data/Figures/Single_RangeDiversityMap.png", map,
       units = "px",
       dpi = 600, width = 1200,
       height = 1800, scale = 3)

####DESCRIBING RESULTS####

####Hotspots in ecoregions####
#Import ecoregions in AF
af_eco <- vect("../spatial_files/Data/Ecoregions_Atlantic_Forest_simplified.gpkg")
plot(af_eco, col = pals::alphabet2(13))

#Import data 
#Import data to plot#
lf <- list.files("Data/Dispersion_sign/", full.names = T)
lf <- lf[!grepl("Other", lf)] %>% as.character()
#Reorder lifeform
lf
lf <- lf[c(1,6,3:5,2)]
lf

data <- pblapply(seq_along(lf), function(i){
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
    mutate(bi_class = paste0(by_x, "-", by_y)) %>% 
    mutate(lifeform = lf_name) %>% 
    mutate(Hotspot = 
                              #Rich-rarity: 3-3
                              ifelse(bi_class == "3-3", "Rich_rare", 
                                     #Poor-rarity: 3-3
                                     ifelse(bi_class %in% c("1-3", "2-3"),
                                            "Poor_rare", NA)))
  return(df.bi)
}) %>% bind_rows()
#Convert dataframe to spatvector
pts <- vect(data, geom = c(x = "x", y = "y"), crs = crs(af_eco))
#Create column with 1 to sum
pts$value <- 1

#Extract number of points of each hostpot within each ecoregion
  #Get hotspots and lifeforms
hotspots <- unique(pts$Hotspot) %>% na.omit()
lfs <- unique(pts$lifeform)

eco_hot <- pblapply(seq_along(lfs), function(i){
  lf_i <- lfs[i]
  pts_i <- subset(pts, pts$lifeform == lf_i)
  eco_i <- lapply(hotspots, function(x){
    pts_x <- subset(pts_i, pts_i$Hotspot == x)
    z_i <- terra::zonal(pts_x, af_eco, fun = "sum", na.rm = TRUE, exact = TRUE) %>% 
      mutate(lifeform = lf_i,
             Hotspot = x,
             .before = 1) %>% 
      mutate(portion = round((value/sum(value))*100,1))
  }) %>% bind_rows() %>% dplyr::select(lifeform, Hotspot, zone, value, portion)
  }) %>% bind_rows()
#Append ecoregion names
eco_id <- data.frame(zone = 1:13, ecoregion = af_eco$ECO_NAME)
eco_data <- left_join(eco_hot, eco_id, by = "zone") %>% 
  dplyr::select(lifeform, Hotspot, ecoregion, value, portion)

#Extract top3 ecoregions for each lifeform and hotspot
top5 <- pblapply(lfs, function(i){
  data_i <- eco_data %>% filter(lifeform == i)
  h_i <- lapply(hotspots, function(x){
    data_x <- data_i %>% filter(Hotspot == x) %>% 
      top_n(portion, n = 5) %>% dplyr::arrange(desc(portion))
  eco_portion <- data.frame(Top_Ecoregions = paste0(data_x$ecoregion, " (", data_x$portion, "%)",
                        collapse = ", ")) %>% 
    mutate(lifeform = i, Hotspot = x, .before = 1)
    }) %>% bind_rows() 
}) %>% bind_rows()

#Get map base of sum of hotspots
b <- rast("Data/Vectors/AF_raster0.25.tiff")
  #Get rasters
#Richness-hotspots
rr_rasters <- pblapply(lfs, function(i){
  data_rr <- data %>% filter(Hotspot == "Rich_rare", lifeform == i)
  rr <- rasterize(data_rr[,c("x", "y")] %>% as.matrix(),
                  b)
  return(rr)
}) %>% rast()
names(rr_rasters) <- lfs
plot(rr_rasters)
#Poor-hotspot
pr_rasters <- pblapply(lfs, function(i){
  data_pr <- data %>% filter(Hotspot == "Poor_rare", lifeform == i)
  pr <- rasterize(data_pr[,c("x", "y")] %>% as.matrix(),
                  b)
  return(pr)
}) %>% rast()
names(pr_rasters) <- lfs
plot(pr_rasters)

#Sum rich-hotspot
sum_rr <- app(rr_rasters[[-1]], sum, na.rm = TRUE)
plot(sum_rr)
library(mapview)
mapview(af_eco) + mapview(sum_rr)

#Sum poor-hotspot
sum_pr <- app(pr_rasters[[-1]], sum, na.rm = TRUE)
plot(sum_pr)
library(mapview)
mapview(af_eco) + mapview(sum_pr)

#Save rasters
  #Rich_rare
writeRaster(rr_rasters, "Data/Rich_rare_hotspots.tiff")
  #Poor_rare
writeRaster(pr_rasters, "Data/Poor_rare_hotspots.tiff")

#Start to describe results
#How many cells in AF?####
b %>% as.data.frame(na.rm = TRUE) %>% nrow()
#How many species considering all life forms?
data %>% filter(lifeform == "All") %>% pull(Richness) %>% min()
data %>% filter(lifeform == "All") %>% pull(Richness) %>% max()
#Mean species
pblapply(lfs, function(i){
  paste(i, data %>% filter(lifeform == i) %>% pull(Richness) %>% mean())
  })
#Sd species
pblapply(lfs, function(i){
  paste(i, data %>% filter(lifeform == i) %>% pull(Richness) %>% sd())
})

#How many cells in each hotspots
data %>% filter(!is.na(Hotspot)) %>% group_by(lifeform, Hotspot) %>%
                  summarise(n = n()) %>% View()

#Get main threats by hotspots and lifeform####
lfs <- unique(data_luc$lifeform)
top5_threats <- pblapply(lfs, function(i){
  data_i <- data_luc %>% filter(lifeform == i)
  h_i <- lapply(unique(data_i$layer), function(x){
    data_x <- data_i %>% filter(layer == x) %>% 
      top_n(portion, n = 5) %>% dplyr::arrange(desc(portion))
  luc_portion <- data.frame(Top_LUC = paste0(data_x$LUC, " (", 
                                               round(data_x$portion, 2), "%)",
                                                      collapse = ", ")) %>% 
      mutate(lifeform = i, Hotspot = x, .before = 1)
  }) %>% bind_rows() 
}) %>% bind_rows()

#How much of the hotspots are covered by natural vegetation####
pblapply(lfs, function(i) {
  v <- data_luc %>% filter(lifeform == i,
                    layer == "Richness-rarity hotspot",
                    LUC %in% c("Natural\nforest", " Other natural\nvegetation")
                    ) %>% pull(portion) %>% sum() %>% round(2)
  paste(i, v, collapse = ":")
})
pblapply(lfs, function(i) {
  v <- data_luc %>% filter(lifeform == i,
                           layer == "Poorness-rarity hotspot",
                           LUC %in% c("Natural\nforest", " Other natural\nvegetation")
  ) %>% pull(portion) %>% sum() %>% round(2)
  paste(i, v, collapse = ":")
})

#Size of hotspots#
df_percentage <- df_f %>%
  group_by(lifeform, layer, Size) %>%
  summarize(Percentage = sum(count) / sum(df_f$count) * 100) %>%
  group_by(lifeform, layer) %>%
  mutate(Percentage = Percentage / sum(Percentage) * 100)
#Averages of sizes
df_percentage %>% filter(Size == "Large (>10ha)") %>% pull(Percentage) %>% mean()
df_percentage %>% filter(Size == "Small (<1ha)") %>% pull(Percentage) %>% mean()
df_percentage %>% filter(Size == "Medium (1-10ha)") %>% pull(Percentage) %>% mean()

####How many species in each hotspot####
#Import hotspots, species info and pam
rr <- rast("Data/Rich_rare_hotspots.tiff")
pr <- rast("Data/Poor_rare_hotspots.tiff")
pam <- readRDS("Data/PAM_0.5.RDS")
sp_info <- read.csv("Data/SpeciesData.csv")
sp_info$species <- gsub(" ", "_", sp_info$species)

#Get lifeforms
lfs <- names(rr)[-1]
sp <- pblapply(lfs, function(i){
  #Subset pam by lifeform
  spp_i <- sp_info %>% filter(lifeForm == i) %>% pull(species)
  spp_i <- spp_i[spp_i %in% colnames(pam)]
  pam_i <- pam %>% dplyr::select(x, y, spp_i)
  #Get rr hotspots
  rr_i <- rr[[i]]
  #Get only cells of pam within hotspot
    #Get indices
  pam_rr <- !is.na(extract(rr_i, pam_i[,c("x", "y")])[[i]])
  pam_rr2 <- pam_i[pam_rr,]
    #Get species
  spp_in_rr <- pam_rr2 %>% dplyr::select(-x, -y) %>% colSums()
  spp_in_rr <- spp_in_rr[spp_in_rr > 1] %>% names()
  
  #Get pr hotspots
  pr_i <- pr[[i]]
  #Get only cells of pam within hotspot
  #Get indices
  pam_pr <- !is.na(extract(pr_i, pam_i[,c("x", "y")])[[i]])
  pam_pr2 <- pam_i[pam_pr,]
  #Get species
  spp_in_pr <- pam_pr2 %>% dplyr::select(-x, -y) %>% colSums()
  spp_in_pr <- spp_in_pr[spp_in_pr > 1] %>% names()
  
  #Exclusive species in pr
  spp_rr_exclusive <- setdiff(spp_in_rr, spp_in_pr)
  spp_pr_exclusive <- setdiff(spp_in_pr, spp_in_rr)
  
  #Mean range size of species
    #Range in Rarity hotspots
  total_range_rr <- pam_i %>% dplyr::select(spp_in_rr) %>% colSums()
  rr_range <- pam_pr2 %>% dplyr::select(spp_in_rr) %>% colSums()
  range_rr <- (rr_range/total_range_rr*100)
    #Range in poorness hotspos
  total_range_pr <- pam_i %>% dplyr::select(spp_in_pr) %>% colSums()
  pr_range <- pam_pr2 %>% dplyr::select(spp_in_pr) %>% colSums()
  range_pr <- (pr_range/total_range_pr*100)
    #Range in all hotspots
  rr_pr_i <- app(c(rr_i, pr_i), "sum", na.rm = TRUE)
  #Get indices
  pam_rr_pr <- !is.na(extract(rr_pr_i, pam_i[,c("x", "y")])[["sum"]])
  pam_rr_pr2 <- pam_i[pam_rr_pr,]
  #Get species
  spp_in_rr_pr <- pam_rr_pr2 %>% dplyr::select(-x, -y) %>% colSums()
  spp_in_rr_pr <- spp_in_rr_pr[spp_in_rr_pr > 1] %>% names()
  #Range
  total_range_rr_pr <- pam_i %>% dplyr::select(spp_in_rr_pr) %>% colSums()
  rr_pr_range <- pam_rr_pr2 %>% dplyr::select(spp_in_rr_pr) %>% colSums()
  range_rr_pr <- (rr_pr_range/total_range_rr_pr*100)
  
  
  #Create dataframe
  RR_sp <-  length(spp_in_rr)
  RR_sp_portion <- round((length(spp_in_rr)/length(spp_i))*100, 2)
  RR_exclusive <- length(spp_rr_exclusive)
  PR_sp <- length(spp_in_pr)
  PR_sp_portion <- round((length(spp_in_pr)/length(spp_i))*100, 2)
  PR_exclusive <- length(spp_pr_exclusive)
  RR_PR_sp <- length(spp_in_rr_pr)
  RR_PR_sp_portion <- round((RR_PR_sp/length(spp_i))*100, 2)
  
  df <- data.frame(lifeform = i) %>% 
    mutate(RR_Stotal = paste0(RR_sp, " (", RR_sp_portion, "%)")) %>% 
    mutate(RR_SExclusive = RR_exclusive) %>% 
    mutate(RR_Rangesize = paste0(round(median(range_rr), 2), "%")) %>% 
    mutate(PR_Stotal = paste0(PR_sp, " (", PR_sp_portion, "%)")) %>% 
    mutate(PR_SExclusive = PR_exclusive) %>% 
    mutate(PR_Rangesize = paste0(round(median(range_pr), 2), "%")) %>% 
    mutate(RRPR_Stotal = paste0(RR_PR_sp, " (", RR_PR_sp_portion, "%)")) %>% 
    mutate(RRPR_Rangesize = paste0(round(median(range_rr_pr), 2), "%"))
    
}) %>% bind_rows()
#Beautiful table
library(flextable)
ft <- flextable(sp) %>% 
  add_header_row(values = c("",
                            "Richness-Rarity Hotspots",
                            "Poorness-Rarity Hotspots",
                            "Rarity-Hotspots*"),
                 colwidths = c(1, 3, 3, 2), top = TRUE
  ) %>% 
  set_header_labels(lifeform = "Life form", RR_Stotal = "Stotal",
                    RR_SExclusive = "SExclusive", RR_Rangesize = "Range size", 
                    PR_Stotal = "Stotal", PR_SExclusive = "SExclusive",
                    PR_Rangesize = "Range size",
                    RRPR_S = "Stotal", RRPR_Rangesize = "Range size") %>% 
  theme_zebra() %>% 
  align(align = "center", part = "all") %>% 
  autofit()
ft
#Save as docx
flextable::save_as_docx(ft, path = "Data/Metrics/Species_in_Hotspots.docx")

####Which poorness-rarity hotspots are more fragmented compared to richness rarity?####
f_df <- data_f %>% group_by(lifeform, layer, Size) %>% summarise(n = sum(count))
lfs <- lf[-1]
sizes <- f_df$Size %>% unique()
more_f <- pblapply(lfs, function(i){
  lapply(sizes, function(x){
    rr_f <- f_df %>% filter(lifeform == i, Size == x, layer == "Rich_rare") %>%
      pull(n)
    pr_f <- f_df %>% filter(lifeform == i, Size == x, layer == "Poor_rare") %>%
      pull(n)
    #Quem tem mais?
    df_x <- data_frame(lifeform = i,
                       size = x,
                       More_fragments = ifelse(rr_f > pr_f, "Rich_rare",
                                               "Poor_rare"))
  }) %>% bind_rows()
}) %>% bind_rows()

#Fragments by rarity-spot (mean across life forms)
  #Get total of fragments by spot
f_total <- df_f %>% group_by(layer, lifeform) %>% 
  summarise(Total = sum(count))
f_total <- left_join(df_f, f_total)
f_total <- f_total %>% mutate(Portion = (count/Total)*100)
  #Mean across life forms
mean_lf_frag <- f_total %>% group_by(layer, Size) %>% 
  summarise(Mean = mean(Portion))


