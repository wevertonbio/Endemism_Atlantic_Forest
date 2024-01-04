#### Plot and describe results ####

library(dplyr)
library(ggplot2)
library(pbapply)
library(tidyr)
library(scales)

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
d1$lifeform <- factor(d1$lifeform, levels = rev(c("All", "Tree", "Liana", "Shrub",
                                              "Subshrub", "Herb")))
d1$Hotspot <- factor(d1$Hotspot, levels = c("Rich_rare", "Poor_rare"),
                    labels = c("Richness-rarity hotspot", "Poorness-rarity hotspot"))
d1$Status <- factor(d1$Status, levels = c("Non_protected", "Protected"),
                    labels = c("Non-protected", "Protected"))



#Plot
g_pa <- ggplot(d1, aes(x = lifeform, y = Value, fill=Status,
                       label = paste0(round(Value/1000, 0), "K"))) +
  geom_bar(stat = 'identity', color = "black") +
  geom_text(position = position_stack(vjust = 0.5), size = 3, angle = 90) +
  geom_text(aes(label = Protection_sig, color = Protection_sig),
            position = position_stack(vjust = 1.01), size = 4, hjust = 0, vjust = -0.01,
            alpha = rep(c(0, 1), 12),
            fontface = "bold") +
  scale_color_manual(values = c("darkgreen", "firebrick"), guide = "none") +
  scale_fill_manual(values=c('gray', 'darkgreen'),
                    guide = guide_legend(reverse = TRUE)) +
  coord_flip() +
  ylab(bquote('Area '(km^2))) +
  xlab("Life form") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()),
                     limits = c(0, 600000)) +
  facet_grid(.~Hotspot) +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom")
g_pa
ggsave("Data/Figures/Protection.png", g_pa,
       units = "px",
       dpi = 600, width = 2500,
       height = 1000, scale = 3)

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
                         labels = c("Richness-rarity hotspot", "Poorness-rarity hotspot"))
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
  theme(legend.position = "bottom")
g_luc
ggsave("Data/Figures/LUC.png", g_luc,
       units = "px",
       dpi = 600, width = 2500,
       height = 1500, scale = 3)

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
  mutate(Size = ifelse(Area_ha < 1, "Small", ifelse(
      between(Area_ha, 1, 10), "Medium", ifelse(
        Area_ha > 10, "Large", NA)
    )
  ))
table(data_f$Size)
#Count data by group
df_f <- data_f %>% group_by(lifeform, layer, Size) %>% 
  summarise(count = n())

#Get area of hotspot to get number of fragments/area
df_area <- data_luc %>% dplyr::select(layer, lifeform, Area_km) %>% distinct()
df_area$layer <- as.character(df_area$layer)
df_area$layer[which(df_area$layer == "Richness-rarity hotspot")] <- "Rich_rare"
df_area$layer[which(df_area$layer == "Poorness-rarity hotspot")] <- "Poor_rare"

df_f <- left_join(df_f, df_area)
df_f$NormalizedFragment <- df_f$count/(df_f$Area_km/100)

#Define factors
df_f$layer <- factor(df_f$layer, levels = c("Rich_rare", "Poor_rare"),
                         labels = c("Richness-rarity hotspot", "Poorness-rarity hotspot"))
df_f$lifeform <- factor(df_f$lifeform, levels = c("All", "Tree", "Liana", "Shrub",
                                                          "Subshrub", "Herb"))
df_f$Size <- factor(df_f$Size, levels = c("Large", "Medium", "Small"),
                    labels = c("Large (>10ha)", "Medium (1-10ha)",
                               "Small (<1ha)"))



#Plot
g_f <- ggplot(df_f, aes(x = lifeform, y = NormalizedFragment, fill=Size, group = Size)) +
  geom_bar(stat = 'identity', position = "stack", color = "black") +
  # scale_fill_manual(values=c('gray', 'darkgreen'),
  #                   guide = guide_legend(reverse = TRUE)) +
  # coord_flip() +
  ylab("Number of fragments/hectare") +
  xlab("Life form") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = c("firebrick", "#FDAE61", "#1A9641"),
                    breaks =  c("Small (<1ha)", "Medium (1-10ha)",
                                "Large (>10ha)"),
                    name = "Fragment size") +
  facet_grid(.~layer) +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom")
g_f
ggsave("Data/Figures/Fragment.png", g_f,
       units = "px",
       dpi = 600, width = 2500,
       height = 1000, scale = 3)
