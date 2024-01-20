#### Calculate efficiency of protected areas and land use cover ####
library(terra)
library(dplyr)
library(pbapply)

#Import data to plot#
lf <- list.files("Data/Dispersion_sign/", full.names = T)
lf <- lf[!grepl("Other", lf)] %>% as.character()
#Reorder lifeform
lf
lf <- lf[c(1,6,3:5,2)]
lf
#Get spatial data
  #Raster base
b <- rast("Data/Vectors/AF_raster0.25.tiff")
plot(b)

#Land use cover
luc <- rast("../atlantic_spatial/002_atlantic_spatial_grouped_classes_wgs84.tif")
plot(luc)

#Fragmentio size
fg <- rast("../atlantic_spatial/007_atlantic_spatial_natural_vegetation_fragment_id_wgs84.tif")
plot(fg)

#Protected areas
pa <- vect("../spatial_files/Data/AF Protected Areas WDPA.gpkg")

#Initiate looping
#Test looping
# i <- 2

m <- pblapply(seq_along(lf), function(i){
  lf_i <- lf[i]
  #Get lifeform name
  lf_name <- gsub("Data/Dispersion_sign/|\\.RDS", "", lf_i)
  print(lf_name)

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
  
  #Identify classes
  df.bi <- df.bi %>% mutate(Hotspot = 
                              #Rich-rarity: 3-3
                              ifelse(bi_class == "3-3", "Rich_rare", 
                              #Poor-rarity: 3-3
                                      ifelse(bi_class %in% c("1-3", "2-3"),
                                             "Poor_rare", NA)))
  #Get total number of pixels
  ptot <- nrow(df.bi)
  #Split hotspots
  rr <- df.bi %>% filter(Hotspot == "Rich_rare") #Rich Rare
  pr <- df.bi %>% filter(Hotspot == "Poor_rare") #Poor Rare

  
  #Convert to polygon
  rr_pol <- rasterize(x = as.matrix(rr[,1:2]), y = b) %>% as.polygons()
  pr_pol <- rasterize(x = as.matrix(pr[,1:2]), y = b) %>% as.polygons()
  
  #Calculate area
  rr_area <- expanse(rr_pol, unit = "km")
  pr_area <- expanse(pr_pol, unit = "km")
  
  #PA Efficiency
  pa_rr_pol <- crop(pa, rr_pol)
  pa_pr_pol <- crop(pa, pr_pol)
  pa_rr_portion <- (expanse(pa_rr_pol, unit = "km")/rr_area)*100
  pa_pr_portion <- (expanse(pa_pr_pol, unit = "km")/pr_area)*100
  
  #Dataframe
  df_pa <- data.frame(lifeform = lf_name,
                      Hotspot = c("Rich_rare", "Poor_rare"),
                      Total_pixels = c(nrow(rr), nrow(pr)),
                      Portion_pixels = c((nrow(rr)/ptot)*100, (nrow(pr)/ptot)*100),
                      Area_km = c(rr_area, pr_area),
                      Portion_protected = c(pa_rr_portion,  pa_pr_portion))
  
  
  ####Land use cover####
  rr_luc <- terra::crop(luc, rr_pol, mask = TRUE) %>% terra::freq() %>% 
    filter(value != 0)
  rr_luc$layer <- "Rich_rare"
  rr_luc$portion <- (rr_luc$count/sum(rr_luc$count))*100
  
  pr_luc <- terra::crop(luc, pr_pol, mask = TRUE) %>% terra::freq() %>% 
    filter(value != 0)
  pr_luc$layer <- "Poor_rare"
  pr_luc$portion <- (pr_luc$count/sum(pr_luc$count))*100
  #Join data
  df_luc <- rbind(rr_luc, pr_luc)
  df_luc$lifeform <- lf_name
  
  
  #Change names
  df_luc$LUC <- NA
  df_luc$LUC[which(df_luc$value == 1)] <- "Natural\nforest"
  df_luc$LUC[which(df_luc$value == 2)] <- "Other natural\nvegetation"
  df_luc$LUC[which(df_luc$value == 3)] <- "Forest\nplantation"
  df_luc$LUC[which(df_luc$value == 4)] <- "Pasture"
  df_luc$LUC[which(df_luc$value == 5)] <- "Temporary\ncrop"
  df_luc$LUC[which(df_luc$value == 6)] <- "Perennial\ncrop"
  df_luc$LUC[which(df_luc$value == 7)] <- "Urban\narea"
  df_luc$LUC[which(df_luc$value == 8)] <- "Mining"
  df_luc$LUC[which(df_luc$value == 9)] <- "Water"
  
  ####Fragment size#### #Boxplot with dots!0
  rr_fg <- terra::crop(fg, rr_pol, mask = TRUE) %>% terra::freq()
  rr_fg$layer <- "Rich_rare"
  pr_fg <- terra::crop(fg, pr_pol, mask = TRUE) %>% terra::freq()
  pr_fg$layer <- "Poor_rare"
  #Join data
  df_fg <- rbind(rr_fg, pr_fg)
  df_fg$lifeform <- lf_name
  
  #Save all objects in a list
  l_final <- list(PA_data =  df_pa,
                  LUC_data = df_luc,
                  Frag_data = df_fg)
  return(l_final)
})
names(m) <- gsub("Data/Dispersion_sign/|\\.RDS", "", lf)
#Save
dir.create("Data/Metrics")
saveRDS(m, "Data/Metrics/Metrics.rds")

####Test efficiency significance####
#Raster base
b <- rast("Data/Vectors/AF_raster0.25.tiff")
plot(b)
#Change resolution of raster base to ~9kmx9km (to create more fine PAs)
b1 <- disagg(b, fact = 3)
res(b1)*111
#AF limits
af <- vect("../spatial_files/Data/AF_dissolved.gpkg")
#Protected areas
pa <- vect("../spatial_files/Data/AF Protected Areas WDPA_v2.gpkg")
#Dissolve polygons
pa_dissolved <- aggregate(pa)
plot(pa_dissolved)
#Get extent of PAs
pa_area <- expanse(pa_dissolved, "km")
#% of AF covered by pas
af_pa_km <- pa_area/expanse(af, "km") #~10%
#Now, create random PAs covering ~10% of AF
#Get cells of raster
cells_df <- as.data.frame(b1, xy = TRUE, na.rm = TRUE, cells = TRUE)[["cell"]]
#Random PAs
random_pas <- pblapply(1:9999, function(i){
  #Extract 10% of cells
  set.seed(i)
  c_i <- sample(cells_df, size = length(cells_df) * af_pa_km)
  b2 <- b1
  b2[c_i] <- 1
  b2 <- app(b2, function(x) ifelse(x == 1, 1, NA))
  #Vectorize
  v_pa <- as.polygons(b2) %>% aggregate()
  return(v_pa)
})


#Get polygons of hotspots
htp <- pblapply(seq_along(lf), function(i){
  lf_i <- lf[i]
  #Get lifeform name
  lf_name <- gsub("Data/Dispersion_sign/|\\.RDS", "", lf_i)
  print(lf_name)
  
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
  
  #Identify classes
  df.bi <- df.bi %>% mutate(Hotspot = 
                              #Rich-rarity: 3-3
                              ifelse(bi_class == "3-3", "Rich_rare", 
                                     #Poor-rarity: 3-3
                                     ifelse(bi_class %in% c("1-3", "2-3"),
                                            "Poor_rare", NA)))
  #Split hotspots
  rr <- df.bi %>% filter(Hotspot == "Rich_rare") #Rich Rare
  pr <- df.bi %>% filter(Hotspot == "Poor_rare") #Poor Rare
  
  #Convert to polygon
  rr_pol <- rasterize(x = as.matrix(rr[,1:2]), y = b) %>% as.polygons()
  pr_pol <- rasterize(x = as.matrix(pr[,1:2]), y = b) %>% as.polygons()
  
  #Return in a list
  lfinal <- list(RR = rr_pol,
                 PR = pr_pol)
  return(lfinal)
})
names(htp) <- gsub("Data/Dispersion_sign/|\\.RDS", "", lf)


#Calculate % of hotpots covered by random PAs
random_protection <- pblapply(seq_along(htp), function(i){
  h_i <- htp[[i]]
  #Rich-rare
  rr_random <- pbsapply(random_pas, function(x){
    RR <- h_i$RR
    RR_area <- expanse(RR, "km")
    RR_pa_area <- sum(expanse(crop(RR, x), "km"))
    RR_random_protection <- RR_pa_area/RR_area
   })
  #Poor_rare
  pr_random <- pbsapply(random_pas, function(x){
    PR <- h_i$PR
    PR_area <- expanse(PR, "km")
    PR_pa_area <- sum(expanse(crop(PR, x), "km"))
    PR_random_protection <- PR_pa_area/PR_area
  })
  #Save in a list
  l_final <- list(RR_random = rr_random,
                  PR_random = pr_random)
  
})
names(random_protection) <- gsub("Data/Dispersion_sign/|\\.RDS", "", lf)
#Save random data to plot
saveRDS(random_protection, "Data/Metrics/Random_protection.RDS")

#Estimated if real protection is better, worse or equal random PAs
#Import metrics again
m <- readRDS("Data/Metrics/Metrics.rds")

protection_significance <- pblapply(names(m), function(i){
  #Get data of lifeform i
  m_i <- m[[i]]
  #Get random values of lifeform i
  random_i <- random_protection[[i]]
  # Real % protection RR and PR
  rr_real <- m_i$PA_data %>% filter(Hotspot == "Rich_rare") %>% 
    pull(Portion_protected)
  pr_real <- m_i$PA_data %>% filter(Hotspot == "Poor_rare") %>% 
    pull(Portion_protected)
  #Get best and worse limits of protection of random PAs
    #RR
  better_random_rr <- quantile(random_i$RR_random, 0.975)[[1]]
  worse_random_rr <- quantile(random_i$RR_random, 0.025)[[1]]
    #PR
  better_random_pr <- quantile(random_i$PR_random, 0.975)[[1]]
  worse_random_pr <- quantile(random_i$PR_random, 0.025)[[1]]
  #Identify if protection is better or worse
  random_rr <- ifelse(rr_real/100 < worse_random_rr, "Worse",
                      ifelse(rr_real/100 > better_random_rr, "Better", "Random"))
  random_pr <- ifelse(pr_real/100 < worse_random_pr, "Worse",
                      ifelse(pr_real/100 > better_random_pr, "Better", "Random"))
  
  #Create dataframe indicating significance
  df_sig <- m_i$PA_data %>%
    mutate(Protection_sig = ifelse(Hotspot == "Rich_rare", random_rr, random_pr))
  return(df_sig)
})
names(protection_significance) <- names(m)
#Save 
saveRDS(protection_significance, "Data/Metrics/ProtectionSignificance.RDS")
