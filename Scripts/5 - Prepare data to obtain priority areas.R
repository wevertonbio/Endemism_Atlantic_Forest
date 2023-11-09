#### Prepare variables to priorization ####

#Load packages
library(dplyr)
library(terra)
# library(ggcorrplot)
# library(reshape2)
library(pbapply)

#Create directories to save results
dir.create("Data/Priorization/Variables", recursive = TRUE)

#Import Atlantic Forest map
af <- vect("https://github.com/wevertonbio/Get_and_Filter_Points/raw/main/Vectors/AF_dissolved..gpkg")

####Planning units: cells of 1km x 1km ####
#Set cost of lands to 1
#Create raster base
r_base <- rast(ext(af), res = 0.0083333, vals = 1) %>% 
  crop(., af, mask = TRUE)
plot(r_base)
names(r_base) <- "Planning_units"
writeRaster(r_base, "Data/Priorization/Variables/Planning_units1km.tiff",
            overwrite = TRUE)


#### Richness and dispersion sign ####
#Create raster base (at 10arc-min) to extract richness and dispersion field
r_base10 <- rast(ext(af), res = 0.16666667, vals = 1) %>% 
  crop(., af, mask = TRUE)
plot(r_base10)
#Get coordinates
coord <- as.data.frame(r_base, xy = TRUE)[-3] %>% as.matrix()

#Import files
f <- list.files("Data/Dispersion_sign/", pattern = ".RDS",
                full.names = F, recursive = F)
#Remove others
f <- f[!grepl("Other", f)]
#Get names of forms
forms <- gsub("\\.RDS", "", f)

#To test looping
i <- forms[1]

#Read files and get the richenss and dispersion field/sign for each lifeform
pblapply(forms, function(i){
  lf <- i
  df_i <- readRDS(paste0("Data/Dispersion_sign/", lf, ".RDS"))
  ##Richness
  rich_r <- rasterize(df_i %>% dplyr::select(x, y) %>% as.matrix(),
                      r_base10, values = df_i$NormalizedRichness)
  #Disaggregate
  rich_r <- disagg(rich_r, fact = 20, method = "bilinear") %>% 
    resample(., r_base)
  names(rich_r) <- "Richness"
  # writeRaster(rich_r,
  #             paste0("Priorization/", lf, "_Richness.tiff"), overwrite = TRUE)
  ##Dispersion field
  disp_r <- rasterize(df_i %>% dplyr::select(x, y) %>% as.matrix(),
                      r_base10, values = df_i$DispersedFieldNormalized)
  #Disaggregate
  disp_r <- disagg(disp_r, fact = 20, method = "bilinear") %>% 
    resample(., r_base)
  names(disp_r) <- "DispersionField"
  # writeRaster(disp_r,
  #             paste0("Priorization/", lf, "_Dispersion.tiff"), overwrite = TRUE)
  ##Dispersion Sign
  disp_s <- rasterize(df_i %>% dplyr::select(x, y) %>% as.matrix(),
                      r_base10, values = df_i$DispersionSign)
  #Disaggregate
  disp_s <- disagg(disp_s, fact = 20, method = "near") %>% 
    resample(., r_base)
  names(disp_s) <- "DispSign"
  # writeRaster(disp_s,
  #             paste0("Priorization/", lf, "_DispSign.tiff"), overwrite = TRUE)
  
  #Join spatial maps
  spt <- c(rich_r, disp_r, disp_s)
  
  #Write
  writeRaster(spt, paste0("Data/Priorization/Variables/", lf, ".tiff"),
              overwrite = T)
})

# ####% of vegetation####
#Not reproducible because the file is to big (> 1Gb), sorry :(
# #Based on mapbiomas
# mb <- rast("Mapbiomas/binary_brasil_coverage_2022.tif")
# plot(mb)
# res(mb)
# #Cut to AF extent
# mb_af <- crop(mb, af, mask = TRUE)
# plot(mb_af)
# #Save Atlantic Forest
# writeRaster(mb_af, "C:/Users/w423f654/Desktop/Mapbiomas/Atlantic_forest_coverage_2022.tiff",
#             overwrite = TRUE)

# #Aggregate cells and get % of the pixel covered by natural vegetation
# fact_mb <- round(0.0083333/res(mb_af), 0)
# mb_nat <- aggregate(mb_af, fact = fact_mb, fun = "mean", na.rm = TRUE)
# #Resample
# mb_nat <- resample(mb_nat, r_base)
# res(mb_nat)
# plot(mb_nat)
# names(mb_nat) <- "VegetationCover"
# #Save
# writeRaster(mb_nat, "Priorization/Vegetation.tiff", overwrite = TRUE)

####Urban areas: Lock out (never include as priority area) ####
ua <- vect("Data/Vectors/Urban_areas_Brasil2015.gpkg")
ua_af <- crop(ua, af)
plot(ua_af)
#Rasterize
ua_r <- rasterize(ua_af, r_base, 1)
plot(ua_r)
names(ua_r) <- "Urban_areas"
writeRaster(ua_r, "Data/Priorization/Variables/Urban_areas.tiff", overwrite = TRUE)

