#### Build PAM in Atlantic Forest ####

library(dplyr)
library(terra)
library(biosurvey)
library(pbapply)
library(tidyverse)
library(data.table)

####Import AF vector ####
af <- vect("Vetores/AF_dissolved.shp")
plot(af)
#Buffer of 10km
af <- buffer(af, width = 10*1000)
plot(af)

#Import any model to rasterize AF
r <- rast("Current_Neotropic/Bio01.tif")
afr <- rasterize(x = af, y = r)
values(afr) <- 0
afr <- crop(afr, af, mask = T)
plot(afr)
#Rasterize and aggregate afr
f <- 0.08333333/res(afr)[1]
afr2 <- aggregate(afr, factor = f, fun = "min")
res(afr2)
writeRaster(afr2, "Vetores/AF_raster.tiff", overwrite = TRUE)

#Pack spatial objects
pack_afr <- terra::wrap(afr2)
pack_af <- terra::wrap(af)

#Get species
spp <- list.dirs("Models", full.names = F, recursive = F)
#spp <- spp[1:500]

#Make cluster?
library(parallel)
cl <- makeCluster(45)
clusterExport(cl, varlist= c("spp", "pack_afr", "pack_af"), #Get all objects created untill now
              envir=environment())
clusterEvalQ(cl, {
  library(dplyr)
  library(pbapply)
  library(terra)
  library(data.table)
})

sp_l <- pblapply(seq_along(spp), function(i){
  tryCatch(
    {#Get specie
  sp <- spp[i]
  sp_dir <- file.path("Models", sp)
  #Unpack spatial objects
  af <- unwrap(pack_af)
  afr <- unwrap(pack_afr)
  #Get raster (continuous)
  sp_r <- rast(file.path(sp_dir, "Current_median.tiff"))
  #Get threshold (10%)
  res <- read.csv(file.path(sp_dir, "Thresholds_Median.csv"))
  thr10 <- res$sensitivity_10
  r10 <- terra::app(sp_r, function(x) ifelse(x < thr10, 0, 1))
  #Sum rasters
  r10_res <- resample(r10, afr, method = "near")
  r10_res[is.na(r10_res)] <- 0
  r_final <- crop(r10_res, af, mask = T)
  names(r_final) <- sp
  # #Convert to dataframe
  # r_df <- as.data.frame(r_final, xy = F, cells = T) %>% 
  #   as.data.table()
  return(r_final)
  gc()}, #Free unused ram memory
  error = function(msg){ #End of trycatch
    message(paste("Error for:", sp))
    return(NULL)
  })
})

r_all <- Filter(Negate(is.null), sp_l)
rast_all <- rast(r_all)
d <- terra::as.data.frame(rast_all, xy = TRUE)


#Save pam
library(data.table)
saveRDS(d, "PAM_5x5km.RDS")


#Subset PAM with species
sp_info <- fread("SpeciesData.csv")
spp <- unique(sp_info$species) %>% gsub(" ", "_", .)
head(colnames(d))
spp_in <- intersect(spp, colnames(d))
pam_spp <- d %>% dplyr::select(x, y, spp_in)
#Remove species without Occurrences in AF
colsump <- colSums(pam_spp[,-c(1:2)])
names(colsump) <- colnames(pam_spp[,-c(1:2)])
sp0 <- colsump[colsump == 0] %>% names()
pam_final <- pam_spp %>% dplyr::select(!sp0)
#Remove rows without Occurrences in AF
#Remove species without Occurrences in AF
rowsump <- rowSums(pam_spp[,-c(1:2)])
row0 <- rowsump > 0
pam_final2 <- pam_final[row0, ]
#Check
colSums(pam_final2[,-c(1:2)]) %>% min()
rowSums(pam_final2[,-c(1:2)]) %>% min()

saveRDS(pam_final2, "Data/PAM.RDS")

####Rasterize some specie just to check####
p <- readRDS("Data/PAM.RDS")
colnames(p) %>% head
sp <- "Xyris_neglecta"

spp <- p %>% dplyr::select(x, y, sp) %>% as.matrix()

spr <- rasterize(spp[,c("x","y")],
                 afr2, values = spp[,3])
plot(spr)
