#### Get indices from randomized matrix ####
library(pbapply)
library(dplyr)
library(GPUmatrix) #To run analysis in GPU
library(terra)

#pam_indices_gpu modified from biosurvey R package to run in GPU (faster!)
source("Scripts/Functions/PAM_indices_gpu.R")

####Calculate indices from null matrix and extract dispersion field####
#Get lifeforms
lf <- list.dirs("Data/Null_Matrix/", recursive = F, full.names = F)
#Create directory to save dispersion fields
dir.create("Data/Null_dispersion_fields")

lapply(seq_along(lf), function(x){
  lf_i <- lf[x]
  print(lf_i)
  lf_dir <- file.path("Data/Null_Matrix", lf_i)
  #Get random matrix
  random_l <- list.files(lf_dir, pattern = "m", full.names = T)
  ds_null <- pblapply(1:length(random_l), function(i){
    random_m <- readRDS(random_l[i])
    #Calculate indices
    ind_i <- PAM_indices_gpu(PAM = random_m, indices = "DF")
    #Save
    saveRDS(ind_i,
            paste0("Data/Null_Matrix/", lf_i, "/r_ind", i, ".rds"))
    #Get dispersion field normalized by site and richness
    n_sites <- ind_i$One_value_indices["Sites_Cells",]
    S <- ind_i$One_value_indices["Species",]
    ds_null_i <- ind_i$Dispersion_field/n_sites
    ds_null_i <- ds_null_i/S
    ds_null_i[which(is.na(ds_null_i))] <- 0
    return(ds_null_i)
    })
  #Write list as RDS file
  saveRDS(ds_null,
          paste0("Data/Null_dispersion_fields/", lf_i, ".RDS"))
  })


####Get percentis and calculate dispersion field sign####
#Create directory to save results
dir.create("Data/Dispersion_sign")
#Get coordinates from PAM
PAM <- readRDS("Data/PAM_0.5.RDS")
xy <- PAM[, c("x", "y")]

#Get list of pam indices
pam_ind <- list.files("Data/PAM_indices/", full.names = T)

pblapply(seq_along(pam_ind), function(i){
  #Get indices from PAM
  ind_i <- readRDS(pam_ind[i])
  #Get lifeform
  lf <- ind_i$lifeform
  
  #Get dispersion field normalized by site and richness
  ds <- (ind_i$Dispersion_field/ind_i$One_value_indices["Sites_Cells",])
  ds <- ds/ind_i$One_value_indices["Species",]
  ds[which(is.na(ds))] <- 0

    
  #Get dispersion field of null models (already normalized by site and richness)
  ds_null <- readRDS(paste0("Data/Null_dispersion_fields/", lf, ".RDS"))
  #Merge columns in a dataframe
  ds_null <- do.call("cbind", ds_null)
  
  #Get position of dispersion fill in the null  values
  ind_pos <- sapply(1:nrow(ds_null), function(i){
    ds_i <- ds[i]
    ds_null_i <- ds_null[i,]
    pos5 <- quantile(ds_null_i, 0.025) # Low 2.5% CI
    pos95 <- quantile(ds_null_i, 0.975) # High 2.5% CI
    pos_i <- ifelse(ds_i < pos5, -1,
                    ifelse(ds_i > pos95, 1, 0))
    return(pos_i)
  })

  #Join with coordinates, Richness, dispersal field normalized and richness normalized
  disp_sign <- cbind(xy,
                     DispersionSign = ind_pos,
                     DispersionField = ind_i$Dispersion_field,
                     DispersedFieldNormalized = ds,
                     Richness = ind_i$Richness,
                     NormalizedRichness = ind_i$Richness_normalized)
                     
  # #Rasterize to see
  afr <- rast("Data/Vectors/AF_raster0.25.tiff")
  disp_sign_r <- rasterize(x = disp_sign %>% dplyr::select(x, y) %>% as.matrix(),
                           y = afr,
                           values = disp_sign$DispersionSign)

  #Save data
  saveRDS(disp_sign,
            paste0("Data/Dispersion_sign/", lf, ".RDS"))
})





