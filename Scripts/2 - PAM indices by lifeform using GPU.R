####PAM indices by lifeform ####
library(dplyr)
library(data.table)
library(terra)
library(GPUmatrix) #To run analysis in GPU


#Load species information
spinfo <- read.csv("Data/SpeciesData.csv")
spinfo$species <- gsub(" ", "_", spinfo$species)

#Import PAM
PAM <- readRDS("Data/PAM_0.5.RDS")

####Get PAM of all species####
pam_all <- PAM %>% dplyr::select(-x, -y) %>% as.matrix()

####Import function####
#pam_indices_gpu modified from biosurvey R package to run in GPU (faster!)
source("Scripts/Functions/PAM_indices_gpu.R")

#Calculate PAM indice from all species together
#Indices to calculate
indices <- c("basic", "DF", "BW", "BL")
ind_all <- PAM_indices_gpu(PAM = pam_all,
                           indices = indices)
#Include in the list x and y
ind_all$xy <- PAM %>% dplyr::select(x, y) %>% as.matrix()
#Include in the list the lifeform
ind_all$lifeform <- "All"
#Empty cuda cache
torch::cuda_empty_cache()
#Save
dir.create("Data/PAM_indices/")
saveRDS(ind_all, "Data/PAM_indices/Indices_All.rds")
rm(ind_all)

#Calculate PAM indices by lifeform
#Get lifeforms
lf <- unique(spinfo$lifeForm)
lf
#Remove other
lf <- lf[-6]

pblapply(seq_along(lf), function(i){
  #Get species in lifeform i
  lf_i <- lf[i] 
  sp_i <- spinfo %>% filter(lifeForm == lf_i)
  spp_i <- subset(colnames(pam_all), colnames(pam_all) %in% sp_i$species)
  #Get PAM of lifeforms i
  pam_i <- PAM %>% dplyr::select(spp_i) %>% as.matrix()
  #Calculate index
  ind_i <- PAM_indices_gpu(PAM = pam_i, indices = indices)
  #Include in the list x and y
  ind_i$xy <- PAM %>% dplyr::select(x, y) %>% as.matrix()
  #Include in the list the lifeform
  ind_i$lifeform <- lf_i
  #Save
  saveRDS(ind_i, paste0("Data/PAM_indices/Indices_", lf_i, ".RDS"))
  #Empty cache
  rm(ind_i)
  gc()
  torch::cuda_empty_cache()
})
