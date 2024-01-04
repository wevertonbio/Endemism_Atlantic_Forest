#### Randomize ecological binary matrices ####

#Get curve ball funciton to randomize
#See: 
#https://www.nature.com/articles/ncomms5114#change-history

source("Scripts/Functions/curve_ball.R")

#Load functions
library(parallel)
library(dplyr)
library(pbapply)


#### Generate Null matrices ####
#Import PAM general
#Load species information to split by lifeform
spinfo <- read.csv("Data/SpeciesData.csv")
spinfo$species <- gsub(" ", "_", spinfo$species)

#Import PAM
PAM <- readRDS("Data/PAM_0.5.RDS")

####Get PAM of all species####
pam_all <- PAM %>% dplyr::select(-x, -y) %>% as.matrix()
#Get number of species
n_sp <- ncol(pam_all)

#Make cluster to run in parallel
cl <- makeCluster(9)
clusterExport(cl, varlist = c("pam_all", "curve_ball"),
              envir=environment())
clusterEvalQ(cl, {
  library(pbapply)
})
#Create directory to save results
dir.create("Data/Null_Matrix/All/", recursive = TRUE)
pblapply(1:n_sp, function(i){
  set.seed(i)
  random_matrix_i <- curve_ball(pam_all)
  saveRDS(random_matrix_i, paste0("Data/Null_Matrix/All/m", i, ".rds"))
}, cl = cl)
stopCluster(cl)

####Get null matrices by lifeform####
#Get lifeforms
lf <- unique(spinfo$lifeForm)
lf
#Remove Other
lf <- lf[-6]

#Make cluster to run in parallel
lapply(seq_along(lf), function(x){
  lf_x <- lf[x] #Get lifeform
  message("Randomizing matrix of ", lf_x)
  #Create directory to save null matrix
  dir.create(file.path("Data/Null_Matrix", lf_x), recursive = TRUE)
  spp_x <- spinfo %>% filter(lifeForm == lf_x) %>% pull(species) #Get specie
  pam_x <- PAM[,spp_x[which(spp_x %in% colnames(PAM))]] #SUbset PAM
  #Get number of species
  n_sp <- ncol(pam_x)
  #Set a minimum of 999 iterations
  if(n_sp < 999) {
    n_sp <- 999
  }
  #Send objects to cluster
  cl <- makeCluster(10)
  clusterEvalQ(cl, {
    library(pbapply)
  })
  clusterExport(cl, varlist = c("pam_all", "curve_ball", "spinfo", "lf", "n_sp",
                                "pam_x", "lf_x"),
                envir=environment())
  #Randomize and save null PAM
  pblapply(1:n_sp, function(i){
    set.seed(i)
    random_matrix_i <- curve_ball(pam_x)
    saveRDS(random_matrix_i, paste0(file.path("Data/Null_Matrix/", lf_x),
                                    "/m", i, ".rds"))
  }, cl = cl)
  #Stop cluster
  stopCluster(cl)
})



