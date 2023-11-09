#Function to run PAM_indices from biosurvey in GPU
PAM_indices_gpu <- function(PAM, indices = "all",
                             exclude_column = NULL) {
  if (missing(PAM)) {
    stop("Argument 'PAM' must be defined.")
  }
  all_in <- c("all", "basic", "AB", "BW", "BL", "SCSC", "SCSR", 
              "DF", "SCC", "WRN", "SRC", "CMSC", "CMSR", "MCC", "MRC")
  if (any(!indices %in% all_in)) {
    stop("One or more elements defined in 'indices' is not valid, check function's help.")
  }
  cpam <- class(PAM)[1]
  if (!cpam %in% c("base_PAM", "matrix", "data.frame")) {
    stop("Argument 'PAM' must be of class 'base_PAM' or 'matrix'.")
  } else {
    if (cpam == "base_PAM") {
      bpam <- PAM
      PAM <- as.matrix(bpam$PAM@data[, -(1:3)])
      rownames(PAM) <- bpam$PAM@data[, "ID"]
      exclude_column <- NULL
    }
  }
  if (!is.null(exclude_column)) {
    if (class(!exclude_column)[1] %in% c("numeric", "character")) {
      stop("Argument 'exclude_column' must be of class 'numeric' or 'character'.")
    }
    if (is.numeric(exclude_column)) {
      PAM <- PAM[, -exclude_column]
    }
    else {
      PAM <- PAM[, !colnames(PAM) %in% exclude_column]
    }
  }
  if (cpam == "data.frame") {
    PAM <- as.matrix(PAM)
  }
  #Put matrix in GPU
  tm1 <- gpu.matrix(t(PAM))
  PAM <- gpu.matrix(PAM)
  S <- ncol(PAM)
  N <- nrow(PAM)
  A <- PAM %*% tm1
  O <- tm1 %*% PAM
  rich <- diag(A)
  rang <- diag(O)
  richS <- rich/S
  rangN <- rang/N
  trA <- sum(rich)
  trO <- sum(rang)
  if (any(indices %in% c("all", "DF", "BL", "WRN", "SRC", 
                         "CMSC"))) {
    d_field <- c(PAM %*% rang)
    d_field <- (d_field - rich)/2
    names(d_field) <- rownames(PAM)
    av_dfield <- mean(d_field)
  } else {
    d_field <- NULL
    av_dfield <- NA
  }
  if (any(indices %in% c("all", "SCC", "CMSR"))) {
    sc_comp <- c(tm1 %*% rich)
    names(sc_comp) <- colnames(PAM)
    av_sccomp <- mean(sc_comp)
  } else {
    sc_comp <- NULL
    av_sccomp <- NA
  }
  if (any(indices %in% c("all", "BW", "WRN", "CMSR", "CMSC"))) {
    BW <- (S * N)/trO
  }  else {
    BW <- NA
  }
  if (any(indices %in% c("all", "AB"))) {
    BA <- S * (1 - (trO/(S * N)))
  }   else {
    BA <- NA
  }
  if (any(indices %in% c("all", "BL"))) {
    BL <- trO - sum(d_field)
  }   else {
    BL <- NA
  }
  
  if (any(indices %in% c("all", "MCC"))) {
    Ccov_mean <- (d_field/(N * S)) - (BW^-1 * richS)
  } else {
    Ccov_mean <- NULL
  }
  
  if (any(indices %in% c("all", "CMSR", "SCSR"))) {
    t_rangN <- gpu.matrix(t(rangN))
    RS_cov <- (O/S) - (rangN %*% t_rangN)
  }   else {
    RS_cov <- NULL
  }
  
  if (any(indices %in% c("all", "MRC"))) {
    Rcov_mean <- (sc_comp/(N * S)) - (BW^-1 * rangN)
  }   else {
    Rcov_mean <- NULL
  }
  
  if (any(indices %in% c("all", "SCSR"))) {
    VRS_cov <- sum(RS_cov)/sum(diag(RS_cov))
  }   else {
    VRS_cov <- NA
  }
  
  if (any(indices %in% c("all", "WRN"))) {
    Nc <- (sum(d_field) - ((N * S)/BW))/2
  } else {
    Nc <- NA
  }
  
  if (any(indices %in% c("all", "SRC"))) {
    Cs <- (sum(O * O) - (N * av_dfield))/2
  } else {
    Cs <- NA
  }
  
  #Calculate after, if necessary (need to free memory in GPU)
  #Remove Schluter_cov_sites_composition"
  # CS_cov <- NULL
  # VCS_cov <- NA
  
  tab_in <- data.frame(Value = c(N, S, av_dfield, av_sccomp, 
                                 BA, BW, BL, VRS_cov, Nc, Cs),
      row.names = c("Sites_Cells", 
                    "Species", "Av_dispersion_field",
                    "Av_shared_community_composition", 
                   "Additive_Beta", "Beta_Whittaker", "Beta_Legendre",          
                   "Schluter_cov_species_ranges", 
                   "Wright_Reeves_nestedness", "Stone_Roberts_Cscore"))
  
  nil <- list(One_value_indices = tab_in, Richness = rich, 
              Range = rang, Richness_normalized = richS, Range_normalized = rangN, 
              Dispersion_field = d_field, Shared_community_composition = sc_comp, 
              Mean_composition_covariance = Ccov_mean, Mean_range_covariance = Rcov_mean, 
              #Cov_mat_sites_composition = CS_cov, #Removed
              Cov_mat_species_ranges = RS_cov)

    return(nil)
}

# ####Get indices####
# library(GPUmatrix)
# library(dplyr)
# library(data.table)
# library(terra)
# dir.create("PAM_indices")
# PAM <- fread("PAM.csv") %>% as.matrix()
# head(colnames(PAM))
# 
# #Get indices from all species
# ind_all <- PAM_indices_gpu(PAM = PAM, indices = "all", exclude_column = 1:2)
# #Save
# saveRDS(ind_all, "PAM_indices/Indices_all.rds")
# 
# #Plots
# ####Plot richness####
# d <- fread("PAM_Final.csv") %>% as.data.frame()
# afr <- rast("Vetores/AF_raster.tiff")
# nil <- ind_trees
# rich_p <- d %>% dplyr::select(x, y) %>% 
#   mutate(richness = nil$Richness)
# rich_r <- rasterize(x = rich_p %>% dplyr::select(x, y) %>% as.matrix(),
#                     y = afr,
#                     values = rich_p$richness)
# plot(rich_r, main = "Richness")
# 
# ####Plot richness normalized####
# richNorm_p <- d %>% dplyr::select(x, y) %>% 
#   mutate(richNormness = nil$Richness_normalized)
# richNorm_r <- rasterize(x = richNorm_p %>% dplyr::select(x, y) %>% as.matrix(),
#                         y = afr,
#                         values = richNorm_p$richNormness)
# plot(richNorm_r)
# plot(richNorm_r, main = "Richness normalized")
# 
# ####Plot dispersion field####
# disp_p <- d %>% dplyr::select(x, y) %>% 
#   mutate(dispness = nil$Dispersion_field)
# disp_r <- rasterize(x = disp_p %>% dplyr::select(x, y) %>% as.matrix(),
#                     y = afr,
#                     values = disp_p$dispness)
# plot(disp_r, main = "Dispersion field")
# 
# ####Plot mean compositivon covariance ####
# comp_p <- d %>% dplyr::select(x, y) %>% 
#   mutate(compness = nil$Mean_composition_covariance)
# comp_r <- rasterize(x = comp_p %>% dplyr::select(x, y) %>% as.matrix(),
#                     y = afr,
#                     values = comp_p$compness)
# plot(comp_r, main = "Mean composition covariance")
# 
# 
# df <- data.frame(Richness = ind_all$Richness_normalized,
#                  Dispersion_Field = ind_all$Dispersion_field,
#                  Mean_dispersion = ind_all$Dispersion_field/ncol(d))
# library(ggplot2)
# g <- ggplot(df, aes(x = Richness, y = Mean_dispersion)) +
#   geom_point()
# g
# 
# ####Get Null metrics####
# library(picante)
# library(pbapply)
# library(dplyr)
# library(data.table)
# 
# #Get PAM
# PAM <- fread("PAM.csv") %>% as.matrix()
# PAM2 <- PAM[,-c(1:2)]
# 
# #Get random matrix in paralelle
# library(parallel)
# cl <- makeCluster(40)
# clusterExport(cl, varlist = "PAM2", #Get PAM2
#               envir=environment())
# clusterEvalQ(cl, {
#   library(picante)
#   library(pbapply)
# })
# 
# dir.create("PAM_indices/Null_Matrix")
# dir.create("PAM_indices/Null_Matrix/All")
# pblapply(1:99, function(i){
#   set.seed(i)
#   random_matrix_i <- picante::randomizeMatrix(samp = PAM2, 
#                       null.model = "trialswap",
#                       iterations = length(PAM2)*10)
#   saveRDS(random_matrix_i, paste0("PAM_indices/Null_Matrix/All/m", i, ".rds"))
# }, cl = cl)
# 
# #get random matrix
# random_l <- list.files("PAM_indices/Null_Matrix/All/", full.names = T)
# 
# #Calculate PAM indices from NULL matrix
# PAM_indices_null <- pblapply(seq_along(random_l), function(i){
#   random_m <- readRDS(random_l[i])
#   ind_i <- PAM_indices_gpu(PAM = random_m, indices = "DF")
#   saveRDS(ind_i, paste0("PAM_indices/Null_Matrix/All/", "r_ind", i, ".rds"))
#   return(ind_i) })
# 
# #Save results
# saveRDS(PAM_indices_null, "PAM_indices/Ind_null.rds")
# 
# ####Get percentis####
# PAM <- fread("PAM.csv") %>% as.matrix()
# ind_null <- readRDS("PAM_indices/Ind_null.rds")
# ind <- readRDS("PAM_indices/Indices.rds")
# 
# #Get dispersion field normalized by site and richness
# ds <- (ind$Dispersion_field/ind$One_value_indices["Sites_Cells",])
# ds <- ds/ind$One_value_indices["Species",]
# ds[which(is.na(ds))] <- 0
# 
# ds_null <- pbsapply(seq_along(ind_null), function(i){
#   ind_null_i <- ind_null[[i]]
#   n_sites <- ind_null_i$One_value_indices["Sites_Cells",]
#   S <- ind_null_i$One_value_indices["Species",]
#   ds_null_i <- ind_null_i$Dispersion_field/n_sites
#   ds_null_i <- ds_null_i/S
#   ds_null_i[which(is.na(ds_null_i))] <- 0
#   return(ds_null_i)
# })
# 
# #See if indice is below 5% or above 95% os values
# #-1 = Below 5
# #0 = 5-95
# #1 = Above 95
# ind_pos <- pbsapply(1:nrow(ds_null), function(i){
#   ds_i <- ds[i]
#   ds_null_i <- ds_null[i,]
#   pos5 <- quantile(ds_null_i, 0.05)
#   pos95 <- quantile(ds_null_i, 0.95)
#   pos_i <- ifelse(ds_i < pos5, -1,
#                   ifelse(ds_i > pos95, 1, 0))
#   return(pos_i)
# })
# #Join with coordinates to plot
# disp_sign <- PAM %>% as.data.frame() %>% dplyr::select(x, y) %>% 
#   mutate(DispersionSign = ind_pos)
# #Join with dispersal field normalized and richness normalized
# disp_sign <- disp_sign %>% 
#   mutate(NormalizedRichness = ind$Richness_normalized,
#          DispersedFieldNormalized = ds)
# 
# #Rasterize to see
# afr <- rast("Vetores/AF_raster.tiff")
# disp_sign_r <- rasterize(x = disp_sign %>% dplyr::select(x, y) %>% as.matrix(),
#                         y = afr,
#                         values = disp_sign$DispersionSign)
# 
# plot(disp_sign_r)
# #Salvar dataframe
# write.csv(disp_sign, "PAM_indices/DispersionFieldSign.csv", row.names = F)
# 
# 
# 
