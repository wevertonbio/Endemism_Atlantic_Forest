#### Priorization with prioritizr ####
#See more in:
#https://prioritizr.net/index.html

#We are going to create 2 types of priority areas: (1) considering equal costs
#to obtain the lands (the cost to protect one cell is equal to 1);
#and (2) considering costs to restaure the land (the cost to protect one cell is
#equal to 1 minus the proportion of natural vegetation remained in the cell). 
#We will also "lock out" urban areas, so they are never be included as prioority
#areas.

#Create directory to save results
dir.create("Data/Priorization/Priority_areas")

#We need to install gurobi to run the analysis
#After install gurobi
#install.packages("c:/gurobi1003/win64/R/gurobi_10.0-3.zip", repos = NULL)

#Load packages
library(prioritizr)
library(terra)
library(gurobi)

####Load lifeforms####
lf <- list.files("Data/Dispersion_sign/", pattern = ".RDS",
           full.names = F, recursive = F)
#Remove others
lf <- lf[!grepl("Other", lf)]
#Get names of forms
lf <- gsub("\\.RDS", "", lf)

#Get planning units (Atlantic forest divided in cells of 1km x 1km)
pu <- rast("Data/Priorization/Variables/Planning_units1km.tiff")
plot(pu)

#Vegetation - Used as proxy of cost: less vegetation, more expensive to protect
veg <- rast("Data/Priorization/Variables/Vegetation.tiff")
plot(veg)
#Using log + 1, because if there are 0 values, they all be always selected
#Multiply by protected areas to set 0 to them, because there is no cost to acquire this areas
cost <- log((1 - veg) + 1)

#Resample to have the same extent than planning units
cost <- resample(cost, pu, method = "bilinear")
plot(cost)

#Lock out urban areas
ua <- rast("Data/Priorization/Variables/Urban_areas.tiff")
plot(ua)


#Looping through life forms
#To test looping
i <- lf[2]
pblapply(lf, function(i){
  #Load spatial variables
  data <- rast(paste0("Data/Priorization/Variables/", i, ".tiff"))
  
  #Invert values of dispersion sign (higher values to endemic places)
  data$DispSign <- data$DispSign * -1 
  #Sum 2 to eliminate negative values
  data$DispSign <- data$DispSign + 2
  
  #Create features:
  feat <- c(data$Richness, data$DispSign)
  #Create problem that do not consider vegetation/restauration
  p <- problem(pu, feat) %>% 
    add_locked_out_constraints(ua) %>% #Never include urban areas
    #Ensure that all selected planning units have at least 2 selected neighbors
    add_neighbor_constraints(2, clamp = TRUE) %>% 
    #Minimize the cost of the solution 
    add_min_set_objective() %>% 
    add_relative_targets(0.17) %>% #Conserve 17%
    add_gurobi_solver(threads = 40) %>% 
    add_proportion_decisions() #Faster
  #Solve problem
  s <- solve(p, force = TRUE)
  names(s) <- "Equal costs"
  #plot(s, main = "Equal costs")
  
  #Create problem that considers vegetation/restauration
  p2 <- problem(cost, feat) %>% 
    add_locked_out_constraints(ua) %>% #Never include urban areas
    #Ensure that all selected planning units have at least 2 selected neighbors
    add_neighbor_constraints(2, clamp = TRUE) %>% 
    #Minimize the cost of the solution 
    add_min_set_objective() %>% 
    add_relative_targets(0.17) %>% #Conserve 17%
    add_gurobi_solver(threads = 35) %>% 
    add_binary_decisions()
  #Solve problem
  s2 <- solve(p2, force = TRUE)
  names(s2) <- "Restauration costs"
  #plot(s2, main = "Restauration costs")
  
  #Join results
  res <- c(s, s2)
  
  #Save results
  writeRaster(res,
              paste0("Data/Priorization/Priority_areas/", i, ".tiff"),
                     overwrite = TRUE)

})

####Create problem with all lifeforms as features####
#Solution tried to protect 17
#Equal costs
lf2 <- lf[lf != "All"]
#test
i <- lf2[2]
#Read rasters
lf2_r <- pblapply(lf2, function(i) {
  #Load spatial variables
  data <- rast(paste0("Data/Priorization/Variables/", i, ".tiff"))
  
  #Invert values of dispersion sign (higher values to endemic places)
  data$DispSign <- data$DispSign * -1
  names(data$DispSign) <- paste0(i, "_", names(data$DispSign))
  #Rename richness
  names(data$Richness) <- paste0(i, "_", names(data$Richness))
  #Sum 2 to eliminate negative values
  data$DispSign <- data$DispSign + 2
  
  #Create features:
  feat <- c(data$Richness, data$DispSign)
  names(feat) <- paste0(i, "_", names(feat))
  return(feat)
  })

lf2 <- lf2_r %>% rast

#Problem with equal costs
p2 <-  problem(pu, lf2) %>% 
  #add_locked_in_constraints(pa) %>% #Always include Protected areas
  add_locked_out_constraints(ua) %>% #Never include urban areas
  #add_linear_penalties(100, data = veg_100) %>% 
  #add_linear_penalties(10, data = hfp10) %>% 
  #add_linear_penalties(100, data = richness) %>% 
  #add_linear_penalties(50, data = sign) %>% 
  #add_linear_penalties(100, disp_field) %>% 
  # add_min_set_objective() %>% 
  add_neighbor_constraints(2, clamp = TRUE) %>% 
  add_min_set_objective() %>% 
  add_relative_targets(0.17) %>% #Conserve 17%
  add_gurobi_solver(threads = 40) %>% 
  add_proportion_decisions()
#Solve problem
r2 <- solve(p2)
#Plot
plot(r2)
#Eval features
eval_feature_representation_summary(p2, r2)

#With Restauration costs
p3 <-  problem(cost, lf2) %>% 
  #add_locked_in_constraints(pa) %>% #Always include Protected areas
  add_locked_out_constraints(ua) %>% #Never include urban areas
  #add_linear_penalties(100, data = veg_100) %>% 
  #add_linear_penalties(10, data = hfp10) %>% 
  #add_linear_penalties(100, data = richness) %>% 
  #add_linear_penalties(50, data = sign) %>% 
  #add_linear_penalties(100, disp_field) %>% 
  # add_min_set_objective() %>% 
  add_neighbor_constraints(2, clamp = TRUE) %>% 
  add_min_set_objective() %>% 
  add_relative_targets(0.17) %>% #Conserve 17%
  add_gurobi_solver(threads = 40) %>% 
  add_proportion_decisions()
#Solve problem
r3 <- solve(p3)
#Plot
plot(r3)

#Join results
all_features <- c(r2, r3)
names(all_features) <- c("Equal costs", "Restauration costs")
#Write
writeRaster(all_features,
            "Data/Priorization/Priority_areas/Lifeforms_as_Features.tiff",
            overwrite = TRUE)



# ####Calculate % of priority area is protected####
# library(terra)
# library(dplyr)
# library(pbapply)
# 
# #Import files
# lf <- list.files("Priorization/Priority_areas/", full.names = T)
# lf <- lf[!grepl("Old", lf)]
# #Expand grid
# g <- expand.grid(P1 = lf, P2 = lf)
# 
# #Import vegetation
# veg <- rast("Priorization/Vegetation.tiff")
# plot(veg)
# #Get inverse of veg
# veg <- 1 - veg
# 
# #Import protected areas
# pa <- rast("Priorization/Protected_areas.tiff")
# plot(pa)
# #Protected areas = 1
# pa <- pa + 1
# plot(pa)
# 
# i <- 2
# res <- pblapply(1:nrow(g), function(i){
#   g_i <- g[i,]
#   r1 <- g_i[,1] %>% as.character() %>% rast()
#   n1 <- sources(r1) %>% sub(".*/(.*?)\\.tiff", "\\1", .)
#   r1[r1 > 0] <- 1
# 
#   
#   r2 <- g_i[,2] %>% as.character() %>% rast()
#   n2 <- sources(r2) %>% sub(".*/(.*?)\\.tiff", "\\1", .)
#   r2[r2 > 0] <- 1
#   
#   #How much r1 protect in r2?
#   r3 <- r1 + r2
#   r_protected <- round((length(r3[r3==2])/length(r1[r1>0]))*100, 1)
#   
#   #How much we need to restaure?
#   r_to_vect <- r1
#   r_to_vect[r_to_vect == 0] <- NA
#   r_vect <- as.polygons(r_to_vect)
#   to_restaure <- crop(veg, r_vect, mask = TRUE) %>% as.data.frame()
#   to_restaure <- round(sum(to_restaure[,1], na.rm = TRUE), 0)
#   
#   #How much is protected
#   is_protected <- r1 + pa
#   df_protected <- as.data.frame(is_protected, na.rm = TRUE)
#   protected <- round(mean(df_protected[,1] == 2, na.rm = TRUE), 3)*100
#   
#   
#   #Get dataframe
#   g_f <- data.frame(Planning = n1,
#                     Target = n2,
#                     Protected_other = r_protected,
#                     is_already_protected = protected,
#                     Need_restauration = to_restaure)
#   return(g_f)
# })
# res_df <- bind_rows(res)
# #Save
# write.csv(res_df, "Priorization/Priority_areas/Data_priotiry_ares.csv",
#           row.names = F)
# 
# #Data with equal costs
# res_equal <- res_df %>% filter(grepl("equal", Planning)) %>% 
#   filter(grepl("equal", Target))
# 
# # Use a função pivot_wider para reformular o dataframe
# library(tidyr)
# reshaped_equal <- pivot_wider(res_equal, names_from = Target,
#                               values_from = Protected_other)
# #Rename data
# colnames(reshaped_equal) <- gsub("_equalCosts", "", colnames(reshaped_equal))
# 
# 
# 
# #Get restauration costs
# res_rest <- res_df %>% filter(grepl("Restauration", Planning)) %>% 
#   filter(grepl("Restauration", Target))
# 
# # Use a função pivot_wider para reformular o dataframe
# library(tidyr)
# reshaped_rest <- pivot_wider(res_rest, names_from = Target,
#                              values_from = Protected_other)
# 
# #Custom table
# res_all <- pivot_wider(res_equal, names_from = Target,
#                        values_from = Protected_other)
# 
# library(flextable)
# flextable(reshaped_equal)
