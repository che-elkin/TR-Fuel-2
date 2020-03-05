# first test of mlr for fuel classification

# install.packages("mlr", dependencies = TRUE)
library(mlr)
library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(stars)
library(GSIF)
library(tmap)
library(RColorBrewer)

## Load the data
modDat <- read.csv("F:/workspace/TR-Fuel-2/Height_.csv",
                   stringsAsFactors = TRUE)  ## the var to be predicted needs to be a factor
# str(modDat)


## define output directory
outDir <- "F:/workspace/TR-Fuel-2/test/"

# ## load the training data 
# response <- read.csv("./cleanData/15_CWD_subPlotSummaryData.csv", 
#                      stringsAsFactors = FALSE) #%>% 
# 
# pred <- read.csv("./cleanData/16_LAS_Metrics.csv", stringsAsFactors = FALSE)
# 
# cwd_compare <- left_join(response, pred, by = c("subPlot" = "Name"))

# ground <- raster::raster("e:/LAS/aleza_tiled/nHeights/LAS_Metrics/mosaics/countG.tif")
# cwdCount <- raster::raster("e:/LAS/aleza_tiled/nHeights/LAS_Metrics/mosaics/DWM.tif")
# 
# DWMRatio <- cwdCount / (cwdCount + ground)
# raster::writeRaster(DWMRatio, "e:/LAS/aleza_tiled/nHeights/LAS_Metrics/mosaics/cwd2ground.tif")
# 
# 
# cov_list <- list.files("e:/LAS/aleza_tiled/nHeights/LAS_Metrics/mosaics/",
#                        pattern = "tif", 
#                        recursive = FALSE,
#                        full.names = TRUE)
# cov_list <- cov_list[!grepl("xml", cov_list)]
# cov_list 
# 
# 
# ### had problems using read_stars
# r <- raster::stack(cov_list)
# 
# 
# 
# ## manually set the names of the raster layers to match that of the model data below
# r_names <- c("GapFraction137", "cwd2ground", "RumpleXYZ", "Rumple", 
#              "zmax", "zmean", "zsd", "zskew", "zkurt", "zentropy",
#              "pzabovezmean", "pzabove2", "zq5", "zq10", "zq15", "zq20", "zq25", 
#              "zq30", "zq35", "zq40", "zq45", "zq50", "zq55", "zq60",
#              "zq65", "zq70", "zq75", "zq80", "zq85", "zq90", "zq95",
#              "zpcum1", "zpcum2", "zpcum3", "zpcum4", "zpcum5", "zpcum6", "zpcum7",
#              "zpcum8", "zpcum9",
#              "VCI_2.5", "VCI_5")
# 
# names(r) <- r_names
# 
# ## compare names -- it is a fit! 
# # setdiff(r_names, names(modDat))
# # setdiff(names(modDat), r_names)

## reduce dataframe to just the variables needed 
modDat <- cwd_compare %>% dplyr::select(VanWagnerVolume:cwd2ground) %>% 
  dplyr::select(-Treatment,  ## shouln't be there 
                -SE,         ## has missing data -- dropped
                -MeanDia,    ## empirical data do not use 
                -cwdCount)   ## do not use this in model

tsk <- makeRegrTask(data = modDat, target = "VanWagnerVolume")


## listLearners()$class ## lists all available MLA
## getLearnerParamSet(lrn)  ## show parameters that can be set
## getHyperPars(lrn)        ## show currently set parameters

lrn <- makeLearner("regr.ranger",
                   num.trees = 500,                         ## number of trees DEFAULT: 500
                   mtry = round(sqrt(ncol(modDat)-1)),      ## MC showed me to declare mtry this way ... 
                   num.threads = parallel::detectCores()*2, ## 
                   predict.type = "se",                     ## provide mean response and se
                   importance = "impurity")


mod <- train(lrn, tsk)
# saveRDS(mod, "./out/21_mlr_ranger_model.rds")



# Predict the landscape

## convert raster to table 
r_spd <- as(r, "SpatialPixelsDataFrame")  ## convert to 

## make an sf object 
r_spd <- st_as_sf(r_spd)

## add an id column ... likely not needed
# r_spd$id <- seq(1:nrow(r_spd))
# r_spd_clean <- r_spd[complete.cases(r_spd),]

## remove NA values 
rspd_clean <- na.omit(r_spd)

## convert to data frame
rspd <- rspd_clean %>% as.data.frame() %>% dplyr::select(-geometry)
# rspd 




pred <- predict(mod, newdata = rspd)
saveRDS(pred, "./out/21_mlr_pred.rds")


# Export rasters

rspd_final <- cbind(rspd_clean, pred)
rspd_final <- rspd_final %>% dplyr::select(response, se)



r_out <- st_rasterize(rspd_final, 
                      template = template)
r_out2 <- st_rasterize(rspd_final["se"],
                       template = template)

# write_stars(r_out, "e:/LAS/aleza_tiled/nHeights/LAS_Metrics/Modelled_DWM.tif")
# write_stars(r_out2, "e:/LAS/aleza_tiled/nHeights/LAS_Metrics/Modelled_DWM_se.tif")






# leave one out validation
loo <- makeResampleDesc("LOO")

for (i in 3:10) {
  # i <- 
  loo.val <- mlr::resample(learner = lrn,
                           task = tsk,
                           resampling = loo,
                           # measures = list(rsq, expvar, mse, rmse),
                           models = TRUE)
  
  loo.table <- loo.val$pred$data
  loo.table$sse <- (loo.table$truth - loo.table$response)^2
  loo.table$sst <- (loo.table$truth - mean(loo.table$truth))^2
  
  loo.r2 <- sum(loo.table$sse) / sum(loo.table$sst)
  # loo.r2
  
  if (i == 1) {
    loo.repeated <- loo.r2
  } else {
    loo.repeated <- append(loo.repeated, loo.r2)}
  
}

## Individual values 
##0.7926467 0.7830891 0.7910770 0.7844581 0.7871193 0.7944832 0.7893260 0.7915169 0.7900639 0.7865126

mean(loo.repeated) 
# 0.7890293

se <- sd(loo.repeated) / sqrt(192)
#sd:  0.0036596
#se:  0.00026



