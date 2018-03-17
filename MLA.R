# title         : MLA.R
# purpose       : Testing machine learning algorithms for soil properties prediction (organic carbon & soil minerals content)
# producer      : A. Chinilin
# address       : Moscow. RSAU-MTAA

library(GSIF)
library(randomForest)
library(plotKML)
library(sp)
library(raster)
library(caret)
library(bartMachine)
library(maptools)
library(ranger)

# load and prepare the data set which consists of soil profiles and stack of rasters containing all covariates:
crs = CRS('+init=epsg:32637')
data <- read.table("Soil_profiles.txt", header = T, sep = "\t")
coordinates(data) <- ~x+y
data@proj4string <- crs

# for prediction minerals content
data.minerals <- read.table("Soil_profiles.txt", header = T, sep = "\t")
which(!complete.cases(data.minerals)) # NA
data.minerals <- na.omit(data.minerals)
coordinates(data.minerals) <- ~x+y
data.minerals@proj4string <- crs

# preparation (producing DEM derinatives, filtering and cutting raster layers, converting,
# overlaying and others operations) of soil covariates was conducted with SAGA GIS (http://www.saga-gis.org/en/index.html)
data.grid <- read.table("grid_L8&DEM_v2.txt", header = T, sep = "\t") # on Landsat 8 data & DEM derivatives (30 m)
data.grid <- read.table("grid_S2&DEM.txt", header = T, sep = "\t") # on Sentinel 2 data & DEM derivatives (10 m)
data.grid[data.grid == -99999] <- NA
# which(!complete.cases(data.grid)) # NA
data.grid <- na.omit(data.grid)
coordinates(data.grid) <- ~x+y
gridded(data.grid) <- T
data.grid@proj4string <- crs
#-----------------------------------------------------------------------------#
# NOT RUN
# lets see a correlation plot between predictors & response variable
library(corrplot)
cor <- data.grid[, -c(1:37,40:43,45:46)]
m <- cor(cor)
corrplot(m, method = "number")
corrplot(m, method = "color")
corrplot(m, method = "ellipse")
corrplot(m, method = "circle")

# correlation matrices
library(PerformanceAnalytics)
chart.Correlation(data.grid[, 47:62], histogram = TRUE, pch = 19) # some example
#-----------------------------------------------------------------------------#
# plot using Leaflet:
library(leaflet)
r = raster(data.grid["DEM"])
pal <- colorNumeric(SAGA_pal[[1]], values(r), na.color = "transparent")
leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik")%>%
  addRasterImage(r, colors = pal, opacity = 1) %>%
  addLegend(pal = pal, values = values(r), title = "Elevation, m")
#-----------------------------------------------------------------------------#
# NOT RUN
# According Hengl T.(http://gsif.isric.org/doku.php), it is also probably a good idea to convert all covariates to independent
# components. This way, it will be easier to subset to the optimal number of
# predictors during the analysis. Principal component analysis (PCA) helps reducing the prediction
# bias, which might happen if the covariates are cross-correlated. A wrapper
# function spc will convert all factor variables to indicators and run PCA on a stack of grids:

# on L8 data (Surface Reflectance Level 2 product)
data_spc1 <- spc(data.grid, ~ L8b2_mean + L8b3_mean + L8b4_mean + # strategy №1 on L8 data
                  L8b5_mean + BG_mean + BR_mean + BNIR_mean + 
                  GB_mean + GR_mean + GNIR_mean + RB_mean + 
                  RG_mean + RNIR_mean + NIRB_mean + NIRG_mean + 
                  NIRR_mean + FA + TWI + SLP)

data_spc2 <- spc(data.grid, ~ L8b2_24MART2014 + L8b3_24MART2014 + L8b4_24MART2014 + # startegy №2 on L8 data
                   L8b5_24MART2014 + BG_24MART2014 + BR_24MART2014 + BNIR_24MART2014 + 
                   GB_24MART2014 + GR_24MART2014 + GNIR_24MART2014 +
                   RB_24MART2014 + RG_24MART2014 + RNIR_24MART2014 + 
                   NIRR_24MART2014 + NIRG_24MART2014 + NIRR_24MART2014 + 
                   FA + TWI + SLP)

data_spc3 <- spc(data.grid, ~ L8b2_25APR2014 + L8b3_25APR2014 + L8b4_25APR2014 + # startegy №3 on L8 data
                   L8b5_25APR2014 + BG_25APR2014 + BR_25APR2014 + BNIR_25APR2014 + 
                   GB_25APR2014 + GR_25APR2014 + GNIR_25APR2014 +
                   RB_25APR2014 + RG_25APR2014 + RNIR_25APR2014 + 
                   NIRR_25APR2014 + NIRG_25APR2014 + NIRR_25APR2014 +
                   FA + TWI + SLP)
# on S2 data (DN coverted to reflectance by using "Semi-Automated Classification Plugin" in QGIS)
data_spc4 <- spc(data.grid, ~ S2_B02_20160409 + S2_B03_20160409 + 
                  S2_B04_20160409 + S2_B08_20160409 + BG_20160409 + 
                  BR_20160409 + BNIR_20160409 + GB_20160409 + 
                  GR_20160409 + GNIR_20160409 + RB_20160409 + 
                  RG_20160409 + RNIR_20160409 + NIRB_20160409 + 
                  NIRG_20160409 + NIRR_20160409 + FA + TWI + SLP)
# The output from this operation is a stack of independent components,
# all numeric and all scaled around 0 value. 
#-----------------------------------------------------------------------------#
# All further analysis is run using the regression matrix (produced
# using overlay of points and grids), which contains values of the
# target variable and all covariates for all training points:

data.grid@data <- cbind(data.grid@data, data_spc1@predicted@data)

overlay <- over(data, data.grid)
reg.matrix <- cbind(overlay, data@data)
dim(reg.matrix)
# or
overlay <- over(data.minerals, data.grid)
reg.matrix <- cbind(overlay, data.minerals@data)
dim(reg.matrix)
#-----------------------------------------------------------------------------#
# on L8 bands, spectral indices & dem derivatives
formulaString1 <- SOC ~ L8b2_mean + L8b3_mean + L8b4_mean + 
  L8b5_mean + BG_mean + BR_mean + BNIR_mean + 
  GB_mean + GR_mean + GNIR_mean + RB_mean + 
  RG_mean + RNIR_mean + NIRB_mean + NIRG_mean + 
  NIRR_mean + FA + TWI + SLP
# on L8 band (MART 2014), spectral indices & dem derivatives
formulaString2 <- Kaol ~ L8b2_24MART2014 + L8b3_24MART2014 + L8b4_24MART2014 +
  L8b5_24MART2014 + BG_24MART2014 + BR_24MART2014 + BNIR_24MART2014 + 
  GB_24MART2014 + GR_24MART2014 + GNIR_24MART2014 +
  RB_24MART2014 + RG_24MART2014 + RNIR_24MART2014 + 
  NIRR_24MART2014 + NIRG_24MART2014 + NIRR_24MART2014 + 
  FA + TWI + SLP
# on L8 band (APR 2014), spectral indices & dem derivatives
formulaString3 <- Kaol ~ L8b2_25APR2014 + L8b3_25APR2014 + L8b4_25APR2014 +
  L8b5_25APR2014 + BG_25APR2014 + BR_25APR2014 + BNIR_25APR2014 + 
  GB_25APR2014 + GR_25APR2014 + GNIR_25APR2014 +
  RB_25APR2014 + RG_25APR2014 + RNIR_25APR2014 + 
  NIRR_25APR2014 + NIRG_25APR2014 + NIRR_25APR2014 +
  FA + TWI + SLP
# on S2 SR bands, spectral indices & dem derivatives
formulaString4 <- Kaol ~ S2_B02_20160409 + S2_B03_20160409 + 
  S2_B04_20160409 + S2_B08_20160409 + BG_20160409 + 
  BR_20160409 + BNIR_20160409 + GB_20160409 + 
  GR_20160409 + GNIR_20160409 + RB_20160409 + 
  RG_20160409 + RNIR_20160409 + NIRB_20160409 + 
  NIRG_20160409 + NIRR_20160409 + FA + TWI + SLP
#-----------------------------------------------------------------------------#
# on principal components
formulaStringSOC <- as.formula(paste("SOC~", paste(paste0("PC", 1:17), collapse="+")))
formulaStringKaol <- as.formula(paste("Kaol~", paste(paste0("PC", 1:17), collapse="+")))
formulaStringSm <- as.formula(paste("Sm~", paste(paste0("PC", 1:17), collapse="+")))

# compile cross-validation settings
set.seed(1234)
ctrl <- trainControl(method = "LOOCV", returnResamp = "final")
ctrl1 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, allowParallel = TRUE) # 5-fold CV
ctrl2 <- trainControl(method = "cv", number = 5)
#-----------------------------------------------------------------------------#
# Models fitting (with "caret" package)
# RF or ranger
rf.tuneGrid <- expand.grid(mtry = seq(1, 19, by = 1))
ranger.tuneGrid <- expand.grid(mtry = seq(1, 19, by = 1),
                               splitrule = c("extratrees", "variance", "maxstat"),
                               min.node.size = 5)
set.seed(1234)
SOC.rf <- train(formulaString1, # can change formulastring
                data = reg.matrix,
                method = "rf", # or "ranger"
                tuneGrid = rf.tuneGrid,
                trControl = ctrl1,
                importance = TRUE,
                preProcess = c("center", "scale")) # "pca"
w1 <- min(SOC.rf$results$RMSE)
plot(varImp(object = SOC.rf), main = "RF - Variable Importance",
     top = 10, ylab = "Variable")
#-----------------------------------------------------------------------------#
# XGBoost
gb.tuneGrid <- expand.grid(eta = c(0.3,0.4,0.5,0.6),
                           nrounds = c(50,100,150),
                           max_depth = 2:3, gamma = 0,
                           colsample_bytree = 0.8, min_child_weight = 1,
                           subsample = 1)
set.seed(1234)
SOC.xgb <- train(formulaString1, data = reg.matrix,
                 method = "xgbTree",
                 tuneGrid = gb.tuneGrid,
                 trControl = ctrl1,
                 preProcess = c("center", "scale"))
w2 <- min(SOC.xgb$results$RMSE)
plot(varImp(object = SOC.xgb), main = "XGBoost - Variable Importance",
     top = 10, ylab = "Variable")
#-----------------------------------------------------------------------------#
# bartMachine (Bayesian Additive Regression Trees)
bm.tuneGrid <- expand.grid(num_trees = c(20,50,80,110),
                           k = 2, alpha = .95,
                           beta = 2, nu = 3)
set.seed(1234)
SOC.bm <- train(formulaString1, data = reg.matrix,
                method = "bartMachine",
                tuneGrid = bm.tuneGrid,
                trControl = ctrl1,
                preProcess = c("center", "scale"),
                verbose = F)
w3 <- min(SOC.bm$results$RMSE)
plot(varImp(object = SOC.bm), main = "BART - Variable Importance",
     top = 10, ylab = "Variable")
#-----------------------------------------------------------------------------#
# the same using the "Cubist" package:
set.seed(1234)
SOC.cb <- train(formulaString1,
                data = reg.matrix,
                method = "cubist",
                tuneGrid = expand.grid(committees = c(1:15),
                                     neighbors = c(5,7,9)),
                trControl = ctrl1,
                preProcess = "pca")
w4 <- min(SOC.cb$results$RMSE)
plot(varImp(object = SOC.cb), main = "Cubist - Variable Importance",
     top = 4, ylab = "Variable")
#-----------------------------------------------------------------------------#
# compare perfomance
# if we use "ctrl1" or "ctrl2" in "trControl" parametres
model_list <- list(RF = SOC.rf, XGBoost = SOC.xgb, BART = SOC.bm, Cubist = SOC.cb)
model_list <- list(RF = Kaol.rf, XGBoost = Kaol.xgb, BART = Kaol.bm)
results <- resamples(model_list)
summary(results)
# boxplot comparing results
bwplot(results, layout = c(3, 1)) # RMSE, MSE and R-squared
bwplot(results, metric = "Rsquared", main = "Algorithms accuracy comparing")
bwplot(results, metric = "RMSE", main = "Algorithms accuracy comparing")
#-----------------------------------------------------------------------------#
# Ensemble prediction:
# SOC (best models: RF, XGBoost & BART with close RMSE & Rsquared)
data.grid$SOC.RF <- predict.train(SOC.rf, data.grid@data, na.action = na.pass)
data.grid$SOC.XGBoost <- predict.train(SOC.xgb, data.grid@data, na.action = na.pass)
data.grid$SOC.bartMachine <- predict.train(SOC.bm, data.grid@data, na.action = na.pass)
data.grid$SOC.Cubist <- predict(SOC.cb, data.grid@data, na.action = na.pass)

# Kaolinit content (best models: RF & BART with close RMSE & Rsquared)
data.grid$Kaol.RF <- predict(Kaol.rf, data.grid@data, na.action = na.pass)
data.grid$Kaol.XGBoost <- predict(Kaol.xgb, data.grid@data, na.action = na.pass)
data.grid$Kaol.bartMachine <- predict(Kaol.bm, data.grid@data, na.action = na.pass)
data.grid$Kaol.Cubist <- predict(Kaol.cb, data.grid@data, na.action = na.pass)

# Smektit content (best models: RF & BART with close RMSE & Rsquared)
data.grid$Sm.RF <- predict(Sm.rf, data.grid@data, na.action = na.pass)
data.grid$Sm.XGBoost <- predict(Sm.xgb, data.grid@data, na.action = na.pass)
data.grid$Sm.bartMachine <- predict(Sm.bm, data.grid@data, na.action = na.pass)
data.grid$Sm.Cubist <- predict(Sm.cb, data.grid@data, na.action = na.pass)

# Klinoptilolit content
data.grid$Klin.RF <- predict(Klin.rf, data.grid@data, na.action = na.pass)
data.grid$Klin.XGBoost <- predict(Klin.xgb, data.grid@data, na.action = na.pass)
data.grid$Klin.bartMachine <- predict(Klin.bm, data.grid@data, na.action = na.pass)
data.grid$Klin.Cubist <- predict(Klin.cb, data.grid@data, na.action = na.pass)

# final prediction as weighted average:
data.grid$SOC.WA <- (data.grid$SOC.RF*w1+data.grid$SOC.XGBoost*w2+data.grid$SOC.bartMachine*w3)/(w1+w2+w3)
data.grid$Kaol.WA <- (data.grid$Kaol.RF*w1+data.grid$Kaol.XGBoost*w2+data.grid$Kaol.bartMachine*w3)/(w1+w2+w3)
data.grid$Sm.WA <- (data.grid$Sm.RF*w1+data.grid$Sm.bartMachine*w3)/(w1+w3)
plot((stack(data.grid[c("SOC.RF", "SOC.XGBoost", "SOC.bartMachine", "SOC.WA")])), col=SAGA_pal[[1]])
plot((stack(data.grid[c("Kaol.RF", "Kaol.bartMachine", "Kaol.WA")])), col=SAGA_pal[[1]])
plot((stack(data.grid[c("Sm.RF", "Sm.bartMachine", "Sm.WA")])), col=SAGA_pal[[1]])
#-----------------------------------------------------------------------------#
# plot using Leaflet:
library(leaflet)
r = raster(data.grid["SOC.WA"])
pal <- colorNumeric(R_pal[["soc_pal"]], values(r), na.color = "transparent")
leaflet() %>% addTiles() %>%
  addRasterImage(r, colors = pal, opacity = 1) %>%
  addLegend(pal = pal, values = values(r), title = "SOC, %")
#-----------------------------------------------------------------------------#
# plot with plotKML
plotKML(data.grid["SOC.WA"], colour_scale = R_pal[["soc_pal"]])
#-----------------------------------------------------------------------------#
# plot in GoogleMaps:
library(plotGoogleMaps)
mp <- plotGoogleMaps(data.grid, filename='SOC.html', zcol='SOC.WA', add=TRUE, colPalette=SAGA_pal[[1]])
#-----------------------------------------------------------------------------#
# or use spplot
# studarea <- readShapePoly("Fields.shp")
# area <- list("sp.polygons", studarea, col = "black", lwd = 3)
points <- list("sp.points", data, pch = "+", cex = 2, col = "black")
scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
              offset = c(565300,5592250), scale = 500, fill = c("transparent","black"))
text1 <- list("sp.text", c(565300,5592310), "0")
text2 <- list("sp.text", c(565800,5592310), "500 m")
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), 
              offset = c(566750,5593650), scale = 250)
# for SOC predictions
spplot(data.grid[c("SOC.RF", "SOC.XGBoost", "SOC.bartMachine", "SOC.WA")],
       col.regions = R_pal[["soc_pal"]],
       # scales = list(draw = T),
       names.attr = c("Random Forest","Gradient Boosting Machine", "BART", "Weighted average"),
       sp.layout = list(#area,
                      points, scale, text1, text2, arrow),
       main = "Predicted soil organic carbon content, %")
#-----------------------------------------------------------------------------#
# for minerals content predictions
spplot(data.grid[c("Kaol.RF", "Kaol.XGBoost", "Kaol.bartMachine", "Kaol.WA")],
       col.regions = SAGA_pal[[3]],
       # scales = list(draw = T),
       names.attr = c("Random Forest","Gradient Boosting Machine", "BART", "Weighted average"),
       sp.layout = list(#area,
         points, scale, text1, text2, arrow),
       main = "Predicted Kaolinite content, %")
#-----------------------------------------------------------------------------#
require(gridExtra)
grid.arrange(spplot(data.grid["Kaol.RF"], col.regions = SAGA_pal[[1]],
                    sp.layout = list(points, scale, text1, text2, arrow),
                    main = "Predicted Kaolinite content, %"),
spplot(data.grid["Sm.RF"], col.regions = SAGA_pal[[1]],
       sp.layout = list(points, scale, text1, text2, arrow),
       main = "Predicted Smektite content, %"),
ncol = 2, nrow = 1)
#-----------------------------------------------------------------------------#
# save as .png with 300 dpi
png("Predicted SOC L8&DEM.png", width = 3200, height = 1800, units = 'px', res = 300)
png("Predicted SOC S2&DEM.png", width = 3200, height = 1800, units = 'px', res = 300)
tiff("Predicted SOC L8&DEM.tif", width = 3200, height = 1800, units = 'px', res = 300)
dev.off()

raster.data <- stack(data.grid)
writeRaster(raster.data$Kaol.RF, filename = "Predicted Kaol L8&DEM.tiff", format = "GTiff",
            overwrite = TRUE, datatype = "FLT4S")
#-----------------------------------------------------------------------------#
# models fitting with "ranger" package
# derivation of RF uncertainty (maps) for regression (from GeoMLA repo, T. Hengl & M. Wright)
quantiles = c((1-.682)/2, 0.5, 1-(1-.682)/2)
# to estimate also the prediction error variance i.e. prediction intervals we set
# "quantreg = TRUE" which initiates the Quantile Regression RF approach:
SOC.qrf <- ranger(formulaString1,
                  reg.matrix,
                  num.trees = 150,
                  importance = "impurity",
                  seed = 1,
                  quantreg = TRUE)
# results
SOC.qrf
xl <- as.list(ranger::importance(SOC.qrf))
print(t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[1:10]])))
# fit model without QRF
SOC.qrf0 <- ranger(formulaString1,
                   reg.matrix,
                   num.trees = 150,
                   importance = "impurity",
                   seed = 1,
                   quantreg = FALSE)
SOC.qrf0
pred.SOC.qrf <- predict(SOC.qrf, data.grid@data, type = "quantiles", quantiles = quantiles)$predictions
# This predicts the "median" value; to predict "mean" using ranger without "quantreg = TRUE
data.grid$SOC_pred <- pred.SOC.qrf[, 2] # median value
data.grid$SOC_pred_U <- pred.SOC.qrf[, 3] # upper quantile
data.grid$SOC_pred_L <- pred.SOC.qrf[, 1] # lower quantile
data.grid$SOC_pred0 <- predict(SOC.qrf0, data.grid@data)$predictions
summary(data.grid$SOC_pred0); summary(data.grid$SOC_pred)
hexbin::hexbinplot(data.grid$SOC_pred0 ~ data.grid$SOC_pred)
# Prediction error s.d.:
data.grid$SOC_pred_range <- (pred.SOC.qrf[, 3] - pred.SOC.qrf[, 1])/2
summary(data.grid$SOC_pred_range)
hist(data.grid$SOC_pred_range)
spplot(data.grid[c("SOC_pred_L","SOC_pred", "SOC_pred_U")],
       col.regions = SAGA_pal[[3]],
       # scales = list(draw = T),
       names.attr = c("Lower quantile", "Random Forest (RF)","Upper quantile"))
#-----------------------------------------------------------------------------#
# since no other covariates are available, we use only geographical (buffer) distances to
# observation points. We first derive buffer distances for each individual point:
grid.dist0 <- GSIF::buffer.dist(data["SOC"], data.grid[1], as.factor(1:nrow(data)))
# which derives a raster map for each observation point. The spatial prediction model is defined as:
dn0 <- paste(names(grid.dist0), collapse = "+")
fm0 <- as.formula(paste("SOC ~ ", dn0))
# i.e., in the formulazinc ~ layer.1 + layer.2 + ... + layer.22 which means that the target 
# variable is a function of 22 covariates. Next, we overlay points and covariates to create a
# regression matrix, so that we can tune and fit a ranger model, and generate predictions:
overlay0 <- over(data["SOC"], grid.dist0)
reg.matrix0 <- cbind(data@data["SOC"], overlay0)
# "quantreg=TRUE" allows to derive the lower and upper quantiles i.e. standard error of the predictions
SOC.qrf0 <- ranger(fm0,
                  reg.matrix0,
                  num.trees = 500,
                  importance = "impurity",
                  seed = 1,
                  quantreg = TRUE)
SOC.qrf0
# The out-of-bag validation R squared (OOB), indicates that the buffer distances explain about 35 %
# of the variation in the response

# next, we fit the model using both thematic covariates and buffer distances:
fm1 <- as.formula(paste("SOC ~ ", dn0, "+", paste(names(data.grid[, c(36,37,42,45:60)]), collapse = "+")))
overlay1 <- over(data["SOC"], data.grid[, c(36,37,42,45:60)])
reg.matrix1 <- cbind(data@data["SOC"], overlay0, overlay1)
SOC.qrf1 <- ranger(fm1,
                   reg.matrix1,
                   num.trees = 500,
                   importance = "impurity",
                   seed = 1,
                   quantreg = TRUE)

SOC.qrf1
# The out-of-bag validation R squared (OOB), indicates that the buffer distances + covariates explain about 44 %
# of the variation in the response. RFsp including additional covariates results in somewhat smaller
# MSE than RFsp with buffer distances only. Nevertheless, it seems that buffer distances are most
# important for mapping SOC i.e. more important than remote sensing data and elevation for producing the final predictions.
xl <- as.list(ranger::importance(SOC.qrf1))
print(t(data.frame(xl[order(unlist(xl), decreasing=TRUE)[1:10]])))
#-----------------------------------------------------------------------------#
# Models fitting (with "caret" package)
# RF
ctrl1 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, allowParallel = TRUE) # 5-fold CV
rf.tuneGrid0 <- expand.grid(mtry = seq(1, 38, by = 1))
set.seed(1234)
SOC.rf0 <- train(fm1, # buffer distance + covariates
                data = reg.matrix1,
                method = "rf",
                tuneGrid = rf.tuneGrid0,
                trControl = ctrl1,
                importance = TRUE,
                preProcess = c("center", "scale"))
SOC.rf0 # R squared about 0,7
w1 <- min(SOC.rf0$results$RMSE)
plot(varImp(object = SOC.rf0), main = "RF - Variable Importance",
     top = 20, ylab = "Variable")
data.grid$SOC.rf0 <- predict(SOC.rf0, cbind(grid.dist0@data, data.grid@data))
# fit the model only using buffer distance as covariates
rf.tuneGrid1 <- expand.grid(mtry = seq(1, 22, by = 1))
SOC.rf1 <- train(fm0, # buffer distance
                 data = reg.matrix1,
                 method = "rf",
                 tuneGrid = rf.tuneGrid1,
                 trControl = ctrl1,
                 importance = TRUE,
                 preProcess = c("center", "scale"))
SOC.rf1 # R squared about 0,5
data.grid$SOC.rf1 <- predict(SOC.rf1, cbind(grid.dist0@data, data.grid@data))

points <- list("sp.points", data, pch = "+", cex = 2, col = "red")
scale <- list("SpatialPolygonsRescale", layout.scale.bar(),
              offset = c(565300,5592250), scale = 500, fill = c("transparent","black"))
text1 <- list("sp.text", c(565300,5592310), "0")
text2 <- list("sp.text", c(565800,5592310), "500 m")
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), 
              offset = c(566750,5593650), scale = 250)
spplot(data.grid[c("SOC.rf0", "SOC.rf1")],
       col.regions = R_pal[["soc_pal"]],
       # scales = list(draw = T),
       names.attr = c("RF (buffer dist+covs)", "RF (buffer dist)"),
       sp.layout = list(points, scale, text1, text2, arrow))
#-----------------------------------------------------------------------------#
