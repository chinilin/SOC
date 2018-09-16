# title         : MLA_parallel.R
# purpose       : Run a list of caret regression models in parallel and compare R2 and RMSE
# producer      : A. Chinilin
# address       : Moscow. RSAU-MTAA

library(caret)
library(doParallel)
library(dplyr)
library(DT)

# load data
load("~/Google Drive/Ph.D. Thesis/Predict soil properties/L8&obs_data.RData")

models <- c("knn","cubist","M5","rf","qrf","ranger","xgbTree",
            "svmLinear","svmRadial","bagEarth","bagEarthGCV")

cl <- makeCluster(detectCores())
registerDoParallel(cl)

# compile cross-validation settings
ctrl1 <- trainControl(method = "repeatedcv", number = 5,
                      repeats = 10, allowParallel = TRUE)

# use lapply/loop to run everything
l <- lapply(models, function(i)
{cat("----------------------------------------------------","\n");
  set.seed(1234); cat(i," <- done\n");
  t <- train(formulaString1, data = reg.matrix, (i), trControl = ctrl1,
             preProcess = c("center", "scale"),
             metric = "RMSE")
}
)

# use lapply to print the results
results <- lapply(1:length(l), function(i) 
{cat(sprintf("%-20s",(models[i])));
  cat(round(l[[i]]$results$Rsquared[which.min(l[[i]]$results$RMSE)],4),"\t");
  cat(round(l[[i]]$results$RMSE[which.min(l[[i]]$results$RMSE)],4),"\t")
  cat(l[[i]]$times$everything[3],"\n")
}
)

# stop the parallel processing and register sequential front-end
stopCluster(cl)
registerDoSEQ()

# preallocate data types
i = 1; MAX = length(l);
x1 <- character() # Name
x2 <- numeric()   # R2
x3 <- numeric()   # RMSE
x4 <- numeric()   # time [s]
x5 <- character() # long model name

# fill data and check indexes and NA
for (i in 1:length(l)) {
  x1[i] <- l[[i]]$method
  x2[i] <- as.numeric(l[[i]]$results$Rsquared[which.min(l[[i]]$results$RMSE)])
  x3[i] <- as.numeric(l[[i]]$results$RMSE[which.min(l[[i]]$results$RMSE)])
  x4[i] <- as.numeric(l[[i]]$times$everything[3])
  x5[i] <- l[[i]]$modelInfo$label
}

# coerce to data frame
df <- data.frame(x1,x2,x3,x4,x5, stringsAsFactors = FALSE)

# print all results to R-GUI
df

# call web browser output with sortable column names
datatable(df,  options = list(
  columnDefs = list(list(className = 'dt-left', targets = c(0,1,2,3,4,5))),
  pageLength = MAX,
  order = list(list(2, 'desc'))),
  colnames = c('Num', 'Name', 'R2', 'RMSE', 'time [s]', 'Model name'),
  caption = paste('Regression results from caret models'),
  class = 'cell-border stripe')  %>%
  formatRound('x2', 3) %>%
  formatRound('x3', 3) %>%
  formatRound('x4', 3) %>%
  formatStyle(2,
              background = styleColorBar(x2, 'steelblue'),
              backgroundSize = '100% 90%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
  )
#-----------------------------------------------------------------------------#
# Run a list cross-validation methods with Cubist method
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# define all cross-validation methods
cvMethods <- c("boot", # bootstrap
               "boot632", # 0.632 bootstrap
               "LGOCV", # leave-one-group cross validation, variant of LOOCV for hierarchical data
               "LOOCV", # leave-one-out cross validation, also known as jacknife
               "cv", # cross validation
               "repeatedcv" # repeated n-fold cross validation
)

# use R lapply function to loop through all CV methos with qrf
all <- lapply(cvMethods, function(x)
{set.seed(1234); print(x); tc <- trainControl(method=(x))
fit1 <- train(formulaString1, data = reg.matrix,
              preProcess = c("center", "scale"),
              trControl = tc,
              method = "cubist")
}
)

# stop cluster
stopCluster(cl)
registerDoSEQ()

# extract the used cvMethods 
myNames <- lapply(1:6, function(x) all[[x]]$control$method)

# save results
results <- sapply(all, getTrainPerf)

# change column Names to cv methods
colnames(results) <- myNames

# get the results
results

#               boot      boot632   LGOCV     LOOCV     cv        repeatedcv
# TrainRMSE     0.7084313 0.5742017 0.5596085 0.5806658 0.5214465 0.5214465 
# TrainRsquared 0.5283114 0.6478973 0.7028943 0.5616086 0.9256366 0.9256366 
# TrainMAE      0.5876084 0.4713989 0.4794849 0.4481851 0.4664032 0.4664032 
