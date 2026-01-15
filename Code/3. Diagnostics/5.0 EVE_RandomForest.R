###### Libraries
library(randomForest)
library(caret)
library(car)
library(foreach)
library(doParallel)
library(ALEPlot)

############################################################
## Input directories
dir.info_events <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_info_events_1979_2002_KGE_Balanced/")

## output directories
dir.rf <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_rf_1979_2002_KGE_Balanced/")

## Initialization
set.seed(123)
SD_SDpre_PAR_Ev_events.all <- read.table(file=paste0(dir.info_events, "SD_SDpre_PAR_Ev_all.txt"), header=TRUE)
############################################################
## Parameter initialization
n_cores <- 40
Ev_type <- "S&I"
#Ev_type <- "R.DRY"
#Ev_type <- "R.WET"

var <- 'et'
#var <- 'eq'

print(paste0("Event type: ", Ev_type, "; Var: ", var))


## Check Redundant Features
SD_SDpre_PAR_Ev_events.i <- SD_SDpre_PAR_Ev_events.all[which(SD_SDpre_PAR_Ev_events.all$event_type2 == Ev_type), ]
SD_SDpre_PAR_Ev_events.i <- SD_SDpre_PAR_Ev_events.i[complete.cases(SD_SDpre_PAR_Ev_events.i), ]
SD_SDpre_PAR_Ev_events.i <- SD_SDpre_PAR_Ev_events.i[, -grep("ID|iparset|AREA|event_type|event_count|year|month|event_type2", 
                                                             colnames(SD_SDpre_PAR_Ev_events.i))]

SD_SDpre_PAR_Ev_env.i <- SD_SDpre_PAR_Ev_events.i[, -c(1, 2)] 
SD_SDpre_PAR_Ev_env.i <- SD_SDpre_PAR_Ev_env.i[, -grep("volume|Ev_snow.cover.cov|Ev_frozen.cover.cov|Env_pet|PAR_vz_s|PAR_sm_perc|preEv_5d_et|preEv_5d_eq|preEv_10d_et|preEv_10d_eq", 
                                                       colnames(SD_SDpre_PAR_Ev_env.i))]

# calculate correlation matrix
correlationMatrix <- cor(SD_SDpre_PAR_Ev_events.i, method="spearman") 
corrplot(correlationMatrix, method="circle")
print(correlationMatrix[, c(1, 2)])

# calculate variance inflation factor
formula_lm <- as.formula(paste0(var, " ~ ", 
                                paste(colnames(SD_SDpre_PAR_Ev_env.i), collapse = ' + ')))
model <- lm(formula_lm, data=SD_SDpre_PAR_Ev_events.i)
summary(model)
vif_values <- vif(model)
vif_values[which(vif_values > 5)]




## Random Forest
SD_SDpre_PAR_Ev_events.i <- SD_SDpre_PAR_Ev_events.all[which(SD_SDpre_PAR_Ev_events.all$event_type2 == Ev_type), ]
SD_SDpre_PAR_Ev_events.i <- SD_SDpre_PAR_Ev_events.i[complete.cases(SD_SDpre_PAR_Ev_events.i), ]
SD_SDpre_PAR_Ev_events.i <- SD_SDpre_PAR_Ev_events.i[, c('et', 'eq',
                                                         'PAR_swe', 'PAR_sm', 'PAR_aet',
                                                         'PAR_lf_s', 'PAR_lf_q', 
                                                         'PAR_gwr', 'PAR_gw2gw', 'PAR_gw2r',
                                                         'PAR_river_s',
                                                         'Ev_Qduration', 'Ev_t2pd', 'Ev_peak', 
                                                         'preEv_1d_et', 'preEv_1d_eq', 'ID')]
														 
 
print(n_cores)  
cl  <- makeCluster(numCores)
registerDoParallel(cl)

foreach(ID.i = unique(SD_SDpre_PAR_Ev_events.i$ID), .packages = c("caret", "car", "randomForest", "ALEPlot")) %dopar% {
  SD_SDpre_PAR_Ev_env.i <- SD_SDpre_PAR_Ev_events.i[which(SD_SDpre_PAR_Ev_events.i$ID == ID.i), -c(1, 2)] 
  SD_SDpre_PAR_Ev_env.i <- SD_SDpre_PAR_Ev_env.i[, -grep("ID", colnames(SD_SDpre_PAR_Ev_env.i))]
  SD_SDpre_PAR_Ev_var.i <- SD_SDpre_PAR_Ev_events.i[which(SD_SDpre_PAR_Ev_events.i$ID == ID.i), var]
  
  rf_result.i <- NULL
  
  trControl <- trainControl(method="boot", number=10, search="random",
                            verboseIter=F, allowParallel=F)
  rf_random.i <- train(x=SD_SDpre_PAR_Ev_env.i, y=SD_SDpre_PAR_Ev_var.i, 
                       method="rf", 
                       metric="RMSE",
                       importance = TRUE,
                       do.trace=100,
                       ntree=500,
                       trControl=trControl)
  
  rf_result.i$Ev_type <- Ev_type
  rf_result.i$var <- var
  #rf_result.i$model <- rf_random.i
  rf_result.i$rf_R2 <- rf_random.i$results$Rsquared[which(rf_random.i$results$mtry == rf_random.i$bestTune$mtry)]
  rf_result.i$rf_RMSE <- rf_random.i$results$RMSE[which(rf_random.i$results$mtry == rf_random.i$bestTune$mtry)]
  rf_result.i$importance <- varImp(rf_random.i, scale=FALSE)
  rf_result.i$importanceNorm <- varImp(rf_random.i)
  
  bi.lm <- lm(as.formula(paste0(var, " ~ ", paste0("preEv_1d_", var))), 
              data=SD_SDpre_PAR_Ev_events.i[which(SD_SDpre_PAR_Ev_events.i$ID == ID.i),])
  rf_result.i$bi_R2 <- summary(bi.lm)$adj.r.squared
  rf_result.i$bi_RMSE <- RMSE(SD_SDpre_PAR_Ev_var.i, bi.lm$fitted.values)
  
  lm <- lm(as.formula(paste0(var, " ~ ", paste(colnames(SD_SDpre_PAR_Ev_env.i), collapse = ' + '))), 
           data=SD_SDpre_PAR_Ev_events.i[which(SD_SDpre_PAR_Ev_events.i$ID == ID.i),])
  rf_result.i$lm_R2 <- summary(lm)$adj.r.squared
  rf_result.i$lm_RMSE <- RMSE(SD_SDpre_PAR_Ev_var.i, lm$fitted.values)
  
  rf_result.i$ID <- ID.i
  
  # ALE
  var.name <- colnames(rf_random.i$trainingData)
  var.name <- var.name[-length(var.name)]
  DAT <- rf_random.i$trainingData[, var.name]
  yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata))
  rf_ALE.i.fmean <- data.frame(PAR=var.name, ALE=NA)
  
  ALE.list <- NULL
  for (i in 1:length(var.name)) {
    print(var.name[i])
    ALE.i <- ALEPlot(DAT,  rf_random.i, pred.fun=yhat, J=i, K=50, NA.plot = TRUE)
    ALE.list[[i]] <- ALE.i
  }
  
  rf_ALE.i.fmean$ALE <- do.call(rbind,lapply(ALE.list, function(x){mean(abs(x$f.values))}))
  rf_ALE.i.fmean$ALE.norm <- scale(rf_ALE.i.fmean$ALE, center = min(rf_ALE.i.fmean$ALE), scale = max(rf_ALE.i.fmean$ALE) - min(rf_ALE.i.fmean$ALE))
  rf_result.i$ALE <- rf_ALE.i.fmean
  
  saveRDS(rf_result.i, paste0(dir.rf, "rf_", Ev_type, "_", var, "_", ID.i, ".rda"))
}
stopCluster(cl)
