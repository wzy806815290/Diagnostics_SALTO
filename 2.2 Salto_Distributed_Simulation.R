
###### Working Directory
setwd("Y:/Home/wangzhen/Eval_SALTO")

###### Libraries
library(foreach)
library(doParallel)

###### Sources
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/Eval_Metrics.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_V2.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_PASS_V2.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_DDS.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_MODEL_INPUT.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_Eventsep_DCMA_ESR.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_Baseflow_Func.R")

############################################################
###### Functions

############################################################
###### Calibration
## Running period
startdate  <- strptime("01:10:1969", format="%d:%m:%Y")
startdate2 <- strptime("01:10:1979", format="%d:%m:%Y")
enddate    <- strptime("31:10:2002", format="%d:%m:%Y")
iwarmup    <- startdate2-startdate 
imaxdays   <- ceiling(as.numeric(enddate-startdate+1))

## Input directories
dir.obs_metinp <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Input_TS_1951_2020_HYDRAS/")
dir.obs_grds   <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Grds/")
dir.obs_q      <- c("Y:/Home/wangzhen/Eval_SALTO/Data/qobs_new/")
dir.dist_par   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_par_1990_2000_KGE_Balanced/")

## Output directories
dir.sim_q <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_qout_1979_2002_KGE_Balanced/")

## Parameter initialization
load(file=paste(dir.dist_par, "PASSout.RData", sep=""))
load(file=paste(dir.dist_par, "trainPASSinput.RData", sep=""))
cat.table <- read.table(file="Y:/Home/wangzhen/Eval_SALTO/Data/cat_table.txt", header=TRUE)
cat.table <- cat.table[order(cat.table$AREA, decreasing=F),]

ME.T <- rep(0, PASS.out$PASS.options$nGroups)
for (i.Group in 1:PASS.out$PASS.options$nGroups) {
  ME.T[i.Group] <- PASS.out$groups[[i.Group]]$overall.eff
}
igroup.best           <- which.max((ME.T))
cat.table$ME_MAX      <- ME.T[igroup.best]
cat.table$igroup.best <- igroup.best

cat.table$TRAIN                                            <- FALSE
for(i.train.ID in train.PASS.input$train.ID) {
  cat.table$TRAIN[cat.table$ID == i.train.ID] <- TRUE
}

write.table(cat.table, file=paste(dir.sim_q, "cat_table_ME.txt", sep=""))

goodme.thres  <- 0.9  # threshold to select good parameter sets (e.g.ME>0.9 MEmax)

##### READ CALIBRATION
par.bounds <- read.table("F:/Zhenyu/D_SALTO/5km_resolution/SALTO_V2_parameter.txt", header=TRUE)
if(is.factor(par.bounds$PAR_NAME)) {
  par.names  <- as.character(levels(par.bounds$PAR_NAME))[par.bounds$PAR_NAME]
} else {
  par.names  <- par.bounds$PAR_NAME
}

##### Simulate Q
start_time <- Sys.time()
numCores   <- 40
cl         <- makeCluster(numCores)
registerDoParallel(cl)

foreach(i.cat = 1:nrow(cat.table)) %dopar% {
#for(i.cat in 398:nrow(cat.table)) {
  dyn.load("Y:/Home/wangzhen/Eval_SALTO/SALTO/SALTO_MOD_V2_64.dll")  # load SALTO model
  
  cat.table.i         <- cat.table[i.cat, ]
  qobs                <- READ.QOBS(cat.table$ID[i.cat], dir.obs_q, startdate, enddate, iwarmup, cat.table$AREA[i.cat])
  model.input         <- READ.MODELINPUT(cat.table$ID[i.cat], dir.obs_grds, dir.obs_metinp, startdate, enddate)
  igrdicat            <- model.input[[1]] 
  grdsincat           <- model.input[[2]]
  prec                <- model.input[[5]]
  temp                <- model.input[[7]]
  imaxgrd             <- dim(grdsincat)[1]
  
  filename.qsim <- paste(dir.sim_q, cat.table$ID[i.cat], ".txt", sep="")
  date          <- as.Date(seq(startdate + (12 * 60 * 60), enddate + (24 * 60 * 60), by="days"), format="%d-%m-%Y")
  qsim          <- data.frame(date)
  qsim$Qobs     <- qobs
  
  if(!is.na(igrdicat) & !anyNA(prec) & !anyNA(temp)) {
    grdname             <- grdsincat$grd
    effarea             <- grdsincat$cover
    flowto              <- grdsincat$flow2
    pet                 <- model.input[[8]]
    dates               <- model.input[[9]]
    igrdicat            <- length(grdname) # BUG: For i.cat = 176, length of grdname is 6 but igrdicat is 1. 
    
    for(i.Group in 1:PASS.out$PASS.options$nGroups) {
      show(paste0("i.cat: ", i.cat, "/", nrow(cat.table), "; i.Group: ", i.Group, "/", PASS.out$PASS.options$nGroups))
      
      if(igrdicat>1) {
        param.mat <- data.frame(PASS.out$groups[[i.Group]]$regionalized.parameters$grd.par.pred[grdname,])
      } else {
        param.mat <- data.frame(t(unlist(PASS.out$groups[[i.Group]]$regionalized.parameters$grd.par.pred[grdname,])))   
      }
      colnames(param.mat) <- par.names
      if(!anyNA(param.mat)) { 
        q.out                         <- SALTO.V2(prec, temp, pet, grdname, effarea, flowto, param.mat)
        qsim[paste0("Qsim", i.Group)] <- q.out$q 
      } 
    }
    qsim <- qsim[-c(1:iwarmup), ]
    write.table(qsim, filename.qsim)
  } else {
    show(c("------",cat.table$ID[i.cat], anyNA(prec), anyNA(temp), length(qobs)))
  }
}
stopCluster(cl)
end_time <- Sys.time()
show(end_time - start_time)



