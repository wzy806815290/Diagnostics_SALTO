
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
SALTO.lumped.optim <- function(param.t) {   
  ##  wrapper for SALTO using in optimization 
  ##  input: lumped parameters (param.t) 
  ##  output: Model efficiency (ME) to be minimized, in the moment ME = 1.- KGE
  
  param.mat <- data.frame(matrix(rep(as.numeric(param.t), length(grdname)), nrow=length(grdname), ncol=length(param.t), byrow = T))
  colnames(param.mat) <- par.names
  q.out <- SALTO.V2(prec, temp, pet,
                    grdname, effarea, flowto, 
                    param.mat)
  
  if(length(q.out) > 1) {
    qsim <- q.out$q
    ME <- KGE_Balance(qobs, qsim, iwarmup)
    if(is.na(ME)) ME <- -1. 
  } else {
    ME <- -1.
  }
  ME_inv <- 1.- ME
  
  return(ME_inv)
} 

############################################################
set.seed(987)

###### Calibration
## Calibration period
startdate  <- strptime("01:11:1985", format="%d:%m:%Y")
startdate2 <- strptime("01:11:1990", format="%d:%m:%Y")
enddate    <- strptime("31:10:2000", format="%d:%m:%Y")
iwarmup    <- startdate2-startdate 
imaxdays   <- ceiling(as.numeric(enddate-startdate+1))

## Input directories
dir.obs_metinp <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Input_TS_1951_2020_HYDRAS/")
dir.obs_grds   <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Grds/")
dir.obs_q      <- c("Y:/Home/wangzhen/Eval_SALTO/Data/qobs_new/")

## Output directories
dir.lump_par <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_par_1990_2000_KGE_Balanced/")

## Parameter initialization
cat.table <- read.table(file="Y:/Home/wangzhen/Eval_SALTO/Data/cat_table.txt", header=TRUE)
cat.table <- cat.table[order(cat.table$AREA, decreasing=F),]

# Calibrated parameter bounds
par.bounds <- read.table("Y:/Home/wangzhen/Eval_SALTO/Data/SALTO_V2_parameter.txt",header=TRUE)
if(is.factor(par.bounds$PAR_NAME)) {
  par.names <- as.character(levels(par.bounds$PAR_NAME))[par.bounds$PAR_NAME]
} else {
  par.names <- par.bounds$PAR_NAME
}
par.min    <- par.bounds$PAR_MIN
par.max    <- par.bounds$PAR_MAX
par.sens   <- which(par.max - par.min >= 0.000001) # Sensitive parameter => param.min != param.max, for not sensitive param = param_min
par.i      <- nrow(par.bounds)

############################################################
###### Calibration of lumped parameters
## Other parameters for calibration
nPopsize <- 50    # number of calibrated parameter sets
nIter_dds <- 800  # iterations of DDS calibration

## Parameters for parallel operation
numCores   <- 40
start_time <- Sys.time()
cal.param  <- list()
cl         <- makeCluster(numCores, outfile='stdout')
registerDoParallel(cl)

## Run calibration
cal.param <- foreach(icat=1:nrow(cat.table), .combine=list, .multicombine = TRUE) %dopar% {
  dyn.load("Y:/Home/wangzhen/Eval_SALTO/SALTO/SALTO_MOD_V2_64.dll")  # load SALTO model
  
  parameter.out <- array(NA, dim=c(nPopsize, par.i + 1))
  istart <- 1
  colnames(parameter.out) <- c("ME", par.names)
  filename.cal <- paste(dir.lump_par, "DDS_", cat.table$ID[icat], ".txt", sep="")
  cal <- TRUE
  
  if(file.exists(filename.cal) ) {
    parameter.out <- read.table(filename.cal, header=TRUE)
    
    colnames(parameter.out) <- c("ME", par.names)
    parameter.out.complete <- parameter.out[complete.cases(parameter.out), ]
    
    # if the number of saved calibrated parameter sets > nPopsize
    if(nrow(parameter.out.complete) >= nPopsize) { 
      cal <- FALSE
    }
    
    # if there are all bad calibrated MEs 
    if((nrow(parameter.out.complete) > nPopsize / 2) & (max(parameter.out.complete[, 1]) <= 0)) { 
      cal <- FALSE
    }
    istart <- nrow(parameter.out.complete) + 1 
  } 
  
  if(cal) {
    qobs <- READ.QOBS(cat.table$ID[icat], dir.obs_q, startdate, enddate, iwarmup, cat.table$AREA[icat])
    model.input <- READ.MODELINPUT(cat.table$ID[icat], dir.obs_grds, dir.obs_metinp, startdate, enddate)
    igrdicat    <- model.input[[1]]
    grdsincat   <- model.input[[2]]
    prec        <- model.input[[5]]
    temp        <- model.input[[7]]
    
    if(!is.na(igrdicat) & !anyNA(prec) & !anyNA(temp) & dim(prec)[1] == length(qobs) & sum(qobs, na.rm=T) > 0.1) {
      show(c("Start calibration"))
      grdname     <- grdsincat$grd
      effarea     <- grdsincat$cover
      flowto      <- grdsincat$flow2
      pet         <- model.input[[8]]
      dates       <- model.input[[9]]
      
      for (irun in istart:nPopsize) {
        dds.results <- dds(par.bounds[, -1], nIter_dds, SALTO.lumped.optim)
        ME          <- 1 - dds.results[[2]][nIter_dds]
        if(is.na(ME)) {
          ME <- -1.
        } 
        
        parameter.out[irun, 1]  <- ME
        parameter.out[irun, -1] <- dds.results[[1]][nIter_dds,]
        write.table(parameter.out, file=filename.cal, row.name=FALSE)
        show(c(irun, 1 - dds.results[[2]][nIter_dds] ))
      }
    } else {
      show(c("------",cat.table$ID[icat], anyNA(prec), anyNA(temp), length(qobs)))
    }
  } 
}
stopCluster(cl)
end_time <- Sys.time()
show(end_time - start_time)
