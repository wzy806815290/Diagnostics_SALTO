
###### Working Directory
setwd("Y:/Home/wangzhen/Eval_SALTO")

###### Libraries
library(foreach)
library(doParallel)
library(raster)
library(zoo)

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
set.seed(987)

###### PASS - Distributed Calibration
## Calibrated parameter bounds
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

## Calibration period
startdate  <- strptime("01:11:1985", format="%d:%m:%Y")
startdate2 <- strptime("01:11:1990", format="%d:%m:%Y")
enddate    <- strptime("31:10:2000", format="%d:%m:%Y")
iwarmup    <- startdate2-startdate 
imaxdays   <- ceiling(as.numeric(enddate - startdate + 1))

## Input directories
dir.obs_metinp <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Input_TS_1951_2020_HYDRAS/")
dir.obs_grds   <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Grds/")
dir.obs_q      <- c("Y:/Home/wangzhen/Eval_SALTO/Data/qobs_new/")
dir.lump_par    <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_par_1990_2000_KGE_Balanced/")

## Output directories
dir.dist_par <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_par_1990_2000_KGE_Balanced/")

## Parameter initialization
cat.table <- read.table(file="Y:/Home/wangzhen/Eval_SALTO/Data/cat_table.txt", header=TRUE)
cat.table <- cat.table[order(cat.table$AREA, decreasing=F),]
cat_CDs   <- read.table("Y:/Home/wangzhen/Eval_SALTO/Data/cats_CDs.txt", header=TRUE)
grds_CDs  <- read.table("Y:/Home/wangzhen/Eval_SALTO/Data/grds_CDs.txt", header=TRUE)
goodme.thres  <- 0.9 # threshold to select good parameter sets (e.g.ME>0.9 MEmax)
master.raster  <- raster("Y:/Home/wangzhen/Eval_SALTO/Data/master_raster_5km.tif", convert=TRUE)

## Read grid data
ina <- NULL
for(icat in 1:nrow(cat.table)) {
  filename.grds <- paste(dir.obs_grds, cat.table$ID[icat], "_upgrds.txt", sep="")
  if(file.exists(filename.grds)) {
    grdsincat <- read.table(file=filename.grds, header=TRUE)
    for(igrd in 1:nrow(grdsincat)) {
      if(anyNA(grds_CDs[grdsincat$grd[igrd],])) {
        neighbours <- c(master.raster[(grdsincat$row[igrd] - 1):(grdsincat$row[igrd] + 1), 
                                      (grdsincat$col[igrd] - 1):(grdsincat$col[igrd] + 1), 1]) 
        cd.na      <- which(is.na(grds_CDs[grdsincat$grd[igrd],]))
        for (icd in cd.na) {
          grds_CDs[grdsincat$grd[igrd], icd] <- mean(grds_CDs[neighbours, icd], na.rm=T) 
        }
      }
    }
  } else {
    ina <- c(ina, icat)
  }
}
show(ina)
cat.table <- cat.table[-ina,]
cat_CDs  <- cat_CDs[match(cat.table$ID, cat_CDs$ID), ]

## Read lumped-calibrated parameters 
cat.table$ME      <- NA
cat.table$iparset <- 0
cat.parameter <- list()
for (i.cat in 1:nrow(cat.table)) {
  filename.cal <- paste(dir.lump_par, "DDS_", cat.table$ID[i.cat], ".txt", sep="")
  if(file.exists(filename.cal) ) {
    results <- read.table(filename.cal, header=TRUE)
    results <- results[complete.cases(results), ] # delete rows with NAs
    colnames(results) <- c("ME", par.names)
    results <- results[results$ME >= goodme.thres * max(results$ME, na.rm=TRUE),]  ## only good ones
    cat.parameter[[i.cat]] <- results
    if(nrow(results) > 0) {
      cat.table$ME[i.cat]      <- max(results$ME)
      cat.table$iparset[i.cat] <- dim(results)[1] 
    }
  }
}

## Training preparation
minparset <- 20
minMEmax  <- 0.75
maxAREA   <- 10000.
icattake <- which(cat.table$ME > minMEmax & cat.table$iparset >= minparset & 
                    cat.table$AREA <= maxAREA) 
show(c("pot. training catchments:", length(icattake)))

num.traincat    <- 100
isample         <- sample(icattake, num.traincat, replace=FALSE)
train.ID        <- cat.table$ID[isample]
train.AREA      <- cat.table$AREA[isample]
train.CD        <- cat_CDs[isample,]
train.parameter <- list()
for (icat in 1:length(train.ID)) {
  itake                   <- isample[icat]
  train.parameter[[icat]] <- cat.parameter[[itake]]
}
cat.table$train <- F
cat.table$train[isample] <- T

numCores <- 30
cl<-makeCluster(numCores)
registerDoParallel(cl)
train.input  <- list()
train.input  <- foreach(icat=1:length(train.ID)) %dopar% { 
  show(c(icat," out of ",length(train.ID)))
  itake <- isample[icat]
  qobs  <- READ.QOBS(cat.table$ID[itake], dir.obs_q, startdate, enddate, iwarmup, cat.table$AREA[itake])
  model.input <- READ.MODELINPUT(cat.table$ID[itake], dir.obs_grds, dir.obs_metinp, startdate, enddate)
  imaxgrd   <- model.input[[1]]
  grdsincat <- model.input[[2]]
  upsneighb <- model.input[[3]]
  area.correction <- model.input[[4]]
  prec      <- model.input[[5]]
  temp      <- model.input[[7]]
  pet       <- model.input[[8]]
  dates     <- model.input[[9]]
  INPUT.cat <- list()
  INPUT.cat[[1]] <- grdsincat   
  INPUT.cat[[2]] <- data.frame(prec)
  INPUT.cat[[3]] <- data.frame(temp)
  INPUT.cat[[4]] <- data.frame(pet)
  INPUT.cat[[5]] <- qobs
  INPUT.cat[[6]] <- NA
  k <- baseflow_RecessionConstant(qobs)
  BFImax<- baseflow_BFImax(qobs, k)
  baseflow <- baseflow_Eckhardt(qobs, BFImax, k)
  INPUT.cat[[6]] <- baseflow  
  return(INPUT.cat)
}
stopCluster(cl)

train.PASS.input <- list()
train.PASS.input[["train.ID"]]          <- train.ID
train.PASS.input[["train.AREA"]]        <- train.AREA   
train.PASS.input[["train.CD"]]          <- train.CD  
train.PASS.input[["train.input"]]       <- train.input  
train.PASS.input[["train.parameter"]]   <- train.parameter  
save(train.PASS.input, file=paste(dir.dist_par, "trainPASSinput.RData", sep=""))

## Training start
load(file=paste(dir.dist_par, "trainPASSinput.RData", sep=""))
train.ID        <- train.PASS.input[["train.ID"]]   
train.AREA      <- train.PASS.input[["train.AREA"]]    
train.CD        <- train.PASS.input[["train.CD"]]     
train.input     <- train.PASS.input[["train.input"]]    
train.parameter <- train.PASS.input[["train.parameter"]]    
show(c("number of available parameter sets:", sum(sapply(train.parameter, function(x) nrow(x)))))
show(c("median ME of training catchments:", median(sapply(train.parameter, function(x) median(x[,c("ME")])))))

# random sampling
PASS.out <- PASS.2(Y=train.parameter,        # list (N.cat) of data.frames (XXX x N.par) with locally lumped calibrated model parameters OR previously obtained PASS output
                   X.cat=train.CD,           # matrix or data.frame (N.cat x N.dsc) of catchment descriptors
                   X.grd=grds_CDs,           # matrix or data.frame (N.grd x N.dsc) of model unit/pixel descriptors
                   model.input=train.input,  # list (N.cat) of model Input containing  4 lists, respectively:
                                             # data.frame with grdname, effarea, flowto as colums and N.grd rows
                                             # prec(itimesteps, N.grds), temp(itimesteps, N.grds), pet(itimesteps, N.grds), qobs(itimesteps)
                   model.eff.fn="Q",         # the function to be optimized (maximized). The function should have as its first argument the vector/matrix of real-valued
                                             #  parameters to optimize, as second argument the catchment index, and return a scalar real result with maximum equal to 1. 
                                             #  'NA' and 'NaN' values are not allowed.
                   iwarmup=iwarmup,          # warmu-up period (in timesteps), for which ME is not calulcated
                   lower=par.min,            # two vectors specifying scalar real lower and upper bounds on each parameter to be optimized, so that the i-th element
                   upper=par.max,            #  of 'lower' and 'upper' applies to the i-th parameter.
                   options=PASS.options(maxLoops=50, nGroups=5, REGloops=15, 
                                        generalized.mean.power=-1, proportion.max.eff.update=goodme.thres, 
                                        sampling='random', optim.subset.cat=0.3, eff.reg.weight=1.5, 
                                        numcat.beta=c(1, 1), reg.meth="DT") # options for PASS
)
save(PASS.out, file=paste(dir.dist_par, "PASSout.RData", sep=""))

load(file=paste(dir.dist_par, "PASSout.RData", sep=""))
PASS.out <- PASS.2(Y=PASS.out,            # list (N.cat) of data.frames (XXX x N.par) with locally lumped calibrated model parameters OR previously obtained PASS output
                   X.cat=train.CD,        # matrix or data.frame (N.cat x N.dsc) of catchment descriptors
                   X.grd=grds_CDs,        # matrix or data.frame (N.grd x N.dsc) of model unit/pixel descriptors
                   model.input=train.input,  # list (N.cat) of model Input containing  4 lists, respectively:
                   # data.frame with grdname,effarea,flowto as colums and N.grd rows
                   #  prec(itimesteps, N.grds), temp(itimesteps, N.grds), pet(itimesteps, N.grds), qobs(itimesteps)
                   model.eff.fn="Q", # the function to be optimized (maximized). The function should have as its first argument the vector/matrix of real-valued
                   #  parameters to optimize, as second argument the catchment index, and return a scalar real result with maximum equal to 1. 
                   #  'NA' and 'NaN' values are not allowed.
                   iwarmup=iwarmup, # warmu-up period (in timesteps), for which ME is not calulcated
                   lower=par.min,        # two vectors specifying scalar real lower and upper bounds on each parameter to be optimized, so that the i-th element
                   upper=par.max,        #  of 'lower' and 'upper' applies to the i-th parameter.
                   options=PASS.options(maxLoops=50, nGroups=5, REGloops=5, 
                                        generalized.mean.power=-1, proportion.max.eff.update=goodme.thres, 
                                        sampling='random', optim.subset.cat=0.3,eff.reg.weight=1.5,numcat.beta=c(1,1),reg.meth="DT") # options for PASS
)
save(PASS.out, file=paste(dir.dist_par, "PASSout.RData", sep=""))

for (ii in 1:20) { 
  
  PASS.out <- PASS.2(Y=PASS.out ,            # list (N.cat) of data.frames (XXX x N.par) with locally lumped calibrated model parameters OR previously obtained PASS output
                     X.cat=train.CD,        # matrix or data.frame (N.cat x N.dsc) of catchment descriptors
                     X.grd=grds_CDs,        # matrix or data.frame (N.grd x N.dsc) of model unit/pixel descriptors
                     model.input=train.input,  # list (N.cat) of model Input containing  4 lists, respectively:
                     # data.frame with grdname,effarea,flowto as colums and N.grd rows
                     #  prec(itimesteps, N.grds), temp(itimesteps, N.grds), pet(itimesteps, N.grds), qobs(itimesteps)
                     model.eff.fn="Q", # the function to be optimized (maximized). The function should have as its first argument the vector/matrix of real-valued
                     #  parameters to optimize, as second argument the catchment index, and return a scalar real result with maximum equal to 1. 
                     #  'NA' and 'NaN' values are not allowed.
                     iwarmup=iwarmup, # warmu-up period (in timesteps), for which ME is not calulcated
                     lower=par.min,        # two vectors specifying scalar real lower and upper bounds on each parameter to be optimized, so that the i-th element
                     upper=par.max,        #  of 'lower' and 'upper' applies to the i-th parameter.
                     options=PASS.options(maxLoops=20, nGroups=5, REGloops=7, 
                                          generalized.mean.power=-1, proportion.max.eff.update=goodme.thres, 
                                          sampling='optim', optim.subset.cat=0.6,eff.reg.weight=1.5,
                                          numcat.beta=c(1,1.1),reg.meth="DT") # options for PASS
  )
  save(PASS.out, file=paste(dir.dist_par, "PASSout.RData", sep=""))
}

for (ii in 1:10) { 
  PASS.out <- PASS.2(Y=PASS.out ,            # list (N.cat) of data.frames (XXX x N.par) with locally lumped calibrated model parameters OR previously obtained PASS output
                     X.cat=train.CD,        # matrix or data.frame (N.cat x N.dsc) of catchment descriptors
                     X.grd=grds_CDs,        # matrix or data.frame (N.grd x N.dsc) of model unit/pixel descriptors
                     model.input=train.input,  # list (N.cat) of model Input containing  4 lists, respectively:
                     # data.frame with grdname,effarea,flowto as colums and N.grd rows
                     #  prec(itimesteps, N.grds), temp(itimesteps, N.grds), pet(itimesteps, N.grds), qobs(itimesteps)
                     model.eff.fn="Q", # the function to be optimized (maximized). The function should have as its first argument the vector/matrix of real-valued
                     #  parameters to optimize, as second argument the catchment index, and return a scalar real result with maximum equal to 1. 
                     #  'NA' and 'NaN' values are not allowed.
                     iwarmup=iwarmup, # warmu-up period (in timesteps), for which ME is not calulcated
                     lower=par.min,        # two vectors specifying scalar real lower and upper bounds on each parameter to be optimized, so that the i-th element
                     upper=par.max,        #  of 'lower' and 'upper' applies to the i-th parameter.
                     options=PASS.options(maxLoops=20, nGroups=5, REGloops=3, 
                                          generalized.mean.power=-1, proportion.max.eff.update=goodme.thres, 
                                          sampling='optim', optim.subset.cat=0.3,eff.reg.weight=1.5,numcat.beta=c(1,1.2),reg.meth="DT") # options for PASS
  )
  save(PASS.out, file=paste(dir.dist_par, "PASSout.RData", sep=""))
}



