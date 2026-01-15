
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
dir.lump_par    <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_par_1990_2000_KGE_Balanced/")

## Output directories
dir.sim_q      <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_qout_1979_2002_KGE_Balanced/")

## Parameter initialization
cat.table <- read.table(file="Y:/Home/wangzhen/Eval_SALTO/Data/cat_table.txt", header=TRUE)
cat.table <- cat.table[order(cat.table$AREA, decreasing=F),]
cat.table$ME_MAX    <- -1
cat.table$iparset   <- 0
cat.table$ipop.best <- 0
cat.parameter       <- list()

goodme.thres  <- 0.9  # threshold to select good parameter sets (e.g.ME>0.9 MEmax)

##### READ CALIBRATION
par.bounds <- read.table("F:/Zhenyu/D_SALTO/5km_resolution/SALTO_V2_parameter.txt", header=TRUE)
if(is.factor(par.bounds$PAR_NAME)) {
  par.names  <- as.character(levels(par.bounds$PAR_NAME))[par.bounds$PAR_NAME]
} else {
  par.names  <- par.bounds$PAR_NAME
}

##### Simulate Q
minparset <- 5

start_time <- Sys.time()
#numCores   <- 40
#cl         <- makeCluster(numCores)
#registerDoParallel(cl)
#foreach(i.cat = 1:nrow(cat.table)) %dopar% {
for(i.cat in 1:nrow(cat.table)) {
  dyn.load("Y:/Home/wangzhen/Eval_SALTO/SALTO/SALTO_MOD_V2_64.dll")  # load SALTO model
  
  filename.cal <- paste(dir.lump_par, "DDS_", cat.table$ID[i.cat], ".txt", sep="")
  if(file.exists(filename.cal)) {
    cat.parameter <- read.table(filename.cal, header=TRUE)  # Read existed calibrated parameters
    
    iparset <- sum((cat.parameter$ME >= goodme.thres * max(cat.parameter$ME, na.rm=TRUE)) & (cat.parameter$ME > 0))
    
    if(iparset > minparset) {
      qobs                <- READ.QOBS(cat.table$ID[i.cat], dir.obs_q, startdate, enddate, iwarmup, cat.table$AREA[i.cat])
      
      model.input         <- READ.MODELINPUT(cat.table$ID[i.cat], dir.obs_grds, dir.obs_metinp, startdate, enddate)
      igrdicat            <- model.input[[1]]
      grdsincat           <- model.input[[2]]
      prec                <- model.input[[5]]
      temp                <- model.input[[7]]
      
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
        
        for(i.pop in 1:nrow(cat.parameter)) {
          show(paste0("i.cat: ", i.cat, "/", nrow(cat.table), "; i.pop: ", i.pop, "/", nrow(cat.parameter)))
          
          ME.i <- cat.parameter$ME[i.pop]
          if((ME.i >= goodme.thres * max(cat.parameter$ME, na.rm=TRUE)) & (ME.i > 0)) {
            cat.parameter.i             <- as.numeric(cat.parameter[i.pop, -1])
            param.mat                   <- data.frame(matrix(rep(cat.parameter.i, length(grdname)),
                                                             nrow = length(grdname), ncol = length(cat.parameter.i), byrow = T))
            colnames(param.mat)         <- par.names
            q.out                       <- SALTO.V2(prec, temp, pet, grdname, effarea, flowto, param.mat)
            qsim[paste0("Qsim", i.pop)] <- q.out$q 
          }
        }
        
        qsim <- qsim[-c(1:iwarmup), ]
        write.table(qsim, filename.qsim)
      } else {
        show(c("------",cat.table$ID[i.cat], anyNA(prec), anyNA(temp), length(qobs)))
      }
    }
  }
}
#stopCluster(cl)
end_time <- Sys.time()
show(end_time - start_time)

##### Summary of parameters
for (i.cat in 1:nrow(cat.table)) {
  filename.cal <- paste(dir.lump_par, "DDS_", cat.table$ID[i.cat], ".txt", sep="")
  if(file.exists(filename.cal)) {
    cat.parameter <- read.table(filename.cal, header=TRUE)  # Read existed calibrated parameters
    
    iparset <- sum((cat.parameter$ME >= goodme.thres * max(cat.parameter$ME, na.rm=TRUE)) & (cat.parameter$ME > 0))
    
    if(iparset > minparset) {
      cat.table[i.cat, 'ME_MAX']    <- max(cat.parameter$ME)
      cat.table[i.cat, 'iparset']   <- iparset
      cat.table[i.cat, 'ipop.best'] <- which.max(cat.parameter$ME)
    }
  }
}
write.table(cat.table, file=paste(dir.sim_q, "cat_table_ME.txt", sep=""))

##### Check NA errors
ntry <- 10
iparset <- 42
i.cat <- which(cat.table$ID == 25842500)
for (i.cat in 1:nrow(cat.table)) {
  filename <- paste("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_qout_1979_2002_KGE_Balanced/", cat.table$ID[i.cat], ".txt", sep="")
  if(file.exists(filename)) {
    q <- read.table(file=filename, header=TRUE)
    na_q <- colSums(is.na(q))
    if(sum(na_q) > 2000){
      na_iparset <- as.integer(gsub('[Qsim]', '', rownames(data.frame(na_q))[which(na_q > 2000)]))
      print(paste0("i.cat: ", i.cat, " CatID: ", cat.table$ID[i.cat], 
                   " iparset: ", na_iparset, " numNA: ", as.integer(na_q[which(na_q > 2000)])))
      for(iparset in na_iparset) {
        for(try in 1:ntry) {
          startdate  <- strptime("01:10:1969", format="%d:%m:%Y")
          startdate2 <- strptime("01:10:1979", format="%d:%m:%Y")
          enddate    <- strptime("31:10:2002", format="%d:%m:%Y")
          iwarmup    <- startdate2-startdate 
          imaxdays   <- ceiling(as.numeric(enddate-startdate+1))
          
          i <- as.integer(gsub('[Qsim]', '', iparset))
          
          model.input <- READ.MODELINPUT(cat.table$ID[i.cat], dir.obs_grds, dir.obs_metinp, startdate, enddate)
          igrdicat    <- model.input[[1]]
          grdsincat   <- model.input[[2]]
          prec        <- model.input[[5]]
          temp        <- model.input[[7]]
          grdname   <- grdsincat$grd
          effarea   <- grdsincat$cover
          flowto    <- grdsincat$flow2
          pet       <- model.input[[8]]
          date      <- model.input[[9]]
          
          dyn.load("Y:/Home/wangzhen/Eval_SALTO/SALTO/SALTO_MOD_V2_64.dll")  # load SALTO model
          filename <- paste("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_par_1990_2000_KGE_Balanced/DDS_", cat.table$ID[i.cat], ".txt", sep="")
          mat <- read.table(filename, header=TRUE)  # Read existed calibrated parameters
          cat.parameter.i     <- as.numeric(mat[i, -1])
          param.mat           <- data.frame(matrix(rep(cat.parameter.i, length(grdname)),
                                                   nrow = length(grdname), ncol = length(cat.parameter.i), byrow = T))
          colnames(param.mat) <- par.names
          q.out               <- SALTO.V2(prec, temp, pet, grdname, effarea, flowto, param.mat)
          qsim <- q.out$q[-c(1:iwarmup)]
          if(sum(is.na(qsim)) < 500) {
            print(sum(is.na(qsim)))
            q[paste0("Qsim", i)] <- qsim
            filename.qsim <- paste("Y:/Home/wangzhen/Eval_SALTO/Out/", cat.table$ID[i.cat], ".txt", sep="")
            write.table(q, filename.qsim)
            break
          }
        }
      }
    }
  }
}
