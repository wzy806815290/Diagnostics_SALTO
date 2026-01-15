
###### Working Directory
setwd("Y:/Home/wangzhen/Eval_SALTO")

###### Libraries
library(lubridate)
library(dplyr)
library(tidyverse)
library(foreach)
library(doParallel)
library(stringr)

###### Sources
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/Eval_Metrics.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/Eval_Metrics.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_V2.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_PASS_V2.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_DDS.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_MODEL_INPUT.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_Eventsep_DCMA_ESR.R")
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/SALTO_Baseflow_Func.R")

############################################################
###### Functions
Pre_Ev.Extract <- function(SDres, preDays) {   
  ##  wrapper for SALTO using in optimization 
  start_time <- SDres %>% 
    group_by(event_type, event_count) %>% 
    filter(!duplicated(event_type, event_count))
  Ev_start <- distinct(SDres[which(SDres$event_type > 0),], 
                       comb=paste(event_type, event_count, sep="_"), .keep_all = TRUE)
  Ev_start.event_type <- Ev_start$event_type
  Ev_start.event_count <- Ev_start$event_count
  Ev_start.time <- date(Ev_start$date)
  
  preEv_start.time <- Ev_start.time - preDays
  preEv_time.idx <- unlist(lapply(preEv_start.time, FUN=function(x) x:(x + preDays - 1)))
  preEv_time.event_type <- unlist(lapply(Ev_start.event_type, FUN=function(x) rep(x, times=preDays)))
  preEv_time.event_count <- unlist(lapply(Ev_start.event_count, FUN=function(x) rep(x, times=preDays)))
  
  preEv_time <- as.Date(preEv_time.idx)
  
  SDres.preEv <- data.frame(date=preEv_time)
  SDres.preEv <- left_join(SDres.preEv, SDres, by="date")
  SDres.preEv$event_type <- preEv_time.event_type
  SDres.preEv$event_count <- preEv_time.event_count
  SDres.preEv$hydcase <- paste0('preEv_', preDays, 'd')
  
  return(SDres.preEv)
} 


############################################################
## Input directories
dir.sim_q <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_qout_1979_2002_KGE_Balanced/")
dir.sd_ts <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_SD_ts_1979_2002_KGE_Balanced/")
dir.event_series <- c("Y:/Home/wangzhen/Eval_SALTO/Out/event_type_1979_2002/")   
dir.lump_par    <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_par_1990_2000_KGE_Balanced/")
dir.obs_metinp <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Input_TS_1951_2020_HYDRAS/")
dir.obs_grds   <- c("Y:/Home/wangzhen/Eval_SALTO/Data/Grds/")

## Output directories
dir.info_events   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_info_events_1979_2002_KGE_Balanced/")

## Running period
startdate  <- strptime("01:10:1969", format="%d:%m:%Y")
startdate2 <- strptime("01:10:1979", format="%d:%m:%Y")
enddate    <- strptime("31:10:2002", format="%d:%m:%Y")
iwarmup    <- startdate2-startdate 
imaxdays   <- ceiling(as.numeric(enddate-startdate+1))

##### READ CALIBRATION
par.bounds <- read.table("Y:/Home/wangzhen/Eval_SALTO/Data/SALTO_V2_parameter.txt", header=TRUE)
if(is.factor(par.bounds$PAR_NAME)) {
  par.names  <- as.character(levels(par.bounds$PAR_NAME))[par.bounds$PAR_NAME]
} else {
  par.names  <- par.bounds$PAR_NAME
}

## Parameter initialization
cat.table <- read.table(file=paste0(dir.sim_q, "cat_table_ME.txt"), header=TRUE)
cat_CDs   <- read.table("Y:/Home/wangzhen/Eval_SALTO/Data/cats_CDs.txt", header=TRUE)

Ev_type_list <- c("SM", "ROI", "ROS", "MIX", "R.WET.INT.LOC", "R.WET.INT.EXT", "R.WET.VOL.LOC.!OVER", "R.WET.VOL.LOC.OVER", 
                  "R.WET.VOL.EXT.!OVER", "R.WET.VOL.EXT.OVER", "R.DRY.INT.LOC.STD", "R.DRY.INT.!STD", "R.DRY.INT.EXT.STD",
                  "R.DRY.VOL.LOC", "R.DRY.VOL.EXT.STD", "R.DRY.VOL.EXT.!STD") 

season <- data.frame(start_month=c(0, 2, 5, 8, 11), 
                     season=c("Winter", "Spring", "Summer", "Autumn", "Winter"))

## Merge tables
SD_SDpre_PAR_Ev_events.all <- NULL

dir.lumped_sd_ts   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_SD_ts_1979_2002_KGE_Balanced/")
dir.dist_sd_ts   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_SD_ts_1979_2002_KGE_Balanced_Ralf_TOP5/")

cat.table.lumped <- read.table(file=paste(dir.lumped_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)
cat.table.dist <- read.table(file=paste(dir.dist_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)

ids_lumped <- cat.table.lumped$ID[cat.table.lumped$ME_MAX >= 0.3]
ids_dist   <- cat.table.dist$ID[cat.table.dist$ME_MAX >= 0.3]
unique_ids <- intersect(ids_lumped, ids_dist)

sdList <- list.files(dir.sd_ts, pattern=".txt", full.names=FALSE)
ids <- sub("_.*", "", sdList)
sdList <- sdList[ids %in% unique_ids]

numCores   <- 40
start_time <- Sys.time()
cal.param  <- list()
cl         <- makeCluster(numCores, outfile='stdout')
registerDoParallel(cl)

SD_SDpre_PAR_Ev_events.all <- foreach(i=1:length(sdList), .combine=rbind, .packages=c('lubridate', 'tidyverse')) %dopar% {
  sdList.i <- sdList[i]
  cat.ID.i <- as.numeric(str_extract(sdList.i, "\\d+"))
  parset.i <- as.numeric(str_extract(sub(".*output", "", sdList.i), "\\d+"))
  
  filename.sd <- paste0(dir.sd_ts, paste0(cat.ID.i, "_output", parset.i, ".txt"))
  filename.event_series <- paste0(dir.event_series, cat.ID.i, ".txt")
  filename.cal <- paste(dir.lump_par, "DDS_", cat.ID.i, ".txt", sep="")
  if(file.exists(filename.sd) & file.exists(filename.event_series) & file.exists(filename.cal)) {
    # SD: Series Distance (Errors in time and runoff)
    SD_series <- read.table(file=filename.sd, header=TRUE)
    SD_series$date <- date(SD_series$date)
    
    # PAR: Model parameters (lumped)
    dyn.load("Y:/Home/wangzhen/Eval_SALTO/SALTO/SALTO_MOD_V2_64.dll")  # load SALTO model
    model.input         <- READ.MODELINPUT(cat.ID.i, dir.obs_grds, dir.obs_metinp, startdate, enddate)
    igrdicat            <- model.input[[1]]
    grdsincat           <- model.input[[2]]
    prec                <- model.input[[5]]
    temp                <- model.input[[7]]
    
    grdname             <- grdsincat$grd
    effarea             <- grdsincat$cover
    flowto              <- grdsincat$flow2
    pet                 <- model.input[[8]]
    dates               <- model.input[[9]]
    imaxgrd             <- dim(grdsincat)[1]
    
    Par <- read.table(filename.cal, header=TRUE)  # Read existed calibrated parameters
    Par.table.i <- Par[parset.i, ] 
    Par.table.i <- as.numeric(Par.table.i[, -grep("ME", colnames(Par.table.i))])
    param.mat <- data.frame(matrix(rep(Par.table.i, length(grdname)),
                                   nrow = length(grdname), ncol=length(Par.table.i), byrow = T))
    colnames(param.mat) <- par.names
    q.out <- SALTO.V2(prec, temp, pet, grdname, effarea, flowto, param.mat)
    
    path <- data.frame(index=1:length(grdname), grdname=grdname, flowto=flowto)
    lag <- 0
    path$lag <- lag
    path$weight <- 0
    path$weight[length(grdname)] <- 100
    grdname.flowb <- path$grdname[length(grdname)]
    while(min(path$weight) == 0) {
      lag <- lag - 1
      grdname.flowb.new <- NULL
      for(grdname.flowb.i in grdname.flowb) {
        idx <- which(path$grdname == grdname.flowb.i | path$flowto == grdname.flowb.i)
        path$weight[idx] <- path$weight[which(path$grdname == grdname.flowb.i)] / length(idx)
        path$lag[which(path$flowto == grdname.flowb.i)] <- lag
        grdname.flowb.new <- c(grdname.flowb.new, path$grdname[which(path$flowto == grdname.flowb.i)])
      }
      grdname.flowb <- grdname.flowb.new
    }
    path$weight <- path$weight / sum(path$weight)
    
    Grd_mean <- function(df, path) {
      if(length(dim(df)) > 2) {
        df <- drop(df)
      }
      for(i in 1:nrow(path)) {
        if(path$lag[i] != 0) {
          df[, i] <- c(rep(0, -path$lag[i]), head(df[, i], path$lag[i])) * path$weight[i]
        }
      }
      return(apply(df, 1, function(x) sum(x, na.rm=T)))
    }
    
    Diff_state <- function(mat) {
      if(length(dim(mat)) > 2) {
        mat <- drop(mat)
      }
      d <- diff(mat, lag = 1)
      return(rbind(rep(0, ncol(mat)), d))
    }
    
    date <- as.Date(seq(startdate + (12 * 60 * 60), enddate + (24 * 60 * 60), by="days"), format="%d-%m-%Y")
    Par_series <- data.frame(date)
    Par_series$Env_prec <- Grd_mean(q.out$prec, path)
    Par_series$Env_temp <- Grd_mean(q.out$temp, path)
    Par_series$Env_pet <- Grd_mean(q.out$pet, path)
    
    Par_series$PAR_swe <- Grd_mean(Diff_state(q.out$swe), path) # Snow-Water-Equivalent [mm]
    Par_series$PAR_sm <- Grd_mean(Diff_state(q.out$sm), path)   # soil moisture [mm/timestep]
    Par_series$PAR_aet <- Grd_mean(q.out$aet, path) # actual evapotranspiration [mm/timestep]
    Par_series$PAR_sm_perc <- Grd_mean(q.out$sm_perc, path) # soil moisture percolation in the next soil layer [mm/timestep]
    Par_series$PAR_lf_s <- Grd_mean(Diff_state(q.out$lf_s), path)   # fast runoff storage [mm]
    Par_series$PAR_lf_q <- Grd_mean(q.out$lf_q, path)   # fast runoff [mm/timestep]
    Par_series$PAR_gw_s <- Grd_mean(Diff_state(q.out$gw_s), path)   # groundwater storage [mm]
    Par_series$PAR_gwr <- Grd_mean(q.out$gwr, path)     # groundwater recharge [mm]
    Par_series$PAR_gw2gw <- Grd_mean(q.out$gw2gw, path) # groundwater discharge to gw [mm/timestep]
    Par_series$PAR_gw2r <- Grd_mean(q.out$gw2r, path)   # groundwater outflow to river[mm/timestep]
    Par_series$PAR_river_s <- Grd_mean(Diff_state(q.out$river_s), path) # river storage [mm]
    Par_series <- Par_series[-c(1:iwarmup), ]
    write.table(Par_series, file=paste0('Y:/Home/wangzhen/Eval_SALTO/Out/lumped_fluxes_1979_2002_KGE_Balanced/', paste0(cat.ID.i, "_output", parset.i, ".txt")))
    
    # Combine SD and Par
    SD_PAR_series <- left_join(SD_series, Par_series, by=c("date"))
    
    # Ev: Runoff Event Indicators
    Ev_series <- read.table(file=filename.event_series, header=TRUE)
    Ev_series$date <- date(Ev_series$date)
    Ev_series <- Ev_series[, -grep("event_", colnames(Ev_series))]
    colnames(Ev_series) <- paste0("Ev_", colnames(Ev_series))
    
    # Combine SD_PAR and Ev
    SD_PAR_Ev_series <- left_join(SD_PAR_series, Ev_series, by=c("date"="Ev_date"))
    SD_PAR_Ev_series$year <- year(SD_PAR_Ev_series$date)
    SD_PAR_Ev_series$month <- month(SD_PAR_Ev_series$date)
    SD_PAR_Ev_series <- SD_PAR_Ev_series[-which(SD_PAR_Ev_series$event_type < 1),] 
    SD_PAR_Ev_events <- SD_PAR_Ev_series %>% 
                          group_by(event_type, event_count) %>% 
                          dplyr::summarise(year=round(mean(year), 0), month=round(mean(month), 0), across(et:Ev_frozen.cover.cov, mean, na.rm=T))
    SD_PAR_Ev_events[which(SD_PAR_Ev_events$event_type >= 1 & SD_PAR_Ev_events$event_type <= 4), "event_type2"] <- "S&I"
    SD_PAR_Ev_events[which(SD_PAR_Ev_events$event_type >= 5 & SD_PAR_Ev_events$event_type <= 10), "event_type2"] <- "R.WET"
    SD_PAR_Ev_events[which(SD_PAR_Ev_events$event_type >= 11 & SD_PAR_Ev_events$event_type <= 16), "event_type2"] <- "R.DRY"
    
    # SD preEvents
    SD_series.preEv_1d <- Pre_Ev.Extract(SD_series, 1)
    SD_events.preEv_1d <- SD_series.preEv_1d %>% 
                            group_by(event_type, event_count) %>% 
                            dplyr::summarise(across(et:eq, mean, na.rm=T))
    colnames(SD_events.preEv_1d) <- paste0("preEv_1d_", colnames(SD_events.preEv_1d))
    
    SD_series.preEv_5d <- Pre_Ev.Extract(SD_series, 5)
    SD_events.preEv_5d <- SD_series.preEv_5d %>% 
      group_by(event_type, event_count) %>% 
      dplyr::summarise(across(et:eq, mean, na.rm=T))
    colnames(SD_events.preEv_5d) <- paste0("preEv_5d_", colnames(SD_events.preEv_5d))
    
    SD_series.preEv_10d <- Pre_Ev.Extract(SD_series, 10)
    SD_events.preEv_10d <- SD_series.preEv_10d %>% 
      group_by(event_type, event_count) %>% 
      dplyr::summarise(across(et:eq, mean, na.rm=T))
    colnames(SD_events.preEv_10d) <- paste0("preEv_10d_", colnames(SD_events.preEv_10d))
    
    SDpre_events <- left_join(SD_events.preEv_1d, SD_events.preEv_5d, 
                              by=c("preEv_1d_event_type"="preEv_5d_event_type", 
                                   "preEv_1d_event_count"="preEv_5d_event_count"))
    SDpre_events <- left_join(SDpre_events, SD_events.preEv_10d, 
                              by=c("preEv_1d_event_type"="preEv_10d_event_type", 
                                   "preEv_1d_event_count"="preEv_10d_event_count"))
    
    # Combine SD_Ev and SD_pre
    SD_SDpre_PAR_Ev_events <- left_join(SD_PAR_Ev_events, SDpre_events, 
                                        by=c("event_type"="preEv_1d_event_type", 
                                             "event_count"="preEv_1d_event_count"))
    
    SD_SDpre_PAR_Ev_events$ID <- cat.ID.i
    SD_SDpre_PAR_Ev_events$iparset <- parset.i
    SD_SDpre_PAR_Ev_events$AREA <- cat.table[which(cat.table$ID == cat.ID.i), 'AREA']
    
    write.table(SD_SDpre_PAR_Ev_events, file=paste0(dir.info_events, paste0(cat.ID.i, "_output", parset.i, ".txt")))

    return(SD_SDpre_PAR_Ev_events)
  }
}
stopCluster(cl)
end_time <- Sys.time()
show(end_time - start_time)

write.table(SD_SDpre_PAR_Ev_events.all, file=paste0(dir.info_events, "SD_SDpre_PAR_Ev_all.txt"))
