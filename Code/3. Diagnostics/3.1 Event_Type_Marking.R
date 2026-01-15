##### PACKAGES AND SOURCES
setwd("Y:/Home/wangzhen/Eval_SALTO/")   # set working directory

library(foreach)
library(doParallel)
library(lubridate)
library(reshape2)
library(plyr)
library(dplyr)
library(gtools)

##############################################
##### Functions
CATEGORIZE <- function(Eresult) {
  ## SM: 1
  ## ROI: 2
  ## ROS: 3
  ## MIX: 4
  ## R.WET.Int.LOC: 5
  ## R.WET.Int.EXT: 6
  ## R.WET.VOL.LOC.!OVER: 7
  ## R.WET.VOL.LOC.OVER: 8
  ## R.WET.VOL.EXT.!OVER: 9
  ## R.WET.VOL.EXT.OVER: 10
  ## R.DRY.INT.LOC.STD: 11
  ## R.DRY.INT.!STD: 12
  ## R.DRY.INT.EXT.STD: 13
  ## R.DRY.VOL.LOC: 14
  ## R.DRY.VOL.EXT.STD: 15
  ## R.DRY.VOL.EXT.!STD: 16
  
  Eresult$event_type[Eresult$ind.type == "SM"]  <- 1
  Eresult$event_type[Eresult$ind.type == "RoI"] <- 2
  Eresult$event_type[Eresult$ind.type == "RoS"] <- 3
  Eresult$event_type[Eresult$ind.type == "Mix"] <- 4
  
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "wet") & (Eresult$temp.type == "Int") & (Eresult$st.type1 == "Loc")] <- 5
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "wet") & (Eresult$temp.type == "Int") & (Eresult$st.type1 == "Ext")] <- 6
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "wet") & (Eresult$temp.type == "Vol") & (Eresult$st.type1 == "Loc") & (Eresult$wet.type3 == "No")] <- 7
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "wet") & (Eresult$temp.type == "Vol") & (Eresult$st.type1 == "Loc") & ((Eresult$wet.type2 == "uniform") | (Eresult$wet.type3 == "Overlap"))] <- 8
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "wet") & (Eresult$temp.type == "Vol") & (Eresult$st.type1 == "Ext") & (Eresult$wet.type3 == "No")] <- 9
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "wet") & (Eresult$temp.type == "Vol") & (Eresult$st.type1 == "Ext") & ((Eresult$wet.type2 == "uniform") | (Eresult$wet.type3 == "Overlap"))] <- 10
  
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "dry") & (Eresult$temp.type == "Int") & (Eresult$st.type2 == "Steady") & (Eresult$st.type1 == "Loc")] <- 11
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "dry") & (Eresult$temp.type == "Int") & (Eresult$st.type2 == "Unsteady") ] <- 12
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "dry") & (Eresult$temp.type == "Int") & (Eresult$st.type2 == "Steady") & (Eresult$st.type1 == "Ext")] <- 13
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "dry") & (Eresult$temp.type == "Vol") & (Eresult$st.type1 == "Loc")] <- 14
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "dry") & (Eresult$temp.type == "Vol") & (Eresult$st.type1 == "Ext") & (Eresult$st.type2 == "Steady")] <- 15
  Eresult$event_type[(Eresult$ind.type == "Rain") & (Eresult$wet.type1 == "dry") & (Eresult$temp.type == "Vol") & (Eresult$st.type1 == "Ext") & (Eresult$st.type2 == "Unsteady")] <- 16
  
  return(Eresult)
}

############################################################
###### Event-type marking
## Calibration period
startdate  <- strptime("01:10:1969", format="%d:%m:%Y")
startdate2 <- strptime("01:10:1979", format="%d:%m:%Y")
enddate    <- strptime("31:10:2002", format="%d:%m:%Y")
iwarmup    <- startdate2-startdate 
imaxdays   <- ceiling(as.numeric(enddate-startdate+1))

## Input directories
dir.event_tbl    <- c("Y:/Home/wangzhen/Eval_SALTO/Data/types/")  

## Output directories
dir.event_series <- c("Y:/Home/wangzhen/Eval_SALTO/Out/event_type_1979_2002/")                                    

######## Read catchment info
cat.table <- read.table(file="Y:/Home/wangzhen/Eval_SALTO/Data/cat_table.txt", header=TRUE)
cat.table <- cat.table[order(cat.table$AREA, decreasing=F),]
preE_days <- 10
Etypes    <- c("SM", "ROI", "ROS", "MIX", "R.WET.Int.LOC", "R.WET.Int.EXT", "R.WET.VOL.LOC.!OVER", "R.WET.VOL.LOC.OVER", 
               "R.WET.VOL.EXT.!OVER", "R.WET.VOL.EXT.OVER", "R.DRY.INT.LOC.STD", "R.DRY.INT.!STD", "R.DRY.INT.EXT.STD",
               "R.DRY.VOL.LOC", "R.DRY.VOL.EXT.STD", "R.DRY.VOL.EXT.!STD") 
nEtypes   <- length(Etypes)

######## Mark the event-type in time series
start_time  <- Sys.time()
numCores    <- 40
cl          <- makeCluster(numCores)
registerDoParallel(cl)

foreach(i.cat = 1:nrow(cat.table), .packages=c("lubridate", "plyr", "dplyr")) %dopar% {
#for(i.cat in 1:nrow(cat.table)) {
  show(paste0("i.cat: ", i.cat, "/", nrow(cat.table)))
  
  filename.event <- paste(dir.event_tbl, cat.table$ID[i.cat], ".txt", sep="")
  date           <- as.Date(seq(startdate2 + (12 * 60 * 60), enddate + (24 * 60 * 60), by="days"), format="%d-%m-%Y")
  
  ## Read event-types and info
  Eresult                 <- read.table(filename.event, header=TRUE)            # Read event parameters
  Eresult                 <- Eresult[!is.na(Eresult$date),]                     # Remove data with NA date
  Eresult$date            <- as.Date(Eresult$date)
  Eresult$E_startdate     <- Eresult$date - days(Eresult$t2pd - 1)              # Start date of the event
  Eresult$E_enddate       <- Eresult$E_startdate + days(Eresult$Qduration - 1)  # End date of the event
  Eresult$preE_startdate  <- Eresult$E_startdate - days(preE_days)              # Start date of the pre-event
  Eresult                 <- Eresult[which((Eresult$preE_startdate > startdate2) & (Eresult$E_enddate < enddate)), ] # Select data with pre-event startdate > sim startdate and event enddate < sim enddate
  Eresult                 <- CATEGORIZE(Eresult)                                # Categorize the events based on the event-type info
  Eresult                 <- Eresult[which(!is.na(Eresult$event_type)), ]
  if(is.null(Eresult$frozen.cover.cov)) {
    Eresult$frozen.cover.cov <- NA
  }

  ## Mark event-types and info
  if(nrow(Eresult) > 0) {
    Eseries                   <- data.frame(date)
    Eseries$event_type        <- 0
    Eseries$event_count       <- 0
    Eseries$event_type        <- 0
    Eseries$event_count       <- 0
    Eseries$Qduration         <- 0
    Eseries$t2pd              <- 0
    Eseries$peak              <- 0
    Eseries$temp.cv           <- 0
    Eseries$Pmax.vol.ratio    <- 0
    Eseries$space_cv          <- 0
    Eseries$mean_RM_ant       <- 0
    Eseries$ant_sm            <- 0
    Eseries$space_cv_sm       <- 0
    Eseries$cov_SM_RM_ts_mean <- 0
    Eseries$snow.cover.cov    <- 0
    Eseries$volumeP           <- 0
    Eseries$volumeR           <- 0
    Eseries$volumeM           <- 0
    Eseries$frozen.cover.cov  <- 0
    
    for(i.event_type in 1:nEtypes) {
      show(paste0("event_type: ", i.event_type))
      event_count <- 1
      Eresult.i   <- Eresult[which(Eresult$event_type == i.event_type), ]
      
      if(nrow(Eresult.i) > 0) {
        dateseq.E             <- NULL
        event_count_seq       <- NULL
        Qduration_seq         <- NULL
        t2pd_seq              <- NULL
        peak_seq              <- NULL
        temp.cv_seq           <- NULL
        Pmax.vol.ratio_seq    <- NULL
        space_cv_seq          <- NULL
        mean_RM_ant_seq       <- NULL
        ant_sm_seq            <- NULL
        space_cv_sm_seq       <- NULL
        cov_SM_RM_ts_mean_seq <- NULL
        snow.cover.cov_seq    <- NULL
        volumeP_seq           <- NULL
        volumeR_seq           <- NULL
        volumeM_seq           <- NULL
        frozen.cover.cov_seq  <- NULL
        
        for(i.row in 1:nrow(Eresult.i)) {
          dateseq.E.i           <- seq(Eresult.i$E_startdate[i.row], Eresult.i$E_enddate[i.row], by="days")
          dateseq.E             <- as.Date(c(dateseq.E, dateseq.E.i), format="%Y-%m-%d")
          event_count_seq       <- c(event_count_seq, rep(event_count, length(dateseq.E.i)))
          Qduration_seq         <- c(Qduration_seq, rep(Eresult.i$Qduration[i.row], length(dateseq.E.i)))
          t2pd_seq              <- c(t2pd_seq, rep(Eresult.i$t2pd[i.row], length(dateseq.E.i)))
          peak_seq              <- c(peak_seq, rep(Eresult.i$peak[i.row], length(dateseq.E.i)))
          temp.cv_seq           <- c(temp.cv_seq, rep(Eresult.i$temp.cv[i.row], length(dateseq.E.i)))
          Pmax.vol.ratio_seq    <- c(Pmax.vol.ratio_seq, rep(Eresult.i$Pmax.vol.ratio[i.row], length(dateseq.E.i)))
          space_cv_seq          <- c(space_cv_seq, rep(Eresult.i$space_cv[i.row], length(dateseq.E.i)))
          mean_RM_ant_seq       <- c(mean_RM_ant_seq, rep(Eresult.i$mean_RM_ant[i.row], length(dateseq.E.i)))
          ant_sm_seq            <- c(ant_sm_seq, rep(Eresult.i$ant_sm[i.row], length(dateseq.E.i)))
          space_cv_sm_seq       <- c(space_cv_sm_seq, rep(Eresult.i$space_cv_sm[i.row], length(dateseq.E.i)))
          cov_SM_RM_ts_mean_seq <- c(cov_SM_RM_ts_mean_seq, rep(Eresult.i$cov_SM_RM_ts_mean[i.row], length(dateseq.E.i)))
          snow.cover.cov_seq    <- c(snow.cover.cov_seq, rep(Eresult.i$snow.cover.cov[i.row], length(dateseq.E.i)))
          volumeP_seq           <- c(volumeP_seq, rep(Eresult.i$volumeP[i.row], length(dateseq.E.i)))
          volumeR_seq           <- c(volumeR_seq, rep(Eresult.i$volumeR[i.row], length(dateseq.E.i)))
          volumeM_seq           <- c(volumeM_seq, rep(Eresult.i$volumeM[i.row], length(dateseq.E.i)))
          frozen.cover.cov_seq  <- c(frozen.cover.cov_seq, rep(Eresult.i$frozen.cover.cov[i.row], length(dateseq.E.i)))
          
          event_count <- event_count + 1
        }
        
        dftmp.E <- data.frame("date"=as.Date(dateseq.E), "event_type"=i.event_type, "event_count"=event_count_seq,
                              "Qduration"=Qduration_seq, "t2pd"=t2pd_seq, "peak"=peak_seq,
                              "temp.cv"=temp.cv_seq, "Pmax.vol.ratio"=Pmax.vol.ratio_seq, "space_cv"=space_cv_seq,
                              "mean_RM_ant"=mean_RM_ant_seq, "ant_sm"=ant_sm_seq, "space_cv_sm"=space_cv_sm_seq,
                              "cov_SM_RM_ts_mean"=cov_SM_RM_ts_mean_seq, "snow.cover.cov"=snow.cover.cov_seq, "volumeP"=volumeP_seq,
                              "volumeR"=volumeR_seq, "volumeM"=volumeM_seq, "frozen.cover.cov"=frozen.cover.cov_seq)
        
        Eseries <- ddply(merge(Eseries, dftmp.E, by="date", all.x=TRUE) %>% mutate(event_type.y = ifelse(is.na(event_type.y), 0, event_type.y), event_count.y = ifelse(is.na(event_count.y), 0, event_count.y),
                                                                                   Qduration.y = ifelse(is.na(Qduration.y), 0, Qduration.y), t2pd.y = ifelse(is.na(t2pd.y), 0, t2pd.y),
                                                                                   peak.y = ifelse(is.na(peak.y), 0, peak.y), temp.cv.y = ifelse(is.na(temp.cv.y), 0, temp.cv.y),
                                                                                   Pmax.vol.ratio.y = ifelse(is.na(Pmax.vol.ratio.y), 0, Pmax.vol.ratio.y), space_cv.y = ifelse(is.na(space_cv.y), 0, space_cv.y),
                                                                                   mean_RM_ant.y = ifelse(is.na(mean_RM_ant.y), 0, mean_RM_ant.y), ant_sm.y = ifelse(is.na(ant_sm.y), 0, ant_sm.y),
                                                                                   space_cv_sm.y = ifelse(is.na(space_cv_sm.y), 0, space_cv_sm.y), cov_SM_RM_ts_mean.y = ifelse(is.na(cov_SM_RM_ts_mean.y), 0, cov_SM_RM_ts_mean.y),
                                                                                   snow.cover.cov.y = ifelse(is.na(snow.cover.cov.y), 0, snow.cover.cov.y), volumeP.y = ifelse(is.na(volumeP.y), 0, volumeP.y),
                                                                                   volumeR.y = ifelse(is.na(volumeR.y), 0, volumeR.y), volumeM.y = ifelse(is.na(volumeM.y), 0, volumeM.y), 
                                                                                   frozen.cover.cov.y = ifelse(is.na(frozen.cover.cov.y), 0, frozen.cover.cov.y)), 
                         .(date), summarise, event_type=event_type.x + event_type.y, event_count=event_count.x + event_count.y, Qduration=Qduration.x + Qduration.y,
                         t2pd=t2pd.x + t2pd.y, peak=peak.x + peak.y, temp.cv=temp.cv.x + temp.cv.y, Pmax.vol.ratio=Pmax.vol.ratio.x + Pmax.vol.ratio.y, space_cv=space_cv.x + space_cv.y,
                         mean_RM_ant=mean_RM_ant.x + mean_RM_ant.y, ant_sm=ant_sm.x + ant_sm.y, space_cv_sm=space_cv_sm.x + space_cv_sm.y, cov_SM_RM_ts_mean=cov_SM_RM_ts_mean.x + cov_SM_RM_ts_mean.y,
                         snow.cover.cov=snow.cover.cov.x + snow.cover.cov.y, volumeP=volumeP.x + volumeP.y, volumeR=volumeR.x + volumeR.y, volumeM=volumeM.x + volumeM.y, frozen.cover.cov=frozen.cover.cov.x + frozen.cover.cov.y)
      }
    }
  }
  
  filename.event_series <- paste(dir.event_series, cat.table$ID[i.cat], ".txt", sep="")
  write.table(Eseries, filename.event_series)
}

stopCluster(cl)
end_time <- Sys.time()
show(end_time - start_time)

