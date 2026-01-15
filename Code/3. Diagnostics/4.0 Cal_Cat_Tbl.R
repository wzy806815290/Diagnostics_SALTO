library(stringr)
###### Working Directory
setwd("Y:/Home/wangzhen/Eval_SALTO")

###### Sources
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/Eval_Metrics.R")

############################################################
###### Functions
Cal_ME <- function(obs, sim) {  
  Qres <- data.frame(obs=obs, sim=sim)
  Qres_complete <- Qres[complete.cases(Qres), ]
  if(nrow(Qres_complete) > 1) {
    ME <- KGE_Balance(Qres_complete$obs, Qres_complete$sim, 0) # change the metrics
    if(is.na(ME)) ME <- NA 
  } else {
    ME <- NA
  }
  return(ME)
} 

############################################################
## Input & output directories
dir.sim_q <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_qout_1979_2002_KGE_Balanced_Ralf/")
dir.sd_ts <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_SD_ts_1979_2002_KGE_Balanced_Ralf_TOP5/")

## Parameter initialization
cat.table <- read.table(file=paste0(dir.sim_q, "cat_table_ME.txt"), header=TRUE)
cat.table <- cat.table[order(cat.table$AREA, decreasing=T),]

for(Ev_type in c("ALL", "SI", "RD", "RW")) {
  for(ME in c("ME", "ET", "EQ")) {
    for(iparset in 1:5) {
      cat.table[, paste(Ev_type, ME, iparset, sep="_")] <- NA
    }
    cat.table[, paste(Ev_type, ME, "MEDIAN", sep="_")] <- NA
  }
}

Ev_type_list <- c("SM", "ROI", "ROS", "MIX", "R.WET.INT.LOC", "R.WET.INT.EXT", "R.WET.VOL.LOC.!OVER", "R.WET.VOL.LOC.OVER", 
                  "R.WET.VOL.EXT.!OVER", "R.WET.VOL.EXT.OVER", "R.DRY.INT.LOC.STD", "R.DRY.INT.!STD", "R.DRY.INT.EXT.STD",
                  "R.DRY.VOL.LOC", "R.DRY.VOL.EXT.STD", "R.DRY.VOL.EXT.!STD") 

sdList <- list.files(dir.sd_ts, pattern=".txt", full.names=FALSE)
sdList <- sort(sdList)
cat.ID.i_old <- as.numeric(str_extract(sdList[1], "\\d+"))

j <- 1
for(i in 1:length(sdList)) {
  sdList.i <- sdList[i]
  cat.ID.i <- as.numeric(str_extract(sdList.i, "\\d+"))
  i.cat <- which(cat.table$ID == cat.ID.i)
  parset.i <- as.numeric(str_extract(sub(".*output", "", sdList.i), "\\d+"))
  
  if(cat.ID.i != cat.ID.i_old) {
    j <- 1
  }
  print(paste(i, cat.ID.i, j, parset.i, seq=" "))

  filename.info_events <- paste0(dir.sd_ts, sdList.i)
  Qres <- read.table(file=filename.info_events, header=TRUE)
  
  for(Ev_type in c("ALL", "SI", "RD", "RW")) {
    if(Ev_type == "SI") {
      Qres.i <- Qres[(Qres$event_type >= 1 & Qres$event_type <= 4),]
    } else if(Ev_type == "RW") {
      Qres.i <- Qres[(Qres$event_type >= 5 & Qres$event_type <= 10),]
    } else if(Ev_type == "RD") {
      Qres.i <- Qres[(Qres$event_type >= 11 & Qres$event_type <= 16),]
    } else {
      Qres.i <- Qres[Qres$event_type >= 1,]
    }
    
    if(nrow(Qres.i) < 5) {
      cat.table[i.cat, paste(Ev_type, "ME", j, sep="_")] <- NA
      cat.table[i.cat, paste(Ev_type, "ET", j, sep="_")] <- NA
      cat.table[i.cat, paste(Ev_type, "EQ", j, sep="_")] <- NA
    } else {
      ME <- Cal_ME(Qres.i$obs, Qres.i$sim)
      cat.table[i.cat, paste(Ev_type, "ME", j, sep="_")] <- ME
      Qres.i.summary <- Qres.i %>%
                        group_by(event_type, event_count) %>% 
                        summarise(obs=mean(obs), et=mean(et), eq=mean(eq))
      cat.table[i.cat, paste(Ev_type, "ET", j, sep="_")] <- median(Qres.i.summary$et, na.rm = T)
      cat.table[i.cat, paste(Ev_type, "EQ", j, sep="_")] <- median(Qres.i.summary$eq, na.rm = T)
    }
  }
  cat.ID.i_old <- cat.ID.i
  j <- j + 1
}

for(Ev_type in c("ALL", "SI", "RD", "RW")) {
  for(ME in c("ME", "ET", "EQ")) {
    cat.table[, paste(Ev_type, ME, "MEDIAN", sep="_")] <- apply(cat.table[, paste(Ev_type, ME, 1:5, sep="_")], 1, median, na.rm=T)
  }
}

write.table(cat.table, file=paste(dir.sd_ts, "cat_table_ME.txt", sep=""))


