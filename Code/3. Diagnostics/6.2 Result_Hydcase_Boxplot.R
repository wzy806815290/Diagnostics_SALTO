
###### Working Directory----
setwd("Y:/Home/wangzhen/Eval_SALTO")

###### Libraries----
library(ggplot2)
library(ggbreak)

library(foreach)
library(doParallel)
library(RColorBrewer)
library(cowplot)
library(tidyverse)
library(dplyr)
library(lubridate)

############################################################
###### Functions----
Pre_Ev.Extract <- function(SDres, preDays) {   
  ##  wrapper for SALTO using in optimization 
  start_time <- SDres %>% 
                  group_by(event_type, event_count) %>% 
                  filter(!duplicated(event_type, event_count))
  Ev_start <- distinct(SDres[which(SDres$event_type > 0),], 
                       comb=paste(event_type, event_count, sep="_"), .keep_all = TRUE)
  Ev_start.event_type <- Ev_start$event_type
  Ev_start.event_count <- Ev_start$event_count
  Ev_start.time <- as.Date(Ev_start$date)
  
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
###### Summerize the data----
## Input directories
dir_sd <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_SD_ts_1979_2002_KGE_Balanced/")

## Output directories
dir_plot <- c("F:/Home/wangzhen/Eval_SALTO/Plot/")

## Parameter initialization
cat.table <- read.table(file="Y:/Home/wangzhen/Eval_SALTO/Data/cat_table.txt", header=TRUE)

Ev_type_list <- c("SM", "ROI", "ROS", "MIX", "R.WET.INT.LOC", "R.WET.INT.EXT", "R.WET.VOL.LOC.!OVER", "R.WET.VOL.LOC.OVER", 
                  "R.WET.VOL.EXT.!OVER", "R.WET.VOL.EXT.OVER", "R.DRY.INT.LOC.STD", "R.DRY.INT.!STD", "R.DRY.INT.EXT.STD",
                  "R.DRY.VOL.LOC", "R.DRY.VOL.EXT.STD", "R.DRY.VOL.EXT.!STD") 

## Read grid data in parallel operation
SDres_summary <- NULL

numCores   <- 20
start_time <- Sys.time()
cal.param  <- list()
cl         <- makeCluster(numCores, outfile='stdout')
registerDoParallel(cl)

sdList <- list.files(dir_sd, pattern=".txt", full.names=FALSE)
sdList <- sort(sdList)
sdList <- sdList[-length(sdList)]

SDres_summary <- foreach(i=1:length(sdList), .combine=rbind, .packages = c("stringr", "dplyr")) %dopar% {
#for(i in 1:length(sdList)) {
  sdList.i <- sdList[i]
  cat.ID.i <- as.numeric(str_extract(sdList.i, "\\d+"))
  #i.cat <- which(cat.table$ID == cat.ID.i)
  parset.i <- as.numeric(str_extract(sub(".*output", "", sdList.i), "\\d+"))
  
  print(sdList.i)
  
  filename.sd <- paste0(dir_sd, sdList.i)
  SDres <- read.table(file=filename.sd, header=TRUE)
  SDres$date <- as.Date(SDres$date)
  
  # Table with Events
  SDres$Ev_type <- NA
  SDres.Ev <- SDres[-which(SDres$event_type < 1),]
  SDres.Ev[(SDres.Ev$event_type >= 1 & SDres.Ev$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
  SDres.Ev[(SDres.Ev$event_type >= 5 & SDres.Ev$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
  SDres.Ev[(SDres.Ev$event_type >= 11 & SDres.Ev$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"
  
  #SDres$hydcase[SDres$hydcase == -2] <- 'Valley'
  SDres.Ev$hydcase[SDres.Ev$hydcase == -1] <- 'Recession'
  SDres.Ev$hydcase[SDres.Ev$hydcase == 1] <- 'Rising'
  #SDres.Ev$hydcase[(SDres.Ev$hydcase == 2 | SDres.Ev$hydcase == -2)] <- 'Peak'
  SDres.Ev$hydcase[SDres.Ev$hydcase == 2] <- 'Peak'
  SDres.Ev$hydcase[SDres.Ev$hydcase == -2] <- 'Trough'
  
  # Table with Last Event distance
  SDres.Ev <- SDres.Ev %>%
                mutate(date_diff = date - lag(date, default = first(date)))
  
  # Table with Pre-events
  SDres.preEv_1d <- Pre_Ev.Extract(SDres, 1)
  SDres.preEv_1d[(SDres.preEv_1d$event_type >= 1 & SDres.preEv_1d$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
  SDres.preEv_1d[(SDres.preEv_1d$event_type >= 5 & SDres.preEv_1d$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
  SDres.preEv_1d[(SDres.preEv_1d$event_type >= 11 & SDres.preEv_1d$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"
  
  SDres.preEv_5d <- Pre_Ev.Extract(SDres, 5)
  SDres.preEv_5d[(SDres.preEv_5d$event_type >= 1 & SDres.preEv_5d$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
  SDres.preEv_5d[(SDres.preEv_5d$event_type >= 5 & SDres.preEv_5d$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
  SDres.preEv_5d[(SDres.preEv_5d$event_type >= 11 & SDres.preEv_5d$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"
  
  SDres.preEv_10d <- Pre_Ev.Extract(SDres, 10)
  SDres.preEv_10d[(SDres.preEv_10d$event_type >= 1 & SDres.preEv_10d$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
  SDres.preEv_10d[(SDres.preEv_10d$event_type >= 5 & SDres.preEv_10d$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
  SDres.preEv_10d[(SDres.preEv_10d$event_type >= 11 & SDres.preEv_10d$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"
  
  SDres <- bind_rows(SDres.Ev, SDres.preEv_1d, SDres.preEv_5d, SDres.preEv_10d)
  if(sum(SDres$hydcase == 0) > 0) {
    SDres <- SDres[-which(SDres$hydcase == 0), ]
  }
  SDres$ID <- cat.ID.i
  SDres$iparset <- parset.i
  
  #SDres_summary <- rbind(SDres_summary, SDres)
  return(SDres)
}
stopCluster(cl)
end_time <- Sys.time()
show(end_time - start_time)

save(SDres_summary, file=paste(dir_sd, "Info_hydcase_Ev_AND_preEv.RData", sep=""))

############################################################
###### Boxplot of errors in event types----
## Input directories
dir.dist_sd <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_SD_ts_1979_2002_KGE_Balanced_Ralf_TOP5/")
dir.lump_sd <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_SD_ts_1979_2002_KGE_Balanced/")

## Output directories
dir.plot <- c("Y:/Home/wangzhen/Eval_SALTO/Plot/")

load(file=paste0(dir.dist_sd, "Info_hydcase_Ev_AND_preEv.RData"))
SDres_summary.dist <- SDres_summary
SDres_summary.dist$Par_type <- "Reg"
load(file=paste0(dir.lump_sd, "Info_hydcase_Ev_AND_preEv.RData"))
SDres_summary.lump <- SDres_summary
SDres_summary.lump$Par_type <- "Lumped"
SDres_summary <- rbind(SDres_summary.dist, SDres_summary.lump)

#rm(list=c("SDres_summary.dist", "SDres_summary.lump"))
#gc()

#SDres_summary <- SDres_summary[complete.cases(SDres_summary),]
#save(SDres_summary, file=paste("Y:/Home/wangzhen/Eval_SALTO/Out/Info_hydcase_Ev_AND_preEv_ALL.RData", sep=""))
SDres_summary <- SDres_summary[which(SDres_summary$hydcase != "preEv_10d" & SDres_summary$hydcase != "preEv_5d"),]
SDres_summary$Ev_type <- factor(SDres_summary$Ev_type, 
                               levels=c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet"))
SDres_summary$hydcase <- factor(SDres_summary$hydcase, 
                               levels=c("preEv_1d", "Rising", "Peak", "Recession", "Trough"))

dir.lumped_sd_ts   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_SD_ts_1979_2002_KGE_Balanced/")
dir.dist_sd_ts   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_SD_ts_1979_2002_KGE_Balanced_Ralf_TOP5/")

cat.table.lumped <- read.table(file=paste(dir.lumped_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)
cat.table.dist <- read.table(file=paste(dir.dist_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)

ids_lumped <- cat.table.lumped$ID[cat.table.lumped$ME_MAX >= 0.3]
ids_dist   <- cat.table.dist$ID[cat.table.dist$ME_MAX >= 0.3]
unique_ids <- intersect(ids_lumped, ids_dist)

SDres_summary <- SDres_summary[which(SDres_summary$ID %in% unique_ids),]

##### Trough analysis
# SDres_summary_trough <- SDres_summary %>% 
#   group_by(event_type, event_count, ID, iparset, Par_type) %>% 
#   mutate(ntrough=sum(hydcase == 'Trough'), npeak=sum(hydcase == 'Peak'), duration=as.numeric(max(date)-min(date)-1))
# 
# SDres_summary_multipeak <- SDres_summary_trough[which(SDres_summary_trough$hydcase != "preEv_1d"), ] %>% 
#   group_by(npeak, Ev_type) %>% 
#   summarise(obs_median = median(obs, na.rm=T), obs_mad=mad(obs, na.rm=T), duration_median=median(duration, na.rm=T), duration_mad=mad(duration, na.rm=T),
#             et_median = median(et, na.rm=T), et_mad=mad(et, na.rm=T), et_5=quantile(et, 0.05, na.rm=T), et_95=quantile(et, 0.95, na.rm=T),
#             eq_median=median(eq, na.rm=T), eq_mad=mad(eq, na.rm=T), eq_5=quantile(eq, 0.05, na.rm=T), eq_95=quantile(eq, 0.95, na.rm=T),
#             n=n())
# SDres_summary_multipeak <- SDres_summary_multipeak %>%
#   group_by(Ev_type) %>% 
#   mutate(perc_n=n/sum(n) * 100)
# 
# SDres_summary_multipeak <- SDres_summary_multipeak %>% 
#   ungroup() %>% 
#   complete(npeak, Ev_type)

#### Elasticity
SDres_summary_event_tmp <- SDres_summary %>% 
  group_by(event_type, event_count, ID, iparset, Par_type, hydcase) %>% 
  summarise(et_mean=mean(et, na.rm=T), eq_mean=mean(eq, na.rm=T)) %>% 
  ungroup() %>% group_by(event_type, event_count, ID, iparset, Par_type) %>% mutate(count=n())

SDres_summary_event_tmp[(SDres_summary_event_tmp$event_type >= 1 & SDres_summary_event_tmp$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
SDres_summary_event_tmp[(SDres_summary_event_tmp$event_type >= 5 & SDres_summary_event_tmp$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
SDres_summary_event_tmp[(SDres_summary_event_tmp$event_type >= 11 & SDres_summary_event_tmp$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"


library(MASS)

x <- SDres_summary_event_tmp$eq_mean[SDres_summary_event_tmp$hydcase == 'preEv_1d' & 
                                       SDres_summary_event_tmp$count > 4 ]
                                       #SDres_summary_event_tmp$Ev_type == "Rain-on-Dry"]
y <- SDres_summary_event_tmp$eq_mean[SDres_summary_event_tmp$hydcase == 'Peak' & 
                                       SDres_summary_event_tmp$count > 4]
                                       #SDres_summary_event_tmp$Ev_type == "Rain-on-Dry"]

valid_idx <- (x !=0) & (y != 0)

log_x <- log(abs(x[valid_idx]))
log_y <- log(abs(y[valid_idx]))

model <-  rlm(y ~ x)
summary(model)

# Plot scatter plot with regression line
plot(x, y, pch = 16, col = "grey", xlab = "X Values", ylab = "Y Values", main = "Linear Regression Plot")
abline(model, col = "red", lwd = 2)  # Add regression line



SDres_summary_event <- SDres_summary[which(SDres_summary$hydcase != "preEv_1d"), ] %>% 
  group_by(event_type, event_count, ID, iparset, Par_type) %>% 
  summarise(ntrough=sum(hydcase == 'Trough'), npeak=sum(hydcase == 'Peak'),
    et_mean=mean(et, na.rm=T), eq_mean=mean(eq, na.rm=T), 
    obs_mean=mean(obs, na.rm=T), obs_mad=mad(obs, na.rm=T),
         duration=as.numeric(max(date)-min(date)))

SDres_summary_event[(SDres_summary_event$event_type >= 1 & SDres_summary_event$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
SDres_summary_event[(SDres_summary_event$event_type >= 5 & SDres_summary_event$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
SDres_summary_event[(SDres_summary_event$event_type >= 11 & SDres_summary_event$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"

SDres_summary_multipeak_event <- SDres_summary_event[which(SDres_summary_event$npeak > 0), ] %>%
  mutate(npeak = case_when(npeak >=7 ~ 7,
                            TRUE ~ npeak)) %>%
  group_by(npeak, Ev_type) %>%
  summarise(et_median = median(et_mean, na.rm=T), et_mad=mad(et_mean, na.rm=T), et_25=quantile(et_mean, 0.25, na.rm=T), et_75=quantile(et_mean, 0.75, na.rm=T),
            eq_median=median(eq_mean, na.rm=T), eq_mad=mad(eq_mean, na.rm=T), eq_25=quantile(eq_mean, 0.25, na.rm=T), eq_75=quantile(eq_mean, 0.75, na.rm=T),
            n=n())
SDres_summary_multipeak_event$Ev_type <- factor(SDres_summary_multipeak_event$Ev_type, levels=c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet"))

SDres_summary_multipeak_event <- SDres_summary_multipeak_event %>%
  group_by(Ev_type) %>%
  mutate(perc_n=n/sum(n) * 100)

SDres_summary_multipeak_event <- SDres_summary_multipeak_event %>%
  ungroup() %>%
  complete(npeak, Ev_type)

# Et
segment_data = data.frame(x = c(1:7),
                          xend = c(1:7), 
                          y = -3,
                          yend = 1)

p_et <- ggplot(SDres_summary_multipeak_event) +
  geom_hline(aes(yintercept = y), 
             data.frame(y = seq(-3, 1, 1)),
             color = "lightgrey") + 
  geom_bar(aes(x = npeak,
               y = et_median,
               fill = Ev_type,
               #colour = Ev_type,
               alpha = log10(n)),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  geom_errorbar(aes(x=npeak, ymin=et_25, ymax=et_75, alpha=log10(n), group=Ev_type), width=0.2 ,colour="grey40", size=0.5, 
                stat = 'identity', position=position_dodge(width=0.9)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_continuous(name="Et", breaks=seq(-3, 1, 1), limits=c(-4.6, 1))+
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  #scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                           xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = 1, 
           label = "1", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0, 
           label = "0", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = -1, 
           label = "-1", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = -2, 
           label = "-2", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = -3, 
           label = "-3", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())

ggsave(filename=paste0(dir.plot, "npeak_et.png"), width = 15, height = 15, units = "cm", plot=p_et)
ggsave(filename=paste0(dir.plot, "npeak_et.pdf"), width = 15, height = 15, units = "cm", plot=p_et)


# Eq
segment_data = data.frame(x = c(1:7),
                          xend = c(1:7), 
                          y = -0.3,
                          yend = 0.6)

p_eq <- ggplot(SDres_summary_multipeak_event) +
  geom_hline(aes(yintercept = y),
             data.frame(y = seq(-0.3, 0.6, 0.15)),
             color = "lightgrey") +
  geom_bar(aes(x = npeak,
               y = eq_median,
               fill = Ev_type,
               #colour = Ev_type,
               alpha = log10(n)),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  geom_errorbar(aes(x=npeak, ymin=eq_25, ymax=eq_75, alpha=log10(n), group=Ev_type), width=0.2 ,colour="grey40", size=0.5, 
                stat = 'identity', position=position_dodge(width=0.9)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_continuous(name="Et", breaks=seq(-0.3, 0.6, 0.15), limits=c(-0.66, 0.6))+
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  #scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                           xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = 0.6, 
           label = "0.6", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0.3, 
           label = "0.3", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0, 
           label = "0", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = -0.3, 
           label = "-0.3", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())


ggsave(filename=paste0(dir.plot, "npeak_eq.png"), width = 15, height = 15, units = "cm", plot=p_eq)
ggsave(filename=paste0(dir.plot, "npeak_eq.pdf"), width = 15, height = 15, units = "cm", plot=p_eq)


SDres_summary_trough <- SDres_summary[which(SDres_summary$hydcase != "preEv_1d"), ] %>% 
  group_by(event_type, event_count, ID, iparset, Par_type) %>% 
  mutate(ntrough=sum(hydcase == 'Trough'), npeak=sum(hydcase == 'Peak'), 
         duration=as.numeric(max(date)-min(date))) %>% 
  ungroup() %>%
  group_by(event_type, event_count, ID, iparset, Par_type, hydcase)  %>%
  summarise(ntrough2=mean(ntrough, na.rm=T), npeak2=mean(npeak, na.rm=T), duration2=mean(duration, na.rm=T),
            et2=mean(et, na.rm=T), eq2=mean(eq, na.rm=T))  

SDres_summary_trough2 <- SDres_summary_trough[which(SDres_summary_trough$npeak2 > 0), ] %>%
  group_by(event_type, event_count, ID, iparset, Par_type) %>% 
  summarise(ntrough3=mean(ntrough2, na.rm=T), npeak3=mean(npeak2, na.rm=T), duration3=mean(duration2, na.rm=T),
            diff_rising_peak_et=et2[hydcase == 'Peak'] - et2[hydcase == 'Rising'], 
            diff_recession_peak_et=et2[hydcase == 'Peak'] - et2[hydcase == 'Recession'], 
            diff_rising_peak_eq=eq2[hydcase == 'Peak'] - eq2[hydcase == 'Rising'], 
            diff_recession_peak_eq=eq2[hydcase == 'Peak'] - eq2[hydcase == 'Recession'])

SDres_summary_trough2[(SDres_summary_trough2$event_type >= 1 & SDres_summary_trough2$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
SDres_summary_trough2[(SDres_summary_trough2$event_type >= 5 & SDres_summary_trough2$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
SDres_summary_trough2[(SDres_summary_trough2$event_type >= 11 & SDres_summary_trough2$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"

SDres_summary_multipeak <- SDres_summary_trough2 %>% 
  mutate(npeak3 = case_when(npeak3 >=7 ~ 7,
                            TRUE ~ npeak3)) %>%
  group_by(npeak3, Ev_type) %>% 
  summarise(duration_median=median(duration3, na.rm=T), duration_mad=mad(duration3, na.rm=T), duration_25=quantile(duration3, 0.25, na.rm=T), duration_75=quantile(duration3, 0.75, na.rm=T),
            diff_rising_peak_et_median=median(diff_rising_peak_et, na.rm=T), diff_rising_peak_et_25=quantile(diff_rising_peak_et, 0.25, na.rm=T), diff_rising_peak_et_75=quantile(diff_rising_peak_et, 0.75, na.rm=T),
            diff_recession_peak_et_median=median(diff_recession_peak_et, na.rm=T), diff_recession_peak_et_25=quantile(diff_recession_peak_et, 0.25, na.rm=T), diff_recession_peak_et_75=quantile(diff_recession_peak_et, 0.75, na.rm=T),
            diff_rising_peak_eq_median=median(diff_rising_peak_eq, na.rm=T), diff_rising_peak_eq_25=quantile(diff_rising_peak_eq, 0.25, na.rm=T), diff_rising_peak_eq_75=quantile(diff_rising_peak_eq, 0.75, na.rm=T),
            diff_recession_peak_eq_median=median(diff_recession_peak_eq, na.rm=T), diff_recession_peak_eq_25=quantile(diff_recession_peak_eq, 0.25, na.rm=T), diff_recession_peak_eq_75=quantile(diff_recession_peak_eq, 0.75, na.rm=T),
            n=n())
SDres_summary_multipeak <- SDres_summary_multipeak %>%
  group_by(Ev_type) %>% 
  mutate(perc_n=n/sum(n) * 100)

SDres_summary_multipeak <- SDres_summary_multipeak %>% 
  ungroup() %>% 
  complete(npeak3, Ev_type)
SDres_summary_multipeak$Ev_type <- factor(SDres_summary_multipeak$Ev_type, levels=c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet"))


# diff_rising_peak_et
segment_data = data.frame(x = c(1:7),
                          xend = c(1:7), 
                          y = -1.5,
                          yend = 1)

p_diff_rising_peak_et <- ggplot(SDres_summary_multipeak) +
  geom_hline(aes(yintercept = y),
             data.frame(y = seq(-1.5, 1, 0.5)),
             color = "lightgrey") +
  geom_bar(aes(x = npeak3,
               y = diff_rising_peak_et_median,
               fill = Ev_type,
               #colour = Ev_type,
               alpha = log10(n)),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  geom_errorbar(aes(x=npeak3, ymin=diff_rising_peak_et_25, ymax=diff_rising_peak_et_75, alpha=log10(n), group=Ev_type), width=0.2 ,colour="grey40", size=0.5, 
                stat = 'identity', position=position_dodge(width=0.9)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_continuous(name="diff_rising_peak_et", breaks=seq(-1.5, 1, 0.5), limits = c(-2.5, 1)) +
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  #scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                 xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = -1, 
           label = "-1", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0, 
           label = "0", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 1, 
           label = "1", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())

ggsave(filename=paste0(dir.plot, "npeak_diff_rising_peak_et.png"), width = 15, height = 15, units = "cm", plot=p_diff_rising_peak_et)
ggsave(filename=paste0(dir.plot, "npeak_diff_rising_peak_et.pdf"), width = 15, height = 15, units = "cm", plot=p_diff_rising_peak_et)


# diff_recession_peak_et
segment_data = data.frame(x = c(1:7),
                          xend = c(1:7), 
                          y = -1.5,
                          yend = 1)

p_diff_recession_peak_et <- ggplot(SDres_summary_multipeak) +
  geom_hline(aes(yintercept = y),
             data.frame(y = seq(-1.5, 1, 0.5)),
             color = "lightgrey") +
  geom_bar(aes(x = npeak3,
               y = diff_recession_peak_et_median,
               fill = Ev_type,
               #colour = Ev_type,
               alpha = log10(n)),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  geom_errorbar(aes(x=npeak3, ymin=diff_recession_peak_et_25, ymax=diff_recession_peak_et_75, alpha=log10(n), group=Ev_type), width=0.2 ,colour="grey40", size=0.5, 
                stat = 'identity', position=position_dodge(width=0.9)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_continuous(name="diff_rising_peak_et", breaks=seq(-1.5, 1, 0.5), limits = c(-2.5, 1)) +
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  #scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                           xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = -1, 
           label = "-1", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0, 
           label = "0", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())

ggsave(filename=paste0(dir.plot, "npeak_diff_recession_peak_et.png"), width = 15, height = 15, units = "cm", plot=p_diff_recession_peak_et)
ggsave(filename=paste0(dir.plot, "npeak_diff_recession_peak_et.pdf"), width = 15, height = 15, units = "cm", plot=p_diff_recession_peak_et)


# diff_rising_peak_eq
segment_data <- data.frame(x = c(1:7),
                          xend = c(1:7), 
                          y = -0.2,
                          yend = 0.6)

p_diff_rising_peak_eq <- ggplot(SDres_summary_multipeak) +
  geom_hline(aes(yintercept = y),
             data.frame(y = seq(-0.2, 0.6, 0.2)),
             color = "lightgrey") +
  geom_bar(aes(x = npeak3,
               y = diff_rising_peak_eq_median,
               fill = Ev_type,
               #colour = Ev_type,
               alpha = log10(n)),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  geom_errorbar(aes(x=npeak3, ymin=diff_rising_peak_eq_25, ymax=diff_rising_peak_eq_75, alpha=log10(n), group=Ev_type), width=0.2 ,colour="grey40", size=0.5, 
                stat = 'identity', position=position_dodge(width=0.9)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_continuous(name="diff_rising_peak_et", breaks=seq(-0.2, 0.6, 0.2), limits = c(-0.52, 0.6)) +
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  #scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                           xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = -0.2, 
           label = "-0.2", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0, 
           label = "0", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0.2, 
           label = "0.2", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())

ggsave(filename=paste0(dir.plot, "npeak_diff_rising_peak_eq.png"), width = 15, height = 15, units = "cm", plot=p_diff_rising_peak_eq)
ggsave(filename=paste0(dir.plot, "npeak_diff_rising_peak_eq.pdf"), width = 15, height = 15, units = "cm", plot=p_diff_rising_peak_eq)


# diff_recession_peak_eq
segment_data <- data.frame(x = c(1:7),
                           xend = c(1:7), 
                           y = -0.2,
                           yend = 0.6)

p_diff_recession_peak_eq <- ggplot(SDres_summary_multipeak) +
  geom_hline(aes(yintercept = y),
             data.frame(y = seq(-0.2, 0.6, 0.2)),
             color = "lightgrey") +
  geom_bar(aes(x = npeak3,
               y = diff_recession_peak_eq_median,
               fill = Ev_type,
               #colour = Ev_type,
               alpha = log10(n)),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  geom_errorbar(aes(x=npeak3, ymin=diff_recession_peak_eq_25, ymax=diff_recession_peak_eq_75, alpha=log10(n), group=Ev_type), width=0.2 ,colour="grey40", size=0.5, 
                stat = 'identity', position=position_dodge(width=0.9)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_continuous(name="diff_rising_peak_et", breaks=seq(-0.2, 0.6, 0.2), limits = c(-0.52, 0.6)) +
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  #scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                           xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = -0.2, 
           label = "-0.2", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0, 
           label = "0", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0.2, 
           label = "0.2", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 0.4, 
           label = "0.4", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())


ggsave(filename=paste0(dir.plot, "npeak_diff_recession_peak_eq.png"), width = 15, height = 15, units = "cm", plot=p_diff_recession_peak_eq)
ggsave(filename=paste0(dir.plot, "npeak_diff_recession_peak_eq.pdf"), width = 15, height = 15, units = "cm", plot=p_diff_recession_peak_eq)


# duration
segment_data <- data.frame(x = c(1:7),
                          xend = c(1:7), 
                          y = 0,
                          yend = 60)

p_duration <- ggplot(SDres_summary_multipeak) +
  geom_hline(aes(yintercept = y),
             data.frame(y = seq(0, 60, 20)),
             color = "lightgrey") +
  geom_bar(aes(x = npeak3,
               y = duration_median,
               fill = Ev_type,
               #colour = Ev_type,
               alpha = log10(n)),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  geom_errorbar(aes(x=npeak3, ymin=duration_25, ymax=duration_75, alpha=log10(n), group=Ev_type), width=0.2 ,colour="grey40", size=0.5, 
                stat = 'identity', position=position_dodge(width=0.9)) +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_continuous(name="diff_rising_peak_et", breaks=seq(0, 60, 20), limits = c(-24, 60)) +
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  #scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                           xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = -0, 
           label = "-0", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 20, 
           label = "20", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 40, 
           label = "40", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 60, 
           label = "60", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())

ggsave(filename=paste0(dir.plot, "npeak_duration.png"), width = 15, height = 15, units = "cm", plot=p_duration)
ggsave(filename=paste0(dir.plot, "npeak_duration.pdf"), width = 15, height = 15, units = "cm", plot=p_duration)

# n
library(scales)
segment_data = data.frame(x = c(1:7),
                          xend = c(1:7), 
                          y = 1,
                          yend = 1e06)

p_n <- ggplot(SDres_summary_multipeak) +
  geom_hline(aes(yintercept = y),
             data.frame(y = c(1, 1e01, 1e02, 1e03, 1e04, 1e05, 1e06)),
             color = "lightgrey") +
  geom_bar(aes(x = npeak3,
               y = n,
               fill = Ev_type,
               colour = Ev_type),
           stat = 'identity',
           linewidth = 0.75,
           position = "dodge2",
           show.legend = TRUE) +
  #scale_alpha_continuous(range = c(0.05, 1)) +
  scale_x_continuous(name="Npeaks", breaks=seq(1, 7, 1), limits=c(0.4, 7.6)) +
  scale_y_log10(name="NEvents", limits=c(1e-02, 1e06)) +
  scale_fill_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  scale_colour_manual(values=c("#B2DF8A", "#A6CEE3", "#1F78B4")) +
  geom_segment(segment_data, mapping = aes(x = x, y = y, 
                                           xend = xend, yend = yend),
               linetype = "dashed",
               color = "gray12") +
  coord_polar() +
  annotate(x = 0.4, 
           y = 1, 
           label = "1", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 1e02, 
           label = "100", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 1e04, 
           label = "10,000", 
           geom = "text", 
           color = "gray12") +
  annotate(x = 0.4, 
           y = 1e06, 
           label = "1,000,000", 
           geom = "text", 
           color = "gray12") +
  theme(#axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 12),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank())

ggsave(filename=paste0(dir.plot, "npeak_n.png"), width = 15, height = 10, units = "cm", plot=p_n)
ggsave(filename=paste0(dir.plot, "npeak_n.pdf"), width = 15, height = 10, units = "cm", plot=p_n)


#####
SDres_summary_event_hydrocomp <- SDres_summary %>% 
  group_by(event_type, event_count, ID, iparset, Par_type, hydcase)  %>%
  summarise(et=mean(et, na.rm=T), eq=mean(eq, na.rm=T), obs=mean(obs, na.rm=T), sim=mean(sim , na.rm=T))  

SDres_summary_event_hydrocomp[(SDres_summary_event_hydrocomp$event_type >= 1 & SDres_summary_event_hydrocomp$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
SDres_summary_event_hydrocomp[(SDres_summary_event_hydrocomp$event_type >= 5 & SDres_summary_event_hydrocomp$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
SDres_summary_event_hydrocomp[(SDres_summary_event_hydrocomp$event_type >= 11 & SDres_summary_event_hydrocomp$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"
SDres_summary_event_hydrocomp$Ev_type <- factor(SDres_summary_event_hydrocomp$Ev_type, 
                                levels=c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet"))
SDres_summary_event_hydrocomp$hydcase <- factor(SDres_summary_event_hydrocomp$hydcase, 
                                levels=c("preEv_1d", "Rising", "Peak", "Recession", "Trough"))


#col <- brewer.pal(5, "Paired")
col <- c("#888888", "#6BAED6", "#084594", "#74C476","#005A32")
p_et <- ggplot(data=SDres_summary_event_hydrocomp, aes(x=Ev_type, y=et, group=factor(hydcase))) + 
        geom_boxplot(aes(color=hydcase), na.rm = TRUE, outlier.shape = NA) + 
        #stat_summary(fun=mean, position=position_dodge(width=.75), geom='point', shape=20, size=3, colour='#000000') +
        scale_y_continuous(breaks=seq(-10, 10, 2)) + coord_cartesian(ylim=c(-10, 10)) +
        scale_color_manual(values=col, name="Hydrograph", labels=c("1-day Pre-event", "Rising", "Peak", "Recession", "Trough")) +
        #geom_jitter(aes(shape = Par_type)) +
        ylab("Et [days]") + facet_grid(.~Ev_type, scales = "free_x") + xlab(NULL) + 
        geom_hline(yintercept=0, linetype=2) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())

ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot2.png"), width = 15, height = 10, units = "cm", plot=p_et)
ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot2.pdf"), width = 15, height = 10, units = "cm", plot=p_et)

p_eq <- ggplot(data=SDres_summary_event_hydrocomp, aes(x=Ev_type, y=eq, group=factor(hydcase))) + 
        geom_boxplot(aes(color=hydcase), na.rm = TRUE, outlier.shape = NA) + 
        #stat_summary(fun=mean, position=position_dodge(width=.75), geom='point', shape=20, size=3, colour='#000000') +
        scale_y_continuous(breaks=seq(-1.6, 1.6, 0.4)) + coord_cartesian(ylim=c(-1.6, 1.6)) +
        scale_color_manual(values=col, name="Hydrograph", labels=c("1-day Pre-event", "Rising", "Peak", "Recession", "Trough")) +
        ylab("Eq") + facet_grid(.~Ev_type, scales = "free_x") + xlab(NULL) +
        geom_hline(yintercept=0, linetype=2) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())

ggsave(filename=paste0(dir.plot, "Hydcase_Eq_Boxplot2.png"), width = 15, height = 10, units = "cm", plot=p_eq)
ggsave(filename=paste0(dir.plot, "Hydcase_Eq_Boxplot2.pdf"), width = 15, height = 10, units = "cm", plot=p_eq)

p <- plot_grid(p_et + theme(legend.position="none"), 
               p_eq + theme(legend.position="none"), 
               get_legend(p_et),
               rel_widths = c(1, 1, 0.3),
               nrow=1)
ggsave(filename=paste0(dir.plot, "Hydcase_Boxplot2.png"), width = 25, height = 10, units = "cm", plot=p)
ggsave(filename=paste0(dir.plot, "Hydcase_Boxplot2.pdf"), width = 25, height = 10, units = "cm", plot=p)


col <- c("#888888", "#6BAED6", "#084594", "#74C476","#005A32")
p_et <- ggplot(data = SDres_summary_event_hydrocomp[which(SDres_summary_event_hydrocomp$Par_type == "Lumped"),], aes(x=Ev_type, y=et, group=factor(hydcase))) + 
  geom_boxplot(aes(color=hydcase), na.rm = TRUE, outlier.shape = NA) + 
  #stat_summary(fun=mean, position=position_dodge(width=.75), geom='point', shape=20, size=3, colour='#000000') +
  scale_y_continuous(limits=c(-8, 6), breaks=seq(-8, 6, 2)) + #coord_cartesian(ylim=c(-10, 10)) +
  scale_color_manual(values=col, name="Hydrograph", labels=c("1-day Pre-event", "Rising", "Peak", "Recession", "Trough")) +
  #scale_y_break(c(6, 7), scale=0.8) + scale_y_break(c(-7, -6), scale=6) +
  #geom_jitter(aes(shape = Par_type)) +
  ylab("Et [days]") + facet_grid(.~Ev_type, scales = "free_x") + xlab(NULL) + 
  geom_hline(yintercept=0, linetype=2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot_Lumped2.png"), width = 15, height = 10, units = "cm", plot=p_et)
ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot_Lumped2.pdf"), width = 15, height = 10, units = "cm", plot=p_et)

p_eq <- ggplot(data=SDres_summary_event_hydrocomp[which(SDres_summary_event_hydrocomp$Par_type == "Lumped"),], aes(x=Ev_type, y=eq, group=factor(hydcase))) + 
  geom_boxplot(aes(color=hydcase), na.rm = TRUE, outlier.shape = NA) + 
  #stat_summary(fun=mean, position=position_dodge(width=.75), geom='point', shape=20, size=3, colour='#000000') +
  scale_y_continuous(limits=c(-1.2, 1.35), breaks=seq(-1.2, 1.2, 0.3)) +# coord_cartesian(ylim=c(-1.6, 1.6)) +
  scale_color_manual(values=col, name="Hydrograph", labels=c("1-day Pre-event", "Rising", "Peak", "Recession", "Trough")) +
  #scale_y_break(c(1.2, 1.3), scale=0.8) + scale_y_break(c(-1.3, -1.2), scale=6) +
  ylab("Eq") + facet_grid(.~Ev_type, scales = "free_x") + xlab(NULL) +
  geom_hline(yintercept=0, linetype=2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(filename=paste0(dir.plot, "Hydcase_Eq_Boxplot_Lumped2.png"), width = 15, height = 10, units = "cm", plot=p_eq)
ggsave(filename=paste0(dir.plot, "Hydcase_Eq_Boxplot_Lumped2.pdf"), width = 15, height = 10, units = "cm", plot=p_eq)

p <- plot_grid(p_et + theme(legend.position="none"), 
               p_eq + theme(legend.position="none"), 
               get_legend(p_et),
               rel_widths = c(1, 1, 0.3),
               nrow=1)
ggsave(filename=paste0(dir.plot, "Hydcase_Boxplot_Lumped2.png"), width = 25, height = 10, units = "cm", plot=p)
ggsave(filename=paste0(dir.plot, "Hydcase_Boxplot_Lumped2.pdf"), width = 25, height = 10, units = "cm", plot=p)


#col <- brewer.pal(5, "Paired")
col <- c("#888888", "#6BAED6", "#084594", "#74C476","#005A32")
p_et <- ggplot(data=SDres_summary_event_hydrocomp[which(SDres_summary_event_hydrocomp$Par_type == "Reg"),], aes(x=Ev_type, y=et, group=factor(hydcase))) + 
  geom_boxplot(aes(color=hydcase), na.rm = TRUE, outlier.shape = NA) + 
  #stat_summary(fun=mean, position=position_dodge(width=.75), geom='point', shape=20, size=3, colour='#000000') +
  scale_y_continuous(limits=c(-9, 7), breaks=seq(-8, 6, 2)) + #coord_cartesian(ylim=c(-10, 10)) +
  scale_color_manual(values=col, name="Hydrograph", labels=c("1-day Pre-event", "Rising", "Peak", "Recession", "Trough")) +
  #scale_y_break(c(6, 7), scale=0.8) + scale_y_break(c(-7, -6), scale=6) +
  #geom_jitter(aes(shape = Par_type)) +
  ylab("Et [days]") + facet_grid(.~Ev_type, scales = "free_x") + xlab(NULL) + 
  geom_hline(yintercept=0, linetype=2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot_Dist2.png"), width = 15, height = 10, units = "cm", plot=p_et)
ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot_Dist2.pdf"), width = 15, height = 10, units = "cm", plot=p_et)

p_eq <- ggplot(data=SDres_summary_event_hydrocomp[which(SDres_summary_event_hydrocomp$Par_type == "Reg"),], aes(x=Ev_type, y=eq, group=factor(hydcase))) + 
  geom_boxplot(aes(color=hydcase), na.rm = TRUE, outlier.shape = NA) + 
  #stat_summary(fun=mean, position=position_dodge(width=.75), geom='point', shape=20, size=3, colour='#000000') +
  scale_y_continuous(limits=c(-1.3, 1.6), breaks=seq(-1.3, 1.6, 0.4)) +# coord_cartesian(ylim=c(-1.6, 1.6)) +
  scale_color_manual(values=col, name="Hydrograph", labels=c("1-day Pre-event", "Rising", "Peak", "Recession", "Trough")) +
  #scale_y_break(c(1.2, 1.3), scale=0.8) + scale_y_break(c(-1.3, -1.2), scale=6) +
  ylab("Eq") + facet_grid(.~Ev_type, scales = "free_x") + xlab(NULL) +
  geom_hline(yintercept=0, linetype=2) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(filename=paste0(dir.plot, "Hydcase_Eq_Boxplot_Dist2.png"), width = 15, height = 10, units = "cm", plot=p_eq)
ggsave(filename=paste0(dir.plot, "Hydcase_Eq_Boxplot_Dist2.pdf"), width = 15, height = 10, units = "cm", plot=p_eq)

p <- plot_grid(p_et + theme(legend.position="none"), 
               p_eq + theme(legend.position="none"), 
               get_legend(p_et),
               rel_widths = c(1, 1, 0.3),
               nrow=1)
ggsave(filename=paste0(dir.plot, "Hydcase_Boxplot_Dist2.png"), width = 25, height = 10, units = "cm", plot=p)
ggsave(filename=paste0(dir.plot, "Hydcase_Boxplot_Dist2.pdf"), width = 25, height = 10, units = "cm", plot=p)
##############################################################
# SDres_summary.jitter <- NULL
# for(Par_type in c("Reg", "Lumped")) {
#   for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
#     for(hydcase in c("preEv_1d", "Rising", "Peak", "Recession", "Trough")) {
#       SDres_summary.i <- SDres_summary[which(SDres_summary$Ev_type == Ev_type &
#                                                SDres_summary$hydcase == hydcase &
#                                                SDres_summary$Par_type == Par_type), ]
#       SDres_summary.i.sampled <- data.frame(et=quantile(SDres_summary.i$et, optimumLHS(10, 1)),
#                                             eq=quantile(SDres_summary.i$eq, optimumLHS(10, 1)))
#       SDres_summary.i.sampled$Ev_type <- Ev_type
#       SDres_summary.i.sampled$hydcase <- hydcase
#       SDres_summary.i.sampled$Par_type <- Par_type
#       SDres_summary.jitter <- rbind(SDres_summary.jitter, SDres_summary.i.sampled)
#     }
#   }
# }
# 
# SDres_summary.jitter$Ev_type <- factor(SDres_summary.jitter$Ev_type, 
#                                 levels=c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet"))
# SDres_summary.jitter$hydcase <- factor(SDres_summary.jitter$hydcase, 
#                                 levels=c("preEv_1d", "Rising", "Peak", "Recession", "Trough"))
# SDres_summary.jitter$Par_type <- factor(SDres_summary.jitter$Par_type, 
#                                        levels=c("Lumped", "Reg"))
# 
# p_et.jitter <- ggplot(data=SDres_summary.jitter, aes(x=Ev_type, y=et, group=factor(hydcase))) + 
#   geom_boxplot(aes(color=hydcase), na.rm = TRUE, outlier.shape = NA) + 
#   #stat_summary(fun=mean, position=position_dodge(width=.75), geom='point', shape=20, size=3, colour='#000000') +
#   scale_y_continuous(breaks=seq(-10, 10, 2)) + coord_cartesian(ylim=c(-10, 10)) +
#   geom_jitter(aes(shape=Par_type, color=hydcase, fill=Par_type), alpha=0.7) +
#   scale_color_manual(values=col, name="Hydrograph") +
#   scale_shape_manual(values=c(15, 17), name="Parameterization", labels=c("Lumped", "Regionalized")) +
#   ylab("Et [days]") + facet_grid(.~Ev_type + hydcase, scales = "free_x") + xlab(NULL) + 
#   geom_hline(yintercept=0, linetype=2) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot_jitter.png"), width = 20, height = 20, units = "cm", plot=p_et.jitter)
# ggsave(filename=paste0(dir.plot, "Hydcase_Et_Boxplot_jitter.pdf"), width = 20, height = 20, units = "cm", plot=p_et.jitter)
##############################################################
###### Correlation of errors in event types----
library(reshape2)
library(Hmisc)

for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  SDres_summary.ev_et <- SDres_summary[which(SDres_summary$Ev_type == Ev_type),] %>% 
    group_by(ID, iparset, event_type, event_count, hydcase, Par_type) %>% 
    summarise(et=mean(et))
  SDres_summary.ev_et.unmelt <- dcast(SDres_summary.ev_et, ID + iparset + event_type + event_count + Par_type ~ hydcase)
  mat.ev_et.unmelt <- as.matrix(SDres_summary.ev_et.unmelt[, c("preEv_1d", "Rising", "Peak", "Recession", "Trough")])
  et.cormat <- Hmisc::rcorr(mat.ev_et.unmelt, type="spearman")
  et.cormat.r <- round(et.cormat$r, 2)
  et.cormat.r[et.cormat$P > 0.05] <- NA
  et.cormat.r[lower.tri(et.cormat.r)] <- NA
  et.cormat.r.melted <- melt(et.cormat.r)
  
  p_cor_et <- ggplot(data = et.cormat.r.melted, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "#FFFFFF") +
    scale_fill_gradient2(low = "#313695", high = "#A50026", mid = "#FFFFFF", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    geom_text(aes(label = value), color = "#FFFFFF") +
    theme_minimal()+ 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_et_", Ev_type, ".png"), width = 10, height = 10, units = "cm", plot=p_cor_et)
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_et_", Ev_type, ".pdf"), width = 10, height = 10, units = "cm", plot=p_cor_et)

  
  SDres_summary.ev_eq <- SDres_summary[which(SDres_summary$Ev_type == Ev_type),] %>% 
    group_by(ID, iparset, event_type, event_count, hydcase, Par_type) %>% 
    summarise(eq=mean(eq))
  SDres_summary.ev_eq.unmelt <- dcast(SDres_summary.ev_eq, ID + iparset + event_type + event_count + Par_type ~ hydcase)
  mat.ev_eq.unmelt <- as.matrix(SDres_summary.ev_eq.unmelt[, c("preEv_1d", "Rising", "Peak", "Recession", "Trough")])
  eq.cormat <- Hmisc::rcorr(mat.ev_eq.unmelt, type="spearman")
  eq.cormat.r <- round(eq.cormat$r, 2)
  eq.cormat.r[eq.cormat$P > 0.05] <- NA
  eq.cormat.r[upper.tri(eq.cormat.r)] <- NA
  eq.cormat.r.melted <- melt(eq.cormat.r)
  
  p_cor_eq <- ggplot(data = eq.cormat.r.melted, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "#FFFFFF") +
    scale_fill_gradient2(low = "#313695", high = "#A50026", mid = "#FFFFFF", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    geom_text(aes(label = value), color = "#FFFFFF") +
    theme_minimal()+ 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_eq_", Ev_type, ".png"), width = 10, height = 10, units = "cm", plot=p_cor_eq)
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_eq_", Ev_type, ".pdf"), width = 10, height = 10, units = "cm", plot=p_cor_eq)
}


for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  SDres_summary.ev_et <- SDres_summary[which(SDres_summary$Ev_type == Ev_type & SDres_summary$Par_type == "Reg"),] %>% 
    group_by(ID, iparset, event_type, event_count, hydcase, Par_type) %>% 
    summarise(et=mean(et))
  SDres_summary.ev_et.unmelt <- dcast(SDres_summary.ev_et, ID + iparset + event_type + event_count + Par_type ~ hydcase)
  mat.ev_et.unmelt <- as.matrix(SDres_summary.ev_et.unmelt[, c("preEv_1d", "Rising", "Peak", "Recession", "Trough")])
  et.cormat <- Hmisc::rcorr(mat.ev_et.unmelt, type="spearman")
  et.cormat.r <- round(et.cormat$r, 2)
  et.cormat.r[et.cormat$P > 0.05] <- NA
  et.cormat.r[lower.tri(et.cormat.r)] <- NA
  et.cormat.r.melted <- melt(et.cormat.r)
  
  p_cor_et <- ggplot(data = et.cormat.r.melted, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "#FFFFFF") +
    scale_fill_gradient2(low = "#313695", high = "#A50026", mid = "#FFFFFF", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    geom_text(aes(label = value), color = "#FFFFFF") +
    theme_minimal()+ 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_et_Dist_", Ev_type, ".png"), width = 10, height = 10, units = "cm", plot=p_cor_et)
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_et_Dist_", Ev_type, ".pdf"), width = 10, height = 10, units = "cm", plot=p_cor_et)
  
  
  SDres_summary.ev_eq <- SDres_summary[which(SDres_summary$Ev_type == Ev_type & SDres_summary$Par_type == "Reg"),] %>% 
    group_by(ID, iparset, event_type, event_count, hydcase, Par_type) %>% 
    summarise(eq=mean(eq))
  SDres_summary.ev_eq.unmelt <- dcast(SDres_summary.ev_eq, ID + iparset + event_type + event_count + Par_type ~ hydcase)
  mat.ev_eq.unmelt <- as.matrix(SDres_summary.ev_eq.unmelt[, c("preEv_1d", "Rising", "Peak", "Recession", "Trough")])
  eq.cormat <- Hmisc::rcorr(mat.ev_eq.unmelt, type="spearman")
  eq.cormat.r <- round(eq.cormat$r, 2)
  eq.cormat.r[eq.cormat$P > 0.05] <- NA
  eq.cormat.r[upper.tri(eq.cormat.r)] <- NA
  eq.cormat.r.melted <- melt(eq.cormat.r)
  
  p_cor_eq <- ggplot(data = eq.cormat.r.melted, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "#FFFFFF") +
    scale_fill_gradient2(low = "#313695", high = "#A50026", mid = "#FFFFFF", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    geom_text(aes(label = value), color = "#FFFFFF") +
    theme_minimal()+ 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_eq_Dist_", Ev_type, ".png"), width = 10, height = 10, units = "cm", plot=p_cor_eq)
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_eq_Dist_", Ev_type, ".pdf"), width = 10, height = 10, units = "cm", plot=p_cor_eq)
}

SDres_summary.ev_eq <- SDres_summary[which(SDres_summary$Par_type == "Lumped"),] %>% 
  group_by(ID, iparset, event_type, event_count, hydcase, Par_type) %>% 
  summarise(eq=mean(eq))
SDres_summary.ev_eq.unmelt <- dcast(SDres_summary.ev_et, ID + iparset + event_type + event_count + Par_type ~ hydcase)
mat.ev_eq.unmelt <- as.matrix(SDres_summary.ev_eq.unmelt[, c("preEv_1d", "Rising", "Peak", "Recession", "Trough")])
eq.cormat <- Hmisc::rcorr(mat.ev_eq.unmelt, type="spearman")
eq.cormat.r <- round(eq.cormat$r, 2)

for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  SDres_summary.ev_et <- SDres_summary[which(SDres_summary$Ev_type == Ev_type & SDres_summary$Par_type == "Lumped"),] %>% 
    group_by(ID, iparset, event_type, event_count, hydcase, Par_type) %>% 
    summarise(et=mean(et))
  SDres_summary.ev_et.unmelt <- dcast(SDres_summary.ev_et, ID + iparset + event_type + event_count + Par_type ~ hydcase)
  mat.ev_et.unmelt <- as.matrix(SDres_summary.ev_et.unmelt[, c("preEv_1d", "Rising", "Peak", "Recession", "Trough")])
  et.cormat <- Hmisc::rcorr(mat.ev_et.unmelt, type="spearman")
  et.cormat.r <- round(et.cormat$r, 2)
  et.cormat.r[et.cormat$P > 0.05] <- NA
  et.cormat.r[lower.tri(et.cormat.r)] <- NA
  et.cormat.r.melted <- melt(et.cormat.r)
  
  p_cor_et <- ggplot(data = et.cormat.r.melted, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "#FFFFFF") +
    scale_fill_gradient2(low = "#313695", high = "#A50026", mid = "#FFFFFF", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    geom_text(aes(label = value), color = "#FFFFFF") +
    theme_minimal()+ 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_et_Lumped_", Ev_type, ".png"), width = 10, height = 10, units = "cm", plot=p_cor_et)
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_et_Lumped_", Ev_type, ".pdf"), width = 10, height = 10, units = "cm", plot=p_cor_et)
  
  
  SDres_summary.ev_eq <- SDres_summary[which(SDres_summary$Ev_type == Ev_type & SDres_summary$Par_type == "Lumped"),] %>% 
    group_by(ID, iparset, event_type, event_count, hydcase, Par_type) %>% 
    summarise(eq=mean(eq))
  SDres_summary.ev_eq.unmelt <- dcast(SDres_summary.ev_eq, ID + iparset + event_type + event_count + Par_type ~ hydcase)
  mat.ev_eq.unmelt <- as.matrix(SDres_summary.ev_eq.unmelt[, c("preEv_1d", "Rising", "Peak", "Recession", "Trough")])
  eq.cormat <- Hmisc::rcorr(mat.ev_eq.unmelt, type="spearman")
  eq.cormat.r <- round(eq.cormat$r, 2)
  eq.cormat.r[eq.cormat$P > 0.05] <- NA
  eq.cormat.r[upper.tri(eq.cormat.r)] <- NA
  eq.cormat.r.melted <- melt(eq.cormat.r)
  
  p_cor_eq <- ggplot(data = eq.cormat.r.melted, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile(color = "#FFFFFF") +
    scale_fill_gradient2(low = "#313695", high = "#A50026", mid = "#FFFFFF", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    geom_text(aes(label = value), color = "#FFFFFF") +
    theme_minimal()+ 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed()
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_eq_Lumped_", Ev_type, ".png"), width = 10, height = 10, units = "cm", plot=p_cor_eq)
  ggsave(filename=paste0(dir.plot, "Hydcase_Cor_eq_Lumped_", Ev_type, ".pdf"), width = 10, height = 10, units = "cm", plot=p_cor_eq)
}


###################################################
###### Conceptual hydrograph----
library(reshape2)
library(Hmisc)
# max peak for R.Dry is on 2002-8-13 ID:550940
SDres_summary_obs_err.Ave <- SDres_summary[-which(SDres_summary$hydcase == "preEv_1d"),] %>% 
  group_by(ID, event_type, event_count, iparset, Par_type) %>% 
  mutate(obs_mean=mean(obs))

range_obs <- data.frame(low=c(0, 5, 10, 20, 40), 
                        range=c("<5.0", "5.0~10.0", "10.0~20.0", "20.0~40.0", ">40.0"))
SDres_summary_obs_err.Ave$range_obs <- range_obs$range[findInterval(SDres_summary_obs_err.Ave$obs_mean, range_obs$low)]
SDres_summary_obs_err.Ave$range_obs <- factor(SDres_summary_obs_err.Ave$range_obs, levels=range_obs$range)

SDres_summary_tmp <- SDres_summary_obs_err.Ave %>%
                      group_by(ID, iparset, event_type, event_count, Par_type, range_obs) %>%
                      mutate(time=row_number(), duration=n() - 1)

SDres_summary_tmp <- SDres_summary_tmp[which(SDres_summary_tmp$Par_type == 'Lumped'),]

# SDres_summary_tmp <- SDres_summary %>% 
#                       group_by(ID, iparset, event_type, event_count, Par_type) %>% 
#                       mutate(time=row_number(), duration=n() - 1)

SDres_summary.pEv <- SDres_summary_tmp[which(SDres_summary_tmp$hydcase == "preEv_1d"),] %>% 
  group_by(ID, iparset, event_type, event_count, Par_type) %>% 
  dplyr::select(obs=obs, et=et, eq=eq, Ev_type=Ev_type, time=0, duration=duration)

SDres_summary.peak <- SDres_summary_tmp[which(SDres_summary_tmp$hydcase == "Peak"),] %>% 
  group_by(ID, iparset, event_type, event_count, Par_type) %>% 
  dplyr::select(obs=obs, et=et, eq=eq, Ev_type=Ev_type, time=time, duration=duration, range_obs=range_obs)

SDres_summary.rising <- SDres_summary_tmp[which(SDres_summary_tmp$hydcase == "Rising"),] %>% 
  group_by(ID, iparset, event_type, event_count, Par_type) %>% 
  filter(row_number()==1) %>%
  dplyr::select(obs=obs, et=et, eq=eq, Ev_type=Ev_type, time=time, duration=duration, range_obs=range_obs)

SDres_summary.recession <- SDres_summary_tmp[which(SDres_summary_tmp$hydcase == "Recession"),] %>% 
  group_by(ID, iparset, event_type, event_count, Par_type) %>% 
  filter(row_number()==n()) %>%
  dplyr::select(obs=obs, et=et, eq=eq, Ev_type=Ev_type, time=time, duration=duration, range_obs=range_obs)

df.all <- NULL
for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  for(range_obs.i in range_obs$range) {
    #SDres_summary.pEv_tmp <- SDres_summary.pEv[which(SDres_summary.pEv$Ev_type == Ev_type & ),]
    SDres_summary.peak_tmp <- SDres_summary.peak[which(SDres_summary.peak$Ev_type == Ev_type & SDres_summary.peak$range_obs == range_obs.i),]
    SDres_summary.rising_tmp <- SDres_summary.rising[which(SDres_summary.rising$Ev_type == Ev_type & SDres_summary.rising$range_obs == range_obs.i),]
    SDres_summary.recession_tmp <- SDres_summary.recession[which(SDres_summary.recession$Ev_type == Ev_type & SDres_summary.recession$range_obs == range_obs.i),]
    
    df <- data.frame(t=c(#0, 0 - median(SDres_summary.pEv_tmp$et, na.rm=T),
                         #0 - quantile(SDres_summary.pEv_tmp$et, 0.75, na.rm=T), 
                         #0 - quantile(SDres_summary.pEv_tmp$et, 0.25, na.rm=T),
                         1, 1 - median(SDres_summary.rising_tmp$et, na.rm=T),
                         1- quantile(SDres_summary.rising_tmp$et, 0.75, na.rm=T), 
                         1 - quantile(SDres_summary.rising_tmp$et, 0.25, na.rm=T),
                         median(SDres_summary.peak_tmp$time, na.rm=T), median(SDres_summary.peak_tmp$time, na.rm=T) - median(SDres_summary.peak_tmp$et, na.rm=T),
                         median(SDres_summary.peak_tmp$time, na.rm=T) - quantile(SDres_summary.peak_tmp$et, 0.75, na.rm=T), 
                         median(SDres_summary.peak_tmp$time, na.rm=T) - quantile(SDres_summary.peak_tmp$et, 0.25, na.rm=T),
                         median(SDres_summary.recession_tmp$time, na.rm=T), median(SDres_summary.recession_tmp$time, na.rm=T) - median(SDres_summary.recession_tmp$et, na.rm=T),
                         median(SDres_summary.recession_tmp$time, na.rm=T) - quantile(SDres_summary.recession_tmp$et, 0.75, na.rm=T), 
                         median(SDres_summary.recession_tmp$time, na.rm=T) - quantile(SDres_summary.recession_tmp$et, 0.25, na.rm=T)),
                     Q=c(#median(SDres_summary.pEv_tmp$obs, na.rm=T), median(SDres_summary.pEv_tmp$obs, na.rm=T) - median(SDres_summary.pEv_tmp$eq, na.rm=T),
                         #median(SDres_summary.pEv_tmp$obs, na.rm=T) - quantile(SDres_summary.pEv_tmp$eq, 0.75, na.rm=T), 
                         #median(SDres_summary.pEv_tmp$obs, na.rm=T) - quantile(SDres_summary.pEv_tmp$eq, 0.25, na.rm=T),
                         median(SDres_summary.rising_tmp$obs, na.rm=T), median(SDres_summary.rising_tmp$obs, na.rm=T) - median(SDres_summary.rising_tmp$eq, na.rm=T),
                         median(SDres_summary.rising_tmp$obs, na.rm=T) - quantile(SDres_summary.rising_tmp$eq, 0.75, na.rm=T), 
                         median(SDres_summary.rising_tmp$obs, na.rm=T) - quantile(SDres_summary.rising_tmp$eq, 0.25, na.rm=T),
                         median(SDres_summary.peak_tmp$obs, na.rm=T), median(SDres_summary.peak_tmp$obs, na.rm=T) - median(SDres_summary.peak_tmp$eq, na.rm=T),
                         median(SDres_summary.peak_tmp$obs, na.rm=T) - quantile(SDres_summary.peak_tmp$eq, 0.75, na.rm=T), 
                         median(SDres_summary.peak_tmp$obs, na.rm=T) - quantile(SDres_summary.peak_tmp$eq, 0.25, na.rm=T),
                         median(SDres_summary.recession_tmp$obs), median(SDres_summary.recession_tmp$obs, na.rm=T) - median(SDres_summary.recession_tmp$eq, na.rm=T),
                         median(SDres_summary.recession_tmp$obs, na.rm=T) - quantile(SDres_summary.recession_tmp$eq, 0.75, na.rm=T), 
                         median(SDres_summary.recession_tmp$obs, na.rm=T) - quantile(SDres_summary.recession_tmp$eq, 0.25, na.rm=T)),
                     components=c(#"pEv_obs", "pEv_sim", "pEv95", "pEv5", 
                                  "Rising_obs", "Rising_sim", "Rising95", "Rising5", 
                                  "Peak_obs", "Peak_sim", "Peak95", "Peak5", 
                                  "Recession_obs", "Recession_sim", "Recession95", "Recession5"),
                     groups=c(#"obs", "sim", "sim95", "sim5",
                              "obs", "sim", "sim95", "sim5",
                              "obs", "sim", "sim95", "sim5",
                              "obs", "sim", "sim95", "sim5"))
    df$Ev_type <- Ev_type
    df$range_obs <- range_obs.i
    df.all <- rbind(df.all, df)
  }
}

#col <- c("#67D294", "#14B79C", "#009A9C", "#007C92", "#005E7D", "#1F4260")
col <- c("#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026")
col <- setNames(col, range_obs$range)

df.all <- df.all[which(df.all$groups == 'obs' | df.all$groups == 'sim'),]
for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  p <- ggplot(data = df.all[which(df.all$Ev_type == Ev_type),], aes(x=t, y=Q)) +
    geom_line(aes(colour=range_obs, linetype = groups)) +
    geom_point(aes(colour=range_obs, shape=groups)) +
    scale_color_manual(values=col, name="Event-averaged Streamflow [mm/s]", drop = FALSE, labels=range_obs$range) +
    scale_x_continuous(breaks=seq(0, 15, 5), limits = c(0, 15))
    #scale_y_continuous(breaks=seq(0, 80, 10), limits = c(0, 80)) 
  ggsave(filename=paste0(dir.plot, paste0("Conceptual_hydrograph_magnitude_", Ev_type,".png")), width = 30, height = 10, units = "cm", plot=p)
  ggsave(filename=paste0(dir.plot, paste0("Conceptual_hydrograph_magnitude_", Ev_type,".pdf")), width = 30, height = 10, units = "cm", plot=p)
}


for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  p <- ggplot(data = df.all[which(df.all$Ev_type == Ev_type),], aes(x=t, y=Q)) +
    geom_line(aes(colour=groups)) +
    geom_point(aes(colour=groups)) +
    scale_x_continuous(breaks=seq(-6, 15, 5), limits = c(-6, 15)) +
    scale_y_continuous(breaks=seq(0, 4, 0.5), limits = c(0, 4)) 
  ggsave(filename=paste0(dir.plot, paste0("Conceptual_hydrograph_", Ev_type,".png")), width = 10, height = 10, units = "cm", plot=p)
  ggsave(filename=paste0(dir.plot, paste0("Conceptual_hydrograph_", Ev_type,".pdf")), width = 10, height = 10, units = "cm", plot=p)
}

###################################################
###### Obs vs Err----
SDres_summary_obs_err.Ave <- SDres_summary[-which(SDres_summary$hydcase == "preEv_1d"),] %>% 
  group_by(ID, event_type, event_count, iparset, Par_type) %>% 
  summarise(obs=mean(obs), et=mean(et), eq=mean(eq))

SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 1 & SDres_summary_obs_err.Ave$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 5 & SDres_summary_obs_err.Ave$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 11 & SDres_summary_obs_err.Ave$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"

range_obs <- data.frame(low=c(0, 5, 10, 20, 40), 
                       range=c("<5.0", "5.0~10.0", "10.0~20.0", "20.0~40.0", ">40.0"))
SDres_summary_obs_err.Ave$range_obs <- range_obs$range[findInterval(SDres_summary_obs_err.Ave$obs, range_obs$low)]
SDres_summary_obs_err.Ave$range_obs <- factor(SDres_summary_obs_err.Ave$range_obs, levels=range_obs$range)

SDres_summary_obs_err.Ave <- SDres_summary_obs_err.Ave %>% 
              group_by(Ev_type, range_obs) %>% 
              summarise(et_median=median(et, na.rm=T), et_5=quantile(et, 0.05, na.rm=T), et_95=quantile(et, 0.95, na.rm=T),
                        eq_median=median(eq, na.rm=T), eq_5=quantile(eq, 0.05, na.rm=T), eq_95=quantile(eq, 0.95, na.rm=T),
                        n=n() / 10)

range_n <- data.frame(low=c(0, 10, 100, 1000, 10000), 
                      range=c("<10", "10~100", "100~1,000", "1,000~10,000", ">10,000"))
SDres_summary_obs_err.Ave$range_n <- range_n$range[findInterval(SDres_summary_obs_err.Ave$n, range_n$low)]
SDres_summary_obs_err.Ave$range_n <- factor(SDres_summary_obs_err.Ave$range_n, levels=range_n$range)

#col <- c("#67D294", "#14B79C", "#009A9C", "#007C92", "#005E7D", "#1F4260")
col <- c("#FED976", "#FEB24C", "#FD8D3C", "#F03B20", "#BD0026")
col <- setNames(col, range_obs$range)

size <- seq(0.2, 1, 0.2)
size <- setNames(size, range_n$range)

for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  SDres_summary_obs_err.Ave.tmp <- SDres_summary_obs_err.Ave[which(SDres_summary_obs_err.Ave$Ev_type == Ev_type),]
  
  p <- ggplot(data = SDres_summary_obs_err.Ave.tmp, 
              aes(x=et_median, y=eq_median, color=range_obs)) +
    #geom_point(aes(size=range_n), alpha=0.6) +
    geom_pointrange(aes(ymin=eq_5, ymax=eq_95, size=range_n), alpha=0.6, linewidth=0.5) + 
    geom_pointrange(aes(xmin=et_5, xmax=et_95, size=range_n), alpha=0.6, linewidth=0.5) + 
    scale_color_manual(values=col, name="Event-averaged Streamflow [mm/s]", drop = FALSE, labels=range_obs$range) +
    scale_size_manual(values=size, name="Number of Events", drop = FALSE, labels=names(size)) + 
    geom_segment(x=0, xend=0, y=-10, yend=10, color="black") +
    geom_segment(x=-10, xend=10, y=0, yend=0, color="black") +
    scale_x_continuous(expand=c(0, 0), limits = c(-5, 5)) +
    scale_y_continuous(expand=c(0, 0), limits = c(-0.6, 0.9), breaks=seq(-0.6, 0.9, 0.3)) +
    theme_bw()
  ggsave(filename=paste0(dir.plot, paste0("Point_obs_err_", Ev_type,".png")), width = 15, height = 10, units = "cm", plot=p)
  ggsave(filename=paste0(dir.plot, paste0("Point_obs_err_", Ev_type,".pdf")), width = 15, height = 10, units = "cm", plot=p)
}
  


SDres_summary_obs_err.Ave <- SDres_summary[-which(SDres_summary$hydcase == "preEv_1d" | SDres_summary$Par_type == "Reg"),] %>% 
  group_by(ID, event_type, event_count, iparset, Par_type) %>% 
  summarise(obs=mean(obs), et=mean(et), eq=mean(eq))

SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 1 & SDres_summary_obs_err.Ave$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 5 & SDres_summary_obs_err.Ave$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 11 & SDres_summary_obs_err.Ave$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"

range_obs <- data.frame(low=c(0, 5, 10, 20, 40), 
                        range=c("<5.0", "5.0~10.0", "10.0~20.0", "20.0~40.0", ">40.0"))
SDres_summary_obs_err.Ave$range_obs <- range_obs$range[findInterval(SDres_summary_obs_err.Ave$obs, range_obs$low)]
SDres_summary_obs_err.Ave$range_obs <- factor(SDres_summary_obs_err.Ave$range_obs, levels=range_obs$range)

SDres_summary_obs_err.Ave <- SDres_summary_obs_err.Ave %>% 
  group_by(Ev_type, range_obs) %>% 
  summarise(et_median=median(et, na.rm=T), et_5=quantile(et, 0.05, na.rm=T), et_95=quantile(et, 0.95, na.rm=T),
            eq_median=median(eq, na.rm=T), eq_5=quantile(eq, 0.05, na.rm=T), eq_95=quantile(eq, 0.95, na.rm=T),
            n=n() / 5)

range_n <- data.frame(low=c(0, 10, 100, 1000, 10000), 
                      range=c("<10", "10~100", "100~1,000", "1,000~10,000", ">10,000"))
SDres_summary_obs_err.Ave$range_n <- range_n$range[findInterval(SDres_summary_obs_err.Ave$n, range_n$low)]
SDres_summary_obs_err.Ave$range_n <- factor(SDres_summary_obs_err.Ave$range_n, levels=range_n$range)

#col <- c("#67D294", "#14B79C", "#009A9C", "#007C92", "#005E7D", "#1F4260")
col <- c("#FDE725", "#5EC962", "#21918C", "#3B528B", "#440154")
col <- setNames(col, range_obs$range)

size <- seq(0.2, 1.8, 0.4)
size <- setNames(size, range_n$range)


for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  SDres_summary_obs_err.Ave.tmp <- SDres_summary_obs_err.Ave[which(SDres_summary_obs_err.Ave$Ev_type == Ev_type),]
  
  p <- ggplot(data = SDres_summary_obs_err.Ave.tmp, 
              aes(x=et_median, y=eq_median, color=range_obs)) +
    #geom_point(aes(size=range_n), alpha=0.6) +
    geom_pointrange(aes(ymin=eq_5, ymax=eq_95, size=range_n), alpha=0.6, linewidth=0.5) + 
    geom_pointrange(aes(xmin=et_5, xmax=et_95, size=range_n), alpha=0.6, linewidth=0.5) + 
    scale_color_manual(values=col, name="Event-averaged Streamflow [mm/s]", drop = FALSE, labels=range_obs$range) +
    scale_size_manual(values=size, name="Number of Events", drop = FALSE, labels=names(size)) + 
    geom_segment(x=0, xend=0, y=-10, yend=10, color="black") +
    geom_segment(x=-10, xend=10, y=0, yend=0, color="black") +
    scale_x_continuous(expand=c(0, 0), limits = c(-9, 6), breaks=seq(-8, 6, 2)) +
    scale_y_continuous(expand=c(0, 0), limits = c(-0.6, 1.05), breaks=seq(-0.6, 0.9, 0.3)) +
    scale_x_break(c(-5, -4), scale=8) + 
    theme_bw()
  
  ggsave(filename=paste0(dir.plot, paste0("Point_obs_err_Lumped_", Ev_type,".png")), width = 15, height = 10, units = "cm", plot=p)
  ggsave(filename=paste0(dir.plot, paste0("Point_obs_err_Lumped_", Ev_type,".pdf")), width = 15, height = 10, units = "cm", plot=p)
}


SDres_summary_obs_err.Ave <- SDres_summary[-which(SDres_summary$hydcase == "preEv_1d" | SDres_summary$Par_type == "Lumped"),] %>% 
  group_by(ID, event_type, event_count, iparset, Par_type) %>% 
  summarise(obs=mean(obs), et=mean(et), eq=mean(eq))

SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 1 & SDres_summary_obs_err.Ave$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 5 & SDres_summary_obs_err.Ave$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
SDres_summary_obs_err.Ave[(SDres_summary_obs_err.Ave$event_type >= 11 & SDres_summary_obs_err.Ave$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"

range_obs <- data.frame(low=c(0, 5, 10, 20, 40), 
                        range=c("<5.0", "5.0~10.0", "10.0~20.0", "20.0~40.0", ">40.0"))
SDres_summary_obs_err.Ave$range_obs <- range_obs$range[findInterval(SDres_summary_obs_err.Ave$obs, range_obs$low)]
SDres_summary_obs_err.Ave$range_obs <- factor(SDres_summary_obs_err.Ave$range_obs, levels=range_obs$range)

SDres_summary_obs_err.Ave <- SDres_summary_obs_err.Ave %>% 
  group_by(Ev_type, range_obs) %>% 
  summarise(et_median=median(et, na.rm=T), et_5=quantile(et, 0.05, na.rm=T), et_95=quantile(et, 0.95, na.rm=T),
            eq_median=median(eq, na.rm=T), eq_5=quantile(eq, 0.05, na.rm=T), eq_95=quantile(eq, 0.95, na.rm=T),
            n=n() / 5)

range_n <- data.frame(low=c(0, 10, 100, 1000, 10000), 
                      range=c("<10", "10~100", "100~1,000", "1,000~10,000", ">10,000"))
SDres_summary_obs_err.Ave$range_n <- range_n$range[findInterval(SDres_summary_obs_err.Ave$n, range_n$low)]
SDres_summary_obs_err.Ave$range_n <- factor(SDres_summary_obs_err.Ave$range_n, levels=range_n$range)


#col <- c("#67D294", "#14B79C", "#009A9C", "#007C92", "#005E7D", "#1F4260")
col <- c("#FDE725", "#5EC962", "#21918C", "#3B528B", "#440154")
col <- setNames(col, range_obs$range)

size <- seq(0.2, 1.8, 0.4)
#size <- seq(0.2, 1, 0.2)
size <- setNames(size, range_n$range)

for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
  SDres_summary_obs_err.Ave.tmp <- SDres_summary_obs_err.Ave[which(SDres_summary_obs_err.Ave$Ev_type == Ev_type),]
  
  p <- ggplot(data = SDres_summary_obs_err.Ave.tmp, 
              aes(x=et_median, y=eq_median, color=range_obs)) +
    #geom_point(aes(size=range_n), alpha=0.6) +
    geom_pointrange(aes(ymin=eq_5, ymax=eq_95, size=range_n), alpha=0.6, linewidth=0.5) + 
    geom_pointrange(aes(xmin=et_5, xmax=et_95, size=range_n), alpha=0.6, linewidth=0.5) + 
    scale_color_manual(values=col, name="Event-averaged Streamflow [mm/s]", drop = FALSE, labels=range_obs$range) +
    scale_size_manual(values=size, name="Number of Events", drop = FALSE, labels=names(size)) + 
    geom_segment(x=0, xend=0, y=-10, yend=10, color="black") +
    geom_segment(x=-10, xend=10, y=0, yend=0, color="black") +
    scale_x_continuous(expand=c(0, 0), limits = c(-10, 8), breaks=seq(-10, 8, 2)) +
    scale_y_continuous(expand=c(0, 0), limits = c(-0.6, 1.35), breaks=seq(-0.6, 1.35, 0.3)) +
    theme_bw()
  
  ggsave(filename=paste0(dir.plot, paste0("Point_obs_err_Dist_", Ev_type,".png")), width = 15, height = 10, units = "cm", plot=p)
  ggsave(filename=paste0(dir.plot, paste0("Point_obs_err_Dist_", Ev_type,".pdf")), width = 15, height = 10, units = "cm", plot=p)
}
# SDres_summary_obs_err.Ave$max_et <- SDres_summary_obs_err.Ave$mean_et + SDres_summary_obs_err.Ave$sd_et
# SDres_summary_obs_err.Ave$min_et <- SDres_summary_obs_err.Ave$mean_et - SDres_summary_obs_err.Ave$sd_et
# SDres_summary_obs_err.Ave$max_eq <- SDres_summary_obs_err.Ave$mean_eq + SDres_summary_obs_err.Ave$sd_eq
# SDres_summary_obs_err.Ave$min_eq <- SDres_summary_obs_err.Ave$mean_eq - SDres_summary_obs_err.Ave$sd_eq

###################################################
###### Obs vs Ev_distance----
# load("Y:/Home/wangzhen/Eval_SALTO/Out/Info_hydcase_Ev_AND_preEv_ALL.RData")
# 
# SDres_summary$date_diff <- as.numeric(SDres_summary$date_diff)
# SDres_summary_dist_err.Ave <- SDres_summary[-which(SDres_summary$hydcase == "preEv_1d" | 
#                                                      SDres_summary$hydcase == "preEv_5d" |
#                                                      SDres_summary$hydcase == "preEv_10d"),] %>% 
#   group_by(ID, iparset, event_type, event_count) %>% 
#   summarise(obs=mean(obs), et=mean(et), eq=mean(eq), Ev_gap=max(date_diff, na.rm=T))
# 
# 
# 
# SDres_summary_dist_err.Ave[(SDres_summary_dist_err.Ave$event_type >= 1 & SDres_summary_dist_err.Ave$event_type <= 4), "Ev_type"] <- "Snow-or-Ice"
# SDres_summary_dist_err.Ave[(SDres_summary_dist_err.Ave$event_type >= 5 & SDres_summary_dist_err.Ave$event_type <= 10), "Ev_type"] <- "Rain-on-Wet"
# SDres_summary_dist_err.Ave[(SDres_summary_dist_err.Ave$event_type >= 11 & SDres_summary_dist_err.Ave$event_type <= 16), "Ev_type"] <- "Rain-on-Dry"
# 
# 
# range_obs <- data.frame(low=c(0, 1, 5, 10, 20, 40), 
#                         range=c("<1.0", "1.0~5.0", "5.0~10.0", "10.0~20.0", "20.0~40.0", ">40.0"))
# SDres_summary_dist_err.Ave$range_obs <- range_obs$range[findInterval(SDres_summary_dist_err.Ave$obs, range_obs$low)]
# SDres_summary_dist_err.Ave$range_obs <- factor(SDres_summary_dist_err.Ave$range_obs, levels=range_obs$range)
# 
# range_gap <- data.frame(low=c(0, 1, 5, 10, 30, 60), 
#                         range=c("0", "1~5", "5~10", "10~30", "30~60", ">60"))
# SDres_summary_dist_err.Ave$range_gap <- range_gap$range[findInterval(SDres_summary_dist_err.Ave$Ev_gap, range_gap$low)]
# SDres_summary_dist_err.Ave$range_gap <- factor(SDres_summary_dist_err.Ave$range_gap, levels=range_gap$range)
# 
# SDres_summary_dist_err.Ave_Ev <- SDres_summary_dist_err.Ave %>%
#   group_by(Ev_type, range_obs, range_gap) %>%
#   summarise(mean_et=mean(et, na.rm=T), sd_et=sd(et, na.rm=T),
#             mean_eq=mean(eq, na.rm=T), sd_eq=sd(eq, na.rm=T))
# 
# 
# col <- brewer.pal(6, "Blues")
# col <- setNames(col, range_obs$range)
# 
# size <- seq(0.2, 1.2, 0.2)
# size <- setNames(size, range_gap$range)
# 
# for(Ev_type in c("Snow-or-Ice", "Rain-on-Dry", "Rain-on-Wet")) {
#   SDres_summary_obs_err.Ave.tmp <- SDres_summary_dist_err.Ave_Ev[which(SDres_summary_dist_err.Ave_Ev$Ev_type == Ev_type),]
#   
#   p <- ggplot(data = SDres_summary_obs_err.Ave.tmp, 
#               aes(x=mean_et, y=mean_eq, color=range_obs)) +
#     #geom_point(aes(size=range_n), alpha=0.6) +
#     geom_pointrange(aes(ymin=mean_eq-sd_eq, ymax=mean_eq+sd_eq, size=range_gap), alpha=0.6, linewidth=0.5) + 
#     geom_pointrange(aes(xmin=mean_et-sd_et, xmax=mean_et+sd_et, size=range_gap), alpha=0.6, linewidth=0.5) + 
#     scale_color_manual(values=col, name="Observed Runoff [mm/s]", drop = FALSE, labels=range_obs$range) +
#     scale_size_manual(values=size, name="Antecedent Non-Event Days", drop = FALSE, labels=names(size)) + 
#     geom_segment(x=0, xend=0, y=-7, yend=7, color="black") +
#     geom_segment(x=-10, xend=7, y=0, yend=0, color="black") +
#     #scale_x_continuous(expand=c(0, 0), limits = c(-5, 3)) +
#     #scale_y_continuous(expand=c(0, 0), limits = c(-0.4, 0.6), breaks=seq(-0.4, 0.6, 0.2)) +
#     theme_bw()
#   ggsave(filename=paste0(dir_plot, paste0("Point_obs_err_", Ev_type,".png")), width = 15, height = 15, units = "cm", plot=p)
#   ggsave(filename=paste0(dir_plot, paste0("Point_obs_err_", Ev_type,".pdf")), width = 15, height = 15, units = "cm", plot=p)
# }

