
###### Working Directory
setwd("Y:/Home/wangzhen/Eval_SALTO/")

###### Libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)

###### Sources

############################################################
###### Functions

############################################################
## Input directories
dir.lumped_sd_ts   <- c("F:/Zhenyu/Eval_SALTO/Out/lumped_SD_ts_1979_2002_KGE_Balanced/")
dir.dist_sd_ts   <- c("F:/Zhenyu/Eval_SALTO/Out/dist_SD_ts_1979_2002_KGE_Balanced_Ralf_TOP5/")

## Output directories
dir_plot <- c("F:/Zhenyu/Eval_SALTO/Plot/")

## Read the data
cat.table.lumped <- read.table(file=paste(dir.lumped_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)
cat.table.dist <- read.table(file=paste(dir.dist_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)
cat.table.lumped <- cat.table.lumped[, grep("_", colnames(cat.table.lumped))]
cat.table.dist <- cat.table.dist[, grep("_", colnames(cat.table.dist))]
cat.table.ave <- (cat.table.lumped + cat.table.dist) / 2
cat.table.lumped$Par_TYPE <- "Lumped"
cat.table.dist$Par_TYPE <- "Reg"
cat.table.ave$Par_TYPE <- "Ave"

cat.table.tmp <- rbind(cat.table.lumped, cat.table.dist)
cat.table.tmp <- rbind(cat.table.tmp, cat.table.ave)
cat.table.ALL <- data.frame(ET=unlist(cat.table.tmp[, paste0("ALL_ET_", 1:5)]),
                           EQ=unlist(cat.table.tmp[, paste0("ALL_EQ_", 1:5)]),
                           ME=unlist(cat.table.tmp[, paste0("ALL_ME_", 1:5)]),
                           Par_TYPE=cat.table.tmp$Par_TYPE)
cat.table.ALL$Ev_TYPE <- "ALL"

cat.table.SI <- data.frame(ET=unlist(cat.table.tmp[, paste0("SI_ET_", 1:5)]),
                           EQ=unlist(cat.table.tmp[, paste0("SI_EQ_", 1:5)]),
                           ME=unlist(cat.table.tmp[, paste0("SI_ME_", 1:5)]),
                           Par_TYPE=cat.table.tmp$Par_TYPE)
cat.table.SI$Ev_TYPE <- "SI"

cat.table.RD <- data.frame(ET=unlist(cat.table.tmp[, paste0("RD_ET_", 1:5)]),
                           EQ=unlist(cat.table.tmp[, paste0("RD_EQ_", 1:5)]),
                           ME=unlist(cat.table.tmp[, paste0("RD_ME_", 1:5)]),
                           Par_TYPE=cat.table.tmp$Par_TYPE)
cat.table.RD$Ev_TYPE <- "RD"

cat.table.RW <- data.frame(ET=unlist(cat.table.tmp[, paste0("RW_ET_", 1:5)]),
                           EQ=unlist(cat.table.tmp[, paste0("RW_EQ_", 1:5)]),
                           ME=unlist(cat.table.tmp[, paste0("RW_ME_", 1:5)]),
                           Par_TYPE=cat.table.tmp$Par_TYPE)
cat.table.RW$Ev_TYPE <- "RW"

#cat.table.all <- rbind(cat.table.ALL, cat.table.SI)
cat.table.all <- rbind(cat.table.SI, cat.table.RD)
cat.table.all <- rbind(cat.table.all, cat.table.RW)

cat.table.all <- cat.table.all[complete.cases(cat.table.all), ]
cat.table <- cat.table.all %>%
  group_by(Par_TYPE, Ev_TYPE) %>%
  summarise(ET_75=quantile(ET, 0.95, na.rm=T), EQ_75=quantile(EQ, 0.95, na.rm=T), ME_75=quantile(ME, 0.95, na.rm=T),
            ET_25=quantile(ET, 0.05, na.rm=T), EQ_25=quantile(EQ, 0.05, na.rm=T), ME_25=quantile(ME, 0.05, na.rm=T),
            ET_50=median(ET, na.rm=T), EQ_50=median(EQ, na.rm=T), ME_50=median(ME, na.rm=T),
            ET_MAD=mad(ET, na.rm=T), EQ_MAD=mad(EQ, na.rm=T), ME_MAD=mad(ME, na.rm=T))

# mean(cat.table.all$ME, na.rm=T)
# sd(cat.table.all$ME, na.rm=T)

range_ME <- data.frame(low=c(-9999999, 0.5, 0.6, 0.7),
                       range=c("<0.50", "0.50~0.60", "0.60~0.70", ">0.70"))
cat.table$range_ME <- range_ME$range[findInterval(cat.table$ME_50, range_ME$low)]
cat.table$range_ME <- factor(cat.table$range_ME, levels=range_ME$range)

## Draw the plot
size <- seq(2, 6.5, 1.5)
size <- setNames(size, range_ME$range)
col <- brewer.pal(3, "Paired")
#col <- c("#999999", col)
#col <- c("#999999", "#D8B365", "#AF8DC3")
col <- setNames(col, unique(cat.table$Ev_TYPE))
#shape <- c(19, 15, 17)
shape <- c(15, 17)
shape <- setNames(shape, unique(cat.table$Par_TYPE))
p <- ggplot(cat.table, aes(x=ET_50, y=EQ_50, color=Ev_TYPE, shape=Par_TYPE)) +
  geom_point(aes(size=range_ME)) + 
  geom_pointrange(aes(ymin=EQ_25, ymax=EQ_75), linewidth=0.5) + 
  geom_pointrange(aes(xmin=ET_25, xmax=ET_75), linewidth=0.5) + 
  #scale_shape_manual(values=shape, name="Parameterization", drop = FALSE, labels=c("Calibration + Regionalization", "Calibration", "Regionalization")) +
  scale_shape_manual(values=shape, name="Parameterization", drop = FALSE, labels=c("Calibration", "Regionalization")) +
  scale_color_manual(values=col, name="Event Types", drop = FALSE, labels=c("Rain-on-Dry", "Rain-on-Wet", "Snow-or-Ice")) + 
  scale_size_manual(values=size, name="Model Efficiency (ME)", drop = FALSE, labels=names(size)) + 
  geom_segment(x=0, xend=0, y=-7, yend=7, color="black") +
  geom_segment(x=-10, xend=7, y=0, yend=0, color="black") +
  scale_x_continuous(expand=c(0, 0), limits = c(-5, 1)) +
  scale_y_continuous(expand=c(0, 0), limits = c(-0.3, 0.6)) +
  ylab("Relative Magnitude Error") + xlab("Timing Error [days]") +
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey90", linewidth=0.5))

#plot_grid(p)
filename.plot <- paste0(dir_plot, "Compare_Cat_Plot.png")
ggsave(filename=filename.plot, width = 15, height = 15, units = "cm", plot=p)
filename.plot <- paste0(dir_plot, "Compare_Cat_Plot.pdf")
ggsave(filename=filename.plot, width = 15, height = 15, units = "cm", plot=p)