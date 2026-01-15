library(splitstackshape)
library(philentropy)
source("Y:/Home/wangzhen/Eval_SALTO/R_functions/Eval_Metrics.R")

###### Working Directory
setwd("Y:/Home/wangzhen/Eval_SALTO")

## Input directories
dir.lumped_sim_q  <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_qout_1979_2002_KGE_Balanced/")
dir.dist_sim_q    <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_qout_1979_2002_KGE_Balanced_Ralf/")
dir.lump_par <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_par_1990_2000_KGE_Balanced/")

## Calibration period
startdate.C <- as.Date(strptime("01:11:1990", format="%d:%m:%Y"))
enddate.C   <- as.Date(strptime("31:10:2000", format="%d:%m:%Y"))

## Validation period
startdate.V <- as.Date(strptime("01:10:1979", format="%d:%m:%Y"))
enddate.V   <- as.Date(strptime("31:10:1990", format="%d:%m:%Y"))

############################################################
cat.table <- read.table(file = paste0(dir.lumped_sim_q, "cat_table_ME.txt"), header=TRUE)
par.bounds <- read.table("F:/Zhenyu/D_SALTO/5km_resolution/SALTO_V2_parameter.txt", header=TRUE)
cat.table_tmp <- cat.table[, 1:8]

for(i in 1:nrow(cat.table_tmp)) {
  filename <- paste(dir.lumped_sim_q, cat.table_tmp$ID[i], ".txt", sep="")
  if(file.exists(filename)) {
    print(filename)
    qsim <- read.table(filename, header=TRUE)
    qsim$date <- as.Date(qsim$date)
    cat.par.raw <- read.table(paste0(dir.lump_par, "DDS_", cat.table_tmp$ID[i], ".txt"), header=TRUE)
    cat.par <- cat.par.raw[, -1]
    
    par.min <- matrix(rep(c(par.bounds$PAR_MIN), nrow(cat.par)), ncol = ncol(cat.par), byrow = TRUE)
    par.range <- matrix(rep(c(par.bounds$PAR_MAX - par.bounds$PAR_MIN), nrow(cat.par)), ncol = ncol(cat.par), byrow = TRUE)
    cat.par.norm <- (cat.par - par.min) / par.range
    cat.par.norm <- cat.par.norm[, -grep("TS|SM_LAYER", colnames(cat.par.norm))]
    
    par.dist <- distance(cat.par.norm, method = "euclidean")
    par.dist[par.dist == 0] <- NA
    cat.par.raw$dist <- apply(par.dist, 2, mean, na.rm=TRUE)
    cat.par.raw$dist[which(cat.par.raw$ME == max(cat.par.raw$ME))] <- max(cat.par.raw$dist) + 1
    cat.par.raw <- cat.par.raw[which(cat.par.raw$ME > 0.9 * max(cat.par.raw$ME)),]
    cat.par.raw <- cat.par.raw[order(cat.par.raw$dist, decreasing = T), ]
    top5.rowname <- paste0("Qsim", rownames(cat.par.raw)[1:5])
    
    qsim.C <- subset(qsim, date >= startdate.C & date <= enddate.C)
    
    for(j in 1:length(top5.rowname)) {
      rowname.i <- top5.rowname[j]
      q_tmp <- qsim.C[, which(colnames(qsim.C) == "Qobs" | colnames(qsim.C) == rowname.i)]
      
      q_tmp <- q_tmp[complete.cases(q_tmp),]
      if(nrow(q_tmp) < 100) {
        next
      }
      cat.table_tmp[i, paste0("Lump_C_Qsim", j)] <- KGE_Balance(q_tmp[, 1], q_tmp[, 2], 0)
    }
    
    qsim.V <- subset(qsim, date >= startdate.V & date <= enddate.V)
    
    for(j in 1:length(top5.rowname)) {
      rowname.i <- top5.rowname[j]
      q_tmp <- qsim.V[, which(colnames(qsim.V) == "Qobs" | colnames(qsim.V) == rowname.i)]
      
      q_tmp <- q_tmp[complete.cases(q_tmp),]
      if(nrow(q_tmp) < 100) {
        next
      }
      cat.table_tmp[i, paste0("Lump_V_Qsim", j)] <- KGE_Balance(q_tmp[, 1], q_tmp[, 2], 0)
    }
  }
}

for(i in 1:nrow(cat.table_tmp)) {
  filename <- paste(dir.dist_sim_q, cat.table_tmp$ID[i], ".txt", sep="")
  if(file.exists(filename)) {
    print(filename)
    qsim <- read.table(filename, header=TRUE)
    qsim$date <- as.Date(qsim$date)
   
    qsim.C <- subset(qsim, date >= startdate.C & date <= enddate.C)
    
    for(j in 1:5) {
      rowname.i <- paste0("Qsim", j)
      q_tmp <- qsim.C[, which(colnames(qsim.C) == "Qobs" | colnames(qsim.C) == rowname.i)]
      
      q_tmp <- q_tmp[complete.cases(q_tmp),]
      if(nrow(q_tmp) < 100) {
        next
      }
      cat.table_tmp[i, paste0("Dist_C_Qsim", j)] <- KGE_Balance(q_tmp[, 1], q_tmp[, 2], 0)
    }
    
    qsim.V <- subset(qsim, date >= startdate.V & date <= enddate.V)
    
    for(j in 1:length(top5.rowname)) {
      rowname.i <- paste0("Qsim", j)
      q_tmp <- qsim.V[, which(colnames(qsim.V) == "Qobs" | colnames(qsim.V) == rowname.i)]
      
      q_tmp <- q_tmp[complete.cases(q_tmp),]
      if(nrow(q_tmp) < 100) {
        next
      }
      cat.table_tmp[i, paste0("Dist_V_Qsim", j)] <- KGE_Balance(q_tmp[, 1], q_tmp[, 2], 0)
    }
  }
}

write.table(cat.table_tmp, "Y:/Home/wangzhen/Eval_SALTO/Out/Cali_Vali.txt")
############################################################
cat.table <- read.table(file = "Y:/Home/wangzhen/Eval_SALTO/Out/Cali_Vali.txt", header=TRUE)
cat.table$Ev <- FALSE
for(i in 1:nrow(cat.table)) {
  filename <- paste("Y:/Home/wangzhen/Eval_SALTO/Out/event_type_1979_2002/", cat.table$ID[i], ".txt", sep="")
  if(file.exists(filename)) {
    cat.table$Ev <- TRUE
  }
}
write.table(cat.table, "Y:/Home/wangzhen/Eval_SALTO/Out/Cali_Vali.txt")

############################################################
library(ggplot2)
library(cowplot)
library(dplyr)

dir_plot <- c("Y:/Home/wangzhen/Eval_SALTO/Plot/")

cat.table <- read.table(file = "Y:/Home/wangzhen/Eval_SALTO/Out/Cali_Vali.txt", header=TRUE)
dist.cat.table <- read.table(file = paste0(dir.dist_sim_q, "cat_table_ME.txt"), header=TRUE)
dist.cat.table <- dist.cat.table[, grep("ID|TRAIN", colnames(dist.cat.table))]
cat.table <- left_join(cat.table, dist.cat.table, by="ID")

cat.table.tmp <- dist.cat.table

cat.table.Lump_C_Qsim <- cat.table[, grep("Lump_C_Qsim", colnames(cat.table))]
cat.table.Lump_C_Qsim[is.na(cat.table.Lump_C_Qsim)] <- -9
cat.table.tmp$"Lump_C_Qsim_MAX" <-  apply(cat.table.Lump_C_Qsim, 1, quantile, probs=0.95, na.rm=TRUE)
cat.table.tmp$"Lump_C_Qsim_MIN" <-  apply(cat.table.Lump_C_Qsim, 1, quantile, probs=0.05, na.rm=TRUE)
cat.table.tmp$"Lump_C_Qsim_MEDIAN" <-  apply(cat.table.Lump_C_Qsim, 1, median, na.rm=TRUE)
cat.table.tmp$"Lump_C_Qsim_AVE" <-  apply(cat.table.Lump_C_Qsim, 1, mean, na.rm=TRUE)

cat.table.Lump_V_Qsim <- cat.table[, grep("Lump_V_Qsim", colnames(cat.table))]
cat.table.Lump_V_Qsim[is.na(cat.table.Lump_V_Qsim)] <- -9
cat.table.tmp$"Lump_V_Qsim_MAX" <-  apply(cat.table.Lump_V_Qsim, 1, quantile, probs=0.95, na.rm=TRUE)
cat.table.tmp$"Lump_V_Qsim_MIN" <-  apply(cat.table.Lump_V_Qsim, 1, quantile, probs=0.05, na.rm=TRUE)
cat.table.tmp$"Lump_V_Qsim_MEDIAN" <-  apply(cat.table.Lump_V_Qsim, 1, median, na.rm=TRUE)
cat.table.tmp$"Lump_V_Qsim_AVE" <-  apply(cat.table.Lump_V_Qsim, 1, mean, na.rm=TRUE)

cat.table.Dist_C_Qsim <- cat.table[, grep("Dist_C_Qsim", colnames(cat.table))]
cat.table.Dist_C_Qsim[is.na(cat.table.Dist_C_Qsim)] <- -9
cat.table.tmp$"Dist_C_Qsim_MAX" <-  apply(cat.table.Dist_C_Qsim, 1, quantile, probs=0.95, na.rm=TRUE)
cat.table.tmp$"Dist_C_Qsim_MIN" <-  apply(cat.table.Dist_C_Qsim, 1, quantile, probs=0.05, na.rm=TRUE)
cat.table.tmp$"Dist_C_Qsim_MEDIAN" <-  apply(cat.table.Dist_C_Qsim, 1, median, na.rm=TRUE)
cat.table.tmp$"Dist_C_Qsim_AVE" <-  apply(cat.table.Dist_C_Qsim, 1, mean, na.rm=TRUE)

cat.table.Dist_V_Qsim <- cat.table[, grep("Dist_V_Qsim", colnames(cat.table))]
cat.table.Dist_V_Qsim[is.na(cat.table.Dist_V_Qsim)] <- -9
cat.table.tmp$"Dist_V_Qsim_MAX" <-  apply(cat.table.Dist_V_Qsim, 1, quantile, probs=0.95, na.rm=TRUE)
cat.table.tmp$"Dist_V_Qsim_MIN" <-  apply(cat.table.Dist_V_Qsim, 1, quantile, probs=0.05, na.rm=TRUE)
cat.table.tmp$"Dist_V_Qsim_MEDIAN" <-  apply(cat.table.Dist_V_Qsim, 1, median, na.rm=TRUE)
cat.table.tmp$"Dist_V_Qsim_AVE" <-  apply(cat.table.Dist_V_Qsim, 1, mean, na.rm=TRUE)

cat.table.tmp1 <- apply(cat.table.tmp, 2, sort)
cat.table.tmp1 <- as.data.frame(cat.table.tmp1)

#cat.table.tmp1 <- cat.table.tmp1[-c(1:15),]
cat.table.tmp1$frac <- c(1:nrow(cat.table.tmp1)) / nrow(cat.table.tmp1)

p1 <-  ggplot(cat.table.tmp1, aes(x=frac)) +
          geom_line(aes(y=Lump_C_Qsim_MEDIAN, color="Calibration"), linetype="solid", linewidth=0.5) + 
          geom_ribbon(aes(ymin=Lump_C_Qsim_MIN, ymax=Lump_C_Qsim_MAX), fill="#A4514F", alpha=0.4) +
          geom_line(aes(y=Dist_C_Qsim_MEDIAN, color="Regionalization"), linetype="solid", linewidth=0.5) + 
          geom_ribbon(aes(ymin=Dist_C_Qsim_MIN, ymax=Dist_C_Qsim_MAX), fill="#6C91C2", alpha=0.4) +
          coord_flip(ylim=c(0, 1)) +
          ylab("Model efficiency score") +
          xlab("Fraction of catchments") +
          ggtitle("(a) Calibration period") +
          scale_color_manual(values = c("Calibration" = "#A4514F", "Regionalization" = "#6C91C2")) +
          theme(legend.title = element_blank(),
                legend.position = c(0.85, 0.09),
                legend.text = element_text(colour="black", size=10.5),
                axis.title.x = element_text(size=12, face="bold", colour = "black"),
                axis.text.x = element_text(size=10.5, colour = "black"),
                axis.title.y = element_text(size=12, face="bold", colour = "black"),
                axis.text.y = element_text(size=10.5, colour = "black"))

p2 <- ggplot(cat.table.tmp1, aes(x=frac)) +
        geom_line(aes(y=Lump_V_Qsim_MEDIAN, color="Calibration"), linetype="dashed", linewidth=0.5) + 
        geom_ribbon(aes(ymin=Lump_V_Qsim_MIN, ymax=Lump_V_Qsim_MAX), fill="#A4514F", alpha=0.4) +
        geom_line(aes(y=Dist_V_Qsim_MEDIAN, color="Regionalization"), linetype="dashed", linewidth=0.5) + 
        geom_ribbon(aes(ymin=Dist_V_Qsim_MIN, ymax=Dist_V_Qsim_MAX), fill="#6C91C2", alpha=0.4) +
        coord_flip(ylim=c(0, 1)) +
        ylab("Model efficiency score") +
        xlab("Fraction of catchments") +
        ggtitle("(b) Validation period") +
        scale_color_manual(values = c("Calibration" = "#A4514F", "Regionalization" = "#6C91C2")) +
        theme(legend.title = element_blank(),
              legend.position = c(0.85, 0.09),
              legend.text = element_text(colour="black", size=10.5),
              axis.title.x = element_text(size=12, face="bold", colour = "black"),
              axis.text.x = element_text(size=10.5, colour = "black"),
              axis.title.y = element_text(size=12, face="bold", colour = "black"),
              axis.text.y = element_text(size=10.5, colour = "black"))

p <- plot_grid(p1, p2, ncol=2, nrow=1, align="h")

filename.plot <- paste0(dir_plot, "Cali_Vali.png")
ggsave(filename=filename.plot, width = 30, height = 12, units = "cm", plot=p)


