
###### Working Directory----
setwd("Y:/Home/wangzhen/Eval_SALTO/")

###### Libraries----
library(ggplot2)
#library(rgdal)
#library(maptools)
library(cowplot)
library(raster)
library(RColorBrewer)
library(dplyr)

############################################################
###### Functions----

############################################################

###### Map Drawing----
## Input directories
dir.lumped_sd_ts   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_SD_ts_1979_2002_KGE_Balanced/")
dir.dist_sd_ts   <- c("Y:/Home/wangzhen/Eval_SALTO/Out/dist_SD_ts_1979_2002_KGE_Balanced_Ralf_TOP5/")


## Output directories
dir_plot <- c("Y:/Home/wangzhen/Eval_SALTO/Plot/")

## Parameter initialization
legend_title <- "ME"

## Read the data
master.raster <- raster("Y:/Home/wangzhen/Eval_SALTO/Data/master_raster_5km.tif", convert=TRUE)
rivers     <- readOGR("Y:/Home/wangzhen/Eval_SALTO/Data/shp_files/rivers_crop.shp")
rivers     <- spTransform(rivers, crs(master.raster)) # transform projection
rivers_df  <- fortify(rivers)
Dbound     <- readOGR("Y:/Home/wangzhen/Eval_SALTO/Data/shp_files/germany.shp")
Dbound     <- spTransform(Dbound, crs(master.raster)) # transform projection
catchments <- readOGR("Y:/Home/wangzhen/Eval_SALTO/Data/shp_files/catchments.shp")
catchments <- spTransform(catchments, crs(master.raster)) 
catchments_df <- data.frame(GaugeID=catchments@data$GaugeID)
catchments_df$id <- as.character(seq(0, nrow(catchments_df) - 1))
catchments_df <- left_join(fortify(catchments), catchments_df, by="id")

############################################################
## Read the data

cat.table.lumped <- read.table(file=paste(dir.lumped_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)
cat.table.dist <- read.table(file=paste(dir.dist_sd_ts, "cat_table_ME.txt", sep=""), header=TRUE)

ids_lumped <- cat.table.lumped$ID[cat.table.lumped$ME_MAX >= 0.3]
ids_dist   <- cat.table.dist$ID[cat.table.dist$ME_MAX >= 0.3]
unique_ids <- intersect(ids_lumped, ids_dist)


cat.table <- cat.table.lumped
for(Ev_type in c("ALL", "SI", "RD", "RW")) {
 for(ME in c("ME", "ET", "EQ")) {
   cat.table[, paste(Ev_type, ME, "MEDIAN", sep="_")] <- (cat.table.lumped[, paste(Ev_type, ME, "MEDIAN", sep="_")] + cat.table.dist[, paste(Ev_type, ME, "MEDIAN", sep="_")]) / 2
 }
}

cat.table <- cat.table[which(cat.table$ID %in% unique_ids),]

cat.table <- cat.table[order(cat.table$AREA, decreasing=T),]
cat.table$GroupOrder <- order(cat.table$AREA, decreasing=T)

range_ME <- data.frame(low=c(-9999999, 0.5, 0.6, 0.7, 0.8, 0.9), 
                       range=c("<0.50", "0.50~0.60", "0.60~0.70", "0.70~0.80", "0.80~0.90", "0.90~1.00"))
range_ET <- data.frame(low=c(-9999999, -4, -3, -2, -1, 0, 1, 2, 3, 4), 
                       range=c("<-4", "-4~-3", "-3~-2", "-2~-1", "-1~0", "0~1", "1~2", "2~3", "3~4", ">4"))
range_EQ <- data.frame(low=c(-9999999, -0.4, -0.2, 0, 0.2, 0.4), 
                       range=c("<-0.4", "-0.4~-0.2", "-0.2~0", "0~0.2", 
                               "0.2~0.4", ">0.4"))
p <- list()

for(Ev_type in c("ALL", "SI", "RD", "RW")) {
  catchments_ME_df <- merge(catchments_df, cat.table[, c("ID", "GroupOrder", "AREA", 
                                                         paste(Ev_type, "ME_MEDIAN", sep="_"), 
                                                         paste(Ev_type, "ET_MEDIAN", sep="_"), 
                                                         paste(Ev_type, "EQ_MEDIAN", sep="_"))], 
                            by.x="GaugeID", by.y="ID")
  
  catchments_ME_df <- catchments_ME_df[complete.cases(catchments_ME_df), ]
  catchments_ME_df <- catchments_ME_df[order(catchments_ME_df$GroupOrder, decreasing=F),]
  catchments_ME_df_Gauge <- catchments_ME_df %>% group_by(GaugeID) %>% summarise(ME=median(ALL_ME_MEDIAN), ET=median(ALL_ET_MEDIAN), EQ=median(ALL_EQ_MEDIAN))
  catchments_ME_df$range_ME <- range_ME$range[findInterval(catchments_ME_df[, paste(Ev_type, "ME_MEDIAN", sep="_")], range_ME$low)]
  catchments_ME_df$range_ET <- range_ET$range[findInterval(catchments_ME_df[, paste(Ev_type, "ET_MEDIAN", sep="_")], range_ET$low)]
  catchments_ME_df$range_EQ <- range_EQ$range[findInterval(catchments_ME_df[, paste(Ev_type, "EQ_MEDIAN", sep="_")], range_EQ$low)]
  catchments_ME_df$range_ME <- factor(catchments_ME_df$range_ME, levels=range_ME$range)
  catchments_ME_df$range_ET <- factor(catchments_ME_df$range_ET, levels=range_ET$range)
  catchments_ME_df$range_EQ <- factor(catchments_ME_df$range_EQ, levels=range_EQ$range)
  
  col1 <- brewer.pal(6, "Greens")
  col1 <- setNames(col1, range_ME$range)
  p1 <- ggplot(catchments_ME_df) +
    geom_polygon(data=Dbound, aes(x=long, y=lat, group=group), fill=NA, color="grey") +
    geom_polygon(data=catchments, aes(x=long, y=lat, group=group), fill=NA, color="grey", linewidth=0.2) +
    geom_polygon(aes(x=long, y=lat, group=GroupOrder, fill=range_ME), color="grey", linewidth=0.1) +
    #geom_polygon(data=catchments_ME_df[catchments_ME_df$TRAIN == TRUE,], aes(x=long, y=lat, group=group), fill=NA, color="grey40", linewidth=0.2) + 
    geom_path(data=rivers_df, aes(x=long, y=lat, group=group), color='steelblue4', linewidth=0.2) +
    scale_fill_manual(values=col1, drop = FALSE,
                      labels=names(col1), name=legend_title) +
    coord_equal() +
    theme(axis.line=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          panel.background=element_blank(), panel.border=element_blank(), 
          panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
          plot.background=element_blank())
  
  
  
  col_et <- rev(brewer.pal(11, "RdYlBu")[-6])
  col_et <- setNames(col_et, range_ET$range)
  p_et <- ggplot(catchments_ME_df[catchments_ME_df[, paste(Ev_type, "ET_MEDIAN", sep="_")] > -99,]) +
    geom_polygon(data=Dbound, aes(x=long, y=lat, group=group), fill=NA, color="grey") +
    geom_polygon(data=catchments, aes(x=long, y=lat, group=group), fill=NA, color="grey", linewidth=0.2) +
    geom_polygon(aes(x=long, y=lat, group=GroupOrder, fill=range_ET), color="grey", linewidth=0.1) +
    #geom_polygon(data=catchments_ME_df[catchments_ME_df$TRAIN == TRUE,], aes(x=long, y=lat, group=GroupOrder), fill=NA, color="grey40", linewidth=0.2) + 
    geom_path(data=rivers_df, aes(x=long, y=lat, group=group), color='steelblue4', linewidth=0.2) +
    scale_fill_manual(values=col_et, drop = FALSE,
                      labels=names(col_et), name="Et [days]") +
    coord_equal() +
    theme(axis.line=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          panel.background=element_blank(), panel.border=element_blank(), 
          panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
          plot.background=element_blank())
  
  col_eq <- rev(brewer.pal(7, "RdYlBu")[-4])
  col_eq <- setNames(col_eq, range_EQ$range)
  p_eq <- ggplot(catchments_ME_df[catchments_ME_df[, paste(Ev_type, "EQ_MEDIAN", sep="_")] > -99,]) +
    geom_polygon(data=Dbound, aes(x=long, y=lat, group=group), fill=NA, color="grey") +
    geom_polygon(data=catchments, aes(x=long, y=lat, group=group), fill=NA, color="grey", linewidth=0.2) +
    geom_polygon(aes(x=long, y=lat, group=GroupOrder, fill=range_EQ), color="grey", linewidth=0.1) +
    #geom_polygon(data=catchments_ME_df[catchments_ME_df$TRAIN == TRUE,], aes(x=long, y=lat, group=GroupOrder), fill=NA, color="grey40", linewidth=0.2) + 
    geom_path(data=rivers_df, aes(x=long, y=lat, group=group), color='steelblue4', linewidth=0.2) +
    scale_fill_manual(values=col_eq, drop = FALSE,
                      labels=names(col_eq), name="Eq") +
    coord_equal() +
    theme(axis.line=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          panel.background=element_blank(), panel.border=element_blank(), 
          panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
          plot.background=element_blank())
  
  # p.i <- plot_grid(p1 + theme(legend.position="none"), 
  #                p_et + theme(legend.position="none"), 
  #                p_eq + theme(legend.position="none"),
  #                rel_widths = c(1, 1, 1),
  #                ncol=3, nrow=1, align="h")
  p[[paste0(Ev_type, "_ME")]] <- p1
  p[[paste0(Ev_type, "_ET")]] <- p_et
  p[[paste0(Ev_type, "_EQ")]] <- p_eq
  # filename.plot <- paste0(dir_plot, "Map_lumped_", Ev_type,".png")
  # ggsave(filename=filename.plot, width = 20, height = 10, units = "cm", plot=p.i)
}

p.ME <- plot_grid(p$ALL_ME + theme(legend.position="none"), 
                  p$SI_ME + theme(legend.position="none"), 
                  p$RD_ME + theme(legend.position="none"), 
                  p$RW_ME + theme(legend.position="none"), 
                  get_legend(p$ALL_ME),
                  #rel_widths = c(1, 1, 1, 1, 0.5),
                  nrow=1, align="h")
p.ET <- plot_grid(p$ALL_ET + theme(legend.position="none"), 
                  p$SI_ET + theme(legend.position="none"), 
                  p$RD_ET + theme(legend.position="none"), 
                  p$RW_ET + theme(legend.position="none"), 
                  get_legend(p$ALL_ET),
                  #rel_widths = c(1, 1, 1, 1, 0.5),
                  nrow=1, align="h")
p.EQ <- plot_grid(p$ALL_EQ + theme(legend.position="none"), 
                  p$SI_EQ + theme(legend.position="none"), 
                  p$RD_EQ + theme(legend.position="none"), 
                  p$RW_EQ + theme(legend.position="none"), 
                  get_legend(p$ALL_EQ),
                  #rel_widths = c(1, 1, 1, 1, 0.5),
                  nrow=1, align="h")

p.all <- plot_grid(p.ME, p.ET, p.EQ,
                   rel_heights = c(1, 1, 1),
                   ncol=1, align="v")

ggsave(filename=paste0(dir_plot, "Map_lumped.png"), width = 25, height = 20, units = "cm", plot=p.all)
ggsave(filename=paste0(dir_plot, "Map_lumped.pdf"), width = 25, height = 20, units = "cm", plot=p.all)


########################################################
median(cat.table$SI_ET_MEDIAN, na.rm=T)
quantile(cat.table$ALL_ET_MEDIAN, 0.05, na.rm=T)
quantile(cat.table$ALL_ET_MEDIAN, 0.95, na.rm=T)
