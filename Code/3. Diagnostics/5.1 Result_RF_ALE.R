###### Libraries
library(randomForest)
library(caret)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(Metrics)
library(ggpubr)
library(doParallel)
library(foreach)
library(ALEPlot)

dir_plot <- c("Y:/Home/wangzhen/Eval_SALTO/Plot/")

fn.list <- list.files("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_rf_1979_2002_KGE_Balanced", pattern=".rda", full.names=T)
df <- NULL
for(fn.i in fn.list) {
  rf_imp <- readRDS(fn.i)
  df.i <- data.frame(R2=rf_imp$rf_R2,
                     RMSE=rf_imp$rf_RMSE,
                     var=rf_imp$var,
                     Ev_type=rf_imp$Ev_type,
                     ID=rf_imp$ID)
  
  df <- rbind(df, df.i)
}
df.group <- df %>% group_by(Ev_type, var) %>% summarise(R2.mean=mean(R2, na.rm=T), 
                                                             R2.sd=sd(R2, na.rm=T), 
                                                             RMSE.mean=mean(RMSE, na.rm=T), 
                                                             RMSE.sd=sd(RMSE, na.rm=T))

###### ImpPerc----  
fn.list <- list.files("Y:/Home/wangzhen/Eval_SALTO/Out/lumped_rf_1979_2002_KGE_Balanced", pattern=".rda", full.names=T)
rf_imp.ALE <- NULL
for(fn.i in fn.list) {
  rf_imp <- readRDS(fn.i)

  rf_imp.ALE.i <- rf_imp$ALE
  rf_imp.ALE.i$rank <- rank(-rf_imp.ALE.i$ALE.norm)
  rf_imp.ALE.i$var <- rf_imp$var
  rf_imp.ALE.i$Ev_type <- rf_imp$Ev_type
  rf_imp.ALE.i$ID <- rf_imp$ID
  rf_imp.ALE <- rbind(rf_imp.ALE, rf_imp.ALE.i)
}
x = rf_imp.ALE %>% group_by(PAR, var, Ev_type) %>% summarise(rank.sd=sd(rank))


rf_imp.ALE.lumped <- rf_imp.ALE
rf_imp.ALE.lumped$Parameterization <- 'Lumped'

fn.list <- list.files("Y:/Home/wangzhen/Eval_SALTO/Out/dist_rf_1979_2002_KGE_Balanced_Ralf_TOP5", pattern=".rda", full.names=T)
rf_imp.ALE <- NULL
for(fn.i in fn.list) {
  rf_imp <- readRDS(fn.i)
  
  rf_imp.ALE.i <- rf_imp$ALE
  rf_imp.ALE.i$var <- rf_imp$var
  rf_imp.ALE.i$Ev_type <- rf_imp$Ev_type
  rf_imp.ALE.i$ID <- rf_imp$ID
  rf_imp.ALE <- rbind(rf_imp.ALE, rf_imp.ALE.i)
}

rf_imp.ALE.dist <- rf_imp.ALE
rf_imp.ALE.dist$Parameterization <- 'Reg'

rf_imp.ALE.all <- rbind(rf_imp.ALE.lumped, rf_imp.ALE.dist)


ID.list <- rf_imp.ALE.all %>% 
  group_by(ID) %>% 
  summarise(n_distinct=n_distinct(Parameterization)) %>%
  filter(n_distinct == 2)

res_cor <- rf_imp.ALE.all %>%
  filter(ID %in% ID.list$ID) %>%
  select(var, Ev_type, ID, PAR, Parameterization, ALE.norm) %>%
  distinct() %>%
  pivot_wider(names_from = Parameterization, values_from = ALE.norm) %>%
  drop_na(Lumped, Reg) %>%
  arrange(var, Ev_type, ID, PAR) %>%
  group_by(var, Ev_type, ID) %>%
  summarise(
    n_PAR = n(),                            
    rho   = suppressWarnings(cor(Lumped, Reg, method = "spearman")),
    pval  = tryCatch(cor.test(Lumped, Reg, method = "spearman")$p.value,
                     error = function(e) NA_real_),
    .groups = "drop"
  )

res_cor %>% group_by(var, Ev_type) %>% summarise(rho.mean=mean(rho), rho.sd=sd(rho))
res_cor %>% group_by(var, Ev_type) %>% summarise(p.mean=median(pval), p.sd=sd(pval))


rf_imp.ALE$PAR <- factor(rf_imp.ALE$PAR, levels=rev(c("preEv_1d_et", "preEv_1d_eq", 
                                                      "Ev_peak", "Ev_t2pd", "Ev_Qduration", 
                                                      "PAR_swe", "PAR_sm", "PAR_aet", "PAR_lf_s", "PAR_lf_q",
                                                      "PAR_gwr", "PAR_gw2gw", "PAR_gw2r", "PAR_river_s")))

rf_imp.ALE$PACKAGE <- NULL
rf_imp.ALE[which(rf_imp.ALE$PAR == "preEv_1d_et" |
                   rf_imp.ALE$PAR == "preEv_1d_eq"), 
                "PACKAGE"] <- "Pre-Event Drivers"
rf_imp.ALE[which(rf_imp.ALE$PAR == "Ev_peak" |
                   rf_imp.ALE$PAR == "Ev_t2pd" |
                   rf_imp.ALE$PAR == "Ev_Qduration"), 
                "PACKAGE"] <- "Event-Type Drivers"
rf_imp.ALE[which(rf_imp.ALE$PAR == "Env_prec" |
                   rf_imp.ALE$PAR == "Env_temp" |
                   rf_imp.ALE$PAR == "PAR_swe" |
                   rf_imp.ALE$PAR == "PAR_sm" |
                   rf_imp.ALE$PAR == "PAR_aet" |
                   rf_imp.ALE$PAR == "PAR_lf_s" |
                   rf_imp.ALE$PAR == "PAR_lf_q" |
                   rf_imp.ALE$PAR == "PAR_gwr" |
                   rf_imp.ALE$PAR == "PAR_gw2gw" |
                   rf_imp.ALE$PAR == "PAR_gw2r" |
                   rf_imp.ALE$PAR == "PAR_river_s"), 
                "PACKAGE"] <- "Model-Process Drivers"

rf_imp.group <- rf_imp.ALE %>% group_by(PAR, Ev_type, var) %>% summarise(mean=mean(ALE.norm, na.rm=T), 
                                                                         q25=quantile(ALE.norm, 0.25, na.rm=T), 
                                                                         q75=quantile(ALE.norm, 0.75, na.rm=T))

rf_imp.group$PAR <- factor(rf_imp.group$PAR, levels=rev(c("preEv_1d_et", "preEv_1d_eq", 
                                                      "Ev_peak", "Ev_t2pd", "Ev_Qduration", 
                                                      "PAR_swe", "PAR_sm", "PAR_aet", "PAR_lf_s", "PAR_lf_q",
                                                      "PAR_gwr", "PAR_gw2gw", "PAR_gw2r", "PAR_river_s")))

rf_imp.group$var <- factor(rf_imp.group$var, levels=c("et", "eq"))

rf_imp.group$PACKAGE <- NULL
rf_imp.group[which(rf_imp.group$PAR == "preEv_1d_et" |
                     rf_imp.group$PAR == "preEv_1d_eq"), 
           "PACKAGE"] <- "Pre-Event Drivers"
rf_imp.group[which(rf_imp.group$PAR == "Ev_peak" |
                     rf_imp.group$PAR == "Ev_t2pd" |
                     rf_imp.group$PAR == "Ev_Qduration"), 
           "PACKAGE"] <- "Event-Type Drivers"
rf_imp.group[which(rf_imp.group$PAR == "Env_prec" |
                     rf_imp.group$PAR == "Env_temp" |
                     rf_imp.group$PAR == "PAR_swe" |
                     rf_imp.group$PAR == "PAR_sm" |
                     rf_imp.group$PAR == "PAR_aet" |
                     rf_imp.group$PAR == "PAR_lf_s" |
                     rf_imp.group$PAR == "PAR_lf_q" |
                     rf_imp.group$PAR == "PAR_gwr" |
                     rf_imp.group$PAR == "PAR_gw2gw" |
                     rf_imp.group$PAR == "PAR_gw2r" |
                     rf_imp.group$PAR == "PAR_river_s"), 
           "PACKAGE"] <- "Model-Process Drivers"

rf_imp.group$mean[which(rf_imp.group$mean < 0.001)] <- -10

one_block <- function(dat, group, show_x = FALSE) {
  dd <- dat %>% filter(PACKAGE %in% group)
  
  x_title_el <- if (show_x) element_text() else element_blank()
  x_text_el  <- if (show_x) element_text() else element_blank()
  
  ggplot(dd, aes(x = mean, y = PAR, color = factor(var))) +
    geom_hline(
      yintercept = seq(0.5, nlevels(dd$PAR) - 0.5, 1),
      color = "grey80", linewidth = 0.3
    ) +
    geom_pointrange(
      aes(xmin = q25, xmax = q75),
      position = position_dodge(width = 0.75),
      alpha = 0.7, na.rm = TRUE
    ) +
    scale_color_discrete(
      name = NULL,
      breaks = c("et", "eq"),
      labels = c("Time Error [days]", "Relative Runoff Error")
    ) +
    scale_x_continuous(
      name  = if (show_x) "Summarized Accumulated Local Effects (ALE)" else NULL,
      trans = "log10"
    ) +
    coord_cartesian(xlim = c(0.001, 1))+
    ylab(NULL) + theme_bw() +
    theme(
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_line(),
      axis.title.x = x_title_el,
      axis.text.x  = x_text_el)
}

for(Ev.i in c("S&I", "R.DRY", "R.WET")) {
  d <- rf_imp.group[rf_imp.group$Ev_type == Ev.i, ]
  
  p1 <- one_block(d, "Pre-Event Drivers", show_x = FALSE)
  p2 <- one_block(d, "Event-Type Drivers",  show_x = FALSE)
  p3 <- one_block(d, "Model-Process Drivers",  show_x = TRUE)
  
  heights <- c(length(ev_props), length(ext_src), length(int_lim))
  p <- (p1 / p2 / p3) +
    plot_layout(heights = heights, guides = "collect") & 
    theme(legend.position = "bottom")
  
  if(Ev.i == "S&I") {
    Ev_name <- "SI"
  } else if(Ev.i == "R.DRY") {
    Ev_name <- "RD"
  } else if(Ev.i == "R.WET") {
    Ev_name <- "RW"
  }
  filename.plot <- paste0(dir_plot, "Boxplot_Lumped_KGE_Balance_RF_PAR_Err_", Ev_name,".png")
  ggsave(filename=filename.plot, width = 7, height = 15, units = "cm", plot=p)
  filename.plot <- paste0(dir_plot, "Boxplot_Lumped_KGE_Balance_RF_PAR_Err_", Ev_name,".pdf")
  ggsave(filename=filename.plot, width = 7, height = 15, units = "cm", plot=p)
}
