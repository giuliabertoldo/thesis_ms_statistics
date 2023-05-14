source("r_scripts/dataviz_functions_app.R")
library(svglite)
library(metafor)
library(tidyverse)

df <- read.csv("performances.csv")
df_sub <- read.csv("performances_sub.csv")


# Figure 1.1: Funnel plot
dat <- dat.raudenbush1985
fem <- rma(yi, vi, data=dat, measure="SMD", method="FE")
png("figures/fig_1_1.png")
funnel(fem)
dev.off()


fig_3_1 <- viz_rejection_rate(df = df_sub, num_studies = 70, bias_type = "None")
fig_3_1
ggsave(file="figures/fig_3_1.png", plot=fig_3_1, width=10, height=8)

fig_3_2 <- viz_rejection_rate(df = df_sub, num_studies = 70, bias_type = "ORB Strong")
fig_3_2
ggsave(file="figures/fig_3_2.png", plot=fig_3_2, width=10, height=8)


fig_3_3 <- viz_hist_perc_excluded_by_d(df = df)
fig_3_3
ggsave(file="figures/fig_3_3.png", plot=fig_3_3, width=10, height=5)

fig_3_4 <- viz_rr_pet_int(df = df_sub, num_studies = 70, bias_type = "ORB Strong")
fig_3_4
ggsave(file="figures/fig_3_4.png", plot=fig_3_4, width=10, height=8)


fig_3_5 <- viz_adj_est_bias(df = df_sub, num_studies = 70, bias_type = "ORB Strong")
fig_3_5
ggsave(file="figures/fig_3_5.png", plot=fig_3_5, width=10, height=8)

fig_3_6 <- viz_adj_est_rmse(df = df_sub, num_studies = 70, bias_type = "ORB Strong")
fig_3_6
ggsave(file="figures/fig_3_6.png", plot=fig_3_6, width=10, height=8)
