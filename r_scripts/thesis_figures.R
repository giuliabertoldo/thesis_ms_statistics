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

fig_3_7 <- viz_compare_pet_peese_estimate(df = df_sub, num_studies = 70, bias_type = "ORB Strong", smd_stsmd = "SMD")
fig_3_7
ggsave(file="figures/fig_3_7.png", plot=fig_3_7, width=10, height=8)

fig_3_8 <- viz_compare_pet_peese_estimate(df = df_sub, num_studies = 70, bias_type = "ORB Strong", smd_stsmd = "Transformed SMD")
fig_3_8
ggsave(file="figures/fig_3_8.png", plot=fig_3_8, width=10, height=8)

# Appendix A
## Moderate bias
krom_mod <- function(p){
  p_inclusion <- exp(-2*p^1.5)
  return(p_inclusion)
}

## Strong bias
krom_strong <- function(p){
  p_inclusion <- exp(-4*p^1.5)
  return(p_inclusion)
}

# Build dataframe
## P-values
p_val <- seq(from = 0, to = 1, by = 0.01)
## Probability of inclusions for the two methods in the two conditions (moderate, strong)
p_incl_krom_mod <- krom_mod(p_val)
p_incl_krom_strong <- krom_strong(p_val)
## Create dataframe
df <- as.data.frame(cbind(p_val, p_incl_krom_mod, p_incl_krom_strong))

fig_appendix_a <- ggplot(data = df, aes(x = p_val)) +
  geom_line(aes(y = p_incl_krom_mod, color = "Moderate bias")) +
  geom_line(aes(y = p_incl_krom_strong, color = "Strong bias")) +
  labs(y = "Probability of inclusion",
       x = "P-value",
       color = "Bias degree") +
  theme_bw()

fig_appendix_a
ggsave(file="figures/fig_appendix_a.png", plot=fig_appendix_a, width=6, height=3)
