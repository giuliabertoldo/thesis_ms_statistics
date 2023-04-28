
library(tidyverse)

df <- read.csv("pet_results/pb_str_orb_no/k_15/pet_results_d0_su0.01_sv0.01_small.csv")

# Add index
df[, "index"] <- 1:1000
max(df$corrected_smd, na.rm = T)

max(df$corrected_st_smd, na.rm = T)

df1 <- df %>%
  filter(index == 65)

df1$peese_int

df2 <- df %>%
  filter(index == 64)

df2$peese_int
#
