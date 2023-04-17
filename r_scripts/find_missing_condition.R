library(tidyverse)

df <- read.csv("performances_data.csv")

# Select only condition of interest
df_sub <- df %>%
  filter(bt == "pb_str_orb_no",
         k == 70) %>%
  select("k", "delta_00", "sigma2_u", "sigma2_v", "psss")

# Create df with all conditions
k <- 70
delta_00 <- c(0, 0.2, 0.5, 0.8)
sigma2u <- c(0.01, 0.06, 0.11)
sigma2v <- c(0.01, 0.06, 0.11)
psss <- c("small", "medium", "large")

conditions <- expand.grid(k, delta_00, sigma2u, sigma2v, psss)
names(conditions) <- c("k", "delta_00","sigma2_u", "sigma2_v", "psss")

# Find missing condition
setdiff(conditions, df_sub)

# Create csv with that condition

# Simulation parameters ----
bias_type <- "pb_str_orb_no"
delta_00 <- 0.2
num_studies <- 70
sigma2u <- 0.06
sigma2v <- 0.01
psss <- "large"
o <- 3

# Combine condition factors into data frame
conditions <- expand.grid(bias_type, delta_00, num_studies, sigma2u, sigma2v, psss, o)
names(conditions) <- c("bias_type", "delta_00", "num_studies", "sigma2u", "sigma2v", "psss", "o")

# Save as csv
name <- sprintf("conditions/missing_%s_k_%d.csv", bias_type, k)
write.csv(conditions, name, row.names = FALSE)

