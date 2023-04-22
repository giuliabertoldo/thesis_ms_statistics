testing_conditions_csv <- function(bias_type, k){
  bias_type <- bias_type
  delta_00 <- 0.2
  num_studies <- k
  sigma2u <- 0.01
  sigma2v <- 0.01
  psss <- c("small", "medium", "large")
  o <- 3

  col1 <- c(bias_type, bias_type, bias_type)
  col2 <- c(delta_00, delta_00, delta_00)
  col3 <- c(num_studies, num_studies, num_studies)
  col4 <- c(sigma2u, sigma2u, sigma2u)
  col5 <- c(sigma2v, sigma2v, sigma2v)
  col6 <- c(psss[1], psss[2], psss[3])
  col7 <- c(o, o, o)

  df <- as.data.frame(cbind(col1, col2, col3, col4, col5, col6, col7))
  names <- c("bias_type", "delta_00", "num_studies", "sigma2u", "sigma2v", "psss", "o")
  names(df) <- names

  # Save as csv
  name <- sprintf("conditions/test_%s_k_%d.csv", bias_type, k)
  write.csv(df, name, row.names = FALSE)
}

conditions_csv <- function(bias_type, k){
  # Simulation parameters ----
  bias_type <- bias_type
  delta_00 <- c(0, 0.2, 0.5, 0.8)
  num_studies <- k
  sigma2u <- c(0.01, 0.06, 0.11)
  sigma2v <- c(0.01, 0.06, 0.11)
  psss <- c("small", "medium", "large")
  o <- 3

  # Combine condition factors into data frame
  conditions <- expand.grid(bias_type, delta_00, num_studies, sigma2u, sigma2v, psss, o)
  names(conditions) <- c("bias_type", "delta_00", "num_studies", "sigma2u", "sigma2v", "psss", "o")

  # Save as csv
  name <- sprintf("conditions/%s_k_%d.csv", bias_type, k)
  write.csv(conditions, name, row.names = FALSE)

}

# UNBIASED
num_studies <- c(15, 30, 70)
conditions_csv(bias_type = bias_type, k = num_studies)

## Check
## test <- read.csv("conditions/pb_no_orb_no_k_30.csv")


# STRONG ORB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_no_orb_str", k = k)
  conditions_csv(bias_type = "pb_no_orb_str", k = k)
}

# STRONG PB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_str_orb_no", k = k)
  conditions_csv(bias_type = "pb_str_orb_no", k = k)
}

# MODERATE ORB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_no_orb_mod", k = k)
  conditions_csv(bias_type = "pb_no_orb_mod", k = k)
}

# MODERATE PB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_mod_orb_no", k = k)
  conditions_csv(bias_type = "pb_mod_orb_no", k = k)
}

# MODERATE PB - MODERATE ORB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_mod_orb_mod", k = k)
  conditions_csv(bias_type = "pb_mod_orb_mod", k = k)
}

# STRONG PB - STRONG ORB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_str_orb_str", k = k)
  conditions_csv(bias_type = "pb_str_orb_str", k = k)
}

# MODERATE PB -STRONG ORB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_mod_orb_str", k = k)
  conditions_csv(bias_type = "pb_mod_orb_str", k = k)
}

# STRONG PB - MODERATE ORB
num_studies <- c(15, 30, 70)
for (k in num_studies){
  testing_conditions_csv(bias_type = "pb_str_orb_mod", k = k)
  conditions_csv(bias_type = "pb_str_orb_mod", k = k)
}

# FULL BIASED
# Function ----
biased_conditions_csv <- function(bias_type){
  bias_type <- bias_type
  delta_00 <- c(0, 0.2, 0.5, 0.8)
  num_studies <- c(15, 30, 70)
  sigma2u <- c(0.01, 0.06, 0.11)
  sigma2v <- c(0.01, 0.06, 0.11)
  psss <- c("small", "medium", "large")
  o <- 3

  # Combine condition factors into data frame
  conditions <- expand.grid(bias_type, delta_00, num_studies, sigma2u, sigma2v, psss, o)
  names(conditions) <- c("bias_type", "delta_00", "num_studies", "sigma2u", "sigma2v", "psss", "o")

  # Save as csv
  name <- sprintf("condition_%s.csv", bias_type)
  write.csv(conditions, name, row.names = FALSE)
}
# Generate csvs ----
biased_conditions_csv("pb_no_orb_str")
biased_conditions_csv("pb_no_orb_mod")
biased_conditions_csv("pb_str_orb_no")
biased_conditions_csv("pb_mod_orb_no")
biased_conditions_csv("pb_str_orb_str")
biased_conditions_csv("pb_mod_orb_mod")
biased_conditions_csv("pb_str_orb_mod")
biased_conditions_csv("pb_mod_orb_str")



