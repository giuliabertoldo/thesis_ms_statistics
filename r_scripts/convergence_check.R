# Convert pets_results to dataframe

# Z score 
z_score <- function(df, col){
  mean_col <- mean(df[, col], na.rm = T)
  sd <- sd(df[, col], na.rm = T)
  z_col_name <- sprintf("z_%s", col)
  df[, z_col_name] <- NA 
  df[, z_col_name] <- (df[, col] - mean_col) / sd
  return(df)
}

# Function
save_pets_results <- function(bt, k, d, su, sv, p){
  temp0 <- sprintf("%s", bt)
  temp1 <- sprintf("k_%d",k)
  temp2 <- sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p)
  file_path <- file.path("data",temp0, temp1, temp2, "pets_results.Rdata")
  
  load(file_path)
  
  # Convert to dataframe
  df <- as.data.frame(do.call(rbind, pets_results))
  
  # Add columns with z scores 
  df <- z_score(df, "pet_int")
  df <- z_score(df, "pet_st_int")
  df <- z_score(df, "pet_slope")
  df <- z_score(df, "pet_st_slope")
 
  df <- z_score(df, "peese_int")
  df <- z_score(df, "peese_st_int")
  df <- z_score(df, "peese_slope")
  df <- z_score(df, "peese_st_slope")
  
  df <- z_score(df, "corrected_smd")
  df <- z_score(df, "corrected_st_smd")
  
  # Save in: 
  save_path_bt <- file.path("pet_results", sprintf("%s", bt))
  if (!file.exists(save_path_bt)){
    dir.create(save_path_bt)
  }
  save_path_k <- file.path("pet_results", sprintf("%s", bt), sprintf("k_%g", k))
  if (!file.exists(save_path_k)){
    dir.create(save_path_k)
  }
  write.csv(df, file.path(save_path_bt, sprintf("k_%g", k), sprintf("pet_results_d%g_su%g_sv%g_%s.csv", d, su, sv, p)), row.names = FALSE)
}

bias_type = c("pb_no_orb_no", "pb_no_orb_str", "pb_no_orb_mod", "pb_str_orb_no", "pb_mod_orb_no")
num_studies = c(15, 30, 70)
delta_00 = c(0, 0.2, 0.5, 0.8)
sigma2_u = c(0.01, 0.06, 0.11)
sigma2_v =c(0.01, 0.06, 0.11)
psss = c("small", "medium", "large")

start.time <- Sys.time()

for (bt in bias_type){
  for(k in num_studies){
    for(d in delta_00){
      for(su in sigma2_u){
        for(sv in sigma2_v){
          for(p in psss){
            save_pets_results(bt = bt, k = k, d = d, su = su, sv = sv, p = p)
          }
        }
      }
    }
  }
}

end.time <- Sys.time()
(time.taken <- end.time - start.time)
