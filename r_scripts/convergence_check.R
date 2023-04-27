# Convert pets_results to dataframe

# Function
save_pets_results <- function(bt, k, d, su, sv, p){
  temp0 <- sprintf("%s", bt)
  temp1 <- sprintf("k_%d",k)
  temp2 <- sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p)
  file_path <- file.path("data",temp0, temp1, temp2, "pets_results.Rdata")
  
  load(file_path)
  
  # Convert to dataframe
  df <- as.data.frame(do.call(rbind, pets_results))
  
  # Save in 
  write.csv(df, file.path("pet_results", sprintf("%s_k%g_d%g_su%g_sv%g_%s.csv", bt, k, d, su, sv, p)), row.names = FALSE)
}

# Use function
save_pets_results(bt = "pb_str_orb_no", k = 15, d = 0, su = 0.01, sv = 0.01, p = "small")
save_pets_results(bt = "pb_str_orb_no", k = 15, d = 0, su = 0.01, sv = 0.01, p = "medium")
save_pets_results(bt = "pb_str_orb_no", k = 15, d = 0, su = 0.01, sv = 0.01, p = "large")

save_pets_results(bt = "pb_str_orb_no", k = 15, d = 0, su = 0.06, sv = 0.06, p = "small")
save_pets_results(bt = "pb_str_orb_no", k = 15, d = 0, su = 0.06, sv = 0.06, p = "medium")
save_pets_results(bt = "pb_str_orb_no", k = 15, d = 0, su = 0.06, sv = 0.06, p = "large")

save_pets_results(bt = "pb_str_orb_no", k = 30, d = 0, su = 0.01, sv = 0.01, p = "small")

save_pets_results(bt = "pb_mod_orb_no", k = 15, d = 0, su = 0.01, sv = 0.01, p = "small")
