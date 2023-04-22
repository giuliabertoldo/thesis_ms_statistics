
# bias_type = c("pb_no_orb_str", "pb_str_orb_no", "pb_str_orb_str")
# num_stud = c(15,30,70)
# delta_00 = c(0, 0.2, 0.5, 0.8)
# sigma2_u = c(0.01, 0.06, 0.11)
# sigma2_v = c(0.01, 0.06, 0.11)
# psss = c("small", "medium", "large")

bias_type = c("pb_str_orb_str")
num_stud = c(15,30, 70)
delta_00 = c(0,0.2, 0.5, 0.8)
sigma2_u = c(0.01, 0.06, 0.11)
sigma2_v = c(0.01, 0.06, 0.11)
psss = c("small", "medium","large")

o = 3
nmeta = 1000

start_time <- Sys.time()
# Initialize dataframes
out_df = data.frame()
df_merged <- data.frame()
df_temp <- data.frame()

for(bt in bias_type){
  for(k in num_stud){
    for(d in delta_00){
      for(su in sigma2_u){
        for(sv in sigma2_v){
          for(p in psss){
            folder_path <- file.path("data", sprintf("%s",bt), sprintf("k_%d", k), sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p))
            
            # Count the actual number of meta-analyses
            path_errors <- file.path(folder_path, "errors.Rdata")
            load(path_errors)
            
            
            
            # for n in nmeta
            for(n in 1:nmeta){
              path_meta = file.path(folder_path, sprintf("biased_meta%d.Rdata", n))
              
              load(path_meta)
              
              # Original number of studies in the meta
              out_df[n, 'k_origin'] <- k
              
              # Original total number of outcomes in the meta
              out_df[n, 'tot_num_out_original'] <- k*o
              
              # Real number of studies in the meta
              out_df[n, 'k_real'] <- length(unique(df_biased$study_id))
              
              # Real total number of outcomes in the meta
              out_df[n, 'tot_num_out_real'] <- dim(df_biased)[1]
              
              # If there are more than zero outcomes
              if(out_df[n, 'tot_num_out_real'] > 0) {
                # Number of outcomes for each study
                n_out_per_study <- aggregate(cbind("num_out" = out_id) ~ study_id,
                                             data = df_biased,
                                             FUN = NROW)
                
                # Average, min, max number of outcomes per study in the meta
                out_df[n, 'avg_n_out_per_study'] <- sum(n_out_per_study$num_out)/out_df$k_real[n]
                out_df[n, 'min_n_out_per_study'] <- min(n_out_per_study$num_out)
                out_df[n, 'max_n_out_per_study'] <- max(n_out_per_study$num_out)
                
                # Percentage selected outcomes
                out_df[n, 'perc_out_selected'] <- (out_df$tot_num_out_real[n]/out_df$tot_num_out_original[n] ) * 100
                
                # Percentage selected studies
                out_df[n, 'perc_stud_selected'] <- (out_df$k_real[n]/out_df$k_origin[n]) * 100
              } else {
                
                # Average, min, max number of outcomes per study in the meta
                out_df[n, 'avg_n_out_per_study'] <- 0
                out_df[n, 'min_n_out_per_study'] <- 0
                out_df[n, 'max_n_out_per_study'] <- 0
                
                # Percentage selected outcomes
                out_df[n, 'perc_out_selected'] <- 0
                
                # Percentage selected studies
                out_df[n, 'perc_stud_selected'] <- 0
              }
            }
            
            
            df_temp[1, 'id'] <-  sprintf("%s_k_%d_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)
            
            df_temp[1, 'avg_perc_out_selected'] <- mean(out_df$perc_out_selected)
            df_temp[1, 'avg_num_out_selected'] <- mean(out_df$tot_num_out_real)
            
            df_temp[1, 'avg_perc_stud_selected'] <- mean(out_df$perc_stud_selected)
            df_temp[1, 'avg_num_study_selected'] <- mean(out_df$k_real)
            
            df_temp[1, 'avg_avg_n_out_per_study'] <- mean(out_df$avg_n_out_per_study)
            df_temp[1, 'avg_min_n_out_per_study'] <- mean(out_df$min_n_out_per_study)
            df_temp[1, 'avg_max_n_out_per_study'] <- mean(out_df$max_n_out_per_study)
            
            
            df_merged <- rbind(df_merged, df_temp)
            
          }
        }
      }
    }
  }
}

write.csv(df_merged, "/vsc-hard-mounts/leuven-data/354/vsc35419/thesis_ms_statistics/counts.csv", row.names = FALSE)

end_time <- Sys.time()
end_time - start_time

