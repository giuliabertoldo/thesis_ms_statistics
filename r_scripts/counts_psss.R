
bias_type = c("pb_no_orb_str", "pb_no_orb_mod", 
              "pb_str_orb_no", "pb_mod_orb_mod", 
              "pb_str_orb_str", "pb_mod_orb_mod",
              "pb_mod_orb_str", "pb_str_orb_mod",
              "pb_no_orb_no")
num_stud = c(15,30, 70)
delta_00 = c(0,0.2, 0.5, 0.8)
sigma2_u = c(0.01, 0.06, 0.11)
sigma2_v = c(0.01, 0.06, 0.11)
psss = c("small", "medium","large")

df_out = data.frame()

j = 0
for(bt in bias_type){
  for(k in num_stud){
    for(d in delta_00){for(su in sigma2_u){
      for(sv in sigma2_v){
        for(p in psss){
          j = j+1
          folder_path <- file.path("data", sprintf("%s",bt), sprintf("k_%d", k), sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p))
          
          load(file.path(folder_path, "pets_results.Rdata"))
          
          ss <- c()
          for (i in 1: 1000){
            ss[i] <- pets_results[[i]]['avg_n_e']+pets_results[[i]]['avg_n_c']
          }
          
          avg_ss <-  mean(ss)
            
          df_out[j, 'id'] <-  sprintf("%s_k_%d_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)
          df_out[j, 'psss_nominal'] <- p 
          df_out[j, 'avg_ss'] <- avg_ss
        }
      }
    }
    
  }
  }
}

write.csv(df_out, "/vsc-hard-mounts/leuven-data/354/vsc35419/thesis_ms_statistics/avg_psss_counts.csv", row.names = FALSE)


    
      
        