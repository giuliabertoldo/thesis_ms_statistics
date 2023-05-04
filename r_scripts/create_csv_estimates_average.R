# Convert the variance stabilized transfomed smd back to smd ----
convert_h_to_d <- function(h, avg_n_e, avg_n_c){
  # If there was an error in pets, h = NA, then sinh(NA)= NA, then = NA
  a <- sqrt(4 + 2*(avg_n_e/avg_n_c) +2*(avg_n_c/avg_n_e))
  d <- a*sinh(h/sqrt(2))
  return(d)
}

bias_type = c("pb_no_orb_no", "pb_no_orb_str", "pb_no_orb_mod", "pb_str_orb_no", "pb_mod_orb_no")
num_studies = c(15, 30, 70)
psss = c("small", "medium", "large")
sigma2_u = c(0.01, 0.06, 0.11)
sigma2_v =  c(0.01, 0.06, 0.11)
delta_00 = c(0, 0.2, 0.5, 0.8)

n_conditions <- length(bias_type)*length(num_studies)*length(psss)*length(sigma2_u)*length(sigma2_v)*length(delta_00)

# Initialize output df
df_out <- data.frame()


start.time <- Sys.time()

i <- 0
for(bt in bias_type){
  for(k in num_studies){
    for(d in delta_00){
      for(su in sigma2_u){
        for(sv in sigma2_v){
          for(p in psss){
            i <-  i +1

            temp0 <- sprintf("%s", bt)

            temp1 <- sprintf("k_%d",k)

            temp2 <- sprintf("pet_results_d%g_su%g_sv%g_%s.csv", d, su, sv, p)

            file_path <- file.path("pet_results",temp0, temp1, temp2)

            df <- read.csv(file_path)

            df_out[i, "index"] <- i
            
            df_out[i, "bt"] <- bt

            df_out[i, "k"] <- k

            df_out[i, "d"] <- d

            df_out[i, "su"] <- su

            df_out[i, "sv"] <- sv

            df_out[i, "p"] <- p
            
            df_out[i, "id"] <- sprintf("%s_k_%g_d%0.02f_su%0.02f_sv%0.02f_%s", bt, k, d, su, sv, p)


            df_out[i,"pet_int_avg"] <-  mean(df$pet_int, na.rm = T)
            df_out[i,"pet_slope_avg"] <-  mean(df$pet_slope, na.rm = T)

            df_out[i,"pet_st_int_avg"] <-  mean(df$pet_st_int, na.rm = T)
            df_out[i,"pet_st_slope_avg"] <-  mean(df$pet_st_slope, na.rm = T)
            
            df_out[i,"pet_st_conv_int_avg"] <-  convert_h_to_d(h = df_out[i,"pet_st_int_avg"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"pet_st_conv_slope_avg"] <-  convert_h_to_d(h = df_out[i,"pet_st_slope_avg"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
            df_out[i,"peese_int_avg"] <-  mean(df$peese_int, na.rm = T)
            df_out[i,"peese_slope_avg"] <-  mean(df$peese_slope, na.rm = T)
            
            df_out[i,"peese_st_int_avg"] <-  mean(df$peese_st_int, na.rm = T)
            df_out[i,"peese_st_slope_avg"] <-  mean(df$peese_st_slope, na.rm = T)
            df_out[i,"peese_st_conv_int_avg"] <-  convert_h_to_d(h = df_out[i,"peese_st_int_avg"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"peese_st_conv_slope_avg"] <-  convert_h_to_d(h = df_out[i,"peese_st_slope_avg"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
            
            df_out[i,"pet_int_max"] <-  max(df$pet_int, na.rm = T)
            df_out[i,"pet_slope_max"] <-  max(df$pet_slope, na.rm = T)
            
            df_out[i,"pet_st_int_max"] <-  max(df$pet_st_int, na.rm = T)
            df_out[i,"pet_st_slope_max"] <-  max(df$pet_st_slope, na.rm = T)
            
            df_out[i,"pet_st_conv_int_max"] <-  convert_h_to_d(h = df_out[i,"pet_st_int_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"pet_st_conv_slope_max"] <-  convert_h_to_d(h = df_out[i,"pet_st_slope_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
            df_out[i,"peese_int_max"] <-  max(df$peese_int, na.rm = T)
            df_out[i,"peese_slope_max"] <-  max(df$peese_slope, na.rm = T)
            
            df_out[i,"peese_st_int_max"] <-  max(df$peese_st_int, na.rm = T)
            df_out[i,"peese_st_slope_max"] <-  max(df$peese_st_slope, na.rm = T)
            df_out[i,"peese_st_conv_int_max"] <-  convert_h_to_d(h = df_out[i,"peese_st_int_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"peese_st_conv_slope_max"] <-  convert_h_to_d(h = df_out[i,"peese_st_slope_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
            
            # ABSOLUTE VALUE 
            
            df_out[i,"abs_pet_int_avg"] <-  mean(abs(df$pet_int), na.rm = T)
            df_out[i,"abs_pet_slope_avg"] <-  mean(abs(df$pet_slope), na.rm = T)
            
            df_out[i,"abs_pet_st_int_avg"] <-  mean(abs(df$pet_st_int), na.rm = T)
            df_out[i,"abs_pet_st_slope_avg"] <-  mean(abs(df$pet_st_slope), na.rm = T)
            
            df_out[i,"abs_pet_st_conv_int_avg"] <-  convert_h_to_d(h = df_out[i,"abs_pet_st_int_avg"] , mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"abs_pet_st_conv_slope_avg"] <-  convert_h_to_d(h = df_out[i,"abs_pet_st_slope_avg"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
            df_out[i,"abs_peese_int_avg"] <-  mean(abs(df$peese_int), na.rm = T)
            df_out[i,"abs_peese_slope_avg"] <-  mean(abs(df$peese_slope), na.rm = T)
            
            df_out[i,"abs_peese_st_int_avg"] <-  mean(abs(df$peese_st_int), na.rm = T)
            df_out[i,"abs_peese_st_slope_avg"] <-  mean(abs(df$peese_st_slope), na.rm = T)
            df_out[i,"abs_peese_st_conv_int_avg"] <-  convert_h_to_d(h = df_out[i,"abs_peese_st_int_avg"] , mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"abs_peese_st_conv_slope_avg"] <-  convert_h_to_d(h = df_out[i,"abs_peese_st_slope_avg"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
            
            df_out[i,"abs_pet_int_max"] <-  max(abs(df$pet_int), na.rm = T)
            df_out[i,"abs_pet_slope_max"] <-  max(abs(df$pet_slope), na.rm = T)
            
            df_out[i,"abs_pet_st_int_max"] <-  max(abs(df$pet_st_int), na.rm = T)
            df_out[i,"abs_pet_st_slope_max"] <-  max(abs(df$pet_st_slope), na.rm = T)
            
            df_out[i,"abs_pet_st_conv_int_max"] <-  convert_h_to_d(h = df_out[i,"abs_pet_st_int_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"abs_pet_st_conv_slope_max"] <-  convert_h_to_d(h = df_out[i,"abs_pet_st_slope_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
            df_out[i,"abs_peese_int_max"] <-  max(abs(df$peese_int), na.rm = T)
            df_out[i,"abs_peese_slope_max"] <-  max(abs(df$peese_slope), na.rm = T)
            
            df_out[i,"abs_peese_st_int_max"] <-  max(abs(df$peese_st_int), na.rm = T)
            df_out[i,"abs_peese_st_slope_max"] <-  max(abs(df$peese_st_slope), na.rm = T)
            df_out[i,"abs_peese_st_conv_int_max"] <-  convert_h_to_d(h = df_out[i,"abs_peese_st_int_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            df_out[i,"abs_peese_st_conv_slope_max"] <-  convert_h_to_d(h = df_out[i,"abs_peese_st_slope_max"], mean(df$avg_n_e, na.rm = T), mean(df$avg_n_c, na.rm = T))
            
          }
        }
      }
    }
  }
}

write.csv(df_out, "estimates_check.csv", row.names = F)
end.time <- Sys.time()
(time.taken <- end.time - start.time)
