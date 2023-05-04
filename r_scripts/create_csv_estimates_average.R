
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

            df_out[i,"pet_int_avg"] <-  avg(df$pet_int)
            df_out[i,"pet_slope_avg"] <-  avg(df$pet_slope)

            df_out[i,"pet_st_int_avg"] <-  avg(df$pet_st_int)
            df_out[i,"pet_st_slope_avg"] <-  avg(df$pet_st_slope)

          }
        }
      }
    }
  }
}

write.csv(df_out, "convergence.csv", row.names = F)
end.time <- Sys.time()
(time.taken <- end.time - start.time)
