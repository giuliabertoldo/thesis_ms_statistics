args <- commandArgs(TRUE)
idx <- args[1]

# Read csv with conditions
conditions <- read.csv(file = 'conditions/condition_pb_no_orb_str.csv')

# Each processor gets a chunck of conditions and idx is one row of this chunck
local_condition = conditions[idx,]


# Decompose the row to retrieve the condition's values
bias_type <- local_condition[[1]]
delta_00 <- local_condition[[2]]
num_stud <- local_condition[[3]]
sigma2_u <- local_condition[[4]]
sigma2_v <- local_condition[[5]]
psss <- local_condition[[6]]
o <- local_condition[[7]]
nmeta = 1000


# Code to execute
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
              if(bt == "pb_no_orb_str" | bt == "pb_no_orb_mod") {
                out_df[n, 'tot_num_out_original'] <- k*o
              } else {
                out_df[n, 'tot_num_out_original'] <- k
              }

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

write.csv(df_merged, "counts.csv", row.names = FALSE)
