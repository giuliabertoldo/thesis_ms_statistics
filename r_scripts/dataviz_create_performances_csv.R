source("r_scripts/dataviz_functions_app.R")

## Create one csv with performances ----------------
# List all conditions
k = c(15, 30, 70)
d = c(0, 0.2, 0.5, 0.8)
p = c("small", "medium", "large")
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
bt = c("pb_no_orb_no", "pb_no_orb_str", "pb_no_orb_mod", "pb_str_orb_no", "pb_mod_orb_no", "pb_str_orb_str", "pb_mod_orb_mod", "pb_str_orb_mod", "pb_mod_orb_str")

# Create df
df <- df_viz(num_studies = k, delta_00 = d, psss = p, sigma2_u = su, sigma2_v = sv, bias_type = bt)
write.csv(df, "/vsc-hard-mounts/leuven-data/354/vsc35419/thesis_ms_statistics/performances_data.csv", row.names = FALSE)

## Create one csv with performances puste----------------
# List all conditions
k = c(15, 30, 70)
d = c(0, 0.2, 0.5, 0.8)
p = c("small", "medium", "large")
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
bt = c("orb_0", "orb_2", "orb_4", "orb_6", "orb_8")
# Create df
df_puste <- df_viz_puste(num_studies = k, delta_00 = d, psss = p, sigma2_u = su, sigma2_v = sv, bias_type = bt)
write.csv(df, "/vsc-hard-mounts/leuven-data/354/vsc35419/thesis_ms_statistics/performances_puste.csv", row.names = FALSE)

## Update performances puste with column probability of censoring----------------
df <- read.csv("performances_puste.csv")
# Add column to specify probability of censoring a non-significant result
for(i in 1:dim(df)[1]){
  if(df$bt[i] == "orb_0"){
    df[i,'prob_censoring'] <- 1
  } else if (df$bt[i] == "orb_2"){
    df[i,'prob_censoring'] <- 0.8
  } else if (df$bt[i] == "orb_4"){
    df[i,'prob_censoring'] <- 0.6
  } else if (df$bt[i] == "orb_6"){
    df[i,'prob_censoring'] <- 0.4
  } else if (df$bt[i] == "orb_8"){
    df[i,'prob_censoring'] <- 0.2
  }
}

write.csv(df, "performances_puste.csv", row.names = FALSE)





# Merge performances with counts -------------
df <- read.csv("performances_all_no_counts.csv")
counts_pb_no_orb_str <- read.csv("slurm_counts/counts_pb_no_orb_str.csv")
counts_pb_no_orb_mod <- read.csv("slurm_counts/counts_pb_no_orb_mod.csv")
counts_pb_str_orb_no <- read.csv("slurm_counts/counts_pb_str_orb_no.csv")
counts_pb_mod_orb_no <- read.csv("slurm_counts/counts_pb_mod_orb_no.csv")
counts_pb_str_orb_str <- read.csv("slurm_counts/counts_pb_str_orb_str.csv")
counts_pb_mod_orb_mod <- read.csv("slurm_counts/counts_pb_mod_orb_mod.csv")
counts_pb_str_orb_mod <- read.csv("slurm_counts/counts_pb_str_orb_mod.csv")
counts_pb_mod_orb_str <- read.csv("slurm_counts/counts_pb_mod_orb_str.csv")

counts <- rbind(counts_pb_no_orb_str, counts_pb_no_orb_mod,
                counts_pb_str_orb_no, counts_pb_mod_orb_no,
                counts_pb_str_orb_str, counts_pb_mod_orb_mod,
                counts_pb_str_orb_mod, counts_pb_mod_orb_str)

# Complete biased
df_biased_complete <- merge(x = df, y = counts, by = "id")

# Complete unbiased
df1 <- df %>%
  filter(bt == "pb_no_orb_no")

# Add info to df1
for (i in 1:dim(df1)[1]){
  if (df1$bt[i] == "pb_no_orb_no"){
    df1$avg_perc_out_selected[i] <- 100

    if(df1$k[i] == 15){
      df1$avg_num_out_selected[i] <- 15*3
    } else if (df1$k[i]  ==  30){
      df1$avg_num_out_selected[i] <- 30*3
    } else if (df1$k[i] ==  70){
      df1$avg_num_out_selected[i] <- 70*3
    }
    df1$avg_perc_stud_selected[i] <- 100

    if(df1$k[i]  ==  15){
      df1$avg_num_study_selected[i] <- 15
    } else if (df1$k[i] == 30){
      df1$avg_num_study_selected[i] <- 30
    } else if (df1$k[i] == 70){
      df1$avg_num_study_selected[i] <- 70
    }

    df1$avg_avg_n_out_per_study[i] <- 3

    df1$avg_min_n_out_per_study[i] <- 3

    df1$avg_max_n_out_per_study[i] <- 3
  }
}

# Give meaningful name
df_unbiased_complete <- df1

# Row bind
performances <- rbind(df_unbiased_complete, df_biased_complete)

write.csv(performances, "performances.csv", row.names = FALSE)
