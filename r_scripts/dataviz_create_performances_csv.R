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





##  Merge performances with counts -------------
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



## Consider in "performances.csv" only the pure pb and orb ------------
df <- read.csv("performances_all.csv")

df1 <- df %>%
  filter(bt != "pb_mod_orb_str",
         bt != "pb_str_orb_mod",
         bt != "pb_str_orb_str",
         bt != "pb_mod_orb_mod")

write.csv(df1, "performances.csv", row.names = FALSE)


## Add to performances.csv columns on bias type, strength of bias --------
df1 <- read.csv("performances.csv")
max <- dim(df1)[1]

# Add colum on bias type
for (i in 1:max){
  if(grepl("orb_no",df1[i, 'bt'])){
    df1$pb_orb[i] <- "pb"
  } else if(grepl("pb_no",df1[i, 'bt'])){
    df1$pb_orb[i] <- "orb"
  }
}

# Add colum on strength of bias
df1[,'mod_str'] <- NA

for (j in 1:max){
  if(grepl("mod", df1[j, 'bt'])){
    df1$mod_str[j] <- "mod"
  } else if(grepl("str",df1[j, 'bt'])){
    df1$mod_str[j] <- "str"
  } else {
    df1$mod_str[j] <- "none"
  }
}
write.csv(df1, "performances.csv", row.names = FALSE)


## Add to performances.csv column with readable bias type ----------------
df1 <- read.csv("performances.csv")

df1[,'bt_read'] <- NA

for (i in 1:dim(df1)[1]){
  if(df1$bt[i] == "pb_no_orb_str" ){
    df1$bt_read[i] <- "ORB Strong"
  } else if (df1$bt[i] == "pb_no_orb_mod") {
    df1$bt_read[i] <- "ORB Moderate"
  } else if (df1$bt[i] == "pb_str_orb_no"){
    df1$bt_read[i] <- "PB Strong"
  } else if(df1$bt[i] == "pb_mod_orb_no"){
    df1$bt_read[i] <- "PB Moderate"
  } else if(df1$bt[i] == "pb_no_orb_no"){
    df1$bt_read[i] <- "None"
  }
}

write.csv(df1, "performances.csv", row.names = FALSE)

## Add to performances.csv column with percentage exlcuded ----------------
df1 <- read.csv("performances.csv")

df1$avg_perc_out_excluded <- 100 - df1$avg_perc_out_selected

check <- df1$avg_perc_out_excluded + df1$avg_perc_out_selected
sum(check != 100)

write.csv(df1, "performances.csv", row.names = FALSE)


