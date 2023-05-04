library(tidyverse)

# Create csv with only conditions where non-convergence was less than 1%
# in all 4 pets

df_conv <- read.csv("convergence.csv")

df_perf <- read.csv("performances.csv")

####### EXCLUDE STUDIES WITH MORE THAN 1% NON-CONVERGENCE (10 STUDIES OVER 1000) ------------

df1 <- df_conv %>%
  filter(pet_smd_perc_non_conv >= 1 |
           pet_tr_smd_perc_non_conv >= 1 |
           peese_smd_perc_non_conv >= 1 |
           peese_tr_smd_perc_non_conv >= 1)



# Create id
df1[1, "id"] <- NA
for (i in 1:dim(df1)[1]){
  bt <- df1[i, "bt"]
  k <- df1[i, "k"]
  d <- df1[i, "d"]
  su <- df1[i, "su"]
  sv <- df1[i, "sv"]
  p <- df1[i, "p"]
  df1[i, "id"] <- sprintf("%s_k_%g_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)
}

id_remove <- df1$id

# Create performances_sub
# keep all ids different from id in df1

df2 <- df_perf %>%
  filter(!id %in% id_remove)

write.csv(df2, "performances_sub.csv", row.names = F)



####### EXCLUDE STUDIES WITH MORE THAN 10% NON-CONVERGENCE (100 STUDIES OVER 1000) ------------

df1 <- df_conv %>%
  filter(pet_smd_perc_non_conv >= 10 |
           pet_tr_smd_perc_non_conv >= 10 |
           peese_smd_perc_non_conv >= 10 |
           peese_tr_smd_perc_non_conv >= 10)



# Create id
df1[1, "id"] <- NA
for (i in 1:dim(df1)[1]){
  bt <- df1[i, "bt"]
  k <- df1[i, "k"]
  d <- df1[i, "d"]
  su <- df1[i, "su"]
  sv <- df1[i, "sv"]
  p <- df1[i, "p"]
  df1[i, "id"] <- sprintf("%s_k_%g_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)
}

id_remove <- df1$id

# Create performances_sub
# keep all ids different from id in df1

df2 <- df_perf %>%
  filter(!id %in% id_remove)

write.csv(df2, "performances_sub_10.csv", row.names = F)

####### EXCLUDE STUDIES WITH MORE THAN 5% NON-CONVERGENCE (50 STUDIES OVER 1000) ------------

df1 <- df_conv %>%
  filter(pet_smd_perc_non_conv >= 5 |
           pet_tr_smd_perc_non_conv >= 5 |
           peese_smd_perc_non_conv >= 5 |
           peese_tr_smd_perc_non_conv >= 5)



# Create id
df1[1, "id"] <- NA
for (i in 1:dim(df1)[1]){
  bt <- df1[i, "bt"]
  k <- df1[i, "k"]
  d <- df1[i, "d"]
  su <- df1[i, "su"]
  sv <- df1[i, "sv"]
  p <- df1[i, "p"]
  df1[i, "id"] <- sprintf("%s_k_%g_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)
}

id_remove <- df1$id

# Create performances_sub
# keep all ids different from id in df1

df2 <- df_perf %>%
  filter(!id %in% id_remove)

write.csv(df2, "performances_sub_5.csv", row.names = F)

####### EXCLUDE STUDIES WITH MORE THAN 3% NON-CONVERGENCE (30 STUDIES OVER 1000) ------------

df1 <- df_conv %>%
  filter(pet_smd_perc_non_conv >= 3 |
           pet_tr_smd_perc_non_conv >= 3 |
           peese_smd_perc_non_conv >= 3 |
           peese_tr_smd_perc_non_conv >= 3)



# Create id
df1[1, "id"] <- NA
for (i in 1:dim(df1)[1]){
  bt <- df1[i, "bt"]
  k <- df1[i, "k"]
  d <- df1[i, "d"]
  su <- df1[i, "su"]
  sv <- df1[i, "sv"]
  p <- df1[i, "p"]
  df1[i, "id"] <- sprintf("%s_k_%g_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)
}

id_remove <- df1$id

# Create performances_sub
# keep all ids different from id in df1

df2 <- df_perf %>%
  filter(!id %in% id_remove)

write.csv(df2, "performances_sub_3.csv", row.names = F)


####### EXCLUDE STUDIES WITH MORE THAN 2% NON-CONVERGENCE (20 STUDIES OVER 1000) ------------

df1 <- df_conv %>%
  filter(pet_smd_perc_non_conv >= 2 |
           pet_tr_smd_perc_non_conv >= 2 |
           peese_smd_perc_non_conv >= 2 |
           peese_tr_smd_perc_non_conv >=23)



# Create id
df1[1, "id"] <- NA
for (i in 1:dim(df1)[1]){
  bt <- df1[i, "bt"]
  k <- df1[i, "k"]
  d <- df1[i, "d"]
  su <- df1[i, "su"]
  sv <- df1[i, "sv"]
  p <- df1[i, "p"]
  df1[i, "id"] <- sprintf("%s_k_%g_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)
}

id_remove <- df1$id

# Create performances_sub
# keep all ids different from id in df1

df2 <- df_perf %>%
  filter(!id %in% id_remove)

write.csv(df2, "performances_sub_2.csv", row.names = F)

## REMOVE SELECTED CONDITIONS --------------------------------
df2 <- df_perf %>%
  filter(id != "pb_str_orb_no_k_15_d0.00_su0.01_sv0.01_small",
         id != "pb_str_orb_no_k_15_d0.00_su0.06_sv0.06_small",
         id != "pb_str_orb_no_k_15_d0.00_su0.01_sv0.01_medium",
         id != "pb_str_orb_no_k_15_d0.00_su0.06_sv0.06_medium",
         id != "pb_str_orb_no_k_15_d0.00_su0.01_sv0.01_large",
         id != "pb_str_orb_no_k_15_d0.20_su0.01_sv0.01_small",
         id != "pb_str_orb_no_k_15_d0.20_su0.06_sv0.06_small",
         id != "pb_str_orb_no_k_30_d0.00_su0.01_sv0.01_small",
         id != "pb_mod_orb_no_k_15_d0.00_su0.01_sv0.01_small")


write.csv(df2, "performances_sub_selected.csv", row.names = F)


