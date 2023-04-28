library(tidyverse)

# Create csv with only conditions where non-convergence was less than 1%
# in all 4 pets

df_conv <- read.csv("convergence.csv")

df_perf <- read.csv("performances.csv")


# Identify conditions to remove
df1 <- df_conv %>%
  filter(pet_smd_perc_non_conv >= 1 &
           pet_tr_smd_perc_non_conv >= 1 &
           peese_smd_perc_non_conv >= 1 &
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

