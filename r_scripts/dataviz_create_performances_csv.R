source("r_scripts/dataviz_functions.R")

# List all conditions
k = c(15, 30, 70)
d = c(0, 0.2, 0.5, 0.8)
p = c("small", "medium", "large")
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
bt = c("pb_no_orb_no", "pb_no_orb_str", "pb_no_orb_mod", "pb_str_orb_no", "pb_mod_orb_no", "pb_str_orb_str", "pb_mod_orb_mod", "pb_str_orb_mod", "pb_mod_orb_str")

# Create one dataframe with all performance measures across all conditions
df <- df_viz(num_studies = k, delta_00 = d, psss = p, sigma2_u = su, sigma2_v = sv, bias_type = bt)
write.csv(df, "/vsc-hard-mounts/leuven-data/354/vsc35419/thesis_ms_statistics/performances_data.csv", row.names = FALSE)

