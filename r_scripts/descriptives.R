source("r_scripts/dataviz_functions.R")
# Load data

df_perf_count_bias = read.csv("performances_counts_biased.csv")



k = 15
bt = "pb_mod_orb_no"
d = c(0, 0.2, 0.5, 0.8)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
across = "su_sv"

viz_per_out_selected(df = df_perf_count_bias, num_studies = k, bias_type = bt, d = d, su = su, sv = sv, ss, across)


k = 70
bt = "pb_str_orb_str"
d = c(0, 0.2, 0.5, 0.8)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
across = "su_sv"

viz_per_out_selected(df = complete_perf, num_studies = k, bias_type = bt, d = d, su = su, sv = sv, ss, across)

k = c(15, 30, 70)
bt = "pb_str_orb_str"
d = c(0, 0.2, 0.5, 0.8)
su = 0.06
sv = 0.06
ss = c("small", "medium", "large")
across = "k"

viz_per_out_selected(df = complete_perf, num_studies = k, bias_type = bt, d = d, su = su, sv = sv, ss, across)




# Then: Learn how to add info
