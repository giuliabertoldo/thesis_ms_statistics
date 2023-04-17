debugSource("r_scripts/dataviz_functions.R")

# List all conditions
k = c(15, 30, 70)
d = c(0, 0.2, 0.5, 0.8)
p = c("small", "medium", "large")
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
bt = c("pb_no_orb_no", "pb_no_orb_str", "pb_no_orb_mod", "pb_str_orb_no", "pb_mod_orb_no", "pb_str_orb_str", "pb_mod_orb_mod", "pb_str_orb_mod", "pb_mod_orb_str")

# Create one dataframe with all performance measures across all conditions
df <- df_viz(num_studies = k, delta_00 = d, psss = p, sigma2_u = su, sigma2_v = sv, bias_type = bt)
write.csv("/vsc-hard-mounts/leuven-data/354/vsc35419/thesis_ms_statistics/performances_data.csv", row.names = FALSE)

# ------------------------------- pb_no_orb_no -----------------------------------
## Rejection rate: across k, small su=sv  ----------------------------------------
# Parameters
num_studies = c(15, 30, 70)
bias_type = "pb_no_orb_no"
d = c(0.2)
su = 0.01
sv = 0.01
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_no_small_su_sv <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_rr_pb_no_orb_no_small_su_sv

## Rejection rate: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
bias_type = "pb_no_orb_no"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_no_small_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_no_small_k

# Rejection rate: across su_sv, medium k --------------------------------------
# Parameters
num_studies = 30
bias_type = "pb_no_orb_no"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_no_medium_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_no_medium_k

# Rejection rate: across su_sv, large k ----------------------------------------
# Parameters
num_studies = 70
bias_type = "pb_no_orb_no"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_no_large_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_no_large_k


# ------------------------------- pb_no_orb_str ---------------------------------------------
## Rejection rate: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
bias_type = "pb_no_orb_str"
d = c(0.2)
su = 0.01
sv = 0.01
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_str_small_su_sv <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_rr_pb_no_orb_str_small_su_sv

## Rejection rate: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
bias_type = "pb_no_orb_str"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_str_small_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_str_small_k

# Rejection rate: across su_sv, medium k --------------------------------------
# Parameters
num_studies = 30
bias_type = "pb_no_orb_str"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_str_medium_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_str_medium_k

# Rejection rate: across su_sv, large k ----------------------------------------
# Parameters
num_studies = 70
bias_type = "pb_no_orb_str"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_rr_pb_no_orb_str_large_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_str_large_k

## Bias: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
bias_type = "pb_no_orb_str"
d = c(0.2)
su = 0.01
sv = 0.01
ss = c("small", "medium", "large")
# Visualization
viz_bias_pb_no_orb_str_small_su_sv <- viz_bias(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_bias_pb_no_orb_str_small_su_sv

## Bias: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
bias_type = "pb_no_orb_str"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_bias_pb_no_orb_str_small_k <- viz_bias(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_bias_pb_no_orb_str_small_k


## MSE: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
bias_type = "pb_no_orb_str"
d = c(0.2)
su = 0.01
sv = 0.01
ss = c("small", "medium", "large")
# Visualization
viz_mse_pb_no_orb_str_small_su_sv <- viz_mse(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_mse_pb_no_orb_str_small_su_sv

## MSE: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
bias_type = "pb_no_orb_str"
d = c(0.2)
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
ss = c("small", "medium", "large")
# Visualization
viz_mse_pb_no_orb_str_small_k <- viz_mse(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_mse_pb_no_orb_str_small_k
