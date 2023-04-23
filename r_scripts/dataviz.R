source("r_scripts/dataviz_functions.R")

# Load dataframe
df <- read.csv("performances_all.csv")

# Fixed parameters
d = c(0, 0.2, 0.5, 0.8)
ss = c("small", "medium", "large")

# ------------------------------- pb_no_orb_no -----------------------------------
bias_type = "pb_no_orb_no"

## Rejection rate: across k, small su=sv  ----------------------------------------
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_rr_pb_no_orb_no_small_su_sv <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_rr_pb_no_orb_no_small_su_sv
# Type I error rate increase with increasing delta_00, decreaasing psss,increasing k, for SMD and not for Transformed SMD

## Rejection rate: across su_sv, small k -------------------------------------
num_studies = 15
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_rr_pb_no_orb_no_small_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_no_small_k
# Type I error rate increases with increasing delta_00, decreasing psss, increasing su & sv, for SMD and not for Transformed SMD

# Rejection rate: across su_sv, medium k --------------------------------------
# Parameters
num_studies = 30
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_rr_pb_no_orb_no_medium_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_no_medium_k
# Same as viz_rr_pb_no_orb_no_small_k BUT note that the Type I error increases even more with increasing k

# Rejection rate: across su_sv, large k ----------------------------------------
# Parameters
num_studies = 70
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_rr_pb_no_orb_no_large_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_no_large_k


# ------------------------------- pb_no_orb_str ---------------------------------------------
bias_type = "pb_no_orb_str"
## Rejection rate: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_rr_pb_no_orb_str_small_su_sv <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_rr_pb_no_orb_str_small_su_sv

# Power
## Rejection rate: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_rr_pb_no_orb_str_small_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_str_small_k

# Rejection rate: across su_sv, medium k --------------------------------------
# Parameters
num_studies = 30
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_rr_pb_no_orb_str_medium_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_str_medium_k

# Rejection rate: across su_sv, large k ----------------------------------------
# Parameters
num_studies = 70
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)

# Visualization
viz_rr_pb_no_orb_str_large_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_no_orb_str_large_k

## Bias: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_bias_pb_no_orb_str_small_su_sv <- viz_bias(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_bias_pb_no_orb_str_small_su_sv

## Bias: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_bias_pb_no_orb_str_small_k <- viz_bias(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_bias_pb_no_orb_str_small_k


## MSE: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_mse_pb_no_orb_str_small_su_sv <- viz_mse(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_mse_pb_no_orb_str_small_su_sv

## MSE: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_mse_pb_no_orb_str_small_k <- viz_mse(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_mse_pb_no_orb_str_small_k

## Power pet int: across k, small su=sv ---------------
# Parameters
d = c(0.2, 0.5, 0.8)
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_ppeti_pb_no_orb_str_small_su_sv <- viz_pwr_pet_int(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_ppeti_pb_no_orb_str_small_su_sv

## Power pet int: across su_sv, small k -------------------------------------
# Parameters
d = c(0.2, 0.5, 0.8)
num_studies = 15
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_ppeti_pb_no_orb_str_small_k <- viz_pwr_pet_int(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_ppeti_pb_no_orb_str_small_k

## Type1 pet int: across k, small su=sv ---------------
# Parameters
d = c(0)
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_ppeti_pb_no_orb_str_small_su_sv <- viz_pwr_pet_int(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_ppeti_pb_no_orb_str_small_su_sv


# ------------------------------- pb_str_orb_no ---------------------------------------------
bias_type = "pb_str_orb_no"
## Rejection rate: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_rr_pb_str_orb_no_small_su_sv <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_rr_pb_str_orb_no_small_su_sv

# Power
## Rejection rate: across su_sv, small k -------------------------------------
# Parameters
num_studies = 15
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_rr_pb_str_orb_no_small_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_str_orb_no_small_k

# Rejection rate: across su_sv, medium k --------------------------------------
# Parameters
num_studies = 30
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)
# Visualization
viz_rr_pb_str_orb_no_medium_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_str_orb_no_medium_k

# Rejection rate: across su_sv, large k ----------------------------------------
# Parameters
num_studies = 70
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)

# Visualization
viz_rr_pb_str_orb_no_large_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_str_orb_no_large_k


# ------------------------------- pb_mod_orb_no ---------------------------------------------
bias_type = "pb_mod_orb_no"
## Rejection rate: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_rr_pb_mod_orb_no_small_su_sv <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_rr_pb_mod_orb_no_small_su_sv

# ------------------------------- pb_str_orb_str ---------------------------------------------
bias_type = "pb_str_orb_str"

## Rejection rate: across k, small su=sv ---------------
# Parameters
num_studies = c(15, 30, 70)
su = 0.01
sv = 0.01
# Visualization
viz_rr_pb_str_orb_str_small_su_sv <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "k")
viz_rr_pb_str_orb_str_small_su_sv

# Rejection rate: across su_sv, large k ----------------------------------------
# Parameters
num_studies = 70
su = c(0.01, 0.06, 0.11)
sv = c(0.01, 0.06, 0.11)

# Visualization
viz_rr_pb_str_orb_str_large_k <- viz_rejection_rate(df = df, num_studies = num_studies, bias_type = bias_type, d = d, su = su, sv = sv, ss = ss, across = "su_sv")
viz_rr_pb_str_orb_str_large_k


