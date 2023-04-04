source("r_scripts/prob_inclusion.R")
# debugSource("r_scripts/prob_inclusion.R")

source("r_scripts/orb.R")
# debugSource("r_scripts/orb.R")

source("r_scripts/pb.R")
# debugSource("r_scripts/pb.R")

pb_orb <- function(df, k, bias_type){

  # Outcome Reporting Bias
  df_orb <- orb(df = df, bias_type = bias_type)
  df_orb <- df_orb$df_biased

  # Publication Bias
  out <- pb(df = df_orb, k = k, bias_type = bias_type)

  return(out)
}


