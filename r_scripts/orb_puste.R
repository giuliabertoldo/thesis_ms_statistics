orb_puste <- function(df, bias_type){
  ## Create new column which indicates if observation should be included or not
  df[, 'included'] <- NA

  ## Select degree of bias
  # No publication bias, Moderate ORB
  if (bias_type == "orb_8") {
    pi = 0.8

  } else if (bias_type == "orb_6") {
    pi = 0.6

  } else if (bias_type == "orb_4"){
    pi = 0.4

  } else if (bias_type == "orb_2") {
    pi = 0.2

  } else if (bias_type == "orb_0"){
    pi = 0

  }

  for (i in seq(1:dim(df)[1])) {
    if (df$pval_t[i] < 0.05){
      df$included[i] <- 1
    } else {
      df$included[i] <- rbinom(n = 1, size = 1, p = pi)
    }
  }

  ## Create final biased dataset with only included studies
  df_biased <- df[df[,'included']==1,]
  ## Initialize empty list to store the two dataframes

  return(df_biased)

  }

# Check
# df <- subset(df_indicator_bias, select = -c(included, prob_incl, random_num))
# bias_type <- "orb_0"


