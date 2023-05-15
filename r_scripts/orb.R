source('r_scripts/prob_inclusion.R')
# debugSource('r_scripts/prob_inclusion.R')

orb <- function(df, bias_type){
  ## Create new column which indicates if observation should be included or not
  df[, 'included'] <- NA

  ## Create new column with probability of inclusion
  df[, 'prob_incl'] <- NA

  ## Create new column with random number from U[0,1]
  df[, 'random_num'] <- NA

  for (i in seq(1:dim(df)[1])) {

    # No publication bias, Moderate ORB
    if (bias_type == "pb_no_orb_mod") {
      # Calculate probability of inclusion
      df$prob_incl[i] <- bias_moderate(df$pval_t[i])

    } else if (bias_type == "pb_no_orb_str") {
      df$prob_incl[i] <- bias_strong(df$pval_t[i])

    } else if (bias_type == "pb_mod_orb_mod"){
      df$prob_incl[i] <- bias_moderate(df$pval_t[i])

    } else if (bias_type == "pb_str_orb_mod") {
      df$prob_incl[i] <- bias_moderate(df$pval_t[i])

    } else if (bias_type == "pb_mod_orb_str"){
      df$prob_incl[i] <- bias_strong(df$pval_t[i])

    } else if (bias_type == "pb_str_orb_str") {
      df$prob_incl[i] <- bias_strong(df$pval_t[i])
    }

    df$random_num[i] <- runif(n = 1, min = 0, max = 1)

    if (df$prob_incl[i] > df$random_num[i]) {
      df$included[i] <- 1
    } else {
      df$included[i] <- 0
    }
  }

  ## Create final biased dataset with only included studies
  df_biased <- df[df[,'included']==1,]

  ## Initialize empty list to store the two dataframes
  out <- list()

  out$df_indicator_bias <- df
  out$df_biased <- df_biased

  return(out)
}


