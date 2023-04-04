source("r_scripts/prob_inclusion.R")
# debugSource("r_scripts/prob_inclusion.R")

pb <- function(df, k, bias_type){
  # Count how many unique studies there are in the input dataframe
  num_uniq_studies <- length(unique(df$study_id))

  # Create new dataframe
  col_names <- c("nmeta_id", "out_id", "study_id", "ss", "n_e", "n_c", "delta_00", "sigma2_u", "u_ok", "d_k", "sigma2_v", "d_hat_jk", "t", "pval_t","sigma2_r_est", "h_jk", "sigma2_h_jk" )
  df_grouped <- as.data.frame(matrix(nrow = num_uniq_studies, ncol = length(col_names)))
  colnames(df_grouped) <- col_names

  # Add
  i <- 1
  for (j in unique(df$study_id)){

    # Group by study
    temp <- df[df['study_id']==j, ]

    n_e = temp$n_e[1]
    n_c = temp$n_c[1]

    # Mean SMD
    d_hat_jk <- mean(temp$d_hat_jk)

    # Estimated sampling variance
    sigma2_r_est <- ((n_e+n_c)/(n_e*n_c))+((d_hat_jk)^2/(2*(n_e+n_c)))

    # Calculate t-value
    t <- (d_hat_jk)/(sqrt(1/n_e + 1/n_c))

    # Calculate p-value
    ## Degrees of freedom
    degrees_free = n_e + n_c -2
    ## pvalue (lower.tail = FALSE gives area on the right of t-statistics)
    pval_t <- 2 * (pt(q = abs(t), df = degrees_free, lower.tail = FALSE))

    # Variance-stabilizing transformation of the smd: Pustejovsky_2018, note pg. 59
    f_jk <- (n_e + n_c - 2) # Pustejovsky_2018, note pg. 59
    w_jk <- (n_e + n_c)/(n_e * n_c) # Pustejovsky_2018, note pg. 59
    a_jk <- sqrt(2*w_jk*f_jk) # Pustejovsky_2018, pg. 60

    # Variance-stabilizing transformation of the smd: hedges_olkin_1985, pg. 88
    h_jk <- sqrt(2)*asinh(d_hat_jk/a_jk)

    # Approximate sampling variance of h_jk: Pustejovsky_2018, pg. 60
    sigma2_h_jk <- (1/f_jk)

    # Populate output dataframe
    df_grouped[i,'nmeta_id'] <- temp[1,'nmeta_id']
    df_grouped[i, 'study_id'] <- temp[1, 'study_id']
    df_grouped[i, 'out_id'] <- temp[1, 'out_id']
    df_grouped[i, 'ss'] <- temp[1, 'ss']
    df_grouped[i, 'n_e'] <- temp[1, 'n_e']
    df_grouped[i, 'n_c'] <- temp[1, 'n_c']
    df_grouped[i, 'delta_00'] <- temp[1, 'delta_00']
    df_grouped[i, 'sigma2_u'] <- temp[1, 'sigma2_u']
    df_grouped[i, 'u_ok'] <- temp[1, 'u_ok']
    df_grouped[i, 'd_k'] <- temp[1, 'd_k']
    df_grouped[i, 'sigma2_v'] <- temp[1, 'sigma2_v']
    df_grouped[i, 'd_hat_jk'] <- d_hat_jk
    df_grouped[i, 't'] <- t
    df_grouped[i, 'pval_t'] <- pval_t
    df_grouped[i, 'sigma2_r_est'] <- sigma2_r_est
    df_grouped[i, 'h_jk'] <- h_jk
    df_grouped[i, 'sigma2_h_jk'] <- sigma2_h_jk

    # Increase counter move to next row until k-th row
    i <- i + 1

  }

  ## Create new column which indicates if observation should be included or not
  df_grouped[, 'included'] <- NA

  ## Create new column with probability of inclusion
  df_grouped[, 'prob_incl'] <- NA

  ## Create new column with random number from U[0,1]
  df_grouped[, 'random_num'] <- NA

  # Apply selection

  for (i in seq(1:dim(df_grouped)[1])) {

    if (bias_type == "pb_mod_orb_no") {
      # Calculate probability of inclusion
      df_grouped$prob_incl[i] <- bias_moderate(df_grouped$pval_t[i])

    } else if (bias_type == "pb_str_orb_no") {
      df_grouped$prob_incl[i] <- bias_strong(df_grouped$pval_t[i])

    } else if (bias_type == "pb_mod_orb_mod") {
      df_grouped$prob_incl[i] <- bias_moderate(df_grouped$pval_t[i])

    } else if (bias_type == "pb_str_orb_mod") {
      df_grouped$prob_incl[i] <- bias_strong(df_grouped$pval_t[i])

    } else if (bias_type == "pb_mod_orb_str") {
      df_grouped$prob_incl[i] <- bias_moderate(df_grouped$pval_t[i])

    } else if (bias_type == "pb_str_orb_str") {
      df_grouped$prob_incl[i] <- bias_strong(df_grouped$pval_t[i])
    }

    df_grouped$random_num[i] <- runif(n = 1, min = 0, max = 1)

    if (df_grouped$prob_incl[i] > df_grouped$random_num[i]) {
      df_grouped$included[i] <- 1
    } else {
      df_grouped$included[i] <- 0
    }
  }

  ## Create final biased dataset with only included studies
  df_biased <- df_grouped[df_grouped[,'included']==1,]

  ## Initialize empty list to store the two dataframes
  out <- list()

  out$df_grouped <- df_grouped
  out$df_biased <- df_biased

  return(out)
}


