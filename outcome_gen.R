# Generate o outcomes
outcome_gen <- function (delta_00, sigma2_v, sigma2_u, o, psss) {

  # Sample correct sample sized based on condition for psss (primary study sample size)
  if (psss == "small") {
    ss = round(6 + rlnorm(n = 1, meanlog = 3.5, sd = 0.5))
  } else if (psss == "medium"){
    ss = round(6 + rlnorm(n = 1, meanlog = 4.5, sd = 0.5))
  } else if (psss == "large") {
    ss = round(6 + rlnorm(n = 1, meanlog = 5, sd = 0.5))
  }

  # Generate sample sizes per group
  n_e = floor(ss/2)
  n_c = ss - n_e

  # Sample study residual u_ok
  u_ok <- rnorm(n = 1, mean = 0, sd = sqrt(sigma2_u))

  # Study population mean
  d_k <- delta_00 + u_ok

  # Initialize empty list
  outcomes <- vector(mode = "list", length = o)

  for(i in 1:o) {
    # Sample outcome residual v_jk
    v_jk <- rnorm(n = 1, mean = 0, sd = sqrt(sigma2_v))

    # Outcome population mean
    d_jk <- d_k + v_jk

    # Sample n_e observed values
    obs_exp <- rnorm(n = n_e, mean = d_jk, sd = 1)

    # Sample n_c observed values
    obs_cont <- rnorm(n = n_c, mean = 0, sd = 1)

    # Pooled standard deviation is always equal to 1 so it is not included in calculations
    # Calculate uncorrected observed SMD
    d_hat_jk_un <- mean(obs_exp) - mean(obs_cont)

    # Apply Hedge's correction (Hedges 1982)
    correction_factor = 1 - (3/(4*ss-9))
    d_hat_jk <- d_hat_jk_un * correction_factor

    # Estimated sampling variance
    sigma2_r_est <- ((n_e+n_c)/(n_e*n_c))+((d_hat_jk)^2/(2*(n_e+n_c)))

    # Calculate t-value and two-sided p-value
    t <- (d_hat_jk)/(sqrt(1/n_e + 1/n_c))

    # Calculate p-value
    ## Degrees of freedom
    df = n_e + n_c -2
    ## pvalue (lower.tail = FALSE gives area on the right of t-statistics)
    pval_t <- 2 * (pt(q = abs(t), df = df, lower.tail = FALSE))

    # Variance-stabilizing transformation of the smd: Pustejovsky_2018, note pg. 59
    f_jk <- (n_e + n_c - 2) # Pustejovsky_2018, note pg. 59
    w_jk <- (n_e + n_c)/(n_e * n_c) # Pustejovsky_2018, note pg. 59
    a_jk <- sqrt(2*w_jk*f_jk) # Pustejovsky_2018, pg. 60

    # Variance-stabilizing transformation of the smd: hedges_olkin_1985, pg. 88
    h_jk <- sqrt(2)*asinh(d_hat_jk/a_jk)

    # Approximate sampling variance of h_jk: Pustejovsky_2018, pg. 60
    sigma2_h_jk <- (1/f_jk)

    # Collect into a vector
    temp_vec <- c(ss, n_e, n_c, delta_00, sigma2_u, u_ok, d_k, sigma2_v, v_jk, d_jk,  d_hat_jk_un, d_hat_jk, t, pval_t, sigma2_r_est, h_jk, sigma2_h_jk)

    # Naming of vector's elements
    vec_names <- c("ss", "n_e", "n_c", "delta_00", "sigma2_u", "u_ok", "d_k", "sigma2_v", "v_jk", "d_jk", "d_hat_jk_un", "d_hat_jk", "t", "pval_t", "sigma2_r_est", "h_jk", "sigma2_h_jk")
    vec <- setNames(temp_vec, vec_names)

     # Store vector in a list
    outcomes[[i]]<- vec
  }

  return(outcomes)
}

# Test function with some values
# (out1 <- outcome_gen(delta_00 = 0.8, sigma2_v = 0.11, sigma2_u = 0.11, o = 3, psss = "large"))



