library(metafor)

## PET = Egger regression (outcome is d_hat_jk and predictor is sqrt(sigma2_r))
pet <- function(df_meta, verbose){ # tol, iter,

  pet_results_names <- c("error_pet", "pet_slope", "se_pet_slope", "p_pet_slope", "ci_lb_pet_slope", "ci_ub_pet_slope", "pet_int", "se_pet_int", "p_pet_int", "ci_lb_pet_int", "ci_ub_pet_int")
  pet_results <- vector(mode = "list", length = length(pet_results_names))
  names(pet_results) <- pet_results_names

  pet <- try(
    rma.mv(yi = d_hat_jk,
           V = sigma2_r_est,
           random = list(~ 1 | study_id / out_id),
           data = df_meta,
           mods = ~ 1 + sqrt(sigma2_r_est),
           test = "t",
           dfs = "contain",
           verbose = verbose) # , control=list(rel.tol=tol, iter.max=iter)
  )

  if (class(pet)[1]=="try-error") {
    pet_results$error_pet <- 1

    pet_results$pet_slope <- NA
    pet_results$se_pet_slope <- NA
    pet_results$p_pet_slope <- NA
    pet_results$ci_lb_pet_slope <- NA
    pet_results$ci_ub_pet_slope <- NA

    pet_results$pet_int <- NA
    pet_results$se_pet_int <- NA
    pet_results$p_pet_int <- NA
    pet_results$ci_lb_pet_int <- NA
    pet_results$ci_ub_pet_int <- NA

  } else {
    pet_results$error_pet <- 0

    pet_results$pet_slope <- pet$beta[2]
    pet_results$se_pet_slope <- pet$se[2]
    pet_results$p_pet_slope <- pet$pval[2]
    pet_results$ci_lb_pet_slope <- pet$ci.lb[2]
    pet_results$ci_ub_pet_slope <- pet$ci.ub[2]

    pet_results$pet_int <- pet$beta[1]
    pet_results$se_pet_int <- pet$se[1]
    pet_results$p_pet_int <- pet$pval[1]
    pet_results$ci_lb_pet_int <- pet$ci.lb[1]
    pet_results$ci_ub_pet_int <- pet$ci.ub[1]
  }

  return(pet_results)
}

# PEESE (outcome is d_hat_jk and predictor is sigma2_r)
peese <- function(df_meta, verbose){ # tol, iter,

  peese_results_names <- c("error_peese", "peese_slope", "se_peese_slope", "p_peese_slope", "ci_lb_peese_slope", "ci_ub_peese_slope", "peese_int", "se_peese_int", "p_peese_int", "ci_lb_peese_int", "ci_ub_peese_int")
  peese_results <- vector(mode = "list", length = length(peese_results_names))
  names(peese_results) <- peese_results_names

  peese <- try(
    rma.mv(yi = d_hat_jk,
           V = sigma2_r_est,
           random = list(~ 1 | study_id / out_id),
           data = df_meta,
           mods = ~ 1 + sigma2_r_est,
           test = "t",
           dfs = "contain",
           verbose = verbose) # , control=list(rel.tol=tol, iter.max=iter)
  )

  if (class(peese)[1] =="try-error") {
    peese_results$error_peese <- 1

    peese_results$peese_slope <- NA
    peese_results$se_peese_slope <- NA
    peese_results$p_peese_slope <- NA
    peese_results$ci_lb_peese_slope <- NA
    peese_results$ci_ub_peese_slope <- NA

    peese_results$peese_int <- NA
    peese_results$se_peese_int <- NA
    peese_results$p_peese_int <- NA
    peese_results$ci_lb_peese_int <- NA
    peese_results$ci_ub_peese_int <- NA

  } else {
    peese_results$error_peese <- 0

    peese_results$peese_slope <- peese$beta[2]
    peese_results$se_peese_slope <- peese$se[2]
    peese_results$p_peese_slope <- peese$pval[2]
    peese_results$ci_lb_peese_slope <- peese$ci.lb[2]
    peese_results$ci_ub_peese_slope <- peese$ci.ub[2]

    peese_results$peese_int <- peese$beta[1]
    peese_results$se_peese_int <- peese$se[1]
    peese_results$p_peese_int <- peese$pval[1]
    peese_results$ci_lb_peese_int <- peese$ci.lb[1]
    peese_results$ci_ub_peese_int <- peese$ci.ub[1]

  }
  return(peese_results)
}


# PET with variance stabilized transformation of the SMD
# (outcome is h_jk and predictor is sqrt(sigma2_h_jk))

pet_var_stab <- function(df_meta, verbose){ # tol, iter,

  pet_st_results_names <- c("error_pet_st", "pet_st_slope", "se_pet_st_slope", "p_pet_st_slope", "ci_lb_pet_st_slope", "ci_ub_pet_st_slope",  "pet_st_int", "se_pet_st_int", "p_pet_st_int", "ci_lb_pet_st_int", "ci_ub_pet_st_int")
  pet_st_results <- vector(mode = "list", length = length(pet_st_results_names))
  names(pet_st_results) <- pet_st_results_names

  pet_st <- try(
    rma.mv(yi = h_jk,
           V = sigma2_h_jk,
           random = list(~ 1 | study_id / out_id),
           data = df_meta,
           mods = ~ 1 + sqrt(sigma2_h_jk),
           test = "t",
           dfs = "contain",
           verbose = verbose) # , control=list(rel.tol=tol, iter.max=iter)
  )

  if (class(pet_st)[1] =="try-error") {
    pet_st_results$error_pet_st <- 1

    pet_st_results$pet_st_slope <- NA
    pet_st_results$se_pet_st_slope <- NA
    pet_st_results$p_pet_st_slope <- NA
    pet_st_results$ci_lb_pet_st_slope <- NA
    pet_st_results$ci_ub_pet_st_slope <- NA

    pet_st_results$pet_st_int <- NA
    pet_st_results$se_pet_st_int <- NA
    pet_st_results$p_pet_st_int <- NA
    pet_st_results$ci_lb_pet_st_int <- NA
    pet_st_results$ci_ub_pet_st_int <- NA

  } else {
    pet_st_results$error_pet_st <- 0

    pet_st_results$pet_st_slope <- pet_st$beta[2]
    pet_st_results$se_pet_st_slope <- pet_st$se[2]
    pet_st_results$p_pet_st_slope <- pet_st$pval[2]
    pet_st_results$ci_lb_pet_st_slope <- pet_st$ci.lb[2]
    pet_st_results$ci_ub_pet_st_slope <- pet_st$ci.ub[2]

    pet_st_results$pet_st_int <- pet_st$beta[1]
    pet_st_results$se_pet_st_int <- pet_st$se[1]
    pet_st_results$p_pet_st_int <- pet_st$pval[1]
    pet_st_results$ci_lb_pet_st_int <- pet_st$ci.lb[1]
    pet_st_results$ci_ub_pet_st_int <- pet_st$ci.ub[1]
  }

return(pet_st_results)
}

# PEESE with variance stabilizing variables
# (outcome is h_jk and predictor is sigma2_h_jk)
peese_var_stab <- function(df_meta, verbose){ # tol, iter,

  peese_st_results_names <- c("error_peese_st", "peese_st_slope", "se_peese_st_slope", "p_peese_st_slope", "ci_lb_peese_st_slope","ci_ub_peese_st_slope","peese_st_int", "se_peese_st_int", "p_peese_st_int", "ci_lb_peese_st_int","ci_ub_peese_st_int")
  peese_st_results <- vector(mode = "list", length = length(peese_st_results_names))
  names(peese_st_results) <- peese_st_results_names

  peese_st <- try(
    rma.mv(yi = h_jk,
           V = sigma2_h_jk,
           random = list(~ 1 | study_id / out_id),
           data = df_meta,
           mods = ~ 1 + sigma2_h_jk,
           test = "t",
           dfs = "contain",
           verbose = verbose) # , control=list(rel.tol=tol, iter.max=iter)
  )

  if (class(peese_st)[1] =="try-error") {
    peese_st_results$error_peese_st <- 1

    peese_st_results$peese_st_slope <- NA
    peese_st_results$se_peese_st_slope <- NA
    peese_st_results$p_peese_st_slope <- NA
    peese_st_results$ci_lb_peese_st_slope <- NA
    peese_st_results$ci_ub_peese_st_slope <- NA

    peese_st_results$peese_st_int <- NA
    peese_st_results$se_peese_st_int <- NA
    peese_st_results$p_peese_st_int <- NA
    peese_st_results$ci_lb_peese_st_int <- NA
    peese_st_results$ci_ub_peese_st_int <- NA


  } else {
    peese_st_results$error_peese_st <- 0

    peese_st_results$peese_st_slope <- peese_st$beta[2]
    peese_st_results$se_peese_st_slope <- peese_st$se[2]
    peese_st_results$p_peese_st_slope <- peese_st$pval[2]
    peese_st_results$ci_lb_peese_st_slope <- peese_st$ci.lb[2]
    peese_st_results$ci_ub_peese_st_slope <- peese_st$ci.ub[2]

    peese_st_results$peese_st_int <- peese_st$beta[1]
    peese_st_results$se_peese_st_int <- peese_st$se[1]
    peese_st_results$p_peese_st_int <- peese_st$pval[1]
    peese_st_results$ci_lb_peese_st_int <- peese_st$ci.lb[1]
    peese_st_results$ci_ub_peese_st_int <- peese_st$ci.ub[1]
  }

  return(peese_st_results)
}


