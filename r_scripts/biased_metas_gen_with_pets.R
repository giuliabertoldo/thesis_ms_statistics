source('r_scripts/list_to_dataframe.R')
# debugSource('r_scripts/list_to_dataframe.R')

source('r_scripts/pets.R')
# debugSource('r_scripts/pets.R')

source('r_scripts/orb.R')
# debugSource('r_scripts/orb.R')

source('r_scripts/pb.R')
# debugSource('r_scripts/pb.R')

source('r_scripts/pb_orb.R')
# debugSource('r_scripts/pb_orb.R')

biased_metas_gen_with_pets <- function(o, k, nmeta, delta_00, sigma2_v, sigma2_u, verbose, save_path, psss, bias_type){

  # Initialize lists
  pets_results = vector(mode = "list", length = nmeta)
  errors <- vector(mode = "list", length = nmeta)

  # Initialize named vector to store corrected estimates
  correct <- vector(mode = "list", length = 10)
  names_correct <- c("corrected_smd", "corrected_smd_se", "corrected_smd_pval", "corrected_smd_95_ci_lb", "corrected_smd_95_ci_ub", "corrected_st_smd", "corrected_st_smd_se", "corrected_st_smd_pval", "corrected_st_smd_95_ci_lb", "corrected_st_smd_95_ci_ub")
  correct <- setNames(correct,names_correct)

  # For each meta-analytic dataset
  for (m in 1:nmeta) {
    # Find the path with the m-th unbiased meta-analytic dataset
    meta_path <- file.path("data", "pb_no_orb_no", sprintf("k_%d",k), sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", delta_00, sigma2_u, sigma2_v, psss), sprintf("studies_meta%d.Rdata", m))

    # If the file exists:
    if(file.exists(meta_path)){
      # Load unbiased meta-analytic dataset
      load(meta_path)

      # Convert from list to dataframe
      df_indicator_bias <- list_to_dataframe(studies = studies, k = k, o = o)

      ## Create new column which indicates which nmeta it is
      df_indicator_bias <- cbind(nmeta_id = m, df_indicator_bias)

      ## Generate biased dataframe

      if (bias_type == "pb_no_orb_mod") {
        out <- orb(df = df_indicator_bias, bias_type = "pb_no_orb_mod")
        df_indicator_bias <- out$df_indicator_bias
        df_biased <- out$df_biased

        # Save datasets (original with indicator + biased)
        save(df_indicator_bias, file=file.path(save_path,sprintf("indicator_bias_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_no_orb_str"){
        out <- orb(df = df_indicator_bias, bias_type = "pb_no_orb_str")
        df_indicator_bias <- out$df_indicator_bias
        df_biased <- out$df_biased

        # Save datasets (original with indicator + biased)
        save(df_indicator_bias, file=file.path(save_path,sprintf("indicator_bias_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_mod_orb_no") {
        out <- pb(df = df_indicator_bias, k = k, bias_type = "pb_mod_orb_no")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save datasets (original with indicator + biased)
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_str_orb_no") {
        out <- pb(df = df_indicator_bias, k = k, bias_type = "pb_str_orb_no")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save datasets (original with indicator + biased)
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_mod_orb_mod") {
        out <- pb_orb(df = df_indicator_bias, k = k, bias_type = "pb_mod_orb_mod")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save dataset
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_str_orb_str") {
        out <- pb_orb(df = df_indicator_bias, k = k,  bias_type = "pb_str_orb_str")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save dataset
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_str_orb_mod") {
        out <- pb_orb(df = df_indicator_bias, k = k, bias_type = "pb_str_orb_mod")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save dataset
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_mod_orb_str") {
        out <- pb_orb(df = df_indicator_bias, k = k, bias_type = "pb_mod_orb_str")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save dataset
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      }

      # Calculate total sample sizes per group in the meta-analysis (to be used to convert h to d)
      avg_n_e <- mean(df_biased$n_e)
      avg_n_c <- mean(df_biased$n_c)

      # Conduct pets
      ## Traditional PET
      pet1_results <- pet(df_meta = df_biased, verbose = verbose)

      ## Traditional PEESE
      peese1_results <- peese(df_meta = df_biased, verbose = verbose)

      ## Variance stabilizing PET
      pet1_st_results <- pet_var_stab(df_meta = df_biased, verbose = verbose)

      ## Variance stabilizing PEESE
      peese1_st_results <- peese_var_stab(df_meta = df_biased, verbose = verbose)

      ## Add the list with 0/1 for errors to the complete list for all nmeta
      errors[[m]] <- c(pet1_results$error_pet, peese1_results$error_peese, pet1_st_results$error_pet_st, peese1_st_results$error_peese_st)

      # Put all pets results in a vector
      temp_pets <- c(pet1_results, peese1_results, pet1_st_results, peese1_st_results)

      ## Choose "corrected" estimate
      ## If PET intercept in one tailed test is greater than zero, then PEESE intercept is the estimate

      if (!is.na(temp_pets$p_pet_int) & !is.na(temp_pets$pet_int)) {
        if (temp_pets$p_pet_int < .10 & temp_pets$pet_int > 0) {

          correct$corrected_smd <- temp_pets$peese_int
          correct$corrected_smd_se <- temp_pets$se_peese_int
          correct$corrected_smd_pval <- temp_pets$p_peese_int
          correct$corrected_smd_95_ci_lb <- temp_pets$ci_lb_peese_int
          correct$corrected_smd_95_ci_ub <- temp_pets$ci_ub_peese_int

        } else {

          correct$corrected_smd <- temp_pets$pet_int
          correct$corrected_smd_se <- temp_pets$se_pet_int
          correct$corrected_smd_pval <- temp_pets$p_pet_int
          correct$corrected_smd_95_ci_lb <- temp_pets$ci_lb_pet_int
          correct$corrected_smd_95_ci_ub <- temp_pets$ci_ub_pet_int

        }
      } else {
        correct$corrected_smd <- NA
        correct$corrected_smd_se <- NA
        correct$corrected_smd_pval <- NA
        correct$corrected_smd_95_ci_lb <- NA
        correct$corrected_smd_95_ci_ub <- NA
      }

      ## corrected_st_smd
      if (!is.na(temp_pets$p_pet_st_int) & !is.na(temp_pets$pet_st_int)) {
        if (temp_pets$p_pet_st_int < .10 & temp_pets$pet_st_int > 0) {
          correct$corrected_st_smd <- temp_pets$peese_st_int
          correct$corrected_st_smd_se <- temp_pets$se_peese_st_int
          correct$corrected_st_smd_pval <- temp_pets$p_peese_st_int
          correct$corrected_st_smd_95_ci_lb <- temp_pets$ci_lb_peese_st_int
          correct$corrected_st_smd_95_ci_ub <- temp_pets$ci_ub_peese_st_int


        } else {
          correct$corrected_st_smd <- temp_pets$pet_st_int
          correct$corrected_st_smd_se <- temp_pets$se_pet_st_int
          correct$corrected_st_smd_pval <- temp_pets$p_pet_st_int
          correct$corrected_st_smd_95_ci_lb <- temp_pets$ci_lb_pet_st_int
          correct$corrected_st_smd_95_ci_ub <- temp_pets$ci_ub_pet_st_int
        }
      } else {
        correct$corrected_st_smd <- NA
        correct$corrected_st_smd_se <- NA
        correct$corrected_st_smd_pval <- NA
        correct$corrected_st_smd_95_ci_lb <- NA
        correct$corrected_st_smd_95_ci_ub <- NA
      }

      # Collect results in one list
      temp1 <- c(temp_pets, correct)
      # Convert list to named vector
      temp2 <- unlist(temp1)
      # Add information for performances calculation
      temp3 <- c(delta_00, avg_n_c, avg_n_e)
      temp3 <- setNames(temp3, c("delta_00", "avg_n_c", "avg_n_e"))
      # Concatenate
      temp4 <- c(temp3, temp2)

      # Populate final list with vectors
      pets_results[[m]]<- temp4
    }
  }
  # Save pets results
  save(pets_results, file=file.path(save_path, "pets_results.Rdata"))

  # Convert list with errors into dataframe with
  errors <- data.frame(t(sapply(errors,c)))
  errors_names <- c("pet", "peese", "pet_st", "peese_st")
  names(errors) <- errors_names
  rownames(errors) <- 1:nrow(errors)
  # Save errors dataframe
  save(errors, file=file.path(save_path, "errors.Rdata"))

  return(pets_results)
}


