source('list_to_dataframe.R')
# debugSource('list_to_dataframe.R')
source('pets.R')
# debugSource('pets.R')
source('orb.R')
# debugSource('orb.R')
source('pb.R')
# debugSource('pb.R')
source('pb_orb.R')
# debugSource('pb_orb.R')

biased_metas_gen_with_pets <- function(o, k, nmeta, delta_00, sigma2_v, sigma2_u, verbose, save_path, psss, bias_type){

  # Initialize lists
  pets_results = vector(mode = "list", length = nmeta)
  errors <- vector(mode = "list", length = nmeta)

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
        out <- pb(df = df_indicator_bias, bias_type = "pb_mod_orb_no")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save datasets (original with indicator + biased)
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_str_orb_no") {
        out <- pb(df = df_indicator_bias, bias_type = "pb_str_orb_no")
        df_grouped <- out$df_grouped
        df_biased <- out$df_biased

        # Save datasets (original with indicator + biased)
        save(df_grouped, file=file.path(save_path,sprintf("grouped_meta%d.Rdata", m)))
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_mod_orb_mod") {
        df_biased <- pb_orb(df = df_indicator_bias, bias_type = "pb_mod_orb_mod")

        # Save dataset
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_str_orb_str") {
        df_biased <- pb_orb(df = df_indicator_bias, bias_type = "pb_str_orb_str")

        # Save dataset
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_str_orb_mod") {
        df_biased <- pb_orb(df = df_indicator_bias, bias_type = "pb_str_orb_mod")

        # Save dataset
        save(df_biased, file=file.path(save_path,sprintf("biased_meta%d.Rdata", m)))

      } else if (bias_type == "pb_mod_orb_str") {
        df_biased <- pb_orb(df = df_indicator_bias, bias_type = "pb_mod_orb_str")

        # Save dataset
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
      errors[[m]] <- c(pet1_results$error, peese1_results$error, pet1_st_results$error, peese1_st_results$error)

      ## Add the choice for the final corrected estimate
      ## If PET intercept in one tailed test is greater then zero: then PEESE intercept is the estimate
      ## corrected_smd

      if (!is.na(pet1_results$p_pet_int) & !is.na(pet1_results$pet_int)) {
        if (pet1_results$p_pet_int < .10 & pet1_results$pet_int > 0) {
          corrected_smd <- peese1_results$peese_int
        } else {
          corrected_smd <- pet1_results$pet_int
        }
      } else {
        corrected_smd <- NA
      }

      ## corrected_st_smd
      if (!is.na(pet1_st_results$p_pet_st_int) & !is.na(pet1_st_results$pet_st_int)) {
        if (pet1_st_results$p_pet_st_int < .10 & pet1_st_results$pet_st_int > 0) {
          corrected_st_smd <- peese1_st_results$peese_st_int
        } else {
          corrected_st_smd <- pet1_st_results$pet_st_int
        }
      } else {
        corrected_st_smd <- NA
      }

      ## Save results of pets in a vector
      pets_values <- c(delta_00, avg_n_e, avg_n_c, pet1_results$pet_slope, pet1_results$se_pet_slope, pet1_results$p_pet_slope, pet1_results$pet_int, pet1_results$se_pet_int, pet1_results$p_pet_int, peese1_results$peese_slope, peese1_results$se_peese_slope, peese1_results$p_peese_slope, peese1_results$peese_int, peese1_results$se_peese_int, peese1_results$p_peese_int, pet1_st_results$pet_st_slope, pet1_st_results$se_pet_st_slope, pet1_st_results$p_pet_st_slope, pet1_st_results$pet_st_int, pet1_st_results$se_pet_st_int, pet1_st_results$p_pet_st_int, peese1_st_results$peese_st_slope, peese1_st_results$se_peese_st_slope, peese1_st_results$p_peese_st_slope, peese1_st_results$peese_st_int, peese1_st_results$se_peese_st_int, peese1_st_results$p_peese_st_int, corrected_smd, corrected_st_smd)
      pets_names <- c("delta_00", "avg_n_e", "avg_n_c", "pet_slope", "se_pet_slope", "p_pet_slope", "pet_int", "se_pet_int", "p_pet_int", "peese_slope", "se_peese_slope", "p_peese_slope", "peese_int", "se_peese_int", "p_peese_int", "pet_st_slope", "se_pet_st_slope", "p_pet_st_slope", "pet_st_int", "se_pet_st_int", "p_pet_st_int", "peese_st_slope", "se_peese_st_slope", "p_peese_st_slope", "peese_st_int", "se_peese_st_int", "p_peese_st_int", "corrected_smd", "corrected_st_smd")
      pets <- setNames(pets_values, pets_names)

      # Populate final list with vectors
      pets_results[[m]] <- pets
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


