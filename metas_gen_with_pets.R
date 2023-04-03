source('outcome_gen.R')
# debugSource('outcome_gen.R')
source('pets.R')
# debugSource('pets.R')
source('list_to_dataframe.R')
# debugSource('list_to_dataframe.R')

metas_gen_with_pets <- function(o, k, nmeta, delta_00, sigma2_v, sigma2_u, verbose, save_path,  psss){ # tol, iter,
  # Initialize lists
  outcomes = vector(mode = "list", length = o)
  studies = vector(mode = "list", length = k)
  pets_results = vector(mode = "list", length = nmeta)
  errors <- vector(mode = "list", length = nmeta)

  for(m in 1:nmeta) {   # loop for the number of times we simulate the same condition meta
    for(j in 1:k){      # loop for k studies
      studies[[j]] <- outcome_gen(delta_00 = delta_00, sigma2_v = sigma2_v, sigma2_u = sigma2_u, o = o, psss = psss)
    }

    # Convert list_to_dataframe
    df_studies <- list_to_dataframe(studies = studies, k = k, o = o)

    # Calculate total sample sizes per group in the meta-analysis (to be used to convert h to d)
    avg_n_e <- mean(df_studies$n_e)
    avg_n_c <- mean(df_studies$n_c)

    # Conduct pets

    ## Traditional PET
    pet1_results <- pet(df_meta = df_studies, verbose = verbose)

    ## Traditional PEESE
    peese1_results <- peese(df_meta = df_studies, verbose = verbose)

    ## Variance stabilizing PET
    pet1_st_results <- pet_var_stab(df_meta = df_studies, verbose = verbose)

    ## Variance stabilizing PEESE
    peese1_st_results <- peese_var_stab(df_meta = df_studies, verbose = verbose)

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

    # Save in R object studies
    save(studies, file=file.path(save_path,sprintf("studies_meta%d.Rdata", m)))

    # Print which meta was done
    # print(sprintf("Meta: %d", m))

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

