# source('r_scripts/outcome_gen.R')
debugSource('r_scripts/outcome_gen.R')
# source('r_scripts/pets.R')
debugSource('r_scripts/pets.R')
# source('r_scripts/list_to_dataframe.R')
debugSource('r_scripts/list_to_dataframe.R')

metas_gen_with_pets <- function(o, k, nmeta, delta_00, sigma2_v, sigma2_u, verbose, save_path,  psss){ # tol, iter,
  # Initialize lists
  outcomes = vector(mode = "list", length = o)
  studies = vector(mode = "list", length = k)
  pets_results = vector(mode = "list", length = nmeta)
  errors <- vector(mode = "list", length = nmeta)

  # Initialize named vector to store corrected estimates
  correct <- vector(mode = "list", length = 10)
  names_correct <- c("corrected_smd", "corrected_smd_se", "corrected_smd_pval", "corrected_smd_95_ci_lb", "corrected_smd_95_ci_ub", "corrected_st_smd", "corrected_st_smd_se", "corrected_st_smd_pval", "corrected_st_smd_95_ci_lb", "corrected_st_smd_95_ci_ub")
  correct <- setNames(correct,names_correct)

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

