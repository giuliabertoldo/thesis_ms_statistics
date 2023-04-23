source('r_scripts/biased_metas_gen_with_pets_puste.R')
# debugSource('r_scripts/biased_metas_gen_with_pets.R')

# Rejection rate
rejection_rate <- function(data, pval_column, nmeta) {
  # Check if there are NA in the pval_colum and update nmeta accordingly
  number_na <- sum(is.na(data[,pval_column]))

  if((number_na > 0)==TRUE) {
    nmeta <- nmeta - number_na
    rej_rate <- (sum(data[, pval_column] <.05, na.rm = TRUE)/nmeta)
  } else {
    rej_rate <- (sum(data[, pval_column] <.05, na.rm = TRUE)/nmeta)
  }
  return(rej_rate)
}

# Convert the variance stabilized transfomed smd back to smd ----
convert_h_to_d <- function(h, avg_n_e, avg_n_c){
    # If there was an error in pets, h = NA, then sinh(NA)= NA, then = NA
    a <- sqrt(4 + 2*(avg_n_e/avg_n_c) +2*(avg_n_c/avg_n_e))
    d <- a*sinh(h/sqrt(2))
  return(d)
}


# Main performance analyzer -----
performance_analyzer <- function(bias_type, delta_00,sigma2_u, sigma2_v, psss, o, k, nmeta, verbose){ # tol, iter,

  # Create directories
  if (!file.exists("data_puste")) { save_path <- dir.create("data_puste") }

  if (!file.exists(sprintf("data_puste/%s",bias_type))) {
    dir.create(file.path("data_puste", sprintf("%s",bias_type)))
  }

  for (num in k) {

    if (!file.exists(sprintf("data_puste/%s/k_%d",bias_type, num))) {
      dir.create(file.path("data_puste", sprintf("%s",bias_type), sprintf("k_%d", num)))
    }

    for (d in delta_00) {

      for (su in sigma2_u) {

        for (sv in sigma2_v) {

          for (p in psss) {
            file_name <- sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p)

            if (!file.exists(file.path(sprintf("data_puste/%s/k_%d", bias_type, num), file_name))) {
              dir.create(file.path(sprintf("data_puste/%s/k_%d",bias_type, num), file_name))

            }
          }
        }
      }
    }
  }

  ###############################

  temp0 <- sprintf("%s", bias_type)
  temp1 <- sprintf("k_%d",k)
  temp2 <- sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", delta_00, sigma2_u, sigma2_v, psss)
  save_path <- file.path("data_puste", temp0, temp1, temp2)



    # Call biased_metas_gen_with_pets
    metas_pets <- biased_metas_gen_with_pets(o = o, k = k, nmeta = nmeta, delta_00 = delta_00, sigma2_v = sigma2_v, sigma2_u = sigma2_u, verbose=verbose, save_path=save_path, psss=psss, bias_type = bias_type) # tol=tol, iter=iter,


  # Convert list to dataframe
  metas_pets <- data.frame(t(sapply(metas_pets,c)))

  # Calculate performance measures
  ## SMD
  rej_pet_slope <- rejection_rate(data = metas_pets, pval_column = "p_pet_slope", nmeta)

  rej_pet_int <- rejection_rate(data = metas_pets, pval_column = "p_pet_int", nmeta )

  bias_pet_int <- mean(metas_pets$pet_int, na.rm = TRUE) - metas_pets$delta_00[1]

  mse_pet_int <- bias_pet_int^2 + var(metas_pets$pet_int, na.rm = TRUE)

  rej_peese_slope <- rejection_rate(data = metas_pets, pval_column = "p_peese_slope", nmeta)

  rej_peese_int <- rejection_rate(data = metas_pets, pval_column = "p_peese_int", nmeta)

  bias_peese_int <-  mean(metas_pets$peese_int, na.rm = TRUE) - metas_pets$delta_00[1]

  mse_peese_int <- bias_peese_int^2 + var(metas_pets$peese_int, na.rm = TRUE)

  bias_corrected_smd <- mean(metas_pets$corrected_smd, na.rm = TRUE) - metas_pets$delta_00[1]

  mse_corrected_smd <- bias_corrected_smd^2 + var(metas_pets$corrected_smd, na.rm = TRUE)

  ## Bias and mse of corrected estimate, only for those pets where pvalue(slope) is less than 5%
  ## indicating small_study effect

  subset_metas_pets <- metas_pets[metas_pets$p_pet_slope < 0.05,]
  subset_size <- dim(subset_metas_pets)[1]

  if (dim(subset_metas_pets)[1]> 0) {
    bias_corrected_smd_subset  <- mean(subset_metas_pets$corrected_smd, na.rm = TRUE) - subset_metas_pets$delta_00[1]
    mse_corrected_smd_subset <- bias_corrected_smd_subset^2 + var(subset_metas_pets$corrected_smd, na.rm = TRUE)
  } else {
    bias_corrected_smd_subset  <- NA
    mse_corrected_smd_subset  <- NA
   }

  ## Variance stabilizing transformation of the SMD

  rej_pet_st_slope <- rejection_rate(data = metas_pets, pval_column = "p_pet_st_slope", nmeta)

  rej_pet_st_int <- rejection_rate(data = metas_pets, pval_column = "p_pet_st_int", nmeta )

  bias_pet_st_int <- mean(convert_h_to_d(metas_pets$pet_st_int, metas_pets$avg_n_e, metas_pets$avg_n_c), na.rm = TRUE) - metas_pets$delta_00[1]

  mse_pet_st_int <- bias_pet_st_int^2 + var(convert_h_to_d(metas_pets$pet_st_int, metas_pets$avg_n_e, metas_pets$avg_n_c), na.rm = TRUE)

  rej_peese_st_slope <- rejection_rate(data = metas_pets, pval_column = "p_peese_st_slope", nmeta)

  rej_peese_st_int <- rejection_rate(data = metas_pets, pval_column = "p_peese_st_int", nmeta)

  bias_peese_st_int <- mean(convert_h_to_d(metas_pets$peese_st_int, metas_pets$avg_n_e, metas_pets$avg_n_c), na.rm = TRUE) - metas_pets$delta_00[1]

  mse_peese_st_int <- bias_peese_st_int^2 + var(convert_h_to_d(metas_pets$peese_st_int, metas_pets$avg_n_e, metas_pets$avg_n_c), na.rm = TRUE)

  bias_corrected_st_smd <- mean(convert_h_to_d(metas_pets$corrected_st_smd, metas_pets$avg_n_e, metas_pets$avg_n_c), na.rm = TRUE) - metas_pets$delta_00[1]

  mse_corrected_st_smd <- bias_corrected_st_smd^2 + var(convert_h_to_d(metas_pets$corrected_st_smd, metas_pets$avg_n_e, metas_pets$avg_n_c), na.rm = TRUE)

  ## Bias and mse of corrected estimate, only for those pets where pvalue(slope) is less than 5%
  ## indicating small_study effect

  if (dim(subset_metas_pets)[1]> 0) {
    bias_corrected_st_smd_subset  <- mean(convert_h_to_d(subset_metas_pets$corrected_st_smd, subset_metas_pets$avg_n_e, subset_metas_pets$avg_n_c), na.rm = TRUE) - subset_metas_pets$delta_00[1]
    mse_corrected_st_smd_subset <- bias_corrected_st_smd_subset^2 + var(convert_h_to_d(subset_metas_pets$corrected_st_smd, subset_metas_pets$avg_n_e, subset_metas_pets$avg_n_c), na.rm = TRUE)
  } else {
    bias_corrected_st_smd_subset  <- NA
    mse_corrected_st_smd_subset  <- NA
  }

  # Add counting
  avg_perc_out_selected <- mean(metas_pets$perc_out_selected)
  avg_num_out_selected <- mean(metas_pets$tot_num_out_real)

  avg_perc_stud_selected <- mean(metas_pets$perc_stud_selected)
  avg_num_study_selected <- mean(metas_pets$k_real)

  avg_avg_n_out_per_study <- mean(metas_pets$avg_n_out_per_study)
  avg_min_n_out_per_study <- mean(metas_pets$min_n_out_per_study)
  avg_max_n_out_per_study <- mean(metas_pets$max_n_out_per_study)

  # Create a named vector to store performance measures
  performance_values <- c( k, delta_00, sigma2_u, sigma2_v, psss, rej_pet_slope ,  rej_pet_int , bias_pet_int , mse_pet_int,
                           rej_peese_slope, rej_peese_int, bias_peese_int, mse_peese_int,
                           rej_pet_st_slope, rej_pet_st_int, bias_pet_st_int, mse_pet_st_int ,
                           rej_peese_st_slope , rej_peese_st_int , bias_peese_st_int , mse_peese_st_int,
                           bias_corrected_smd, mse_corrected_smd, bias_corrected_st_smd, mse_corrected_st_smd,
                           bias_corrected_smd_subset, mse_corrected_smd_subset, bias_corrected_st_smd_subset, mse_corrected_st_smd_subset, subset_size,
                           avg_perc_out_selected, avg_num_out_selected, avg_perc_stud_selected, avg_num_study_selected,
                           avg_avg_n_out_per_study, avg_min_n_out_per_study, avg_max_n_out_per_study)
  performance_names <- c("k", "delta_00", "sigma2_u", "sigma2_v", "psss", "rej_pet_slope" ,  "rej_pet_int" , "bias_pet_int" , "mse_pet_int",
                         "rej_peese_slope", "rej_peese_int", "bias_peese_int", "mse_peese_int",
                         "rej_pet_st_slope", "rej_pet_st_int", "bias_pet_st_int", "mse_pet_st_int" ,
                         "rej_peese_st_slope" , "rej_peese_st_int" , "bias_peese_st_int" , "mse_peese_st_int",
                         "bias_corrected_smd", "mse_corrected_smd", "bias_corrected_st_smd", "mse_corrected_st_smd",
                         "bias_corrected_smd_subset", "mse_corrected_smd_subset", "bias_corrected_st_smd_subset", "mse_corrected_st_smd_subset", "subset_size",
                         "avg_perc_out_selected", "avg_num_out_selected", "avg_perc_stud_selected", "avg_num_study_selected",
                         "avg_avg_n_out_per_study", "avg_min_n_out_per_study", "avg_max_n_out_per_study")
  performances <- setNames(performance_values, performance_names)

  save(performances, file=file.path(save_path, "performances.Rdata"))

  return(performances)

}


