library(tidyverse)
# Create dataframe with performance data for all conditions to be visualized
df_viz <- function(num_studies, delta_00, psss, sigma2_u, sigma2_v, bias_type){
  df = data.frame()

  for(bt in bias_type){
    for (k in num_studies) {
      for(d in delta_00) {
        for(p in psss) {
          for(su in sigma2_u) {
            for(sv in sigma2_v) {

              temp0 <- sprintf("%s", bt)
              temp1 <- sprintf("k_%d",k)
              temp2 <- sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p)
              file_path <- file.path("data",temp0, temp1, temp2, "performances.Rdata")

              if(file.exists(file_path)){
                load(file_path)

                # Create condition ID to the vector
                id <- sprintf("%s_k_%d_d%0.2f_su%0.2f_sv%0.2f_%s", bt, k, d, su, sv, p)

                # Create ID grouping by psss
                id_rollup_psss <- sprintf("%s_k_%d_d%0.2f_su%0.2f_sv%0.2f", bt, k, d, su, sv)

                # Add bias_type & condition ID to the vector
                vec <- cbind(id, id_rollup_psss, bt, as.data.frame(t(performances)))

                # Add performances to dataframe
                df <- rbind(df,vec)
              }
            }
          }
        }
      }
    }
  }

  # Add k as character
  df$k_cat <- as.factor(x = df$k)
  levels(df$k_cat) <- c("small", "medium", "large")
  df$k_cat <- as.character(df$k_cat)

  # Add su as character
  df$sigma2_u_cat <- as.factor(df$sigma2_u)
  levels(df$sigma2_u_cat) <- c("small", "medium", "large")
  df$sigma2_u_cat <- as.character(df$sigma2_u_cat)

  # Add sv as character
  df$sigma2_v_cat <- as.factor(df$sigma2_v)
  levels(df$sigma2_v_cat) <- c("small", "medium", "large")
  df$sigma2_v_cat <- as.character(df$sigma2_v_cat)

  # Convert some columns to numeric
  for (i in 1:dim(df)[2]) {
    if(!names(df)[i] %in% c("psss","bt", "id", "id_rollup_psss", "k_cat", "sigma2_u_cat", "sigma2_v_cat")) {
      df[,i] <- as.numeric(df[,i])
      # df[,i] <- round(df[,i], 4) Round to 4 decimal points
    }
  }

  return(df)
}

# Visualize rejection rates
viz_rejection_rate <- function(df, num_studies, bias_type, d, su, sv, ss, across){

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k %in% num_studies,
           bt %in% bias_type,
           delta_00 %in% d,
           sigma2_u %in% su,
           sigma2_v %in% sv,
           psss %in% ss)

  # Set the correct parameters for the plot
  if(bias_type == "pb_no_orb_no"){
    hline = 0.05
    limits_bounds = c(0, 0.6)
  } else {
    hline = 0.80
    limits_bounds = c(0, 1)
  }

  if(bias_type == "pb_no_orb_no"){
    dataset = "No PB, No ORB"
  } else if(bias_type == "pb_str_orb_no") {
    dataset = "Strong PB, No ORB"
  } else if(bias_type == "pb_mod_orb_no") {
    dataset = "Moderate PB, No ORB"
  } else if(bias_type == "pb_no_orb_str") {
    dataset = "No PB, Strong ORB"
  } else if(bias_type == "pb_no_orb_mod") {
    dataset = "No PB, Moderate ORB"
  } else if(bias_type == "pb_str_orb_str") {
    dataset = "Strong PB, Strong ORB"
  } else if(bias_type == "pb_mod_orb_mod") {
    dataset = "Moderate PB, Moderate ORB"
  } else if (bias_type == "pb_mod_orb_str") {
    dataset = "Moderate PB, Strong ORB"
  } else if (bias_type == "pb_str_orb_mod") {
    dataset = "Strong PB, Moderate ORB"
  }

  if(across == "k") {
    # Make plot
    viz <- df1 %>%
      ggplot(aes(x = delta_00)) +
      geom_point(aes(y = rej_pet_slope, color = "SMD"),  pch = 1) +
      geom_point(aes(y = rej_pet_st_slope, color = "Transformed SMD"), pch = 2) +
      facet_grid(k_cat ~ fct_rev(psss)) +
      geom_hline(yintercept = hline, linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
      scale_y_continuous(limits = limits_bounds,sec.axis = sec_axis(~ . , name = "Meta-analytic dataset size", breaks = NULL, labels = NULL)) +
      scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
      labs(y = "Rejection rate",
           x = "Population SMD",
           caption = sprintf("Between-study variance: %s. Within-study variance: %s. Bias type: %s.", df1$sigma2_u_cat, df1$sigma2_v_cat, dataset)) +
      theme_bw()
  } else if(across == "su_sv"){
    viz <- df1 %>%
      ggplot(aes(x = delta_00)) +
      geom_point(aes(y = rej_pet_slope, color = "SMD"),  pch = 1) +
      geom_point(aes(y = rej_pet_st_slope, color = "Transformed SMD"), pch = 2) +
      facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
      geom_hline(yintercept = hline, linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
      scale_y_continuous(limits = limits_bounds,sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
      scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
      labs(y = "Rejection rate",
           x = "Population SMD",
           caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
      theme_bw()
  }



  return (viz)
}

# Visualize bias
viz_bias <- function(df, num_studies, bias_type, d, su, sv, ss, across){

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k %in% num_studies,
           bt %in% bias_type,
           delta_00 %in% d,
           sigma2_u %in% su,
           sigma2_v %in% sv,
           psss %in% ss)

  # Set the correct parameters for the plot
  # if(bias_type == "pb_no_orb_no"){
  #   hline = 0.05
  #   limits_bounds = c(0, 0.6)
  # } else {
  #   hline = 0.80
  #   limits_bounds = c(0, 1)
  # }

  if(bias_type == "pb_no_orb_no"){
    dataset = "No PB, No ORB"
  } else if(bias_type == "pb_str_orb_no") {
    dataset = "Strong PB, No ORB"
  } else if(bias_type == "pb_mod_orb_no") {
    dataset = "Moderate PB, No ORB"
  } else if(bias_type == "pb_no_orb_str") {
    dataset = "No PB, Strong ORB"
  } else if(bias_type == "pb_no_orb_mod") {
    dataset = "No PB, Moderate ORB"
  } else if(bias_type == "pb_str_orb_str") {
    dataset = "Strong PB, Strong ORB"
  } else if(bias_type == "pb_mod_orb_mod") {
    dataset = "Moderate PB, Moderate ORB"
  } else if (bias_type == "pb_mod_orb_str") {
    dataset = "Moderate PB, Strong ORB"
  } else if (bias_type == "pb_str_orb_mod") {
    dataset = "Strong PB, Moderate ORB"
  }

  if(across == "k") {
    # Make plot
    viz <- df1 %>%
      ggplot(aes(x = delta_00)) +
      geom_point(aes(y = bias_corrected_smd, color = "SMD"),  pch = 1) +
      geom_point(aes(y = bias_corrected_st_smd, color = "Transformed SMD"), pch = 2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      facet_grid(k_cat ~ fct_rev(psss)) +
      scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Meta-analytic dataset size", breaks = NULL, labels = NULL)) +
      scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
      labs(y = "Bias",
           x = "Population SMD",
           caption = sprintf("Between-study variance: %s. Within-study variance: %s. Bias type: %s.", df1$sigma2_u_cat, df1$sigma2_v_cat, dataset)) +
      theme_bw()
  }

  else if(across == "su_sv"){
    viz <- df1 %>%
      ggplot(aes(x = delta_00)) +
      geom_point(aes(y = bias_corrected_smd, color = "SMD"),  pch = 1) +
      geom_point(aes(y = bias_corrected_st_smd, color = "Transformed SMD"), pch = 2) +
      facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
      scale_y_continuous(sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
      scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
      labs(y = "Rejection rate",
           x = "Population SMD",
           caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
      theme_bw()
  }



  return (viz)
}

# Visualize mse
viz_mse <- function(df, num_studies, bias_type, d, su, sv, ss, across){

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k %in% num_studies,
           bt %in% bias_type,
           delta_00 %in% d,
           sigma2_u %in% su,
           sigma2_v %in% sv,
           psss %in% ss)

  if(bias_type == "pb_no_orb_no"){
    dataset = "No PB, No ORB"
  } else if(bias_type == "pb_str_orb_no") {
    dataset = "Strong PB, No ORB"
  } else if(bias_type == "pb_mod_orb_no") {
    dataset = "Moderate PB, No ORB"
  } else if(bias_type == "pb_no_orb_str") {
    dataset = "No PB, Strong ORB"
  } else if(bias_type == "pb_no_orb_mod") {
    dataset = "No PB, Moderate ORB"
  } else if(bias_type == "pb_str_orb_str") {
    dataset = "Strong PB, Strong ORB"
  } else if(bias_type == "pb_mod_orb_mod") {
    dataset = "Moderate PB, Moderate ORB"
  } else if (bias_type == "pb_mod_orb_str") {
    dataset = "Moderate PB, Strong ORB"
  } else if (bias_type == "pb_str_orb_mod") {
    dataset = "Strong PB, Moderate ORB"
  }

  if(across == "k") {
    # Make plot
    viz <- df1 %>%
      ggplot(aes(x = delta_00)) +
      geom_point(aes(y = mse_corrected_smd, color = "SMD"),  pch = 1) +
      geom_point(aes(y = mse_corrected_st_smd, color = "Transformed SMD"), pch = 2) +
      facet_grid(k_cat ~ fct_rev(psss)) +
      scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
      scale_y_continuous(limits = c(0, 0.50), sec.axis = sec_axis(~ . , name = "Meta-analytic dataset size", breaks = NULL, labels = NULL)) +
      scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
      labs(y = "MSE",
           x = "Population SMD",
           caption = sprintf("Between-study variance: %s. Within-study variance: %s. Bias type: %s.", df1$sigma2_u_cat, df1$sigma2_v_cat, dataset)) +
      theme_bw()
  }

  else if(across == "su_sv"){
    viz <- df1 %>%
      ggplot(aes(x = delta_00)) +
      geom_point(aes(y = mse_corrected_smd, color = "SMD"),  pch = 1) +
      geom_point(aes(y = mse_corrected_st_smd, color = "Transformed SMD"), pch = 2) +
      facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
      scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
      scale_y_continuous(limits = c(0, 0.50), sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
      scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
      labs(y = "MSE",
           x = "Population SMD",
           caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
      theme_bw()
  }



  return (viz)
}

