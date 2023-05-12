library(tidyverse)
library(ggplot2)
library(kableExtra)
library(DT)
library(stringr)
library(ggpubr)
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

df_viz_puste <- function(num_studies, delta_00, psss, sigma2_u, sigma2_v, bias_type){
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
              file_path <- file.path("data_puste",temp0, temp1, temp2, "performances.Rdata")

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

table_psss <- function(df){
  table_out <- df %>%
    select("psss_nominal",
           "avg_ss") %>%
    group_by(psss_nominal) %>%
    summarize(mean = round(mean(avg_ss, na.rm = T)),
              min = round(min(avg_ss, na.rm = T)),
              max = round(max(avg_ss, na.rm = T)),
              range = max - min) %>%
      rename(sampleSize = psss_nominal)

  return(table_out)

}

table_perc_out_excluded_by_bt <- function(df){

  table_out <- df %>%
    group_by(bt_read) %>%
    summarize(mean = round(mean(avg_perc_out_excluded, na.rm = T)),
              min = round(min(avg_perc_out_excluded, na.rm = T)),
              max = round(max(avg_perc_out_excluded, na.rm = T)),
              range = max - min) %>%
    rename(biasType = bt_read)

  return(table_out)
}

viz_hist_perc_excluded <- function(df){

  # Select only biased data
  df <- df %>%
    filter(bt != "pb_no_orb_no")

  # Add labels
  new_labels <- c("Outcome Reporting Bias", "Publication Bias")
  names(new_labels) <- c("orb", "pb")

  # Convert to factor
  df$pb_orb <- as.factor(df$pb_orb)

  viz <- df %>%
    ggplot(aes(x = avg_perc_out_excluded, fill = factor(mod_str))) +
    geom_histogram(aes(y=..count../sum(..count..)),
                   alpha = 0.6) +
    facet_grid(. ~ pb_orb, labeller = labeller(pb_orb = new_labels ))+
    scale_y_continuous(limits = c(0,0.07)) +
    labs(x = "Percentage of outcomes excluded from original dataset",
         y = "Percent") +
    scale_fill_manual(name="Bias strength",values=c("#E69F00","#0072B2"),labels=c("Moderate","Strong")) +
    theme_bw()

  return(viz)
}

table_perc_out_excluded_by_bt_delta <- function(df){

  table_out <- df %>%
    group_by(bt_read, delta_00) %>%
    summarize(mean = round(mean(avg_perc_out_excluded, na.rm = T)),
              min = round(min(avg_perc_out_excluded, na.rm = T)),
              max = round(max(avg_perc_out_excluded, na.rm = T)),
              range = max - min) %>%
    rename(biasType = bt_read,
           populationSMD = delta_00 )

  return(table_out)
}

viz_hist_perc_excluded_by_d <- function(df){

  # Select only biased data
  df <- df %>%
    filter(bt != "pb_no_orb_no")

  # Add labels
  new_labels <- c("Outcome Reporting Bias", "Publication Bias")
  names(new_labels) <- c("orb", "pb")

  # Convert to factors
  df$pb_orb <- as.factor(df$pb_orb)

  viz <- df %>%
    ggplot(aes(x = avg_perc_out_excluded, fill = factor(mod_str))) +
    geom_histogram(aes(y=..count../sum(..count..)),
                   alpha = 0.6) +
    facet_grid(delta_00  ~ pb_orb, labeller = labeller(pb_orb = new_labels ))+
    scale_y_continuous(limits = c(0,0.07), sec.axis = sec_axis(~ . , name = "Population SMD", breaks = NULL, labels = NULL)) +
    labs(x = "Percentage of outcomes excluded from original dataset",
         y = "Percent") +
    scale_fill_manual(name="Bias strength",values=c("#E69F00","#0072B2"),labels=c("Moderate","Strong")) +
    theme_bw()

  return(viz)
}

viz_hist_perc_excluded_by_psss <- function(df){

  # Select only biased data
  df <- df %>%
    filter(bt != "pb_no_orb_no")

  # Add labels
  new_labels <- c("Outcome Reporting Bias", "Publication Bias")
  names(new_labels) <- c("orb", "pb")

  # Convert to factors
  df$pb_orb <- as.factor(df$pb_orb)

  viz <- df %>%
    ggplot(aes(x = avg_perc_out_excluded, fill = factor(mod_str))) +
    geom_histogram(aes(y=..count../sum(..count..)),
                   alpha = 0.6) +
    facet_grid(psss  ~ pb_orb, labeller = labeller(pb_orb = new_labels ))+
    scale_y_continuous(limits = c(0,0.07), sec.axis = sec_axis(~ . , name = "Primary studies sample size", breaks = NULL, labels = NULL)) +
    labs(x = "Percentage of outcomes excluded from original dataset",
         y = "Percent") +
    scale_fill_manual(name="Bias strength",values=c("#E69F00","#0072B2"),labels=c("Moderate","Strong")) +
    theme_bw()

  return(viz)
}

subset_pivot_data <- function(df){

  df1 <- df %>%
    select(id, bt, k, delta_00, sigma2_u, sigma2_v, psss, rej_pet_slope, rej_pet_st_slope, sigma2_u_cat, k_cat) %>%
    pivot_longer(cols = c(rej_pet_slope, rej_pet_st_slope),
                 names_to = "smd",
                 values_to = "rejection_rate") %>%
    rename(parameter_name = smd )

  df1[, 'smd'] <- NA
  for(i in 1:dim(df1)[1]){
    if(grepl("st",df1[i, 'parameter_name'])){
      df1$smd[i] <- "smd_st"
    } else {
      df1$smd[i] <- "smd"
    }
  }

  return(df1)
}

viz_rejection_rate_pivot <- function(df, num_studies, bias_type){

  df <- subset_pivot_data(df)

  d <- c(0, 0.2, 0.5, 0.8)
  su <- c(0.01, 0.06, 0.11)
  sv <- c(0.01, 0.06, 0.11)
  ss <- c("small", "medium", "large")

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  } else if (bias_type == "None"){
    bias_type <- "pb_no_orb_no"
    dataset <- "None"
  }

  # Is rejection rate Type I error or power?
  if(bias_type =="pb_no_orb_no"){
    type_of_rejection_rate = "Type I Error"
  } else if (bias_type !="pb_no_orb_no") {
    type_of_rejection_rate = "Power"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt %in% bias_type,
           delta_00 %in% d,
           sigma2_u %in% su,
           sigma2_v %in% sv,
           psss %in% ss,
           sigma2_u == sigma2_v)

  # Set the correct parameters for the plot
  if(bias_type == "pb_no_orb_no"){
    hline = 0.05
    limits_bounds = c(0, 1)
  } else {
    hline = 0.80
    limits_bounds = c(0, 1)
  }

  # Set factors
  df1$sigma2_u_cat <- as.factor(df1$sigma2_u_cat)

  viz <- df1 %>%
    ggplot(aes(x = delta_00, y = rejection_rate, color = smd, shape = smd, line = smd)) +
    geom_point() +
    scale_color_hue(l=40, c=35) +
    geom_line() +
    scale_color_grey() +
    # (name = "Effect Size", labels = c("SMD", "Transf. SMD"), values = c("#E69F00", "#0072B2")) +
    #scale_linetype_manual(name = "Effect Size", labels = c("SMD", "Transf. SMD"), values=c("solid", "dotted")) +
    # scale_shape_manual(name = "Effect Size", labels = c("SMD", "Transf. SMD"), values=c(1, 2)) +
    facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
    geom_hline(yintercept = hline, linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
    scale_y_continuous(limits = limits_bounds,sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    labs(y = type_of_rejection_rate,
         x = "Population SMD",
         caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
    theme_bw()


  ggplotly(viz)

  return (viz)
}

viz_rejection_rate <- function(df, num_studies, bias_type){

  d <- c(0, 0.2, 0.5, 0.8)
  su <- c(0.01, 0.06, 0.11)
  sv <- c(0.01, 0.06, 0.11)
  ss <- c("small", "medium", "large")

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  } else if (bias_type == "None"){
    bias_type <- "pb_no_orb_no"
    dataset <- "None"
  }

  # Is rejection rate Type I error or power?
  if(bias_type =="pb_no_orb_no"){
    type_of_rejection_rate = "Type I Error"
  } else if (bias_type !="pb_no_orb_no") {
    type_of_rejection_rate = "Power"
  }

  # Select from dataframe only the observations of interest
    df1 <- df %>%
      filter(k == num_studies,
             bt %in% bias_type,
             delta_00 %in% d,
             sigma2_u %in% su,
             sigma2_v %in% sv,
             psss %in% ss,
             sigma2_u == sigma2_v)

  # Set the correct parameters for the plot
  if(bias_type == "pb_no_orb_no"){
    hline = 0.05
    limits_bounds = c(0, 1)
  } else {
    hline = 0.80
    limits_bounds = c(0, 1)
  }

  # Set factors
    df1$sigma2_u_cat <- as.factor(df1$sigma2_u_cat)
    viz <- df1 %>%
      ggplot(aes(x = delta_00)) +
      geom_point(aes(y = rej_pet_slope,  color = "SMD")) +
      geom_line(aes(y = rej_pet_slope, color ="SMD", linetype = "SMD")) +
      geom_point(aes(y = rej_pet_st_slope, color = "Transformed SMD")) +
      geom_line(aes(y = rej_pet_st_slope, color = "Transformed SMD", linetype = "Transformed SMD")) +
      facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
      geom_hline(yintercept = hline, linetype = "dashed") +
      scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
      scale_y_continuous(limits = limits_bounds,sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
      scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
      scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
      labs(y = type_of_rejection_rate,
           x = "Population SMD",
           caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
      theme_bw()



  return (viz)
}

table_rejection_rate <- function(df, num_studies, bias_type){

  d <- c(0, 0.2, 0.5, 0.8)
  su <- c(0.01, 0.06, 0.11)
  sv <- c(0.01, 0.06, 0.11)
  ss <- c("small", "medium", "large")

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
  } else if (bias_type == "None"){
    bias_type <- "pb_no_orb_no"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt %in% bias_type,
           delta_00 %in% d,
           sigma2_u %in% su,
           sigma2_v %in% sv,
           psss %in% ss,
           sigma2_u == sigma2_v)

  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, rej_pet_slope, rej_pet_st_slope) %>%
    transmute(populationSMD = delta_00,
           sampleSize  = psss,
           heterogeneity = sigma2_u_cat,
           type1Smd = round(rej_pet_slope, 2),
           type1TranSmd = round(rej_pet_st_slope, 2))

  # table_out <- knitr::kable(df_out, "html", align = "c") %>%
  #   kable_paper(full_width = F) %>%
  #   column_spec(1, bold = T) %>%
  #   collapse_rows(columns = 1:2, valign = "top") %>%
  #   scroll_box()

  if(bias_type =="pb_no_orb_no"){
    df_out <- datatable(df_out,
                        filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                        colnames = c("Population SMD", "n", "Heterogeneity", "Type I SMD", "Type I Transf. SMD" ))

  } else if (bias_type !="pb_no_orb_no") {
    df_out <- datatable(df_out,
                        filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                        colnames = c("Population SMD", "n", "Heterogeneity", "Power SMD", "Power Transf. SMD" ))

  }

  return(df_out)

}

viz_rejection_rate_puste <- function(df, num_studies, prob_cens){

  d <- c(0, 0.2, 0.5, 0.8)
  su <- c(0.01, 0.06, 0.11)
  sv <- c(0.01, 0.06, 0.11)
  ss <- c("small", "medium", "large")

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k %in% num_studies,
           delta_00 %in% d,
           sigma2_u %in% su,
           sigma2_v %in% sv,
           psss %in% ss,
           prob_censoring == prob_cens) %>%
    filter(sigma2_u == sigma2_v)

  # Set the correct parameters for the plot
  hline = 0.80
  limits_bounds = c(0, 1)

  viz <- df1 %>%
    ggplot(aes(x = delta_00)) +
    geom_point(aes(y = rej_pet_slope,  color = "SMD")) +
    geom_line(aes(y = rej_pet_slope, color ="SMD", linetype = "SMD")) +
    geom_point(aes(y = rej_pet_st_slope, color = "Transformed SMD")) +
    geom_line(aes(y = rej_pet_st_slope, color = "Transformed SMD", linetype = "Transformed SMD")) +
    facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
    geom_hline(yintercept = hline, linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 0.20, 0.50, 0.80), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
    scale_y_continuous(limits = limits_bounds,sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
    scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
    labs(y = "Power",
         x = "True population SMD",
         caption = sprintf("Meta-analytic dataset size: %s. Probability of censoring: %f.", df1$k_cat, df1$prob_censoring)) +
    theme_bw()

  return(viz)


}

table_rejection_rate_puste <- function(df, num_studies, prob_cens){

  d <- c(0, 0.2, 0.5, 0.8)
  su <- c(0.01, 0.06, 0.11)
  sv <- c(0.01, 0.06, 0.11)
  ss <- c("small", "medium", "large")

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           prob_censoring == prob_cens,
           delta_00 %in% d,
           sigma2_u %in% su,
           sigma2_v %in% sv,
           psss %in% ss,
           sigma2_u == sigma2_v)

  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, rej_pet_slope, rej_pet_st_slope) %>%
    transmute(populationSMD = delta_00,
              sampleSize  = psss,
              heterogeneity = sigma2_u_cat,
              type1Smd = round(rej_pet_slope, 2),
              type1TranSmd = round(rej_pet_st_slope, 2))

  df_out <- datatable(df_out,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Population SMD", "n", "Heterogeneity", "Power SMD", "Power Transf. SMD" ))

  return(df_out)

}

viz_rejection_rate_puste_paper <- function(df, num_studies, ss){
  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           psss == ss,
           sigma2_u == sigma2_v)

  # Set the correct parameters for the plot
  hline = 0.80
  limits_bounds = c(0, 1)

  viz <- df1 %>%
    ggplot(aes(x = prob_censoring)) +
    geom_point(aes(y = rej_pet_slope,  color = "SMD")) +
    geom_line(aes(y = rej_pet_slope, color ="SMD", linetype = "SMD")) +
    geom_point(aes(y = rej_pet_st_slope, color = "Transformed SMD")) +
    geom_line(aes(y = rej_pet_st_slope, color = "Transformed SMD", linetype = "Transformed SMD")) +
    facet_grid(sigma2_u_cat ~ factor(delta_00)) +
    geom_hline(yintercept = hline, linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 0.20, 0.40, 0.60, 0.80, 1), sec.axis = sec_axis(~ . , name = "True population SMD", breaks = NULL, labels = NULL)) +
    scale_y_continuous(limits = limits_bounds,sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
    scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
    labs(y = "Power",
         x = "Probability of censoring",
         caption = sprintf("Meta-analytic dataset size: %s. Primary studies sample size: %s.", df1$k_cat, df1$psss)) +
    theme_bw()

  return(viz)


}

viz_rejection_pet_intercept <- function(df, num_studies, bias_type){

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  viz <- df1 %>%
    ggplot(aes(x = delta_00)) +
    geom_point(aes(y = rej_pet_int,  color = "SMD")) +
    geom_line(aes(y = rej_pet_int, color ="SMD", linetype = "SMD")) +
    geom_point(aes(y = rej_pet_st_int, color = "Transformed SMD")) +
    geom_line(aes(y = rej_pet_st_int, color = "Transformed SMD", linetype = "Transformed SMD")) +
    facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
    geom_hline(yintercept = 0.05, linetype = "dashed") +
    geom_hline(yintercept = 0.80, linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 0.2, 0.5, 0.8), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
    scale_y_continuous(limits=c(0, 1),sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
    scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
    labs(y = "Rejection rate",
         x = "Population SMD",
         caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
    theme_bw()



  return (viz)

}

table_rejection_rate_pet_int <- function(df, num_studies, bias_type){

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
  } else if (bias_type == "None"){
    bias_type <- "pb_no_orb_no"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, rej_pet_slope, rej_pet_st_slope) %>%
    transmute(populationSMD = delta_00,
              sampleSize  = psss,
              heterogeneity = sigma2_u_cat,
              rr_Smd = round(rej_pet_slope, 2),
              rr_TranSmd = round(rej_pet_st_slope, 2))

    df_out <- datatable(df_out,
                        filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                        colnames = c("Population SMD", "n", "Heterogeneity", "Rej.Rate SMD", "Rej.Rate Transf. SMD" ))
    return(df_out)

  }

viz_pwr_pet_int_subset <- function(df){
  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(sigma2_u == 0.01,
           sigma2_v == 0.01,
           prob_censoring == 1,
           k == 70,
           delta_00 %in% c(0.5, 0.8)) %>%
    slice(1:4)

  # For catpion
  p_cens <- 1

  viz <- df1 %>%
    ggplot(aes(x = delta_00)) +
    geom_point(aes(y = rej_pet_int,  color = "SMD")) +
    geom_line(aes(y = rej_pet_int, color ="SMD", linetype = "SMD")) +
    geom_point(aes(y = rej_pet_st_int, color = "Transformed SMD")) +
    geom_line(aes(y = rej_pet_st_int, color = "Transformed SMD", linetype = "Transformed SMD")) +
    facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
    geom_hline(yintercept = 0.80, linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 0.2, 0.5, 0.8), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
    scale_y_continuous(limits=c(0, 1),sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
    scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
    labs(y = "Power",
         x = "Population SMD",
         caption = sprintf("Meta-analytic dataset size: %s. Probability censoring: %d.", df1$k_cat, p_cens)) +
    theme_bw()



  return (viz)
}

table_pet_int_subset <- function(df){
  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(sigma2_u == 0.01,
           sigma2_v == 0.01,
           prob_censoring == 1,
           k == 70,
           delta_00 %in% c(0.5, 0.8)) %>%
    slice(1:4)

  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, rej_pet_int, rej_pet_st_int) %>%
    transmute(populationSMD = delta_00,
              sampleSize  = psss,
              heterogeneity = sigma2_u_cat,
              pwrPetInt = round(rej_pet_int, 2),
              pwrPetIntTr = round(rej_pet_st_int, 2))

  df_out <- datatable(df_out,
                      filter = 'top', options = list(pageLength = 4, autoWidth = TRUE),
                      colnames = c("Population SMD", "n", "Heterogeneity", "Power SMD", "Power Transf. SMD" ))

  return(df_out)

}

table_mse_subset<- function(df){
  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(sigma2_u == 0.01,
           sigma2_v == 0.01,
           prob_censoring == 1,
           k == 70,
           delta_00 %in% c(0.5, 0.8)) %>%
    slice(2:4)


  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, mse_corrected_smd, mse_corrected_st_smd) %>%
    transmute(populationSMD = delta_00,
              sampleSize  = psss,
              heterogeneity = sigma2_u_cat,
              mseSmd = round(mse_corrected_smd, 3),
              mseTranSmd = round(mse_corrected_st_smd, 3))

  df_out <- datatable(df_out,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Population SMD", "n", "Heterogeneity", "MSE SMD", "MSE Transf. SMD" ))

  return(df_out)
}

viz_rr_pet_int <- function(df, num_studies, bias_type){
  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  viz <- df1 %>%
    ggplot(aes(x = delta_00)) +
    geom_point(aes(y = rej_pet_int,  color = "SMD")) +
    geom_line(aes(y = rej_pet_int, color ="SMD", linetype = "SMD")) +
    geom_point(aes(y = rej_pet_st_int, color = "Transformed SMD")) +
    geom_line(aes(y = rej_pet_st_int, color = "Transformed SMD", linetype = "Transformed SMD")) +
    facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
    geom_hline(yintercept = 0.05, linetype = "dashed") +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    scale_x_continuous(breaks = c(0, 0.2, 0.5, 0.8), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
    scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
    labs(y = "Rejection Rate",
         x = "Population SMD",
         caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
    theme_bw()

  return (viz)
}

table_rr_pet_int <- function(df, num_studies, bias_type){
  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, rej_pet_int, rej_pet_st_int) %>%
    transmute(populationSMD = delta_00,
              sampleSize  = psss,
              heterogeneity = sigma2_u_cat,
              rrSmd = round(rej_pet_int, 2),
              rrTranSmd = round(rej_pet_st_int, 2))

  df_out <- datatable(df_out,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Population SMD", "n", "Heterogeneity", "Rej.Rate SMD", "Rej.Rate Transf.SMD" ))

  return(df_out)

}

viz_adj_est_rmse <- function(df, num_studies, bias_type){

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  viz <- df1 %>%
    ggplot(aes(x = delta_00)) +
    geom_point(aes(y = rmse_corrected_smd,  color = "SMD")) +
    geom_line(aes(y = rmse_corrected_smd, color ="SMD", linetype = "SMD")) +
    geom_point(aes(y = rmse_corrected_st_smd, color = "Transformed SMD")) +
    geom_line(aes(y = rmse_corrected_st_smd, color = "Transformed SMD", linetype = "Transformed SMD")) +
    facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
    scale_x_continuous(breaks = c(0, 0.2, 0.5, 0.8), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
    scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
    labs(y = "RMSE",
         x = "Population SMD",
         caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
    theme_bw()

  return (viz)
}

table_adj_est_rmse <- function(df, num_studies, bias_type){
  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, rmse_corrected_smd, rmse_corrected_st_smd) %>%
    transmute(populationSMD = delta_00,
              sampleSize  = psss,
              heterogeneity = sigma2_u_cat,
              rmseSmd = round(rmse_corrected_smd, 3),
              rmseTranSmd = round(rmse_corrected_st_smd, 3))

  df_out <- datatable(df_out,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Population SMD", "n", "Heterogeneity", "RMSE SMD", "RMSE Transf.SMD" ))

  return(df_out)
}

viz_adj_est_bias <- function(df, num_studies, bias_type){

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  viz <- df1 %>%
    ggplot(aes(x = delta_00)) +
    geom_point(aes(y = bias_corrected_smd,  color = "SMD")) +
    geom_line(aes(y = bias_corrected_smd, color ="SMD", linetype = "SMD")) +
    geom_point(aes(y = bias_corrected_st_smd, color = "Transformed SMD")) +
    geom_line(aes(y = bias_corrected_st_smd, color = "Transformed SMD", linetype = "Transformed SMD")) +
    facet_grid(sigma2_u_cat ~ fct_rev(psss)) +
    scale_x_continuous(breaks = c(0, 0.2, 0.5, 0.8), sec.axis = sec_axis(~ . , name = "Primary Studies Sample Size", breaks = NULL, labels = NULL)) +
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Between & within study variance", breaks = NULL, labels = NULL)) +
    scale_color_manual(name = "Effect Size", values = c("SMD" = "#E69F00", "Transformed SMD" = "#0072B2")) +
    scale_linetype_manual(name = "Effect Size", values=c("SMD"="solid", "Transformed SMD"="dotted")) +
    labs(y = "Bias",
         x = "Population SMD",
         caption = sprintf("Meta-analytic dataset size: %s. Bias type: %s.", df1$k_cat, dataset)) +
    theme_bw()

  return (viz)
}

table_adj_est_bias <- function(df, num_studies, bias_type){

  # Convert inputs
  if(bias_type == "ORB Strong"){
    bias_type <- "pb_no_orb_str"
    dataset <- "ORB Strong"
  } else if (bias_type == "ORB Moderate") {
    bias_type <- "pb_no_orb_mod"
    dataset <- "ORB Moderate"
  } else if (bias_type == "PB Strong"){
    bias_type <- "pb_str_orb_no"
    dataset <- "PB Strong"
  } else if(bias_type == "PB Moderate"){
    bias_type <- "pb_mod_orb_no"
    dataset <- "PB Moderate"
  }

  # Select from dataframe only the observations of interest
  df1 <- df %>%
    filter(k == num_studies,
           bt == bias_type,
           sigma2_u == sigma2_v)

  df_out <- df1 %>%
    select(delta_00, psss, sigma2_u_cat, bias_corrected_smd, bias_corrected_st_smd) %>%
    transmute(populationSMD = delta_00,
              sampleSize  = psss,
              heterogeneity = sigma2_u_cat,
              biasSmd = round(bias_corrected_smd, 3),
              biasTranSmd = round(bias_corrected_st_smd, 3))

  df_out <- datatable(df_out,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Population SMD", "n", "Heterogeneity", "Bias SMD", "Bias Transf.SMD" ))

  return(df_out)

}

table_perc_non_conv <- function(df){

  # Add readable bias_type
  df[, 'bt_read'] <- NA

  for (i in 1:dim(df)[1]){
    if(df$bt[i] == "pb_no_orb_no"){
      df$bt_read[i] <- "None"
    } else if(df$bt[i] == "pb_no_orb_str"){
      df$bt_read[i] <- "ORB Strong"
    } else if(df$bt[i] == "pb_no_orb_mod"){
      df$bt_read[i] <- "ORB Moderate"
    } else if(df$bt[i] == "pb_str_orb_no"){
      df$bt_read[i] <- "PB Strong"
    } else if(df$bt[i] == "pb_mod_orb_no"){
      df$bt_read[i] <- "PB Moderate"
    }
  }

  df[, 'su_read'] <- NA
  df[, 'sv_read'] <- NA
  for(i in 1:dim(df)[1]){
    if(df$su[i] == 0.01){
      df$su_read[i] <- "small"
      df$sv_read[i] <- "small"
    } else if(df$su[i] == 0.06){
      df$su_read[i] <- "medium"
      df$sv_read[i] <- "medium"
    } else if(df$su[i] == 0.11){
      df$su_read[i] <- "large"
      df$sv_read[i] <- "medium"
    }
  }

  # Filter df out to keep only su == sv
  df1 <- df %>%
    filter(su == sv) %>%
    select(bt_read, k , d, su_read, sv_read, p,
           pet_smd_perc_non_conv, pet_tr_smd_perc_non_conv,
           peese_smd_perc_non_conv, peese_tr_smd_perc_non_conv)



  df_out <- datatable(df1,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Bias type", "k", "Pop. SMD", "Between-study var.", "Within-study var.", "Primary studies n", "M.Egger SMD", "M.Egger Tr.SMD", "M.PEESE SMD", "M.PEESE Tr.SMD" ))

  return(df_out)


}

table_estimates_check_sd <- function(df){

  # Add readable bias_type
  df[, 'bt_read'] <- NA

  for (i in 1:dim(df)[1]){
    if(df$bt[i] == "pb_no_orb_no"){
      df$bt_read[i] <- "None"
    } else if(df$bt[i] == "pb_no_orb_str"){
      df$bt_read[i] <- "ORB Strong"
    } else if(df$bt[i] == "pb_no_orb_mod"){
      df$bt_read[i] <- "ORB Moderate"
    } else if(df$bt[i] == "pb_str_orb_no"){
      df$bt_read[i] <- "PB Strong"
    } else if(df$bt[i] == "pb_mod_orb_no"){
      df$bt_read[i] <- "PB Moderate"
    }
  }

  df[, 'su_read'] <- NA
  df[, 'sv_read'] <- NA
  for(i in 1:dim(df)[1]){
    if(df$su[i] == 0.01){
      df$su_read[i] <- "small"
      df$sv_read[i] <- "small"
    } else if(df$su[i] == 0.06){
      df$su_read[i] <- "medium"
      df$sv_read[i] <- "medium"
    } else if(df$su[i] == 0.11){
      df$su_read[i] <- "large"
      df$sv_read[i] <- "medium"
    }
  }

  # Filter df out to keep only su == sv
  df1 <- df %>%
    filter(su == sv) %>%
    select(bt_read, k , d, su_read, sv_read, p,
           pet_int_sd, pet_slope_sd, pet_st_int_sd, pet_st_slope_sd,
           peese_int_sd, peese_slope_sd, peese_st_int_sd, peese_st_slope_sd) %>%
    mutate(across(7:14, round, 2))

  df_out <- datatable(df1,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Bias type", "k", "Pop. SMD", "Between-study var.", "Within-study var.", "Primary studies n", "M.Egger SMD Int.", "M.Egger SMD Slo.", "M.Egger Tr.SMD Int.", "M.Egger Tr.SMD Slo.", "M.PEESE SMD Int.", "M.PEESE SMD Slo.", "M.PEESE Tr.SMD Int.", "M.PEESE Tr.SMD Slo." ))

  return(df_out)
}

table_estimates_check_max <- function(df){

  # Add readable bias_type
  df[, 'bt_read'] <- NA

  for (i in 1:dim(df)[1]){
    if(df$bt[i] == "pb_no_orb_no"){
      df$bt_read[i] <- "None"
    } else if(df$bt[i] == "pb_no_orb_str"){
      df$bt_read[i] <- "ORB Strong"
    } else if(df$bt[i] == "pb_no_orb_mod"){
      df$bt_read[i] <- "ORB Moderate"
    } else if(df$bt[i] == "pb_str_orb_no"){
      df$bt_read[i] <- "PB Strong"
    } else if(df$bt[i] == "pb_mod_orb_no"){
      df$bt_read[i] <- "PB Moderate"
    }
  }

  df[, 'su_read'] <- NA
  df[, 'sv_read'] <- NA
  for(i in 1:dim(df)[1]){
    if(df$su[i] == 0.01){
      df$su_read[i] <- "small"
      df$sv_read[i] <- "small"
    } else if(df$su[i] == 0.06){
      df$su_read[i] <- "medium"
      df$sv_read[i] <- "medium"
    } else if(df$su[i] == 0.11){
      df$su_read[i] <- "large"
      df$sv_read[i] <- "medium"
    }
  }

  # Filter df out to keep only su == sv
  df1 <- df %>%
    filter(su == sv) %>%
    select(bt_read, k , d, su_read, sv_read, p,
           abs_pet_int_max, abs_pet_slope_max, abs_pet_st_int_max, abs_pet_st_slope_max,
           abs_peese_int_max, abs_peese_slope_max, abs_peese_st_int_max, abs_peese_st_slope_max) %>%
    mutate(across(7:14, round, 2))

  df_out <- datatable(df1,
                      filter = 'top', options = list(pageLength = 9, autoWidth = TRUE),
                      colnames = c("Bias type", "k", "Pop. SMD", "Between-study var.", "Within-study var.", "Primary studies n", "M.Egger SMD Int.", "M.Egger SMD Slo.", "M.Egger Tr.SMD Int.", "M.Egger Tr.SMD Slo.", "M.PEESE SMD Int.", "M.PEESE SMD Slo.", "M.PEESE Tr.SMD Int.", "M.PEESE Tr.SMD Slo." ))

  return(df_out)
}


bt = "ORB Strong"
k = 15
d = 0
su_sv ="Small"
p = "small"

viz_hist_estimates_megger <- function(bt, k, d, su_sv, p){

  k <- as.numeric(k)

  d <- as.numeric(d)

  # Convert input
  if(bt == "None"){
    bt = "pb_no_orb_no"
  } else if(bt =="ORB Strong"){
    bt = "pb_no_orb_str"
  } else if(bt == "ORB Moderate"){
    bt = "pb_no_orb_mod"
  } else if(bt == "PB Strong"){
    bt = "pb_str_orb_no"
  } else if(bt == "PB Moderate"){
    bt = "pb_mod_orb_no"
  }

  if(su_sv == "Small"){
    su_sv = 0.01
  } else if(su_sv == "Medium"){
    su_sv = 0.06
  } else if(su_sv == "Large"){
    su_sv = 0.11
  }

  su_sv <- as.numeric(su_sv)

  if(p == "Small"){
    p = "small"
  } else if(p == "Medium"){
    p = "medium"
  } else if(p == "Large"){
    p = "large"
  }

  temp0 <- sprintf("%s", bt)
  temp1 <- sprintf("k_%g",k)
  temp2 <- sprintf("pet_results_d%g_su%g_sv%g_%s.csv", d, su_sv, su_sv, p)
  path <- file.path("pet_results",temp0, temp1, temp2)

  df <- read.csv(path)

  megger_int_smd <- ggplot(df) +
    geom_histogram(aes(x=pet_int)) +
    labs(y = "Count",
         x = "Intercept, SMD") +
    theme_bw()

  megger_slope_smd <- ggplot(df) +
    geom_histogram(aes(x=pet_slope)) +
    labs(y = "Count",
         x = "Slope, SMD") +
    theme_bw()

  megger_int_tr_smd <- ggplot(df) +
    geom_histogram(aes(x=pet_st_int)) +
    labs(y = "Count",
         x = "Intercept, Transformed SMD ") +
    theme_bw()

  megger_slope_tr_smd <- ggplot(df) +
    geom_histogram(aes(x=pet_st_slope)) +
    labs(y = "Count",
         x = "Slope, Transformed SMD") +
    theme_bw()



  figure <- ggarrange(megger_int_smd, megger_slope_smd,
                      megger_int_tr_smd, megger_slope_tr_smd,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)

  return(figure)

}

viz_hist_estimates_peese<- function(bt, k, d, su_sv, p){

  k <- as.numeric(k)

  d <- as.numeric(d)

  # Convert input
  if(bt == "None"){
    bt = "pb_no_orb_no"
  } else if(bt =="ORB Strong"){
    bt = "pb_no_orb_str"
  } else if(bt == "ORB Moderate"){
    bt = "pb_no_orb_mod"
  } else if(bt == "PB Strong"){
    bt = "pb_str_orb_no"
  } else if(bt == "PB Moderate"){
    bt = "pb_mod_orb_no"
  }

  if(su_sv == "Small"){
    su_sv = 0.01
  } else if(su_sv == "Medium"){
    su_sv = 0.06
  } else if(su_sv == "Large"){
    su_sv = 0.11
  }

  su_sv <- as.numeric(su_sv)

  if(p == "Small"){
    p = "small"
  } else if(p == "Medium"){
    p = "medium"
  } else if(p == "Large"){
    p = "large"
  }

  temp0 <- sprintf("%s", bt)
  temp1 <- sprintf("k_%g",k)
  temp2 <- sprintf("pet_results_d%g_su%s_sv%s_%s.csv", d, su_sv, su_sv, p)
  path <- file.path("pet_results",temp0, temp1, temp2)

  df <- read.csv(path)

  peese_int_smd <- ggplot(df) +
    geom_histogram(aes(x=peese_int)) +
    labs(y = "Count",
         x = "Intercept, SMD") +
    theme_bw()

   peese_slope_smd <- ggplot(df) +
    geom_histogram(aes(x=peese_slope)) +
    labs(y = "Count",
         x = "Slope, SMD") +
    theme_bw()

  peese_int_tr_smd <- ggplot(df) +
    geom_histogram(aes(x=peese_st_int)) +
    labs(y = "Count",
         x = "Intercept, Transformed SMD ") +
    theme_bw()

  peese_slope_tr_smd <- ggplot(df) +
    geom_histogram(aes(x=peese_st_slope)) +
    labs(y = "Count",
         x = "Slope, Transformed SMD") +
    theme_bw()



  figure <- ggarrange(peese_int_smd, peese_slope_smd,
                      peese_int_tr_smd, peese_slope_tr_smd,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)

  return(figure)

}


table_descriptives <- function(df, bias_type, num_studies, d, su_sv, p){

  num_studies <- as.numeric(num_studies)

  d <- as.numeric(d)

  # Convert input
  if(bias_type == "None"){
    bias_type = "pb_no_orb_no"
  } else if(bias_type =="ORB Strong"){
    bias_type = "pb_no_orb_str"
  } else if(bias_type == "ORB Moderate"){
    bias_type = "pb_no_orb_mod"
  } else if(bias_type == "PB Strong"){
    bias_type = "pb_str_orb_no"
  } else if(bias_type == "PB Moderate"){
    bias_type = "pb_mod_orb_no"
  }

  if(su_sv == "Small"){
    su_sv = 0.01
  } else if(su_sv == "Medium"){
    su_sv = 0.06
  } else if(su_sv == "Large"){
    su_sv = 0.11
  }

  su_sv <- as.numeric(su_sv)

  if(p == "Small"){
    p = "small"
  } else if(p == "Medium"){
    p = "medium"
  } else if(p == "Large"){
    p = "large"
  }

  df2 <- df %>%
    filter(bt == bias_type,
           k == num_studies,
           delta_00 == d,
           sigma2_u == su_sv,
           sigma2_v == su_sv,
           psss == p) %>%
    select(avg_num_study_selected,
           avg_num_out_selected,
           avg_perc_out_excluded) %>%
    transmute(avgNumberStudies = round(avg_num_study_selected),
              avgNumberOutcomes = round(avg_num_out_selected),
              avgPercentExcluded = round(avg_perc_out_excluded))



  return(df2)
}

