create_directories <- function(num_studies, delta_00, sigma2_u, sigma2_v, psss) {

  if (!file.exists("data")) { save_path <- dir.create("data") }

  for (k in num_studies) {

    if (!file.exists(sprintf("data/k_%d",k))) {
        dir.create(file.path("data", sprintf("k_%d", k)))
    }

    for (d in delta_00) {

      for (su in sigma2_u) {

        for (sv in sigma2_v) {

          for (p in psss) {
            file_name <- sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p)

            if (!file.exists(file.path(sprintf("data/k_%d",k), file_name))) {
              dir.create(file.path(sprintf("data/k_%d",k), file_name))

          }
          }
        }
      }
    }
  }
}

parameters_to_list <- function(delta_00, sigma2_u, sigma2_v, psss) {
  temp <- vector(mode = "list",length = length(delta_00)*length(sigma2_u)*length(sigma2_v)*length(psss))
  idx <- 1
  for (d in delta_00) {
    for (su in sigma2_u) {
      for (sv in sigma2_v) {
        for (p in psss) {
          temp[[idx]] <- list(d, su, sv, p)
          idx <- idx + 1
        }
      }
    }
  }
  return(temp)

}

biased_parameters_to_list <- function(delta_00, sigma2_u, sigma2_v, psss, bias_type) {

  temp <- vector(mode = "list",length = length(delta_00)*length(sigma2_u)*length(sigma2_v)*length(psss)*length(bias_type))
  idx <- 1
  for (d in delta_00) {

    for (su in sigma2_u) {

      for (sv in sigma2_v) {

        for (p in psss) {

          for (bt in bias_type) {

            temp[[idx]] <- list(d, su, sv, p, bt)

            idx <- idx + 1

          }
        }
      }
    }
  }

  return(temp)

}

create_biased_directories <- function(num_studies, delta_00, sigma2_u, sigma2_v, psss, bias_type) {

  for (bt in bias_type){
    if (!file.exists(sprintf("data_%s", bt))) {save_path <- dir.create(sprintf("data_%s", bt)) }

    for (k in num_studies) {

      if (!file.exists(sprintf("data_%s/k_%d",bt, k))) {
        dir.create(file.path(sprintf("data_%s", bt), sprintf("k_%d", k)))
      }

      for (d in delta_00) {

        for (su in sigma2_u) {

          for (sv in sigma2_v) {

            for (p in psss) {
              file_name <- sprintf("d%0.2f_su%0.2f_sv%0.2f_%s", d, su, sv, p)

              if (!file.exists(file.path(sprintf("data_%s/k_%d",bt,k), file_name))) {
                dir.create(file.path(sprintf("data_%s/k_%d",bt,k), file_name))

              }
            }
          }
        }
      }
    }

  }
}
