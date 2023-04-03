# Convert list of lists into a dataframe

list_to_dataframe <- function(studies, k, o){
  ## Create empty dataframe
  df <- data.frame(matrix(nrow=(k*o), ncol = 17))
  ## Set column names
  colnames(df) <- names(studies[[1]][[1]])
  ## Add study_id and out_id
  df <- cbind(out_id = NA, df)
  df <- cbind(study_id = NA, df)

  # Initialize line counter
  line_index <- 1

  # Fill in dataframe using values in the list
  for(j in 1:k){
    for(i in 1:o){
      df[line_index,3:19] <- studies[[j]][[i]]
      df[line_index,'study_id'] <- j
      df[line_index,'out_id'] <- i
      line_index <- line_index + 1
    }
  }
  return(df)
}

# # Example
# source('outcome_gen.r')
# outcomes = list()
# studies = list()
# for(j in 1:2){      # loop for k studies
#   studies[[j]] <- outcome_gen(delta_00 = 0.8, sigma2_v = 0.11, sigma2_u = 0.11, o = 3, psss="small")
#  }
#
# df1 <- list_to_dataframe(studies = studies, k = 2, o = 3)
# View(df1)
