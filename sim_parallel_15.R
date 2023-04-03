library(parallel)
library("readr")

source("performance_analyzer.R")
# debugSource("performance_analyzer.R")
source("utilities.R")
# debugSource("utilities.R")
source('metas_gen_with_pets.R')
# debugSource('metas_gen_with_pets.R')

##############################################################
# Parameters
##############################################################

#base_directory <- getwd()
input_idx <- 1:3
set.seed(9)

##############################################################
# functions
##############################################################
fun1 <- function(idx){
  # Read csv with conditions
  conditions <- read_csv(file = 'pb_no_orb_no_0.2_15_0.1_0.1.csv')

  # Each processor gets a chunck of conditions and idx is one row of this chunck
  local_condition = conditions[idx,]

  # Decompose the row to retrieve the condition's values
  bias_type <- local_condition[[1]]
  delta_00 <- local_condition[[2]]
  num_studies <- local_condition[[3]]
  sigma2_u <- local_condition[[4]]
  sigma2_v <- local_condition[[5]]
  psss <- local_condition[[6]]
  o <- local_condition[[7]]

  # Check that all cores actually get the conditions
  # output <- c(delta_00, num_studies, sigma2_u, sigma2_v, psss, o )
  # write.csv(output, file=sprintf('p_%d.csv',Sys.getpid()))

  performance_analyzer(bias_type = bias_type, delta_00 = delta_00,sigma2_u = sigma2_u, sigma2_v = sigma2_v, psss = psss, o = o, k = num_studies, nmeta = 10, verbose = FALSE) # tol = 1e-3, iter = 50,

}

sink("dumper")
##############################################################
# gets the number of cores and creates a cluster
num_cores <- detectCores()
print('num cores:')
print(num_cores)

cl <- makeCluster(num_cores)
clusterEvalQ(cl, library("readr"))
clusterEvalQ(cl, source("metas_gen_with_pets.R"))
clusterEvalQ(cl, source("performance_analyzer.R"))
clusterEvalQ(cl, source("outcome_gen.R"))
clusterEvalQ(cl, source("list_to_dataframe.R"))
clusterEvalQ(cl, source("pets.R"))
clusterEvalQ(cl, library("metafor"))

performances <- parLapply(cl, input_idx, fun1, chunk.size = 1)


stopCluster(cl)


