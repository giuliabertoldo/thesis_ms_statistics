library("readr")

# source("r_scripts/performance_analyzer.R")
debugSource("r_scripts/performance_analyzer.R")

##############################################################
# Parameters
##############################################################

#base_directory <- getwd()
set.seed(9)

# Read csv with conditions
conditions <- read_csv(file = 'conditions/test_pb_mod_orb_mod_k_15.csv')

# Get 1 condition
# Each processor gets a chunck of conditions and idx is one row of this chunck
local_condition = conditions[1,]

# Decompose the row to retrieve the condition's values
bias_type <- local_condition[[1]]
delta_00 <- local_condition[[2]]
num_studies <- local_condition[[3]]
sigma2_u <- local_condition[[4]]
sigma2_v <- local_condition[[5]]
psss <- local_condition[[6]]
o <- local_condition[[7]]

performance_analyzer(bias_type = bias_type, delta_00 = delta_00,sigma2_u = sigma2_u, sigma2_v = sigma2_v, psss = psss, o = o, k = num_studies, nmeta = 10, verbose = FALSE) # tol = 1e-3, iter = 50,
