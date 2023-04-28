

# source("r_scripts/performance_analyzer.R")
debugSource("r_scripts/performance_analyzer.R")

##############################################################
# Parameters
##############################################################

#base_directory <- getwd()
set.seed(9)

# Decompose the row to retrieve the condition's values
bias_type <- "pb_no_orb_no"
delta_00 <- 0
num_studies <- 15
sigma2_u <- 0.01
sigma2_v <- 0.01
psss <- "small"
o <- 3

performance_analyzer(bias_type = bias_type, delta_00 = delta_00,sigma2_u = sigma2_u, sigma2_v = sigma2_v, psss = psss, o = o, k = num_studies, nmeta = 250, verbose = 3) # , tol = 1e-3, iter = 50,
