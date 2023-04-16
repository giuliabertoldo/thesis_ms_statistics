args <- commandArgs(TRUE)
idx <- args[1]

library("readr")
library("metafor")
source("r_scripts/biased_metas_gen_with_pets.R")
source("r_scripts/performance_analyzer.R")
source("r_scripts/list_to_dataframe.R")
source("r_scripts/pets.R")
source("r_scripts/orb.R")
source("r_scripts/pb.R")
source("r_scripts/pb_orb.R")
source("r_scripts/prob_inclusion.R")

source("r_scripts/performance_analyzer.R")
# debugSource("performance_analyzer.R")


##############################################################
# Parameters
##############################################################

set.seed(9)

##############################################################
# functions
##############################################################

# Read csv with conditions
conditions <- read_csv(file = 'conditions/pb_str_orb_str_k_70.csv')

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

performance_analyzer(bias_type = bias_type, delta_00 = delta_00,sigma2_u = sigma2_u, sigma2_v = sigma2_v, psss = psss, o = o, k = num_studies, nmeta = 1000, verbose = FALSE) # tol = 1e-3, iter = 50,


sink("dumper")
##############################################################


