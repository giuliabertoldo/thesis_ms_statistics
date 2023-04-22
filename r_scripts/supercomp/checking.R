# Find which pets did not converge, given a specific error file
errors[errors$pet== 1 | errors$peese== 1 | errors$pet_st== 1 | errors$peese_st == 1, ]


# pb_no_orb_str
# k = 15

load("data/pb_no_orb_str/k_15/d0.00_su0.01_sv0.01_large/biased_meta1.Rdata")

aggregate(out_id ~ study_id), df_biased), 
function(x) cbind(count=length(x), avg_delay=mean(x, na.rm = TRUE)),
na.action = NULL
# Find how many Studies and how many effects per study there are 

subset(aggregate(dep_delay ~ day + month, flights, 
                 function(x) cbind(count=length(x), avg_delay=mean(x, na.rm = TRUE)),
                 na.action = NULL), 
       dep_delay[,1] > 1000)