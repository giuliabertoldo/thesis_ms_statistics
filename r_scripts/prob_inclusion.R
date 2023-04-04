# Moderate bias
bias_moderate <- function(p){
  p_inclusion <- exp(-2*p^1.5)
  return(p_inclusion)
}

# Strong bias
bias_strong <- function(p){
  p_inclusion <- exp(-4*p^1.5)
  return(p_inclusion)
}



