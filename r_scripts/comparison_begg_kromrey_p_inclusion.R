# Goal: Check if the probability of inclusions proposed by
# Begg & Mazumdar (1994) and Kromrey & Rendina-Gobioff (2006)
# are equivalent

library(ggplot2)

## Moderate bias
krom_mod <- function(p){
  p_inclusion <- exp(-2*p^1.5)
  return(p_inclusion)
}

beg_mod <- function(p){
  p_inclusion <- exp(-4*p^3)
  return(p_inclusion)
}

## Strong bias
krom_strong <- function(p){
  p_inclusion <- exp(-4*p^1.5)
  return(p_inclusion)
}

beg_strong <- function(p){
  p_inclusion <- exp(-4*p^1.5)
  return(p_inclusion)
}

# Build dataframe
## P-values
p_val <- seq(from = 0, to = 1, by = 0.01)
## Probability of inclusions for the two methods in the two conditions (moderate, strong)
p_incl_krom_mod <- krom_mod(p_val)
p_incl_begg_mod <- beg_mod(p_val)
p_incl_krom_strong <- krom_strong(p_val)
p_incl_begg_strong <- beg_strong(p_val)
## Create dataframe
df <- as.data.frame(cbind(p_val, p_incl_krom_mod,p_incl_begg_mod, p_incl_krom_strong, p_incl_begg_strong))

## Plot curves
ggplot(data = df, aes(x = p_val)) +
  geom_line(aes(y = p_incl_krom_mod, color = "Kromrey: Moderate bias")) +
  geom_line(aes(y = p_incl_begg_mod, color = "Begg: Moderate bias")) +
  geom_line(aes(y = p_incl_krom_strong, color = "Kromrey: Strong bias")) +
  geom_line(aes(y = p_incl_begg_strong, color = "Begg: Strong bias")) +
  labs(y = "Probability of inclusion",
       x = "P-value",
       color = "Bias")

## Conclusion:
# (1) When bias is strong, the two methods give the same results
# (2) When bias is moderate:
## (2a) p-values lower than ~0.60 have a higher probability to be included in Begg compared to Kromrey
## (2b) p-values larger than ~0.60 have a much lower probability to be included in Begg compared to Kromrey
###### reaching a point where for high p-values there is no difference between the moderate and strong publication bias condition

# To keep the moderate and strong publication bias conditions more clearly separated, Kromrey's approach
# may be better because in the moderate bias condition still allows for p-values (~ > .90) to be included

# Graph with Kromrey only:
ggplot(data = df, aes(x = p_val)) +
  geom_line(aes(y = p_incl_krom_mod, color = "Moderate bias")) +
  geom_line(aes(y = p_incl_krom_strong, color = "Strong bias")) +
  labs(y = "Probability of inclusion",
       x = "P-value",
       color = "Bias degree") +
  theme_bw()
