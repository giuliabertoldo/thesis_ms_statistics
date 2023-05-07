# Explore the distributions from which primary studies' sample sizes are sampled
# Based on Van den Noortgate 2015 to get distributions with mean similar to Fernandez-Castilla 2021
# but more right skewed

# Log normal distribution
get_quartiles <- function(n, meanlog, sdlog) {
  values <- 6 + rlnorm(n = n, meanlog = meanlog, sdlog =sdlog)
  summary <- round(summary(values))
  q1 <- round(6 + qlnorm(p = 0.25, meanlog = meanlog, sdlog = sdlog))
  q2 <- round(6 + qlnorm(p = 0.50, meanlog = meanlog, sdlog = sdlog))
  q3 <- round(6 + qlnorm(p = 0.75, meanlog = meanlog, sdlog = sdlog))

  return(c(summary, "q1:", q1, "q2:", q2, "q3:",q3))
}

# Small sample size
get_quartiles(n = 10000, meanlog = 3.5, sd = 0.5)

# Medium sample size
get_quartiles(n = 10000, meanlog = 4.5, sd = 0.5)

# Large sample size
get_quartiles(n = 10000, meanlog = 5, sd = 0.5)


