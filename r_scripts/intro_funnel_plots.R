
# Load pacakges
library(metafor)

# Load data
# Source: https://www.metafor-project.org/doku.php/analyses:raudenbush1985
dat <- dat.raudenbush1985
View(dat)

# Fit FEM
fem <- rma(yi, vi, data=dat, measure="SMD", method="FE")

# Funnel plot
# Source: https://www.metafor-project.org/doku.php/plots:funnel_plot_variations
funnel(fem)
