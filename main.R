library(gplots)
library(yaml)

sink("outputs/process.log")

timestamp()

# Read configuration parameters from the YAML file
config.dat <- read_yaml("config_CoLab_factors.yml")

base <- config.dat$base
maxval <- config.dat$maxval
palette <- colorRampPalette(config.dat$palette)(100)
factors <- config.dat$factors
countries <- config.dat$countries
use_empirical_prior <- config.dat$use_empirical_prior

"Making global context map..."
ptm <- proc.time()
source("make_global_map.R")
proc.time() - ptm

"Making factor maps..."
ptm <- proc.time()
source("make_factor_maps.R")
proc.time() - ptm

"Making country context maps..."
ptm <- proc.time()
source("make_context_maps.R")
proc.time() - ptm

sink()
