# Load packages
library(future.apply)

# Load functions
source("R/cdf_t.R")
source("R/integrand_t.R")
source("R/posterior_t.R")
source("R/power_optim.R")
source("R/ssp_rope.R")
source("R/ssp_tost.R")

## Create fail safe ROPE function
ssp_rope_safe <- function(opt, band, delta) {
  out <- tryCatch(
    {
      r <- ssp_rope(opt = opt, band = band, delta = delta)
      
      list(result = r, error = NULL)
    },
    error = function(e) {
      
      list(result = NULL, error = e)
    }
  )
  return(out)
}

# Create directory to save the results in
ifelse(!dir.exists("./rope-res"),
       dir.create("./rope-res"),
       "Directory already exists.")

# Get the number of cores on the machine
availableCores()

# Set the number of cores that you want to allocate here
n_cores <- 2

# Make a future plan for one multicore machine
plan(multisession, workers = n_cores)

# Create datatable storing possible ROPE options
rope_options <- 
  expand.grid(
    power = seq(0.5, 0.95, by = 0.01),
    delta = seq(0, 2, by = 0.05),
    band = seq(0.1, 0.5, by = 0.01)
  )

rope_options <- rope_options[order(rope_options[, 1], rope_options[, 2]), ]

rope_options$n_iterate <- 1:nrow(rope_options)

# Slice the part of the options that are assigned to this machine
set <- rope_options[4001:60000, ]

saveRDS(set, "./rope-res/all-set.rds")

# Split the data for each iteration
set_split <- split(set, set$n_iterate)

# Run the calculation
## As a safety net after every 100 calculations we save the data and empty the memory.
## Init variables for the loop
n_saves <- length(set_split)  / 100
init <- 1

## Run the calculations
for (i in 1:n_saves) {
  # Slice the data
  slice_n <- i * 100
  
  set_split_sliced <- set_split[init:slice_n]
  
  init <- slice_n + 1
  
  # Calculate ROPE
  rope_res <-  future.apply::future_lapply(set_split_sliced, function(x) ssp_rope_safe(opt = x$power, band = x$band, delta = x$delta))
  
  # Saving data
  saveRDS(rope_res, paste0("./rope-res/set-", i, ".rds"))
  
  # Remove object
  rm(rope_res)
  
  # Empty memory
  gc()
}
