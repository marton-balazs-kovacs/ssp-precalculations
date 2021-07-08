# Load packages
library(future.apply)
library(purrr)

# Load functions
source("R/cdf_t.R")
source("R/integrand_t.R")
source("R/posterior_t.R")
source("R/power_optim.R")
source("R/ssp_rope.R")
source("R/ssp_tost.R")

## Create fail safe ROPE function
ssp_rope_safe <- function(tpr, eq_band, delta, prior_scale) {
  out <- tryCatch(
    {
      r <- ssp_rope(tpr = tpr, eq_band = eq_band, delta = delta, prior_scale = prior_scale)
      
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
n_cores <- availableCores() - 1

# Make a future plan for one multicore machine
plan(multisession, workers = n_cores)

# Create datatable storing possible ROPE options
rope_options <- 
  expand.grid(
    tpr = seq(0.5, 0.95, by = 0.05),
    delta = seq(0, 2, by = 0.05),
    eq_band = seq(0.1, 0.5, by = 0.05),
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  )

rope_options$iterate <- 1:nrow(rope_options)

# Split the data for each iteration
rope_options_split <- split(rope_options, rope_options$iterate)

# Run the calculation
## As a safety net after every 100 calculations we save the data and empty the memory.
## Init variables for the loop
n_saves <- length(rope_options_split)  / 100
init <- 1

## Run the calculations
for (i in 1:n_saves) {
  # Print the current iteration
  print(paste("The", i, "iteration is running currently."))
  
  # Slice the data
  slice_n <- i * 100
  
  rope_options_sliced <- rope_options_split[init:slice_n]
  
  init <- slice_n + 1
  
  # Calculate ROPE
  rope_res <-  future.apply::future_lapply(rope_options_sliced, function(x) ssp_rope_safe(tpr = x$tpr, eq_band = x$eq_band, delta = x$delta, prior_scale = x$prior_scale))
  
  # Saving data
  saveRDS(rope_res, paste0("./rope-res/set-", i, ".rds"))
  
  # Remove object
  rm(rope_res)
  
  # Empty memory
  gc()
}
