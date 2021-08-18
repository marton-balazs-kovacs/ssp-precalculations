# Load packages
library(future.apply)
library(purrr)

# Load functions
source("R/ssp_eq_bf.R")
source("R/ssp_tost.R")
source("R/tpr_optim.R")
source("R/cdf_t.R")
source("R/integrand_t.R")
source("R/posterior_t.R")

## Create fail safe eq-bf function
ssp_eq_bf_safe <- function(tpr, delta, thresh, eq_band, prior_scale, iterate) {
  out <- tryCatch(
    {
      r <- ssp_eq_bf(tpr = tpr, eq_band = eq_band, delta = delta, thresh = thresh, prior_scale = prior_scale)
      
      list(
        result = r,
        error = NULL,
        params = list(
          tpr = tpr,
          eq_band = eq_band,
          delta = delta,
          thresh = thresh,
          prior_scale = prior_scale,
          iterate = iterate
          )
        )
    },
    error = function(e) {
      
      list(
        result = NULL,
        error = e,
        params = list(
          tpr = tpr,
          eq_band = eq_band,
          delta = delta,
          thresh = thresh,
          prior_scale = prior_scale,
          iterate = iterate
        ))
    }
  )
  return(out)
}

# Create directory to save the results in
ifelse(!dir.exists("./eq-bf-res"),
       dir.create("./eq-bf-res"),
       "Directory already exists.")

# Get the number of cores on the machine
availableCores()

# Set the number of cores that you want to allocate here
n_cores <- availableCores() - 1

# Make a future plan for one multicore machine
plan(multisession, workers = n_cores)

# Create datatable storing possible eq-bf options
eq_bf_options <- 
  expand.grid(
    tpr = seq(0.5, 0.95, by = 0.05),
    eq_band = seq(0.1, 0.5, by = 0.05),
    delta = seq(0, 2, by = 0.05),
    thresh = c(3, 6, 10),
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  )

eq_bf_options$iterate <- 1:nrow(eq_bf_options)

# Split the data for each iteration
eq_bf_options_split <- split(eq_bf_options, eq_bf_options$iterate)

# Run the calculation
## As a safety net after every 100 calculations we save the data and empty the memory
## Init variables for the loop
n_saves <- length(eq_bf_options_split) / 100
init <- 1

## Run the calculations
for (i in 1:n_saves) {
  # Print the current iteration
  print(paste("The", i, "iteration is running currently."))
  
  # Slice the data
  slice_n <- i * 100
  
  eq_bf_options_sliced <- eq_bf_options_split[init:slice_n]
  
  init <- slice_n + 1
  
  # Calculate Equivalence Bayes factor
  eq_bf_res <-  future.apply::future_lapply(eq_bf_options_sliced, function(x) ssp_eq_bf_safe(tpr = x$tpr, eq_band = x$eq_band, delta = x$delta, thresh = x$thresh, prior_scale = x$prior_scale, iterate = x$iterate))
  
  # Saving data
  saveRDS(eq_bf_res, paste0("./eq-bf-res/set-", i, ".rds"))
  
  # Remove object
  rm(eq_bf_res)
  
  # Empty memory
  gc()
}
