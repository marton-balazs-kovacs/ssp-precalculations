# Load packages
library(future.apply)
library(purrr)

# Load functions
source("R/ssp_infer_bf.R")
source("R/tpr_optim.R")
source("R/integrand_t.R")
source("R/cdf_t.R")
source("R/posterior_t.R")
source("R/bf10_t.R")

## Create fail safe infer-bf function
ssp_infer_bf_safe <- function(tpr, delta, thresh, ni_margin, prior_scale, iterate) {
  out <- tryCatch(
    {
      r <- ssp_infer_bf(tpr = tpr, ni_margin = ni_margin, delta = delta, thresh = thresh, prior_scale = prior_scale)
      
      list(
        result = r,
        error = NULL,
        params = list(
          tpr = tpr,
          ni_margin = ni_margin,
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
          ni_margin = ni_margin,
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
ifelse(!dir.exists("./infer-bf-res"),
       dir.create("./infer-bf-res"),
       "Directory already exists.")

# Get the number of cores on the machine
availableCores()

# Set the number of cores that you want to allocate here
n_cores <- availableCores() - 1

# Make a future plan for one multicore machine
plan(multisession, workers = n_cores)

# Create datatable storing possible infer-bf options
infer_bf_options <-
  expand.grid(
    tpr = seq(0.5, 0.95, by = 0.05),
    ni_margin = seq(0, 0.5, by = 0.05),
    delta = seq(-0.5, 1, by = 0.05),
    thresh = c(3, 6, 10),
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  )

infer_bf_options$iterate <- 1:nrow(infer_bf_options)

# Split the data for each iteration
infer_bf_options_split <- split(infer_bf_options, infer_bf_options$iterate)

# Run the calculation
## As a safety net after every 100 calculations we save the data and empty the memory
## Init variables for the loop
n_saves <- length(infer_bf_options_split) %/% 100 + 1
init <- 1

## Run the calculations
for (i in 1:n_saves) {
  # Print the current iteration
  print(paste("The", i, "iteration is running currently."))
  
  # Slice the data
  if (i == n_saves) {
    slice_n <- length(infer_bf_options_split)
  } else {
    slice_n <- i * 100
  }
  
  infer_bf_options_sliced <- infer_bf_options_split[init:slice_n]
  
  init <- slice_n + 1
  
  # Calculate Non-inferiority Bayes factor
  infer_bf_res <-  future.apply::future_lapply(infer_bf_options_sliced, function(x) ssp_infer_bf_safe(tpr = x$tpr, ni_margin = x$ni_margin, delta = x$delta, thresh = x$thresh, prior_scale = x$prior_scale, iterate = x$iterate))
  
  # Saving data
  saveRDS(infer_bf_res, paste0("./infer-bf-res/set-", i, ".rds"))
  
  # Remove object
  rm(infer_bf_res)
  
  # Empty memory
  gc()
}
