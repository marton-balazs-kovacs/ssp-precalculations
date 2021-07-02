# Load packages
library(future.apply)

# Load functions
source("R/ssp_bfda.R")

## Create fail safe ROPE function
ssp_bfda_safe <- function(tpr, delta, thresh, prior_scale) {
  out <- tryCatch(
    {
      r <- ssp_bfda(tpr = tpr, delta = delta, thresh = thresh, prior_scale = prior_scale)
      
      list(result = r, error = NULL)
    },
    error = function(e) {
      
      list(result = NULL, error = e)
    }
  )
  return(out)
}

# Create directory to save the results in
ifelse(!dir.exists("./bfda-res"),
       dir.create("./bfda-res"),
       "Directory already exists.")

# Get the number of cores on the machine
availableCores()

# Set the number of cores that you want to allocate here
n_cores <- availableCores() - 1

# Make a future plan for one multicore machine
plan(multisession, workers = n_cores)

# Create datatable storing possible BFDA options
bfda_options <- 
  expand.grid(
    tpr = seq(0.5, 0.95, by = 0.05),
    delta = seq(0, 2, by = 0.05),
    thresh = c(3, 6, 10),
    # We do not recalculate the results with medium scale because we already did that
    prior_scale = c(1, sqrt(2))
  )

# We start the iteration from 1231 as we already calculated the first 1230 iterations with medium prior
bfda_options$iterate <- as.numeric(rownames(bfda_options)) + 1230

# Split the data for each iteration
bfda_options_split <- split(bfda_options, bfda_options$iterate)

# Run the calculation
## As a safety net after every 100 calculations we save the data and empty the memory
## Init variables for the loop
n_saves <- length(bfda_options_split) / 100
init <- 1

## Run the calculations
for (i in 1:n_saves) {
  # Print the current iteration
  print(paste("The", i, "iteration is running currently."))
  
  # Slice the data
  slice_n <- i * 100

  bfda_options_sliced <- bfda_options_split[init:slice_n]

  init <- slice_n + 1

  # Calculate BFDA
  bfda_res <-  future.apply::future_lapply(bfda_options_sliced, function(x) ssp_bfda_safe(tpr = x$tpr, delta = x$delta, thresh = x$thresh, prior_scale = x$prior_scale))

  # Saving data
  saveRDS(bfda_res, paste0("./bfda-res/set-", i, ".rds"))

  # Remove object
  rm(bfda_res)

  # Empty memory
  gc()
}
