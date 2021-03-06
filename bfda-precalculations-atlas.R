# Load packages
library(future.apply)

# Load functions
source("R/ssp_bfda.R")

## Create fail safe ROPE function
ssp_bfda_safe <- function(tpr, delta, thresh) {
  out <- tryCatch(
    {
      r <- ssp_bfda(tpr = tpr, delta = delta, thresh = thresh)
      
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
    thresh = c(3, 6, 10)
  )

bfda_options$iterate <- 1:nrow(bfda_options)

# Split the data for each iteration
bfda_options_split <- split(bfda_options, bfda_options$iterate)

# Run the calculation
## As a safety net after every 100 calculations we save the data and empty the memory
## Init variables for the loop
n_saves <- length(bfda_options_split) / 100
init <- 1

## Run the calculations
for (i in 1:n_saves) {
  # Slice the data
  slice_n <- i * 100
  
  bfda_options_sliced <- bfda_options_split[init:slice_n]
  
  init <- slice_n + 1
  
  # Calculate ROPE
  bfda_res <-  future.apply::future_lapply(bfda_options_sliced, function(x) ssp_bfda_safe(tpr = x$tpr, delta = x$delta, thresh = x$thresh))
  
  # Saving data
  saveRDS(bfda_res, paste0("./bfda-res/set-", i, ".rds"))
  
  # Remove object
  rm(bfda_res)
  
  # Empty memory
  gc()
}
