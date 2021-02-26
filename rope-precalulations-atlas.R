# Load packages
library(future.apply)
library(readr)

# Load functions
source("R/cdf_t.R")
source("R/integrand_t.R")
source("R/posterior_t.R")
source("R/power_optim.R")
source("R/ssp_rope.R")
source("R/ssp_tost.R")

## Create fail safe ROPE function
ssp_rope_safe <- function(tpr, eq_band, delta) {
  out <- tryCatch(
    {
      r <- ssp_rope(tpr = tpr, eq_band = eq_band, delta = delta)
      
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
set <- readr::read_csv("rope_data_rerun.csv")

# Split the data for each iteration
set_split <- split(set, set$iterate)

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
  rope_res <-  future.apply::future_lapply(set_split_sliced, function(x) ssp_rope_safe(tpr = x$power, eq_band = x$band, delta = x$delta))
  
  # Saving data
  saveRDS(rope_res, paste0("./rope-res/set-", i, ".rds"))
  
  # Remove object
  rm(rope_res)
  
  # Empty memory
  gc()
}
