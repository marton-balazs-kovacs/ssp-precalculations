# Load packages
library(purrr)
library(dplyr)
library(furrr)
library(tidyr)
library(readr)

# Load functions
r_scripts <- list.files("R/", full.names = TRUE)
walk(r_scripts, source)

## Create fail safe ROPE function
ssp_rope_safe <- purrr::safely(ssp_rope)

# Create directory to save the results in
ifelse(!dir.exists("./rope-res"),
       dir.create("./rope-res"),
       "Directory already exists.")

# Get the number of cores on the machine
availableCores()

# Make a future plan for one multicore machine
plan(multisession, workers = availableCores() - 1)

# Create datatable storing possible ROPE options
rope_options <- 
  tidyr::expand_grid(
    power = seq(0.5, 0.95, by = 0.01),
    delta = seq(0, 2, by = 0.05),
    band = seq(0.1, 0.5, by = 0.01)
  ) %>% 
  mutate(n_iterate = 1:n())

# Slice the part of the options that are assigned to this machine
set <- 
  rope_options %>% 
  slice(1:3000)

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
  
  set_split_sliced <- 
    set_split %>% 
    slice(init:slice_n)
  
  init <- slice_n + 1
  
  # Calculate ROPE
  rope_res <-  furrr::future_map(set_split_sliced,
                                  ~ ssp_rope_safe(opt = .$power,
                                                  band = .$band,
                                                  delta = .$delta))
  
  # Saving data
  write_rds(rope_res, paste0("./rope-res/set-", i, ".rds"))
  
  # Empty memory
  gc()
}
