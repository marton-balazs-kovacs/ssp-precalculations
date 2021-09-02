# I made a mistake in the precalculation code and it only calculated
# number of options that are dividable with a 100 so I need to
# supplement the remaining precalculations
# Load packages
library(future.apply)
library(tibble)
library(purrr)
library(dplyr)
library(tidyr)

# Load functions
source("R/ssp_bfda.R")

## Create fail safe BFDA function
ssp_bfda_safe <- function(tpr, delta, thresh, prior_scale, iterate) {
  out <- tryCatch(
    {
      r <- ssp_bfda(tpr = tpr, delta = delta, thresh = thresh, prior_scale = prior_scale)
      
      list(
        result = r,
        error = NULL,
        params = list(
          tpr = tpr,
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
          delta = delta,
          thresh = thresh,
          prior_scale = prior_scale,
          iterate = iterate
        )
      )
    }
  )
  return(out)
}

# Create directory to save the results in
ifelse(!dir.exists("./bfda-res-remaining"),
       dir.create("./bfda-res-remaining"),
       "Directory already exists.")

# Read the outputs of all iterations
bfda_data <- 
  tibble(filename = list.files(path = "./bfda-res/", pattern = ".rds")) %>%
  mutate(file = here::here("bfda-res", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(params = map(data, "params"),
         tpr = map_dbl(params, ~ pluck(.x, "tpr", .default = NA_real_)),
         delta = map_dbl(params, ~ pluck(.x, "delta", .default = NA_real_)),
         thresh = map_dbl(params, ~ pluck(.x, "thresh", .default = NA_real_)),
         prior_scale = map_dbl(params, ~ pluck(.x, "prior_scale", .default = NA_real_))) %>% 
  select(-file, -filename, -params)

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
    prior_scale = c(1 / sqrt(2), 1, sqrt(2))
  )

bfda_options_remaining <-
  bfda_options %>% 
  anti_join(., bfda_data, by = c("tpr", "delta", "thresh", "prior_scale"))

bfda_options_remaining$iterate <- as.numeric(rownames(bfda_options_remaining)) + 3600

# Split the data for each iteration
bfda_options_remaining_split <- split(bfda_options_remaining, bfda_options_remaining$iterate)

# Calculate BFDA
bfda_res <-  future.apply::future_lapply(bfda_options_remaining_split, function(x) ssp_bfda_safe(tpr = x$tpr, delta = x$delta, thresh = x$thresh, prior_scale = x$prior_scale, iterate = x$iterate))

# Saving data
saveRDS(bfda_res, "./bfda-res-remaining/set-37.rds")
