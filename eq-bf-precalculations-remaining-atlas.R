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
ifelse(!dir.exists("./eq-bf-res-remaining"),
       dir.create("./eq-bf-res-remainings"),
       "Directory already exists.")

# Read the outputs of all iterations
eq_bf_data <- 
  tibble(filename = list.files(path = "./eq-bf-res/", pattern = ".rds")) %>%
  mutate(file = here::here("eq-bf-res", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(params = map(data, "params"),
         tpr = map_dbl(params, ~ pluck(.x, "tpr", .default = NA_real_)),
         delta = map_dbl(params, ~ pluck(.x, "delta", .default = NA_real_)),
         prior_scale = map_dbl(params, ~ pluck(.x, "prior_scale", .default = NA_real_)),
         eq_band = map_dbl(params, ~ pluck(.x, "eq_band", .default = NA_real_)),
         thresh = map_dbl(params, ~ pluck(.x, "thresh", .default = NA_real_)),
         iterate = map_dbl(params, ~ pluck(.x, "iterate", .default = NA_real_))
  ) %>% 
  select(-file, -filename, -params)

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

eq_bf_options_remaining <-
  eq_bf_options %>% 
  anti_join(., eq_bf_data, by = c("tpr", "delta", "thresh", "prior_scale", "eq_band"))

eq_bf_options_remaining$iterate <- 1:nrow(eq_bf_options_remaining) + 33200

# Split the data for each iteration
eq_bf_options_remaining_split <- split(eq_bf_options_remaining, eq_bf_options_remaining$iterate)

# Calculate Equivalence Bayes factor
eq_bf_res <-  future.apply::future_lapply(eq_bf_options_remaining_split, function(x) ssp_eq_bf_safe(tpr = x$tpr, eq_band = x$eq_band, delta = x$delta, thresh = x$thresh, prior_scale = x$prior_scale, iterate = x$iterate))

# Saving data
saveRDS(eq_bf_res, "./eq-bf-res-remaining/set-333.rds")