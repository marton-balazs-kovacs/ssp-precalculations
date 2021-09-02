# Load packages
library(future.apply)
library(tibble)
library(purrr)
library(dplyr)
library(tidyr)

# Load functions
source("R/cdf_t.R")
source("R/integrand_t.R")
source("R/posterior_t.R")
source("R/tpr_optim.R")
source("R/ssp_rope.R")
source("R/ssp_tost.R")

## Create fail safe ROPE function
ssp_rope_safe <- function(tpr, eq_band, delta, prior_scale, iterate) {
  out <- tryCatch(
    {
      r <- ssp_rope(tpr = tpr, eq_band = eq_band, delta = delta, prior_scale = prior_scale)
      
      list(
        result = r,
        error = NULL,
        params = list(
          tpr = tpr,
          eq_band = eq_band,
          delta = delta,
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
          prior_scale = prior_scale,
          iterate = iterate
        )
      )
    }
  )
  return(out)
}

# Create directory to save the results in
ifelse(!dir.exists("./rope-res-remaining"),
       dir.create("./rope-res-remaining"),
       "Directory already exists.")

# Read the outputs of all iterations
rope_data <- 
  tibble(filename = list.files(path = "./rope-res/", pattern = ".rds")) %>%
  mutate(file = here::here("rope-res", filename),
         data = map(file, readRDS)) %>% 
  unnest(data) %>% 
  mutate(params = map(data, "params"),
         tpr = map_dbl(params, ~ pluck(.x, "tpr", .default = NA_real_)),
         eq_band = map_dbl(params, ~ pluck(.x, "eq_band", .default = NA_real_)),
         delta = map_dbl(params, ~ pluck(.x, "delta", .default = NA_real_)),
         prior_scale = map_dbl(params, ~ pluck(.x, "prior_scale", .default = NA_real_))
  ) %>% 
  select(-file, -filename, -params) 

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

rope_options_remaining <-
  rope_options %>% 
  anti_join(., rope_data, by = c("tpr", "delta", "eq_band", "prior_scale"))

rope_options_remaining$iterate <- 1:nrow(rope_options_remaining) + 11000

# Split the data for each iteration
rope_options_remaining_split <- split(rope_options_remaining, rope_options_remaining$iterate)

# Calculate ROPE
rope_res <-  future.apply::future_lapply(rope_options_remaining_split, function(x) ssp_rope_safe(tpr = x$tpr, eq_band = x$eq_band, delta = x$delta, prior_scale = x$prior_scale, iterate = x$iterate))

# Saving data
saveRDS(rope_res, "./rope-res-remaining/set-111.rds")
