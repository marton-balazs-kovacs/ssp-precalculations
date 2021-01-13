# Precalculation of methods results for SPP project
## Load packages
library(tidyverse)
library(future)
library(googleComputeEngineR)
# devtools::install_github("marton-balazs-kovacs/SampleSizePlanner", force = TRUE)
library(SampleSizePlanner)
library(furrr)

## Get gce project information
gce_get_project()

## Get possible vm machine types for the project
machine_types <- gce_list_machinetype()

## Get SSH information 
ssh <- list(
  "username" = "marton",
  # "ssh_overwrite" = FALSE,
  "key.pub" = file.path("C:","Users", "marton", ".ssh", "id_rsa.pub"), 
  "key.private" = file.path("C:","Users", "marton", ".ssh", "id_rsa")
)

## Create a vm instance
instance_name <- "test"
vm <- gce_vm_create(name = instance_name, 
                    predefined_type = "f1-micro",
                    metadata = list(start_date = as.character(Sys.Date())))

## Connect to existing instance
vm <- gce_vm(instance_name)

## Setup ssh on the instance
vm <- gce_ssh_setup(vm,
                    username = "marton",
                    key.pub = file.path("C:","Users", "marton", ".ssh", "id_rsa.pub"),
                    key.private = file.path("C:","Users", "marton", ".ssh", "id_rsa"))

## Add SSH keys to the instance
vm <- gce_ssh_addkeys(vm,
                      username = "marton",
                      key.pub = file.path("C:","Users", "marton", ".ssh", "id_rsa.pub"),
                      key.private = file.path("C:","Users", "marton", ".ssh", "id_rsa"))

## Test SSH on the instance
gce_ssh(vm, "echo foo")

## Create vm cluster
vms <- gce_vm_cluster(
  vm_prefix = "spp-precalc-", 
  cluster_size = 2,
  docker_image = "rocker/r-parallel", 
  ssh_args = ssh,
  project = gce_get_global_project(), 
  zone = gce_get_global_zone()
)

## Set data for ROPE function test 
power = .8
band = .2
delta = 0

## Create fail safe ROPE function
ssp_rope_safe <- purrr::safely(ssp_rope)

## Test the function
ssp_rope_safe(power, band, delta)
ssp_rope(power, band, delta)

## Create datatable storing possible ROPE options
rope_options <- 
  tidyr::expand_grid(
    power = seq(0.5, 0.95, by = 0.01),
    delta = seq(0, 2, by = 0.05),
    band = seq(0.1, 0.5, by = 0.01)
    )

## Create a test set
test_set <- 
  rope_options %>% 
  slice(100:102) %>% 
  mutate(n_iteration = 1:n())

### Number of available cores
availableCores()

### Make a future plan for one multicore machine
plan(multisession, workers = 3)

test_set_split <- split(test_set, test_set$n_iteration)

tictoc::tic()
rope_data <-  furrr::future_map(test_set_split, 
                                ~ ssp_rope_safe(opt = .$power,
                                                band = .$band,
                                                delta = .$delta))
tictoc::toc()