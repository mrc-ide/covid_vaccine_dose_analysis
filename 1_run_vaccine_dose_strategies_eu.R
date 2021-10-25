library(safir)
library(squire)
library(nimue)
library(data.table)
library(ggplot2)
library(parallel)
library(tidyverse)
library(countrycode)
library(furrr)
library(zoo)
library(here)

source("R/utils.R")
source("R/run_function_eu.R")
source("R/plotting_utils.R")

target_pop <- 1e6
income_group <- "HIC"
hs_constraints <- "Present"
dt <- 0.5
repetition <- 1:3
vacc_start <- "1/1/2021"
vaccine_doses <- c(2,3)
max_coverage <- 0.8
age_groups_covered <- 5
seeding_cases <- 10
variant_fold_reduction <- 1
dose_3_fold_increase <- 6
dose_strategy <- c("simple", "realistic")

#### Create scenarios ##########################################################

scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         vaccine_doses = vaccine_doses,
                         max_coverage = max_coverage,
                         age_groups_covered = age_groups_covered,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         variant_fold_reduction = variant_fold_reduction,
                         dose_3_fold_increase = dose_3_fold_increase,
                         dose_strategy = dose_strategy)

vaccine_doses <- 2
max_coverage <- 0
age_groups_covered <- 1
variant_fold_reduction <- 1
dose_strategy <- "realistic"

scenarios_counter <- expand_grid(income_group = income_group,
                                 target_pop = target_pop,
                                 hs_constraints = hs_constraints,
                                 vaccine_doses = vaccine_doses,
                                 max_coverage = max_coverage,
                                 age_groups_covered = age_groups_covered,
                                 vacc_start = vacc_start,
                                 dt = dt,
                                 repetition = repetition,
                                 seeding_cases = seeding_cases,
                                 variant_fold_reduction = variant_fold_reduction,
                                 dose_3_fold_increase = dose_3_fold_increase,
                                 dose_strategy = dose_strategy)

scenarios <- rbind(scenarios, scenarios_counter)

scenarios$scenario <- 1:nrow(scenarios)

nrow(scenarios)

write_csv(scenarios, "scenarios_eu.csv")

#### Run the model #############################################################
plan(multicore, workers = 4)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### OR Run the model on cluster ###############################################
#################################################################################
# to run on cluster instead
# Load functions
sources <- c("R/run_function_eu.R", "R/utils.R")
src <- conan::conan_sources(c("mrc-ide/safir", "mrc-ide/squire", "mrc-ide/nimue"))
ctx <- context::context_save("context",
                             sources = sources,
                             packages = c("tibble", "dplyr", "tidyr", "countrycode", "safir", "nimue", "squire"),
                             package_sources = src)

config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--didemrchnb")
# Create the queue
run <- didehpc::queue_didehpc(ctx, config = config)
# Summary of all available clusters
# run$cluster_load(nodes = FALSE)
# Run
runs <- run$enqueue_bulk(scenarios, run_scenario, do_call = TRUE, progress = TRUE)

