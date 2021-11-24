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
source("R/run_function_abmodel_age.R")
source("R/plotting_utils.R")

target_pop <- 1e6
income_group <- "HIC"
hs_constraints <- "Absent"
dt <- 0.5
repetition <- 1
vacc_start <- "1/1/2021"
vaccine_doses <- c(2,3)
vaccine <- "Pfizer"
max_coverage <- 0.9
age_groups_covered <- 15
age_groups_covered_d3 <- c(5, 9, 13, 15)
seeding_cases <- 10
variant_fold_reduction <- 1
dose_3_fold_increase <- 6
vacc_per_week <- 0.05
name <- "rq1_hic_age"
ab_model_infection <- TRUE
strategy <- "realistic"
period_s <- 250
t_period_l <- 365

#### Create scenarios ##########################################################

scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         vaccine_doses = vaccine_doses,
                         vaccine = vaccine,
                         max_coverage = max_coverage,
                         age_groups_covered = age_groups_covered,
                         age_groups_covered_d3 = age_groups_covered_d3,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         variant_fold_reduction = variant_fold_reduction,
                         dose_3_fold_increase = dose_3_fold_increase,
                         vacc_per_week = vacc_per_week,
                         ab_model_infection = ab_model_infection,
                         period_s = period_s,
                         t_period_l = t_period_l)

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

nrow(scenarios)

write_csv(scenarios, paste0("scenarios_", name, ".csv"))

#### OR Run the model on cluster ###############################################
# to run on cluster instead
# Load functions
sources <- c("R/run_function_abmodel_age.R", "R/utils.R", "R/vaccine_strategy.R")
src <- conan::conan_sources(c("mrc-ide/safir", "mrc-ide/squire", "mrc-ide/nimue"))
ctx <- context::context_save("context",
                             sources = sources,
                             packages = c("tibble", "dplyr", "tidyr", "countrycode", "safir", "nimue", "squire", "data.table"),
                             package_sources = src)

config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--didemrchnb")
# Create the queue
run <- didehpc::queue_didehpc(ctx, config = config)
# Summary of all available clusters
# run$cluster_load(nodes = FALSE)
# Run
runs <- run$enqueue_bulk(scenarios, run_scenario_age, do_call = TRUE, progress = TRUE)

