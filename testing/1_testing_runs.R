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

source("R/utils_test.R")
source("R/run_function_abmodel_test.R")
source("R/plotting_utils.R")
source("R/vaccine_strategy_test.R")

name <- "testing_runs"

target_pop <- 8e5
income_group <- "HIC"
hs_constraints <- "Present"
dt <- c(0.1, 0.2, 0.25, 0.5, 1)
repetition <- 1
vacc_start <- "1/1/2021"
vaccine_doses <- c(2,3)
vaccine <- "Pfizer"
max_coverage <- c(0.9, 0.98)
age_groups_covered <- c(17, 15)
age_groups_covered_d3 <- c(5, 15)
seeding_cases <- 10
variant_fold_reduction <- 1
dose_3_fold_increase <- 6
vacc_per_week <- 0.05
ab_model_infection <- TRUE
strategy <- "realistic"
period_s <- 78.98979
t_period_l <- period_s + 275.1839
t_d3 <- 180
max_Rt <- 3
std10 <- 0.02#c(0.44, 0.02)

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
                         t_period_l = t_period_l,
                         t_d3 = t_d3,
                         max_Rt = max_Rt,
                         std10 = std10) %>%
  filter((vaccine_doses == 2 & age_groups_covered_d3 == 5 ) | (vaccine_doses == 3) ) %>%
  unique()

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

nrow(scenarios)

write_csv(scenarios, paste0("scenarios_", name, ".csv"))

#### Run the model on cluster ###############################################
# Load functions
sources <- c("R/run_function_abmodel_test.R", "R/utils_test.R", "R/vaccine_strategy_test.R")
src <- conan::conan_sources(c("mrc-ide/safir", "mrc-ide/squire", "mrc-ide/nimue"))
ctx <- context::context_save("context",
                             sources = sources,
                             packages = c("tibble", "dplyr", "tidyr", "countrycode", "safir", "nimue", "squire", "data.table"),
                             package_sources = src)

config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--didemrchnb")
#config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--dideclusthn")

# Create the queue
run <- didehpc::queue_didehpc(ctx, config = config)
# Summary of all available clusters
# run$cluster_load(nodes = FALSE)
# Run
runs <- run$enqueue_bulk(scenarios, run_scenario_add, do_call = TRUE, progress = TRUE)
runs$status()

