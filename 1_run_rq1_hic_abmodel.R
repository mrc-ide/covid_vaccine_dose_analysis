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
source("R/run_function_abmodel.R")
source("R/plotting_utils.R")
source("R/vaccine_strategy.R")

name <- "rq1_hic_abmodel"

target_pop <- 1e6
income_group <- "HIC"
hs_constraints <- "Present"
dt <- 0.5
repetition <- 1:5
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
ab_model_infection <- TRUE
strategy <- "realistic"
period_s <- c(250, 150)
t_period_l <- c(365, 200)
t_d3 <- c(180, 240, 360)

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
                         t_d3 = t_d3) %>%
  filter((period_s == 250 & t_period_l == 365) | (period_s == 150 & t_period_l == 200)) %>%
  filter((t_d3 == 180) | (t_d3 != 180 & period_s == 250 & t_period_l == 365)) %>%
  filter((vaccine_doses == 2 & age_groups_covered_d3 == 5 ) | (vaccine_doses == 3) ) %>%
  unique()

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

nrow(scenarios)

write_csv(scenarios, paste0("scenarios_", name, ".csv"))

#### Run the model on cluster ###############################################
# Load functions
sources <- c("R/run_function_abmodel.R", "R/utils.R", "R/vaccine_strategy.R")
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
runs <- run$enqueue_bulk(scenarios, run_scenario, do_call = TRUE, progress = TRUE)

