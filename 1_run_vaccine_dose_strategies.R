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

source("R/utils.R")
source("R/run_function.R")

income_group <- c("HIC", "LMIC") #c("HIC", "UMIC", "LMIC", "LIC")
target_pop <- 1e6
hs_constraints <- "Present"
R0 <- 2
Rt1 <- 1.2
Rt2 <- 3
tt_Rt1 <- 60
tt_Rt1_stop <- 300+90
tt_Rt2 <- 300+180
dt <- 1
repetition <- 1:20
time_period <- 300+365+365+180
vacc_start <- 300+60
vaccine_doses <- c(2,3)
max_coverage <- 0.8
age_groups_covered <- c(5,9)
seeding_cases <- 20
variant_fold_reduction <- 1

#### Create scenarios ##########################################################

scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         tt_Rt1 = tt_Rt1,
                         tt_Rt2 = tt_Rt2,
                         tt_Rt1_stop = tt_Rt1_stop,
                         time_period = time_period,
                         vaccine_doses = vaccine_doses,
                         max_coverage = max_coverage,
                         age_groups_covered = age_groups_covered,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         variant_fold_reduction = variant_fold_reduction) %>%
  mutate(dose_3_fold_increase = if_else(variant_fold_reduction == 1, 4, 
                                        if_else(variant_fold_reduction == 4, 6, 1)))

vaccine_doses <- 2
max_coverage <- 0
age_groups_covered <- 1
variant_fold_reduction <- 1

scenarios_counter <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         tt_Rt1 = tt_Rt1,
                         tt_Rt2 = tt_Rt2,
                         tt_Rt1_stop = tt_Rt1_stop,
                         time_period = time_period,
                         vaccine_doses = vaccine_doses,
                         max_coverage = max_coverage,
                         age_groups_covered = age_groups_covered,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         variant_fold_reduction = variant_fold_reduction) %>%
  mutate(dose_3_fold_increase = if_else(variant_fold_reduction == 1, 4, 
                                        if_else(variant_fold_reduction == 4, 6, 1)))

scenarios <- rbind(scenarios, scenarios_counter)

scenarios$scenario <- 1:nrow(scenarios)

nrow(scenarios)

write_csv(scenarios, "scenarios.csv")

#### Run the model #############################################################
plan(multicore, workers = 4)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### OR Run the model on cluster ###############################################
#################################################################################
# to run on cluster instead
# Load functions
sources <- c("R/run_function.R", "R/utils.R")
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

