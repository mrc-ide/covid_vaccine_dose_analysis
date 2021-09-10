library(safir)
library(squire)
library(nimue)
library(data.table)
library(ggplot2)
library(parallel)
library(tidyverse)
library(countrycode)
library(furrr)

source("safir_examples/utils.R")
source("function_vacc_dose_strategies.R")

hs_constraints <- "Present"
income_group <- c("HIC", "LMIC") #c("HIC", "UMIC", "LMIC", "LIC")
target_pop <- 1.3e4
R0 <- 2.5
Rt1 <- 1.4
Rt2 <- 3
tt_Rt1 <- 60
tt_Rt2 <- 300+180
dt <- 0.2
repetition <- 1:2
time_period <- 300+365+365
vacc_start <- 300+60
vaccine_doses <- c(2,3)
max_coverage <- 0.8
coverage <- c(0.2, 0.3, 0.4)
seeding_cases <- 20
variant_fold_reduction <- 4#c(1, 4)

#### Create scenarios ##########################################################

scenarios <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         tt_Rt1 = tt_Rt1,
                         tt_Rt2 = tt_Rt2,
                         time_period = time_period,
                         vaccine_doses = vaccine_doses,
                         max_coverage = max_coverage,
                         coverage = coverage,
                         vacc_start = vacc_start,
                         dt = dt,
                         repetition = repetition,
                         seeding_cases = seeding_cases,
                         variant_fold_reduction = variant_fold_reduction) %>%
  mutate(dose_3_fold_increase = if_else(variant_fold_reduction == 1, 4, 
                                        if_else(variant_fold_reduction == 4, 6, 1)))

vaccine_doses <- 2
max_coverage <- 0
coverage <- 0.2
variant_fold_reduction <- 1

scenarios_counter <- expand_grid(income_group = income_group,
                         target_pop = target_pop,
                         hs_constraints = hs_constraints,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         tt_Rt1 = tt_Rt1,
                         tt_Rt2 = tt_Rt2,
                         time_period = time_period,
                         vaccine_doses = vaccine_doses,
                         max_coverage = max_coverage,
                         coverage = coverage,
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

write.csv(scenarios, "scenarios_cluster_v4.csv")

#### Run the model #############################################################
plan(multicore, workers = 4)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### Format output #############################################################
out_format <- left_join(scenarios, do.call(rbind,out), by = "scenario")

### Save output ################################################################
saveRDS(out_format, "output/1_vaccine_dose_strategies.rds")
################################################################################

#### OR Run the model on cluster ###############################################
#################################################################################
# to run on cluster instead
# Load functions
sources <- c("function_vacc_dose_strategies_cluster.R", "safir_examples/utils.R")
src <- conan::conan_sources(c("mrc-ide/safir", "mrc-ide/squire", "mrc-ide/nimue"))
ctx <- context::context_save("context",
                             sources = sources,
                             packages = c("tibble", "dplyr", "tidyr", "countrycode", "individual", "safir", "nimue", "squire"),
                             package_sources = src)

config <- didehpc::didehpc_config(use_rrq = FALSE, use_workers = FALSE, cluster="fi--didemrchnb")
# Create the queue
run <- didehpc::queue_didehpc(ctx, config = config)
# Summary of all available clusters
# run$cluster_load(nodes = FALSE)
# Run
runs <- run$enqueue_bulk(scenarios, run_scenario, do_call = TRUE, progress = TRUE)

