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
source("R/run_function_abmodel_zerocovid.R")
source("R/plotting_utils.R")
source("R/vaccine_strategy.R")

name <- "rq3_hic_abmodel_zerocovid"

target_pop <- 1e6
income_group <- "HIC"
hs_constraints <- "Present"
dt <- 0.25
repetition <- 1:10
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
t_d3 <- 180
mu_ab_infection <- 1
max_Rt = 5

# how many days to reach 80% vaccinated with 2 doses, at 5% vacc per dose per week?
days <- floor(0.8 / (vacc_per_week/7))*2
d1 <- as.Date(x = (as.Date("1/1/2021", format = "%m/%d/%Y") + days), format = "%m/%d/%Y")

R0_t3_in <- c("8/1/2021", "10/1/2021", "2/1/2022")

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
                         t_d3 = t_d3,
                         mu_ab_infection = mu_ab_infection,
                         R0_t3_in = R0_t3_in,
                         max_Rt = max_Rt)  %>%
  filter((vaccine_doses == 2 & age_groups_covered_d3 == 5 ) | (vaccine_doses == 3) ) %>%
  unique()

scenarios$scenario <- 1:nrow(scenarios)
scenarios$name <- name
scenarios$strategy <- strategy

nrow(scenarios)

write_csv(scenarios, paste0("scenarios_", name, ".csv"))

#### Run the model on cluster ###############################################
# Load functions
sources <- c("R/run_function_abmodel_zerocovid.R", "R/utils.R", "R/vaccine_strategy.R")
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
runs <- run$enqueue_bulk(scenarios, run_scenario_zerocovid, do_call = TRUE, progress = TRUE)
runs$status()

