library(safir)
library(squire)
library(nimue)
library(data.table)
library(ggplot2)
library(parallel)
library(tidyverse)
library(countrycode)
library(furrr)

source("function_vacc_dose_strategies.R")

hs_constraints <- "Present"
income_group <- "HIC" #c("HIC", "UMIC", "LMIC", "LIC")
target_pop <- 50e4
R0 <- 2
Rt1 <- 1.05
Rt2 <- 3
tt_Rt1 <- 40
tt_Rt2 <- 300+180
dt <- 1
nrep <- 5
time_period <- 300+365+365
vacc_start <- 300+60
vaccine_doses <- c(2,3)
max_coverage <- c(0, 0.8)
coverage <- c(0.2,0.5)

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
                         nrep = nrep)

scenarios$scenario <- 1:nrow(scenarios)

nrow(scenarios)

#### Run the model #############################################################
plan(multicore, workers = 2)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### Format output #############################################################
out_format <- left_join(scenarios, do.call(rbind,out), by = "scenario")

### Save output ################################################################
saveRDS(out_format, "output/1_vaccine_dose_strategies.rds")
################################################################################
