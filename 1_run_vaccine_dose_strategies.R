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

options("mc.cores" = 2)

hs_constraints <- "Present"
income_group <- "HIC"
target_pop <- 50e2
nrep <- 10
R0 <- 2
Rt1 <- 1.05
Rt2 <- 4
tt_Rt1 <- 20
tt_Rt2 <- 70
dt <- 0.1
nrep <- 5
time_period <- 100
vaccine_doses <- 2
max_coverage <- c(0.2, 0.5)

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
                         dt = dt,
                         nrep = nrep)

scenarios$scenario <- 1:nrow(scenarios)

nrow(scenarios)

#### Run the model #############################################################
plan(multiprocess, workers = 2)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE,)})

#### Format output #############################################################
out_format <- left_join(scenarios, out)

### Save output ################################################################
saveRDS(out_format, "output/1_vaccine_dose_strategies.rds")
################################################################################
