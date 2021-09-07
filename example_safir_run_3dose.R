rm(list=ls());gc()

# packages to run safir
library(safir)
library(nimue)
library(squire)
library(individual)

# analysis
library(tidyverse)
library(patchwork)

# sean's plotting
library(ggplot2)
library(data.table)

# some parameters for safir
tmax <- 365
dt <- 0.25

R0 <- c(2, 1.05, 1.85)
tt_R0 <- c(0, 30, 200)


# --------------------------------------------------------------------------------
# vaccine parameters (I took these from your code here: https://github.com/mrc-ide/covid-titre-efficacy/blob/main/vaccines_with_booster.R)
# --------------------------------------------------------------------------------

variant_fold_reduction <- 4
dose_3_fold_increase <- 6

mu_ab_list <- data.frame(name = c("Oxford-AstraZeneca", "Pfizer", "Moderna"),
                         mu_ab_d1 = c(20/59, 20/94, ((185+273)/2)/321),
                         mu_ab_d2 = c(32/59, 223/94,  654/158)) %>%
  mutate(mu_ab_d1 = mu_ab_d1/variant_fold_reduction,
         mu_ab_d2 = mu_ab_d2/variant_fold_reduction) %>%
  mutate(mu_ab_d3 = mu_ab_d2 * dose_3_fold_increase)


ab_50 <- 0.2 # titre relative to convalescent required to provide 50% protection from infection, on linear scale
ab_50_severe <- 0.03
std10 <- 0.44 # Pooled standard deviation of antibody level on log10 data
k <- 2.94 # shape parameter of efficacy curve
t_d2 <- 84 # timing of second dose relative to first
t_d3 <- 240 # timing of third dose relative to second
hl_s <- 108 # Half life of antibody decay - short
hl_l <- 3650 # Half life of antibody decay - long
period_s <- 250
t_period_l <- 365 # Time point at which to have switched to longest half-life


# --------------------------------------------------------------------------------
# create the parameter objects for safir
# --------------------------------------------------------------------------------

vaccine_doses <- 3
dose_period <- c(NaN, t_d2, t_d3) # min time between doses
vaccine_set <- floor(c(0, seq(from = 1e1, to = 1e2, length.out = tmax-1))) # vaccine available per day
# vaccine_set <- vaccine_set * 0

# safir can either use a single strategy matrix for all vaccine doses or a list with a matrix for each dose,
# we'll use a list for this example.
vaccine_coverage_strategy <- list()
vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = 0.8)
vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = 0.8)
vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = 0.8)

next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1,ncol = ncol(vaccine_coverage_strategy[[1]]))
next_dose_priority[1:nrow(next_dose_priority), 15:17] <- 1 # prioritize 3 oldest age groups for next dose

# parameters related to ab titre and efficacy
vaccine_ab_efficacy_parameters <- get_vaccine_ab_titre_parameters(
  vaccine = "Pfizer", max_dose = vaccine_doses, correlated = TRUE,
  hl_s = hl_s, hl_l = hl_l, period_s = period_s, t_period_l = t_period_l,
  ab_50 = ab_50, ab_50_severe = ab_50_severe, std10 = std10, k = k,
  mu_ab_list = mu_ab_list
)

# base parameters from squire
iso3c <- "GBR"
pop <- safir::get_population(iso3c)
pop$n <- as.integer(pop$n / 1e3)
contact_mat <- squire::get_mixing_matrix(iso3c = iso3c)

# this function passes ... arguments down to squire::parameters_explicit_SEEIR
# so we can specify anything that squire can accept.
squire_parameters <- get_parameters(
  population = pop$n,
  contact_matrix_set = contact_mat,
  iso3c = iso3c,
  R0 = R0,
  tt_R0 = tt_R0,
  time_period = tmax,
  dt = dt,
  seeding_cases = 60
)

# combine base parameters from squire with ab titire parameters
parameters <- make_vaccine_parameters(
  safir_parameters = squire_parameters,
  vaccine_ab_parameters = vaccine_ab_efficacy_parameters,
  vaccine_set = vaccine_set,
  dose_period = dose_period,
  strategy_matrix = vaccine_coverage_strategy,
  next_dose_priority_matrix = next_dose_priority
)


# --------------------------------------------------------------------------------
# create objects for safir: for running safir in parallel, these objects
# need to be made on each run
# --------------------------------------------------------------------------------

# create variables
timesteps <- parameters$time_period/dt
variables <- create_variables(pop = pop, parameters = parameters)
variables <- create_vaccine_variables(variables = variables,parameters = parameters)

# create events
events <- create_events(parameters = parameters)
events <- create_events_vaccination(events = events,parameters = parameters)
attach_event_listeners(variables = variables,events = events,parameters = parameters, dt = dt)
attach_event_listeners_vaccination(variables = variables,events = events,parameters = parameters,dt = dt)

# make renderers
renderer <- Render$new(parameters$time_period)
dose_renderer <- Render$new(parameters$time_period)

# processes
processes <- list(
  vaccine_ab_titre_process(parameters = parameters,variables = variables,events = events,dt = dt),
  vaccination_process(parameters = parameters,variables = variables,events = events,dt = dt),
  infection_process_vaccine_cpp(parameters = parameters,variables = variables,events = events,dt = dt),
  categorical_count_renderer_process_daily(renderer = renderer,variable = variables$states,categories = variables$states$get_categories(),dt = dt),
  integer_count_render_process_daily(renderer = dose_renderer,variable = variables$dose_num,margin = 0:vaccine_doses,dt = dt)
)

setup_events_vaccine(parameters = parameters,events = events,variables = variables,dt = dt)

system.time(simulation_loop_vaccine(
  variables = variables,
  events = events,
  processes = processes,
  timesteps = timesteps,
  TRUE
))


# --------------------------------------------------------------------------------
# plot output
# --------------------------------------------------------------------------------

# plot: vaccinations
dose_out <- dose_renderer$to_dataframe()
colnames(dose_out)[2:(vaccine_doses+2)] <- as.character(0:vaccine_doses)
dose_out <- melt(as.data.table(dose_out),id.vars="timestep")
setnames(dose_out, "variable", "dose")

ggplot(data = dose_out) +
  geom_line(aes(x=timestep,y=value,color=dose)) +
  theme_bw()

# states
saf_dt <- as.data.table(renderer$to_dataframe())
saf_dt[, IMild_count := IMild_count + IAsymp_count]
saf_dt[, IAsymp_count := NULL]
saf_dt <- melt(saf_dt,id.vars = c("timestep"),variable.name = "name")
saf_dt[, name := gsub("(^)(\\w*)(_count)", "\\2", name)]
setnames(x = saf_dt,old = c("timestep","name","value"),new = c("t","compartment","y"))

ggplot(data = saf_dt, aes(t,y,color = compartment)) +
  geom_line() +
  geom_line() +
  facet_wrap(~compartment, scales = "free")
