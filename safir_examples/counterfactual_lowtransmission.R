# packages to run safir
library(safir)
library(squire)
library(individual)

# analysis
library(ggplot2)
library(data.table)

# other
library(parallel)
library(countrycode)
library(here)
library(dplyr) # for mutate

rm(list=ls());gc()


# --------------------------------------------------------------------------------
# interpolate Rt (piecewise linear)
# --------------------------------------------------------------------------------

# from Azra:
# 1. R=0.5 through to Oct 21. R=3 from then onwards (most boosters delivered by then)
# 2. R=0.5 through to Apr 22, R =3 after then (allowing time for all over 60s to have received boosters)
# 3. Might want a third mid-point between these - say relaxing in Jan 22.

# Rt changes

# this sets up for the 1st scenario. To change to 2nd, just change to
# R0_t3 <- as.Date(x = "3/1/2022", format = "%m/%d/%Y")
# R0_t4 <- as.Date(x = "4/1/2022", format = "%m/%d/%Y")
# (I usually start ramping up the Rt a month before it should reach a new stable level)
# and you would want to change tmax_date to whatever the new maximum simulation 
# time is.
# likewise you could move things earlier to relax in Jan 2022

# piecewise segments
R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")
R0_t1 <- as.Date(x = "3/1/2020", format = "%m/%d/%Y")
R0_t2 <- as.Date(x = "5/1/2020", format = "%m/%d/%Y")
R0_t3 <- as.Date(x = "9/1/2021", format = "%m/%d/%Y")
R0_t4 <- as.Date(x = "10/1/2021", format = "%m/%d/%Y")

tmax_date <- as.Date(x = "6/1/2022", format = "%m/%d/%Y")
time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))

dates <- c(R0_t0, R0_t1, R0_t2, R0_t3, R0_t4)
rt <-    c(2.5,   2.5,   0.5,   0.5,   3)
rt_out <- safir::interpolate_rt(dates = dates, rt = rt, max_date = tmax_date)

# daily per-capita prob of external infection
lambda_external <- rep(0, time_period)

lambda_t0 <- as.integer(difftime(R0_t1, R0_t0 - 1))
lambda_t1 <- as.integer(difftime(R0_t4 - 10, R0_t0 - 1))

p <- 1/1e4 # daily prob of infection
lambda_external[lambda_t0:lambda_t1] <- -log(1 - p)

# make a pulse of hazard before the next wave
t_spread <- 10
lambda_tt <- as.integer(difftime(R0_t4, R0_t0 - 1))
lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.05

# plot Rt and pulsed external FoI
par(mfrow=c(2,1))
plot(x = rt_out$Rt_tt, y = rt_out$Rt, type = "l", ylim = c(0, 3))
plot(x = seq_along(lambda_external), y = lambda_external, type = "l")
par(mfrow=c(1,1))


# --------------------------------------------------------------------------------
# get parameters
# --------------------------------------------------------------------------------

source(here::here("safir_examples/utils.R"))

target_pop <- 1e6
income_group = "HIC"
hs_constraints = "Present"
seeding_cases <- 10

# simulation parameters
dt <- 0.5

# get country and standardize population
rep_country <- get_representative_country(income_group = income_group)
iso3c <- countrycode::countrycode(rep_country, origin = "country.name", destination = "iso3c")

pop <- squire::get_population(country = rep_country)
pop_standardise <- target_pop / sum(pop$n)
pop$n <- as.integer(pop$n * pop_standardise)

contact_mat <- squire::get_mixing_matrix(country = rep_country)

# Hospital capacity
hc <- get_capacity(country = rep_country, income_group = income_group, pop = pop$n, hs_constraints = hs_constraints)

# Poorer health outcomes for LMICs and LICs
pnsdt <- get_prob_non_severe_death_treatment(income_group, hs_constraints) 

# base parameters
base_parameters <- safir::get_parameters(
  population = pop$n,
  contact_matrix_set = contact_mat,
  iso3c = iso3c,
  R0 = rt_out$Rt,
  tt_R0 = rt_out$Rt_tt,
  time_period = time_period,
  dt = dt,
  hosp_bed_capacity = hc$hosp_beds,
  ICU_bed_capacity = hc$ICU_beds,
  prob_non_severe_death_treatment = pnsdt,
  dur_R = 365,
  seeding_cases = seeding_cases,
  lambda_external = lambda_external
)


# --------------------------------------------------------------------------------
# get vaccine parameters
# --------------------------------------------------------------------------------

max_coverage <- 0.8
coverage <- 0.2
vacc_start <- 30
vaccine_doses <- 3

vax_pars <- get_vaccine_pars()

dose_period <- c(NaN, 28, 268)

# doses available each day
doses_per_day <- floor(sum(pop$n) * 0.025 / 7)
# check how many days it takes to vaccinate to desired coverage with 2 doses
days_to_vacc <- floor(coverage / (0.025/7) * max_coverage * 2)
# vaccine vector: vector of length time_period
vaccine_set <- c(rep(0, vacc_start), rep(doses_per_day, days_to_vacc), rep(0, 240 - days_to_vacc), rep(doses_per_day, time_period - vacc_start  - 240))
vaccine_set <- vaccine_set * 0 # no vaccines for this run (baseline)

vaccine_coverage_strategy <- list()
vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)

next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1,ncol = ncol(vaccine_coverage_strategy[[1]]))
next_dose_priority[1:nrow(next_dose_priority), 15:17] <- 1 # prioritize 3 oldest age groups for next dose

# combine base parameters from squire with ab titire parameters
vaccine_parameters <- make_vaccine_parameters(
  safir_parameters = base_parameters,
  vaccine_ab_parameters = vax_pars,
  vaccine_set = vaccine_set,
  dose_period = dose_period,
  strategy_matrix = vaccine_coverage_strategy,
  next_dose_priority_matrix = next_dose_priority
)


# --------------------------------------------------------------------------------
# run safir-squire; faster to calibrate with this version of the model
# --------------------------------------------------------------------------------

# run one trajectory
timesteps <- base_parameters$time_period/dt
variables <- create_variables(pop = pop, parameters = base_parameters)
events <- create_events(parameters = base_parameters)
attach_event_listeners(variables = variables,events = events,parameters = base_parameters, dt = dt)
renderer <- individual::Render$new(base_parameters$time_period)
processes <- list(
  infection_process_cpp(parameters = base_parameters,variables = variables,events = events,dt = dt),
  categorical_count_renderer_process_daily(renderer, variables$state, categories = variables$states$get_categories(),dt = dt)
)
setup_events(parameters = base_parameters,events = events,variables = variables,dt = dt)

system.time(individual::simulation_loop(
  variables = variables,
  events = events,
  processes = processes,
  timesteps = timesteps
))
df <- renderer$to_dataframe()

saf_dt <- as.data.table(df)
saf_dt[, IMild_count := IMild_count + IAsymp_count]
saf_dt[, IAsymp_count := NULL]
saf_dt <- melt(saf_dt,id.vars = c("timestep"),variable.name = "name")
saf_dt[, model := "safir"]
saf_dt[, name := gsub("(^)(\\w*)(_count)", "\\2", name)]
setnames(x = saf_dt,old = c("timestep","name","value"),new = c("t","compartment","y"))

ggplot(data = saf_dt, aes(t,y,color = compartment)) +
  geom_line() +
  facet_wrap(~compartment, scales = "free")

saf_deaths <- as.data.table(df)
saf_deaths <- saf_deaths[, c("timestep", "D_count")]
setnames(saf_deaths, c("D_count", "timestep"), c("D", "t"))
saf_deaths <- saf_deaths[,  .(dy = diff(D), t = t[1:(length(t)-1)])]
saf_deaths[, date := as.Date(t, origin = "2/1/2020", format = "%m/%d/%Y")]

ggplot(data = saf_deaths) +
  geom_line(aes(x = date, y = dy)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%m/%Y")
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12))
