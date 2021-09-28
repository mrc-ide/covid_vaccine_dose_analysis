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

rm(list=ls());gc()


# --------------------------------------------------------------------------------
# interpolate Rt (piecewise linear)
# --------------------------------------------------------------------------------

# from Azra:
#   
#   1st wave Feb 20 - May 20 R=2.5
# Jun 20 - Sept 20 R=0.9
# 2nd wave Oct 20 - Jan 21 R=1.5
# Feb 21 - Jun 21 R=1
# 3rd wave Jul 21 - Sept 21 R=3
# Oct 21 onwards continue R=3?
#   
#   Vaccine doses 1 and 2 - Start Jan 21 through to Jul 21
# Boosters - start Oct 21 onwards.

# Rt changes

# piecewise segments
R0_t0 <- as.Date(x = "2/1/2020", format = "%m/%d/%Y")
R0_t1 <- as.Date(x = "3/1/2020", format = "%m/%d/%Y")
R0_t2 <- as.Date(x = "6/1/2020", format = "%m/%d/%Y")
R0_t3 <- as.Date(x = "9/1/2020", format = "%m/%d/%Y")
R0_t4 <- as.Date(x = "10/1/2020", format = "%m/%d/%Y")
R0_t5 <- as.Date(x = "1/1/2021", format = "%m/%d/%Y")
R0_t6 <- as.Date(x = "2/1/2021", format = "%m/%d/%Y")
R0_t7 <- as.Date(x = "6/1/2021", format = "%m/%d/%Y")
R0_t8 <- as.Date(x = "7/1/2021", format = "%m/%d/%Y")

tmax_date <- as.Date(x = "12/1/2021", format = "%m/%d/%Y")
time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))

dates <- c(R0_t0, R0_t1, R0_t2, R0_t3, R0_t4, R0_t5, R0_t6, R0_t7, R0_t8)
rt <-    c(2.5,   2.5,   0.9,   0.9,   1.5,   1.5,   1.0,   1.0,   3)

rt_out <- safir::interpolate_rt(dates = dates, rt = rt, max_date = tmax_date)



# # weekly per-capita prob of external infection
lambda_external <- rep(0, time_period)

# # increase external FoI from R0_t2 to R0_t4
# lambda_tt <- as.integer(difftime(R0_t2, R0_t0 - 1)):as.integer(difftime(R0_t4, R0_t0 - 1))
# lambda_vals <- approx(x = c(lambda_tt[1], lambda_tt[length(lambda_tt)]), y = c(0, 0.01), xout = lambda_tt)$y
# lambda_external[lambda_tt] <- lambda_vals
# 
# lambda_tt <- as.integer(difftime(R0_t4, R0_t0 - 1)):as.integer(difftime(R0_t5, R0_t0 - 1))
# lambda_external[lambda_tt] <- 0.05


# lambda_tt <- as.integer(difftime(R0_t3, R0_t0 - 1)):as.integer(difftime(R0_t5, R0_t0 - 1))
# lambda_vals <- approx(x = c(lambda_tt[1], lambda_tt[length(lambda_tt)]), y = c(0, 0.01), xout = lambda_tt)$y
# lambda_external[lambda_tt] <- lambda_vals
# lambda_external[(lambda_tt[length(lambda_tt)]+1):length(lambda_external)] <- 0.01

t_spread <- 10
lambda_tt <- as.integer(difftime(R0_t3, R0_t0 - 1))
lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.05

t_spread <- 20
lambda_tt <- as.integer(difftime(R0_t7, R0_t0 - 1))
lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.01

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
dt <- 0.2
# nrep <- 10 # for MC reps
# options("mc.cores" = nrep)

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


# # --------------------------------------------------------------------------------
# # get vaccine parameters
# # --------------------------------------------------------------------------------
# 
# max_coverage <- 0.8
# coverage <- 0.2
# vacc_start <- 30
# vaccine_doses <- 3
# 
# vax_pars <- get_vaccine_pars()
# 
# dose_period <- c(NaN, 28, 268)
# 
# # doses available each day
# doses_per_day <- floor(sum(pop$n) * 0.025 / 7)
# # check how many days it takes to vaccinate to desired coverage with 2 doses
# days_to_vacc <- floor(coverage / (0.025/7) * max_coverage * 2)
# # vaccine vector: vector of length time_period
# vaccine_set <- c(rep(0, vacc_start), rep(doses_per_day, days_to_vacc), rep(0, 240 - days_to_vacc), rep(doses_per_day, time_period - vacc_start  - 240))
# # vaccine_set <- vaccine_set * 0 # no vaccines for this run (baseline)
# 
# vaccine_coverage_strategy <- list()
# vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
# vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
# vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
# 
# next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1,ncol = ncol(vaccine_coverage_strategy[[1]]))
# next_dose_priority[1:nrow(next_dose_priority), 15:17] <- 1 # prioritize 3 oldest age groups for next dose
# 
# # combine base parameters from squire with ab titire parameters
# vaccine_parameters <- make_vaccine_parameters(
#   safir_parameters = base_parameters,
#   vaccine_ab_parameters = vax_pars,
#   vaccine_set = vaccine_set,
#   dose_period = dose_period,
#   strategy_matrix = vaccine_coverage_strategy,
#   next_dose_priority_matrix = next_dose_priority
# )

# --------------------------------------------------------------------------------
# run safir-squire; faster to calibrate with this version of the model
# --------------------------------------------------------------------------------




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

ggplot(data = saf_deaths) +
  geom_line(aes(x = t, y = dy)) +
  scale_x_continuous(name = "Time (days)") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12))


# system.time(
#   saf_reps <- mclapply(X = 1:nrep, FUN = function(x){
# 
#     timesteps <- base_parameters$time_period/dt
#     variables <- create_variables(pop = pop, parameters = base_parameters)
#     events <- create_events(parameters = base_parameters)
#     attach_event_listeners(variables = variables,events = events,parameters = base_parameters, dt = dt)
#     renderer <- individual::Render$new(base_parameters$time_period)
#     processes <- list(
#       infection_process_cpp(parameters = base_parameters,variables = variables,events = events,dt = dt),
#       categorical_count_renderer_process_daily(renderer, variables$state, categories = variables$states$get_categories(),dt = dt)
#     )
#     setup_events(parameters = base_parameters,events = events,variables = variables,dt = dt)
# 
#     individual::simulation_loop(
#       variables = variables,
#       events = events,
#       processes = processes,
#       timesteps = timesteps
#     )
#     df <- renderer$to_dataframe()
#     df$repetition <- x
#     return(df)
#   })
# )
# 
# saf_reps <- do.call(rbind,saf_reps)
# 
# saf_dt <- as.data.table(saf_reps)
# saf_dt[, IMild_count := IMild_count + IAsymp_count]
# saf_dt[, IAsymp_count := NULL]
# saf_dt <- melt(saf_dt,id.vars = c("timestep","repetition"),variable.name = "name")
# saf_dt[, model := "safir"]
# saf_dt[, name := gsub("(^)(\\w*)(_count)", "\\2", name)]
# setnames(x = saf_dt,old = c("timestep","name","value"),new = c("t","compartment","y"))
# 
# ggplot(data = saf_dt, aes(t,y,color = compartment, group = repetition)) +
#   geom_line(alpha = 0.5) +
#   # geom_vline(xintercept = c(t1, t2, t3), linetype = 2, alpha = 0.5) +
#   facet_wrap(~compartment, scales = "free")
# 
# saf_deaths <- saf_dt[compartment == "D",  .(dy = diff(y), t = t[1:(length(t)-1)]), by = .(repetition)]
# 
# # a bit of a fudge to plot R0 on top of the deaths...scale it to that
# # max R0 is roughly in the middle of the y-axis (about 60 deaths/day)
# R0_df <- data.frame(x = c(tt_R0, time_period), y = c(R0, R0[length(R0)]))
# scaling_factor <- (60 / max(R0_df$y))
# R0_df$y <- R0_df$y * scaling_factor
# 
# ggplot(data = saf_deaths) +
#   geom_line(aes(x = t, y = dy, group = repetition), alpha = 0.5) +
#   geom_vline(xintercept = c(t1, t2, t3), linetype = 2, alpha = 0.5) +
#   geom_text(x = t1, y = -1, label = R0_t0) +
#   geom_text(x = t2, y = -1, label = R0_t1) +
#   geom_text(x = t3, y = -1, label = R0_t2) +
#   geom_text(x = time_period, y = -1, label = R0_t0 + time_period) +
#   geom_line(aes(x = x, y = y), data = R0_df) +
#   scale_y_continuous(sec.axis = sec_axis(~ ./scaling_factor, name = "R0"), name = "Deaths/day") +
#   scale_x_continuous(name = "Time (days)") +
#   theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12))


# --------------------------------------------------------------------------------
# run safir-vaccination model, but not distributing any vaccines
# --------------------------------------------------------------------------------

system.time(
  saf_reps <- mclapply(X = 1:nrep, FUN = function(x){
    
    # create variables
    timesteps <- vaccine_parameters$time_period/dt
    variables <- create_variables(pop = pop, parameters = vaccine_parameters)
    variables <- create_vaccine_variables(variables = variables,parameters = vaccine_parameters)
    
    # create events
    events <- create_events(parameters = vaccine_parameters)
    events <- create_events_vaccination(events = events,parameters = vaccine_parameters)
    attach_event_listeners(variables = variables,events = events,parameters = vaccine_parameters, dt = dt)
    attach_event_listeners_vaccination(variables = variables,events = events,parameters = vaccine_parameters,dt = dt)
    
    # make renderers
    renderer <- Render$new(vaccine_parameters$time_period)
    dose_age_renderer <- Render$new(vaccine_parameters$time_period)

    # processes
    processes <- list(
      vaccine_ab_titre_process(parameters = vaccine_parameters,variables = variables,events = events,dt = dt),
      vaccination_process(parameters = vaccine_parameters,variables = variables,events = events,dt = dt),
      infection_process_vaccine_cpp(parameters = vaccine_parameters,variables = variables,events = events,dt = dt),
      dose_age_render_process_daily(renderer = dose_age_renderer, age = variables$discrete_age, dose = variables$dose_num, parameters = vaccine_parameters, dt = dt),
      categorical_count_renderer_process_daily(renderer = renderer,variable = variables$states,categories = variables$states$get_categories(),dt = dt)
    )
    
    setup_events_vaccine(parameters = vaccine_parameters,events = events,variables = variables,dt = dt)
    
    simulation_loop_vaccine(
      variables = variables,
      events = events,
      processes = processes,
      timesteps = timesteps,
      progress = FALSE
    )
    
    df <- as.data.table(renderer$to_dataframe())
    df[, "repetition" := x]
    
    df_dose_age <- as.data.table(dose_age_renderer$to_dataframe())
    df_dose_age[, "repetition" := x]
    
    return(list(
      compartments = df,
      doses_age = df_dose_age
    ))
  })
)

# extract the trajectories of each compartment
saf_dt <- do.call(what = rbind, args = lapply(X = saf_reps, FUN = function(x){x$compartments}))

saf_dt[, IMild_count := IMild_count + IAsymp_count]
saf_dt[, IAsymp_count := NULL]
saf_dt <- melt(saf_dt,id.vars = c("timestep","repetition"),variable.name = "name")
saf_dt[, model := "safir"]
saf_dt[, name := gsub("(^)(\\w*)(_count)", "\\2", name)]
setnames(x = saf_dt,old = c("timestep","name","value"),new = c("t","compartment","y"))

ggplot(data = saf_dt, aes(t,y,color = compartment, group = repetition)) +
  geom_line(alpha = 0.5) +
  # geom_vline(xintercept = c(t1, t2, t3), linetype = 2, alpha = 0.5) +
  facet_wrap(~compartment, scales = "free")

# extract the doses/age trajectories
vaccine_dt <- do.call(what = rbind, args = lapply(X = saf_reps, FUN = function(x){x$doses_age}))
vaccine_dt <- melt(data = vaccine_dt, id.vars = c("timestep", "repetition"))
vaccine_dt[, c("dose", "age") := tstrsplit(x = variable, "_", keep = c(2,4)) ]
vaccine_dt[, c("dose", "age") := .(as.integer(dose), as.integer(age))]
vaccine_dt[, variable := NULL]

# need to divide rows corresponding to an age group by total size of that group
# to compare proportions
age_dt <- data.table(size = pop$n, age = 1:17)

vaccine_dt <- vaccine_dt[age_dt, on = .(age)]
vaccine_dt[, coverage := value / size]
vaccine_dt[, c("value", "size") := NULL]

ggplot(data = vaccine_dt) +
  geom_line(aes(x = timestep, y = coverage, color = as.factor(dose))) +
  facet_grid(age ~ .) +
  theme_bw()
  
# data.table of deaths is just difference in cumulative deaths
saf_deaths <- saf_dt[compartment == "D",  .(dy = diff(y), t = 1:(.N-1)), by = .(repetition)]

# again, the fudge to plot R0 on the same ggplot
R0_df <- data.frame(x = c(tt_R0, time_period), y = c(R0, R0[length(R0)]))
scaling_factor <- (60 / max(R0_df$y))
R0_df$y <- R0_df$y * scaling_factor

ggplot(data = saf_deaths) +
  geom_line(aes(x = t, y = dy, group = repetition), alpha = 0.5) +
  geom_vline(xintercept = c(t1, t2, t3), linetype = 2, alpha = 0.5) +
  geom_text(x = t1, y = -1, label = R0_t0) + 
  geom_text(x = t2, y = -1, label = R0_t1) + 
  geom_text(x = t3, y = -1, label = R0_t2) +
  geom_text(x = time_period, y = -1, label = R0_t0 + time_period) +
  geom_line(aes(x = x, y = y), data = R0_df) + 
  scale_y_continuous(sec.axis = sec_axis(~ ./scaling_factor, name = "R0"), name = "Deaths/day") +
  scale_x_continuous(name = "Time (days)") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12))