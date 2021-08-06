library(safir)
library(squire)
library(data.table)
library(ggplot2)
library(parallel)

iso3c <- "ATG"
contact_mat <- squire::get_mixing_matrix(iso3c = iso3c)
pop <- safir:::get_population(iso3c)

# use as many as you want normally.
options("mc.cores" = 2)

nrep <- 10
# Scale it for speed
pop$n <- as.integer(pop$n / 5)

# Create our simulation parameters
R0 <- 2
time_period <- 200

  parameters <- safir::get_parameters(
  population = pop$n,
  contact_matrix_set = contact_mat,
  iso3c = iso3c,
  R0 = R0,
  time_period = time_period
)

dt <- 0.1
system.time(
  saf_reps <- mclapply(X = 1:nrep,FUN = function(x){
    
    timesteps <- parameters$time_period/dt
    variables <- create_variables(pop = pop, parameters = parameters)
    events <- create_events(parameters = parameters)
    attach_event_listeners(variables = variables,events = events,parameters = parameters, dt = dt)
    renderer <- individual::Render$new(parameters$time_period)
    processes <- list(
      infection_process_cpp(parameters = parameters,variables = variables,events = events,dt = dt),
      categorical_count_renderer_process_daily(renderer, variables$state, categories = variables$states$get_categories(),dt = dt)
    )
    setup_events(parameters = parameters,events = events,variables = variables,dt = dt)
    
    individual::simulation_loop(
      variables = variables,
      events = events,
      processes = processes,
      timesteps = timesteps
    )
    df <- renderer$to_dataframe()
    df$repetition <- x
    return(df)
  })
)
#>    user  system elapsed 
#>  19.580   0.644  23.966

saf_reps <- do.call(rbind,saf_reps)

# safir
saf_dt <- as.data.table(saf_reps)
saf_dt[, IMild_count := IMild_count + IAsymp_count]
saf_dt[, IAsymp_count := NULL]
saf_dt <- melt(saf_dt,id.vars = c("timestep","repetition"),variable.name = "name")
saf_dt[, model := "safir"]
saf_dt[, name := gsub("(^)(\\w*)(_count)", "\\2", name)]
setnames(x = saf_dt,old = c("timestep","name","value"),new = c("t","compartment","y"))
saf_dt <- saf_dt[, .(ymin = quantile(y,0.025), ymax = quantile(y,0.975), y = median(y)), by = .(t,compartment,model)]
