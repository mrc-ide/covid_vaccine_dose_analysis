# Specify the country chosen to represent each income group
get_representative_country <- function(income_group){
  case_when(income_group == "HIC" ~ "Malta",
            income_group == "UMIC" ~ "Grenada",
            income_group == "LMIC" ~ "Nicaragua",
            income_group == "LIC" ~ "Madagascar")
}

# Set hospital and ICU capacity
get_capacity <- function(country, income_group, pop, hs_constraints){
  hc <- squire::get_healthcare_capacity(country = country)
  
  # Unconstrained healthcare
  if(hs_constraints == "Absent"){
    hc$hosp_beds <- 1000000
    hc$ICU_beds <- 1000000
  }
  
  if(hs_constraints == "Present"){
    if(income_group %in% c("HIC", "UMIC")){
      hc$hosp_beds <- 1000000
      hc$ICU_beds <- 1000000
    }
    if(income_group %in% c("LMIC", "LIC")){
      hc$ICU_beds <- 0
    }
  }
  
  hc$hosp_beds <- round(hc$hosp_beds * sum(pop) / 1000)
  hc$ICU_beds <- round(hc$ICU_beds * sum(pop) / 1000)
  
  return(hc)
}

# Parameterise poorer health outcomes in LMIC and LIC
get_prob_non_severe_death_treatment <- function(income_group, hs_constraints){
  psdt <- squire:::probs$prob_non_severe_death_treatment
  
  if(income_group  == "LIC" & hs_constraints == "Present"){
    psdt <- c(rep(0.25, 16), 0.5804312)
  }
  return(psdt)
}

# main running function
run_scenario <- 
  function(scenario = NA,
           target_pop = 50e5,
           income_group = "HIC",
           hs_constraints = "Present",
           R0 = 2,
           Rt1 = 1.1,
           Rt2 = 2.5,
           tt_Rt1 = 50,
           tt_Rt2 = 200,
           vacc_start = 30,
           time_period = 365,
           vaccine_doses = 2,
           max_coverage = 0.8,
           coverage = 0.2,
           dt = 1,
           nrep = 10){
    
    stopifnot(all(c(tt_Rt1, tt_Rt2) < time_period))
    
    # Population and mixing
    rep_country <- get_representative_country(income_group = income_group)
    iso3c <- countrycode(rep_country, origin = "country.name", destination = "iso3c")
    pop <- squire::get_population(country = rep_country)
    pop_standardise <- target_pop / sum(pop$n)
    pop$n <- as.integer(pop$n * pop_standardise)
    contact_mat <- squire::get_mixing_matrix(country = rep_country)

    # Hospital capacity
    hc <- get_capacity(country = rep_country, income_group = income_group, pop = pop$n, hs_constraints = hs_constraints)
    
    # Poorer health outcomes for LMICs and LICs
    pnsdt = get_prob_non_severe_death_treatment(income_group, hs_constraints) 
    
    # Vaccine parameters
    # dosing
    if (vaccine_doses == 2) {dose_period <- c(NaN, 28)}
    if (vaccine_doses == 3) {dose_period <- c(NaN, 28, 268)}
    
    # doses available each day
    doses_per_day <- floor(sum(pop$n) * 0.025 / 7)
    # check how many days it takes to vaccinate to desired coverage with 2 doses
    days_to_vacc <- floor(coverage / (0.025/7) * max_coverage * 2)
    # vaccine vector: vector of length time_period
    vaccine_set <- c(rep(0, vacc_start), rep(doses_per_day, days_to_vacc), rep(0, 240 - days_to_vacc), rep(doses_per_day, time_period - vacc_start  - 240))

    # vaccine strategy
    vaccine_coverage_mat <- strategy_matrix(strategy = "Elderly", max_coverage = max_coverage)
    next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_mat))
    next_dose_priority[1, 15:17] <- 1 # prioritize 3 oldest age groups for next dose
    
    # base parameters
    parameters <- safir::get_parameters(
      population = pop$n,
      contact_matrix_set = contact_mat,
      iso3c = iso3c,
      R0 = c(R0, Rt1, Rt2),
      tt_R0 = c(0, tt_Rt1, tt_Rt2),
      time_period = time_period,
      dt = dt,
      hosp_bed_capacity = hc$hosp_beds,
      ICU_bed_capacity = hc$ICU_beds,
      prob_non_severe_death_treatment = pnsdt,
      dur_R = 365
    )
    
    # vaccine parameters
    ab_parameters <- get_vaccine_ab_titre_parameters(vaccine = "Pfizer", max_dose = vaccine_doses, correlated = TRUE)
    
    # combine parameters and verify
    parameters <- make_vaccine_parameters(
      safir_parameters = parameters,
      vaccine_ab_parameters = ab_parameters,
      vaccine_set = vaccine_set,
      dose_period = dose_period,
      strategy_matrix = vaccine_coverage_mat,
      next_dose_priority_matrix = next_dose_priority
    )
    
    # create variables
    timesteps <- parameters$time_period/dt

    # run the simulation
    saf_reps <- mclapply(X = 1:nrep, FUN = function(x){
      
      # creates the categorical states and ages for the simulated population
      variables <- create_variables(pop = pop, parameters = parameters)
      variables <- create_vaccine_variables(variables = variables, parameters = parameters)
      
      # creates the list of events and attaches listeners which handle state changes and queue future events
      events <- create_events(parameters = parameters)
      events <- create_events_vaccination(events = events, parameters = parameters)
      attach_event_listeners(variables = variables, events = events, parameters = parameters, dt = dt)
      attach_event_listeners_vaccination(variables = variables,events = events,parameters = parameters,dt = dt)
      
      # renderer object is made
      renderer <- individual::Render$new(parameters$time_period)
      vaxx_renderer <- individual::Render$new(parameters$time_period)
      
      # processes
      processes <- list(
        vaccine_ab_titre_process(parameters = parameters,variables = variables,events = events, dt = dt),
        vaccination_process(parameters = parameters,variables = variables,events = events, dt = dt),
        infection_process_vaccine_cpp(parameters = parameters,variables = variables,events = events, dt = dt),
        categorical_count_renderer_process_daily(renderer = renderer, variable = variables$states, categories = variables$states$get_categories(),dt = dt),
        integer_count_render_process_daily(renderer = vaxx_renderer, variable = variables$dose_num, margin = 1:4, dt = dt)
      )
      
      # schedule events for individuals at initialisation
      setup_events_vaccine(parameters = parameters,
                           events = events,
                           variables = variables,
                           dt = dt)
      
      simulation_loop_vaccine(
        variables = variables,
        events = events,
        processes = processes,
        timesteps = timesteps
      )
      df <- renderer$to_dataframe()
      df_vacc <- vaxx_renderer$to_dataframe()
      df_vacc$repetition <- x
      df$repetition <- x
      df <- left_join(df, df_vacc, by = c("timestep", "repetition"))
      return(df)
    })
    
    # bind results
    saf_reps <- do.call(rbind,saf_reps)
    
    # summarise
    saf_reps_summarise <- saf_reps %>%
      mutate(IMild_count = IMild_count + IAsymp_count) %>%
      select(-IAsymp_count) %>%
      pivot_longer(cols = contains("count"), names_to = "compartment") %>%
      filter(compartment %in% c("D_count", "X1_count", "X2_count", "X3_count")) %>%
      group_by(compartment, repetition) %>%
      mutate(value = value - lag(value),
             value = if_else(is.na(value), 0, value)) %>%
      ungroup() %>%
      pivot_wider(id_cols = c(timestep, repetition,), names_from = "compartment", values_from = "value") %>%
      group_by(repetition) %>%
      mutate(deaths = sum(D_count),
             vaccines = sum(X1_count + X2_count + X3_count)) %>%
      ungroup() %>%
      mutate(deaths_med = median(deaths),
             deaths_lower = quantile(deaths, 0.025),
             deaths_upper = quantile(deaths, 0.975),
             vaccines_med = median(vaccines)) %>%
      group_by(timestep, deaths_med, deaths_lower, deaths_upper, vaccines_med) %>%
      summarise(deaths_t = median(D_count),
                deaths_tmin = quantile(D_count, 0.025),
                deaths_tmax = quantile(D_count, 0.975),
                vaccines_t = median(X1_count + X2_count + X3_count),
                .groups = 'drop') %>%
      nest(cols = c(timestep, deaths_t, deaths_tmin, deaths_tmax, vaccines_t)) %>%
      mutate(scenario = scenario)
    
    return(saf_reps_summarise)
  }
