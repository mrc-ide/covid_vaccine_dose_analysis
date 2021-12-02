# main running function
run_scenario_add <- 
  function(scenario = 1,
           target_pop = 1e6,
           income_group = "HIC",
           hs_constraints = "Present",
           vacc_start = "1/1/2021",
           vaccine_doses = 2,
           vaccine = "Pfizer",
           max_coverage = 0.8,
           age_groups_covered = 14,
           dt = 0.2,
           repetition = 1,
           seeding_cases = 10,
           variant_fold_reduction = 1,
           dose_3_fold_increase = 1,
           age_groups_covered_d3 = 14,
           vacc_per_week = 0.05,
           name = "scenario1",
           ab_model_infection = TRUE,
           std10 = 0.44,
           t_d3 = 180,
           period_s = 115.8142,
           t_period_l = 357.6847,
           max_dose = 3,
           mu_ab_infection = 1.9,
           strategy = "realistic",
           max_Rt = 3){
    
    # set up transmission
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
    #R0_t2 <- as.Date(x = "5/1/2020", format = "%m/%d/%Y")
    R0_t2 <- as.Date(x = "4/15/2020", format = "%m/%d/%Y")
    R0_t3 <- as.Date(x = "11/1/2020", format = "%m/%d/%Y")
    R0_t4 <- as.Date(x = "12/1/2020", format = "%m/%d/%Y")
    R0_t5 <- as.Date(x = "1/1/2021", format = "%m/%d/%Y")
    R0_t6 <- as.Date(x = "2/1/2021", format = "%m/%d/%Y")
    #R0_t7 <- as.Date(x = "6/1/2021", format = "%m/%d/%Y")
    R0_t7 <- as.Date(x = "8/1/2021", format = "%m/%d/%Y")
    
    R0_t8 <- as.Date(x = "12/31/2021", format = "%m/%d/%Y")

    tmax_date <- as.Date(x = "12/31/2022", format = "%m/%d/%Y")
    time_period <- as.integer(difftime(tmax_date, R0_t0 - 1))
    # get index for 1 Jan 2022, as don't want to vacc any children <10y before this time
    t_10y_start <- as.integer(difftime(as.Date("01/01/2022", format = "%m/%d/%Y"), R0_t0-1))
    dates <- c(R0_t0, R0_t1, R0_t2, R0_t3, R0_t4, R0_t5, R0_t6, R0_t7, R0_t8)
    rt <-    c(2.5,   2.5,   0.8,   0.8,   1.05,   1.05,   1.0,   1.0,  max_Rt)
    rt_out <- safir::interpolate_rt(dates = dates, rt = rt, max_date = tmax_date)
    
    vacc_start <- as.Date(x = vacc_start, format = "%m/%d/%Y")
    days_to_vacc_start <- as.integer(difftime(vacc_start, R0_t0))
    
    # daily per-capita prob of external infection
    lambda_external <- rep(0.000001, time_period)
    
    # first pulse, spread out hazard of 0.05 over 10 days right before 2nd wave
    t_spread <- 10
    lambda_tt <- as.integer(difftime(R0_t3, R0_t0 - 1))
    lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
    lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
    lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.05

    # second pulse, spread out hazard of 0.01 over 20 days right before 2nd wave
    #t_spread <- 20
    t_spread <- 10
    lambda_tt <- as.integer(difftime(R0_t7, R0_t0 - 1))
    lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
    lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
    lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.01

    # BASE PARAMETERS
    
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

    # base parameters
    parameters <- safir::get_parameters(
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
    # doses available each day
    doses_per_day <- floor(sum(pop$n) * vacc_per_week / 7)
    
    vaccine_out <- get_vaccine_strategy(strategy, days_to_vacc_start = days_to_vacc_start, doses_per_day = doses_per_day, time_period = time_period, max_coverage = max_coverage, age_groups_covered = age_groups_covered, age_groups_covered_d3 = age_groups_covered_d3, vaccine_doses = vaccine_doses, pop = pop, vacc_per_week = vacc_per_week, t_d3 = t_d3, t_10y_start = t_10y_start)
    
    vaccine_set <- vaccine_out$vaccine_set
    vaccine_coverage_strategy <- vaccine_out$vaccine_coverage_strategy
    next_dose_priority <- vaccine_out$next_dose_priority
    t_d3 <- vaccine_out$t_d3
    
    # profiles and dosing

    
    vax_pars <- get_vaccine_pars(vaccine = vaccine,
                                variant_fold_reduction = variant_fold_reduction,
                                dose_3_fold_increase = dose_3_fold_increase,
                               vaccine_doses = vaccine_doses,
                                std10 = std10,
                                t_d3 = t_d3,
                                period_s = period_s,
                                t_period_l = t_period_l
                                )
    
    # dosing
    if (vaccine_doses == 2) {dose_period <- c(NaN, 28)}
    if (vaccine_doses == 3) {dose_period <- c(NaN, 28, (t_d3 + 28))}
    
    # combine parameters and verify
    parameters <- make_vaccine_parameters(
      safir_parameters = parameters,
      vaccine_ab_parameters = vax_pars,
      vaccine_set = vaccine_set,
      dose_period = dose_period,
      strategy_matrix = vaccine_coverage_strategy,
      next_dose_priority_matrix = next_dose_priority
    )
    
    if (ab_model_infection == TRUE){
      parameters$mu_ab_infection <- mu_ab_infection
      
    }
    
######################################################
    # run the simulation
######################################################
    
    # create variables
    timesteps <- parameters$time_period/dt
    # creates the categorical states and ages for the simulated population
    variables <- create_variables(pop = pop, parameters = parameters)
    variables <- create_vaccine_variables(variables = variables, parameters = parameters)
    
    if (ab_model_infection == TRUE){
    variables <- create_natural_immunity_variables(variables = variables, parameters = parameters)
    }
      
    # creates the list of events and attaches listeners which handle state changes and queue future events
    events <- create_events(parameters = parameters)
    events <- create_events_vaccination(events = events, parameters = parameters)
    attach_event_listeners(variables = variables, events = events, parameters = parameters, dt = dt)
    attach_event_listeners_vaccination(variables = variables,events = events,parameters = parameters,dt = dt)
    if (ab_model_infection == TRUE){
      
    attach_event_listeners_natural_immunity(variables = variables, events = events, parameters = parameters, dt = dt, additive = TRUE)
    }
      
    # renderer object is made
    renderer <- individual::Render$new(parameters$time_period)
    vaxx_renderer <- individual::Render$new(parameters$time_period)
    inf_renderer <- individual::Render$new(parameters$time_period)
    incidence_renderer <- individual::Render$new(timesteps)
    attach_tracking_listener_incidence(events=events, renderer = incidence_renderer)
    
    
   # processes
    if (ab_model_infection == TRUE) {
      processes <- list(
         natural_immunity_ab_titre_process(parameters = parameters,variables = variables, dt = dt),
        vaccination_process(parameters = parameters,variables = variables,events = events, dt = dt),
        infection_process_vaccine_cpp(parameters = parameters,variables = variables,events = events, dt = dt),
        categorical_count_renderer_process_daily(renderer = renderer, variable = variables$states, categories = variables$states$get_categories(),dt = dt),
        integer_count_render_process_daily(renderer = vaxx_renderer, variable = variables$dose_num, margin = 1:4, dt = dt)
        
      )
    } else {
      processes <- list(
        # natural_immunity_ab_titre_process(parameters = parameters,variables = variables, dt = dt),
        vaccination_process(parameters = parameters,variables = variables,events = events, dt = dt),
        infection_process_vaccine_cpp(parameters = parameters,variables = variables,events = events, dt = dt),
        categorical_count_renderer_process_daily(renderer = renderer, variable = variables$states, categories = variables$states$get_categories(),dt = dt),
        integer_count_render_process_daily(renderer = vaxx_renderer, variable = variables$dose_num, margin = 1:4, dt = dt))
    }

      
      setup_events(parameters = parameters, events=events, variables = variables, dt=dt)
  
      simulation_loop_safir(
        variables = variables,
        events = events,
        processes = processes,
        timesteps = timesteps,
        variables_dont_update = c("discrete_age", "phase"),
        progress = TRUE
      )
      
      df <- renderer$to_dataframe()
      df_vacc <- vaxx_renderer$to_dataframe()
      df_inc <- incidence_renderer$to_dataframe()
      
      df_inc <- df_inc %>%
        mutate(timestep = floor((timestep-1)*dt)) %>% 
        mutate(incidence = coalesce(incidence,0))%>%
        group_by(timestep) %>%
        summarise(incidence = sum(incidence))
                  
      df <- left_join(df, df_vacc, by = c("timestep"))
      df <- left_join(df, df_inc, by = c("timestep"))
      df_rt <- as.data.frame(rt_out) %>%
        rename("timestep" = "Rt_tt")
      df <- left_join(df, df_rt, by = c("timestep"))
      
    # summarise
    saf_reps_summarise <- df %>%
      mutate(IMild_count = IMild_count + IAsymp_count) %>%
      select(-IAsymp_count) %>%
      pivot_longer(cols = contains(c("count", "Rt","incidence")), names_to = "compartment") %>%
      
      filter(compartment %in% c("D_count", "X1_count", "X2_count", "X3_count", "R_count", "IMild_count", "ICase_count", "Rt", "incidence")) %>%
      group_by(compartment) %>%
      mutate(value = if_else(compartment == "D_count", value - lag(value), value),
             value = if_else(is.na(value), 0, value)) %>%
      ungroup() %>%
      pivot_wider(id_cols = timestep, names_from = "compartment", values_from = "value")  %>%
      mutate(deaths = sum(D_count[timestep >= days_to_vacc_start]),
             inc = sum(incidence[timestep >= days_to_vacc_start]),
             doses = X1_count + X2_count * 2 + X3_count * 3,
             total_doses = max(doses)) %>%
      ungroup() %>%
      nest(cols = c(timestep, D_count, X1_count, X2_count, X3_count, R_count, IMild_count, ICase_count, doses, Rt, incidence)) %>%
      mutate(scenario = scenario)
    
    # get prop in Recovered when vaccination starts
    prop_R <- df %>%
      select(timestep, R_count) %>%
      filter(timestep == days_to_vacc_start) %>%
      mutate(prop_R = round(R_count/target_pop * 100,2)) %>%
      select(prop_R) %>%
      mutate(scenario = scenario)
    
    saf_reps_summarise <- left_join(saf_reps_summarise, prop_R, by = "scenario")
    
    # Save output
    output_address <- paste0("raw_outputs/output_", name, "/scenario_", scenario, ".rds")
    saveRDS(saf_reps_summarise, output_address)

  }
