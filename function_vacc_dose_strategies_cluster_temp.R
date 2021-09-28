# main running function
run_scenario <- 
  function(scenario = NA,
           target_pop = 50e5,
           income_group = "HIC",
           hs_constraints = "Present",
           R0 = 2,
           Rt1 = 1.1,
           Rt2 = 2.5,
           tt_Rt1 = 30,
           tt_Rt2 = 200,
           tt_Rt1_stop = 180,
           vacc_start = 30,
           time_period = 365,
           vaccine_doses = 2,
           max_coverage = 0.8,
           age_groups_covered = 4,
           dt = 1,
           repetition = 1,
           seeding_cases = seeding_cases,
           variant_fold_reduction = 1,
           dose_3_fold_increase = 1){
    
    stopifnot(all(c(tt_Rt1, tt_Rt2) < time_period))

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
    
    # transmission
    # slope change points in terms of numeric days
    t1 <- tt_Rt1
    t2 <- tt_Rt1_stop - tt_Rt1 + 1
    t3 <- tt_Rt2 - tt_Rt1_stop + 1
    t4 <- time_period - tt_Rt2+ 1
    
    # first piecewise element: initial decay
    R0_seq1 <- seq(from = 0, to = t1, by = 1)
    R0_vseq1 <- approx(x = c(0, t1), y = c(R0, Rt1), xout = R0_seq1)$y
    
    # second piecewise element: suppression
    R0_seq2 <- seq(from = t1, to = tt_Rt1_stop, by = 1)
    R0_vseq2 <- approx(x = c(t1, tt_Rt1_stop), y = c(Rt1, Rt1), xout = R0_seq2)$y
    
    # third piecewise element: second wave
    R0_seq3 <- seq(from = tt_Rt1_stop, to = tt_Rt2, by = 1)
    R0_vseq3 <- approx(x = c(tt_Rt1_stop, tt_Rt2), y = c(Rt1, Rt2), xout = R0_seq3)$y
    
    # stick them together for get_parameters to turn into time series of beta
    tt_R0_vec <- c(R0_seq1, R0_seq2, R0_seq3)
    R0_vec <- c(R0_vseq1, R0_vseq2, R0_vseq3)

    # base parameters
    parameters <- safir::get_parameters(
      population = pop$n,
      contact_matrix_set = contact_mat,
      iso3c = iso3c,
      R0 = R0_vec,
      tt_R0 = tt_R0_vec,
      time_period = time_period,
      dt = dt,
      hosp_bed_capacity = hc$hosp_beds,
      ICU_bed_capacity = hc$ICU_beds,
      prob_non_severe_death_treatment = pnsdt,
      dur_R = 365,
      seeding_cases = seeding_cases,
      lambda_external = rep(0.00001, time_period)
    )
    
    # VACCINE PARAMETERS
    # dosing
    
    vax_pars <- get_vaccine_pars(variant_fold_reduction = variant_fold_reduction,
                                 dose_3_fold_increase = dose_3_fold_increase,
                                 vaccine_doses = vaccine_doses)
    
    if (vaccine_doses == 2) {dose_period <- c(NaN, 28)}
    if (vaccine_doses == 3) {dose_period <- c(NaN, 28, 268)}
    
    # doses available each day
    doses_per_day <- floor(sum(pop$n) * 0.02 / 7)
    # check how many days it takes to vaccinate desired age groups with 2 doses
    coverage <- sum(pop$n[(17 - age_groups_covered + 1):17])/sum(pop$n)
    days_to_vacc <- floor(coverage / (0.02/7) * max_coverage * 2)
    days_to_boost <- floor(days_to_vacc/2)
    # vaccine vector: vector of length time_period
    vaccine_set <- c(rep(0, vacc_start), rep(doses_per_day, days_to_vacc), rep(0, 268 - days_to_vacc), rep(doses_per_day, days_to_boost), rep(0, time_period - vacc_start - 268 - days_to_boost))
    #vaccine_set <- c(rep(0, vacc_start), rep(doses_per_day, time_period - vacc_start))

    # safir can either use a single strategy matrix for all vaccine doses or a list with a matrix for each dose,
    # we'll use a list for this example.
    vaccine_coverage_strategy <- list()
    vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
    vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
    
    if (vaccine_doses == 3) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
    vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]}
    
 if (vaccine_doses == 2) {
   next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
 } else if (vaccine_doses == 3) {
   next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
   next_dose_priority[,(17 - age_groups_covered + 1):17] <- 1
   
 }


    
    # if (vaccine_doses == 3) {
    #   next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1,ncol = ncol(vaccine_coverage_strategy[[1]]))
    #   next_dose_priority[1:nrow(next_dose_priority), 14:17] <- 1 # prioritize 3 oldest age groups for next dose
    # }
    
    # combine parameters and verify
    parameters <- make_vaccine_parameters(
      safir_parameters = parameters,
      vaccine_ab_parameters = vax_pars,
      vaccine_set = vaccine_set,
      dose_period = dose_period,
      strategy_matrix = vaccine_coverage_strategy,
      next_dose_priority_matrix = next_dose_priority
    )
    
    # create variables
    timesteps <- parameters$time_period/dt

    # run the simulation
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
      df$scenario <- scenario
      df <- left_join(df, df_vacc, by = c("timestep"))

    # summarise
    saf_reps_summarise <- df %>%
      mutate(IMild_count = IMild_count + IAsymp_count) %>%
      select(-IAsymp_count) %>%
      pivot_longer(cols = contains("count"), names_to = "compartment") %>%
      filter(compartment %in% c("D_count", "X1_count", "X2_count", "X3_count", "R_count")) %>%
      group_by(compartment) %>%
      mutate(value = if_else(compartment == "D_count", value - lag(value), value),
             value = if_else(is.na(value), 0, value)) %>%
      ungroup() %>%
      pivot_wider(id_cols = timestep, names_from = "compartment", values_from = "value")  %>%
      mutate(deaths = sum(D_count)) %>%
      ungroup() %>%
      nest(cols = c(timestep, D_count, X1_count, X2_count, X3_count, R_count)) %>%
      mutate(scenario = scenario)
    
    # get prop in Recovered when vaccination starts
    prop_R <- df %>%
      select(timestep, R_count) %>%
      filter(timestep == vacc_start) %>%
      mutate(prop_R = round(R_count/target_pop * 100,2)) %>%
      select(prop_R) %>%
      mutate(scenario = scenario)
    
    saf_reps_summarise <- left_join(saf_reps_summarise, prop_R)
    
    # Save output
    output_address <- paste0("output_cluster/scenario_", scenario, ".rds")
    saveRDS(saf_reps_summarise, output_address)

  }
