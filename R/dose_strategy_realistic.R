

    # VACCINE PARAMETERS
    days_to_vacc <- floor(coverage / (0.025/7) * max_coverage * 2)
    days_to_boost <- floor(days_to_vacc/2)
    
    vacc_start <- as.Date(x = vacc_start, format = "%m/%d/%Y")
    days_to_vacc_start <- as.integer(difftime(vacc_start, R0_t0))
    
    # vaccine vector: vector of length time_period
    vaccine_set <- c(rep(0, vacc_start),
                     rep(doses_per_day/2, 28),
                     rep(doses_per_day, days_to_vacc - 28),
                     rep(doses_per_day/2, 28),
                     rep(0, 268 - days_to_vacc - 28),
                     rep(doses_per_day, days_to_boost),
                     rep(0, time_period - vacc_start - 268 - days_to_boost))

    # safir can either use a single strategy matrix for all vaccine doses or a list with a matrix for each dose,
    # we'll use a list for this example.
    vaccine_coverage_strategy <- list()
    
    if (vaccine_doses == 2) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
    } else if (vaccine_doses == 3) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]}
    
    if (vaccine_doses == 2) {
      next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
    } else if (vaccine_doses == 3) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[,(17 - age_groups_covered + 1):17] <- 1
    }
    
  