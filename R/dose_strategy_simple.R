coverage_all <- sum(pop$n[(17 - 14 + 1):17])/sum(pop$n)

# how many days to vaccinate everyone with first dose?
days_to_vacc_1d <- floor(coverage_all / (0.025/7) * max_coverage)

# check how many days it takes to vaccinate desired age groups with 2 doses
days_to_vacc_2d <- floor(coverage / (0.025/7) * max_coverage)
days_to_boost <- days_to_vacc_2d
days_to_finish_2d <- floor((coverage_all - coverage) / (0.025/7) * max_coverage)
days_phase_3 <- min(days_to_boost, days_to_finish_2d)

vacc_start <- as.Date(x = vacc_start, format = "%m/%d/%Y")
days_to_vacc_start <- as.integer(difftime(vacc_start, R0_t0))
# vaccine vector: vector of length time_period
vaccine_set <- c(rep(0, days_to_vacc_start),
                 rep(doses_per_day, days_to_vacc_1d),
                 rep(doses_per_day, days_to_vacc_2d),
                 rep(0, 268 - days_to_vacc_2d),
                 rep(doses_per_day, days_phase_3),
                 rep(0, time_period - days_to_vacc_start - days_to_vacc_1d - 268 - days_phase_3))

# safir can either use a single strategy matrix for all vaccine doses or a list with a matrix for each dose,
# we'll use a list for this example.
vaccine_coverage_strategy <- list()
vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly", max_coverage = max_coverage)[1:14,]
vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly", max_coverage = max_coverage)[1:14,]

if (vaccine_doses == 3) {
  vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:14,]}

if (vaccine_doses == 2) {
  next_dose_priority <- matrix(data = 0, nrow = 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
} else if (vaccine_doses == 3) {
  next_dose_priority <- matrix(data = 0, nrow = 2, ncol = ncol(vaccine_coverage_strategy[[1]]))
  next_dose_priority[2,(17 - age_groups_covered + 1):17] <- 1
}