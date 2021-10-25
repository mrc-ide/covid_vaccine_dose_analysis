get_vaccine_strategy <- function(name, days_to_vacc_start, doses_per_day, time_period, max_coverage, age_groups_covered, age_groups_covered_d3 = NA, vaccine_doses, pop, vacc_per_week){
  
  if (name %in% c("scenario1", "scenario5")) {
    
    # now we want to simulate a country having vaccinated all of the eligible population with the first and second dose (15 years + ) before switching to a booster dose
    vaccine_set <- c(rep(0, days_to_vacc_start), rep(doses_per_day, time_period - days_to_vacc_start))
    
    vaccine_coverage_strategy <- list()
    
    if (vaccine_doses == 2) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
    } else if (vaccine_doses == 3) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered_d3,]}
    
    next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
    
    # don't prioritise anyone for a booster until all population has received primary series
    if (vaccine_doses == 3){
      next_dose_priority[2,] <- 0
    }
  }
  
  if (name %in% c("scenario2", "scenario6")){
    
    coverage <- sum(pop$n[(17 - age_groups_covered + 1):17])/sum(pop$n)
    days_to_vacc <- floor(coverage / (vacc_per_week/7) * max_coverage * 2)
    days_to_boost <- floor(days_to_vacc)
      
    if (days_to_vacc < 28) {days_to_vacc = 28}
      
      vaccine_set <- c(rep(0, days_to_vacc_start),
                       rep(doses_per_day/2, 28),
                       rep(doses_per_day, floor(days_to_vacc - 28)),
                       rep(doses_per_day/2, 28),
                       rep(0, 268 - days_to_vacc + 28),
                       rep(doses_per_day/2, days_to_boost),
                       rep(0, time_period - days_to_vacc_start - 268 -28 -28 - days_to_boost))
      
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
  }

  return(list(vaccine_set = vaccine_set, vaccine_coverage_strategy = vaccine_coverage_strategy, next_dose_priority = next_dose_priority))
  
}