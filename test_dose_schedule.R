weighted_eff <- function(maxt,
                         population_size,
                         rel_dose_supply,
                         dose_gap,
                         strategy){
  
  rep_country <- "United Kingdom"
  target_pop <- 1e6
  pop <- squire::get_population(country = rep_country)
  pop_standardise <- target_pop / sum(pop$n)
  pop$n <- as.integer(pop$n * pop_standardise)
  strategy <- "prioritise_2d"
  
  dose_gap <- 28
  
  # doses to administer
  doses_to_give <- sum(pop$n[(17 - age_groups_covered + 1):17]) * 2
  
  maxt <- 250
  
  doses_per_day <- rep(0,maxt)
  doses_per_day[1:maxt] <- floor(sum(pop$n) * 0.025 / 7)
  
  # initialise matrices: for each matrix, rows represent time points going forward, and each column represents the prior timestep.
  # So at time i, row i in each matrix represents the number of individuals currently vaccinated, where each column represents how long ago they were vaccinated
  
  d1 <- matrix(data = 0, nrow = maxt, ncol = maxt)
  d2 <- d1
  
  d1[1,1] <- doses_per_day[1] # allocate the first doses
  
  # loop over time
  for (i in 2:maxt){
    print(sum(d1[i-1,]) + sum(d2[i-1,]*2))
    if ((sum(d1[i-1,]) + sum(d2[i-1,]*2)) < doses_to_give){ 
    # move previously delivered doses back a time step
    for (j in 2:i) {
      d1[i,j] <- d1[i-1,j-1]
      d2[i,j] <- d2[i-1,j-1]
    }
    
    # deliver new doses
    if (i <= dose_gap){ # if within first X days - all new doses are first doses
      d1[i,1] <- doses_per_day[i]
    } else if (i > dose_gap) { # if after first X days, can either prioritise 2nd doses, or keep allocating 1st doses
      if (strategy == "prioritise_1d"){
        d1[i,1] <- doses_per_day[i] # note: have not yet built in to start giving out second doses after the 1st dose population is fully vaccinated. nor have I built in a check to stop vaccinated after everyone has received both doses
      } else if (strategy == "prioritise_2d"){
        
        # check how many people available to receive second dose
        avail_d2 <- sum(d1[i, (dose_gap+1):ncol(d1)])
        # if less than supply, give all those people their second dose, and give the rest as first doses
        if (avail_d2 <= doses_per_day[i]){
          d2[i,1] <- avail_d2
          d1[i,(dose_gap+1):ncol(d1)] <- 0
          d1[i,1] <- doses_per_day[i] - avail_d2
          # if more than supply, give people vaccinated the longest time ago their second dose
        } else if (avail_d2 > doses_per_day[i]){
          dose_rem <- doses_per_day[i]
          d2[i,1] <- doses_per_day[i]
          for (j in i:(dose_gap+1)) {
            if (d1[i,j] <= dose_rem){
              dose_rem <- dose_rem - d1[i,j]
              d1[i,j] <- 0
            } else if (d1[i,j] > dose_rem){
              d1[i,j] <- d1[i,j] - dose_rem
              dose_rem <- 0
            }
          }
          
        }
      }
    }
    } else {
    break
  }
  }
  # calculate coverage for each row (i.e. each time point)
  v1 <- rowSums(d1)
  v2 <- rowSums(d2)
  
  # first doses delivered at each time point (for max_vaccine model input)
  mv <- round(d1[,1])
  
  
  return(data.frame(eff = eff, mv = mv))
}