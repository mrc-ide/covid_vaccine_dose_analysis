#' @title Specify the country chosen to represent each income group
#' @param income_group "HIC", "UMIC", "LMIC" or "LIC
get_representative_country <- function(income_group){
  case_when(income_group == "HIC" ~ "Malta",
            income_group == "UMIC" ~ "Grenada",
            income_group == "LMIC" ~ "Nicaragua",
            income_group == "LIC" ~ "Madagascar")
}

#' @title Set hospital and ICU capacity
#' @param country name of the country (not ISO code)
#' @param income_group "HIC", "UMIC", "LMIC" or "LIC
#' @param pop a vector of population sizes by age group
#' @param hs_constraints "Present" or "Absent"
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

#' @title Parameterise poorer health outcomes in LMIC and LIC
#' @param income_group "HIC", "UMIC", "LMIC" or "LIC
#' @param hs_constraints "Present" or "Absent"
get_prob_non_severe_death_treatment <- function(income_group, hs_constraints){
  psdt <- squire:::probs$prob_non_severe_death_treatment
  
  if(income_group  == "LIC" & hs_constraints == "Present"){
    psdt <- c(rep(0.25, 16), 0.5804312)
  }
  return(psdt)
}

#' @title Get vaccine parameters
#' @param vaccine which vaccine? should be one of: "Pfizer", "Oxford-AstraZeneca", "Moderna" (controls the mean titre associated with each dose)
#' @param vaccine_doses how many doses?
#' @param variant_fold_reduction
#' @param dose_3_fold_increase
#' @param hl_s Half life of antibody decay - short
#' @param hl_l Half life of antibody decay - long
#' @param period_s length of the initial decay rate (shortest half-life)
#' @param t_period_l Time point at which to have switched to longest half-life
#' @param ab_50 titre relative to convalescent required to provide 50% protection from infection, on linear scale
#' @param ab_50_severe titre relative to convalescent required to provide 50% protection from severe disease, on linear scale
#' @param std10 Pooled standard deviation of antibody level on log10 data
#' @param k shape parameter of efficacy curve
#' @param mu_ab_list a data.frame
get_vaccine_pars <- function(
  vaccine = "Pfizer",
  vaccine_doses = 3,
  variant_fold_reduction = 1,
  dose_3_fold_increase = 6,
  ab_50 = 0.1126788,
  ab_50_severe = 0.02398354,
  std10 = 0.44,
  k = 3.005344,
  t_d2 = 28,
  t_d3 = 180,
  hl_s = 96.27053,
  hl_l = 535.3493,
  period_s = 118.5979,
  t_period_l = 245.6135
){
  mu_ab_list <- data.frame(name = c("Oxford-AstraZeneca", "Pfizer"),
                           mu_ab_d1 = c(0.1229219, 0.1633969),
                           mu_ab_d2 = c(32/59, 223/94),
                           fold_red_delta = c(1.979628, 3.529696)) %>%
    mutate(#mu_ab_d1 = mu_ab_d1 / fold_red_delta,
           mu_ab_d2 = mu_ab_d2 / fold_red_delta) %>%
    mutate(mu_ab_d1 = mu_ab_d1/variant_fold_reduction,
           mu_ab_d2 = mu_ab_d2/variant_fold_reduction) %>%
    mutate(mu_ab_d3 = mu_ab_d2 * dose_3_fold_increase) %>%
      select(-fold_red_delta)
  
  ab_parameters <- safir::get_vaccine_ab_titre_parameters(
    vaccine = vaccine, max_dose = vaccine_doses, correlated = FALSE,
    hl_s = hl_s, hl_l = hl_l, period_s = period_s, t_period_l = t_period_l,
    ab_50 = ab_50, ab_50_severe = ab_50_severe, std10 = std10, k = k,
    mu_ab_list = mu_ab_list
  )
  ab_parameters$max_ab <- 3 # max titre on natural log scale
  return(ab_parameters) 
}
