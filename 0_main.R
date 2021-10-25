# Analysis for a paper investigating vaccination scenarios for a COVID-19 booster dose, using safir

# 'scenario1' and 'scenario2' refer to research questions 1 and 2 respectively, for HIC and UMIC settings
# 'scenario5' and 'scenario6' refer to research questions 1 and 2 respectively, for LMIC and LIC settings

# set up scenarios and perform runs on cluster
source("1_run_dose_strategies_scenario1.R")
source("1_run_dose_strategies_scenario2.R")
source("1_run_dose_strategies_scenario5.R")
source("1_run_dose_strategies_scenario6.R")

# postprocess and create some "checking" plots
source("2_postprocess_scenario1.R")
source("2_postprocess_scenario2.R")
source("2_postprocess_scenario5.R")
source("2_postprocess_scenario6.R")

# plot impact for each research question/income setting combination
source("3_plots_impact_scenario1.R")
source("3_plots_impact_scenario2.R")
source("3_plots_impact_scenario5.R")
source("3_plots_impact_scenario6.R")

# plot dosing strategies for each research question
source("4_plots_doses_scenario1_scenario5.R")
source("4_plots_doses_scenario2_scenario6.R")
