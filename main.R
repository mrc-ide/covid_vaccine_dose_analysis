# Analysis of impact of COVID-19 booster doses on epidemic dynamics in different global settings, using the individual-based model of SARS-CoV-2 transmission, "safir"
# Authors: AB Hogan, SL Wu, AC Ghani, P Doohan, P Winskill, OJ Watson
# Date: 8 December 2021

library(safir)
library(squire)
library(nimue)
library(data.table)
library(ggplot2)
library(parallel)
library(tidyverse)
library(countrycode)
library(furrr)
library(zoo)

source("R/plotting_utils.R")

# Run counterfactual to quantify deaths to end-2020 in starting trajectory
source("1_delta_analysis/1_run_counterfactual.R")
source("1_delta_analysis/2_postprocess_counterfactual.R")

# Run and process Delta analysis for each research question/category (make sure runs are complete before running the postprocessing scripts)
source("1_delta_analysis/1_run_rq1_hic.R")
source("1_delta_analysis/1_run_rq1_hic_age.R")
source("1_delta_analysis/1_run_rq2_lmic.R")
source("1_delta_analysis/1_run_rq3_hic.R")

source("1_delta_analysis/2_postprocess_rq1_hic.R")
source("1_delta_analysis/2_postprocess_rq1_hic_age.R")
source("1_delta_analysis/2_postprocess_rq2_lmic.R")
source("1_delta_analysis/2_postprocess_rq3_hic.R")

# Run and process Omicron analysis for each research question/category
source("1_omicron_analysis/1_run_rq1_hic_omicron_heatmap.R")
source("1_omicron_analysis/1_run_rq1_hic_omicron.R")
source("1_omicron_analysis/1_run_rq2_lmic_omicron.R")
source("1_omicron_analysis/1_run_rq3_hic_omicron.R")

source("1_omicron_analysis/2_postprocess_rq1_hic_omicron_heatmap.R")
source("1_omicron_analysis/2_postprocess_rq1_hic_omicron.R")
source("1_omicron_analysis/2_postprocess_rq2_lmic_omicron.R")
source("1_omicron_analysis/2_postprocess_rq3_hic_omicron.R")

# Create figures
source("2_create_figures/figure_2.R")
source("2_create_figures/figure_3.R")
source("2_create_figures/figure_4.R")
source("2_create_figures/figure_rq3_doses.R")
source("2_create_figures/plot_Rt.R")