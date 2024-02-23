library(zoltr)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(hubUtils)
library(hubEnsembles)

library(patchwork)
source("R/evaluation_functions.R")
source("R/as_covid_hub_forecasts.R")

# Generate ensembles
flu_dates_21_22 <- as.Date("2022-01-24") + weeks(0:21)
flu_dates_22_23 <- as.Date("2022-10-17") + weeks(0:30)
all_flu_dates <- c(flu_dates_21_22, flu_dates_22_23)

# Read in data
flu_truth_all <- readr::read_rds("data/flu_truth_all.rds")
flu_baseline_all <- readr::read_rds("data/flu_baseline_all.rds")

flu_files <- list.files(path="data", pattern="ensemble_", full.names=TRUE)
flu_forecasts_ensembles <- purrr::map_dfr(flu_files, .f=read_rds) |>
  as_covid_hub_forecasts() |>
  dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                target_variable, target_end_date, type, quantile, value)

flu_truth_21_22 <- flu_truth_all |>
  dplyr::filter(target_end_date < "2022-08-01")
flu_truth_22_23 <- flu_truth_all |>
  dplyr::filter(target_end_date > "2022-08-01")  
  

# Score Forecasts
flu_scores_baseline <- flu_baseline_all |> 
  covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)
readr::write_rds(flu_scores_baseline, "data/flu_scores_baseline.rds", "xz", compression = 9L)


flu_scores_ensembles <- flu_forecasts_ensembles |> 
covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)
readr::write_rds(flu_scores_ensembles, "data/flu_scores_ensembles.rds", "xz", compression = 9L)

