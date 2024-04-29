library(dplyr)
library(purrr)
library(lubridate)
library(readr)
source("R/evaluation_functions.R")
source("R/as_covid_hub_forecasts.R")

# Read in data
flu_truth_all <- readr::read_rds("analysis/data/raw_data/flu_truth_all.rds")
flu_baseline_all <- readr::read_rds("analysis/data/raw_data/flu_baseline_hub.rds") |>
  as_covid_hub_forecasts()

flu_files <- list.files(path="analysis/data/raw_data", pattern="hub", full.names=TRUE)
flu_forecasts_ensembles <- purrr::map_dfr(flu_files, .f=read_rds) |>
  as_covid_hub_forecasts() |>
  dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                target_variable, target_end_date, type, quantile, value)

# Score Forecasts
flu_scores_baseline <- flu_baseline_all |>
  covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)
readr::write_rds(flu_scores_baseline, "analysis/data/raw_data/flu_scores_baseline.rds", "xz", compression = 9L)

flu_scores_ensembles <- flu_forecasts_ensembles |> 
covidHubUtils::score_forecasts(flu_truth_all, return_format="wide", use_median_as_point=TRUE)
readr::write_rds(flu_scores_ensembles, "analysis/data/raw_data/flu_scores_ensembles.rds", "xz", compression = 9L)
