library(dplyr)
library(purrr)
library(lubridate)
library(readr)
source("R/zoltar_to_model_out.R")
source("R/as_covid_hub_forecasts.R")

# Read in data
flu_truth <- readr::read_rds("analysis/data/raw_data/flu_truth-zoltar.rds") |>
  dplyr::rename(location = unit, forecast_date = timezero) |>
  dplyr::mutate(model = "flu-truth", target_variable = "inc flu hosp",
                target_end_date = ceiling_date(forecast_date, "weeks") - days(1)) |>
  dplyr::select(model, target_variable, target_end_date, location, value)

flu_baseline_forecasts <- readr::read_rds("analysis/data/raw_data/flu_baseline-zoltar.rds") |>
  zoltar_to_model_out() |>
  as_covid_hub_forecasts()

flu_ensemble_files <- list.files(path = "analysis/data/derived_data", pattern = "forecasts", full.names = TRUE)
flu_ensembles_forecasts <- purrr::map_dfr(flu_ensemble_files, .f = read_rds) |>
  as_covid_hub_forecasts() |>
  dplyr::select(model, forecast_date, location, horizon, temporal_resolution,
                target_variable, target_end_date, type, quantile, value)

flu_component_files <- list.files(path = "analysis/data/raw_data", pattern = "forecasts-zoltar", full.names = TRUE)
flu_component_forecasts <- purrr::map_dfr(flu_component_files, .f = read_rds) |>
  dplyr::filter(!model %in% c("Flusight-baseline", "Flusight-ensemble")) |>
  zoltar_to_model_out() |>
  as_covid_hub_forecasts() |>
  dplyr::select(model, forecast_date, location, horizon, temporal_resolution,
                target_variable, target_end_date, type, quantile, value)

# Score Forecasts
flu_baseline_scores <- flu_baseline_forecasts |>
  covidHubUtils::score_forecasts(flu_truth, return_format = "wide", use_median_as_point = TRUE)
readr::write_rds(flu_baseline_scores, "analysis/data/derived_data/flu_baseline_scores.rds", "xz", compression = 9L)

flu_ensembles_scores <- flu_ensembles_forecasts |>
  covidHubUtils::score_forecasts(flu_truth, return_format = "wide", use_median_as_point = TRUE)
readr::write_rds(flu_ensembles_scores, "analysis/data/derived_data/flu_ensembles_scores.rds", "xz", compression = 9L)

flu_component_scores <- flu_component_forecasts |>
  split(flu_component_forecasts$model) |>
  purrr::map(covidHubUtils::score_forecasts, flu_truth, return_format = "wide", use_median_as_point = TRUE) |>
  purrr::list_rbind()
readr::write_rds(flu_component_scores, "analysis/data/derived_data/flu_component_scores.rds", "xz", compression = 9L)
