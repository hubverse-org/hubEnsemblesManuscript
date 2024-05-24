library(dplyr)
library(purrr)
library(lubridate)
library(readr)
source("R/as_covid_hub_forecasts.R")

# Read in data
flu_truth <- readr::read_rds("analysis/data/raw_data/flu_truth-zoltar.rds") |>
  dplyr::rename(location = unit, forecast_date = timezero) |>
  dplyr::mutate(model = "flu-truth", target_variable = "inc flu hosp",
                target_end_date = ceiling_date(forecast_date, "weeks") - days(1)) |>
  dplyr::select(model, target_variable, target_end_date, location, value)

flu_baseline_forecasts <- readr::read_rds("analysis/data/raw_data/flu_baseline-zoltar.rds") |>
  dplyr::rename(forecast_date = timezero, location=unit) |>
  tidyr::separate(target, sep = " ", convert = TRUE,
                  into = c("horizon", "target"), extra = "merge") |>
  dplyr::mutate(target_end_date = 
                  ceiling_date(forecast_date, "weeks") - days(1)) |>
  hubUtils::as_model_out_tbl(
    model_id_col = "model",
    output_type_col = "class",
    output_type_id_col = "quantile",
    value_col = "value",
    sep = "-",
    trim_to_task_ids = FALSE,
    hub_con = NULL,
    task_id_cols = 
      c("forecast_date", "location", "horizon", "target", target_end_date),
    remove_empty = TRUE
  ) |>
  as_covid_hub_forecasts()

flu_files <- list.files(path="analysis/data/derived_data", pattern="forecasts", full.names=TRUE)
flu_ensembles_forecasts <- purrr::map_dfr(flu_files, .f = read_rds) |>
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
