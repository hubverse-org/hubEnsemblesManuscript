library(dplyr)
library(lubridate)
library(readr)
library(hubEvals)
source("R/zoltar_to_model_out.R")

# Read in data
flu_target_data <- readr::read_rds("analysis/data/raw_data/flu_truth-zoltar.rds")
flu_time_series <- flu_target_data |>
  rename(location = unit, forecast_date = timezero, observation = value) |>
  mutate(
    target = "wk ahead inc flu hosp",
    target_end_date = ceiling_date(forecast_date, "weeks") - days(1)
  ) |>
  select(target, target_end_date, location, observation)
readr::write_rds(flu_time_series, "analysis/data/derived_data/flu_time_series.rds", "xz", compression = 9L)

flu_oracle_output <- flu_time_series |>
  mutate(output_type = "quantile", output_type_id = NA) |>
  rename(oracle_value = observation)
readr::write_rds(flu_oracle_output, "analysis/data/derived_data/flu_oracle_output.rds", "xz", compression = 9L)

flu_baseline_forecasts <- readr::read_rds("analysis/data/raw_data/flu_baseline-zoltar.rds") |>
  zoltar_to_model_out()

flu_ensembles_forecasts <- list.files(path = "analysis/data/derived_data", pattern = "forecasts.rds", full.names = TRUE) |> 
  purrr::map(.f = read_rds) |>
  purrr::list_rbind()

flu_component_files <- list.files(path = "analysis/data/raw_data", pattern = "forecasts-zoltar", full.names = TRUE)
flu_component_forecasts <- purrr::map(flu_component_files, .f = read_rds) |>
  purrr::list_rbind() |>
  filter(!model %in% c("Flusight-baseline", "Flusight-ensemble")) |>
  zoltar_to_model_out()


# Score Forecasts

flu_baseline_ensembles_scores <- flu_baseline_forecasts |>
  rbind(flu_ensembles_forecasts) |>
  hubEvals::score_model_out(
    flu_oracle_output,
    metrics = c("wis", "overprediction", "underprediction", "dispersion", "bias", "ae_median", "interval_coverage_50", "interval_coverage_95"),
    relative_metrics = c("wis", "ae_median"),
    baseline = "Flusight-baseline",
    summarize = FALSE
  )
readr::write_rds(flu_baseline_ensembles_scores, "analysis/data/derived_data/flu_baseline_ensembles_scores.rds", "xz", compression = 9L)

flu_component_scores <- flu_baseline_forecasts |>
  rbind(flu_component_forecasts) |>
  score_model_out(
    flu_oracle_output,
    metrics = c("wis", "overprediction", "underprediction", "dispersion", "bias", "ae_median", "interval_coverage_50", "interval_coverage_95"),
    relative_metrics = c("wis", "ae_median"),
    baseline = "Flusight-baseline",
    summarize = FALSE
  ) |>
  filter(!model_id %in% c("Flusight-baseline", "Flusight-ensemble"))
readr::write_rds(flu_component_scores, "analysis/data/derived_data/flu_component_scores.rds", "xz", compression = 9L)
