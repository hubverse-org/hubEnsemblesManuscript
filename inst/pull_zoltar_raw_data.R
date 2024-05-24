library(zoltr)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)

# Establish connection
zoltar_connection <- new_connection()
zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))

# Get projects and associated info
the_projects <- projects(zoltar_connection)
project_url <- the_projects[the_projects$name == "CDC Influenza Hospitalization Forecasts", "url"]
the_project_info <- project_info(zoltar_connection, project_url)
the_models <- models(zoltar_connection, project_url)

# Flu dates
flu_dates_21_22 <- as.Date("2022-01-24") + weeks(0:21)
flu_dates_22_23 <- as.Date("2022-10-17") + weeks(0:30)
flu_dates_all <- c(flu_dates_21_22, flu_dates_22_23)


# Get truth data
flu_truth_raw <- purrr::map_dfr(flu_dates_all, .f = function(dates_vector) {
  do_zoltar_query(zoltar_connection, project_url, query_type = "truth",
                  timezeros = dates_vector, types = "quantile")
})

readr::write_rds(flu_truth_raw, "analysis/data/raw_data/flu_truth-zoltar.rds", "xz", compression = 9L)


# Query raw forecasts
flu_forecasts_raw_21_22 <- purrr::map_dfr(flu_dates_21_22, .f = function(dates_vector) {
  do_zoltar_query(zoltar_connection, project_url, query_type = "forecasts",
                  timezeros = dates_vector, types = "quantile")
})

flu_forecasts_raw_22_23 <- purrr::map_dfr(flu_dates_22_23, .f = function(dates_vector) {
  do_zoltar_query(zoltar_connection, project_url, query_type = "forecasts",
                  timezeros = dates_vector, types = "quantile")
})

# save forecasts
readr::write_rds(flu_forecasts_raw_21_22, "analysis/data/raw_data/flu_forecasts-zoltar_21-22.rds", "xz", compression = 9L)
readr::write_rds(flu_forecasts_raw_22_23, "analysis/data/raw_data/flu_forecasts-zoltar_22-23.rds", "xz", compression = 9L)


# save baseline forecasts separately (for scoring)
rbind(flu_forecasts_raw_21_22, flu_forecasts_raw_22_23) |>
  dplyr::filter(model == "Flusight-baseline") |>
  readr::write_rds("analysis/data/raw_data/flu_baseline-zoltar.rds", "xz", compression = 9L)
