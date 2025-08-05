#' Reformat model outputs stored as a `model_output_tbl` class to that of a
#' `data.frame` formatted according to standards of the COVID-19 Forecasting
#' Hub which can be processed by functions from the `covidHubUtils` package
#' such as `score_forecasts()` or `plot_forecasts()`. The supplied
#' `model_output_tbl` should have columns defining properties akin to
#' reference dates, locations, horizons, and targets.
#'
#' @param model_outputs an object of class `model_output_tbl` with component
#'   model outputs (e.g., predictions). Should have columns containing the
#'   following information: model name, reference date, location, horizon,
#'   target, output type, output type id, and value. The target column should
#'   contain targets of the form "(temporal resolution) (target)" or
#'   "(temporal resolution) ahead (target)", such as "wk ahead inc flu hosp"
#'   "wk inc flu hosp", to properly create a temporal resolution column.
#'
#' @return a `data.frame` of reformatted model outputs that may be fed into
#'   any of the `covidHubUtils` functions with 10 total columns: model,
#'   forecast_date, location, horizon, temporal_resolution, target_variable,
#'   target_end_date, type, quantile, value.
#' @export
#'
#' @examples
#' @importFrom rlang .data

as_covid_hub_forecasts <- function(model_outputs) {
  if (all(c("mean", "median") %in% unique(model_outputs[["output_type"]]))) {
    stop("You may only have one type of point forecast.")
  }

  model_outputs <- model_outputs |>
    dplyr::rename(model = .data$model_id, target_variable = .data$target)

  model_outputs <- model_outputs |>
    dplyr::rename(target = .data$target_variable) |>
    dplyr::mutate(target = ifelse(
      stringr::str_detect(.data$target, "ahead"),
      stringr::str_replace(.data$target, "ahead", "") |>
        stringr::str_squish(), .data$target
    )) |>
    tidyr::separate(.data$target,
      sep = " ", convert = TRUE,
      into = c("temporal_resolution", "target_variable"),
      extra = "merge"
    )

  covid_hub_outputs <- model_outputs |>
    dplyr::mutate(
      type = ifelse(.data$output_type %in% c("mean", "median"), 
                    "point", .data$output_type),
      quantile = .data$output_type_id
    ) |>
    dplyr::select(.data$model, .data$forecast_date, .data$location, .data$horizon, 
                  .data$temporal_resolution, .data$target_variable, 
                  .data$target_end_date, .data$type, .data$quantile, .data$value)

  return(covid_hub_outputs)
}
