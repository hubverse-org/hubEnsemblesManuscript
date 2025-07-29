#' Reformat a data frame of forecasts stored in the zoltar format to that of a
#' `model_output_tbl` object defined by the hubverse
#'
#' @param model_outputs a data frame of forecasts formatted according to zoltar
#'   standards with the following columns: model, forecast_date, location, horizon,
#'   temporal_resolution, target_variable, target_end_date, type, quantile, value.
#'
#' @return a `model_out_tbl` of reformatted forecasts with the following columns:
#'   model_id, forecast_date, location, horizon, target, target_end_date,
#'   output_type, output_type_id, value.
#' @export
#'
#' @examples
#' @importFrom rlang .data

zoltar_to_model_out <- function(zoltar_forecasts) {
  task_ids <- c("forecast_date", "location", "horizon", "target", "target_end_date")
  zoltar_forecasts |>
    dplyr::rename(forecast_date = .data[["timezero"]], location = .data[["unit"]]) |>
    tidyr::separate(
      .data[["target"]], sep = " ", convert = TRUE,
      into = c("horizon", "target"), extra = "merge"
    ) |>
    dplyr::mutate(
      target_end_date = lubridate::ceiling_date(.data[["forecast_date"]], "weeks") - lubridate::days(1)
    ) |>
    hubUtils::as_model_out_tbl(
      model_id_col = "model",
      output_type_col = "class",
      output_type_id_col = "quantile",
      value_col = "value",
      sep = "-",
      trim_to_task_ids = FALSE,
      hub_con = NULL,
      task_id_cols = task_ids,
      remove_empty = TRUE
    )
}
