#' Pulls truth data for incident flu hospitalizations from zoltar and
#' formats the data to be fed into `covidHubUtils::score_forecasts()`
#'
#' @param zoltar_connection a connection through which to access zoltar
#' @param project_url `character` string of the URL for these zoltar project 
#'   from which to query the component forecasts
#' @param origin_date `character` string that specifies the date on which
#'   the component forecasts were made that should be used for the ensemble
#'
#' @return a `data.frame` of truth data for flu hospitalizations with columns:
#'   model, target_variable, target_end_date, location, value
#' @export
#'
#' @examples
#' @importFrom rlang .data

get_flu_truth_single_date <- function(zoltar_connection, project_url, origin_date) {
  raw_truth <- zoltar_connection |>
    do_zoltar_query(project_url, query_type ="truth",
                    units = NULL,
                    targets = NULL,
                    timezeros = origin_date,
                    types = "quantile")

  # Format truth
  truth_data <- raw_truth |>
    dplyr::rename(location = .data$unit, forecast_date = .data$timezero) |>
    dplyr::mutate(model="flu-truth", target_variable="inc flu hosp",
                  target_end_date=
                    lubridate::ceiling_date(.data$forecast_date, "weeks")-
                      lubridate::days(1)) |>
    dplyr::select(.data$model, .data$target_variable, .data$target_end_date, .data$location, .data$value)
  
  return (truth_data)
}


#' Pulls forecasts for incident flu hospitalizations from zoltar and
#' turns the data-frame into a `model_out_tbl` object as defined by the
#' hubverse to be fed into ensembling functions from `hubEnsembles`
#'
#' @param zoltar_connection a connection through which to access zoltar
#' @param project_url `character` string of the URL for these zoltar project 
#'   from which to query the component forecasts
#' @param origin_date `character` string that specifies the date on which
#'   the component forecasts were made that should be used for the ensemble
#'
#' @return a `data.frame` of forecast data for flu hospitalizations with columns:
#'   model_id, forecast_date, location, horizon, temporal_resolution, 
#'   target_long, output_type, output_type_id, value
#' @export
#'
#' @examples
#' @importFrom rlang .data

get_flu_forecasts_single_date <- function(zoltar_connection, project_url, origin_date) {
  raw_forecasts <- zoltar_connection |>
    do_zoltar_query(project_url, query_type ="forecasts",
                    models = NULL,
                    units = NULL,
                    targets = NULL,
                    timezeros = origin_date,
                    types = "quantile")
  
  task_id_cols <- c("forecast_date", "location", "horizon", "target_long")

  # Format forecasts
  forecast_data <- raw_forecasts |>
    tidyr::separate(.data$target, sep=" ", convert=TRUE, 
                    into=c("horizon", "target_long"), extra="merge") |>
    dplyr::rename(location = .data$unit, forecast_date = .data$timezero) |>
    as_model_out_tbl(model_id_col = "model",
                     output_type_col = "class",
                     output_type_id_col = "quantile",
                     value_col = "value",
                     sep = "-",
                     trim_to_task_ids = FALSE,
                     hub_con = NULL,
                     task_id_cols = task_id_cols,
                     remove_empty = TRUE) #|>
#    dplyr::select(-season)
      
  return (forecast_data)
}


#' Compute ensemble model outputs as a quantile average, quantile median,
#' linear pool with normal tails, or linear pool with log normal tails
#' for each combination model task, output type, and output type id.
#' Component model forecasts for incident flu hospitalizations are queried 
#' from zoltar with ensemble outputs formatted to be fed into 
#' `covidHubUtils::score_forecasts()` for scoring
#'
#' @param zoltar_connection a connection through which to access zoltar
#' @param project_url `character` string of the URL for these zoltar project 
#'   from which to query the component forecasts
#' @param origin_date `character` string that specifies the date on which
#'   the component forecasts were made that should be used for the ensemble
#' @param include_baseline `logical` that specifies whether to include the
#'   Flusight-baseline model in the ensemble.  Defaults to FALSE.
#' @param ensemble_type `character` string that specifies the ensembling method
#'   to be used on the flu forecasts. Can be "mean", "median", or "linear_pool".
#' @param tail_dist `character` string that specifies the type of distribution 
#'   to use when calculating the tails for a linear pool ensemble. 
#'   Defaults to NULL. This argument is ignored for Vincentization.  
#' @param ... parameters that are passed to `distfromq::make_q_fun`, specifying
#'   details of how to estimate a quantile function from provided quantile levels 
#'   and quantile values.
#'
#' @return a `model_out_tbl` object of ensemble predictions for flu hospitalizations.
#' @export
#'
#' @examples
#' @importFrom rlang .data
#' 
generate_flu_ensemble_single_date <- function(zoltar_connection, project_url,
                                              origin_date, include_baseline=FALSE,
                                              ensemble_type, tail_dist=NULL, ...) {

  model_outputs <- zoltar_connection |> 
    get_flu_forecasts_single_date(project_url, origin_date) |>
    dplyr::filter(.data$model_id != "Flusight-ensemble")
    
  if (!include_baseline) {
    model_outputs <- model_outputs |>
      dplyr::filter(.data$model_id != "Flusight-baseline")
  }

  task_id_cols <- c("forecast_date", "location", "horizon", "target_long")

  # Ensemble forecasts
  if (ensemble_type == "linear_pool") {
    if (is.null(tail_dist)) tail_dist = "norm"
    lp_type <- ifelse(tail_dist == "lnorm", "lognormal", "normal")
    ensemble_outputs <- model_outputs |>
      hubEnsembles::linear_pool(weights=NULL, weights_col_name=NULL,
                                model_id=paste("lp", lp_type, sep="-"),
                                task_id_cols = task_id_cols,
                                tail_dist = tail_dist,
                                n_samples = 1e5)

  } else {
    ensemble_outputs <- model_outputs |>
      hubEnsembles::simple_ensemble(weights=NULL, weights_col_name=NULL,
                                    agg_fun = ensemble_type,
                                    model_id=paste(ensemble_type, "ensemble", sep="-"),
                                    task_id_cols = task_id_cols)
  }
  
  ensemble_outputs <- ensemble_outputs |> 
    dplyr::rename(model = .data$model_id,
                  type = .data$output_type, 
                  quantile = .data$output_type_id) |>
    tidyr::separate(.data$target_long, sep=" ", 
                    convert=TRUE,
                    into=c("temporal_resolution", "ahead", "target_variable"), 
                    extra="merge") |>
    dplyr::mutate(value = ifelse(.data$value < 0, 0, .data$value)) |>
    dplyr::mutate(target_end_date=
                    lubridate::ceiling_date(.data$forecast_date, "weeks")-
                      lubridate::days(1)) |>
    dplyr::select(.data$model, .data$forecast_date, .data$location, .data$horizon, 
                  .data$temporal_resolution, .data$target_variable, 
                  .data$target_end_date, .data$type, .data$quantile, .data$value)
  
  return (ensemble_outputs)
}


#' Compute ensemble model outputs for incident for your hospitalizations 
#' as a quantile average, quantile median, linear pool with normal tails, 
#' or linear pool with log normal tails for each combination of model task, 
#' output type, and output type id. The outputs are formatted to be fed into
#' `covidHubUtils::score_forecasts()` for scoring
#'
#' @param model_outputs an object of class `model_output_df` with component
#'   model outputs (e.g., predictions).
#' @param include_baseline `logical` that specifies whether to include the
#'   Flusight-baseline model in the ensemble.  Defaults to FALSE.
#' @param ensemble_type `character` string that specifies the ensembling method
#'   to be used on the flu forecasts. Can be "mean", "median", or "linear_pool".
#' @param tail_dist `character` string that specifies the type of distribution 
#'   to use when calculating the tails for a linear pool ensemble. 
#'   Defaults to NULL. This argument is ignored for Vincentization.  
#' @param ... parameters that are passed to `distfromq::make_q_fun`, specifying
#'   details of how to estimate a quantile function from provided quantile levels 
#'   and quantile values.
#'
#' @return a `model_out_tbl` object of ensemble predictions for flu hospitalizations.
#' @export
#'
#' @examples
#' @importFrom rlang .data

generate_flu_ensemble <- function(model_outputs, include_baseline=FALSE, ensemble_type, tail_dist=NULL, ...) {

  model_outputs <- model_outputs |>
    dplyr::filter(.data$model_id != "Flusight-ensemble")
    
  if (!include_baseline) {
    model_outputs <- model_outputs |>
      dplyr::filter(.data$model_id != "Flusight-baseline")
  }

  task_id_cols <- c("forecast_date", "location", "horizon", "target_long")

  # Ensemble forecasts
  if (ensemble_type == "linear_pool") {
    if (is.null(tail_dist)) tail_dist = "norm"
    lp_type <- ifelse(tail_dist == "lnorm", "lognormal", "normal")
    ensemble_outputs <- model_outputs |>
      hubEnsembles::linear_pool(weights=NULL, weights_col_name=NULL,
                                model_id=paste("lp", lp_type, sep="-"),
                                task_id_cols = task_id_cols,
                                tail_dist = tail_dist,
                                n_samples = 1e5)

  } else {
    ensemble_outputs <- model_outputs |>
      hubEnsembles::simple_ensemble(weights=NULL, weights_col_name=NULL,
                                    agg_fun = ensemble_type,
                                    model_id=paste(ensemble_type, "ensemble", sep="-"),
                                    task_id_cols = task_id_cols)
  }
  
  ensemble_outputs <- ensemble_outputs |> 
    dplyr::rename(model = .data$model_id,
                  type = .data$output_type, 
                  quantile = .data$output_type_id) |>
    tidyr::separate(.data$target_long, sep=" ", 
                    convert=TRUE,
                    into=c("temporal_resolution", "ahead", "target_variable"), 
                    extra="merge") |>
#    dplyr::mutate(value = ifelse(.data$value < 0, 0, .data$value)) |>
    dplyr::mutate(target_end_date=
                    lubridate::ceiling_date(.data$forecast_date, "weeks")-
                      lubridate::days(1), 
                  .before = .data$value) |>
    dplyr::select(.data$model, .data$forecast_date, .data$location, .data$horizon, 
                  .data$temporal_resolution, .data$target_variable, 
                  .data$target_end_date, .data$type, .data$quantile, .data$value)
  
  return (ensemble_outputs)
}

