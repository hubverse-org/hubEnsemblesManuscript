### CODE TO CREATE SUBFIGURES FOR OVERVIEW FIGURE IN HUBENSEMBELS MS -----------

# use same target data and predictions from hubExamples that is included in ms
target_data_plot <- hubExamples::forecast_target_ts |>
  dplyr::filter(
    location == "25",
    date < "2022-12-17",
    date >= "2022-10-15"
  )

# anonymize models to match table 1
plot_df = hubExamples::forecast_outputs |>
  dplyr::filter(
    location == "25",
    reference_date == "2022-12-17",
    output_type == "quantile",
    output_type_id %in% c(0.05, 0.25, 0.5, 0.75, 0.95)
  ) |>
  dplyr::mutate(model_id = paste0("model-", ifelse(substr(model_id,1,3) == "MOB",
                                                   "X", ifelse(substr(model_id,1,3) == "Flu", "Y", "Z")))) |>
  hubUtils::as_model_out_tbl()


hubVis::plot_step_ahead_model_output(
  model_out_tbl = plot_df,
  target_data = target_data_plot,
  use_median_as_point = TRUE,
  interactive = FALSE,
  intervals = c(0.9),
  # facet = model_id,
  show_legend = TRUE,
  pal_color = "Set1",
  fill_transparency = 0.1,
  x_col_name = "target_end_date",
  x_target_col_name = "date"
) +
  ggplot2::facet_wrap(ggplot2::vars(model_id), ncol = 1) +
  ggplot2::theme_bw() +
  ggplot2::labs(y = "incident hospitalizations") +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
        legend.position = "none",
        legend.title = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank())+
  ggplot2::scale_y_continuous(limits = c(-1,1200))
ggplot2::ggsave("inst/overview_fig_panelA.pdf", width = 3, height = 6)


ens_df = hubExamples::forecast_outputs |>
  dplyr::filter(output_type != "median") |>
  hubEnsembles::linear_pool(model_id = "linear-pool") |>
  dplyr::filter(
    location == "25",
    reference_date == "2022-12-17",
    output_type == "quantile",
    output_type_id %in% c(0.05, 0.25, 0.5, 0.75, 0.95)
  ) |>
  hubUtils::as_model_out_tbl()

hubVis::plot_step_ahead_model_output(
  model_out_tbl = ens_df,
  target_data = target_data_plot,
  use_median_as_point = TRUE,
  interactive = FALSE,
  intervals = c(0.9),
  show_legend = TRUE,
  pal_color = "Set1",
  fill_transparency = 0.1,
  x_col_name = "target_end_date",
  x_target_col_name = "date",
  ens_color = "black",
  ens_name = "linear-pool"
) +
  ggplot2::theme_bw() +
  ggplot2::labs(y = "incident hospitalizations") +
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
        legend.position = "none",
        legend.title = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank()) +
  ggplot2::scale_y_continuous(limits = c(0,1200))
ggplot2::ggsave("inst/overview_fig_panelc.pdf", width = 3, height = 2)

