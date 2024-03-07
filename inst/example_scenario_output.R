## code to prepare `example_scenario_output` dataset goes here
## note: requires example-complex-scenario-hub and hubEnsemblesManuscript are
##       cloned into the same folder

library(hubData)
hub_path <- "../example-complex-scenario-hub"
here::here("analysis/data/raw_data/flu_truth_all.rds")
example_scenario_output <- hubData::connect_hub(hub_path) |>
  dplyr::collect() |>
  dplyr::select(model_id, origin_date, location, horizon, target, scenario_id, output_type, output_type_id, value) |>
  dplyr::filter(location == "US",
                origin_date == "2021-03-07",
                horizon == 26,
                target == "cum death",
                output_type_id %in% c(0.25, 0.5, 0.75),
                model_id == "HUBuni-simexamp")

# saveRDS(example_scenario_output, file = "analysis/data/example_data/example_scenario_output.rds")

