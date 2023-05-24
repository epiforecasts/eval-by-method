library(dplyr)
library(googlesheets4)

gs4_auth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1XgXLYBCpdtjztJFhWDJz6G7A_Uw92dnnr-WFqGAFGn4/edit#gid=0"

hub_repo_path = "C:/Users/kaths/Documents/GitHub/covid19-forecast-hub-europe"

metadata_files <- list.files(
  file.path(paste0(hub_repo_path, "/model-metadata")),
  full.names = TRUE)
metadata <- purrr::map_dfr(metadata_files, yaml::read_yaml)

metadata <- metadata |>
  filter(!team_model_designation %in% c("secondary", "other")) |>
  select(model_abbr,
         website_url, repo_url, citation,
         methods, methods_long, data_inputs) |>
  distinct()

write_sheet(metadata, ss = sheet_url, sheet = "metadata-raw")
