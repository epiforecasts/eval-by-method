library(here)
library(purrr)
library(lubridate)
source(here("R", "metadata-utils.R"))

# set repo paths
repos <- c("euro" = "covid19-forecast-hub-europe/covid19-forecast-hub-europe",
           "us" = "reichlab/covid19-forecast-hub")

# date range for submissions
min_date <- "2021-03-07"
max_date <- as.Date("2023-03-10") - weeks(4)

# get euro metadata
eu_submissions <- get_model_submissions(repo = repos[["euro"]],
                                        min_date = min_date,
                                        max_date = max_date)
eu_metadata_raw <- get_metadata_raw(repo = repos[["euro"]],
                                    model_abbr = unique(eu_submissions$model))
write_metadata(metadata_raw = eu_metadata_raw,
               sheet_name = "euro-metadata-raw", write_local = FALSE)

# get US metadata
us_submissions <- get_model_submissions(repo = repos[["us"]],
                                        min_date = min_date,
                                        max_date = max_date)
us_metadata_raw <- get_metadata_raw(repo = repos[["us"]],
                                    model_abbr = unique(us_submissions$model))

us_metadata_raw <- us_metadata_raw |>
  mutate(euro_overlap = model_abbr %in% unique(eu_submissions$model))

write_metadata(metadata_raw = us_metadata_raw,
               sheet_name = "us-metadata-raw", write_local = FALSE)
