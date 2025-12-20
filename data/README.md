README
================

### Data processing

The following R scripts were used to process the data stored here:

- [`R/utils-data.R`](R/utils-data.R) : Code to access forecasts,
  observed data, and population size for each location
  - `populations.csv`
  - `observed-case.csv`, `observed-death.csv`
  - `covid19-forecast-hub-europe.parquet`
- [`R/utils-metadata.R`](R/utils-metadata.R) : Functions to access model
  names, model submissions, and raw metadata from Hub GitHub repos
  - `model-classification.csv`
- [`R/process-score.R`](R/process-score.R) : Code to score forecasts
  using scoringutils
  - `scores-raw-case.csv`, `scores-raw-death.csv`

#### Metadata

Code used to access and store metadata:

``` r
library(here)
library(purrr)
library(lubridate)
source(here("R", "utils-metadata.R"))

# set repo paths
repos <- c(
  "euro" = "covid19-forecast-hub-europe/covid19-forecast-hub-europe",
  "us" = "reichlab/covid19-forecast-hub"
)

# date range for submissions
min_date <- "2021-03-07"
max_date <- as.Date("2023-03-10") - weeks(4)

# get euro metadata
eu_submissions <- get_model_submissions(
  repo = repos[["euro"]],
  min_date = min_date,
  max_date = max_date
)
eu_metadata_raw <- get_metadata_raw(
  repo = repos[["euro"]],
  model_abbr = unique(eu_submissions$model)
)
write_metadata(
  metadata_raw = eu_metadata_raw,
  sheet_name = "euro-metadata-raw", write_local = FALSE
)
```

Code can also be used to access US Hub metadata:

``` r
# get US metadata
us_submissions <- get_model_submissions(
  repo = repos[["us"]],
  min_date = min_date,
  max_date = max_date
)
us_metadata_raw <- get_metadata_raw(
  repo = repos[["us"]],
  model_abbr = unique(us_submissions$model)
)

us_metadata_raw <- us_metadata_raw |>
  mutate(euro_overlap = model_abbr %in% unique(eu_submissions$model))

write_metadata(
  metadata_raw = us_metadata_raw,
  sheet_name = "us-metadata-raw", write_local = FALSE
)
```
