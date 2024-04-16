##' Get estimates shown at \url{https://epiforecasts.io/covid} for all times
##'
##' Nowcasts at \url{https://epiforecasts.covid} and the related github repository at
##' \url{https://github.com/epiforecasts/covid-rt-estimates} only cover the last 3
##' months. This function downloads all available estimates and applies a median
##' average to the provided quantiles to provide an estimate of a time series
##' covering all times available
##'
##' @param dataset character; data set corresponding to directories at
##' \url{https://github.com/epiforecasts/covid-rt-estimates}. Default is
##' \code{"national/cases"}, other examples of possible values are:
##' \code{"subnational/brazil/cases"} or
##' \code{"subnational/united-kingdom/admission"}
##' @param variable character; quantity to consider; by default: "rt", but
##' could also be, e.g.. "growth_rate", or "cases_by_infection"; corresponds to
##' the name of \code{.csv} files in the summary directories at
##' \url{https://github.com/epiforecasts/covid-rt-estimates}
##' @param earliest_date character; the earliest date to consider; by default:
##' "30 August 2020" (the day after estimates started being produced using
##' EpiNow2). It is not recommended to set this to an earlier date as it would
##' lead to a mixture of methods. If set to NULL will only download the
##' latest estimates
##' @return a list of data frames containing estimates for all times
##' @importFrom gh gh
##' @importFrom readr read_csv
##' @importFrom dplyr bind_rows group_by summarise
##' @importFrom tidyr pivot_longer pivot_wider
##' @author Sebastian Funk
library("gh")
library("readr")
library("dplyr")
library("tidyr")
get_covid19_nowcasts <-
  function(dataset = "national/cases", variable = "rt",
           earliest_date = "2020-08-30") {
    owner <- "epiforecasts"
    repo <- "covid-rt-estimates"
    path <- paste(dataset, "summary", paste0(variable, ".csv"), sep = "/")

    query <- "/repos/{owner}/{repo}/commits?path={path}"

    if (!is.null(earliest_date)) {
      query <- paste0(query, "&since={date}")
      limit <- Inf
    } else {
      limit <- 1
    }

    commits <-
      gh::gh(query,
             owner = owner,
             repo = repo,
             path = path,
             date = earliest_date,
             .limit = limit)

    shas <- vapply(commits, "[[", "", "sha")

    estimates <-
      lapply(
        shas,
        function(sha)
          readr::read_csv(
            paste("https://raw.githubusercontent.com", owner, repo,
                  sha, path, sep = "/"))
      )

    median_estimates <- estimates %>%
      bind_rows()

    if ("bottom" %in% colnames(median_estimates)) {
      median_estimates <- median_estimates %>%
        mutate(lower_90 = if_else(is.na(lower_90), bottom, lower_90)) %>%
        select(-bottom)
    }
    if ("lower" %in% colnames(median_estimates)) {
      median_estimates <- median_estimates %>%
        mutate(lower_50 = if_else(is.na(lower_50), lower, lower_50)) %>%
        select(-lower)
    }
    if ("central_lower" %in% colnames(median_estimates)) {
      median_estimates <- median_estimates %>%
        mutate(lower_20 = if_else(is.na(lower_20), central_lower, lower_20)) %>%
        select(-central_lower)
    }
    if ("central_upper" %in% colnames(median_estimates)) {
      median_estimates <- median_estimates %>%
        mutate(upper_20 = if_else(is.na(upper_20), central_upper, upper_20)) %>%
        select(-central_upper)
    }
    if ("upper" %in% colnames(median_estimates)) {
      median_estimates <- median_estimates %>%
        mutate(upper_50 = if_else(is.na(upper_50), upper, upper_50)) %>%
        select(-upper)
    }
    if ("top" %in% colnames(median_estimates)) {
      median_estimates <- median_estimates %>%
        mutate(upper_90 = if_else(is.na(upper_90), top, upper_90)) %>%
        select(-top)
    }
    median_estimates <- median_estimates %>%
      mutate(mean = if_else(is.na(mean), median, mean), ## assume normal if mean not given
             sd = if_else(is.na(sd), (upper_50 - lower_50) / 1.34896, sd)) %>% ## assume normal if SD not given
      select(-strat, -type) %>%
      pivot_longer(c(median, mean, sd, starts_with("lower_"), starts_with("upper_"))) %>%
      group_by_at(vars(1, "date", "name")) %>%
      summarise(value = median(value), .groups = "drop") %>%
      pivot_wider()

    return(median_estimates)
  }


