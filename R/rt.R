# rt <- read_csv("https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/deaths/summary/rt.csv")

# Get Rt estimates
# from: https://github.com/lin-lab/COVID19-Viz/tree/master/clean_data_pois
# download.file("https://hsph-covid-study.s3.us-east-2.amazonaws.com/website_files_pois/rt_table_export.csv.zip",
#                        destfile = here("data", "rt_lin.csv.zip"))

rt_lin <- read_csv(here("data", "rt_lin.csv.zip")) |>
  filter(resolution == "country") |>
  select(target_end_date = date_lag, location_name = dispID,
         rt_lower, rt, rt_upper) |>
  left_join(read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/main/data-locations/locations_eu.csv") |>
              select(location_name, location))

# categorise Rt
rt_lin <- rt_lin |>
  mutate(rt_cat = case_when(rt_lower < 1 & rt_upper > 1 ~ "Cross",
                            rt_lower >= 1 & rt_upper > 1 ~ "Over",
                            rt_lower <= 1 & rt_upper <= 1 ~ "Under"))

# Get scores ----------------------------------------
scores_raw <- read_csv(here("data", "scores-raw.csv"))
scores_raw_rt <- scores_raw |>
  left_join(rt_lin, by = c("target_end_date", "location"))

# Score all forecasts relative to each other (pairwise)
score_by <- c("rt_cat", "model", "target_variable", "scale")

scores_pairwise_rt <- scores_raw_rt |>
  select(model, horizon, location, forecast_date, target_variable,
         interval_score, scale,
         rt_cat) |>
  pairwise_comparison(
    metric = "interval_score",
    baseline = "EuroCOVIDhub-ensemble",
    by = score_by)

scores_pairwise_rt <- scores_pairwise_rt |>
  filter(compare_against == "EuroCOVIDhub-ensemble") |>
  select(model, all_of(score_by),
         rel_wis = scaled_rel_skill) |>
  mutate(rt_cat = factor(rt_cat, levels = c("Under", "Cross", "Over")))

# visualise all scores
scores_pairwise_rt |>
  ggplot(aes(y = rel_wis, x = rt_cat)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap("scale")

# visualise by method
metadata <- read_csv(here("data", "model-classification.csv")) |>
  select(model, method_type = classification)

by_method_data <- scores_pairwise_rt |>
  left_join(metadata) |>
  # exclude ensembles
  filter(!grepl("Other", method_type)) |>
  # regroup to any mechanistic
  mutate(any_mechanistic = ifelse(grepl("(m|M)echanistic", method_type),
                                  "Semi/fully mechanistic", method_type))

methods_counts <- c("n" = nrow(by_method_data),
                    "n_5p" = nrow(filter(by_method_data, rel_wis > 5)))
# Boxplot
by_method_data |>
  mutate(method_type = as.factor(method_type)) |>
  ggplot(aes(y = rel_wis, x = rt_cat,
             col = method_type)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 1, lty = 2) +
  labs(x = "Rt v 1", y = "Relative WIS",
       col = "Model method type",
       title = "Performance by horizon and model method type",
       caption = paste0("N=", methods_counts[["n"]],
                        ". Outliers with rWIS >5 not shown, n=",
                        methods_counts[["n_5p"]])) +
  scale_y_continuous(limits = c(NA,5)) +
  scale_color_brewer(type = "qual", palette = 2) +
  facet_wrap("scale") +
  theme_bw() +
  theme(legend.position = "bottom")
