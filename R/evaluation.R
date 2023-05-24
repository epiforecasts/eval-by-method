# scoring from scratch -----------------------

# Get forecasts

# Get observed data

# Exclude anomalies

# Transform to log scale

# Score wis & mae by week, target, model

# scoring from the hub eval -------------------

library(readr)
library(lubridate)

# scores from before data change march 10
scores <- read_csv("https://raw.githubusercontent.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/3ad05ecb51ba27a2493c93ab1b12489158500654/evaluation/scores.csv")

