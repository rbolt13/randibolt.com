# Run from the project root:
# source("blog/data/R/nyc-311-data.R")
library(tidyverse)
library(lubridate)
library(httr2)

response <- request("https://data.cityofnewyork.us/resource/wewp-mm3p.csv") |>
  req_url_query(
    `$select` = "date_trunc_ymd(date_time) AS date, date_extract_hh(date_time) AS hour, count(*) AS inquiry_count",
    `$where` = "date_time >= '2015-01-01T00:00:00' AND date_time < '2019-01-01T00:00:00'",
    `$group` = "date,hour",
    `$order` = "date,hour",
    `$limit` = 100000
  ) |>
  req_timeout(180) |>
  req_perform()

data <- read_csv(I(resp_body_string(response)), show_col_types = FALSE) |>
  mutate(date = as_date(date))

dir.create("blog/data", recursive = TRUE, showWarnings = FALSE)
write_csv(data, "blog/data/nyc-311-data.csv")

preview <- head(data) |>
  mutate(
    row = row_number(),
    date = format(date, "%Y-%m-%d"),
    hour = as.character(hour),
    inquiry_count = scales::comma(inquiry_count)
  ) |>
  pivot_longer(-row, names_to = "field", values_to = "value") |>
  mutate(field = factor(field, levels = c("date", "hour", "inquiry_count")))

headers <- tibble(
  field = factor(
    c("date", "hour", "inquiry_count"),
    levels = c("date", "hour", "inquiry_count")
  ),
  value = c("date", "hour", "inquiry_count")
)
