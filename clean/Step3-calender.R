library(bizdays)
library(lubridate)
library(dplyr)
library(fasttime)

df <- read.csv("entsoe_de_cleanstep2.csv",
               colClasses = c(DateTime = "character")) |>
  mutate(
    DateTime_utc = fastPOSIXct(DateTime, tz = "UTC"), # String to UTC
    DateTime = with_tz(DateTime_utc, "Europe/Berlin"), # Convert to CET/CEST
    date = as.Date(DateTime) # Correct calendar date
  ) |>
  select(-DateTime_utc)

df <- df |>
  mutate(
    hour_of_day = hour(DateTime), # 0-23
    day_of_week = wday(date, week_start = 1), # 1=Mon ... 7=Sun
    month_of_year = month(DateTime),
    year = year(DateTime)
  )

# List of national holidays in Germany (2014-2025)
german_holidays_named <- c(
  # 2014
  '2014-01-01' = 'New Year', '2014-04-18' = 'Good Friday',
  '2014-04-21' = 'Easter Monday', '2014-05-01' = 'Labor Day',
  '2014-05-29' = 'Ascension Day', '2014-06-09' = 'Whit Monday',
  '2014-10-03' = 'German Unity Day',
  '2014-12-25' = 'Christmas Day', '2014-12-26' = 'Boxing Day',
  # 2015
  '2015-01-01' = 'New Year', '2015-04-03' = 'Good Friday',
  '2015-04-06' = 'Easter Monday', '2015-05-01' = 'Labor Day',
  '2015-05-14' = 'Ascension Day', '2015-05-25' = 'Whit Monday',
  '2015-10-03' = 'German Unity Day',
  '2015-12-25' = 'Christmas Day', '2015-12-26' = 'Boxing Day',
  # 2016
  '2016-01-01' = 'New Year', '2016-03-25' = 'Good Friday',
  '2016-03-28' = 'Easter Monday', '2016-05-01' = 'Labor Day',
  '2016-05-05' = 'Ascension Day', '2016-05-16' = 'Whit Monday',
  '2016-10-03' = 'German Unity Day',
  '2016-12-25' = 'Christmas Day', '2016-12-26' = 'Boxing Day',
  # 2017
  '2017-01-01' = 'New Year', '2017-04-14' = 'Good Friday',
  '2017-04-17' = 'Easter Monday', '2017-05-01' = 'Labor Day',
  '2017-05-25' = 'Ascension Day', '2017-06-05' = 'Whit Monday',
  '2017-10-03' = 'German Unity Day',
  '2017-12-25' = 'Christmas Day', '2017-12-26' = 'Boxing Day',
  # 2018
  '2018-01-01' = 'New Year', '2018-03-30' = 'Good Friday',
  '2018-04-02' = 'Easter Monday', '2018-05-01' = 'Labor Day',
  '2018-05-10' = 'Ascension Day', '2018-05-21' = 'Whit Monday',
  '2018-10-03' = 'German Unity Day',
  '2018-12-25' = 'Christmas Day', '2018-12-26' = 'Boxing Day',
  # 2019
  '2019-01-01' = 'New Year', '2019-04-19' = 'Good Friday',
  '2019-04-22' = 'Easter Monday', '2019-05-01' = 'Labor Day',
  '2019-05-30' = 'Ascension Day', '2019-06-10' = 'Whit Monday',
  '2019-10-03' = 'German Unity Day',
  '2019-12-25' = 'Christmas Day', '2019-12-26' = 'Boxing Day',
  # 2020
  '2020-01-01' = 'New Year', '2020-04-10' = 'Good Friday',
  '2020-04-13' = 'Easter Monday', '2020-05-01' = 'Labor Day',
  '2020-05-21' = 'Ascension Day', '2020-06-01' = 'Whit Monday',
  '2020-10-03' = 'German Unity Day',
  '2020-12-25' = 'Christmas Day', '2020-12-26' = 'Boxing Day',
  # 2021
  '2021-01-01' = 'New Year', '2021-04-02' = 'Good Friday',
  '2021-04-05' = 'Easter Monday', '2021-05-01' = 'Labor Day',
  '2021-05-13' = 'Ascension Day', '2021-05-24' = 'Whit Monday',
  '2021-10-03' = 'German Unity Day',
  '2021-12-25' = 'Christmas Day', '2021-12-26' = 'Boxing Day',
  # 2022
  '2022-01-01' = 'New Year', '2022-04-15' = 'Good Friday',
  '2022-04-18' = 'Easter Monday', '2022-05-01' = 'Labor Day',
  '2022-05-26' = 'Ascension Day', '2022-06-06' = 'Whit Monday',
  '2022-10-03' = 'German Unity Day',
  '2022-12-25' = 'Christmas Day', '2022-12-26' = 'Boxing Day',
  # 2023
  '2023-01-01' = 'New Year', '2023-04-07' = 'Good Friday',
  '2023-04-10' = 'Easter Monday', '2023-05-01' = 'Labor Day',
  '2023-05-18' = 'Ascension Day', '2023-05-29' = 'Whit Monday',
  '2023-10-03' = 'German Unity Day',
  '2023-12-25' = 'Christmas Day', '2023-12-26' = 'Boxing Day',
  # 2024
  '2024-01-01' = 'New Year', '2024-03-29' = 'Good Friday',
  '2024-04-01' = 'Easter Monday', '2024-05-01' = 'Labor Day',
  '2024-05-09' = 'Ascension Day', '2024-05-20' = 'Whit Monday',
  '2024-10-03' = 'German Unity Day',
  '2024-12-25' = 'Christmas Day', '2024-12-26' = 'Boxing Day',
  # 2025
  '2025-01-01' = 'New Year', '2025-04-18' = 'Good Friday',
  '2025-04-21' = 'Easter Monday', '2025-05-01' = 'Labor Day',
  '2025-05-29' = 'Ascension Day', '2025-06-09' = 'Whit Monday',
  '2025-10-03' = 'German Unity Day',
  '2025-12-25' = 'Christmas Day', '2025-12-26' = 'Boxing Day'
)

german_holidays <- as.Date(names(german_holidays_named)) # Pure date vector

# Register bizdays calendar
create.calendar(
  name = "Germany_manual",
  holidays = german_holidays,
  weekdays = c("saturday", "sunday"),
  adjust.from = "next",
  adjust.to = "previous"
)

# Read main data and create calendar variables
df <- df %>%
  mutate(
    DateTime = as.POSIXct(DateTime, tz = "Europe/Berlin"),
    date = as.Date(DateTime),
    hour_of_day = hour(DateTime),
    day_of_week = wday(date, week_start = 1), # 1 = Monday, 7 = Sunday
    month_of_year = month(DateTime),
    year = year(DateTime),
    is_weekend = as.integer(day_of_week %in% c(6, 7)),
    is_holiday = as.integer(date %in% german_holidays),
    is_bizday = as.integer(is.bizday(date, "Germany_manual")),
    is_pre_holiday = as.integer((date + 1) %in% german_holidays),
    is_post_holiday = as.integer((date - 1) %in% german_holidays)
  )

# Export results
print(head(df[, c("DateTime", "is_holiday", "is_pre_holiday", "is_post_holiday")]))
write.csv(df, "entsoe_de_cleanstep3.csv", row.names = FALSE)
