# =============================
# General Data Preprocessing Toolkit (Supports Cross-Set Lag Integration)
# =============================

library(dplyr)
library(lubridate)
library(zoo)

# Standardize datetime column: convert character to datetime, complete time, set timezone
preprocess_datetime <- function(df, datetime_col = "DateTime", timezone = "Europe/Berlin") {
  df[[datetime_col]] <- as.character(df[[datetime_col]])
  # Append "00:00:00" to datetime strings that don't have time information
  df[[datetime_col]][!grepl("\\d{2}:\\d{2}:\\d{2}", df[[datetime_col]])] <-
    paste0(df[[datetime_col]][!grepl("\\d{2}:\\d{2}:\\d{2}", df[[datetime_col]])], " 00:00:00")
  df[[datetime_col]] <- ymd_hms(df[[datetime_col]], tz = "UTC")
  df[[datetime_col]] <- with_tz(df[[datetime_col]], tzone = timezone)
  return(df)
}

# Add time feature columns
add_time_features <- function(df, datetime_col = "DateTime") {
  df <- df %>%
    mutate(
      hour_of_day = hour(.data[[datetime_col]]),
      day_of_week = wday(.data[[datetime_col]], week_start = 1),
      month_of_year = month(.data[[datetime_col]]),
      year = year(.data[[datetime_col]])
    )
  return(df)
}

# Check for hourly continuity
check_hourly_continuity <- function(df, datetime_col = "DateTime") {
  if (!inherits(df[[datetime_col]], "POSIXct")) {
    stop("🛑 The datetime column must be of POSIXct type. Please convert it first!")
  }
  df <- df[order(df[[datetime_col]]), ]
  diffs <- as.numeric(difftime(df[[datetime_col]][-1], df[[datetime_col]][-nrow(df)], units = "hours"))
  if (all(diffs == 1)) {
    message("✅ Time is continuous: One record per hour, no missing data ✔️")
  } else {
    message("⚠️ Time is not continuous! Missing records detected, missing intervals are as follows:")
    broken_idx <- which(diffs != 1)
    missing_ranges <- data.frame(
      Expected = df[[datetime_col]][broken_idx] + hours(1),
      Actual = df[[datetime_col]][broken_idx + 1],
      Gap_Hours = diffs[broken_idx]
    )
    print(missing_ranges)
  }
}

# Interpolate to fill missing values
fill_and_interpolate <- function(df, datetime_col = "DateTime", value_col = "Price") {
  full_seq <- data.frame(DateTime = seq(min(df[[datetime_col]]), max(df[[datetime_col]]), by = "hour"))
  df_filled <- full_seq %>% left_join(df, by = datetime_col)
  df_filled[[datetime_col]] <- as.POSIXct(df_filled[[datetime_col]])
  df_filled <- df_filled %>% arrange(.data[[datetime_col]]) %>% distinct(.data[[datetime_col]], .keep_all = TRUE)
  df_filled[[value_col]] <- na.approx(df_filled[[value_col]], x = as.numeric(df_filled[[datetime_col]]), rule = 2)
  return(df_filled)
}

# One-click processing + cross-set processing (core function)
full_preprocess <- function(current_df, before_df = NULL, datetime_col = "DateTime", value_col = "Price") {
  current_df <- preprocess_datetime(current_df, datetime_col)
  if (!is.null(before_df)) {
    before_df <- preprocess_datetime(before_df, datetime_col)
  }
  if (!is.null(before_df)) {
    combined_df <- bind_rows(before_df, current_df)
  } else {
    combined_df <- current_df
  }
  combined_df <- fill_and_interpolate(combined_df, datetime_col, value_col)
  combined_df <- add_time_features(combined_df, datetime_col)
  current_only <- combined_df %>% filter(.data[[datetime_col]] %in% current_df[[datetime_col]])
  return(current_only)
}

# Construct lagged variables and extended features
construct_lags <- function(current_df, before_df = NULL) {
  full_df <- if (!is.null(before_df)) bind_rows(before_df, current_df) else current_df
  full_df <- full_df %>% arrange(DateTime)
  # Standardize
  full_df <- full_df %>%
    group_by(hour_of_day) %>%
    mutate(
      Price_winsorized_z = scale(Price_winsorized)[,1],
      Load_DA_z = scale(Load_DA)[,1],
      Solar_DA_z = scale(Solar_DA)[,1],
      WindOn_DA_z = scale(WindOn_DA)[,1],
      WindOff_DA_z = scale(WindOff_DA)[,1]
    ) %>% ungroup()
  # Lagged variables (by day)
  full_df <- full_df %>% mutate(
    Lag_1d_Pw = lag(Price_winsorized, n = 24),
    Lag_2d_Pw = lag(Price_winsorized, n = 48),
    Lag_24d_Pw = lag(Price_winsorized, n = 24 * 24),
    Lag_48d_Pw = lag(Price_winsorized, n = 48 * 24),
    Lag_72d_Pw = lag(Price_winsorized, n = 72 * 24),
    Lag_168d_Pw = lag(Price_winsorized, n = 168 * 24),
    Lag_1d_Load = lag(Load_DA_z, n = 24),
    Lag_2d_Load = lag(Load_DA_z, n = 48),
    Lag_24d_Load = lag(Load_DA_z, n = 24 * 24),
    Lag_48d_Load = lag(Load_DA_z, n = 48 * 24),
    Lag_72d_Load = lag(Load_DA_z, n = 72 * 24),
    Lag_168d_Load = lag(Load_DA_z, n = 168 * 24),
    Lag_1d_Solar = lag(Solar_DA_z, n = 24),
    Lag_2d_Solar = lag(Solar_DA_z, n = 48),
    Lag_24d_Solar = lag(Solar_DA_z, n = 24 * 24),
    Lag_48d_Solar = lag(Solar_DA_z, n = 48 * 24),
    Lag_72d_Solar = lag(Solar_DA_z, n = 72 * 24),
    Lag_168d_Solar = lag(Solar_DA_z, n = 168 * 24),
    Lag_1d_WindOff = lag(WindOff_DA_z, n = 24),
    Lag_2d_WindOff = lag(WindOff_DA_z, n = 48),
    Lag_24d_WindOff = lag(WindOff_DA_z, n = 24 * 24),
    Lag_48d_WindOff = lag(WindOff_DA_z, n = 48 * 24),
    Lag_72d_WindOff = lag(WindOff_DA_z, n = 72 * 24),
    Lag_168d_WindOff = lag(WindOff_DA_z, n = 168 * 24)
  )
  # Extended time features
  full_df <- full_df %>%
    mutate(
      hour_sin = sin(2 * pi * hour_of_day / 24),
      hour_cos = cos(2 * pi * hour_of_day / 24),
      daylight_hours = ifelse(hour_of_day %in% 6:20, hour_of_day, 0),
      hour_sin2 = sin(4 * pi * hour_of_day / 24),
      hour_cos2 = cos(4 * pi * hour_of_day / 24),
      daylight_sin = sin(2 * pi * daylight_hours / 24),
      daylight_cos = cos(2 * pi * daylight_hours / 24),
      month_sin = sin(2 * pi * month_of_year / 12),
      month_cos = cos(2 * pi * month_of_year / 12),
      hour_solar = ifelse(hour_of_day %in% 6:20, (hour_of_day - 13)^2, 0),
      solar_period = case_when(
        hour_of_day %in% 6:11 ~ "morning",
        hour_of_day %in% 12:17 ~ "afternoon",
        TRUE ~ "no_solar"
      ),
      load_hour_type = case_when(
        hour_of_day %in% c(7:9, 18:20) ~ "peak",
        hour_of_day %in% c(10:17) ~ "day_offpeak",
        hour_of_day %in% c(1:6, 21:23) ~ "night"
      )
    )
  # Extract current portion (retain training set window)
  result_df <- full_df %>% filter(DateTime %in% current_df$DateTime)
  return(result_df)
}
