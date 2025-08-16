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
  # Standardize datetime for current and historical data
  current_df <- preprocess_datetime(current_df, datetime_col)
  if (!is.null(before_df)) {
    before_df <- preprocess_datetime(before_df, datetime_col)
  }
  # Merge historical data and fill in missing values
  if (!is.null(before_df)) {
    combined_df <- bind_rows(before_df, current_df)
  } else {
    combined_df <- current_df
  }
  combined_df <- fill_and_interpolate(combined_df, datetime_col, value_col)
  combined_df <- add_time_features(combined_df, datetime_col)
  # Restore current data portion (maintain consistent features)
  current_only <- combined_df %>% filter(.data[[datetime_col]] %in% current_df[[datetime_col]])
  return(current_only)
}
