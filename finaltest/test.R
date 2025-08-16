library(dplyr)
library(lubridate)
library(RcppRoll)
library(moments)

# Load datasets
test_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/3test/test_df_winsorized.csv")
valid_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/2valid/valid_processed.csv")

# Set paths
arx_model_path <- "D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre/rolling_window_10_ARXmodels.rds"
marx_model_path <- "D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre2/rolling_window_10_MARXmodels.rds"
arx_data_path <- "D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre/rolling_window_10_ARXdata.rds"

# Read models and data
arx_model <- readRDS(arx_model_path)
marx_model <- readRDS(marx_model_path)
arx_data <- readRDS(arx_data_path)
hourly_stats <- readRDS("D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre/rolling_window_10_hourly_stats.rds")

# Convert DateTime
test_df$DateTime <- as.character(test_df$DateTime)
test_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", test_df$DateTime)] <-
  paste0(test_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", test_df$DateTime)], " 00:00:00")
test_df$DateTime <- ymd_hms(test_df$DateTime, tz = "UTC")
test_df$DateTime <- with_tz(test_df$DateTime, tzone = "Europe/Berlin")

# Function to fill and interpolate missing data
fill_and_interpolate <- function(df, datetime_col = "DateTime", value_col = "Price") {
  library(dplyr)
  library(lubridate)
  library(zoo)
  
  # Generate a complete time series (hourly)
  full_seq <- data.frame(
    DateTime = seq(min(df[[datetime_col]]), max(df[[datetime_col]]), by = "hour")
  )
  
  # Fill in the time
  df_filled <- full_seq %>%
    left_join(df, by = datetime_col)
  
  # Ensure the time column is POSIXct, sorted, and unique
  df_filled[[datetime_col]] <- as.POSIXct(df_filled[[datetime_col]])
  df_filled <- df_filled %>%
    arrange(.data[[datetime_col]]) %>%
    distinct(.data[[datetime_col]], .keep_all = TRUE)
  
  # Interpolate: Pass in numeric time, rule=2 ensures boundary interpolation
  df_filled[[value_col]] <- na.approx(
    df_filled[[value_col]],
    x = as.numeric(df_filled[[datetime_col]]),
    rule = 2
  )
  
  return(df_filled)
}

test_df <- fill_and_interpolate(test_df, datetime_col = "DateTime", value_col = "Price")

# Function to check hourly continuity
check_hourly_continuity <- function(df, datetime_col = "DateTime") {
  if (!inherits(df[[datetime_col]], "POSIXct")) {
    stop("🛑 DateTime column must be POSIXct type, please convert first!")
  }
  df <- df[order(df[[datetime_col]]), ]
  
  diffs <- as.numeric(difftime(df[[datetime_col]][-1], df[[datetime_col]][-nrow(df)], units = "hours"))
  
  if (all(diffs == 1)) {
    message("✅ Time series is continuous: hourly records with no gaps ✔️")
  } else {
    message("⚠️ Time series is not continuous! Missing records found:")
    broken_idx <- which(diffs != 1)
    missing_ranges <- data.frame(
      Expected = df[[datetime_col]][broken_idx] + hours(1),
      Actual = df[[datetime_col]][broken_idx + 1],
      Gap_Hours = diffs[broken_idx]
    )
    print(missing_ranges)
  }
}

check_hourly_continuity(test_df)

# Feature engineering
test_df <- test_df %>%
  mutate(
    hour_of_day = hour(DateTime),
    day_of_week = wday(DateTime, week_start = 1),
    month_of_year = month(DateTime),
    year = year(DateTime)
  )

names(hourly_stats)
names(test_df)

# Join with hourly statistics
test_df <- test_df %>%
  left_join(hourly_stats, by = "hour_of_day")

# Normalize variables
vars_to_norm <- c("Price_winsorized", "Load_DA", "Solar_DA", "WindOn_DA", "WindOff_DA")
for (var in vars_to_norm) {
  mean_col <- paste0(var, "_mean")
  sd_col <- paste0(var, "_sd")
  z_col <- paste0(var, "_z")
  
  # Handle missing and abnormal values
  test_df[[z_col]] <- (test_df[[var]] - test_df[[mean_col]]) /
    ifelse(test_df[[sd_col]] == 0 | is.na(test_df[[sd_col]]), 1, test_df[[sd_col]])
}

# Concatenate historical and test datasets
library(dplyr)
library(data.table) # Use shift function for more flexibility and efficiency

valid_df$DateTime <- as.character(valid_df$DateTime)
valid_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", valid_df$DateTime)] <-
  paste0(valid_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", valid_df$DateTime)], " 00:00:00")
valid_df$DateTime <- ymd_hms(valid_df$DateTime, tz = "UTC")
valid_df$DateTime <- with_tz(valid_df$DateTime, tzone = "Europe/Berlin")

valid_df <- valid_df |>
  mutate(
    hour_of_day = hour(DateTime),
    day_of_week = wday(date, week_start = 1),
    month_of_year = month(DateTime),
    year = year(DateTime)
  )

names(valid_df)

library(data.table)
setDT(valid_df)
setDT(test_df)
valid_df <- valid_df[order(DateTime)]
test_df <- test_df[order(DateTime)]
valid_df[, data_type := "history"]
test_df[, data_type := "test"]

full_df <- rbindlist(list(valid_df, test_df), use.names = TRUE, fill = TRUE)
setorder(full_df, DateTime)

# Lag unit hours
lag_hours <- c(24, 48, 24*24, 48*24, 72*24, 168*24)

# Variable name mapping (variables ending with z → lag column suffix)
vars_to_lag <- list(
  Price_winsorized_z = "Pw",
  Load_DA_z = "Load",
  Solar_DA_z = "Solar",
  WindOff_DA_z = "WindOff"
)

# Construct lag columns
for (var_z in names(vars_to_lag)) {
  suffix <- vars_to_lag[[var_z]]
  for (lag_hr in lag_hours) {
    lag_name <- paste0("Lag_", lag_hr / 24, "d_", suffix)
    full_df[, (lag_name) := shift(.SD[[1]], n = lag_hr, type = "lag"), .SDcols = var_z]
  }
}

# Split out test data, lag columns correctly generated
test_with_lags <- full_df[data_type == "test"]
valid_df_with_lags <- full_df[data_type == "history"]

# Clean up auxiliary columns
test_with_lags[, data_type := NULL]
valid_df_with_lags[, data_type := NULL]

# Return results
test_with_lags

# Convert back to data.frame if you use dplyr later
test_with_lags <- as.data.frame(test_with_lags)
test_with_lags <- test_with_lags %>%
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
    )
  )

test_with_lags <- test_with_lags %>%
  mutate(
    load_is_peak = ifelse(hour_of_day %in% c(7:9, 18:20), 1, 0)
  )

library(dplyr)
library(lubridate)

# Store prediction results
preds_list <- list()
for (h in 0:23) {
  hour_data <- test_with_lags %>% filter(hour_of_day == h)
  
  if (nrow(hour_data) == 0) next
  
  model_name <- paste0("hour_", h)
  model <- arx_model[[model_name]] # or marx_model, depending on which model you use
  
  if (is.null(model)) {
    message(paste("No model for hour", h))
    next
  }
  
  # Predict
  preds <- predict(model, newdata = hour_data)
  
  preds_list[[model_name]] <- data.frame(
    DateTime = hour_data$DateTime,
    predicted = preds
  )
}

# Combine all hourly prediction results
arx_final_preds <- bind_rows(preds_list) %>%
  arrange(DateTime)

# View results
head(arx_final_preds)

# MARX
preds_list <- list()
for (h in 0:23) {
  hour_data <- test_with_lags %>% filter(hour_of_day == h)
  
  if (nrow(hour_data) == 0) next
  
  model_name <- paste0("hour_", h)
  model <- marx_model[[model_name]]
  
  if (is.null(model)) {
    message(paste("No model for hour", h))
    next
  }
  
  # Predict
  preds <- predict(model, newdata = hour_data)
  
  preds_list[[model_name]] <- data.frame(
    DateTime = hour_data$DateTime,
    predicted = preds
  )
}

# Combine all hourly prediction results
marx_final_preds <- bind_rows(preds_list) %>%
  arrange(DateTime)

# View results
head(marx_final_preds)

quantiles <- seq(0.01, 0.99, by = 0.01)

# Assume qra_models is the loaded list of QRA models
qra_models <- readRDS("D:/Personal/study/CS/PythonSeminar/github/DS3/qra_models/rolling_window_10_QRA_models.rds")
print(qra_models)

test_with_lags <- test_with_lags[1:nrow(arx_final_preds), ] # Ensure consistent length
test_with_lags$arx_pred <- arx_final_preds$predicted
test_with_lags$marx_pred <- marx_final_preds$predicted

quantiles <- seq(0.01, 0.99, by = 0.01)
for (q in quantiles) {
  model_name <- paste0("q", q * 100) # e.g., "q5" represents 5%
  coefs <- coef(qra_models[[model_name]])
  
  test_with_lags[[model_name]] <- coefs[1] +
    coefs["arx"] * test_with_lags$arx_pred +
    coefs["marx"] * test_with_lags$marx_pred
}

saveRDS(test_with_lags, "D:/Personal/study/CS/PythonSeminar/github/DS3/finaltest/test_qra_predictions.rds")
write.csv(test_with_lags, "D:/Personal/study/CS/PythonSeminar/github/DS3/finaltest/test_qra_predictions.csv", row.names = FALSE)

qra_q50 <- test_with_lags %>% select(DateTime, q50)
write.csv(qra_q50, "D:/Personal/study/CS/PythonSeminar/github/DS3/finaltest/qra_q50.csv", row.names = FALSE)

evaluate_qra_predictions <- function(df, actual_col = "Price_winsorized_z",
                                     quantiles = seq(0.01, 0.99, by = 0.01),
                                     interval_bounds = c(0.05, 0.95),
                                     plot = TRUE) {
  library(dplyr)
  library(ggplot2)
  
  # Pinball Loss
  pinball_loss <- function(q, y_true, y_pred) {
    err <- y_true - y_pred
    return(mean(ifelse(err >= 0, q * err, (q - 1) * err), na.rm = TRUE))
  }
  
  pinball_results <- sapply(quantiles, function(q) {
    col_name <- paste0("q", q * 100)
    pinball_loss(q, df[[actual_col]], df[[col_name]])
  })
  names(pinball_results) <- paste0("q", quantiles * 100)
  
  # Interval Score (e.g., 90% interval)
  q_low <- paste0("q", interval_bounds[1] * 100)
  q_high <- paste0("q", interval_bounds[2] * 100)
  
  df <- df %>% mutate(
    interval_width = .data[[q_high]] - .data[[q_low]],
    covered = (.data[[actual_col]] >= .data[[q_low]]) & (.data[[actual_col]] <= .data[[q_high]])
  )
  
  avg_width <- mean(df$interval_width, na.rm = TRUE)
  coverage <- mean(df$covered, na.rm = TRUE)
  
  # Simple CRPS estimate (average Pinball loss)
  crps_est <- mean(pinball_results, na.rm = TRUE)
  
  # Visualization
  if (plot) {
    p <- ggplot(df, aes(x = DateTime)) +
      geom_line(aes(y = !!sym(actual_col)), color = "black", size = 0.7) +
      geom_line(aes(y = q50), color = "blue", linetype = "dashed") +
      geom_ribbon(aes(ymin = !!sym(q_low), ymax = !!sym(q_high)), fill = "skyblue", alpha = 0.3) +
      labs(title = "QRA Prediction Intervals vs Actual Values", y = "Standardized Electricity Price", x = "") +
      theme_minimal()
    print(p)
  }
  
  # Summary of results
  result_summary <- list(
    pinball_loss_per_q = pinball_results,
    pinball_loss_mean = mean(pinball_results, na.rm = TRUE),
    avg_interval_width = avg_width,
    interval_coverage = coverage,
    crps_estimate = crps_est
  )
  
  return(result_summary)
}

# Assume test_df contains q1, q2, ..., q99 and Price_winsorized_z
result <- evaluate_qra_predictions(test_with_lags)

# Print summary
str(result)

library(ggplot2)

# Extract data from results
pinball_loss <- result$pinball_loss_per_q
quantiles <- as.numeric(gsub("q", "", names(pinball_loss))) # Extract 1-99 quantiles

# Create data frame
plot_data <- data.frame(
  Quantile = quantiles,
  PinballLoss = as.numeric(pinball_loss)
)

# Plot Pinball loss curve
ggplot(plot_data, aes(x = Quantile, y = PinballLoss)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "QRA Pinball Loss Across Quantiles",
    subtitle = paste("Mean Pinball Loss:", round(result$pinball_loss_mean, 4),
                     "| CRPS:", round(result$crps_estimate, 4)),
    x = "Quantile Level (%)",
    y = "Pinball Loss"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  # Mark key quantile regions
  annotate("rect", xmin = 1, xmax = 5, ymin = 0, ymax = max(plot_data$PinballLoss),
           alpha = 0.1, fill = "green") +
  annotate("rect", xmin = 25, xmax = 40, ymin = 0, ymax = max(plot_data$PinballLoss),
           alpha = 0.1, fill = "red") +
  annotate("rect", xmin = 95, xmax = 99, ymin = 0, ymax = max(plot_data$PinballLoss),
           alpha = 0.1, fill = "green") +
  annotate("text", x = 3, y = max(plot_data$PinballLoss)*0.95,
           label = "Low Loss\n(Extreme Quantiles)", size = 3) +
  annotate("text", x = 32.5, y = max(plot_data$PinballLoss)*0.95,
           label = "High Loss\n(25-40 Quantiles)", size = 3) +
  annotate("text", x = 97, y = max(plot_data$PinballLoss)*0.95,
           label = "Low Loss\n(Extreme Quantiles)", size = 3)

library(ggplot2)
library(dplyr)

plot_calibration <- function(df, actual_col = "Price_winsorized_z", quantiles = seq(0.01, 0.99, by = 0.01)) {
  
  coverage_results <- sapply(quantiles, function(q) {
    lower_col <- paste0("q", floor(q * 100))
    
    # Calculate the proportion of actual observations less than or equal to the predicted quantile
    mean(df[[actual_col]] <= df[[lower_col]], na.rm = TRUE)
  })
  
  calib_df <- data.frame(
    Quantile = quantiles,
    Empirical_Coverage = coverage_results
  )
  
  ggplot(calib_df, aes(x = Quantile, y = Empirical_Coverage)) +
    geom_line(color = "blue", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "QRA Prediction Calibration Plot",
      x = "Theoretical Quantile",
      y = "Actual Coverage"
    ) +
    theme_minimal() +
    coord_equal()
}

# Example call, ensure your df contains q1, q2, ... q99 columns, and the standardized actual value column
plot_calibration(test_with_lags, actual_col = "Price_winsorized_z")

library(ggplot2)
library(dplyr)

plot_reliability_diagram <- function(df, actual_col = "Price_winsorized_z", quantiles = seq(0.01, 0.99, by = 0.01)) {
  coverage_results <- sapply(quantiles, function(q) {
    col_name <- paste0("q", floor(q * 100))
    mean(df[[actual_col]] <= df[[col_name]], na.rm = TRUE)
  })
  
  calib_df <- data.frame(
    Theoretical_Quantile = quantiles,
    Empirical_Coverage = coverage_results
  )
  
  ggplot(calib_df, aes(x = Theoretical_Quantile, y = Empirical_Coverage)) +
    geom_line(color = "blue", size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Reliability Diagram",
         x = "Theoretical Quantile (Predicted Probability)",
         y = "Empirical Coverage (Actual Frequency)") +
    theme_minimal() +
    coord_equal()
}

# Run example
plot_reliability_diagram(test_with_lags, actual_col = "Price_winsorized_z")

library(ggplot2)
library(dplyr)

plot_coverage_width_tradeoff <- function(df, actual_col = "Price_winsorized_z", interval_probs = seq(0.05, 0.95, by = 0.05)) {
  results <- lapply(interval_probs, function(p) {
    lower_q <- (1 - p) / 2
    upper_q <- 1 - lower_q
    
    lower_col <- paste0("q", floor(lower_q * 100))
    upper_col <- paste0("q", floor(upper_q * 100))
    
    interval_width <- mean(df[[upper_col]] - df[[lower_col]], na.rm = TRUE)
    coverage <- mean(df[[actual_col]] >= df[[lower_col]] & df[[actual_col]] <= df[[upper_col]], na.rm = TRUE)
    
    data.frame(
      Coverage_Prob = p,
      Interval_Width = interval_width,
      Empirical_Coverage = coverage
    )
  }) %>% bind_rows()
  
  ggplot(results, aes(x = Interval_Width, y = Empirical_Coverage)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "red") +
    labs(title = "Coverage-Width Tradeoff Curve",
         x = "Average Prediction Interval Width",
         y = "Actual Coverage") +
    theme_minimal()
}

# Run example
plot_coverage_width_tradeoff(test_with_lags, actual_col = "Price_winsorized_z")

coverage_error <- function(df, actual_col = "Price_winsorized_z", quantiles = seq(0.01, 0.99, by = 0.01)) {
  coverage_results <- sapply(quantiles, function(q) {
    col_name <- paste0("q", floor(q * 100))
    mean(df[[actual_col]] <= df[[col_name]], na.rm = TRUE)
  })
  error <- mean(abs(coverage_results - quantiles))
  return(error)
}

coverage_err <- coverage_error(test_with_lags, actual_col = "Price_winsorized_z")
cat(sprintf("Average Quantile Coverage Error: %.4f\n", coverage_err))

