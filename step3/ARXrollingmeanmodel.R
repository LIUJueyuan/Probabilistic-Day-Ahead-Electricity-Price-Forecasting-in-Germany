library(dplyr)
library(lubridate)
library(RcppRoll)
library(moments)

# ================== Data Reading ==================
train_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/2train_df_Abnormalvalue.csv")
beforetrain <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/before_df_winsorized.csv")

# ================== Datetime Column Preprocessing ==================
# 1. Convert to character if it's character or factor
train_df$DateTime <- as.character(train_df$DateTime)
# 2. Append "00:00:00" if time is not present to avoid NA
train_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", train_df$DateTime)] <-
  paste0(train_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", train_df$DateTime)], " 00:00:00")
# 3. Parse as POSIXct without specifying timezone
train_df$DateTime <- ymd_hms(train_df$DateTime, tz = "UTC")
# 4. Convert to Europe/Berlin timezone (simulate local time)
train_df$DateTime <- with_tz(train_df$DateTime, tzone = "Europe/Berlin")
train_df <- train_df %>%
  mutate(
    hour_of_day = hour(DateTime),
    day_of_week = wday(date, week_start = 1),
    month_of_year = month(DateTime),
    year = year(DateTime)
  )

# 1. Convert to character if it's character or factor
beforetrain$DateTime <- as.character(beforetrain$DateTime)
# 2. Append "00:00:00" if time is not present to avoid NA
beforetrain$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", beforetrain$DateTime)] <-
  paste0(beforetrain$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", beforetrain$DateTime)], " 00:00:00")
# 3. Parse as POSIXct without specifying timezone
beforetrain$DateTime <- ymd_hms(beforetrain$DateTime, tz = "UTC")
# 4. Convert to Europe/Berlin timezone (simulate local time)
beforetrain$DateTime <- with_tz(beforetrain$DateTime, tzone = "Europe/Berlin")
beforetrain <- beforetrain %>%
  mutate(
    hour_of_day = hour(DateTime),
    day_of_week = wday(date, week_start = 1),
    month_of_year = month(DateTime),
    year = year(DateTime)
  )

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

check_hourly_continuity(train_df)
check_hourly_continuity(beforetrain)

fill_and_interpolate <- function(df, datetime_col = "DateTime", value_col = "Price") {
  library(dplyr)
  library(lubridate)
  library(zoo)
  
  # Generate complete time sequence (hourly)
  full_seq <- data.frame(
    DateTime = seq(min(df[[datetime_col]]), max(df[[datetime_col]]), by = "hour")
  )
  
  # Fill in time
  df_filled <- full_seq %>%
    left_join(df, by = datetime_col)
  
  # Ensure datetime column is POSIXct, sorted, and unique
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

train_df <- fill_and_interpolate(train_df, datetime_col = "DateTime", value_col = "Price")
beforetrain <- fill_and_interpolate(beforetrain, datetime_col = "DateTime", value_col = "Price")

check_hourly_continuity(train_df)
check_hourly_continuity(beforetrain)

# ================== Rolling Window Parameters ==================
window_size_days <- 180    # Use a 180-day window
step_size_days <- 7        # Roll every 7 days
start_date <- min(train_df$DateTime) + days(window_size_days)
end_date <- max(train_df$DateTime)

# Generate rolling prediction date sequence
dates_to_predict <- seq(from = start_date, to = end_date, by = "7 days")

# ================== Build Lag and Standardization Functions ==================
construct_lags <- function(train_df, beforetrain) {
  # Merge windows
  full_df <- bind_rows(beforetrain, train_df) %>%
    arrange(DateTime)
  
  # ===== Step 1: Standardize first, then lag =====
  full_df <- full_df %>%
    add_count(hour_of_day) %>%      # Count each group
    filter(n > 1) %>%
    mutate(
      Price_winsorized_z = scale(Price_winsorized)[,1],
      Load_DA_z = scale(Load_DA)[,1],
      Solar_DA_z = scale(Solar_DA)[,1],
      WindOn_DA_z = scale(WindOn_DA)[,1],
      WindOff_DA_z = scale(WindOff_DA)[,1]
    ) %>%
    ungroup()
  
  # Construct lagged variables (unit in "days", i.e., hours × 24)
  full_df <- full_df %>%
    mutate(
      Lag_1d_Pw = dplyr::lag(Price_winsorized, n = 24),
      Lag_2d_Pw = dplyr::lag(Price_winsorized, n = 48),
      Lag_24d_Pw = dplyr::lag(Price_winsorized, n = 24 * 24),
      Lag_48d_Pw = dplyr::lag(Price_winsorized, n = 48 * 24),
      Lag_72d_Pw = dplyr::lag(Price_winsorized, n = 72 * 24),
      Lag_168d_Pw = dplyr::lag(Price_winsorized, n = 168 * 24),
      
      Lag_1d_Load = dplyr::lag(Load_DA_z, n = 24),
      Lag_2d_Load = dplyr::lag(Load_DA_z, n = 48),
      Lag_24d_Load = dplyr::lag(Load_DA_z, n = 24 * 24),
      Lag_48d_Load = dplyr::lag(Load_DA_z, n = 48 * 24),
      Lag_72d_Load = dplyr::lag(Load_DA_z, n = 72 * 24),
      Lag_168d_Load = dplyr::lag(Load_DA_z, n = 168 * 24),
      
      Lag_1d_Solar = dplyr::lag(Solar_DA_z, n = 24),
      Lag_2d_Solar = dplyr::lag(Solar_DA_z, n = 48),
      Lag_24d_Solar = dplyr::lag(Solar_DA_z, n = 24 * 24),
      Lag_48d_Solar = dplyr::lag(Solar_DA_z, n = 48 * 24),
      Lag_72d_Solar = dplyr::lag(Solar_DA_z, n = 72 * 24),
      Lag_168d_Solar = dplyr::lag(Solar_DA_z, n = 168 * 24),
      
      Lag_1d_WindOff = dplyr::lag(WindOff_DA_z, n = 24),
      Lag_2d_WindOff = dplyr::lag(WindOff_DA_z, n = 48),
      Lag_24d_WindOff = dplyr::lag(WindOff_DA_z, n = 24 * 24),
      Lag_48d_WindOff = dplyr::lag(WindOff_DA_z, n = 48 * 24),
      Lag_72d_WindOff = dplyr::lag(WindOff_DA_z, n = 72 * 24),
      Lag_168d_WindOff = dplyr::lag(WindOff_DA_z, n = 168 * 24)
    )
  
  # Keep only the train_df portion
  result_df <- full_df %>%
    slice((nrow(full_df) - nrow(train_df) + 1):nrow(full_df))
  
  return(result_df)
}

# Run the function to get the constructed feature set
processed_train <- construct_lags(train_df = train_df, beforetrain = beforetrain)

# Quickly check the number of rows, columns, and variable names
dim(processed_train)       # Output number of rows and columns
names(processed_train)     # All variable names

processed_train %>%
  summarise(
    mean_Pw_z = mean(Price_winsorized_z, na.rm = TRUE),
    sd_Pw_z = sd(Price_winsorized_z, na.rm = TRUE)
  )

summary(processed_train$Lag_1d_Pw)
summary(processed_train$Lag_168d_Load)

# Visualization
library(ggplot2)
ggplot(processed_train, aes(x = Lag_1d_Pw)) + geom_histogram(bins = 50)
ggplot(processed_train, aes(x = Lag_1d_Pw)) +
  geom_density(fill = "skyblue", alpha = 0.4) +
  geom_vline(aes(xintercept = mean(Lag_1d_Pw, na.rm = TRUE)),
             color = "red", linetype = "dashed") +
  labs(title = "Lag_1d_Pw Density", x = "Lag_1d_Pw", y = "Density")

processed_train$hour_sin <- sin(2 * pi * processed_train$hour_of_day / 24)
processed_train$hour_cos <- cos(2 * pi * processed_train$hour_of_day / 24)
processed_train$daylight_hours <- ifelse(processed_train$hour_of_day %in% 6:20, processed_train$hour_of_day, 0)
processed_train$hour_sin2 <- sin(4 * pi * processed_train$hour_of_day / 24)
processed_train$hour_cos2 <- cos(4 * pi * processed_train$hour_of_day / 24)
processed_train$daylight_sin <- sin(2 * pi * processed_train$daylight_hours / 24)
processed_train$daylight_cos <- cos(2 * pi * processed_train$daylight_hours / 24)

# Scheme 1: Segmented interaction (morning vs afternoon)
processed_train <- processed_train %>%
  mutate(
    solar_period = case_when(
      hour_of_day %in% 6:11 ~ "morning",
      hour_of_day %in% 12:17 ~ "afternoon",
      TRUE ~ "no_solar"
    ),
    solar_period = factor(solar_period)
  )

# Scheme 2: Polynomial term to capture inverted U-shape
processed_train$hour_solar <- ifelse(processed_train$hour_of_day %in% 6:20,
                                     (processed_train$hour_of_day - 13)^2, 0)  # Using 13 o'clock as vertex

# Load pattern classification based on typical electricity load curves
processed_train <- processed_train %>%
  mutate(
    load_hour_type = case_when(
      hour_of_day %in% c(7:9, 18:20) ~ "peak",
      hour_of_day %in% c(10:17) ~ "day_offpeak",
      hour_of_day %in% c(1:6, 21:23) ~ "night"
    )
  )

processed_train <- processed_train %>%
  mutate(
    month_sin = sin(2 * pi * month_of_year / 12),
    month_cos = cos(2 * pi * month_of_year / 12)
  )

processed_train <- processed_train %>%
  arrange(DateTime) %>%
  mutate(rolling_sd_price_24h = zoo::rollapply(Price_winsorized_z,
                                               width = 24,  # By hour
                                               FUN = sd,
                                               fill = NA,
                                               align = "right"))

rolling_arx_model <- function(data,
                              datetime_col = "DateTime",
                              target_col = "Price_winsorized_z",
                              window_size_days = 180,
                              step_size_days = 7,
                              min_train_size = 3000) {
  # Ensure necessary packages are loaded
  require(dplyr)
  require(lubridate)
  
  # 1. Prepare time series
  data <- data %>%
    arrange(!!sym(datetime_col)) %>%
    mutate(row_id = row_number())
  
  # 2. Formula
  simple_formula <- as.formula(paste(target_col, "~
    Lag_1d_Pw + Lag_2d_Pw + Lag_24d_Pw + Lag_48d_Pw + Lag_72d_Pw + Lag_168d_Pw +
    Load_DA_z + Solar_DA_z + WindOff_DA_z +
    Lag_24d_WindOff + Lag_48d_WindOff + Lag_72d_WindOff + Lag_168d_WindOff + Lag_24d_Solar + Lag_24d_Load +
    hour_sin2 + hour_cos2 +
    month_sin + is_pre_holiday + is_post_holiday +
    WindOff_DA_z * month_sin +
    load_hour_type * is_post_holiday +
    Load_DA_z * load_hour_type +
    Solar_DA_z * daylight_sin"))
  
  # 3. Generate prediction date sequence
  start_date <- min(data[[datetime_col]]) + days(window_size_days)
  end_date <- max(data[[datetime_col]])
  dates_to_predict <- seq(start_date, end_date, by = paste(step_size_days, "days"))
  
  # 4. Rolling prediction
  results <- lapply(dates_to_predict, function(current_date) {
    # Calculate window boundaries
    train_start <- current_date - days(window_size_days)
    train_end <- current_date - seconds(1)  # Up to the previous second
    test_end <- current_date + days(step_size_days) - seconds(1)
    
    # Get training and testing data
    train_data <- data %>%
      filter(!!sym(datetime_col) >= train_start,
             !!sym(datetime_col) <= train_end)
    
    test_data <- data %>%
      filter(!!sym(datetime_col) >= current_date,
             !!sym(datetime_col) <= test_end)
    
    # Skip windows that do not meet conditions
    if (nrow(train_data) < min_train_size || nrow(test_data) == 0) {
      return(NULL)
    }
    
    # Train model and predict
    model <- lm(simple_formula, data = train_data)
    predictions <- predict(model, newdata = test_data)
    
    # Return results
    data.frame(
      DateTime = test_data[[datetime_col]],
      Actual = test_data[[target_col]],
      Predicted = predictions,
      WindowStart = current_date
    )
  }) %>%
    bind_rows()  # Combine all results
  
  return(results)
}

# Run
simple_results <- rolling_arx_model(
  data = processed_train,
  window_size_days = 180,
  step_size_days = 7
)

# View prediction effect overview
library(ggplot2)
ggplot(simple_results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(color = "red") +
  labs(title = "Actual vs Predicted Values") +
  theme_minimal()

# View prediction effect over time
ggplot(simple_results, aes(x = DateTime)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Time Series Prediction Effect", y = "Standardized Price") +
  theme_minimal()

# Check for NA in prediction results
summary(simple_results)

simple_results <- simple_results %>%
  mutate(Residual = Actual - Predicted)

ggplot(simple_results, aes(x = Residual)) +
  geom_histogram(bins = 50, fill = "tomato", alpha = 0.7) +
  labs(title = "Residual Distribution")

ggplot(simple_results, aes(x = DateTime, y = Residual)) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Residuals Over Time")

# Check for periodic bias
simple_results$hour <- hour(simple_results$DateTime)
ggplot(simple_results, aes(x = factor(hour), y = Residual)) +
  geom_boxplot() +
  labs(title = "Residuals by Hour of Day")

# Simple error analysis
simple_results <- simple_results %>%
  mutate(error = Predicted - Actual,
         abs_error = abs(error),
         squared_error = error^2)

# Overall performance
overall_metrics <- simple_results %>%
  summarise(
    RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
    MAE = mean(abs_error, na.rm = TRUE),
    R2 = 1 - sum(squared_error, na.rm = TRUE) / sum((Actual - mean(Actual, na.rm = TRUE))^2, na.rm = TRUE)
  )
print(overall_metrics)

mean_bias <- mean(simple_results$error, na.rm = TRUE)
print(mean_bias)

window_metrics <- simple_results %>%
  group_by(WindowStart) %>%
  summarise(
    RMSE = sqrt(mean((Predicted - Actual)^2, na.rm = TRUE)),
    MAE = mean(abs(Predicted - Actual), na.rm = TRUE),
    bias = mean(Predicted - Actual, na.rm = TRUE),
    .groups = "drop"
  )

# View first few rows
print(head(window_metrics, 10))

# 1. Calculate error (if not calculated beforehand)
simple_results <- simple_results %>%
  mutate(error = Predicted - Actual,
         abs_error = abs(error))

# 2. Add original feature information (e.g., load_hour_type)
simple_results <- simple_results %>%
  left_join(processed_train %>% select(DateTime, load_hour_type, is_post_holiday), by = "DateTime")

# 3. Summarize error metrics by feature
bias_by_feature <- simple_results %>%
  group_by(load_hour_type, is_post_holiday) %>%
  summarise(
    mean_error = mean(error, na.rm = TRUE),
    mean_abs_error = mean(abs_error, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(mean_abs_error))
print(bias_by_feature)

# 1. Find top 50 samples with the largest bias
top_errors <- simple_results %>%
  mutate(abs_error = abs(Predicted - Actual)) %>%
  arrange(desc(abs_error)) %>%
  slice_head(n = 50)
print(top_errors)

rolling_arx_model_save <- function(data,
                                   datetime_col = "DateTime",
                                   target_col = "Price_winsorized_z",
                                   window_size_days = 180,
                                   step_size_days = 7,
                                   min_train_size = 3000) {
  require(dplyr)
  require(lubridate)
  
  data <- data %>%
    arrange(!!sym(datetime_col)) %>%
    mutate(row_id = row_number())
  
  # Model formula
  simple_formula <- as.formula(paste(target_col, "~
    Lag_1d_Pw + Lag_2d_Pw + Lag_24d_Pw + Lag_48d_Pw + Lag_72d_Pw + Lag_168d_Pw +
    Load_DA_z + Solar_DA_z + WindOff_DA_z +
    Lag_24d_WindOff + Lag_48d_WindOff + Lag_72d_WindOff + Lag_168d_WindOff +
    Lag_24d_Solar + Lag_24d_Load +
    hour_sin2 + hour_cos2 + month_sin +
    is_pre_holiday + is_post_holiday +
    WindOff_DA_z * month_sin +
    load_hour_type * is_post_holiday +
    Load_DA_z * load_hour_type +
    Solar_DA_z * daylight_sin"))
  
  start_date <- min(data[[datetime_col]]) + days(window_size_days)
  end_date <- max(data[[datetime_col]])
  dates_to_predict <- seq(start_date, end_date, by = paste(step_size_days, "days"))
  
  model_results <- list()
  
  for (i in seq_along(dates_to_predict)) {
    current_date <- dates_to_predict[i]
    train_start <- current_date - days(window_size_days)
    train_end <- current_date - seconds(1)
    test_end <- current_date + days(step_size_days) - seconds(1)
    
    train_data <- data %>%
      filter(!!sym(datetime_col) >= train_start, !!sym(datetime_col) <= train_end)
    
    test_data <- data %>%
      filter(!!sym(datetime_col) >= current_date, !!sym(datetime_col) <= test_end)
    
    if (nrow(train_data) < min_train_size || nrow(test_data) == 0) {
      next
    }
    
    model <- lm(simple_formula, data = train_data)
    predictions <- predict(model, newdata = test_data)
    
    model_results[[paste0("window_", i)]] <- list(
      model = model,
      train_range = c(train_start, train_end),
      test_range = c(current_date, test_end),
      test_predictions = data.frame(
        DateTime = test_data[[datetime_col]],
        Actual = test_data[[target_col]],
        Predicted = predictions
      )
    )
  }
  
  return(model_results)
}

model_list <- rolling_arx_model_save(processed_train)
saveRDS(model_list, "arx_rolling_models.rds")
length(model_list)

library(ggplot2)
df1 <- model_list[[1]]$test_predictions
ggplot(df1, aes(x = DateTime)) +
  geom_line(aes(y = Actual), color = "black", size = 1) +
  geom_line(aes(y = Predicted), color = "red", linetype = "dashed") +
  labs(title = "Rolling ARX Model - Window 1",
       y = "Price (z)", x = "DateTime") +
  theme_minimal()

summary(model_list[[1]]$model)
