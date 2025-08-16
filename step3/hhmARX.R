library(dplyr)
library(lubridate)
library(RcppRoll)
library(moments)

# ================== Data Loading ==================
train_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/2train_df_Abnormalvalue.csv")
beforetrain <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/before_df_winsorized.csv")

# ================== DateTime Preprocessing ==================
# Process train_df datetime
train_df$DateTime <- as.character(train_df$DateTime)
train_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", train_df$DateTime)] <-
  paste0(train_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", train_df$DateTime)], " 00:00:00")
train_df$DateTime <- ymd_hms(train_df$DateTime, tz = "UTC")
train_df$DateTime <- with_tz(train_df$DateTime, tzone = "Europe/Berlin")
train_df <- train_df %>%
  mutate(
    hour_of_day = hour(DateTime),
    day_of_week = wday(date, week_start = 1),
    month_of_year = month(DateTime),
    year = year(DateTime)
  )

# Process beforetrain datetime
beforetrain$DateTime <- as.character(beforetrain$DateTime)
beforetrain$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", beforetrain$DateTime)] <-
  paste0(beforetrain$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", beforetrain$DateTime)], " 00:00:00")
beforetrain$DateTime <- ymd_hms(beforetrain$DateTime, tz = "UTC")
beforetrain$DateTime <- with_tz(beforetrain$DateTime, tzone = "Europe/Berlin")
beforetrain <- beforetrain %>%
  mutate(
    hour_of_day = hour(DateTime),
    day_of_week = wday(date, week_start = 1),
    month_of_year = month(DateTime),
    year = year(DateTime)
  )

# ================== Continuity Check Function ==================
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

check_hourly_continuity(train_df)
check_hourly_continuity(beforetrain)

# ================== Fill and Interpolate Function ==================
fill_and_interpolate <- function(df, datetime_col = "DateTime", value_col = "Price") {
  library(dplyr)
  library(lubridate)
  library(zoo)
  
  # Generate complete hourly time sequence
  full_seq <- data.frame(
    DateTime = seq(min(df[[datetime_col]]), max(df[[datetime_col]]), by = "hour")
  )
  
  # Fill missing timestamps
  df_filled <- full_seq %>%
    left_join(df, by = datetime_col)
  
  # Ensure POSIXct format, sort, and remove duplicates
  df_filled[[datetime_col]] <- as.POSIXct(df_filled[[datetime_col]])
  df_filled <- df_filled %>%
    arrange(.data[[datetime_col]]) %>%
    distinct(.data[[datetime_col]], .keep_all = TRUE)
  
  # Interpolate missing values using rule=2 for boundary handling
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

# ================== Feature Engineering ==================
# Cyclical time features
train_df$hour_sin <- sin(2 * pi * train_df$hour_of_day / 24)
train_df$hour_cos <- cos(2 * pi * train_df$hour_of_day / 24)
train_df$daylight_hours <- ifelse(train_df$hour_of_day %in% 6:20, train_df$hour_of_day, 0)
train_df$hour_sin2 <- sin(4 * pi * train_df$hour_of_day / 24)
train_df$hour_cos2 <- cos(4 * pi * train_df$hour_of_day / 24)
train_df$daylight_sin <- sin(2 * pi * train_df$daylight_hours / 24)
train_df$daylight_cos <- cos(2 * pi * train_df$daylight_hours / 24)

# Solar period segmentation (morning vs afternoon)
train_df <- train_df %>%
  mutate(
    solar_period = case_when(
      hour_of_day %in% 6:11 ~ "morning",
      hour_of_day %in% 12:17 ~ "afternoon",
      TRUE ~ "no_solar"
    ),
    solar_period = factor(solar_period)
  )

# Polynomial term for inverted U-shape solar pattern
train_df$hour_solar <- ifelse(train_df$hour_of_day %in% 6:20,
                              (train_df$hour_of_day - 13)^2, 0)

# Load pattern classification
train_df <- train_df %>%
  mutate(
    load_is_peak = ifelse(hour_of_day %in% c(7:9, 18:20), 1, 0)
  )

# Monthly cyclical features
train_df <- train_df %>%
  mutate(
    month_sin = sin(2 * pi * month_of_year / 12),
    month_cos = cos(2 * pi * month_of_year / 12)
  )

# Remove unnecessary columns
train_df <- train_df %>% select(-c(med, mad_val, upper, lower))

# ================== Unified Rolling Window Parameters ==================
WINDOW_SIZE_DAYS <- 180    # Rolling window size in days
STEP_SIZE_DAYS <- 7        # Rolling step size in days
MAX_ITERATIONS <- 10       # Maximum number of rolling windows

# Calculate start date for rolling windows
start_date <- min(train_df$DateTime)

# ================== Rolling Window Data Preparation Function ==================
rolling_window_data_prep <- function(beforetrain, train_window, datetime_col = "DateTime",
                                     target_col = "Price_winsorized",
                                     vars_to_standardize = c("Price_winsorized", "Load_DA", "Solar_DA", "WindOn_DA", "WindOff_DA"),
                                     lag_days = c(1, 2, 24, 48, 72, 168)) {
  library(dplyr)
  library(lubridate)
  
  # Convert lag days to hours for internal calculation
  lag_hours <- lag_days * 24
  
  combined_train <- bind_rows(beforetrain, train_window) %>% arrange(.data[[datetime_col]])
  combined_train <- combined_train %>% mutate(hour_of_day = hour(.data[[datetime_col]]))
  
  # Calculate hourly statistics from training window
  hourly_stats <- train_window %>%
    mutate(hour_of_day = hour(.data[[datetime_col]])) %>%
    group_by(hour_of_day) %>%
    summarise(across(all_of(vars_to_standardize),
                     list(mean = ~mean(.x, na.rm=TRUE), sd = ~sd(.x, na.rm=TRUE)),
                     .names = "{.col}_{.fn}"),
              .groups = "drop")
  
  # Join hourly statistics to combined training data
  combined_train <- combined_train %>%
    left_join(hourly_stats, by = "hour_of_day")
  
  # Standardize variables by hour
  for (var in vars_to_standardize) {
    mean_col <- paste0(var, "_mean")
    sd_col <- paste0(var, "_sd")
    z_col <- paste0(var, "_z")
    combined_train[[z_col]] <- (combined_train[[var]] - combined_train[[mean_col]]) /
      ifelse(combined_train[[sd_col]] == 0, 1, combined_train[[sd_col]])
  }
  
  # Create lag features based on standardized variables
  combined_train <- combined_train %>%
    arrange(.data[[datetime_col]])
  
  for (i in seq_along(lag_days)) {
    lag <- lag_hours[i]
    lag_day <- lag_days[i]
    combined_train[[paste0("Lag_", lag_day, "d_Pw")]] <- dplyr::lag(combined_train$Price_winsorized_z, n = lag)
    combined_train[[paste0("Lag_", lag_day, "d_Load")]] <- dplyr::lag(combined_train$Load_DA_z, n = lag)
    combined_train[[paste0("Lag_", lag_day, "d_Solar")]] <- dplyr::lag(combined_train$Solar_DA_z, n = lag)
    combined_train[[paste0("Lag_", lag_day, "d_WindOff")]] <- dplyr::lag(combined_train$WindOff_DA_z, n = lag)
  }
  
  # Filter to keep only training window data
  train_final <- combined_train %>%
    filter(.data[[datetime_col]] >= min(train_window[[datetime_col]]),
           .data[[datetime_col]] <= max(train_window[[datetime_col]]))
  
  return(list(
    data = train_final,
    hourly_stats = hourly_stats
  ))
}

# ================== Hourly Model Training Function ==================
train_hourly_marx_models <- function(data, datetime_col = "DateTime", target_col = "Price_winsorized_z",
                                     min_train_size = 100, hour_feature_map = NULL) {
  data <- data %>%
    arrange(.data[[datetime_col]]) %>%
    mutate(hour_of_day = hour(.data[[datetime_col]]))
  
  models_list <- list()
  
  for (h in 0:23) {
    # Filter data for specific hour
    hour_data <- data %>% filter(hour_of_day == h)
    
    if (nrow(hour_data) < min_train_size) {
      message(paste("Hour", h, "insufficient training data, skipping"))
      next
    }
    
    # Use hour-specific formula if provided, otherwise use default
    if (!is.null(hour_feature_map) && !is.null(hour_feature_map[[as.character(h)]])) {
      formula <- hour_feature_map[[as.character(h)]]
    } else {
      formula <- as.formula(paste(target_col, "~ Lag_1d_Pw + Load_DA_z + Solar_DA_z + WindOff_DA_z"))
    }
    
    model <- lm(formula, data = hour_data)
    models_list[[paste0("hour_", h)]] <- model
    
    message(paste("Hour", h, "model training completed, sample size:", nrow(hour_data)))
  }
  
  return(models_list)
}

# ================== Hour-Specific Feature Mapping ==================
# Initialize empty list
hour_feature_map <- list()
for (d in 0:23) {
  # Base formula (common for all hours)
  base_terms <- "
    Lag_1d_Pw + Lag_2d_Pw + Lag_24d_Pw + Lag_48d_Pw + Lag_72d_Pw + Lag_168d_Pw +
    hour_sin + hour_cos + month_sin + Load_DA_z + WindOff_DA_z + Solar_DA_z"
  
  formula <- NULL
  
  if (d %in% 0:5) {
    # 🌙 Early morning: Focus on load & wind
    formula <- paste0(base_terms, " +
      Lag_24d_Load + Lag_168d_Load + Lag_24d_WindOff + Lag_168d_WindOff +
      is_post_holiday + load_is_peak + WindOff_DA_z * month_sin")
    
  } else if (d %in% 6:9) {
    # 🌅 Morning: Rising load, increasing wind and solar
    formula <- paste0(base_terms, " +
      Lag_24d_Solar + Lag_168d_Solar + daylight_sin + is_weekend +
      Solar_DA_z * daylight_sin + Load_DA_z * load_is_peak")
    
  } else if (d %in% 10:14) {
    # ☀️ Noon: Full solar, stable load
    formula <- paste0(base_terms, " +
      Lag_24d_Solar + Lag_48d_Solar + Lag_72d_Solar + Lag_168d_Solar +
      daylight_sin + daylight_cos + Solar_DA_z * daylight_sin")
    
  } else if (d %in% 15:17) {
    # 🌤️ Afternoon: Balanced influencing factors
    formula <- paste0(base_terms, " +
      is_weekend + is_pre_holiday +
      Solar_DA_z * daylight_cos + WindOff_DA_z * is_weekend")
    
  } else if (d %in% 18:19) {
    # 🌇 Evening peak: Strong wind, high load
    formula <- paste0(base_terms, " +
      Lag_24d_Load + Lag_168d_Load + Lag_24d_WindOff + Lag_168d_WindOff +
      is_post_holiday + is_pre_holiday +
      WindOff_DA_z * month_sin + Load_DA_z * load_is_peak")
    
  } else if (d %in% 20:23) {
    # 🌙 Night: Wind dominant + holiday effects
    formula <- paste0(base_terms, " +
      Lag_48d_Load + Lag_48d_WindOff + Lag_168d_WindOff +
      is_weekend + is_holiday + is_pre_holiday +
      WindOff_DA_z * is_weekend + Load_DA_z * is_holiday")
    
  }
  
  # If not captured by above, apply default structure
  if (is.null(formula)) {
    formula <- paste0(base_terms, " +
      daylight_sin + is_weekend + load_is_peak")
  }
  
  # Construct full formula
  full_formula <- as.formula(paste("Price_winsorized_z ~", formula))
  hour_feature_map[[as.character(d)]] <- full_formula
}

# ================== Training Window Extraction Function ==================
get_train_window <- function(data, start_date, window_size_days, datetime_col = "DateTime") {
  end_date <- start_date + days(window_size_days) - seconds(1)
  data %>% filter(.data[[datetime_col]] >= start_date & .data[[datetime_col]] <= end_date)
}

# ================== Model Evaluation Function ==================
library(Metrics)
evaluate_train_fit <- function(models_list, train_data, datetime_col = "DateTime", target_col = "Price_winsorized_z") {
  train_data <- train_data %>% mutate(hour_of_day = lubridate::hour(.data[[datetime_col]]))
  
  fit_results <- data.frame(hour = integer(), mse = numeric(), n = integer())
  
  for (h in 0:23) {
    model_name <- paste0("hour_", h)
    if (!(model_name %in% names(models_list))) next
    
    model <- models_list[[model_name]]
    hour_data <- train_data %>% filter(hour_of_day == h)
    
    if (nrow(hour_data) == 0) next
    
    preds <- predict(model, newdata = hour_data)
    mse_val <- Metrics::mse(hour_data[[target_col]], preds)
    
    fit_results <- rbind(fit_results, data.frame(hour = h, mse = mse_val, n = nrow(hour_data)))
  }
  
  return(fit_results)
}

# ================== Verbose Model Evaluation Function ==================
evaluate_train_fit_verbose <- function(models_list, train_data, datetime_col = "DateTime", target_col = "Price_winsorized_z") {
  library(lubridate)
  library(Metrics)
  
  train_data <- train_data %>% mutate(hour_of_day = lubridate::hour(.data[[datetime_col]]))
  
  fit_results <- data.frame(hour = integer(), mse = numeric(), n = integer())
  
  for (h in 0:23) {
    model_name <- paste0("hour_", h)
    if (!(model_name %in% names(models_list))) {
      message(paste("❌ Model", model_name, "does not exist, skipping"))
      next
    }
    
    hour_data <- train_data %>% filter(hour_of_day == h)
    cat("🕑 Hour:", h, "Data count:", nrow(hour_data), "\n")
    if (nrow(hour_data) == 0) next
    
    # Add tryCatch for prediction errors
    preds <- tryCatch({
      predict(models_list[[model_name]], newdata = hour_data)
    }, error = function(e) {
      message(paste("❗ Prediction failed for hour", h, "Reason:", e$message))
      return(NULL)
    })
    
    if (is.null(preds)) {
      fit_results <- rbind(fit_results, data.frame(hour = h, mse = NA, n = nrow(hour_data)))
      next
    }
    
    mse_val <- tryCatch({
      mean((hour_data[[target_col]] - preds)^2, na.rm = TRUE)
    }, error = function(e) {
      message(paste("❗ MSE calculation failed for hour", h, "Reason:", e$message))
      return(NA)
    })
    
    message("Hour ", h, " -> Target NA:", sum(is.na(hour_data[[target_col]])),
            " | Pred NA:", sum(is.na(preds)))
    
    fit_results <- rbind(fit_results, data.frame(hour = h, mse = mse_val, n = nrow(hour_data)))
  }
  
  return(fit_results)
}

# ================== Main Rolling Window Training Loop ==================
all_models_rolling <- list()
all_data_rolling <- list()
for (i in 0:(MAX_ITERATIONS - 1)) {
  current_start_date <- start_date + days(i * STEP_SIZE_DAYS)
  train_window_slice <- get_train_window(train_df, current_start_date, WINDOW_SIZE_DAYS)
  
  if (nrow(train_window_slice) < 100) {
    message(paste("Rolling window", i + 1, "insufficient data, skipping"))
    next
  }
  
  prep_res_slice <- rolling_window_data_prep(
    beforetrain = beforetrain,
    train_window = train_window_slice,
    datetime_col = "DateTime",
    target_col = "Price_winsorized"
  )
  
  train_ready_slice <- prep_res_slice$data
  
  # Store processed data
  all_data_rolling[[paste0("rolling_window_", i + 1)]] <- train_ready_slice
  
  # Train models
  models_slice <- train_hourly_marx_models(train_ready_slice, hour_feature_map = hour_feature_map)
  all_models_rolling[[paste0("rolling_window_", i + 1)]] <- models_slice
  
  message(paste("Rolling window", i + 1, "model training completed"))
}

# ================== Model Evaluation Example ==================
# Evaluate the first rolling window's model fit
if (length(all_models_rolling) > 0 && length(all_data_rolling) > 0) {
  result <- evaluate_train_fit_verbose(
    models_list = all_models_rolling[[1]],
    train_data = all_data_rolling[[1]]
  )
  print("Model evaluation results for first rolling window:")
  print(result)
}

saveRDS(all_models_rolling, "all_models_rolling.rds")
# Load and use the i-th rolling window:
all_models_rolling <- readRDS("all_models_rolling.rds")
models_list <- all_models_rolling[[i]]

# ================== Enhanced Model Evaluation Function ==================
evaluate_model_performance <- function(models_list, test_data,
                                       datetime_col = "DateTime",
                                       target_col = "Price_winsorized_z") {
  library(Metrics)
  library(dplyr)
  
  test_data <- test_data %>%
    mutate(hour_of_day = hour(.data[[datetime_col]])) %>%
    arrange(.data[[datetime_col]])
  
  # Initialize results storage
  hourly_results <- data.frame(
    hour = integer(),
    mse = numeric(),
    mae = numeric(),
    r2 = numeric(),
    n = integer()
  )
  
  all_preds <- numeric(nrow(test_data))
  all_actuals <- numeric(nrow(test_data))
  pred_count <- 0
  
  for (h in 0:23) {
    model_name <- paste0("hour_", h)
    if (!(model_name %in% names(models_list))) next
    
    hour_data <- test_data %>% filter(hour_of_day == h)
    if (nrow(hour_data) == 0) next
    
    # Make predictions
    preds <- tryCatch({
      predict(models_list[[model_name]], newdata = hour_data)
    }, error = function(e) {
      message(paste("Prediction failed for hour", h, ":", e$message))
      rep(NA, nrow(hour_data))
    })
    
    # Calculate metrics
    actual <- hour_data[[target_col]]
    valid_idx <- !is.na(preds) & !is.na(actual)
    
    if (sum(valid_idx) > 0) {
      mse_val <- mse(actual[valid_idx], preds[valid_idx])
      mae_val <- mae(actual[valid_idx], preds[valid_idx])
      r2_val <- 1 - sum((actual[valid_idx] - preds[valid_idx])^2) /
        sum((actual[valid_idx] - mean(actual[valid_idx]))^2)
      
      # Store results
      hourly_results <- rbind(hourly_results, data.frame(
        hour = h,
        mse = mse_val,
        mae = mae_val,
        r2 = r2_val,
        n = sum(valid_idx)
      ))
      
      # Aggregate for overall metrics
      start_idx <- pred_count + 1
      end_idx <- pred_count + length(preds)
      all_preds[start_idx:end_idx] <- preds
      all_actuals[start_idx:end_idx] <- actual
      pred_count <- pred_count + length(preds)
    }
  }
  
  # Calculate overall metrics
  valid_overall <- !is.na(all_preds) & !is.na(all_actuals)
  overall_results <- data.frame(
    metric = c("mse", "mae", "r2"),
    value = c(
      mse(all_actuals[valid_overall], all_preds[valid_overall]),
      mae(all_actuals[valid_overall], all_preds[valid_overall]),
      1 - sum((all_actuals[valid_overall] - all_preds[valid_overall])^2) /
        sum((all_actuals[valid_overall] - mean(all_actuals[valid_overall]))^2)
    )
  )
  
  return(list(
    hourly_results = hourly_results,
    overall_results = overall_results,
    predictions = data.frame(
      datetime = test_data[[datetime_col]],
      actual = all_actuals,
      predicted = all_preds
    )
  ))
}

# ================== Rolling Window Evaluation ==================
all_window_results <- list()
for (i in seq_along(all_models_rolling)) {
  window_name <- names(all_models_rolling)[i]
  cat("\nEvaluating", window_name, "...\n")
  
  eval_result <- evaluate_model_performance(
    models_list = all_models_rolling[[i]],
    test_data = all_data_rolling[[i]]
  )
  
  all_window_results[[window_name]] <- eval_result
  
  # Print summary
  cat("Overall performance for", window_name, ":\n")
  print(eval_result$overall_results)
}

# ================== Performance Visualization ==================
library(ggplot2)
library(tidyr)

# 1. Plot rolling window metrics over time
window_metrics <- do.call(rbind, lapply(names(all_window_results), function(wn) {
  data.frame(
    window = as.numeric(gsub("rolling_window_", "", wn)),
    mse = all_window_results[[wn]]$overall_results$value[1],
    mae = all_window_results[[wn]]$overall_results$value[2],
    r2 = all_window_results[[wn]]$overall_results$value[3]
  )
}))

ggplot(window_metrics, aes(x = window)) +
  geom_line(aes(y = mse, color = "MSE"), size = 1) +
  geom_line(aes(y = mae, color = "MAE"), size = 1) +
  labs(title = "Rolling Window Performance Metrics",
       x = "Window Number",
       y = "Metric Value",
       color = "Metric") +
  scale_color_manual(values = c("MSE" = "red", "MAE" = "blue")) +
  theme_minimal()

ggplot(window_metrics, aes(x = window, y = r2)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(title = "Rolling Window R-squared",
       x = "Window Number",
       y = "R²") +
  ylim(0, 1) +
  theme_minimal()

# 2. Plot hourly performance heatmap
hourly_perf <- do.call(rbind, lapply(names(all_window_results), function(wn) {
  cbind(
    window = as.numeric(gsub("rolling_window_", "", wn)),
    all_window_results[[wn]]$hourly_results
  )
}))

ggplot(hourly_perf, aes(x = hour, y = window, fill = mse)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Hourly MSE Across Windows",
       x = "Hour of Day",
       y = "Window Number",
       fill = "MSE") +
  theme_minimal()

# ================== Generate Final Report ==================
generate_performance_report <- function(all_results) {
  overall_df <- do.call(rbind, lapply(names(all_results), function(wn) {
    data.frame(
      Window = wn,
      MSE = all_results[[wn]]$overall_results$value[1],
      MAE = all_results[[wn]]$overall_results$value[2],
      R2 = all_results[[wn]]$overall_results$value[3]
    )
  }))
  
  cat("=== Rolling Window Performance Summary ===\n")
  print(overall_df)
  
  cat("\n=== Average Metrics Across All Windows ===\n")
  print(colMeans(overall_df[, -1], na.rm = TRUE))
  
  cat("\n=== Best Performing Window ===\n")
  best_window <- overall_df[which.max(overall_df$R2), ]
  print(best_window)
  
  return(overall_df)
}

# Generate and save report
performance_report <- generate_performance_report(all_window_results)
write.csv(performance_report, "D:/Personal/study/CS/PythonSeminar/github/DS3/d/rolling_window_performance_report.csv", row.names = FALSE)

# ================== Diagnostic Analysis ==================
# Combine all predictions
all_predictions <- do.call(rbind, lapply(all_window_results, function(x) x$predictions))

# Plot actual vs predicted for all windows
ggplot(all_predictions, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Values (All Windows)",
       x = "Actual Price (standardized)",
       y = "Predicted Price (standardized)") +
  theme_minimal()

# Residual analysis
all_predictions$residuals <- all_predictions$actual - all_predictions$predicted
ggplot(all_predictions, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Analysis",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()

# Save all evaluation results
saveRDS(all_window_results, "all_window_evaluation_results.rds")
