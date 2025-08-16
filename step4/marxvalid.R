library(dplyr)
library(lubridate)
library(RcppRoll)
library(moments)

train_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/2train_df_Abnormalvalue.csv")
valid_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/2valid/valid_df_winsorized.csv")

# Process valid_df datetime
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

# Process train_df datetime
train_df$DateTime <- as.character(train_df$DateTime)
train_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", train_df$DateTime)] <-
  paste0(train_df$DateTime[!grepl("\\d{2}:\\d{2}:\\d{2}", train_df$DateTime)], " 00:00:00")
train_df$DateTime <- ymd_hms(train_df$DateTime, tz = "UTC")
train_df$DateTime <- with_tz(train_df$DateTime, tzone = "Europe/Berlin")
train_df <- train_df |>
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

check_hourly_continuity(valid_df)
check_hourly_continuity(train_df)

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

valid_df <- fill_and_interpolate(valid_df, datetime_col = "DateTime", value_col = "Price")
train_df <- fill_and_interpolate(train_df, datetime_col = "DateTime", value_col = "Price")

check_hourly_continuity(valid_df)
check_hourly_continuity(train_df)

window_size_days <- 180    # Use a 180-day window
step_size_days <- 7        # Roll every 7 days
start_date <- min(train_df$DateTime) + days(window_size_days)
end_date <- max(train_df$DateTime)

# Generate rolling prediction date sequence
dates_to_predict <- seq(from = start_date, to = end_date, by = "7 days")

# ================== Construct Lag and Standardization Function ==================
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
  
  result_df <- full_df %>%
    slice((nrow(full_df) - nrow(train_df) + 1):nrow(full_df))
  
  return(result_df)
}

processed_valid <- construct_lags(train_df = valid_df, beforetrain = train_df)

processed_valid %>%
  summarise(
    mean_Pw_z = mean(Price_winsorized_z, na.rm = TRUE),
    sd_Pw_z = sd(Price_winsorized_z, na.rm = TRUE)
  )

summary(processed_valid$Lag_1d_Pw)
summary(processed_valid$Lag_168d_Load)

write.csv(processed_valid, "D:/Personal/study/CS/PythonSeminar/github/DS3/d/2valid/valid_processed.csv")

# Visualization
library(ggplot2)
ggplot(processed_valid, aes(x = Lag_1d_Pw)) + geom_histogram(bins = 50)
ggplot(processed_valid, aes(x = Lag_1d_Pw)) +
  geom_density(fill = "skyblue", alpha = 0.4) +
  geom_vline(aes(xintercept = mean(Lag_1d_Pw, na.rm = TRUE)),
             color = "red", linetype = "dashed") +
  labs(title = "Lag_1d_Pw Density", x = "Lag_1d_Pw", y = "Density")

processed_valid <- processed_valid %>%
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

processed_valid <- processed_valid %>%
  mutate(
    load_is_peak = ifelse(hour_of_day %in% c(7:9, 18:20), 1, 0)
  )

WINDOW_SIZE_DAYS <- 180    # Rolling window size in days
STEP_SIZE_DAYS <- 7        # Rolling step size in days
MAX_ITERATIONS <- 10       # Maximum number of rolling windows
start_date <- min(processed_valid$DateTime)

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
    
    message("Hour ", h, " -> Target NA:", sum(is.na(hour_data[[target_col]])), " | Pred NA:", sum(is.na(preds)))
    
    fit_results <- rbind(fit_results, data.frame(hour = h, mse = mse_val, n = nrow(hour_data)))
  }
  
  return(fit_results)
}

all_models_rolling <- list()
all_data_rolling <- list()
for (i in 0:(MAX_ITERATIONS - 1)) {
  current_start_date <- start_date + days(i * STEP_SIZE_DAYS)
  valid_window_slice <- get_train_window(processed_valid, current_start_date, WINDOW_SIZE_DAYS)
  
  if (nrow(valid_window_slice) < 100) {
    message(paste("Rolling window", i + 1, "insufficient data, skipping"))
    next
  }
  
  prep_res_slice <- rolling_window_data_prep(
    beforetrain = train_df,
    train_window = processed_valid,
    datetime_col = "DateTime",
    target_col = "Price_winsorized"
  )
  
  valid_ready_slice <- prep_res_slice$data
  
  valid_ready_slice <- valid_ready_slice %>%
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
      load_is_peak = ifelse(hour_of_day %in% c(7:9, 18:20), 1, 0)
    )
  
  # Train models
  models_slice <- train_hourly_marx_models(valid_ready_slice, hour_feature_map = hour_feature_map)
  
  all_models_rolling[[paste0("rolling_window_", i + 1)]] <- models_slice
  all_data_rolling[[paste0("rolling_window_", i + 1)]] <- valid_ready_slice
  message(paste("Rolling window", i + 1, "model training completed"))
  
  window_name <- paste0("rolling_window_", i + 1)
  
  # Save models
  saveRDS(models_slice, file = paste0("D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre2/", window_name, "_MARXmodels.rds"))
  
  # Save processed data
  saveRDS(valid_ready_slice, file = paste0("D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre2/", window_name, "_MARXdata.rds"))
  
  preds_slice <- collect_rolling_predictions_by_window(
    list(window_name = models_slice),
    list(window_name = valid_ready_slice)
  )[[1]]
  
  saveRDS(preds_slice, file = paste0("D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre2/", window_name, "_MARXpreds.rds"))
}

collect_rolling_predictions_by_window <- function(all_models_rolling, all_data_rolling,
                                                  datetime_col = "DateTime",
                                                  target_col = "Price_winsorized_z") {
  # Final list, each element is a window
  all_preds_list <- list()
  
  for (win_name in names(all_models_rolling)) {
    models_list <- all_models_rolling[[win_name]]
    train_data <- all_data_rolling[[win_name]]
    
    train_data <- train_data %>%
      mutate(hour_of_day = lubridate::hour(.data[[datetime_col]]))
    
    window_preds <- data.frame()
    
    for (h in 0:23) {
      model_name <- paste0("hour_", h)
      if (!model_name %in% names(models_list)) next
      
      model <- models_list[[model_name]]
      hour_data <- train_data %>% filter(hour_of_day == h)
      if (nrow(hour_data) == 0) next
      
      preds <- predict(model, newdata = hour_data)
      
      df_preds <- hour_data %>%
        select(all_of(datetime_col), all_of(target_col)) %>%
        mutate(
          hour = h,
          predicted = preds,
          window_id = win_name
        )
      
      window_preds <- bind_rows(window_preds, df_preds)
    }
    
    # Add single window data to list
    all_preds_list[[win_name]] <- window_preds
  }
  
  return(all_preds_list)
}

# Evaluate
library(dplyr)
library(Metrics)
evaluate_rolling_fit <- function(all_models_rolling, all_data_rolling, datetime_col = "DateTime", target_col = "Price_winsorized_z") {
  results <- data.frame()
  
  for (win_name in names(all_models_rolling)) {
    models_list <- all_models_rolling[[win_name]]
    train_data <- all_data_rolling[[win_name]]
    
    train_data <- train_data %>% mutate(hour_of_day = lubridate::hour(.data[[datetime_col]]))
    
    for (h in 0:23) {
      model_name <- paste0("hour_", h)
      if (!model_name %in% names(models_list)) next
      
      model <- models_list[[model_name]]
      hour_data <- train_data %>% filter(hour_of_day == h)
      if (nrow(hour_data) == 0) next
      
      preds <- predict(model, newdata = hour_data)
      mse_val <- mean((hour_data[[target_col]] - preds)^2, na.rm = TRUE)
      
      results <- rbind(results, data.frame(
        rolling_window = win_name,
        hour = h,
        mse = mse_val,
        n_samples = nrow(hour_data)
      ))
    }
  }
  
  return(results)
}

fit_results <- evaluate_rolling_fit(all_models_rolling, all_data_rolling)
print(fit_results)

collect_rolling_predictions <- function(all_models_rolling, all_data_rolling, datetime_col = "DateTime", target_col = "Price_winsorized_z") {
  all_preds <- data.frame()
  
  for (win_name in names(all_models_rolling)) {
    models_list <- all_models_rolling[[win_name]]
    train_data <- all_data_rolling[[win_name]]
    train_data <- train_data %>% mutate(hour_of_day = lubridate::hour(.data[[datetime_col]]))
    
    for (h in 0:23) {
      model_name <- paste0("hour_", h)
      if (!model_name %in% names(models_list)) next
      
      model <- models_list[[model_name]]
      hour_data <- train_data %>% filter(hour_of_day == h)
      if (nrow(hour_data) == 0) next
      
      preds <- predict(model, newdata = hour_data)
      df_preds <- hour_data %>%
        select(all_of(datetime_col), all_of(target_col)) %>%
        mutate(
          rolling_window = win_name,
          predicted = preds,
          DateTime = format(as.POSIXct(.data[[datetime_col]], format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"), "%Y-%m-%d %H:%M:%S")
        )
      all_preds <- rbind(all_preds, df_preds)
    }
  }
  
  return(all_preds)
}

all_predictions <- collect_rolling_predictions(all_models_rolling, all_data_rolling)

# View partial results
head(all_predictions)

library(ggplot2)
library(dplyr)

# Assuming fit_results data frame format:
# fit_results <- data.frame(rolling_window = factor(...), mse = numeric())

library(ggplot2)
ggplot(fit_results, aes(x = as.factor(rolling_window), y = mse, group = 1)) +
  geom_line(color = "dodgerblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "📈 Rolling Window MSE Trend",
    x = "Rolling Window Number",
    y = "MSE"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate error for each prediction
all_predictions <- all_predictions %>%
  mutate(
    error = predicted - Price_winsorized_z,
    abs_error = abs(error),
    hour_of_day = lubridate::hour(DateTime)
  )

# Box plot
ggplot(all_predictions, aes(x = factor(hour_of_day), y = abs_error)) +
  geom_boxplot(fill = "skyblue", outlier.size = 1) +
  labs(title = "Error Distribution by Hour",
       x = "Hour of Day",
       y = "Absolute Error") +
  theme_minimal()

# Violin plot (optional)
ggplot(all_predictions, aes(x = factor(hour_of_day), y = abs_error)) +
  geom_violin(fill = "lightgreen", alpha = 0.6) +
  labs(title = "Error Distribution by Hour (Violin Plot)",
       x = "Hour of Day",
       y = "Absolute Error") +
  theme_minimal()

write.csv(all_predictions, "D:/Personal/study/CS/PythonSeminar/github/DS3/step4/marxpredicted.csv", row.names = FALSE)
