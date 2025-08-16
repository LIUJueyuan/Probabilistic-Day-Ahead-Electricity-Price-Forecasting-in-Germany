library(quantreg)
library(dplyr)
library(ggplot2)

# Define paths
root1 <- "D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre/"
root2 <- "D:/Personal/study/CS/PythonSeminar/github/DS3/Qrapre2/"
model_save_path <- "D:/Personal/study/CS/PythonSeminar/github/DS3/qra_models/"
dir.create(model_save_path, showWarnings = FALSE)

# Identify common windows for ARX and MARX predictions
window_ids1 <- list.files(root1, pattern = "_ARXpreds\\.rds$") %>% gsub("_ARXpreds\\.rds$", "", .)
window_ids2 <- list.files(root2, pattern = "_MARXpreds\\.rds$") %>% gsub("_MARXpreds\\.rds$", "", .)
common_windows <- intersect(window_ids1, window_ids2)

# Define quantiles
quantiles <- seq(0.01, 0.99, by = 0.01)

# Initialize list to store predictions
all_qra_preds <- list()

# Process each window
for (win in common_windows) {
  # Load ARX and MARX predictions
  arx <- readRDS(paste0(root1, win, "_ARXpreds.rds"))
  marx <- readRDS(paste0(root2, win, "_MARXpreds.rds"))
  
  # Merge predictions
  df <- inner_join(arx, marx, by = "DateTime", suffix = c("_arx", "_marx")) %>%
    select(DateTime, Price_winsorized_z_arx, predicted_arx, predicted_marx) %>%
    rename(y = Price_winsorized_z_arx, arx = predicted_arx, marx = predicted_marx)
  
  # Train QRA models for each quantile
  qra_models <- list()
  for (q in quantiles) {
    qra_models[[paste0("q", q * 100)]] <- rq(y ~ arx + marx, data = df, tau = q)
  }
  
  # Save QRA models
  saveRDS(qra_models, paste0(model_save_path, win, "_QRA_models.rds"))
  
  # Make predictions using the trained models
  df_qra <- df %>% select(DateTime, arx, marx)
  for (q in quantiles) {
    coefs <- coef(qra_models[[paste0("q", q * 100)]])
    df_qra[[paste0("q", q * 100)]] <- coefs[1] + coefs["arx"] * df_qra$arx + coefs["marx"] * df_qra$marx
  }
  
  # Store true values and predictions
  df_qra$true_value <- df$y
  df_qra$window_id <- win
  all_qra_preds[[win]] <- df_qra
}

# Combine predictions from all windows
qra_result <- bind_rows(all_qra_preds)
saveRDS(qra_result, "D:/Personal/study/CS/PythonSeminar/github/DS3/qra/all_qra_predictions.rds")

# Define Pinball Loss function
pinball_loss <- function(y_true, y_pred, tau) {
  diff <- y_true - y_pred
  mean(ifelse(diff > 0, tau * diff, (tau - 1) * diff), na.rm = TRUE)
}

# Calculate Pinball Loss for the 90th quantile
pinball_90 <- pinball_loss(qra_result$true_value, qra_result$q90, 0.9)
print(pinball_90)

# Calculate Pinball Loss for all quantiles
results <- data.frame(
  quantile = quantiles,
  pinball = sapply(quantiles, function(tau) {
    q_col <- paste0("q", tau * 100)
    if (!q_col %in% names(qra_result)) return(NA)
    pinball_loss(qra_result$true_value, qra_result[[q_col]], tau)
  })
)

# Plot Pinball Loss across quantiles
ggplot(results, aes(x = quantile, y = pinball)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(title = "QRA Pinball Loss Across Quantiles",
       x = "Quantile (Tau)",
       y = "Pinball Loss") +
  theme_minimal()

# Define Winkler Score function
winkler_score <- function(y, L, U, alpha) {
  valid <- !(is.na(y) | is.na(L) | is.na(U))
  y <- y[valid]
  L <- L[valid]
  U <- U[valid]
  
  score <- numeric(length(y))
  inside <- (y >= L) & (y <= U)
  
  score[inside] <- U[inside] - L[inside]
  score[!inside] <- (U[!inside] - L[!inside]) +
    (2 / alpha) * pmax(L[!inside] - y[!inside], 0) +
    (2 / alpha) * pmax(y[!inside] - U[!inside], 0)
  
  mean(score)
}

# Calculate Winkler Score for 95% prediction intervals
alpha <- 0.05
L <- qra_result$q5
U <- qra_result$q95
y <- qra_result$true_value
winkler_95 <- winkler_score(y, L, U, alpha)
print(winkler_95)

# Calculate Coverage Rate
coverage_rate <- function(y, L, U) {
  mean((y >= L) & (y <= U), na.rm = TRUE)
}

coverage_95 <- coverage_rate(y, L, U)
print(coverage_95)

# Compute Probability Integral Transform (PIT)
compute_pit <- function(y, qra_df, quantiles = seq(0.01, 0.99, 0.01)) {
  pits <- numeric(length(y))
  
  for (i in seq_along(y)) {
    y_i <- y[i]
    
    if (is.na(y_i)) {
      pits[i] <- NA
      next
    }
    
    q_vals <- sapply(quantiles, function(tau) qra_df[[paste0("q", tau * 100)]][i])
    
    if (all(is.na(q_vals))) {
      pits[i] <- NA
      next
    }
    
    valid_idx <- which(!is.na(q_vals))
    q_vals <- q_vals[valid_idx]
    valid_quantiles <- quantiles[valid_idx]
    
    if (length(q_vals) == 0) {
      pits[i] <- NA
      next
    }
    
    if (y_i <= q_vals[1]) {
      pits[i] <- valid_quantiles[1]
    } else if (y_i >= q_vals[length(q_vals)]) {
      pits[i] <- valid_quantiles[length(valid_quantiles)]
    } else {
      idx <- which(q_vals >= y_i)[1]
      pits[i] <- approx(x = q_vals[c(idx-1, idx)], y = valid_quantiles[c(idx-1, idx)], xout = y_i)$y
    }
  }
  pits
}

pits <- compute_pit(qra_result$true_value, qra_result)

# Plot PIT Histogram
ggplot(data.frame(pits = pits), aes(x = pits)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black") +
  labs(title = "PIT Histogram", x = "PIT values", y = "Frequency") +
  theme_minimal()

# Compute Reliability
compute_reliability <- function(y, qra_df, quantiles = seq(0.01, 0.99, 0.01)) {
  results <- data.frame()
  
  for (tau in quantiles) {
    q_col <- paste0("q", tau * 100)
    covered <- mean(y <= qra_df[[q_col]], na.rm = TRUE)
    results <- rbind(results, data.frame(tau = tau, coverage = covered))
  }
  results
}

rel_data <- compute_reliability(qra_result$true_value, qra_result)

# Plot Reliability Diagram
ggplot(rel_data, aes(x = tau, y = coverage)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Reliability Diagram", x = "Nominal Quantile (Tau)", y = "Empirical Coverage") +
  theme_minimal()
