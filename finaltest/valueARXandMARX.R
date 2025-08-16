# Load the dataset from a CSV file
test_with_lags <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/finaltest/test_qra_predictions.csv")

# Load the dplyr library for data manipulation
library(dplyr)

# Function to evaluate predictions
# df: Data frame containing the data
# pred_col: Column name of the predictions
# actual_col: Column name of the actual values, default is "Price_winsorized_z"
evaluate_preds <- function(df, pred_col, actual_col = "Price_winsorized_z") {
  pred <- df[[pred_col]] # Extract predictions
  actual <- df[[actual_col]] # Extract actual values
  
  # Calculate evaluation metrics
  mse <- mean((actual - pred)^2, na.rm = TRUE) # Mean Squared Error
  mae <- mean(abs(actual - pred), na.rm = TRUE) # Mean Absolute Error
  r2 <- 1 - sum((actual - pred)^2, na.rm = TRUE) / sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE) # R-squared
  
  # Ljung-Box test for residual autocorrelation
  lb_test <- Box.test(actual - pred, lag = 10, type = "Ljung-Box")
  
  # Return evaluation metrics and test results
  list(
    mse = mse,
    mae = mae,
    r2 = r2,
    ljung_box = lb_test
  )
}

# Evaluate ARX predictions
arx_eval <- evaluate_preds(test_with_lags, "arx_pred")
cat("ARX Model Performance:\n")
cat(sprintf("MSE: %.6f\n", arx_eval$mse))
cat(sprintf("MAE: %.6f\n", arx_eval$mae))
cat(sprintf("R2: %.4f\n", arx_eval$r2))
cat("Residual Autocorrelation Test:\n")
print(arx_eval$ljung_box)

# Evaluate MARX predictions
marx_eval <- evaluate_preds(test_with_lags, "marx_pred")
cat("\nMARX Model Performance:\n")
cat(sprintf("MSE: %.6f\n", marx_eval$mse))
cat(sprintf("MAE: %.6f\n", marx_eval$mae))
cat(sprintf("R2: %.4f\n", marx_eval$r2))
cat("Residual Autocorrelation Test:\n")
print(marx_eval$ljung_box)
