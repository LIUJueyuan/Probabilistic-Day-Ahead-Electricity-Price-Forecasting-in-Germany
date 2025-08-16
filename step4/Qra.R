library(dplyr)
library(lubridate)
library(RcppRoll)
library(moments)

marx <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/step4/marxpredicted.csv")
arx <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/step4/trymarxonarx.csv")

names(marx)
names(arx)

marx_selected <- marx %>%
  select(DateTime, Price_winsorized_z, predicted) %>%
  rename(marx_pred = predicted)

arx_selected <- arx %>%
  select(DateTime, Price_winsorized_z, predicted) %>%
  rename(arx_pred = predicted)

arx_unique <- arx_selected %>%
  group_by(DateTime) %>%
  slice(1) %>%
  ungroup()

marx_unique <- marx_selected %>%
  group_by(DateTime) %>%
  slice(1) %>%
  ungroup()

merged_df <- marx_unique %>%
  inner_join(arx_unique %>% select(DateTime, arx_pred), by = "DateTime")

library(quantreg)

# Set the quantiles to fit: from 0.01 to 0.99, totaling 99 quantiles
quantiles <- seq(0.01, 0.99, by = 0.01)

# To store regression coefficients for each quantile
qra_models <- list()
qra_coefs <- data.frame()

for (q in quantiles) {
  # Fit quantile regression model
  model <- rq(Price_winsorized_z ~ marx_pred + arx_pred,
              data = merged_df, tau = q)
  
  qra_models[[paste0("q", q * 100)]] <- model
  
  # Store coefficients
  coefs <- coef(model)
  qra_coefs <- rbind(qra_coefs,
                     data.frame(Quantile = q, Intercept = coefs[1],
                                Coef_marx = coefs["marx_pred"],
                                Coef_arx = coefs["arx_pred"]))
}
