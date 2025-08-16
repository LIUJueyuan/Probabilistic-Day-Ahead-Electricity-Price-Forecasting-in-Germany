# Probabilistic Day-Ahead Electricity Price Forecasting in Germany
## ✨ Author
- **Jueyuan Liu** – MSc Student,Econometrics in TU Dortmund
## 📌 Project Overview
This project develops and evaluates **probabilistic day-ahead electricity price forecasts** for the German market.  
The study employs three core modeling approaches:
- **ARX** (Autoregressive with Exogenous Variables)  
- **MARX** (Multi-hour Adaptive Regression with Exogenous Variables)  
- **QRA** (Quantile Regression Averaging)  

The goal is to move beyond point forecasts by providing **quantile-based probabilistic forecasts**, enabling robust risk management and decision-making in highly volatile electricity markets.

---

## ⚡ Motivation
Electricity price forecasting is essential for:  
- Market participants → bidding strategies & hedging  
- System operators → grid balancing & reserve planning  
- Policymakers → market design & renewable integration  

Germany’s day-ahead market, with its high share of renewables, requires **probabilistic approaches** to better quantify **uncertainty**.

---

## 🛠 Methodology
- **Data source**: ENTSO-E Transparency Platform (2017–2025)  
- **Preprocessing**:
  - Time format standardization & CET/CEST alignment  
  - Gap filling via interpolation & forward/backward fill  
  - Outlier detection with **hour-specific MAD** & Winsorization  
  - Calendar feature engineering (holidays, weekends, seasonality)  
- **Models**:
  - ARX → captures temporal dependencies with lagged exogenous inputs  
  - MARX → fits 24 hourly models to capture intra-day heterogeneity  
  - QRA → ensembles ARX & MARX predictions into full predictive distributions  
- **Rolling window setup**:  
  - Training: 2019–2021  
  - Validation: 2022  
  - Test: 2023–2025  
  - 180-day rolling windows, 7-day forecasting horizon  

---

## 📊 Evaluation
- **Point forecast metrics**: MSE, MAE, R²  
- **Probabilistic metrics**: Pinball Loss, CRPS, Interval Coverage, Calibration  
- **Key findings**:
  - ARX fits training data well but overfits in validation/test  
  - MARX generalizes better across changing conditions  
  - QRA provides calibrated quantile forecasts but tends to under-estimate uncertainty in mid-quantiles  

---


## 📖 References
- Weron, R. (2014). *Electricity price forecasting: A review of the state-of-the-art.*  
- Nowotarski, J., & Weron, R. (2018). *Probabilistic and interval methods in electricity price forecasting.*  
- Maciejowska, K. et al. (2016). *Factor Quantile Regression Averaging.*  

---


