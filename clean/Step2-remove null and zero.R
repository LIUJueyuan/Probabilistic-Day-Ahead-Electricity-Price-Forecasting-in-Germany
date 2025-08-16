library(dplyr)
library(lubridate)
library(zoo)

df <- read.csv("entsoe_de_cleanstep1.csv") %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%   # Ensure it is POSIXct
  arrange(DateTime) %>%
  distinct(DateTime, .keep_all = TRUE)       # Remove duplicates first! Core patch

# Generate complete hourly sequence
complete_time <- seq(min(df$DateTime), max(df$DateTime), by = "hour")

# Left join to attach the complete sequence
df_full <- tibble(DateTime = complete_time) %>%
  left_join(df, by = "DateTime") %>%
  arrange(DateTime)

# Interpolate and forward/backward fill
num_cols <- names(df_full)[sapply(df_full, is.numeric)]
for (col in num_cols) {
  # rule = 2 => copy endpoints, ensure length remains unchanged
  df_full[[col]] <- na.approx(df_full[[col]],
                              x = as.numeric(df_full$DateTime),
                              na.rm = FALSE,
                              rule = 2)
  
  df_full[[col]] <- na.locf(df_full[[col]], na.rm = FALSE)
  df_full[[col]] <- na.locf(df_full[[col]], fromLast = TRUE, na.rm = FALSE)
}


stopifnot(
  nrow(df_full) == length(complete_time),        # Ensure row count matches
  all(colSums(is.na(df_full[num_cols])) == 0)    # Ensure no NA values
)

# Output the result
write.csv(df_full, "entsoe_de_cleanstep2.csv", row.names = FALSE)
