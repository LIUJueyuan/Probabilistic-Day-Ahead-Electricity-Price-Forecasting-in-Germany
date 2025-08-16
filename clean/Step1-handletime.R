library(tidyverse)
library(lubridate)

# Load data
df <- read.csv("entsoe_de.csv")

# Check and print non-standard formatted times
non_standard_times <- df$DateTime[!str_detect(df$DateTime, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")]
print(non_standard_times)

# Standardize time format example: assuming time format is "DD/MM/YYYY HH:MM:SS"
# Adjustments need to be made based on the actual non-standard format
df$DateTime <- str_replace_all(df$DateTime, "(\\d{2})/(\\d{2})/(\\d{4})", "\\3-\\2-\\1")

# Convert time column to POSIXct format
df$DateTime <- ymd_hms(df$DateTime)

# Check for any times that failed to parse
failed_rows <- df[is.na(df$DateTime), ]
print(failed_rows)

# Handle entries with unparseable times example: remove these rows
df <- df[!is.na(df$DateTime), ]

# Handle duplicate timestamps example: remove duplicate rows
df <- df[!duplicated(df$DateTime), ]

# Sort by time
df <- df %>% arrange(DateTime) %>% as.data.frame()

# Set time as index
rownames(df) <- df$DateTime

# Save the cleaned data right next to the original file
readr::write_csv(df, "entsoe_de_cleanstep1.csv")


df %>% 
  mutate(hour = hour(DateTime)) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Hourly Observation Counts")

