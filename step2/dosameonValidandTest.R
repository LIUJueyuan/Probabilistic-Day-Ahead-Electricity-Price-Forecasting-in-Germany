
library(dplyr)
valid_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/2valid/time__step1_valid_df.csv")
test_df <- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/3test/time__step1_test_df.csv")
train_df<- read.csv("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/2train_df_Abnormalvalue.csv")
beforetrain_df <-read.csv ("D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/time_beforetrain_df.csv")
hourly_stats <- train_df %>%
  group_by(hour_of_day) %>%
  summarise(
    med = median(Price, na.rm = TRUE),
    mad_val = mad(Price, na.rm = TRUE),
    upper = med + 3 * mad_val,
    lower = med - 3 * mad_val
  )

valid_df_winsorized <- valid_df %>%
  left_join(hourly_stats, by = "hour_of_day") %>%
  mutate(
    Price_winsorized = pmin(pmax(Price, lower), upper)
  )

test_df_winsorized <- test_df %>%
  left_join(hourly_stats, by = "hour_of_day") %>%
  mutate(
    Price_winsorized = pmin(pmax(Price, lower), upper))

before_df_winsorized <- beforetrain_df%>%
  left_join(hourly_stats, by = "hour_of_day") %>%
  mutate(
    Price_winsorized = pmin(pmax(Price, lower), upper))
write.csv(valid_df_winsorized, "D:/Personal/study/CS/PythonSeminar/github/DS3/d/2valid/valid_df_winsorized.csv", row.names = FALSE)
write.csv(test_df_winsorized, "D:/Personal/study/CS/PythonSeminar/github/DS3/d/3test/test_df_winsorized.csv", row.names = FALSE)
write.csv(before_df_winsorized, "D:/Personal/study/CS/PythonSeminar/github/DS3/d/1train/before_df_winsorized.csv", row.names = FALSE)
