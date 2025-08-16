library(dbx)
install.packages("RMariaDB")
library(RMariaDB)
con <- dbConnect(MariaDB(), 
                 host = "132.252.60.112", 
                 port = 3306,
                 user = "student", 
                 password = "#q6a21I&OA5k", 
                 dbname = "ENTSOE")

dbListTables(con)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)

# Database credentials
db_user <- "student"
db_password <- "#q6a21I&OA5k"

# Establish connection
db <- dbxConnect(
  adapter = "mysql",
  host = "132.252.60.112",
  port = 3306,
  dbname = "ENTSOE",
  user = db_user,
  password = db_password
)

# Function to retrieve data
get_data <- function(x, values, mapcode = FALSE) {
  if (mapcode) {
    specs_new <- x %>%
      dplyr::select(TimeSeriesID, Name, ProductionType, Type, ResolutionCode, MapCode)
    
    values %>%
      filter(TimeSeriesID %in% !!specs_new$TimeSeriesID) %>%
      collect() %>%
      select(TimeSeriesID, DateTime, Value) %>%
      left_join(specs_new, by = "TimeSeriesID") %>%
      mutate_at(vars(Type, ProductionType), str_replace_all, " ", "_") %>%
      mutate_at(vars(MapCode), str_replace_all, "-", "_") %>%
      select(-TimeSeriesID) %>%
      arrange(ProductionType, desc(Type)) %>%
      pivot_wider(names_from = c(MapCode, Name, ProductionType, Type, ResolutionCode),
                  values_from = Value) %>%
      mutate(DateTime = anytime::anytime(DateTime, tz = "UTC", asUTC = TRUE)) %>%
      arrange(DateTime)
  } else {
    specs_new <- x %>%
      dplyr::select(TimeSeriesID, Name, ProductionType, Type, ResolutionCode)
    
    values %>%
      filter(TimeSeriesID %in% !!specs_new$TimeSeriesID) %>%
      collect() %>%
      select(TimeSeriesID, DateTime, Value) %>%
      left_join(specs_new, by = "TimeSeriesID") %>%
      mutate_at(vars(Type, ProductionType), str_replace_all, " ", "_") %>%
      select(-TimeSeriesID) %>%
      arrange(ProductionType, desc(Type)) %>%
      pivot_wider(names_from = c(Name, ProductionType, Type, ResolutionCode),
                  values_from = Value) %>%
      mutate(DateTime = anytime::anytime(DateTime, tz = "UTC", asUTC = TRUE)) %>%
      arrange(DateTime)
  }
}

# Load spec and vals tables
spec <- tbl(db, "spec") %>% collect()
values <- tbl(db, "vals")

# Germany-specific queries
specs_list <- list()

# Day-ahead prices for DE_AT_LU and DE_LU
specs_list[["prices"]] <- spec %>%
  filter(
    Name %in% c("Price"),
    Type %in% c("DayAhead"),
    ResolutionCode == "PT60M",
    MapCode %in% c("DE_AT_LU", "DE_LU")
  )

# Load and generation for Germany
specs_load <- spec %>%
  filter(
    Name == "Load",
    Type %in% c("DayAhead", "Actual"),
    MapCode == "DE"
  )

specs_generation <- spec %>%
  filter(
    Name == "Generation",
    Type %in% c("DayAhead", "Actual"),
    ProductionType %in% c("Wind Onshore", "Wind Offshore", "Solar"),
    MapCode == "DE",
    is.na(Specification) | Specification == "Output"
  )

specs_list[["load_gen"]] <- bind_rows(specs_load, specs_generation)

# Retrieve data
data_de <- purrr::map(specs_list, get_data, values = values, mapcode = TRUE)

# Process load and generation data: aggregate to hourly
data_de[["load_gen"]] <-
  data_de %>%
  purrr::pluck("load_gen") %>%
  mutate(
    DateTime = anytime::anytime(DateTime, tz = "UTC", asUTC = TRUE),
    DateTime = floor_date(DateTime, "hour"),
    across(-DateTime, ~ .x / 1000)
  ) %>%
  group_by(DateTime) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  mutate(across(-DateTime, ~ round(.x, 5)))

# Merge prices with load and generation
entsoe_de <- data_de %>%
  purrr::pluck("prices") %>%
  mutate(
    Price = case_when(
      DateTime < ymd_hms("2018-09-30 22:00:00") ~ DE_AT_LU_Price_NA_DayAhead_PT60M,
      DateTime >= ymd_hms("2018-09-30 22:00:00") ~ DE_LU_Price_NA_DayAhead_PT60M
    )
  ) %>%
  select(DateTime, Price) %>%
  full_join(data_de[["load_gen"]], by = "DateTime") %>%
  select(
    DateTime,
    Price,
    "Load_DA" = `DE_Load_NA_DayAhead_PT15M`,
    "Load_Act" = `DE_Load_NA_Actual_PT15M`,
    "Solar_DA" = `DE_Generation_Solar_DayAhead_PT15M`,
    "Solar_Act" = `DE_Generation_Solar_Actual_PT15M`,
    "WindOn_DA" = `DE_Generation_Wind_Offshore_DayAhead_PT15M`,
    "WindOn_Act" = `DE_Generation_Wind_Offshore_Actual_PT15M`,
    "WindOff_DA" = `DE_Generation_Wind_Onshore_DayAhead_PT15M`,
    "WindOff_Act" = `DE_Generation_Wind_Onshore_Actual_PT15M`
  ) %>%
  arrange(DateTime)

# Optional: View or save the final dataset
View(entsoe_de)
save(entsoe_de, file = "D:/Personal/zu/Dortmund/Couse_arrangement/So 25/DSEE/entsoe_DE_only.Rds")
