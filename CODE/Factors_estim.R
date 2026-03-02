library(readxl)
library(RPostgres)
library(DBI)
library(zoo)
library(dplyr)
library(xts)
library(pcaMethods)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "fed_speeches",
  host   = "localhost",
  port   = "5432",
  user   = "borisgerat"
)

stopifnot(dbIsValid(con))

data <- dbReadTable(con, "sentiment_base")

data$Quarter <- as.yearqtr(data$Quarter, format = "%Y Q%q")

data <- data %>%
  filter(
    Quarter >= as.yearqtr("1995 Q1"),
    Quarter <= as.yearqtr("2025 Q4")
  ) %>%
  mutate(Date = as.Date(Quarter)) %>%
  arrange(Date)

vars <- c("gdelt_avgTone","fed_minutes_vader","gov_vader","fed_speech_vader","sec_vader")

df_use <- data %>% arrange(Date)

date_df <- df_use %>% transmute(Quarter, Date)



fit <- pca_risk_index(
  df = df_use,
  vars = c("gdelt_avgTone",
           "fed_minutes_vader",
           "gov_vader",
           "fed_speech_vader",
           "sec_vader"),
  date_col = "Date"
)

fit$explained
fit$loadings
head(fit$scores_ts)  # Quarter/Date + PC1
