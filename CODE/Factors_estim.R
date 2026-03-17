library(readxl)
library(RPostgres)
library(DBI)
library(zoo)
library(dplyr)
library(xts)
library(pcaMethods)
library(ggplot2)
library(tseries)   

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

df_use <- data %>%
  filter(
    Quarter >= as.yearqtr("1995 Q1"),
    Quarter <= as.yearqtr("2025 Q4")
  ) %>%
  mutate(Date = as.Date(Quarter)) %>%
  arrange(Date)

colnames(data)

##########
########## SENTIMENT VADER
##########

sentiment_vars <- c(
  "gdelt_avgTone",
  "fed_minutes_vader",
  "gov_vader",
  "fed_speech_vader",
  "sec_vader"
)

fit_vader <- pca_risk_index(
  df                 = df_use,
  vars               = sentiment_vars,
  date_col           = "Date",
  n_factors          = 1,
  method             = "ppca",
  center             = TRUE,
  scale              = TRUE,
  impute_pre         = FALSE,
  stationarity_check = TRUE,
  adf_pval           = 0.05,
  max_diff           = 2L,
  diff_pad           = "na",
  max_iter           = 1000,
  conv_threshold     = 1e-5,
  seed               = 42,
  flip_sign          = TRUE,
  z_score            = TRUE,
  plot_factor        = TRUE,
  factor_name        = "Sentiment_VADER"
)

print(fit_vader$convergence)
print(fit_vader$explained)
print(fit_vader$loadings)
print(fit_vader$diff_orders)   # shows which vars were differenced
print(head(fit_vader$scores))

df_with_index_vader <- df_use %>%
  bind_cols(
    fit_vader$scores %>% select(PC1) %>% rename(SENTIMENT_INDEX_VADER = PC1)
  )

##########
########## SENTIMENT FINBERT
##########

sentiment_vars_FINBERT <- c(
  "gdelt_avgTone",
  "fed_minutes_finbert",
  "gov_finbert",
  "fed_speech_finbert",
  "sec_finbert"
)

sentiment_vars_FINBERT

fit_finbert <- pca_risk_index(
  df                 = df_use,
  vars               = sentiment_vars_FINBERT,
  date_col           = "Date",
  n_factors          = 1,
  method             = "ppca",
  center             = TRUE,
  scale              = TRUE,
  impute_pre         = FALSE,
  stationarity_check = TRUE,
  adf_pval           = 0.05,
  max_diff           = 2L,
  diff_pad           = "na",
  max_iter           = 1000,
  conv_threshold     = 1e-5,
  seed               = 42,
  flip_sign          = TRUE,
  z_score            = TRUE,
  plot_factor        = TRUE,
  factor_name        = "Sentiment_FINBERT"
)

print(fit_finbert$convergence)
print(fit_finbert$explained)
print(fit_finbert$loadings)
print(fit_finbert$diff_orders)
print(head(fit_finbert$scores))

df_with_index_finbert <- df_use %>%
  bind_cols(
    fit_finbert$scores %>% select(PC1) %>% rename(SENTIMENT_INDEX_FINBERT = PC1)
  )

##########
########## FRED-QD - First factor
##########

fred_qd_raw <- read.csv("/Users/borisgerat/Documents/Projects/SDF_AssetPricing/FRED_QD_1.csv")
fred_qd_raw$sasdate <- as.Date(fred_qd_raw$sasdate)
fred_qd_raw_xts <- xts(fred_qd_raw$PC1, order.by = fred_qd_raw$sasdate)
head(fred_qd_raw_xts)
plot(fred_qd_raw_xts)





##########
########## Custom time series index
##########

excel_data <- read_excel(path = "/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx")
