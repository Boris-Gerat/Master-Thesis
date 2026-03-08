library(readxl)
library(RPostgres)
library(DBI)
library(zoo)
library(dplyr)
library(xts)
library(pcaMethods)
library(ggplot2)

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

fit <- pca_risk_index(
  df             = df_use,
  vars           = sentiment_vars,
  date_col       = "Date",
  n_factors      = 1,
  method         = "ppca",
  center         = TRUE,
  scale          = TRUE,
  impute_pre     = FALSE,
  max_iter       = 1000,
  conv_threshold = 1e-5,
  seed           = 42,
  flip_sign      = TRUE,
  z_score        = TRUE,
  plot_factor    = TRUE,
  factor_name    = "Sentiment_VADER"
)

print(fit$convergence)
print(fit$explained)
print(fit$loadings)
print(head(fit$scores))

df_with_index <- df_use %>%
  bind_cols(
    fit$scores %>% select(PC1) %>% rename(SENTIMENT_INDEX = PC1)
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

fit <- pca_risk_index(
  df             = df_use,
  vars           = sentiment_vars_FINBERT,
  date_col       = "Date",
  n_factors      = 1,
  method         = "ppca",
  center         = TRUE,
  scale          = TRUE,
  impute_pre     = FALSE,
  max_iter       = 1000,
  conv_threshold = 1e-5,
  seed           = 42,
  flip_sign      = TRUE,
  z_score        = TRUE,
  plot_factor    = TRUE,
  factor_name    = "Sentiment_FINBERT"
)

print(fit$convergence)
print(fit$explained)
print(fit$loadings)
print(head(fit$scores))

df_with_index <- df_use %>%
  bind_cols(
    fit$scores %>% select(PC1) %>% rename(SENTIMENT_INDEX = PC1)
  )

##########
########## Custom time series index
##########





