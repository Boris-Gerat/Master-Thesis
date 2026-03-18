library(readxl)
library(RPostgres)
library(DBI)
library(zoo)
library(dplyr)
library(xts)
library(pcaMethods)
library(ggplot2)
library(tseries)   
library(lubridate)

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
fred_qd_time <- fred_qd_raw %>%
  mutate(sasdate = as.Date(sasdate)) %>%
  mutate(sasdate = floor_date(sasdate, "quarter")) %>%
  filter(sasdate >= as.Date("1997-01-01"),
         sasdate <= as.Date("2025-04-01"))

head(fred_qd_time)

##########
########## Custom time series index
##########

excel_data <- read_excel(path = "/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="my_index_new")
excel_data$time <- as.yearqtr(excel_data$time, format = "%Y Q%q")

excel_data_time <- excel_data %>% filter(
					 time >= as.yearqtr("1997 Q1"),
					 time <= as.yearqtr("2025 Q2"))



excel_data_time  <- excel_data_time %>% mutate(time = as.Date(time))
colnames(excel_data_time)

credit_stress <- c( 
"Moodys_BAA_spread", 
"ICE_BoA_HighY_Spread", 
"ICE_BoA_Corpo_Spread")

credit_stress_factor <- pca_risk_index(
  df                 = excel_data_time,
  vars               = credit_stress,
  date_col           = "time",
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
  factor_name        = "Credit_Stress_Factor"
)

print(credit_stress_factor$convergence)
print(credit_stress_factor$explained)
print(credit_stress_factor$loadings)
print(credit_stress_factor$diff_orders)
print(head(credit_stress_factor$scores))

funding_stess <- c("Commertial_paper_spread", 	
		   "Prime_rate_spread_FFE", 
		   "Commercail_paper_spread_Treasury3month")

funding_stress_factor <- pca_risk_index(
  df                 = excel_data_time,
  vars               = funding_stess,
  date_col           = "time",
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
  factor_name        = "Funding Stress Factor"
)

print(funding_stress_factor$convergence)
print(funding_stress_factor$explained)
print(funding_stress_factor$loadings)
print(funding_stress_factor$diff_orders)
print(head(funding_stress_factor$scores))

policy_stress <- c("T10Y2Y",
		  "T10Y3M", 
		  "T10YFF"
)

policy_stress_factor <- pca_risk_index(
  df                 = excel_data_time,
  vars               = policy_stress,
  date_col           = "time",
  n_factors          = 1,
  method             = "ppca",
  center             = TRUE,
  scale              = TRUE,
  impute_pre         = FALSE,
  stationarity_check = TRUE,
  adf_pval           = 0.05,
  max_diff           = 2L,
  diff_pad           = "zero",
  max_iter           = 1000,
  conv_threshold     = 1e-5,
  seed               = 42,
  flip_sign          = TRUE,
  z_score            = TRUE,
  plot_factor        = TRUE,
  factor_name        = "Policy Stress Factor"
)

print(policy_stress_factor$convergence)
print(policy_stress_factor$explained)
print(policy_stress_factor$loadings)
print(policy_stress_factor$diff_orders)
print(head(policy_stress_factor$scores))

market_stress <- c("NASDAQ", 
		  "SP500", 
		  "Rusell"
)

excel_data_time <- excel_data_time %>% 
	mutate(across(all_of(market_stress), ~ as.numeric(as.character(.))))


excel_data_time <- excel_data_time %>% 
	mutate(
	       NASDAQ = c(NA, diff(log(NASDAQ))),
		SP500 = c(NA, diff(log(SP500))),
	       Rusell = c(NA, diff(log(Rusell)))
	       )  %>% 
	filter(!is.na(NASDAQ))

market_stress_factor <- pca_risk_index(
  df                 = excel_data_time,
  vars               = market_stress,
  date_col           = "time",
  n_factors          = 1,
  method             = "ppca",
  center             = TRUE,
  scale              = TRUE,
  impute_pre         = FALSE,
  stationarity_check = FALSE,
  adf_pval           = 0.05,
  max_diff           = 2L,
  diff_pad           = "zero",
  max_iter           = 1000,
  conv_threshold     = 1e-5,
  seed               = 42,
  flip_sign          = FALSE,
  z_score            = TRUE,
  plot_factor        = TRUE,
  factor_name        = "Market Stress Factor"
)

print(market_stress_factor$convergence)
print(market_stress_factor$explained)
print(market_stress_factor$loadings)
print(market_stress_factor$diff_orders)
print(head(market_stress_factor$scores))

##########
########## Full Risk index construction 
##########

df_risk_index_parts <- credit_stress_factor$scores %>% 
  rename(CREDIT_STRESS = PC1) %>%
  left_join(policy_stress_factor$scores %>% select(time, PC1) %>% rename(POLICY_STRESS = PC1), by = "time") %>%
  left_join(funding_stress_factor$scores %>% select(time, PC1) %>% rename(FUNDING_STRESS = PC1), by = "time") %>%
  left_join(market_stress_factor$scores %>% select(time, PC1) %>% rename(MARKET_STRESS = PC1), by = "time")

df_risk_index_parts_FRED <- credit_stress_factor$scores %>% 
  rename(CREDIT_STRESS = PC1) %>%
  left_join(policy_stress_factor$scores %>% select(time, PC1) %>% rename(POLICY_STRESS = PC1), by = "time") %>%
  left_join(funding_stress_factor$scores %>% select(time, PC1) %>% rename(FUNDING_STRESS = PC1), by = "time") %>%
  left_join(market_stress_factor$scores %>% select(time, PC1) %>% rename(MARKET_STRESS = PC1), by = "time")  %>% 
  left_join(fred_qd_time  %>% select(sasdate, PC1)  %>% rename(FRED_1 = PC1) ,by =c("time" = "sasdate"))
tail(df_risk_index_parts_FRED)

df_risk_index_parts <- df_risk_index_parts %>%
mutate(across(c(CREDIT_STRESS, POLICY_STRESS, FUNDING_STRESS, MARKET_STRESS), 
	~ pmin(pmax(., -3), 3)))

df_risk_index_parts_FRED <- df_risk_index_parts_FRED %>%
mutate(across(c(CREDIT_STRESS, POLICY_STRESS, FUNDING_STRESS, MARKET_STRESS), 
	~ pmin(pmax(., -3), 3)))


 custom_risk_index <- pca_risk_index(
  df                 = df_risk_index_parts,
  vars               = ,
  date_col           = "time",
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
  flip_sign          = FALSE,
  z_score            = TRUE,
  plot_factor        = TRUE,
  factor_name        = "Custom Risk Index"
)

print(custom_risk_index$convergence)
print(custom_risk_index$explained)
print(custom_risk_index$loadings)
print(custom_risk_index$diff_orders)
print(head(custom_risk_index$scores))


##########
########## Mixed Custom Sentiment Index
##########

mixed_inputs <- custom_risk_index$scores  %>% rename(Classic_Index = PC1)  %>% 
	left_join(fit_finbert$scores  %>% select(PC1,Date)  %>% rename(SENTIMENT_INDEX_FINBERT = PC1),
		  by= c("time" = "Date") ) 

tail(mixed_inputs)

 custom_mix_index <- pca_risk_index(
  df                 = mixed_inputs,
  vars               = ,
  date_col           = "time",
  n_factors          = 1,
  method             = "ppca",
  center             = TRUE,
  scale              = TRUE,
  impute_pre         = FALSE,
  stationarity_check = FALSE,
  adf_pval           = 0.05,
  max_diff           = 2L,
  diff_pad           = "na",
  max_iter           = 1000,
  conv_threshold     = 1e-5,
  seed               = 42,
  flip_sign          = FALSE,
  z_score            = TRUE,
  plot_factor        = TRUE,
  factor_name        = "Custom Mixed Risk Index"
)

print(custom_mix_index$convergence)
print(custom_mix_index$explained)
print(custom_mix_index$loadings)
print(custom_mix_index$diff_orders)
print(head(custom_mix_index$scores))

##########
########## Sector Factors
##########



