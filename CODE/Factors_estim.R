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
library(purrr)

library(corrplot)
library(RColorBrewer)

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
    Quarter >= as.yearqtr("1997 Q1"),
    Quarter <= as.yearqtr("2024 Q4")
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
########## Custom time series index
##########

excel_data <- read_excel(path = "/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="my_index_new")
excel_data$time <- as.yearqtr(excel_data$time, format = "%Y Q%q")

excel_data_time <- excel_data %>% filter(
					 time >= as.yearqtr("1997 Q1"),
					 time <= as.yearqtr("2024 Q4"))



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


df_risk_index_parts <- df_risk_index_parts %>%
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



########## Banking

banking_sector_data <- read_excel("/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="FDIC")

colnames(banking_sector_data)
banking_vars <- c("WholeSaleFunding","LiquidityBuffer", "CapitalCushion", "ComInduLoans")

banking_sector_window <- banking_sector_data %>% mutate(time = as.yearqtr(Quarter)) %>% 
	filter(time >= as.yearqtr("1997 Q1"),
	       time <= as.yearqtr("2024 Q4"))

banking_factor <- pca_risk_index(
df                 = banking_sector_window,
vars               = banking_vars,
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
flip_sign          = FALSE,
z_score            = TRUE,
plot_factor        = TRUE,
factor_name        = "Banking Factor"
)

print(banking_factor$convergence)
print(banking_factor$explained)
print(banking_factor$loadings)
print(banking_factor$diff_orders)
print(head(banking_factor$scores))


########## Shadow Banking

shadow_sector_data <- read_excel("/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="Shadow")

colnames(shadow_sector_data)
shadow_variables <- c("WholeSaleFunding","LiquidityBuffer", "CapitalCushion", "TotalLoans")

shadow_variables_filtered <- shadow_sector_data %>% mutate(time = as.yearqtr(Quarter)) %>% 
	filter(time >= as.yearqtr("1997 Q1"), time <= as.yearqtr("2024 Q4"))


shadow_factor_broad <- pca_risk_index(
df                 = shadow_variables_filtered,
vars               = shadow_variables,
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
flip_sign          = FALSE,
z_score            = TRUE,
plot_factor        = TRUE,
factor_name        = "Shadowbanking Factor Broad"
)

print(shadow_factor_broad$convergence)
print(shadow_factor_broad$explained)
print(shadow_factor_broad$loadings)
print(shadow_factor_broad$diff_orders)
print(head(shadow_factor_broad$scores))



########## Hedge Fund

hedge_data <- read_excel("/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="Hedge")

hedge_vars <- c("WholeSaleFunding","LiquidityBuffer", "CapitalCushion", "TotalLoans")

colnames(hedge_data)
hedge_data_filtered <- hedge_data %>% mutate(time = as.yearqtr(Quarter))  %>% 
	filter(time <= as.yearqtr("2024 Q4"))

hedge_factor <- pca_risk_index(
df                 = hedge_data_filtered, 
vars               = hedge_vars,
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
flip_sign          = FALSE,
z_score            = TRUE,
plot_factor        = TRUE,
factor_name        = "Hedge Funds Factor"
)

print(hedge_factor$convergence)
print(hedge_factor$explained)
print(hedge_factor$loadings)
print(hedge_factor$diff_orders)
print(head(hedge_factor$scores))


##########
########## Risk Index DataFrame
##########

FedRiskIndicies <- read_excel("/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="FED_RISK")

FedRiskIndicies_filtered <- FedRiskIndicies %>% mutate(time = as.yearqtr(Time)) %>% 
	filter(time >= as.yearqtr("1997 Q1"),
	time <= as.yearqtr("2024 Q4"))

Shapiro <- read_excel("/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="Shapiro")

shapiro_quart <- Shapiro %>% mutate(time= as.yearqtr(date)) %>% 
	group_by(time) %>% 
	summarise(across(where(is.numeric), mean, na.rm= TRUE), .groups= "drop")  %>% 
	filter(time >= as.yearqtr("1997 Q1"),
	time <= as.yearqtr("2024 Q4"))

# CombinedRiskDataframe <- FedRiskIndicies_filtered %>% 

fit_vader$scores   <- fit_vader$scores   %>% rename(time = Date)
fit_finbert$scores <- fit_finbert$scores %>% rename(time = Date) 

to_date <- function(df) {
  df %>% mutate(time = as.Date(time))
}


##########
########## Shadow Rate
##########

parse_time_col <- function(x) {
  sapply(x, function(val) {
    num <- suppressWarnings(as.numeric(val))
    if (!is.na(num) && nchar(trimws(as.character(val))) == 6) {
      as.Date(paste0(as.character(as.integer(num)), "01"), format = "%Y%m%d")
    } else {
      as.Date(as.character(val), format = "%m/%d/%y")
    }
  }) |> as.Date(origin = "1970-01-01")
}

shadowRate_raw <- read_excel(
  "/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx",
  sheet = "ShadowRate",
  col_types = "text"
)

parse_time_col <- function(x) {
  sapply(x, function(val) {
    num <- suppressWarnings(as.numeric(val))
    if (!is.na(num) && nchar(trimws(val)) == 6) {
      # YYYYMM format
      as.Date(paste0(val, "01"), format = "%Y%m%d")
    } else if (!is.na(num)) {
      # Excel serial date
      as.Date(as.integer(num), origin = "1899-12-30")
    } else {
      as.Date(val, format = "%m/%d/%y")
    }
  }) |> as.Date(origin = "1970-01-01")
}

shadowRate <- shadowRate_raw %>%
  mutate(
    Time_parsed = parse_time_col(Time),
    Shadow      = as.numeric(Shadow),
    time        = as.yearqtr(Time_parsed)
  ) %>%
  arrange(time) %>%
  group_by(time) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  filter(
    time >= as.yearqtr("1997 Q1"),
    time <= as.yearqtr("2024 Q4")
  )

CombinedRiskDataframe <- list(
  FedRiskIndicies_filtered,
  shapiro_quart,
  custom_risk_index$scores     %>% rename(CUSTOM_INDEX      = PC1),
  custom_mix_index$scores      %>% rename(CUSTOM_MIX        = PC1),
  fit_vader$scores             %>% rename(SENTIMENT_VADER   = PC1),
  fit_finbert$scores           %>% rename(SENTIMENT_FINBERT = PC1),
  shadowRate                   %>% select(time, Shadow)
) %>%
  map(to_date) %>%
  reduce(left_join, by = "time")

colnames(CombinedRiskDataframe)



########## Shadow Banking Small 

shadow_sector_data_small <- read_excel("/Users/borisgerat/Documents/Projects/MA_Thesis/DATA_MAIN_MA.xlsx", sheet ="ShadowTogether")

colnames(shadow_sector_data_small)
shadow_variables <- c("WholeSaleFunding","LiquidityBuffer", "CapitalCushion", "TotalLoans")

shadow_variables_filtered <- shadow_sector_data_small %>% mutate(time = as.yearqtr(Time)) %>% 
	filter(time >= as.yearqtr("1997 Q1"), time <= as.yearqtr("2024 Q4"))

shadow_variables_filtered
shadow_factor_small <- pca_risk_index(
df                 = shadow_variables_filtered,
vars               = shadow_variables,
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
flip_sign          = FALSE,
z_score            = TRUE,
plot_factor        = TRUE,
factor_name        = "Shadowbanking Factor Small"
)

print(shadow_factor_small$convergence)
print(shadow_factor_small$explained)
print(shadow_factor_small$loadings)
print(shadow_factor_small$diff_orders)
print(head(shadow_factor_small$scores))

colnames(CombinedRiskDataframe)

CombinedRiskDataframe <- CombinedRiskDataframe %>%
  mutate(SENTIMENT_VADER = -SENTIMENT_VADER)


risk_vars <- CombinedRiskDataframe %>%
  select(STLFSI, NFCI, KCFSI, VIX, EPUI, NSI_Shapiro, 
         CUSTOM_INDEX, CUSTOM_MIX, SENTIMENT_VADER, SENTIMENT_FINBERT, Shadow) %>%
  rename(
    `STL FSI`       = STLFSI,
    `NFCI`          = NFCI,
    `KC FSI`        = KCFSI,
    `VIX`           = VIX,
    `EPU Index`     = EPUI,
    `NSI (Shapiro)` = NSI_Shapiro,
    `Custom Index`  = CUSTOM_INDEX,
    `Custom Mix`    = CUSTOM_MIX,
    `VADER Sent.`   = SENTIMENT_VADER,
    `FinBERT Sent.` = SENTIMENT_FINBERT,
    `Fed Shadow Rate` = Shadow
  )

# Compute correlation matrix (pairwise complete for robustness)
cor_matrix <- cor(risk_vars, use = "pairwise.complete.obs")

# --- Plot ---
# Highlight Shadow Rate: bold its row/column label
n     <- ncol(cor_matrix)
labels <- colnames(cor_matrix)
shadow_idx <- which(labels == "Fed Shadow Rate")

# Font styles: bold (2) for Shadow Rate, plain (1) for others
font_styles <- rep(1, n)
font_styles[shadow_idx] <- 2

pdf("fig_correlation_matrix.pdf", width = 10, height = 10)

corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  order       = "hclust",
  tl.col      = ifelse(labels[hclust(dist(cor_matrix))$order == shadow_idx], "firebrick", "black"),
  addCoef.col = "black",
  number.cex  = 0.65,
  tl.cex      = 0.85,
  tl.srt      = 45,
  col         = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
  cl.cex      = 0.75,
  mar         = c(0, 0, 2, 0),
  title       = "Correlation Matrix — Risk Indices & Fed Shadow Rate"
)

dev.off() 
