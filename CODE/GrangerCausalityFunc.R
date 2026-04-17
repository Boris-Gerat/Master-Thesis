granger_risk_analysis <- function(risk_df, sector_ts, max_lag = 4, sector_name = "sector",
                                  exclude_cols = c("time", "Time", "date", "Date")) {
  library(lmtest)
  library(dplyr)

  common_idx <- intersect(rownames(risk_df), names(sector_ts))
  if (length(common_idx) == 0) {
    n <- min(nrow(risk_df), length(sector_ts))
    risk_aligned   <- risk_df[1:n, , drop = FALSE]
    sector_aligned <- sector_ts[1:n]
  } else {
    risk_aligned   <- risk_df[common_idx, , drop = FALSE]
    sector_aligned <- sector_ts[common_idx]
  }

  risk_aligned <- risk_aligned %>% select(-any_of(exclude_cols))

  run_test <- function(idx_name, formula_str) {
    df <- data.frame(
      sector = as.numeric(sector_aligned),
      risk   = as.numeric(risk_aligned[[idx_name]])
    )
    df <- df[complete.cases(df), ]
    tryCatch({
      test <- grangertest(as.formula(formula_str), order = max_lag, data = df)
      list(p = round(test$`Pr(>F)`[2], 4))
    }, error = function(e) list(p = NA))
  }

  fmt <- function(p) {
    sig <- case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.10 ~ "*",
      TRUE     ~ "n.s."
    )
    paste0("p=", p, " ", sig)
  }

  results <- lapply(colnames(risk_aligned), function(idx_name) {
    r_to_s <- run_test(idx_name, "sector ~ risk")
    s_to_r <- run_test(idx_name, "risk ~ sector")

    sig_r_to_s <- !is.na(r_to_s$p) && r_to_s$p < 0.05
    sig_s_to_r <- !is.na(s_to_r$p) && s_to_r$p < 0.05

    causality <- case_when(
      sig_r_to_s & sig_s_to_r  ~ "Bidirectional",
      sig_r_to_s & !sig_s_to_r ~ paste0(idx_name, " -> ", sector_name),
      !sig_r_to_s & sig_s_to_r ~ paste0(sector_name, " -> ", idx_name),
      TRUE                      ~ "No causality"
    )

    data.frame(
      risk_index        = idx_name,
      risk_to_sector    = fmt(r_to_s$p),
      sector_to_risk    = fmt(s_to_r$p),
      min_p             = min(r_to_s$p, s_to_r$p, na.rm = TRUE),
      lags              = max_lag,
      granger_causality = causality,
      stringsAsFactors  = FALSE
    )
  })

  bind_rows(results) %>%
    arrange(min_p) %>%
    select(-min_p)
}






##############
############## ESTIMATION
##############

ResultBankingGranger <- granger_risk_analysis(CombinedRiskDataframe, banking_factor$scores$PC1,
					      sector_name = "Banking Factor")
colnames(ResultBankingGranger)[2:3] <- c("Risk -> Banking Factor", "Banking Factor -> Risk")
ResultBankingGranger

##############  SHADOWBANKING 

ResultShadowbankingGranger <- granger_risk_analysis(CombinedRiskDataframe, shadow_factor_broad$scores$PC1,
					      sector_name = "Shadowbanking Factor Broad")
colnames(ResultShadowbankingGranger)[2:3] <- c("Risk -> Shadowbanking Factor Broad", "Shadowbanking Factor Broad -> Risk")
ResultShadowbankingGranger

############## HEDGEFUNDS

ResultHedgefundGranger <- granger_risk_analysis(CombinedRiskDataframe, hedge_factor$scores$PC1,
					      sector_name = "Hedgefund Factor")
colnames(ResultHedgefundGranger)[2:3] <- c("Risk -> Hedgefund Factor", "Hedgefund Factor -> Risk")
ResultHedgefundGranger

############## ShadowSmall 

ShadowBankingSmallGranger <- granger_risk_analysis(CombinedRiskDataframe, shadow_factor_small$scores$PC1,
					      sector_name = "Shadow banking Factor Small")
colnames(ShadowBankingSmallGranger)[2:3] <- c("Risk -> Shadow banking Factor Small", "Shadow Factor small -> Risk")
ShadowBankingSmallGranger








