banking_sector_bvar <- banking_sector_window %>%
  select(time, WholeSaleFunding, LiquidityBuffer, CapitalCushion, ComInduLoans) %>%
  rename(
    Bank_WholeSale = WholeSaleFunding,
    Bank_Liquidity = LiquidityBuffer,
    Bank_Capital   = CapitalCushion,
    Bank_CILoans   = ComInduLoans
  ) %>%
  mutate(time = as.Date(as.yearqtr(time)))

risk_mp_vix <- FedRiskIndicies_filtered %>%
  mutate(time = as.Date(as.yearqtr(time))) %>%
  select(time, VIX) %>%
  left_join(
    CombinedRiskDataframe %>%
      mutate(time = as.Date(time)) %>%
      select(time, Shadow),
    by = "time"
  ) %>%
  select(time, Shadow, VIX)

stopifnot(exists("banking_sector_bvar"), nrow(banking_sector_bvar) > 0)
cat("OK — banking rows:", nrow(banking_sector_bvar), "\n")



SR_bank_vix <- list(
  Shadow         = 1,
  VIX            =  1,
  Bank_WholeSale =  1,
  Bank_Liquidity = -1,
  Bank_Capital   = -1,
  Bank_CILoans   =  1
)

bvar_bank_vix <- bvar_risk_channel(
  sector_dfs     = list(Banking = banking_sector_bvar),
  risk_mp_df     = risk_mp_vix,
  time_col       = "time",
  mp_var         = "Shadow",
  risk_var       = "VIX",
  sector_vars    = list(
    Banking = c("Bank_WholeSale","Bank_Liquidity","Bank_Capital","Bank_CILoans")
  ),
  start_date     = "1997-01-01",
  end_date       = "2024-10-01",
  lags           = 3L,
  n_draw         = 20000L,
  n_burn         = 7000L,
  n_thin         = 5L,
  horizon        = 20L,
  seed           = 42L,
  sign_restr       = SR_bank_vix,
  sign_strict      = FALSE,
  try_scaled       = TRUE,
  try_orientations = TRUE,
  run_cholesky     = TRUE,
  cholesky_order   = c("Shadow","VIX","Bank_WholeSale","Bank_Liquidity","Bank_Capital","Bank_CILoans"),
  plot             = TRUE,
  title_prefix     = "Banking | VIX",
  verbose          = TRUE
)
