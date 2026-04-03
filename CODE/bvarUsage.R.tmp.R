# ===========================================================================
# SIGN RESTRICTIONS — full first column (monetary easing shock)
# Every variable in the VAR needs a sign for shock 1
# Economic logic: shadow rate falls -> risk rises -> sectors take more risk
# ===========================================================================

# --- Banking model signs ---
SR_banking_base <- list(
  Shadow         = -1,   # MP eases: shadow rate falls
  CUSTOM_INDEX   =  1,   # risk index rises (risk-taking channel)
  Bank_WholeSale =  1,   # wholesale funding expands (cheaper to fund)
  Bank_Liquidity = -1,   # liquidity buffer shrinks (more risk-taking)
  Bank_Capital   = -1,   # capital cushion erodes (leverage up)
  Bank_CILoans   =  1    # C&I lending expands
)

SR_banking_mix <- list(
  Shadow         = -1,
  CUSTOM_MIX     =  1,
  Bank_WholeSale =  1,
  Bank_Liquidity = -1,
  Bank_Capital   = -1,
  Bank_CILoans   =  1
)

SR_banking_stlfsi <- list(
  Shadow         = -1,
  STLFSI         =  1,
  Bank_WholeSale =  1,
  Bank_Liquidity = -1,
  Bank_Capital   = -1,
  Bank_CILoans   =  1
)

SR_banking_nfci <- list(
  Shadow         = -1,
  NFCI           =  1,
  Bank_WholeSale =  1,
  Bank_Liquidity = -1,
  Bank_Capital   = -1,
  Bank_CILoans   =  1
)

SR_banking_kcfsi <- list(
  Shadow         = -1,
  KCFSI          =  1,
  Bank_WholeSale =  1,
  Bank_Liquidity = -1,
  Bank_Capital   = -1,
  Bank_CILoans   =  1
)

SR_banking_vix <- list(
  Shadow         = -1,
  VIX            =  1,
  Bank_WholeSale =  1,
  Bank_Liquidity = -1,
  Bank_Capital   = -1,
  Bank_CILoans   =  1
)

SR_banking_epui <- list(
  Shadow         = -1,
  EPUI           =  1,
  Bank_WholeSale =  1,
  Bank_Liquidity = -1,
  Bank_Capital   = -1,
  Bank_CILoans   =  1
)

# --- Shadow Banking model signs ---
SR_shadow_base <- list(
  Shadow          = -1,
  CUSTOM_INDEX    =  1,
  Shad_WholeSale  =  1,
  Shad_Liquidity  = -1,
  Shad_Capital    = -1,
  Shad_TotalLoans =  1
)

SR_shadow_stlfsi <- list(
  Shadow          = -1,
  STLFSI          =  1,
  Shad_WholeSale  =  1,
  Shad_Liquidity  = -1,
  Shad_Capital    = -1,
  Shad_TotalLoans =  1
)

SR_shadow_nfci <- list(
  Shadow          = -1,
  NFCI            =  1,
  Shad_WholeSale  =  1,
  Shad_Liquidity  = -1,
  Shad_Capital    = -1,
  Shad_TotalLoans =  1
)

SR_shadow_vix <- list(
  Shadow          = -1,
  VIX             =  1,
  Shad_WholeSale  =  1,
  Shad_Liquidity  = -1,
  Shad_Capital    = -1,
  Shad_TotalLoans =  1
)

# --- Hedge Fund model signs ---
SR_hedge_base <- list(
  Shadow           = -1,
  CUSTOM_INDEX     =  1,
  Hedge_WholeSale  =  1,
  Hedge_Liquidity  = -1,
  Hedge_Capital    = -1,
  Hedge_TotalLoans =  1
)

SR_hedge_stlfsi <- list(
  Shadow           = -1,
  STLFSI           =  1,
  Hedge_WholeSale  =  1,
  Hedge_Liquidity  = -1,
  Hedge_Capital    = -1,
  Hedge_TotalLoans =  1
)

SR_hedge_nfci <- list(
  Shadow           = -1,
  NFCI             =  1,
  Hedge_WholeSale  =  1,
  Hedge_Liquidity  = -1,
  Hedge_Capital    = -1,
  Hedge_TotalLoans =  1
)

SR_hedge_vix <- list(
  Shadow           = -1,
  VIX              =  1,
  Hedge_WholeSale  =  1,
  Hedge_Liquidity  = -1,
  Hedge_Capital    = -1,
  Hedge_TotalLoans =  1
)

# Cholesky orderings
CHOL_banking <- c("Shadow","CUSTOM_INDEX","Bank_WholeSale","Bank_Liquidity","Bank_Capital","Bank_CILoans")
CHOL_shadow  <- c("Shadow","CUSTOM_INDEX","Shad_WholeSale","Shad_Liquidity","Shad_Capital","Shad_TotalLoans")
CHOL_hedge   <- c("Shadow","CUSTOM_INDEX","Hedge_WholeSale","Hedge_Liquidity","Hedge_Capital","Hedge_TotalLoans")

# ===========================================================================
# RISK MP DATAFRAMES — one per risk index, trimmed to only what the VAR needs
# ===========================================================================

risk_mp_custom  <- CombinedRiskDataframe %>% select(time, Shadow, CUSTOM_INDEX)
risk_mp_mix     <- CombinedRiskDataframe %>% select(time, Shadow, CUSTOM_MIX)
risk_mp_stlfsi  <- FedRiskIndicies_filtered %>% select(time, Shadow = STLFSI) %>%
                     left_join(CombinedRiskDataframe %>% select(time, Shadow), by = "time") %>%
                     select(time, Shadow, STLFSI = Shadow.x)

# cleaner approach — build one combined df with all Fed indices + Shadow
fed_indices_mp <- FedRiskIndicies_filtered %>%
  select(time, STLFSI, NFCI, KCFSI, VIX, EPUI) %>%
  mutate(time = as.Date(as.yearqtr(time))) %>%
  left_join(CombinedRiskDataframe %>% select(time, Shadow), by = "time")

# now individual risk_mp dfs are trivial
risk_mp_stlfsi  <- fed_indices_mp %>% select(time, Shadow, STLFSI)
risk_mp_nfci    <- fed_indices_mp %>% select(time, Shadow, NFCI)
risk_mp_kcfsi   <- fed_indices_mp %>% select(time, Shadow, KCFSI)
risk_mp_vix     <- fed_indices_mp %>% select(time, Shadow, VIX)
risk_mp_epui    <- fed_indices_mp %>% select(time, Shadow, EPUI)

# ===========================================================================
# HELPER — single model runner to avoid repetition
# ===========================================================================

run_bvar <- function(sector_df, sector_nm, sector_vars_vec,
                     risk_mp, risk_nm, sign_restr, chol_order) {
  bvar_risk_channel(
    sector_dfs     = setNames(list(sector_df), sector_nm),
    risk_mp_df     = risk_mp,
    time_col       = "time",
    mp_var         = "Shadow",
    risk_var       = risk_nm,
    sector_vars    = setNames(list(sector_vars_vec), sector_nm),
    start_date     = BVAR_START,
    end_date       = BVAR_END,
    lags           = BVAR_LAGS,
    n_draw         = BVAR_DRAWS,
    n_burn         = BVAR_BURN,
    n_thin         = BVAR_THIN,
    horizon        = BVAR_HORIZON,
    seed           = BVAR_SEED,
    sign_restr     = sign_restr,
    sign_strict    = FALSE,
    try_scaled     = TRUE,
    try_orientations = TRUE,
    run_cholesky   = TRUE,
    cholesky_order = chol_order,
    plot           = TRUE,
    title_prefix   = sprintf("%s | %s", sector_nm, risk_nm),
    verbose        = TRUE
  )
}

BANK_VARS  <- c("Bank_WholeSale","Bank_Liquidity","Bank_Capital","Bank_CILoans")
SHAD_VARS  <- c("Shad_WholeSale","Shad_Liquidity","Shad_Capital","Shad_TotalLoans")
HEDGE_VARS <- c("Hedge_WholeSale","Hedge_Liquidity","Hedge_Capital","Hedge_TotalLoans")

# ===========================================================================
# BANKING RUNS
# ===========================================================================

bvar_bank_custom <- run_bvar(banking_sector_bvar, "Banking", BANK_VARS,
                              risk_mp_custom, "CUSTOM_INDEX", SR_banking_base,
                              CHOL_banking)

bvar_bank_stlfsi <- run_bvar(banking_sector_bvar, "Banking", BANK_VARS,
                              risk_mp_stlfsi, "STLFSI", SR_banking_stlfsi,
                              c("Shadow","STLFSI",BANK_VARS))

bvar_bank_nfci   <- run_bvar(banking_sector_bvar, "Banking", BANK_VARS,
                              risk_mp_nfci, "NFCI", SR_banking_nfci,
                              c("Shadow","NFCI",BANK_VARS))

bvar_bank_kcfsi  <- run_bvar(banking_sector_bvar, "Banking", BANK_VARS,
                              risk_mp_kcfsi, "KCFSI", SR_banking_kcfsi,
                              c("Shadow","KCFSI",BANK_VARS))

bvar_bank_vix    <- run_bvar(banking_sector_bvar, "Banking", BANK_VARS,
                              risk_mp_vix, "VIX", SR_banking_vix,
                              c("Shadow","VIX",BANK_VARS))

bvar_bank_epui   <- run_bvar(banking_sector_bvar, "Banking", BANK_VARS,
                              risk_mp_epui, "EPUI", SR_banking_epui,
                              c("Shadow","EPUI",BANK_VARS))

# ===========================================================================
# SHADOW BANKING RUNS
# ===========================================================================

bvar_shad_custom <- run_bvar(shadow_sector_bvar, "ShadowBank", SHAD_VARS,
                              risk_mp_custom, "CUSTOM_INDEX", SR_shadow_base,
                              CHOL_shadow)

bvar_shad_stlfsi <- run_bvar(shadow_sector_bvar, "ShadowBank", SHAD_VARS,
                              risk_mp_stlfsi, "STLFSI", SR_shadow_stlfsi,
                              c("Shadow","STLFSI",SHAD_VARS))

bvar_shad_nfci   <- run_bvar(shadow_sector_bvar, "ShadowBank", SHAD_VARS,
                              risk_mp_nfci, "NFCI", SR_shadow_nfci,
                              c("Shadow","NFCI",SHAD_VARS))

bvar_shad_vix    <- run_bvar(shadow_sector_bvar, "ShadowBank", SHAD_VARS,
                              risk_mp_vix, "VIX", SR_shadow_vix,
                              c("Shadow","VIX",SHAD_VARS))

# ===========================================================================
# HEDGE FUND RUNS
# ===========================================================================

bvar_hedge_custom <- run_bvar(hedge_sector_bvar, "HedgeFund", HEDGE_VARS,
                               risk_mp_custom, "CUSTOM_INDEX", SR_hedge_base,
                               CHOL_hedge)

bvar_hedge_stlfsi <- run_bvar(hedge_sector_bvar, "HedgeFund", HEDGE_VARS,
                               risk_mp_stlfsi, "STLFSI", SR_hedge_stlfsi,
                               c("Shadow","STLFSI",HEDGE_VARS))

bvar_hedge_nfci   <- run_bvar(hedge_sector_bvar, "HedgeFund", HEDGE_VARS,
                               risk_mp_nfci, "NFCI", SR_hedge_nfci,
                               c("Shadow","NFCI",HEDGE_VARS))

bvar_hedge_vix    <- run_bvar(hedge_sector_bvar, "HedgeFund", HEDGE_VARS,
                               risk_mp_vix, "VIX", SR_hedge_vix,
                               c("Shadow","VIX",HEDGE_VARS))

# ===========================================================================
# IRF PLOTS — Cholesky (always available), sign-ID where it converged
# ===========================================================================

# Banking — shadow rate shock -> each sector variable
for (v in BANK_VARS) {
  print(bvar_rc_plot(bvar_bank_custom, shock = "Shadow", response = v, id = "cholesky"))
}

# Shadow banking
for (v in SHAD_VARS) {
  print(bvar_rc_plot(bvar_shad_custom, shock = "Shadow", response = v, id = "cholesky"))
}

# Hedge funds
for (v in HEDGE_VARS) {
  print(bvar_rc_plot(bvar_hedge_custom, shock = "Shadow", response = v, id = "cholesky"))
}
