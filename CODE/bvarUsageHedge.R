# -----------------------------------------------------------------------------
# HEDGE FUND SECTOR BVAR
# -----------------------------------------------------------------------------

# 1. PREPARE HEDGE FUND SECTOR DATA
hedge_sector_bvar <- hedge_data_filtered %>%
  select(time, WholeSaleFunding, LiquidityBuffer, CapitalCushion, TotalLoans) %>%
  rename(
    Hedge_WholeSaleFunding = WholeSaleFunding,
    Hedge_LiquidityBuffer  = LiquidityBuffer,
    Hedge_CapitalCushion   = CapitalCushion,
    Hedge_TotalLoans       = TotalLoans
  ) %>%
  mutate(time = as.Date(as.yearqtr(time)))

# 2. LOOP
RISK_DISPLAY_NAMES <- c(
  "STLFSI"            = "STLFSI",
  "NFCI"              = "NFCI",
  "KCFSI"             = "KCFSI",
  "VIX"               = "VIX",
  "EPUI"              = "EPUI",
  "NSI_Shapiro"       = "NSI (Shapiro)",
  "CUSTOM_INDEX"      = "Custom Index",
  "CUSTOM_MIX"        = "Custom Mix",
  "SENTIMENT_VADER"   = "Sentiment VADER",
  "SENTIMENT_FINBERT" = "Sentiment FinBERT"
)

all_plots_mp_risk_hedge  <- list()
all_plots_facet_hedge    <- list()
all_cor_tables_hedge     <- list()

for (risk_idx in RISK_INDICES) {

  display_name <- RISK_DISPLAY_NAMES[[risk_idx]]
  cat(sprintf("\n\n========== HEDGE FUNDS | RUNNING: %s ==========\n", display_name))

  risk_col <- CombinedRiskDataframe %>%
    mutate(time = as.Date(time)) %>%
    select(time, all_of(risk_idx))

  if (risk_idx %in% FLIP_INDICES) {
    cat(sprintf("  [INFO] Flipping %s (sentiment index)\n", display_name))
    risk_col[[risk_idx]] <- -risk_col[[risk_idx]]
  }

  risk_col <- risk_col %>% rename(!!display_name := all_of(risk_idx))

  risk_mp_df <- shadow_df %>%
    left_join(risk_col, by = "time") %>%
    select(time, Shadow, all_of(display_name))

  result <- tryCatch({
    bvar_risk_channel(
      sector_dfs  = list(Hedge = hedge_sector_bvar),
      risk_mp_df  = risk_mp_df,
      time_col    = "time",

      mp_var      = "Shadow",
      risk_var    = display_name,
      sector_vars = list(
        Hedge = c("Hedge_WholeSaleFunding", "Hedge_LiquidityBuffer",
                  "Hedge_CapitalCushion", "Hedge_TotalLoans")
      ),

      start_date  = "1997-01-01",
      end_date    = "2024-10-01",

      lags        = 3L,
      n_draw      = 20000L,
      n_burn      = 7000L,
      n_thin      = 5L,
      horizon     = 20L,
      seed        = 42L,

      sign_restr     = NULL,
      run_cholesky   = TRUE,
      cholesky_order = c(display_name, "Shadow",
                         "Hedge_WholeSaleFunding", "Hedge_LiquidityBuffer",
                         "Hedge_CapitalCushion", "Hedge_TotalLoans"),

      plot         = TRUE,
      title_prefix = sprintf("Hedge Funds | %s", display_name),
      verbose      = FALSE
    )
  }, error = function(e) {
    cat(sprintf("  [ERROR] %s failed: %s\n", display_name, conditionMessage(e)))
    NULL
  })

  if (is.null(result)) next

  risk_transformed <- result$var_map$risk_var
  mp_transformed   <- result$var_map$mp_var

  mp_risk_key <- paste0("chol_", mp_transformed, "_to_", risk_transformed)
  p_mp_risk   <- result$plots[[mp_risk_key]]
  p_facet     <- result$plots[["chol_facet_Hedge"]]

  if (!is.null(p_mp_risk)) all_plots_mp_risk_hedge[[risk_idx]] <- p_mp_risk
  if (!is.null(p_facet))   all_plots_facet_hedge[[risk_idx]]   <- p_facet

  if (!is.null(result$vcov_cholesky)) {
    cor_mat <- round(cov2cor(result$vcov_cholesky), 3)
    all_cor_tables_hedge[[risk_idx]] <- cor_mat
    cat(sprintf("  [OK] %s — correlation matrix extracted\n", display_name))
  }
}

# 3. SAVE INDIVIDUAL PDFs
out_dir_hedge <- "~/Documents/Projects/Ma_Thesis/CODE/bvar_plots/hedge_funds"
dir.create(out_dir_hedge, showWarnings = FALSE, recursive = TRUE)

for (risk_idx in RISK_INDICES) {

  p1           <- all_plots_mp_risk_hedge[[risk_idx]]
  p2           <- all_plots_facet_hedge[[risk_idx]]
  display_name <- RISK_DISPLAY_NAMES[[risk_idx]]
  suffix       <- ifelse(risk_idx %in% FLIP_INDICES, "_flipped", "")
  file_name    <- gsub("[^A-Za-z0-9_]", "_", display_name)

  if (!is.null(p1)) {
    pdf(file.path(out_dir_hedge, sprintf("%s%s_shadow_to_risk.pdf", file_name, suffix)),
        width = 10, height = 5)
    print(p1)
    dev.off()
  }

  if (!is.null(p2)) {
    pdf(file.path(out_dir_hedge, sprintf("%s%s_risk_to_hedge.pdf", file_name, suffix)),
        width = 12, height = 7)
    print(p2)
    dev.off()
  }

  cat(sprintf("  [SAVED] %s\n", display_name))
}

cat(sprintf("\nDone. Files saved to: %s\n", out_dir_hedge))

# 4. SAVE CORRELATION TABLES PDF
cairo_pdf("~/Documents/Projects/Ma_Thesis/CODE/bvar_tables_hedge.pdf",
          width = 14, height = 8, onefile = TRUE)

for (risk_idx in names(all_cor_tables_hedge)) {
  display_name <- RISK_DISPLAY_NAMES[[risk_idx]]
  cor_mat      <- all_cor_tables_hedge[[risk_idx]]
  clean_names <- function(nm) {
    nm <- sub("_dlog$", " (Δlog)", nm)
    nm <- sub("_diff$", " (Δ)", nm)
    nm <- sub("_lvl$", " (lvl)", nm)
    gsub("_", " ", nm)
  }
  rownames(cor_mat) <- clean_names(rownames(cor_mat))
  colnames(cor_mat) <- clean_names(colnames(cor_mat))
  display_df <- cbind(Variable = rownames(cor_mat), as.data.frame(cor_mat))
  rownames(display_df) <- NULL
  grid.newpage()
  grid.text(
    sprintf("Hedge Funds | Residual Correlation Matrix — %s%s",
            display_name, ifelse(risk_idx %in% FLIP_INDICES, " [flipped]", "")),
    x = 0.5, y = 0.95, gp = gpar(fontsize = 13, fontface = "bold")
  )
  grid.text("Reduced-form residual correlations (Cholesky BVAR, p=3, 1997Q1-2024Q4)",
            x = 0.5, y = 0.90, gp = gpar(fontsize = 9, col = "grey40"))
  tbl <- tableGrob(display_df, rows = NULL, theme = ttheme_minimal(base_size = 8))
  tbl$vp <- viewport(y = 0.45, height = 0.8)
  grid.draw(tbl)
}

dev.off()

cat("\n===== HEDGE FUNDS DONE =====\n")
cat(sprintf("Models estimated : %d / %d\n",
            length(all_cor_tables_hedge), length(RISK_INDICES)))
