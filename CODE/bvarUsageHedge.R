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
all_plots_mp_risk_hedge  <- list()
all_plots_facet_hedge    <- list()
all_cor_tables_hedge     <- list()

for (risk_idx in RISK_INDICES) {

  cat(sprintf("\n\n========== HEDGE | RUNNING: %s ==========\n", risk_idx))

  risk_col <- CombinedRiskDataframe %>%
    mutate(time = as.Date(time)) %>%
    select(time, all_of(risk_idx))

  if (risk_idx %in% FLIP_INDICES) {
    cat(sprintf("  [INFO] Flipping %s (sentiment index)\n", risk_idx))
    risk_col[[risk_idx]] <- -risk_col[[risk_idx]]
  }

  risk_mp_df <- shadow_df %>%
    left_join(risk_col, by = "time") %>%
    select(time, Shadow, all_of(risk_idx))

  result <- tryCatch({
    bvar_risk_channel(
      sector_dfs  = list(Hedge = hedge_sector_bvar),
      risk_mp_df  = risk_mp_df,
      time_col    = "time",

      mp_var      = "Shadow",
      risk_var    = risk_idx,
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
      cholesky_order = c(risk_idx, "Shadow",
                         "Hedge_WholeSaleFunding", "Hedge_LiquidityBuffer",
                         "Hedge_CapitalCushion", "Hedge_TotalLoans"),

      plot         = TRUE,
      title_prefix = sprintf("Hedge | %s", risk_idx),
      verbose      = FALSE
    )
  }, error = function(e) {
    cat(sprintf("  [ERROR] %s failed: %s\n", risk_idx, conditionMessage(e)))
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
    cat(sprintf("  [OK] %s — correlation matrix extracted\n", risk_idx))
  }
}

# 3. SAVE PDFs
graphics.off()

cairo_pdf("~/Documents/Projects/Ma_Thesis/CODE/bvar_plots_hedge.pdf",
          width = 12, height = 7, onefile = TRUE)

for (risk_idx in names(all_plots_mp_risk_hedge)) {
  grid.newpage()
  grid.text(
    sprintf("Hedge | Risk Index: %s%s", risk_idx,
            ifelse(risk_idx %in% FLIP_INDICES, "  [FLIPPED]", "")),
    gp = gpar(fontsize = 18, fontface = "bold")
  )
  p1 <- all_plots_mp_risk_hedge[[risk_idx]]
  if (!is.null(p1)) print(p1)
  p2 <- all_plots_facet_hedge[[risk_idx]]
  if (!is.null(p2)) print(p2)
}

dev.off()

cairo_pdf("~/Documents/Projects/Ma_Thesis/CODE/bvar_tables_hedge.pdf",
          width = 14, height = 8, onefile = TRUE)

for (risk_idx in names(all_cor_tables_hedge)) {
  cor_mat <- all_cor_tables_hedge[[risk_idx]]
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
  grid.text(sprintf("Hedge | Residual Correlation Matrix — %s%s", risk_idx,
                    ifelse(risk_idx %in% FLIP_INDICES, " [flipped]", "")),
            x = 0.5, y = 0.95, gp = gpar(fontsize = 13, fontface = "bold"))
  grid.text("Reduced-form residual correlations (Cholesky BVAR, p=3, 1997Q1-2024Q4)",
            x = 0.5, y = 0.90, gp = gpar(fontsize = 9, col = "grey40"))
  tbl <- tableGrob(display_df, rows = NULL, theme = ttheme_minimal(base_size = 8))
  tbl$vp <- viewport(y = 0.45, height = 0.8)
  grid.draw(tbl)
}

dev.off()

cat("\n===== HEDGE DONE =====\n")
cat(sprintf("Models estimated : %d / %d\n",
            length(all_cor_tables_hedge), length(RISK_INDICES)))
