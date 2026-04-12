# -----------------------------------------------------------------------------
# SHADOW BANKING SECTOR BVAR
# -----------------------------------------------------------------------------

# 1. PREPARE SHADOW SECTOR DATA
shadow_sector_bvar <- shadow_variables_filtered %>%
  select(time, WholeSaleFunding, LiquidityBuffer, CapitalCushion, TotalLoans) %>%
  rename(
    Shadow_WholeSaleFunding = WholeSaleFunding,
    Shadow_LiquidityBuffer  = LiquidityBuffer,
    Shadow_CapitalCushion   = CapitalCushion,
    Shadow_TotalLoans       = TotalLoans
  ) %>%
  mutate(time = as.Date(as.yearqtr(time)))

# 2. LOOP — identical to banking, just swap sector_dfs and sector_vars
all_plots_mp_risk_shadow  <- list()
all_plots_facet_shadow    <- list()
all_cor_tables_shadow     <- list()

for (risk_idx in RISK_INDICES) {

  cat(sprintf("\n\n========== SHADOW | RUNNING: %s ==========\n", risk_idx))

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
      sector_dfs  = list(Shadow = shadow_sector_bvar),
      risk_mp_df  = risk_mp_df,
      time_col    = "time",

      mp_var      = "Shadow",
      risk_var    = risk_idx,
      sector_vars = list(
        Shadow = c("Shadow_WholeSaleFunding", "Shadow_LiquidityBuffer",
                   "Shadow_CapitalCushion", "Shadow_TotalLoans")
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
      cholesky_order = c (
                         "Shadow_WholeSaleFunding", "Shadow_LiquidityBuffer",
                         "Shadow_CapitalCushion", "Shadow_TotalLoans", risk_idx,"Shadow"),

      plot         = TRUE,
      title_prefix = sprintf("Shadow | %s", risk_idx),
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
  p_facet     <- result$plots[["chol_facet_Shadow"]]

  if (!is.null(p_mp_risk)) all_plots_mp_risk_shadow[[risk_idx]] <- p_mp_risk
  if (!is.null(p_facet))   all_plots_facet_shadow[[risk_idx]]   <- p_facet

  if (!is.null(result$vcov_cholesky)) {
    cor_mat <- round(cov2cor(result$vcov_cholesky), 3)
    all_cor_tables_shadow[[risk_idx]] <- cor_mat
    cat(sprintf("  [OK] %s — correlation matrix extracted\n", risk_idx))
  }
}

# 3. SAVE PDFs
graphics.off()

cairo_pdf("~/Documents/Projects/Ma_Thesis/CODE/bvar_plots_shadow.pdf",
          width = 12, height = 7, onefile = TRUE)

for (risk_idx in names(all_plots_mp_risk_shadow)) {
  grid.newpage()
  grid.text(
    sprintf("Shadow | Risk Index: %s%s", risk_idx,
            ifelse(risk_idx %in% FLIP_INDICES, "  [FLIPPED]", "")),
    gp = gpar(fontsize = 18, fontface = "bold")
  )
  p1 <- all_plots_mp_risk_shadow[[risk_idx]]
  if (!is.null(p1)) print(p1)
  p2 <- all_plots_facet_shadow[[risk_idx]]
  if (!is.null(p2)) print(p2)
}

dev.off()

cairo_pdf("~/Documents/Projects/Ma_Thesis/CODE/bvar_tables_shadow.pdf",
          width = 14, height = 8, onefile = TRUE)

for (risk_idx in names(all_cor_tables_shadow)) {
  cor_mat <- all_cor_tables_shadow[[risk_idx]]
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
  grid.text(sprintf("Shadow | Residual Correlation Matrix — %s%s", risk_idx,
                    ifelse(risk_idx %in% FLIP_INDICES, " [flipped]", "")),
            x = 0.5, y = 0.95, gp = gpar(fontsize = 13, fontface = "bold"))
  grid.text("Reduced-form residual correlations (Cholesky BVAR, p=3, 1997Q2-2024Q4)",
            x = 0.5, y = 0.90, gp = gpar(fontsize = 9, col = "grey40"))
  tbl <- tableGrob(display_df, rows = NULL, theme = ttheme_minimal(base_size = 8))
  tbl$vp <- viewport(y = 0.45, height = 0.8)
  grid.draw(tbl)
}

dev.off()

cat("\n===== SHADOW DONE =====\n")
cat(sprintf("Models estimated : %d / %d\n",
            length(all_cor_tables_shadow), length(RISK_INDICES)))

