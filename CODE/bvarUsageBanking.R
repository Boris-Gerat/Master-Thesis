# =============================================================================
# BVAR Loop — All Risk Indices, Banking Sector
# =============================================================================

setwd("~/Documents/Projects/Ma_Thesis/CODE")
source("bvarFunc.R")
source("Factors_func.R")
source("Factors_estim.R")

library(dplyr)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)

# -----------------------------------------------------------------------------
# CONFIG
# -----------------------------------------------------------------------------

RISK_INDICES <- c("STLFSI", "NFCI", "KCFSI", "VIX", "EPUI",
                  "NSI_Shapiro", "CUSTOM_INDEX", "CUSTOM_MIX",
                  "SENTIMENT_VADER", "SENTIMENT_FINBERT")

FLIP_INDICES <- c("SENTIMENT_VADER", "SENTIMENT_FINBERT", "NSI_Shapiro")

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

# -----------------------------------------------------------------------------
# 1. PREPARE SECTOR DATA
# -----------------------------------------------------------------------------
banking_sector_bvar <- banking_sector_window %>%
  select(time, WholeSaleFunding, LiquidityBuffer, CapitalCushion, ComInduLoans) %>%
  rename(
    Bank_WholeSaleFunding = WholeSaleFunding,
    Bank_LiquidityBuffer  = LiquidityBuffer,
    Bank_CapitalCushion   = CapitalCushion,
    Bank_CILoans          = ComInduLoans
  ) %>%
  mutate(time = as.Date(as.yearqtr(time)))

shadow_df <- CombinedRiskDataframe %>%
  mutate(time = as.Date(time)) %>%
  select(time, Shadow)

# -----------------------------------------------------------------------------
# 2. LOOP
# -----------------------------------------------------------------------------
all_plots_mp_risk <- list()
all_plots_facet   <- list()
all_cor_tables    <- list()

for (risk_idx in RISK_INDICES) {

  display_name <- RISK_DISPLAY_NAMES[[risk_idx]]
  cat(sprintf("\n\n========== RUNNING: %s ==========\n", display_name))

  # ── 2a. Build risk_mp dataframe ──────────────────────────────────────────
  risk_col <- CombinedRiskDataframe %>%
    mutate(time = as.Date(time)) %>%
    select(time, all_of(risk_idx))

  if (risk_idx %in% FLIP_INDICES) {
    cat(sprintf("  [INFO] Flipping %s (sentiment index)\n", display_name))
    risk_col[[risk_idx]] <- -risk_col[[risk_idx]]
  }

  # Rename column to display name so it appears correctly in plot titles
  risk_col <- risk_col %>% rename(!!display_name := all_of(risk_idx))

  risk_mp_df <- shadow_df %>%
    left_join(risk_col, by = "time") %>%
    select(time, Shadow, all_of(display_name))

  # ── 2b. Estimate BVAR ────────────────────────────────────────────────────
  result <- tryCatch({
    bvar_risk_channel(
      sector_dfs  = list(Banking = banking_sector_bvar),
      risk_mp_df  = risk_mp_df,
      time_col    = "time",

      mp_var      = "Shadow",
      risk_var    = display_name,          # ← display name
      sector_vars = list(
        Banking = c("Bank_WholeSaleFunding", "Bank_LiquidityBuffer",
                    "Bank_CapitalCushion", "Bank_CILoans")
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
      cholesky_order = c(display_name, "Shadow",   # ← display name
                         "Bank_WholeSaleFunding", "Bank_LiquidityBuffer",
                         "Bank_CapitalCushion", "Bank_CILoans"),

      plot         = TRUE,
      title_prefix = sprintf("Banking | %s", display_name),
      verbose      = FALSE
    )
  }, error = function(e) {
    cat(sprintf("  [ERROR] %s failed: %s\n", display_name, conditionMessage(e)))
    NULL
  })

  if (is.null(result)) next

  # ── 2c. Extract plots ─────────────────────────────────────────────────────
  risk_transformed <- result$var_map$risk_var
  mp_transformed   <- result$var_map$mp_var

  mp_risk_key <- paste0("chol_", mp_transformed, "_to_", risk_transformed)
  p_mp_risk   <- result$plots[[mp_risk_key]]
  p_facet     <- result$plots[["chol_facet_Banking"]]

  if (!is.null(p_mp_risk)) all_plots_mp_risk[[risk_idx]] <- p_mp_risk
  if (!is.null(p_facet))   all_plots_facet[[risk_idx]]   <- p_facet

  # ── 2d. Correlation matrix ───────────────────────────────────────────────
  if (!is.null(result$vcov_cholesky)) {
    cor_mat <- round(cov2cor(result$vcov_cholesky), 3)
    all_cor_tables[[risk_idx]] <- cor_mat
    cat(sprintf("  [OK] %s — correlation matrix extracted\n", display_name))
  }
}

# -----------------------------------------------------------------------------
# 3. SAVE INDIVIDUAL PDFs
# -----------------------------------------------------------------------------

out_dir <- "~/Documents/Projects/Ma_Thesis/CODE/bvar_plots"
dir.create(out_dir, showWarnings = FALSE)

for (risk_idx in RISK_INDICES) {

  p1           <- all_plots_mp_risk[[risk_idx]]
  p2           <- all_plots_facet[[risk_idx]]
  display_name <- RISK_DISPLAY_NAMES[[risk_idx]]
  suffix       <- ifelse(risk_idx %in% FLIP_INDICES, "_flipped", "")
  file_name    <- gsub("[^A-Za-z0-9_]", "_", display_name)

  # ── PDF 1: Shadow → Risk ──────────────────────────────────────────────────
  if (!is.null(p1)) {
    pdf(file.path(out_dir, sprintf("%s%s_shadow_to_risk.pdf", file_name, suffix)),
        width = 10, height = 5)
    print(p1)
    dev.off()
  }

  # ── PDF 2: Risk → Sector ──────────────────────────────────────────────────
  if (!is.null(p2)) {
    pdf(file.path(out_dir, sprintf("%s%s_risk_to_banking.pdf", file_name, suffix)),
        width = 12, height = 7)
    print(p2)
    dev.off()
  }

  cat(sprintf("  [SAVED] %s\n", display_name))
}

cat(sprintf("\nDone. Files saved to: %s\n", out_dir))
# Save tables PDF
pdf("~/Documents/Projects/Ma_Thesis/CODE/bvar_tables.pdf",
    width = 14, height = 8, onefile = TRUE)

for (risk_idx in names(all_cor_tables)) {
  cor_mat <- all_cor_tables[[risk_idx]]
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
  grid.text(sprintf("Residual Correlation Matrix — %s%s", risk_idx,
                    ifelse(risk_idx %in% FLIP_INDICES, " [flipped]", "")),
            x = 0.5, y = 0.95, gp = gpar(fontsize = 13, fontface = "bold"))
  tbl <- tableGrob(display_df, rows = NULL, theme = ttheme_minimal(base_size = 8))
  tbl$vp <- viewport(y = 0.45, height = 0.8)
  grid.draw(tbl)
}

dev.off()

