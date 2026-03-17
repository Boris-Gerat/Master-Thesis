# =============================================================================
# pca_risk_index — Robust EM / Probabilistic PCA Risk Index
# =============================================================================
# Dependencies : pcaMethods (Bioconductor), ggplot2, tseries
# Author notes : All fixes from review incorporated + new robustness layer
#                + stationarity checking and automatic differencing (v3)
#                  fix: diff loop now always diffs the original series by d
#                  to avoid cumulative length drift
# =============================================================================

pca_risk_index <- function(
    df,
    vars            = NULL,
    date_col        = NULL,
    n_factors       = 1,
    method          = c("ppca", "svd", "svdImpute", "bpca", "nipals"),

    # Pre-processing
    center          = TRUE,
    scale           = TRUE,
    impute_pre      = FALSE,        # mean-impute BEFORE ppca (use cautiously)

    # Stationarity
    stationarity_check = TRUE,      # run ADF test on each column
    adf_pval        = 0.05,         # significance level for ADF
    max_diff        = 2L,           # maximum differencing order attempted
    diff_pad        = "na",         # how to pad lost obs: "na" or "zero"

    # EM tuning (ppca / bpca / nipals only)
    max_iter        = 1000,
    conv_threshold  = 1e-5,
    seed            = 42,

    # Sign convention
    flip_sign       = TRUE,         # orient PC1 so majority loadings are +ve

    # Output
    z_score         = TRUE,
    plot_factor     = TRUE,
    factor_name     = "RISK_INDEX",
    plot_color      = NULL          # override hex color
) {

  # ============================================================
  # 0. Package checks
  # ============================================================
  if (!requireNamespace("pcaMethods", quietly = TRUE))
    stop("Install pcaMethods: BiocManager::install('pcaMethods')")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2: install.packages('ggplot2')")
  if (stationarity_check && !requireNamespace("tseries", quietly = TRUE))
    stop("Install tseries for ADF tests: install.packages('tseries')")

  method <- match.arg(method)

  # ============================================================
  # 1. Variable selection & basic validation
  # ============================================================
  if (is.null(vars)) {
    X_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
  } else {
    missing_vars <- setdiff(vars, names(df))
    if (length(missing_vars))
      stop("Variables not found in df: ", paste(missing_vars, collapse = ", "))
    X_df <- df[, vars, drop = FALSE]
  }

  if (ncol(X_df) == 0L)
    stop("No numeric columns found / selected.")
  if (nrow(X_df) < 3L)
    stop("At least 3 rows are required.")

  # Remove zero-variance columns (degenerate for any PCA variant)
  col_sds_raw <- apply(X_df, 2, sd, na.rm = TRUE)
  zero_var    <- col_sds_raw == 0 | is.na(col_sds_raw)
  if (any(zero_var)) {
    warning("Dropping zero-variance columns: ",
            paste(names(X_df)[zero_var], collapse = ", "))
    X_df <- X_df[, !zero_var, drop = FALSE]
  }
  if (ncol(X_df) == 0L)
    stop("All columns have zero variance after filtering.")

  # ============================================================
  # 2. Stationarity check + automatic differencing
  #
  #  Strategy:
  #    - Run ADF (augmented Dickey-Fuller) on each column using observed
  #      (non-missing) values.
  #    - If unit root is not rejected at adf_pval, difference the column
  #      and retest. Repeat up to max_diff times.
  #    - IMPORTANT: always diff the *original* series by order d in a single
  #      diff(..., differences = d) call. This guarantees output length is
  #      always n - d and padding is exactly d rows — avoids the cumulative
  #      length drift that occurs when iteratively diffing an already-
  #      differenced series and padding d NAs each iteration.
  #    - Pad the d leading NAs introduced by differencing with NA or zero
  #      depending on diff_pad — NA is recommended so that ppca EM handles
  #      them correctly; "zero" is provided for svd-based methods.
  #    - Record differencing orders applied for the audit trail.
  #    - ADF is run with automatic lag selection (tseries default: k chosen
  #      by Akaike via the Schwert rule).
  #    - Columns that are still non-stationary after max_diff differences
  #      receive a warning but are kept — the analyst should inspect them.
  # ============================================================
  diff_orders <- setNames(integer(ncol(X_df)), names(X_df))  # audit trail

  .adf_stationary <- function(x, pval) {
    # Returns TRUE if ADF rejects the unit root null at pval
    obs <- x[!is.na(x)]
    if (length(obs) < 10L) {
      warning("Too few observations for ADF test; assuming stationary.")
      return(TRUE)
    }
    tryCatch({
      result <- tseries::adf.test(obs, alternative = "stationary")
      result$p.value < pval
    }, error = function(e) {
      warning("ADF test failed for a column (", conditionMessage(e),
              "); assuming stationary.")
      TRUE
    })
  }

  if (stationarity_check) {
    message("\n--- Stationarity screening (ADF, p < ", adf_pval, ") ---")

    for (col in names(X_df)) {
      x_orig     <- X_df[[col]]   # keep original for clean re-diff each iteration
      d          <- 0L
      x          <- x_orig
      stationary <- .adf_stationary(x, adf_pval)

      while (!stationary && d < max_diff) {
        d <- d + 1L

        # Always diff the ORIGINAL series by current order d in one call.
        # Output length = n - d; pad = exactly d rows. No length drift.
        x_diff <- diff(x_orig, differences = d)

        x <- if (diff_pad == "na") {
          c(rep(NA_real_, d), x_diff)
        } else {
          c(rep(0, d), x_diff)
        }

        stationary <- .adf_stationary(x, adf_pval)
      }

      if (!stationary) {
        warning(sprintf(
          "Column '%s' is still non-stationary after %d difference(s). Kept as-is.",
          col, d
        ))
      }

      if (d > 0L) {
        message(sprintf(
          "  %-30s differenced %d time(s) — now %s",
          col, d,
          if (stationary) "stationary" else "NON-STATIONARY (kept)"
        ))
        X_df[[col]]      <- x
        diff_orders[col] <- d
      } else {
        message(sprintf("  %-30s stationary (d = 0)", col))
      }
    }

    n_diffed <- sum(diff_orders > 0L)
    message(sprintf(
      "--- %d / %d column(s) required differencing ---\n",
      n_diffed, ncol(X_df)
    ))

    # After differencing, re-check for zero-variance (diff of constant = 0)
    col_sds_post <- apply(X_df, 2, sd, na.rm = TRUE)
    new_zero     <- col_sds_post == 0 | is.na(col_sds_post)
    if (any(new_zero)) {
      warning("Dropping columns with zero variance after differencing: ",
              paste(names(X_df)[new_zero], collapse = ", "))
      diff_orders <- diff_orders[!new_zero]
      X_df        <- X_df[, !new_zero, drop = FALSE]
    }
    if (ncol(X_df) == 0L)
      stop("All columns have zero variance after differencing.")
  }

  X <- as.matrix(X_df)

  # ============================================================
  # 3. n_factors validation
  #    PPCA requires nPcs < ncol (needs residual dims for noise var σ²)
  #    SVD-based methods allow nPcs == min(n,p)
  # ============================================================
  ppca_methods <- c("ppca", "bpca", "nipals")
  max_factors  <- if (method %in% ppca_methods) {
    min(nrow(X), ncol(X)) - 1L
  } else {
    min(nrow(X), ncol(X))
  }

  if (n_factors < 1L)
    stop("n_factors must be >= 1.")
  if (n_factors > max_factors)
    stop(sprintf(
      "n_factors (%d) exceeds maximum allowed for method '%s' (%d). Reduce n_factors.",
      n_factors, method, max_factors))

  # ============================================================
  # 4. Missing data handling
  #
  #  Strategy:
  #    - ppca / bpca / nipals: EM handles missingness internally.
  #      We do NOT impute beforehand (avoids double-imputation bias).
  #      Centering/scaling uses observed values (na.rm=TRUE) — this is the
  #      best available approximation pre-EM; EM then re-estimates.
  #    - svd / svdImpute: require complete data; apply mean imputation if
  #      impute_pre = TRUE, otherwise error on missingness.
  # ============================================================
  has_na <- anyNA(X)

  if (has_na) {
    na_frac <- mean(is.na(X))
    message(sprintf("Missing data detected: %.1f%% of cells.", na_frac * 100))

    if (method %in% c("svd", "svdImpute")) {
      if (!impute_pre)
        stop(paste0(
          "Method '", method, "' requires complete data. ",
          "Set impute_pre = TRUE for column-mean imputation, ",
          "or switch to method = 'ppca' which handles NAs via EM."
        ))
      # Mean-impute column-wise
      for (j in seq_len(ncol(X))) {
        nas <- is.na(X[, j])
        if (any(nas)) X[nas, j] <- mean(X[, j], na.rm = TRUE)
      }
      message("Applied column-mean imputation for method '", method, "'.")
    }
    # For ppca / bpca / nipals: pass X with NAs directly to pcaMethods
  }

  # ============================================================
  # 5. Centering & scaling
  #    Store params for downstream inversion / interpretation
  # ============================================================
  col_means <- colMeans(X, na.rm = TRUE)
  col_sds   <- apply(X, 2, sd, na.rm = TRUE)
  col_sds[col_sds == 0 | is.na(col_sds)] <- 1  # safety (already filtered above)

  X_scaled <- X
  if (center) X_scaled <- sweep(X_scaled, 2, col_means, "-")
  if (scale)  X_scaled <- sweep(X_scaled, 2, col_sds,   "/")

  # ============================================================
  # 6. Run PCA / PPCA
  #    Pass EM tuning params where supported; set seed for reproducibility
  # ============================================================
  set.seed(seed)

  fit <- tryCatch(
    pcaMethods::pca(
      X_scaled,
      method    = method,
      nPcs      = n_factors,
      center    = FALSE,   # already done above
      scale     = "none",  # already done above
      # pcaMethods accepts these for iterative methods:
      maxIterations = max_iter,
      threshold     = conv_threshold
    ),
    error = function(e)
      stop("pcaMethods::pca() failed: ", conditionMessage(e))
  )

  # ============================================================
  # 7. Convergence check (EM methods expose iteration metadata)
  # ============================================================
  if (method %in% ppca_methods) {
    r2 <- tryCatch(fit@R2, error = function(e) NULL)
    if (!is.null(r2) && any(is.na(r2) | r2 < 0)) {
      warning(
        "EM convergence may be suspect: R² contains NA or negative values. ",
        "Consider increasing max_iter (currently ", max_iter, ") or ",
        "reducing n_factors."
      )
    }
    sigma2 <- tryCatch(fit@sDev[length(fit@sDev)]^2, error = function(e) NULL)
    if (!is.null(sigma2) && sigma2 < .Machine$double.eps * 100) {
      warning(
        "Estimated noise variance (σ²) is effectively zero. ",
        "The PPCA solution may be degenerate — check for near-collinear inputs."
      )
    }
  }

  # ============================================================
  # 8. Extract loadings & scores; verify row alignment
  # ============================================================
  L <- pcaMethods::loadings(fit)   # p × nPcs
  S <- pcaMethods::scores(fit)     # n × nPcs

  if (nrow(S) != nrow(df))
    stop(sprintf(
      paste0("PCA scores have %d rows but input data has %d rows. ",
             "Method '%s' may have dropped rows with excessive NAs. ",
             "Inspect missingness or switch to method = 'ppca'."),
      nrow(S), nrow(df), method
    ))

  colnames(L) <- paste0("PC", seq_len(n_factors))
  colnames(S) <- paste0("PC", seq_len(n_factors))

  # ============================================================
  # 9. Sign convention — orient so majority of PC1 loadings are positive
  #    Ensures risk index is consistently directional across runs
  # ============================================================
  if (flip_sign) {
    for (k in seq_len(n_factors)) {
      if (sum(L[, k] < 0) > sum(L[, k] >= 0)) {
        L[, k] <- -L[, k]
        S[, k] <- -S[, k]
        message(sprintf("PC%d sign flipped: majority of loadings were negative.", k))
      }
    }
  }

  # ============================================================
  # 10. Explained variance
  #    Use only columns that passed the variance filter
  # ============================================================
  eigenvals     <- fit@sDev^2
  total_var     <- sum(apply(X_scaled, 2, var, na.rm = TRUE))
  total_var     <- max(total_var, .Machine$double.eps)  # avoid /0
  explained_var <- eigenvals / total_var

  explained_df <- data.frame(
    factor        = paste0("PC", seq_len(n_factors)),
    eigenvalue    = eigenvals[seq_len(n_factors)],
    explained_var = explained_var[seq_len(n_factors)],
    cumulative    = cumsum(explained_var[seq_len(n_factors)])
  )

  # ============================================================
  # 11. Build scores data.frame; attach dates
  # ============================================================
  scores_df   <- as.data.frame(S)
  loadings_df <- data.frame(variable = rownames(L), as.data.frame(L),
                             row.names = NULL)

  if (!is.null(date_col)) {
    if (!date_col %in% names(df))
      stop("date_col '", date_col, "' not found in df.")
    scores_df[[date_col]] <- df[[date_col]]
  }

  # ============================================================
  # 12. Z-score all PC columns (applied AFTER sign convention)
  #     Note: explained_var reflects pre-z-score structure intentionally
  # ============================================================
  if (z_score) {
    pc_cols <- paste0("PC", seq_len(n_factors))
    scores_df[pc_cols] <- lapply(scores_df[pc_cols], function(col) {
      s <- sd(col, na.rm = TRUE)
      if (is.na(s) || s < .Machine$double.eps) {
        warning("A PC score column has near-zero variance after EM; z-scoring skipped for that column.")
        return(col)
      }
      as.numeric(scale(col))
    })
  }

  # ============================================================
  # 13. Plot
  # ============================================================
  quant_palette <- c(
    STLFSI     = "#1F77B4",
    NFCI       = "#C9A227",
    KCFSI      = "#58508D",
    VIX        = "#A60628",
    SHAPIRO    = "#58508D",
    EPUI       = "#C9A227",
    RISK_INDEX = "#1F77B4"
  )

  line_color <- if (!is.null(plot_color)) {
    plot_color
  } else if (factor_name %in% names(quant_palette)) {
    quant_palette[[factor_name]]
  } else {
    "#1F77B4"
  }

  # NBER recession bands
  recessions <- data.frame(
    start = as.Date(c(
      "1945-02-01", "1948-11-01", "1953-07-01", "1957-08-01",
      "1960-04-01", "1969-12-01", "1973-11-01", "1980-01-01",
      "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01",
      "2020-02-01"
    )),
    end = as.Date(c(
      "1945-10-01", "1949-10-01", "1954-05-01", "1958-04-01",
      "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01",
      "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01",
      "2020-04-01"
    ))
  )

  p <- NULL
  if (plot_factor && !is.null(date_col)) {

    date_min    <- min(scores_df[[date_col]], na.rm = TRUE)
    date_max    <- max(scores_df[[date_col]], na.rm = TRUE)
    rec_clipped <- recessions[recessions$end >= date_min & recessions$start <= date_max, ]
    rec_clipped$start <- pmax(rec_clipped$start, date_min)
    rec_clipped$end   <- pmin(rec_clipped$end,   date_max)

    p <- ggplot2::ggplot(
      scores_df,
      ggplot2::aes(x = .data[[date_col]], y = PC1)
    ) +
      ggplot2::geom_rect(
        data        = rec_clipped,
        inherit.aes = FALSE,
        ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
        fill  = "grey70",
        alpha = 0.6
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          color = "grey60", linewidth = 0.5) +
      ggplot2::geom_line(color = line_color, linewidth = 1) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(
        title    = paste("Latent Factor:", factor_name),
        subtitle = sprintf(
          "Method: %s | PC1 explains %.1f%% of variance | seed = %d",
          toupper(method),
          explained_df$explained_var[1] * 100,
          seed
        ),
        y = if (z_score) "Factor Score (Z-scored)" else "Factor Score",
        x = NULL
      ) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold"),
        plot.subtitle    = ggplot2::element_text(color = "grey40", size = 10),
        panel.grid.minor = ggplot2::element_blank()
      )
    print(p)
  }

  # ============================================================
  # 14. Return
  # ============================================================
  return(invisible(list(
    model         = fit,
    loadings      = loadings_df,
    scores        = scores_df,
    explained     = explained_df,
    center_params = list(means = col_means, sds = col_sds),
    dropped_cols  = names(X_df)[zero_var],   # audit trail
    diff_orders   = diff_orders,             # named int vec: d per column
    convergence   = list(
      r2     = tryCatch(fit@R2,   error = function(e) NULL),
      method = method,
      seed   = seed
    ),
    plot = p
  )))
}
