# =============================================================================
# bvar_risk_channel — Hierarchical Sign-Restricted BVAR for Risk Channel Analysis
# =============================================================================
# Dependencies : BVAR, vars, ggplot2, zoo, tseries, dplyr, patchwork
# Author notes : Designed for plug-and-run pipeline (loop-friendly) AND
#                granular single-run exploration. Mirrors pca_risk_index style.
#
# Architecture:
#   [Sector DFs + Risk/MP DF] -> merge -> stationarity -> BVAR estimation
#   -> sign restriction validation -> IRF extraction -> plotting
#
# Two identification schemes per model:
#   (1) Sign restrictions on shock column 1 (risk/MP shock)
#   (2) Cholesky (recursive) ordering as fallback / comparison
# =============================================================================

bvar_risk_channel <- function(

    # =========================================================================
    # INPUT DATA
    # =========================================================================
    sector_dfs,                       # named list of data.frames
    risk_mp_df,                       # data.frame with risk indices + MP var
    time_col        = "Quarter",      # shared time column name across all inputs

    # =========================================================================
    # VARIABLE SELECTION
    # =========================================================================
    mp_var          = NULL,
    risk_var        = NULL,
    sector_vars     = NULL,           # named list: list(FDIC = c("v1","v2"), ...)
    var_order       = NULL,           # explicit VAR ordering; NULL = mp, risk, sectors

    # =========================================================================
    # SAMPLE SELECTION
    # =========================================================================
    start_date      = NULL,
    end_date        = NULL,

    # =========================================================================
    # STATIONARITY
    # =========================================================================
    stationarity_check = TRUE,
    adf_pval        = 0.05,
    max_diff        = 2L,
    transform_hint  = NULL,           # named list: list(VIX = "dlog", Ratio = "diff", X = "level")
    ratio_patterns  = c("ratio", "share", "margin", "percent", "pct",
                        "Buffer", "Cushion", "Funding", "Leverage",
                        "AOCI", "WholeSale", "Retail"),

    # =========================================================================
    # BVAR ESTIMATION
    # =========================================================================
    lags            = 2L,
    n_draw          = 20000L,
    n_burn          = 7000L,
    n_thin          = 5L,
    horizon         = 24L,
    seed            = 42L,

    # =========================================================================
    # IDENTIFICATION — SIGN RESTRICTIONS
    # =========================================================================
    sign_restr      = NULL,           # named list for shock 1: list(VIX = 1, Shadow = -1, ...)
                                      # use raw variable names — suffixes resolved automatically
                                      # every variable in the VAR must have a sign (0 = unrestricted)
    sign_lim        = 2e6L,
    sign_strict     = TRUE,
    try_scaled      = TRUE,
    try_orientations = TRUE,

    # =========================================================================
    # IDENTIFICATION — CHOLESKY
    # =========================================================================
    run_cholesky    = TRUE,
    cholesky_order  = NULL,

    # =========================================================================
    # IRF SELECTION
    # =========================================================================
    irf_pairs       = NULL,
    irf_all         = FALSE,

    # =========================================================================
    # PLOTTING
    # =========================================================================
    plot            = TRUE,
    plot_theme      = "minimal",
    line_col        = "#111827",
    band_col        = "#60A5FA",
    band_edge       = "#1E3A8A",
    band_alpha      = 0.18,
    base_size       = 13,
    ncol_facet      = 2,
    free_y          = TRUE,
    wrap_width      = 18,
    title_prefix    = NULL,

    # =========================================================================
    # OUTPUT CONTROL
    # =========================================================================
    verbose         = TRUE,
    return_data     = TRUE
) {

  # ===========================================================================
  # 0. DEPENDENCY CHECK
  # ===========================================================================
  for (pkg in c("BVAR", "vars", "ggplot2", "zoo", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Required package '%s' not installed.", pkg))
  }
  if (stationarity_check && !requireNamespace("tseries", quietly = TRUE))
    stop("Install tseries for ADF tests: install.packages('tseries')")

  # ===========================================================================
  # 1. INPUT VALIDATION
  # ===========================================================================
  if (!is.list(sector_dfs) || is.data.frame(sector_dfs))
    stop("sector_dfs must be a named list of data.frames.")
  if (is.null(names(sector_dfs)) || any(names(sector_dfs) == ""))
    stop("sector_dfs must be a NAMED list (e.g. list(FDIC = df1, Shadow = df2)).")
  if (!is.data.frame(risk_mp_df))
    stop("risk_mp_df must be a data.frame.")
  for (nm in names(sector_dfs)) {
    if (!time_col %in% names(sector_dfs[[nm]]))
      stop(sprintf("time_col '%s' not found in sector_dfs$%s", time_col, nm))
  }
  if (!time_col %in% names(risk_mp_df))
    stop(sprintf("time_col '%s' not found in risk_mp_df", time_col))

  # ===========================================================================
  # 2. INTERNAL HELPERS
  # ===========================================================================

  .parse_time <- function(x) {
    if (inherits(x, "Date"))    return(x)
    if (inherits(x, "POSIXt"))  return(as.Date(x))
    if (inherits(x, "yearqtr")) return(as.Date(x))
    if (inherits(x, "yearmon")) return(as.Date(x))
    xc <- as.character(x)
    td <- try(as.Date(xc), silent = TRUE)
    if (!inherits(td, "try-error") && mean(!is.na(td)) > 0.9) return(td)
    for (fmt in c("%Y Q%q", "%Y-Q%q", "%Y:Q%q", "Q%q %Y", "%qQ%Y", "%YQ%q")) {
      yq <- try(zoo::as.yearqtr(xc, format = fmt), silent = TRUE)
      if (!inherits(yq, "try-error")) return(as.Date(yq))
    }
    ym <- try(zoo::as.yearmon(xc), silent = TRUE)
    if (!inherits(ym, "try-error")) return(as.Date(ym))
    as.Date(zoo::as.yearqtr(seq_along(xc)))
  }

  .to_numeric <- function(x) {
    if (is.numeric(x)) return(x)
    x <- trimws(as.character(x))
    has_dc <- grepl("\\d,\\d", x)
    x[has_dc] <- gsub("\\.", "", x[has_dc])
    x[has_dc] <- gsub(",", ".", x[has_dc])
    x <- gsub("[^0-9eE+\\-\\.]", "", x)
    suppressWarnings(as.numeric(x))
  }

  .is_ratio <- function(nm) {
    any(vapply(ratio_patterns, function(pat) grepl(pat, nm, ignore.case = TRUE), logical(1)))
  }

  .adf_stationary <- function(x, pval) {
    obs <- x[!is.na(x)]
    if (length(obs) < 10L) return(TRUE)
    tryCatch(
      tseries::adf.test(obs, alternative = "stationary")$p.value < pval,
      error = function(e) TRUE
    )
  }

  .pretty_one <- function(nm) {
    base <- nm; sf <- ""
    if (grepl("(_dlog|_logdiff)$", base, TRUE)) { base <- sub("(_dlog|_logdiff)$", "", base, TRUE); sf <- " (\u0394log)" }
    else if (grepl("(_diff|_d1)$", base, TRUE)) { base <- sub("(_diff|_d1)$",       "", base, TRUE); sf <- " (\u0394)"    }
    else if (grepl("(_lvl|_level)$", base, TRUE)){ base <- sub("(_lvl|_level)$",    "", base, TRUE); sf <- " (level)"    }
    base <- gsub("_", " ", base)
    base <- gsub("\\bnum\\b", "Number", base, TRUE)
    base <- gsub("\\bpc1\\b", "PC1",    base, TRUE)
    paste0(tools::toTitleCase(base), sf)
  }

  .pretty_names <- function(nms) {
    out <- vapply(nms, .pretty_one, "", USE.NAMES = FALSE)
    out <- vapply(out, function(s) paste(strwrap(s, wrap_width), collapse = "\n"), "")
    names(out) <- nms; out
  }

  # ===========================================================================
  # 3. MERGE ALL DATAFRAMES ON TIME COLUMN
  # ===========================================================================
  if (verbose) message("\n=== STAGE 1: Merging data ===")

  .normalize_time_df <- function(df) {
    df[[time_col]] <- as.Date(zoo::as.yearqtr(.parse_time(df[[time_col]])))
    df
  }

  merged <- .normalize_time_df(risk_mp_df)

  for (nm in names(sector_dfs)) {
    sdf <- .normalize_time_df(sector_dfs[[nm]])
    common <- intersect(setdiff(names(merged), time_col), setdiff(names(sdf), time_col))
    if (length(common) > 0) {
      if (verbose) message(sprintf("  Renaming overlapping columns from '%s': %s", nm, paste(common, collapse = ", ")))
      idx <- match(common, names(sdf))
      names(sdf)[idx] <- paste0(names(sdf)[idx], "_", nm)
    }
    merged <- dplyr::full_join(merged, sdf, by = time_col) |>
      dplyr::arrange(.data[[time_col]])                    |>
      dplyr::distinct(.data[[time_col]], .keep_all = TRUE)
  }

  if (verbose) message(sprintf("  Merged panel: %d obs x %d cols", nrow(merged), ncol(merged)))

  # ===========================================================================
  # 4. SAMPLE TRIMMING
  # ===========================================================================
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    merged <- merged[merged[[time_col]] >= start_date, , drop = FALSE]
  }
  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
    merged <- merged[merged[[time_col]] <= end_date, , drop = FALSE]
  }
  if (verbose) message(sprintf("  After trimming: %d obs (%s to %s)",
                               nrow(merged),
                               format(min(merged[[time_col]]), "%Y-Q%q"),
                               format(max(merged[[time_col]]), "%Y-Q%q")))

  # ===========================================================================
  # 5. STATIONARITY + TRANSFORMATION
  # ===========================================================================
  if (verbose) message("\n=== STAGE 2: Stationarity & transformation ===")

  time_vec <- merged[[time_col]]
  num_cols <- setdiff(names(merged), time_col)
  X <- as.data.frame(lapply(merged[, num_cols, drop = FALSE], .to_numeric), check.names = FALSE)

  transforms  <- setNames(character(ncol(X)), names(X))
  diff_orders <- setNames(integer(ncol(X)),   names(X))
  new_names   <- character(ncol(X))

  for (j in seq_len(ncol(X))) {
    col_name <- names(X)[j]
    x        <- X[[j]]

    if (!is.null(transform_hint) && col_name %in% names(transform_hint)) {
      choice <- match.arg(transform_hint[[col_name]], c("dlog", "diff", "level"))
      transforms[j] <- choice
      if (verbose) message(sprintf("  %-30s forced: %s", col_name, choice))
    } else if (.is_ratio(col_name)) {
      transforms[j] <- "diff"
      if (verbose) message(sprintf("  %-30s heuristic: diff (ratio pattern)", col_name))
    } else if (all(x[is.finite(x)] > 0, na.rm = TRUE)) {
      transforms[j] <- "dlog"
      if (verbose) message(sprintf("  %-30s heuristic: dlog (positive levels)", col_name))
    } else {
      transforms[j] <- "diff"
      if (verbose) message(sprintf("  %-30s heuristic: diff (mixed/negative)", col_name))
    }

    tr <- transforms[j]
    if (tr == "dlog") {
      X[[j]]      <- c(NA, diff(log(x), 1L))
      new_names[j] <- paste0(col_name, "_dlog")
      diff_orders[j] <- 1L
    } else if (tr == "diff") {
      X[[j]]      <- c(NA, diff(x, 1L))
      new_names[j] <- paste0(col_name, "_diff")
      diff_orders[j] <- 1L
    } else {
      new_names[j]   <- paste0(col_name, "_lvl")
      diff_orders[j] <- 0L
    }
  }

  max_lag_diff <- max(diff_orders, na.rm = TRUE)
  if (max_lag_diff > 0L) {
    X        <- X[(max_lag_diff + 1):nrow(X), , drop = FALSE]
    time_vec <- time_vec[(max_lag_diff + 1):length(time_vec)]
  }
  names(X) <- new_names

  if (stationarity_check) {
    if (verbose) message("\n  ADF verification on transformed series:")
    for (j in seq_len(ncol(X))) {
      is_stat <- .adf_stationary(X[[j]], adf_pval)
      status  <- if (is_stat) "PASS" else "WARN: non-stationary"
      if (verbose) message(sprintf("    %-30s %s", names(X)[j], status))
      if (!is_stat && diff_orders[j] < max_diff) {
        X[[j]]       <- c(NA, diff(X[[j]], 1L))
        old_nm       <- names(X)[j]
        names(X)[j]  <- sub("_(dlog|diff|lvl)$", "_d2", old_nm)
        diff_orders[j] <- diff_orders[j] + 1L
        if (verbose) message(sprintf("    %-30s -> differenced again (d=%d)", names(X)[j], diff_orders[j]))
      }
    }
  }

  col_sds   <- vapply(X, function(z) { zf <- z[is.finite(z)]; if (length(zf) < 5) 0 else sd(zf) }, numeric(1))
  drop_mask <- col_sds < 1e-8
  if (any(drop_mask)) {
    if (verbose) message("  Dropping near-constant: ", paste(names(X)[drop_mask], collapse = ", "))
    X <- X[, !drop_mask, drop = FALSE]
  }

  stationary_df <- data.frame(Quarter = time_vec, X, check.names = FALSE)
  stationary_df <- stationary_df[complete.cases(stationary_df), , drop = FALSE]
  time_vec      <- stationary_df$Quarter

  if (verbose) message(sprintf("  Final panel: %d obs x %d vars", nrow(stationary_df), ncol(X)))

  # ===========================================================================
  # 6. RESOLVE VARIABLE NAMES (post-transformation suffixes)
  # ===========================================================================
  all_transformed <- setdiff(names(stationary_df), "Quarter")

  .find_transformed <- function(raw_name) {
    if (raw_name %in% all_transformed) return(raw_name)
    candidates <- paste0(raw_name, c("_dlog", "_diff", "_lvl", "_d2"))
    found <- candidates[candidates %in% all_transformed]
    if (length(found) == 1L) return(found)
    if (length(found) > 1L) return(found[1])
    hits <- grep(paste0("^", raw_name), all_transformed, value = TRUE)
    if (length(hits) >= 1L) return(hits[1])
    NULL
  }

  mp_resolved   <- if (!is.null(mp_var))   .find_transformed(mp_var)   else NULL
  risk_resolved <- if (!is.null(risk_var)) .find_transformed(risk_var) else NULL

  sector_resolved <- list()
  if (!is.null(sector_vars)) {
    for (nm in names(sector_vars)) {
      sector_resolved[[nm]] <- vapply(sector_vars[[nm]], function(v) {
        r <- .find_transformed(v)
        if (is.null(r)) { warning(sprintf("Variable '%s' from sector '%s' not found after transform.", v, nm)); NA_character_ }
        else r
      }, character(1))
      sector_resolved[[nm]] <- sector_resolved[[nm]][!is.na(sector_resolved[[nm]])]
    }
  } else {
    for (nm in names(sector_dfs)) {
      raw_cols <- setdiff(names(sector_dfs[[nm]]), time_col)
      resolved <- vapply(raw_cols, function(v) {
        r <- .find_transformed(v); if (is.null(r)) NA_character_ else r
      }, character(1))
      sector_resolved[[nm]] <- resolved[!is.na(resolved)]
    }
  }

  all_sector_vars <- unique(unlist(sector_resolved, use.names = FALSE))

  if (verbose) {
    message("\n=== Variable resolution ===")
    if (!is.null(mp_resolved))   message(sprintf("  MP variable  : %s", mp_resolved))
    if (!is.null(risk_resolved)) message(sprintf("  Risk variable: %s", risk_resolved))
    for (nm in names(sector_resolved))
      message(sprintf("  Sector %-10s: %s", nm, paste(sector_resolved[[nm]], collapse = ", ")))
  }

  # ===========================================================================
  # 7. BUILD VAR ORDERING
  # ===========================================================================
  if (!is.null(var_order)) {
    var_order_resolved <- vapply(var_order, function(v) {
      r <- .find_transformed(v); if (is.null(r)) v else r
    }, character(1))
    use_vars <- var_order_resolved[var_order_resolved %in% all_transformed]
  } else {
    use_vars <- unique(c(mp_resolved, risk_resolved, all_sector_vars))
    use_vars <- use_vars[use_vars %in% all_transformed]
  }

  if (length(use_vars) < 2L)
    stop("Need at least 2 variables for VAR estimation. Found: ", paste(use_vars, collapse = ", "))

  Y <- as.matrix(stationary_df[, use_vars, drop = FALSE])
  k <- ncol(Y)

  if (verbose) {
    message(sprintf("\n=== STAGE 3: BVAR estimation ==="))
    message(sprintf("  Variables (%d): %s", k, paste(use_vars, collapse = ", ")))
    message(sprintf("  Observations  : %d", nrow(Y)))
  }

  # ===========================================================================
  # 8. LAG SELECTION
  # ===========================================================================
  if (is.character(lags) && lags == "auto") {
    vs   <- vars::VARselect(Y)
    lags <- as.integer(vs$selection["AIC(n)"])
    lags <- max(1L, min(lags, 4L))
    if (verbose) message(sprintf("  Auto lag selection (AIC): p = %d", lags))
  } else {
    lags <- as.integer(lags)
    if (verbose) message(sprintf("  Lag order: p = %d", lags))
  }
  p <- lags

  # ===========================================================================
  # 9. SIGN RESTRICTION MATRIX
  # ===========================================================================
  # Builds a K x K matrix.
  # Column 1 = identified MP/risk shock:
  #   +1 / -1 for restricted variables (from sign_restr list)
  #    0      for unrestricted variables (participates but no sign requirement)
  # Columns 2:K = NA (not identified via sign restrictions)

  .build_SR <- function(vars, k, signs_list) {
    SR <- matrix(NA_real_, nrow = k, ncol = k,
                 dimnames = list(vars, paste0("shock", seq_len(k))))

    if (!is.null(signs_list) && length(signs_list) > 0) {
      for (nm in names(signs_list)) {
        resolved <- .find_transformed(nm)
        if (is.null(resolved)) resolved <- nm
        if (resolved %in% vars && !is.na(signs_list[[nm]])) {
          SR[resolved, 1L] <- signs_list[[nm]]
        }
      }
    }

    # fill remaining NAs in col 1 with 0 so BVAR sees a fully specified
    # first shock (0 = unrestricted participant, not excluded)
    col1_restricted <- !is.na(SR[, 1L])
    if (any(col1_restricted)) {
      SR[!col1_restricted, 1L] <- 0
    }

    SR
  }

  SR          <- .build_SR(use_vars, k, sign_restr)
  SR_original <- SR

  # ===========================================================================
  # 10. SIGN-RESTRICTED BVAR
  # ===========================================================================
  sign_result <- NULL
  sign_report <- NULL

  if (!is.null(sign_restr) && sum(SR[, 1L] != 0, na.rm = TRUE) > 0) {

    if (verbose) {
      message("\n  Sign restriction matrix (shock 1):")
      for (i in seq_len(k)) {
        v <- use_vars[i]
        s <- SR[v, 1L]
        lbl <- if (is.na(s)) "NA" else if (s > 0) "+" else if (s < 0) "-" else "0 (unrestricted)"
        message(sprintf("    %-30s %s", v, lbl))
      }
    }

    .try_sign_bvar <- function(data_mat, SR_mat, lim) {
      fit <- BVAR::bvar(
        data    = data_mat,
        lags    = p,
        n_draw  = n_draw,
        n_burn  = n_burn,
        n_thin  = n_thin,
        irf     = BVAR::bv_irf(horizon = horizon, sign_restr = SR_mat, sign_lim = lim),
        verbose = FALSE
      )
      ir <- BVAR::irf(fit)
      list(fit = fit, irfs = ir)
    }

    X_list <- list(unscaled = Y)
    if (try_scaled) X_list$scaled <- scale(Y)

    SR_list <- list(SR_pos = SR)
    if (try_orientations) {
      SR_neg  <- SR; SR_neg[, 1L] <- -SR[, 1L]
      SR_list <- list(SR_pos = SR, SR_neg = SR_neg)
    }

    lims <- unique(c(sign_lim, 1e7, 5e7))

    # build flat attempt grid for progress bar
    attempts <- expand.grid(
      xnm  = names(X_list),
      srnm = names(SR_list),
      lim  = lims,
      stringsAsFactors = FALSE
    )
    n_attempts <- nrow(attempts)

    set.seed(seed)
    found  <- FALSE
    bar_w  <- 30L
    spin   <- c("|", "/", "-", "\\")

    for (ai in seq_len(n_attempts)) {
      if (found) break

      xnm  <- attempts$xnm[ai]
      srnm <- attempts$srnm[ai]
      lim  <- attempts$lim[ai]

      # --- progress bar ---
      filled <- round(bar_w * ai / n_attempts)
      pct    <- round(ai / n_attempts * 100)
      bar    <- paste0(
        "[", strrep("=", filled), strrep(" ", bar_w - filled), "]",
        sprintf(" %3d%%  attempt %d/%d  (%s / %s / lim=%.0e)",
                pct, ai, n_attempts, xnm, srnm, lim)
      )
      cat(sprintf("\r  [sign-ID] %s", bar))
      flush.console()

      out <- try(.try_sign_bvar(X_list[[xnm]], SR_list[[srnm]], lim), silent = TRUE)

      if (!inherits(out, "try-error")) {
        cat(sprintf("\n  [sign-ID] CONVERGED: %s / %s / lim=%.0e\n", xnm, srnm, lim))
        sign_result             <- out
        sign_result$scaled      <- (xnm == "scaled")
        sign_result$orientation <- srnm
        sign_result$SR_used     <- SR_list[[srnm]]
        sign_result$sign_lim    <- lim
        found <- TRUE
      }
    }
    if (!found) cat("\n")

    if (!found) {
      if (sign_strict) {
        stop("Sign restrictions could not be satisfied. Try sign_strict = FALSE to relax.")
      } else {
        if (verbose) message("  [sign-ID] FAILED: relaxing all sign restrictions")
        SR[, 1L] <- NA_real_
        sign_report <- data.frame(
          variable  = rownames(SR_original),
          requested = SR_original[, 1L],
          status    = ifelse(SR_original[, 1L] == 0, "unrestricted",
                      ifelse(is.na(SR_original[, 1L]), "unconstrained", "FAILED")),
          stringsAsFactors = FALSE
        )
      }
    } else {
      # validate impact signs at h=0
      .irf_quants_to_df <- function(irfs_obj, shock = 1L) {
        Q      <- irfs_obj$quants
        dd     <- dim(Q)
        K <- dd[2]; H <- dd[3]
        if (is.character(shock)) shock <- as.integer(sub("\\D+", "", shock))
        med    <- Q[2, , , shock]
        vars_nm <- irfs_obj$variables
        if (is.null(vars_nm) || length(vars_nm) != K) vars_nm <- paste0("y", seq_len(K))
        do.call(rbind, lapply(seq_len(K), function(j) {
          data.frame(horizon = 0:(H - 1), variable = vars_nm[j],
                     median  = as.numeric(med[j, ]),
                     lower   = as.numeric(Q[1, j, , shock]),
                     upper   = as.numeric(Q[3, j, , shock]),
                     stringsAsFactors = FALSE)
        }))
      }

      df_h0 <- .irf_quants_to_df(sign_result$irfs, shock = 1L)
      df_h0 <- df_h0[df_h0$horizon == 0L, ]
      want  <- sign_result$SR_used[, 1L]
      want  <- want[!is.na(want) & want != 0]

      impact_ok <- data.frame(
        variable  = names(want),
        target    = want,
        median    = df_h0$median[match(names(want), df_h0$variable)],
        stringsAsFactors = FALSE
      )
      impact_ok$satisfied <- sign(impact_ok$median) == sign(impact_ok$target)

      if (!all(impact_ok$satisfied) && !sign_strict) {
        failed_vars <- impact_ok$variable[!impact_ok$satisfied]
        if (verbose) message("  [sign-ID] Relaxing unsatisfied: ", paste(failed_vars, collapse = ", "))
        for (fv in failed_vars) SR[fv, 1L] <- NA_real_
      }

      sign_report <- impact_ok
      if (verbose) {
        message("\n  Impact sign check (h=0):")
        print(impact_ok, row.names = FALSE)
      }
    }
  }

  # ===========================================================================
  # 11. CHOLESKY-IDENTIFIED BVAR
  # ===========================================================================
  cholesky_result <- NULL

  if (run_cholesky) {
    if (verbose) message("\n  Running Cholesky-identified BVAR...")

    chol_order <- if (!is.null(cholesky_order)) {
      vapply(cholesky_order, function(v) { r <- .find_transformed(v); if (is.null(r)) v else r }, character(1))
    } else {
      use_vars
    }
    chol_order <- chol_order[chol_order %in% use_vars]
    Y_chol     <- Y[, chol_order, drop = FALSE]

    spin_i <- 0L
    cat("  [Cholesky] estimating BVAR ")
    flush.console()

    set.seed(seed)
    chol_fit <- tryCatch({
      withCallingHandlers(
        BVAR::bvar(
          data    = Y_chol,
          lags    = p,
          n_draw  = n_draw,
          n_burn  = n_burn,
          n_thin  = n_thin,
          irf     = BVAR::bv_irf(horizon = horizon, identification = TRUE),
          verbose = FALSE
        ),
        message = function(m) {
          spin_i <<- (spin_i + 1L) %% 4L
          cat(sprintf("\r  [Cholesky] estimating BVAR %s  %s",
                      c("|", "/", "-", "\\")[spin_i + 1L],
                      trimws(conditionMessage(m))))
          flush.console()
          invokeRestart("muffleMessage")
        }
      )
    }, error = function(e) { warning("Cholesky BVAR failed: ", conditionMessage(e)); NULL })

    cat(sprintf("\r  [Cholesky] done%s\n", strrep(" ", 60)))

    if (!is.null(chol_fit)) {
      chol_irfs       <- BVAR::irf(chol_fit)
      cholesky_result <- list(fit = chol_fit, irfs = chol_irfs, order = chol_order)
      if (verbose) message("  Cholesky BVAR: OK")
    }
  }

  # ===========================================================================
  # 12. COVARIANCE MATRICES
  # ===========================================================================
  .extract_vcov <- function(bvar_fit) {
    tryCatch({
      res <- residuals(bvar_fit)
      if (is.null(res)) return(NULL)
      if (is.list(res) && !is.null(res$res)) res <- res$res
      if (!is.matrix(res)) res <- as.matrix(res)
      crossprod(res) / (nrow(res) - 1)
    }, error = function(e) NULL)
  }

  vcov_sign     <- if (!is.null(sign_result))     .extract_vcov(sign_result$fit)     else NULL
  vcov_cholesky <- if (!is.null(cholesky_result)) .extract_vcov(cholesky_result$fit) else NULL

  # ===========================================================================
  # 13. IRF EXTRACTION
  # ===========================================================================
  .irf_quants_to_df_full <- function(irfs_obj) {
    Q       <- irfs_obj$quants
    dd      <- dim(Q)
    K <- dd[2]; H <- dd[3]; S <- dd[4]
    vars_nm <- irfs_obj$variables
    if (is.null(vars_nm) || length(vars_nm) != K) vars_nm <- paste0("y", seq_len(K))
    do.call(rbind, lapply(seq_len(S), function(s) {
      do.call(rbind, lapply(seq_len(K), function(j) {
        data.frame(
          shock    = vars_nm[s],
          variable = vars_nm[j],
          horizon  = 0:(H - 1),
          median   = as.numeric(Q[2, j, , s]),
          lower    = as.numeric(Q[1, j, , s]),
          upper    = as.numeric(Q[3, j, , s]),
          stringsAsFactors = FALSE
        )
      }))
    }))
  }

  if (is.null(irf_pairs)) {
    irf_pairs <- list()
    if (!is.null(mp_resolved) && !is.null(risk_resolved))
      irf_pairs <- c(irf_pairs, list(list(shock = mp_resolved, response = risk_resolved)))
    if (!is.null(risk_resolved)) {
      for (sv in all_sector_vars)
        irf_pairs <- c(irf_pairs, list(list(shock = risk_resolved, response = sv)))
    }
    if (is.null(risk_resolved) && !is.null(mp_resolved)) {
      for (sv in all_sector_vars)
        irf_pairs <- c(irf_pairs, list(list(shock = mp_resolved, response = sv)))
    }
  } else {
    irf_pairs <- lapply(irf_pairs, function(pr) {
      list(
        shock    = { r <- .find_transformed(pr$shock);    if (is.null(r)) pr$shock    else r },
        response = { r <- .find_transformed(pr$response); if (is.null(r)) pr$response else r }
      )
    })
  }

  irf_df_sign     <- if (!is.null(sign_result))     .irf_quants_to_df_full(sign_result$irfs)     else NULL
  irf_df_cholesky <- if (!is.null(cholesky_result)) .irf_quants_to_df_full(cholesky_result$irfs) else NULL

  # ===========================================================================
  # 14. PLOTTING
  # ===========================================================================
  plots <- list()

  .irf_plot_pair <- function(irf_df, shock_var, resp_var, id_label, subtitle_extra = "") {
    d <- irf_df[irf_df$shock == shock_var & irf_df$variable == resp_var, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    shock_pretty <- .pretty_one(shock_var)
    resp_pretty  <- .pretty_one(resp_var)
    ttl  <- sprintf("%s -> %s", shock_pretty, resp_pretty)
    if (!is.null(title_prefix)) ttl <- paste0(title_prefix, ": ", ttl)
    sub  <- paste0("ID: ", id_label, " | p = ", p, subtitle_extra)
    base_theme <- if (plot_theme == "bw") ggplot2::theme_bw(base_size = base_size) else ggplot2::theme_minimal(base_size = base_size)
    ggplot2::ggplot(d, ggplot2::aes(x = horizon, y = median, ymin = lower, ymax = upper)) +
      ggplot2::geom_ribbon(fill = band_col, alpha = band_alpha) +
      ggplot2::geom_line(ggplot2::aes(y = upper), color = band_edge, linewidth = 0.35) +
      ggplot2::geom_line(ggplot2::aes(y = lower), color = band_edge, linewidth = 0.35) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
      ggplot2::geom_line(linewidth = 0.9, color = line_col) +
      ggplot2::labs(title = ttl, subtitle = sub, x = "Horizon (quarters)", y = "Response") +
      base_theme +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold"),
        plot.subtitle    = ggplot2::element_text(color = "grey40", size = 10),
        strip.text       = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(linewidth = 0.3)
      )
  }

  .irf_plot_faceted <- function(irf_df, shock_var, resp_vars, id_label, subtitle_extra = "") {
    d <- irf_df[irf_df$shock == shock_var & irf_df$variable %in% resp_vars, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    d$variable_pretty <- .pretty_names(d$variable)[d$variable]
    shock_pretty      <- .pretty_one(shock_var)
    ttl  <- sprintf("Responses to %s shock", shock_pretty)
    if (!is.null(title_prefix)) ttl <- paste0(title_prefix, ": ", ttl)
    sub  <- paste0("ID: ", id_label, " | p = ", p, subtitle_extra)
    base_theme <- if (plot_theme == "bw") ggplot2::theme_bw(base_size = base_size) else ggplot2::theme_minimal(base_size = base_size)
    ggplot2::ggplot(d, ggplot2::aes(x = horizon, y = median, ymin = lower, ymax = upper)) +
      ggplot2::geom_ribbon(fill = band_col, alpha = band_alpha) +
      ggplot2::geom_line(ggplot2::aes(y = upper), color = band_edge, linewidth = 0.35) +
      ggplot2::geom_line(ggplot2::aes(y = lower), color = band_edge, linewidth = 0.35) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
      ggplot2::geom_line(linewidth = 0.9, color = line_col) +
      ggplot2::facet_wrap(~ variable_pretty, scales = if (free_y) "free_y" else "fixed", ncol = ncol_facet) +
      ggplot2::labs(title = ttl, subtitle = sub, x = "Horizon (quarters)", y = "Response") +
      base_theme +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold"),
        plot.subtitle    = ggplot2::element_text(color = "grey40", size = 10),
        strip.text       = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(linewidth = 0.3)
      )
  }

  if (plot) {
    if (verbose) message("\n=== STAGE 4: Generating IRF plots ===")
    for (i in seq_along(irf_pairs)) {
      pr <- irf_pairs[[i]]
      if (!is.null(irf_df_sign)) {
        p_sign <- .irf_plot_pair(irf_df_sign, pr$shock, pr$response, "Sign-restricted")
        if (!is.null(p_sign)) { nm <- paste0("sign_", pr$shock, "_to_", pr$response); plots[[nm]] <- p_sign }
      }
      if (!is.null(irf_df_cholesky)) {
        p_chol <- .irf_plot_pair(irf_df_cholesky, pr$shock, pr$response, "Cholesky")
        if (!is.null(p_chol)) { nm <- paste0("chol_", pr$shock, "_to_", pr$response); plots[[nm]] <- p_chol }
      }
    }
    if (!is.null(risk_resolved)) {
      for (sec_nm in names(sector_resolved)) {
        svars <- sector_resolved[[sec_nm]]
        if (length(svars) == 0) next
        if (!is.null(irf_df_sign)) {
          pf <- .irf_plot_faceted(irf_df_sign, risk_resolved, svars, "Sign-restricted", paste0(" | sector: ", sec_nm))
          if (!is.null(pf)) plots[[paste0("sign_facet_", sec_nm)]] <- pf
        }
        if (!is.null(irf_df_cholesky)) {
          pf <- .irf_plot_faceted(irf_df_cholesky, risk_resolved, svars, "Cholesky", paste0(" | sector: ", sec_nm))
          if (!is.null(pf)) plots[[paste0("chol_facet_", sec_nm)]] <- pf
        }
      }
    }
    for (nm in names(plots)) {
      if (verbose) message(sprintf("  Printing: %s", nm))
      print(plots[[nm]])
    }
  }

  # ===========================================================================
  # 15. SUMMARY REPORT
  # ===========================================================================
  if (verbose) {
    cat("\n========== BVAR RISK CHANNEL REPORT ==========\n")
    cat(sprintf("Sectors         : %s\n", paste(names(sector_dfs), collapse = ", ")))
    cat(sprintf("Variables (K)   : %d\n", k))
    cat(sprintf("Observations    : %d\n", nrow(Y)))
    cat(sprintf("Lags (p)        : %d\n", p))
    if (!is.null(mp_resolved))   cat(sprintf("MP variable     : %s\n", mp_resolved))
    if (!is.null(risk_resolved)) cat(sprintf("Risk variable   : %s\n", risk_resolved))
    cat(sprintf("Ordering        : %s\n", paste(use_vars, collapse = " -> ")))
    if (!is.null(sign_result)) {
      cat("\n[Sign-ID]\n")
      cat(sprintf("  Scaled data   : %s\n", if (sign_result$scaled) "YES" else "NO"))
      cat(sprintf("  Orientation   : %s\n", sign_result$orientation))
      cat(sprintf("  sign_lim used : %g\n", sign_result$sign_lim))
      lml <- tryCatch(as.numeric(sign_result$fit$log_ml), error = function(e) NA)
      if (!is.na(lml)) cat(sprintf("  Log ML        : %.4f\n", lml))
    }
    if (!is.null(cholesky_result)) {
      cat("\n[Cholesky]\n")
      cat(sprintf("  Order         : %s\n", paste(cholesky_result$order, collapse = " -> ")))
      lml <- tryCatch(as.numeric(cholesky_result$fit$log_ml), error = function(e) NA)
      if (!is.na(lml)) cat(sprintf("  Log ML        : %.4f\n", lml))
    }
    cat("================================================\n")
  }

  # ===========================================================================
  # 16. RETURN
  # ===========================================================================
  out <- list(
    sign_result     = sign_result,
    cholesky_result = cholesky_result,
    vcov_sign       = vcov_sign,
    vcov_cholesky   = vcov_cholesky,
    irf_df_sign     = irf_df_sign,
    irf_df_cholesky = irf_df_cholesky,
    SR_requested    = SR_original,
    SR_final        = SR,
    sign_report     = sign_report,
    plots           = plots,
    var_map         = list(
      mp_var      = mp_resolved,
      risk_var    = risk_resolved,
      sector_vars = sector_resolved,
      all_vars    = use_vars,
      var_order   = use_vars,
      transforms  = transforms,
      diff_orders = diff_orders
    ),
    spec = list(
      lags    = p,
      horizon = horizon,
      n_draw  = n_draw,
      n_burn  = n_burn,
      n_thin  = n_thin,
      seed    = seed
    )
  )

  if (return_data) {
    out$data <- list(
      merged     = merged,
      stationary = stationary_df,
      Y          = Y,
      time       = time_vec
    )
  }

  return(invisible(out))
}


# =============================================================================
# CONVENIENCE WRAPPERS
# =============================================================================

bvar_rc_irf <- function(res, shock, response, id = c("sign", "cholesky")) {
  id     <- match.arg(id)
  irf_df <- if (id == "sign") res$irf_df_sign else res$irf_df_cholesky
  if (is.null(irf_df)) stop(sprintf("No %s IRFs available.", id))

  .resolve <- function(nm) {
    if (nm %in% unique(irf_df$shock) || nm %in% unique(irf_df$variable)) return(nm)
    candidates <- paste0(nm, c("_dlog", "_diff", "_lvl", "_d2"))
    found <- candidates[candidates %in% unique(c(irf_df$shock, irf_df$variable))]
    if (length(found) >= 1) return(found[1])
    nm
  }

  shock_r    <- .resolve(shock)
  response_r <- .resolve(response)
  d <- irf_df[irf_df$shock == shock_r & irf_df$variable == response_r, , drop = FALSE]
  if (nrow(d) == 0) stop(sprintf("IRF pair not found: %s -> %s", shock_r, response_r))
  d
}

bvar_rc_plot <- function(res, shock, response, id = c("sign", "cholesky"), ...) {
  d  <- bvar_rc_irf(res, shock, response, id)
  id <- match.arg(id)
  ggplot2::ggplot(d, ggplot2::aes(x = horizon, y = median, ymin = lower, ymax = upper)) +
    ggplot2::geom_ribbon(fill = "#60A5FA", alpha = 0.18) +
    ggplot2::geom_line(ggplot2::aes(y = upper), color = "#1E3A8A", linewidth = 0.35) +
    ggplot2::geom_line(ggplot2::aes(y = lower), color = "#1E3A8A", linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.9, color = "#111827") +
    ggplot2::labs(
      title    = sprintf("%s -> %s", shock, response),
      subtitle = sprintf("ID: %s", id),
      x = "Horizon (quarters)", y = "Response"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

bvar_rc_batch <- function(sector_dfs, risk_mp_df, configs, ...) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  results <- list()
  for (cfg in configs) {
    nm   <- cfg$name %||% paste0("model_", length(results) + 1)
    message(sprintf("\n######## BATCH: %s ########", nm))
    args <- cfg[setdiff(names(cfg), "name")]
    if (!"sector_dfs" %in% names(args)) args$sector_dfs <- sector_dfs
    if (!"risk_mp_df" %in% names(args)) args$risk_mp_df <- risk_mp_df
    dots <- list(...)
    for (d in names(dots)) { if (!d %in% names(args)) args[[d]] <- dots[[d]] }
    results[[nm]] <- tryCatch(
      do.call(bvar_risk_channel, args),
      error = function(e) { warning(sprintf("Model '%s' failed: %s", nm, conditionMessage(e))); NULL }
    )
  }
  results
}
