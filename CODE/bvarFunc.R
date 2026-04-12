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
# Three identification schemes:
#   (1) Sign restrictions on multiple shock columns (shock1, shock2, ...)
#   (2) Cholesky (recursive) ordering as fallback / comparison
#   (3) Hybrid: sign-restricted shocks + unrestricted remaining shocks
#
# Sign restriction relaxation:
#   - When sign_strict = FALSE, violated restrictions are individually relaxed
#   - Full reporting of which restrictions held vs. were relaxed
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
    # IDENTIFICATION — SIGN RESTRICTIONS (MULTI-SHOCK)
    # =========================================================================
    # Two formats supported:
    #
    # Format 1 (single shock - backward compatible):
    #   sign_restr = list(VIX = 1, Shadow = -1, GDP = 0)
    #
    # Format 2 (multi-shock):
    #   sign_restr = list(
    #     shock1 = list(VIX = 1, Shadow = -1, GDP = 0),
    #     shock2 = list(VIX = 0, Shadow = 1, Inflation = -1)
    #   )
    #
    # Values: +1 = positive, -1 = negative, 0 = unrestricted (participates but no sign)
    #         NA or missing = unconstrained
    # =========================================================================
    sign_restr      = NULL,
    sign_lim        = 2e6L,
    sign_strict     = TRUE,           # FALSE = relax violated restrictions individually
    try_scaled      = TRUE,
    try_orientations = TRUE,
    sign_horizons   = 0L,             # horizons to check sign restrictions (0 = impact only)

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
  # 9. SIGN RESTRICTION MATRIX — MULTI-SHOCK SUPPORT
  # ===========================================================================
  # Builds a K x K matrix where:
  #   - Each column represents a shock
  #   - +1/-1 = sign restricted, 0 = unrestricted (participates), NA = unconstrained
  #
  # Input formats:
  #   Format 1 (single shock): list(VIX = 1, GDP = -1)
  #   Format 2 (multi-shock):  list(shock1 = list(...), shock2 = list(...))

  .normalize_sign_restr <- function(sr) {
    # Detect format: if any element is a list, it's multi-shock format
    if (is.null(sr) || length(sr) == 0) return(NULL)
    
    is_multi <- any(vapply(sr, is.list, logical(1)))
    
    if (is_multi) {
      # Already in multi-shock format
      # Ensure shock names are standardized
      shock_names <- names(sr)
      if (is.null(shock_names)) {
        names(sr) <- paste0("shock", seq_along(sr))
      } else {
        # Standardize names like "shock1", "shock_1", "1" -> "shock1"
        names(sr) <- vapply(seq_along(sr), function(i) {
          nm <- shock_names[i]
          if (grepl("^\\d+$", nm)) return(paste0("shock", nm))
          if (grepl("^shock[_]?\\d+$", nm, ignore.case = TRUE)) {
            num <- gsub("\\D", "", nm)
            return(paste0("shock", num))
          }
          nm
        }, character(1))
      }
      return(sr)
    } else {
      # Single-shock format -> convert to multi-shock
      return(list(shock1 = sr))
    }
  }

  .build_SR_multi <- function(vars, k, sign_restr_list) {
    # Initialize matrix: all NA (unconstrained)
    SR <- matrix(NA_real_, nrow = k, ncol = k,
                 dimnames = list(vars, paste0("shock", seq_len(k))))
    
    if (is.null(sign_restr_list) || length(sign_restr_list) == 0) {
      return(SR)
    }
    
    # Normalize to multi-shock format
    sr_normalized <- .normalize_sign_restr(sign_restr_list)
    
    # Process each shock
    for (shock_name in names(sr_normalized)) {
      # Extract shock number
      shock_num <- as.integer(gsub("\\D", "", shock_name))
      if (is.na(shock_num) || shock_num < 1 || shock_num > k) {
        warning(sprintf("Invalid shock identifier '%s', skipping.", shock_name))
        next
      }
      
      shock_signs <- sr_normalized[[shock_name]]
      
      # Apply sign restrictions for this shock
      for (var_name in names(shock_signs)) {
        resolved <- .find_transformed(var_name)
        if (is.null(resolved)) resolved <- var_name
        
        if (resolved %in% vars) {
          sign_val <- shock_signs[[var_name]]
          if (!is.null(sign_val) && !is.na(sign_val)) {
            SR[resolved, shock_num] <- sign_val
          }
        } else {
          warning(sprintf("Variable '%s' not found in VAR variables for %s.", var_name, shock_name))
        }
      }
      
      # Fill remaining NAs in this column with 0 (unrestricted participant)
      # only if at least one restriction was set
      col_restricted <- !is.na(SR[, shock_num])
      if (any(col_restricted)) {
        SR[!col_restricted, shock_num] <- 0
      }
    }
    
    SR
  }

  # Build the sign restriction matrix
  SR          <- .build_SR_multi(use_vars, k, sign_restr)
  SR_original <- SR

  # Count how many shocks have restrictions
  n_restricted_shocks <- sum(apply(SR, 2, function(col) any(!is.na(col) & col != 0)))

  # ===========================================================================
  # 10. SIGN-RESTRICTED BVAR

  # ===========================================================================
  sign_result <- NULL
  sign_report <- NULL
  sign_report_detailed <- NULL

  if (!is.null(sign_restr) && n_restricted_shocks > 0) {

    if (verbose) {
      message("\n  Sign restriction matrix:")
      message(sprintf("  Number of identified shocks: %d", n_restricted_shocks))
      for (s in seq_len(min(n_restricted_shocks, k))) {
        col_signs <- SR[, s]
        if (all(is.na(col_signs))) next
        message(sprintf("\n  Shock %d restrictions:", s))
        for (i in seq_len(k)) {
          v <- use_vars[i]
          sval <- col_signs[v]
          lbl <- if (is.na(sval)) "NA (unconstrained)" 
                 else if (sval > 0) "+" 
                 else if (sval < 0) "-" 
                 else "0 (unrestricted)"
          message(sprintf("    %-30s %s", v, lbl))
        }
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
      
      # Validate IRFs actually computed (sign restrictions satisfied)
      # If sign restrictions failed, quants will be all NA or dims wrong
      Q <- ir$quants
      if (is.null(Q) || all(is.na(Q))) {
        stop("IRF quants are NULL or all NA — sign restrictions not satisfied")
      }
      
      list(fit = fit, irfs = ir)
    }

    X_list <- list(unscaled = Y)
    if (try_scaled) X_list$scaled <- scale(Y)

    SR_list <- list(SR_pos = SR)
    if (try_orientations) {
      # Flip signs for all restricted shocks
      SR_neg <- SR
      for (s in seq_len(n_restricted_shocks)) {
        SR_neg[, s] <- -SR[, s]
      }
      SR_list <- list(SR_pos = SR, SR_neg = SR_neg)
    }

    lims <- unique(c(sign_lim, 1e7, 5e7))

    # Build flat attempt grid for progress bar
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

    for (ai in seq_len(n_attempts)) {
      if (found) break

      xnm  <- attempts$xnm[ai]
      srnm <- attempts$srnm[ai]
      lim  <- attempts$lim[ai]

      # Progress bar
      filled <- round(bar_w * ai / n_attempts)
      pct    <- round(ai / n_attempts * 100)
      bar    <- paste0(
        "[", strrep("=", filled), strrep(" ", bar_w - filled), "]",
        sprintf(" %3d%%  attempt %d/%d  (%s / %s / lim=%.0e)",
                pct, ai, n_attempts, xnm, srnm, lim)
      )
      cat(sprintf("\r  [sign-ID] %s", bar))
      flush.console()

      out <- tryCatch(
        suppressWarnings(.try_sign_bvar(X_list[[xnm]], SR_list[[srnm]], lim)),
        error = function(e) {
          # Capture specific error types for better diagnostics
          msg <- conditionMessage(e)
          attr(e, "sign_error") <- grepl("sign", msg, ignore.case = TRUE) ||
                                   grepl("restriction", msg, ignore.case = TRUE)
          e
        }
      )

      if (!inherits(out, "error") && !inherits(out, "try-error")) {
        cat(sprintf("\n  [sign-ID] CONVERGED: %s / %s / lim=%.0e\n", xnm, srnm, lim))
        sign_result             <- out
        sign_result$scaled      <- (xnm == "scaled")
        sign_result$orientation <- srnm
        sign_result$SR_used     <- SR_list[[srnm]]
        sign_result$sign_lim    <- lim
        found <- TRUE
      }
    }
    if (!found) {
      cat("\n  [sign-ID] No valid rotation found after all attempts\n")
    }

    # =========================================================================
    # 10a. VALIDATE AND RELAX SIGN RESTRICTIONS
    # =========================================================================
    
    .extract_irf_quantiles <- function(irfs_obj) {
      # Extract IRF quantiles into a usable format
      Q      <- irfs_obj$quants
      dd     <- dim(Q)
      K <- dd[2]; H <- dd[3]; S <- dd[4]
      vars_nm <- irfs_obj$variables
      if (is.null(vars_nm) || length(vars_nm) != K) vars_nm <- paste0("y", seq_len(K))
      
      result <- list()
      for (s in seq_len(S)) {
        shock_name <- paste0("shock", s)
        result[[shock_name]] <- list()
        for (j in seq_len(K)) {
          var_name <- vars_nm[j]
          result[[shock_name]][[var_name]] <- list(
            median = as.numeric(Q[2, j, , s]),
            lower  = as.numeric(Q[1, j, , s]),
            upper  = as.numeric(Q[3, j, , s])
          )
        }
      }
      result
    }
    
    .validate_restrictions <- function(irfs_obj, SR_mat, check_horizons = 0L) {
      # Validate each restriction and return detailed report
      irf_data <- .extract_irf_quantiles(irfs_obj)
      K <- nrow(SR_mat)
      vars_nm <- rownames(SR_mat)
      
      report <- data.frame(
        shock       = character(),
        variable    = character(),
        horizon     = integer(),
        target_sign = numeric(),
        median_irf  = numeric(),
        lower_irf   = numeric(),
        upper_irf   = numeric(),
        empirical_sign = numeric(),
        satisfied   = logical(),
        confidence  = character(),
        stringsAsFactors = FALSE
      )
      
      for (s in seq_len(K)) {
        shock_name <- paste0("shock", s)
        if (!shock_name %in% names(irf_data)) next
        
        for (v in vars_nm) {
          target <- SR_mat[v, s]
          if (is.na(target) || target == 0) next  # Skip unconstrained/unrestricted
          
          for (h in check_horizons) {
            h_idx <- h + 1  # R is 1-indexed
            
            if (!v %in% names(irf_data[[shock_name]])) next
            
            irf_v <- irf_data[[shock_name]][[v]]
            med   <- irf_v$median[h_idx]
            lo    <- irf_v$lower[h_idx]
            hi    <- irf_v$upper[h_idx]
            
            emp_sign <- sign(med)
            satisfied <- (emp_sign == sign(target))
            
            # Determine confidence level
            # Strong: entire CI has correct sign
            # Weak: median has correct sign but CI crosses zero
            # Violated: median has wrong sign
            if (target > 0) {
              if (lo > 0) {
                conf <- "strong"
              } else if (med > 0) {
                conf <- "weak"
              } else {
                conf <- "violated"
              }
            } else {  # target < 0
              if (hi < 0) {
                conf <- "strong"
              } else if (med < 0) {
                conf <- "weak"
              } else {
                conf <- "violated"
              }
            }
            
            report <- rbind(report, data.frame(
              shock       = shock_name,
              variable    = v,
              horizon     = h,
              target_sign = target,
              median_irf  = med,
              lower_irf   = lo,
              upper_irf   = hi,
              empirical_sign = emp_sign,
              satisfied   = satisfied,
              confidence  = conf,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      report
    }

    if (!found) {
      if (sign_strict) {
        stop("Sign restrictions could not be satisfied. Try sign_strict = FALSE to relax.")
      } else {
        if (verbose) message("  [sign-ID] FAILED: Could not satisfy sign restrictions")
        if (verbose) message("  [sign-ID] Running fallback BVAR without sign restrictions...")
        
        # Run fallback BVAR without sign restrictions
        set.seed(seed)
        fallback_fit <- tryCatch({
          BVAR::bvar(
            data    = Y,
            lags    = p,
            n_draw  = n_draw,
            n_burn  = n_burn,
            n_thin  = n_thin,
            irf     = BVAR::bv_irf(horizon = horizon, identification = TRUE),
            verbose = FALSE
          )
        }, error = function(e) NULL)
        
        if (!is.null(fallback_fit)) {
          fallback_irfs <- BVAR::irf(fallback_fit)
          sign_result <- list(
            fit         = fallback_fit,
            irfs        = fallback_irfs,
            scaled      = FALSE,
            orientation = "fallback_cholesky",
            SR_used     = SR,
            sign_lim    = NA,
            fallback    = TRUE
          )
          if (verbose) message("  [sign-ID] Fallback BVAR (Cholesky) completed")
        }
        
        # Create failure report
        sign_report_detailed <- data.frame(
          shock       = character(),
          variable    = character(),
          horizon     = integer(),
          target_sign = numeric(),
          status      = character(),
          stringsAsFactors = FALSE
        )
        
        for (s in seq_len(k)) {
          for (v in use_vars) {
            tgt <- SR_original[v, s]
            if (!is.na(tgt) && tgt != 0) {
              sign_report_detailed <- rbind(sign_report_detailed, data.frame(
                shock       = paste0("shock", s),
                variable    = v,
                horizon     = 0L,
                target_sign = tgt,
                status      = "FAILED_CONVERGENCE",
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        
        SR[, ] <- NA_real_  # Relax all
        sign_report <- sign_report_detailed
      }
    } else {
      # Validate restrictions at specified horizons
      sign_report_detailed <- .validate_restrictions(
        sign_result$irfs, 
        sign_result$SR_used, 
        check_horizons = sign_horizons
      )
      
      # Check for violations
      violations <- sign_report_detailed[sign_report_detailed$confidence == "violated", ]
      
      if (nrow(violations) > 0) {
        if (verbose) {
          message("\n  Sign restriction violations detected:")
          for (i in seq_len(nrow(violations))) {
            v <- violations[i, ]
            message(sprintf("    %s -> %s (h=%d): target=%s, empirical=%s",
                            v$shock, v$variable, v$horizon,
                            ifelse(v$target_sign > 0, "+", "-"),
                            ifelse(v$empirical_sign > 0, "+", "-")))
          }
        }
        
        if (!sign_strict) {
          # Relax violated restrictions
          if (verbose) message("\n  Relaxing violated restrictions (sign_strict = FALSE)...")
          
          for (i in seq_len(nrow(violations))) {
            v <- violations[i, ]
            shock_num <- as.integer(gsub("\\D", "", v$shock))
            SR[v$variable, shock_num] <- NA_real_
            sign_report_detailed$status[
              sign_report_detailed$shock == v$shock & 
              sign_report_detailed$variable == v$variable &
              sign_report_detailed$horizon == v$horizon
            ] <- "RELAXED"
          }
          
          # Mark satisfied restrictions
          sign_report_detailed$status[is.na(sign_report_detailed$status)] <- 
            ifelse(sign_report_detailed$satisfied[is.na(sign_report_detailed$status)], 
                   "SATISFIED", "RELAXED")
          
        } else {
          stop("Sign restrictions violated and sign_strict = TRUE. Set sign_strict = FALSE to relax.")
        }
      } else {
        # All restrictions satisfied
        sign_report_detailed$status <- ifelse(
          sign_report_detailed$confidence == "strong", "SATISFIED_STRONG",
          ifelse(sign_report_detailed$confidence == "weak", "SATISFIED_WEAK", "SATISFIED")
        )
      }
      
      sign_report <- sign_report_detailed
      
      if (verbose) {
        message("\n  Sign restriction validation report:")
        # Summary by status
        status_summary <- table(sign_report$status)
        for (st in names(status_summary)) {
          message(sprintf("    %s: %d", st, status_summary[st]))
        }
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
      
      if (isTRUE(sign_result$fallback)) {
        cat("  STATUS        : FALLBACK (sign restrictions failed, using Cholesky)\n")
      } else {
        cat(sprintf("  Scaled data   : %s\n", if (sign_result$scaled) "YES" else "NO"))
        cat(sprintf("  Orientation   : %s\n", sign_result$orientation))
        cat(sprintf("  sign_lim used : %g\n", sign_result$sign_lim))
      }
      cat(sprintf("  Shocks ID'd   : %d\n", n_restricted_shocks))
      lml <- tryCatch(as.numeric(sign_result$fit$log_ml), error = function(e) numeric(0))
      if (length(lml) == 1L && !is.na(lml)) cat(sprintf("  Log ML        : %.4f\n", lml))
      
      # Sign restriction summary
      if (!is.null(sign_report) && nrow(sign_report) > 0) {
        cat("\n  Restriction Status Summary:\n")
        status_tab <- table(sign_report$status)
        for (st in names(status_tab)) {
          cat(sprintf("    %-20s: %d\n", st, status_tab[st]))
        }
      }
    }
    
    if (!is.null(cholesky_result)) {
      cat("\n[Cholesky]\n")
      cat(sprintf("  Order         : %s\n", paste(cholesky_result$order, collapse = " -> ")))
      lml <- tryCatch(as.numeric(cholesky_result$fit$log_ml), error = function(e) numeric(0))
      if (length(lml) == 1L && !is.na(lml)) cat(sprintf("  Log ML        : %.4f\n", lml))
    }
    cat("================================================\n")
  }

  # ===========================================================================
  # 16. RETURN
  # ===========================================================================
  

  # Determine if sign restrictions were successfully applied
  sign_success <- !is.null(sign_result) && 
                  !isTRUE(sign_result$fallback) &&
                  !is.null(sign_report) &&
                  any(grepl("SATISFIED", sign_report$status))
  
  out <- list(
    # Model results
    sign_result     = sign_result,
    cholesky_result = cholesky_result,
    
    # Covariance matrices
    vcov_sign       = vcov_sign,
    vcov_cholesky   = vcov_cholesky,
    
    # IRF data frames
    irf_df_sign     = irf_df_sign,
    irf_df_cholesky = irf_df_cholesky,
    
    # Sign restriction reporting
    SR_requested    = SR_original,
    SR_final        = SR,
    sign_report     = sign_report,           # Detailed validation report
    sign_success    = sign_success,          # Did sign restrictions work?
    sign_fallback   = isTRUE(sign_result$fallback),  # Did we fall back to Cholesky?
    n_restricted_shocks = n_restricted_shocks,
    
    # Plots
    plots           = plots,
    
    # Variable mapping
    var_map         = list(
      mp_var      = mp_resolved,
      risk_var    = risk_resolved,
      sector_vars = sector_resolved,
      all_vars    = use_vars,
      var_order   = use_vars,
      transforms  = transforms,
      diff_orders = diff_orders
    ),
    
    # Estimation specification
    spec = list(
      lags         = p,
      horizon      = horizon,
      n_draw       = n_draw,
      n_burn       = n_burn,
      n_thin       = n_thin,
      seed         = seed,
      sign_strict  = sign_strict,
      sign_horizons = sign_horizons
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

#' Extract IRF for a specific shock-response pair
#' 
#' @param res Result object from bvar_risk_channel
#' @param shock Shock variable name (raw or transformed)
#' @param response Response variable name (raw or transformed)
#' @param id Identification scheme: "sign" or "cholesky"
#' @return Data frame with IRF quantiles
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

#' Plot IRF for a specific shock-response pair
#' 
#' @param res Result object from bvar_risk_channel
#' @param shock Shock variable name
#' @param response Response variable name
#' @param id Identification scheme: "sign" or "cholesky"
#' @param ... Additional arguments (unused)
#' @return ggplot2 object
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

#' Get sign restriction report
#' 
#' @param res Result object from bvar_risk_channel
#' @param shock Optional: filter by shock (e.g., "shock1")
#' @param status Optional: filter by status (e.g., "SATISFIED", "RELAXED")
#' @return Data frame with restriction validation details
bvar_rc_sign_report <- function(res, shock = NULL, status = NULL) {
  report <- res$sign_report
  if (is.null(report)) {
    message("No sign restrictions were applied.")
    return(NULL)
  }
  
  if (!is.null(shock)) {
    report <- report[report$shock == shock, , drop = FALSE]
  }
  if (!is.null(status)) {
    report <- report[report$status %in% status, , drop = FALSE]
  }
  
  report
}

#' Print sign restriction matrices (requested vs final)
#' 
#' @param res Result object from bvar_risk_channel
bvar_rc_print_SR <- function(res) {
  cat("\n=== REQUESTED Sign Restriction Matrix ===\n")
  print(res$SR_requested)
  
  cat("\n=== FINAL Sign Restriction Matrix (after relaxation) ===\n")
  print(res$SR_final)
  
  if (!is.null(res$sign_report)) {
    cat("\n=== Restriction Status ===\n")
    print(res$sign_report[, c("shock", "variable", "target_sign", "empirical_sign", "status")])
  }
  
  invisible(res)
}

#' Run batch of BVAR models with different configurations
#' 
#' @param sector_dfs Named list of sector data frames
#' @param risk_mp_df Risk/MP data frame
#' @param configs List of configuration lists, each with model parameters
#' @param ... Additional arguments passed to all models
#' @return Named list of results
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

#' Compare sign-restricted vs Cholesky IRFs
#' 
#' @param res Result object from bvar_risk_channel
#' @param shock Shock variable name
#' @param response Response variable name
#' @return ggplot2 object with both IRFs overlaid
bvar_rc_compare_irfs <- function(res, shock, response) {
  if (is.null(res$irf_df_sign) || is.null(res$irf_df_cholesky)) {
    stop("Both sign-restricted and Cholesky IRFs required for comparison.")
  }
  
  d_sign <- bvar_rc_irf(res, shock, response, "sign")
  d_chol <- bvar_rc_irf(res, shock, response, "cholesky")
  
  d_sign$method <- "Sign-restricted"
  d_chol$method <- "Cholesky"
  
  d <- rbind(d_sign, d_chol)
  
  ggplot2::ggplot(d, ggplot2::aes(x = horizon, y = median, color = method, fill = method)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
    ggplot2::scale_color_manual(values = c("Sign-restricted" = "#2563EB", "Cholesky" = "#DC2626")) +
    ggplot2::scale_fill_manual(values = c("Sign-restricted" = "#60A5FA", "Cholesky" = "#FCA5A5")) +
    ggplot2::labs(
      title = sprintf("%s -> %s: Method Comparison", shock, response),
      x = "Horizon (quarters)", 
      y = "Response",
      color = "Method",
      fill = "Method"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Diagnose sign restriction feasibility
#' 
#' Checks whether requested sign restrictions are compatible with the data
#' by examining the Cholesky IRFs at impact
#' 
#' @param res Result object from bvar_risk_channel (must have run_cholesky = TRUE)
#' @return Data frame comparing requested signs with Cholesky impact responses
bvar_rc_diagnose_signs <- function(res) {
  if (is.null(res$irf_df_cholesky)) {
    stop("Cholesky IRFs required for diagnosis. Re-run with run_cholesky = TRUE.")
  }
  
  SR <- res$SR_requested
  if (is.null(SR)) {
    message("No sign restrictions were requested.")
    return(NULL)
  }
  
  irf_df <- res$irf_df_cholesky
  vars <- rownames(SR)
  k <- nrow(SR)
  
  # Get h=0 IRFs from Cholesky
  h0 <- irf_df[irf_df$horizon == 0, ]
  
  diagnosis <- data.frame(
    shock           = character(),
    variable        = character(),
    requested_sign  = numeric(),
    cholesky_median = numeric(),
    cholesky_sign   = numeric(),
    compatible      = logical(),
    ci_excludes_zero = logical(),
    stringsAsFactors = FALSE
  )
  
  for (s in seq_len(k)) {
    shock_var <- vars[s]
    for (v in vars) {
      req_sign <- SR[v, s]
      if (is.na(req_sign) || req_sign == 0) next
      
      row <- h0[h0$shock == shock_var & h0$variable == v, ]
      if (nrow(row) == 0) next
      
      chol_med <- row$median[1]
      chol_sign <- sign(chol_med)
      compatible <- (chol_sign == sign(req_sign))
      ci_excludes <- (row$lower[1] > 0 && req_sign > 0) || 
                     (row$upper[1] < 0 && req_sign < 0)
      
      diagnosis <- rbind(diagnosis, data.frame(
        shock           = shock_var,
        variable        = v,
        requested_sign  = req_sign,
        cholesky_median = round(chol_med, 6),
        cholesky_sign   = chol_sign,
        compatible      = compatible,
        ci_excludes_zero = ci_excludes,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(diagnosis) == 0) {
    message("No binding sign restrictions found.")
    return(NULL)
  }
  
  # Summary
  n_compat <- sum(diagnosis$compatible)
  n_total  <- nrow(diagnosis)
  
  cat(sprintf("\n=== Sign Restriction Diagnosis ===\n"))
  cat(sprintf("Restrictions compatible with Cholesky: %d / %d (%.0f%%)\n\n",
              n_compat, n_total, 100 * n_compat / n_total))
  
  if (any(!diagnosis$compatible)) {
    cat("INCOMPATIBLE restrictions (Cholesky IRF has opposite sign):\n")
    incompat <- diagnosis[!diagnosis$compatible, ]
    for (i in seq_len(nrow(incompat))) {
      r <- incompat[i, ]
      cat(sprintf("  %s -> %s: requested %s, Cholesky shows %s (median=%.4f)\n",
                  r$shock, r$variable,
                  ifelse(r$requested_sign > 0, "+", "-"),
                  ifelse(r$cholesky_sign > 0, "+", "-"),
                  r$cholesky_median))
    }
    cat("\nThese restrictions are unlikely to be satisfied.\n")
    cat("Consider: (1) relaxing these, (2) different variable ordering, or (3) different shock definition.\n")
  }
  
  invisible(diagnosis)
}
