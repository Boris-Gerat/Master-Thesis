pca_risk_index <- function(df,
                           vars = NULL,
                           date_col = NULL,
                           n_factors = 1,
                           method = c("ppca", "svd", "svdImpute", "bpca", "nipals"),
                           center = TRUE,
                           scale = TRUE,
                           plot_factor = TRUE,
                           factor_name = "RISK_INDEX",
                           z_score = TRUE) {
  method <- match.arg(method)
  if (!requireNamespace("pcaMethods", quietly = TRUE))
    stop("Install pcaMethods via BiocManager::install('pcaMethods')")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Install ggplot2")

  # --------------------------
  # Variable selection
  # --------------------------
  if (is.null(vars)) {
    X_df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]
  } else {
    X_df <- df[, vars, drop = FALSE]
  }
  X <- as.matrix(X_df)

  # --------------------------
  # Standardization
  # --------------------------
  if (center)
    X <- sweep(X, 2, colMeans(X, na.rm = TRUE), "-")
  if (scale) {
    sds <- apply(X, 2, sd, na.rm = TRUE)
    sds[sds == 0 | is.na(sds)] <- 1
    X <- sweep(X, 2, sds, "/")
  }

  # --------------------------
  # PCA / PPCA
  # --------------------------
  fit <- pcaMethods::pca(X,
                         method = method,
                         nPcs = n_factors,
                         center = FALSE,
                         scale = "none")

  L <- pcaMethods::loadings(fit)
  S <- pcaMethods::scores(fit)

  # Correct explained variance
  eigenvals  <- fit@sDev^2
  total_var  <- sum(apply(X, 2, var, na.rm = TRUE))
  explained_var <- eigenvals / total_var

  # FIX: force column names safely
  colnames(L) <- paste0("PC", seq_len(n_factors))
  colnames(S) <- paste0("PC", seq_len(n_factors))

  loadings_df <- data.frame(variable = rownames(L), L, row.names = NULL)
  scores_df   <- data.frame(S)

  # --------------------------
  # Attach dates
  # --------------------------
  if (!is.null(date_col)) {
    scores_df[[date_col]] <- df[[date_col]]
  }

  # --------------------------
  # Z-score if desired
  # --------------------------
  if (z_score) {
    scores_df$PC1 <- as.numeric(scale(scores_df$PC1))
  }

  # --------------------------
  # Quant Color Scheme
  # --------------------------
  quant_cols <- c(
    STLFSI     = "#1F77B4",
    NFCI       = "#C9A227",
    KCFSI      = "#58508D",
    VIX        = "#A60628",
    SHAPIRO    = "#58508D",
    EPUI       = "#C9A227",
    RISK_INDEX = "#1F77B4"
  )

  # FIX: resolve color using factor_name, fall back to default blue
  plot_color <- ifelse(factor_name %in% names(quant_cols),
                       quant_cols[factor_name],
                       "#1F77B4")

  # --------------------------
  # Plot
  # --------------------------
  if (plot_factor && !is.null(date_col)) {
    # FIX: use .data[[]] instead of deprecated aes_string()
    # FIX: use linewidth instead of deprecated size
    p <- ggplot2::ggplot(scores_df,
                         ggplot2::aes(x = .data[[date_col]], y = PC1)) +
      ggplot2::geom_line(color = plot_color, linewidth = 1) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(title = paste("Latent Factor:", factor_name),
                    y = "Factor (Z-scored)",
                    x = NULL) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
    print(p)
  }

  return(list(
    model    = fit,
    loadings = loadings_df,
    scores   = scores_df,
    explained = data.frame(
      factor        = paste0("PC", seq_len(n_factors)),
      eigenvalue    = eigenvals[seq_len(n_factors)],
      explained_var = explained_var[seq_len(n_factors)]
    )
  ))
}
