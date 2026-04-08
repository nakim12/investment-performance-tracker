# Allocation Lab: long-only min-variance (quadprog) and heuristic portfolios.

regularize_cov <- function(Sigma) {
  S <- (Sigma + t(Sigma)) / 2
  n <- nrow(S)
  ev <- eigen(S, symmetric = TRUE)$values
  eps <- max(1e-10, 1e-4 * max(abs(ev), 1e-12))
  if (length(ev) && min(ev) < eps) {
    S <- S + diag(n) * (eps - min(ev))
  }
  S
}

# Long-only min variance; max_w in (0,1] caps each weight (1 = no cap).
alloc_min_variance <- function(Sigma, max_w = 1) {
  n <- nrow(Sigma)
  if (n <= 1L) {
    nm <- colnames(Sigma)
    if (is.null(nm) || !length(nm)) nm <- rownames(Sigma)
    if (is.null(nm) || !length(nm)) nm <- "x"
    return(stats::setNames(1, nm[[1]]))
  }

  S <- regularize_cov(Sigma)
  Dmat <- 2 * S
  dvec <- rep(0, n)

  Amat <- cbind(matrix(1, n, 1), diag(n))
  bvec <- c(1, rep(0, n))
  meq  <- 1L

  if (max_w < 1 - 1e-8) {
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max_w, n))
  }

  fit <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    w <- rep(1 / n, n)
  } else {
    w <- pmax(fit$solution, 0)
  }
  if (sum(w) < 1e-12) w <- rep(1 / n, n)
  w <- w / sum(w)
  nm <- colnames(Sigma)
  if (is.null(nm)) nm <- rownames(Sigma)
  stats::setNames(w, nm)
}

alloc_inverse_volatility <- function(Sigma) {
  n <- nrow(Sigma)
  v <- sqrt(pmax(diag(Sigma), 1e-16))
  w <- 1 / v
  w <- w / sum(w)
  nm <- colnames(Sigma)
  if (is.null(nm)) nm <- rownames(Sigma)
  stats::setNames(w, nm)
}

alloc_equal_weight <- function(tickers) {
  n <- length(tickers)
  stats::setNames(rep(1 / n, n), tickers)
}

# Unconstrained tangency then clip & renormalize (approximate long-only max Sharpe).
alloc_max_sharpe_projected <- function(Sigma, mu) {
  n <- length(mu)
  if (n <= 1L) {
    nm <- names(mu)
    if (is.null(nm) || !length(nm)) nm <- "x"
    return(stats::setNames(1, nm[[1]]))
  }

  S <- regularize_cov(Sigma)
  w <- tryCatch(
    as.numeric(solve(S) %*% mu),
    error = function(e) rep(1 / n, n)
  )
  w <- pmax(w, 0)
  if (sum(w) < 1e-12) w <- rep(1 / n, n)
  w <- w / sum(w)
  stats::setNames(w, names(mu))
}

compute_mu_sigma_annual <- function(price_data, tickers) {
  tickers <- unique(tickers)
  pd <- price_data %>% filter(ticker %in% tickers)
  rw <- returns_wide_from_price_data(pd)
  nm <- intersect(tickers, setdiff(names(rw), "date"))
  if (length(nm) < 1L) return(NULL)

  rw <- rw %>% select(date, dplyr::all_of(nm)) %>% drop_na()
  if (nrow(rw) < 5L) return(NULL)

  X <- as.matrix(rw[, nm, drop = FALSE])
  mu_ann <- colMeans(X, na.rm = TRUE) * 252
  sigma_ann <- stats::cov(X, use = "pairwise.complete.obs") * 252
  dimnames(sigma_ann) <- list(nm, nm)
  names(mu_ann) <- nm
  list(mu = mu_ann, Sigma = sigma_ann, n_days = nrow(rw))
}

align_weights <- function(w, tickers) {
  w <- w[tickers]
  w[is.na(w)] <- 0
  w / sum(w)
}

portfolio_ann_metrics <- function(w, mu, Sigma) {
  nm <- names(w)
  w <- as.numeric(w)
  mu_v <- as.numeric(mu[nm])
  Sig <- Sigma[nm, nm, drop = FALSE]
  er <- sum(w * mu_v)
  vol <- sqrt(max(0, as.numeric(t(w) %*% Sig %*% w)))
  sharpe <- if (vol > 1e-12) er / vol else NA_real_
  list(ann_return = er, ann_vol = vol, sharpe = sharpe)
}

propose_allocation <- function(method, Sigma, mu, max_w = 1) {
  tickers <- colnames(Sigma)
  if (is.null(tickers)) tickers <- rownames(Sigma)
  out <- switch(
    method,
    minvar         = alloc_min_variance(Sigma, max_w = max_w),
    invvol         = alloc_inverse_volatility(Sigma),
    maxsharpe_proj = alloc_max_sharpe_projected(Sigma, mu[tickers]),
    equal          = alloc_equal_weight(tickers),
    NULL
  )
  if (is.null(out)) out <- alloc_equal_weight(tickers)
  out
}
