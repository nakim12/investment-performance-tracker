# Portfolio JSON snapshots, diagnosis pin comparison, and bundled report helpers.

SNAPSHOT_FORMAT <- "pil-portfolio-snapshot"
SNAPSHOT_VERSION <- 1L

#' Build list for JSON export (holdings + build inputs; optional metrics summary).
build_portfolio_snapshot_list <- function(holdings, benchmark_ticker, date_start, date_end,
                                          label = "", metrics = NULL) {
  h <- holdings %>%
    transmute(ticker = toupper(as.character(ticker)), weight = as.numeric(weight)) %>%
    filter(nzchar(ticker), !is.na(weight), weight > 0)

  list(
    format          = SNAPSHOT_FORMAT,
    version         = SNAPSHOT_VERSION,
    label           = as.character(label)[1],
    saved_utc       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    benchmark_ticker = as.character(benchmark_ticker)[1],
    date_range      = list(
      start = as.character(as.Date(date_start)),
      end   = as.character(as.Date(date_end))
    ),
    holdings        = mapply(
      function(ticker, weight) list(ticker = ticker, weight = weight),
      h$ticker, h$weight,
      SIMPLIFY = FALSE
    ),
    metrics_summary = metrics
  )
}

#' Parse uploaded JSON; returns list or stops with readable error.
parse_portfolio_snapshot <- function(raw) {
  if (length(raw) != 1L || !nzchar(raw)) {
    stop("Snapshot file is empty.", call. = FALSE)
  }
  obj <- tryCatch(
    jsonlite::fromJSON(raw, simplifyVector = TRUE),
    error = function(e) stop("Invalid JSON: ", conditionMessage(e), call. = FALSE)
  )
  if (!is.list(obj)) stop("Snapshot root must be a JSON object.", call. = FALSE)
  if (is.null(obj$format) || obj$format != SNAPSHOT_FORMAT) {
    stop("Not an Axis portfolio snapshot (wrong format field).", call. = FALSE)
  }
  ver <- suppressWarnings(as.integer(obj$version))
  if (is.na(ver) || ver != SNAPSHOT_VERSION) {
    stop("Unsupported snapshot version. Re-export from the latest app.", call. = FALSE)
  }
  if (is.null(obj$holdings) || !length(obj$holdings)) {
    stop("Snapshot has no holdings.", call. = FALSE)
  }
  obj
}

#' Normalize holdings from snapshot to tibble with sectors.
snapshot_holdings_tibble <- function(obj) {
  h <- obj$holdings
  if (is.data.frame(h)) {
    tickers <- toupper(as.character(h$ticker))
    wts     <- as.numeric(h$weight)
  } else if (is.list(h) && length(h) && is.list(h[[1]])) {
    tickers <- vapply(h, function(x) toupper(as.character(x$ticker)), "")
    wts     <- vapply(h, function(x) as.numeric(x$weight), NA_real_)
  } else {
    stop("Snapshot holdings layout is invalid.", call. = FALSE)
  }
  ok <- nzchar(tickers) & !is.na(wts) & wts > 0
  tickers <- tickers[ok]
  wts     <- wts[ok]
  if (!length(tickers)) stop("No valid holdings in snapshot.", call. = FALSE)
  sectors <- vapply(tickers, get_sector_for_ticker, "")
  tibble(ticker = tickers, weight = wts, sector = sectors)
}

#' Metrics list safe for JSON (optional attachment to snapshot).
metrics_summary_for_json <- function(m) {
  if (is.null(m) || !length(m)) return(NULL)
  nm <- names(m)
  out <- vector("list", length(nm))
  names(out) <- nm
  for (i in seq_along(nm)) {
    v <- m[[nm[i]]]
    if (length(v) != 1L) {
      out[[i]] <- NULL
    } else if (is.na(v)) {
      out[[i]] <- NULL
    } else {
      out[[i]] <- as.numeric(v)
    }
  }
  out[!vapply(out, is.null, logical(1))]
}

#' Pin record for in-session compare (Diagnosis tab).
build_diagnosis_pin <- function(label, m, holdings, benchmark_name) {
  lab <- trimws(as.character(label))
  if (!nzchar(lab)) lab <- "Pin"
  list(
    label         = lab,
    saved_at      = format(Sys.time(), usetz = TRUE),
    benchmark_name = as.character(benchmark_name),
    holdings_line = paste(
      paste0(holdings$ticker, " (", sprintf("%.1f", holdings$weight), "%)"),
      collapse = ", "
    ),
    metrics_table = build_diagnosis_metrics_tibble(m)
  )
}

#' Wide table: Metric, Current, and one column per pin (by label).
compare_diagnosis_pins_table <- function(cur_tb, pin_a, pin_b) {
  out <- tibble(Metric = cur_tb$Metric, Current = cur_tb$Value)
  add_pin_col <- function(df, pin) {
    if (is.null(pin)) return(df)
    col <- make.names(pin$label)
    if (col %in% names(df)) col <- paste0(col, "_2")
    vals <- pin$metrics_table$Value[match(df$Metric, pin$metrics_table$Metric)]
    vals[is.na(vals)] <- "—"
    df[[col]] <- vals
    df
  }
  out <- add_pin_col(out, pin_a)
  add_pin_col(out, pin_b)
}
