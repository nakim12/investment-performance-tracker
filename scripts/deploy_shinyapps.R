#!/usr/bin/env Rscript
# Deploy app.R (and R/ helpers) to shinyapps.io. Run from repository root.
# Requires env: SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, SHINYAPPS_SECRET
# Optional: SHINYAPPS_APP_NAME (default: investment-performance-tracker)

account <- Sys.getenv("SHINYAPPS_ACCOUNT", unset = "")
token   <- Sys.getenv("SHINYAPPS_TOKEN", unset = "")
secret  <- Sys.getenv("SHINYAPPS_SECRET", unset = "")

if (account == "" || token == "" || secret == "") {
  stop(
    "Missing shinyapps.io credentials. Set SHINYAPPS_ACCOUNT, ",
    "SHINYAPPS_TOKEN, and SHINYAPPS_SECRET (GitHub Actions secrets).",
    call. = FALSE
  )
}

app_name <- Sys.getenv("SHINYAPPS_APP_NAME", unset = "")
if (app_name == "") app_name <- "investment-performance-tracker"

rsconnect::setAccountInfo(
  name   = account,
  token  = token,
  secret = secret
)

# shinyapps.io downloads package *sources* from URLs in the bundle manifest.
# - Named repos c(CRAN = "https://...") can still serialize as scheme "CRAN/..." on server.
# - Linux/binary installs sometimes record Version like "15.2.4-1" but CRAN tarballs are
#   "15.2.4", so fetches 404 / wrong path.
cran_root <- "https://cloud.r-project.org"
options(repos = cran_root)

if (nzchar(Sys.getenv("CI", ""))) {
  message("CI: reinstalling Rcpp stack from CRAN source for rsconnect/shinyapps manifest...")
  utils::install.packages(
    c("Rcpp", "cpp11", "RcppArmadillo"),
    repos = cran_root,
    type  = "source",
    quiet = TRUE
  )
}

# Only bundle the Shiny app. If the whole repo is bundled, rsconnect's renv
# snapshot pulls in knitr/rmarkdown/tseries from the Rmd and fails CI.
r_files <- if (dir.exists("R")) {
  list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
} else {
  character()
}
app_files <- c(
  "app.R",
  if (file.exists("global.R")) "global.R",
  sort(r_files)
)

rsconnect::deployApp(
  appDir        = ".",
  appPrimaryDoc = "app.R",
  appName       = app_name,
  appFiles      = app_files,
  lint          = FALSE,
  forceUpdate   = TRUE
)
