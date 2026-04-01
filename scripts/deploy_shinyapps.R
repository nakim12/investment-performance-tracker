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

rsconnect::deployApp(
  appDir        = ".",
  appPrimaryDoc = "app.R",
  appName       = app_name,
  lint          = FALSE,
  forceUpdate   = TRUE
)
