# Automated deploy to shinyapps.io

Pushing to `main` runs [`.github/workflows/deploy-shinyapps.yml`](../workflows/deploy-shinyapps.yml), which installs R packages and runs `scripts/deploy_shinyapps.R`.

## Repository secrets

In the GitHub repo: **Settings → Secrets and variables → Actions → New repository secret**.

| Secret | Required | Description |
|--------|----------|-------------|
| `SHINYAPPS_ACCOUNT` | Yes | Your shinyapps.io username (same as in the dashboard URL). |
| `SHINYAPPS_TOKEN` | Yes | Create at [shinyapps.io → Account → Tokens](https://www.shinyapps.io/admin/#/tokens). |
| `SHINYAPPS_SECRET` | Yes | Shown once when the token is created; paste the full value. |
| `SHINYAPPS_APP_NAME` | No | App name on the server. Defaults to `investment-performance-tracker` if unset. |

If any required secret is missing, the workflow job fails at deploy time with a clear error.

## Manual deploy

From the repo root in R:

```r
Sys.setenv(
  SHINYAPPS_ACCOUNT = "your_username",
  SHINYAPPS_TOKEN   = "...",
  SHINYAPPS_SECRET  = "..."
)
source("scripts/deploy_shinyapps.R")  # or: Rscript scripts/deploy_shinyapps.R
```

Or use **Actions → Deploy to shinyapps.io → Run workflow** after secrets are set.

## Notes

- The workflow uses `DESCRIPTION` only so `r-lib/actions` can install CRAN dependencies; the project is still a Shiny app, not a published R package.
- Local `rsconnect/` credentials are gitignored; CI does not use them.
