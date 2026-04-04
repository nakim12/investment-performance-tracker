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

## Why deploy can feel slow

- **Cold or expired cache:** The first run after a cache miss reinstalls many R packages (`forecast`, `quantmod`, etc. pull a large tree). Cached runs are usually much faster (often a few minutes vs 10+).
- **Compile from source:** If a binary isn’t available for the runner’s Linux + R version, some packages compile from source, which adds time.
- **Deploy step:** `rsconnect` bundles the app and uploads it to Posit’s servers; that step is mostly network + their processing.

**What we already do:** Ubuntu, dependency caching in `setup-r-dependencies`, and **CRAN (`cloud.r-project.org`)** for the deploy job—not Posit Package Manager in that workflow. Using RSPM/PPM on the Actions runner can make `rsconnect` write a manifest with a `RSPM` repository shorthand; **shinyapps.io** then fails to fetch packages (`Unsupported url scheme: RSPM/...`). `scripts/deploy_shinyapps.R` also sets `options(repos = ...)` before `deployApp()` so the bundle lists full `https://` URLs.

**Workflow tweak:** Deploy runs on `push` to `main` only when `app.R`, `R/`, `scripts/deploy_shinyapps.R`, `DESCRIPTION`, or the deploy workflow itself changes—so README-only pushes don’t trigger a full deploy.

## Notes

- The workflow uses `DESCRIPTION` only so `r-lib/actions` can install CRAN dependencies; the project is still a Shiny app, not a published R package.
- `scripts/deploy_shinyapps.R` deploys only `app.R` and `R/*.R` so the R Markdown report is not bundled (avoids renv snapshot errors for knitr/rmarkdown on CI).
- `.rscignore` excludes the same paths if you publish from RStudio without the script.
- Local `rsconnect/` credentials are gitignored; CI does not use them.
