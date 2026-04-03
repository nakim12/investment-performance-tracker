# Shiny loads this file before app.R so R/ modules are available on any host.

r_mods <- c(
  "config.R",
  "helpers.R",
  "app_theme.R",
  "portfolio.R",
  "landing_ui.R",
  "methodology_ui.R"
)
for (m in r_mods) {
  f <- file.path("R", m)
  if (file.exists(f)) source(f, local = FALSE)
}
