# Global UI theme (dark, matches landing aesthetic). Loaded from R/ after config.

GLOBAL_APP_CSS <- "
:root {
  --app-bg: #04070d;
  --app-surface: rgba(255, 255, 255, 0.045);
  --app-surface-2: rgba(0, 0, 0, 0.35);
  --app-border: rgba(255, 255, 255, 0.1);
  --app-text: #e2e8f0;
  --app-muted: #94a3b8;
  --app-accent: #22d3ee;
  --app-accent-2: #5eead4;
}

html, body {
  background-color: var(--app-bg) !important;
  color: var(--app-text);
  font-family: 'DM Sans', system-ui, -apple-system, sans-serif;
}

body > .container-fluid {
  background-color: var(--app-bg);
  max-width: 100%;
}

/* Nav bar (all tabs) */
.navbar-default,
nav.navbar.navbar-default {
  background: rgba(6, 10, 18, 0.94) !important;
  border-color: rgba(255, 255, 255, 0.08) !important;
  backdrop-filter: blur(14px);
  -webkit-backdrop-filter: blur(14px);
  margin-bottom: 0;
}
.navbar-default .navbar-brand {
  color: #f8fafc !important;
  font-weight: 600;
}
.navbar-default .navbar-nav > li > a {
  color: rgba(241, 245, 249, 0.9) !important;
}
.navbar-default .navbar-nav > li > a:hover,
.navbar-default .navbar-nav > li > a:focus {
  color: #fff !important;
  background: rgba(255, 255, 255, 0.06) !important;
}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {
  background: rgba(34, 211, 238, 0.14) !important;
  color: var(--app-accent-2) !important;
}
.navbar-default .navbar-toggle {
  border-color: rgba(255, 255, 255, 0.25);
}
.navbar-default .navbar-toggle .icon-bar {
  background-color: rgba(255, 255, 255, 0.8);
}

/* Tab panels */
.tab-content {
  background-color: var(--app-bg) !important;
  color: var(--app-text);
  border: none !important;
}

/* Sidebars & wells */
.well {
  background: var(--app-surface) !important;
  border: 1px solid var(--app-border) !important;
  border-radius: 12px !important;
  color: var(--app-text);
  box-shadow: none !important;
}
.well h4, .well h5, .sidebar-panel h4 {
  color: #f1f5f9;
}

/* Headings & copy */
h1, h2, h3, h4, h5, h6 {
  color: #f1f5f9;
}
.main-header, .lead {
  color: var(--app-text);
}
.text-muted {
  color: var(--app-muted) !important;
}

/* Form controls */
.shiny-input-container label {
  color: #cbd5e1 !important;
}
.form-control, select.form-control {
  background-color: var(--app-surface-2) !important;
  border: 1px solid var(--app-border) !important;
  color: #f1f5f9 !important;
  border-radius: 8px;
}
.form-control:focus {
  border-color: rgba(34, 211, 238, 0.45) !important;
  box-shadow: 0 0 0 2px rgba(34, 211, 238, 0.15);
}
.selectize-input, .selectize-dropdown {
  background-color: var(--app-surface-2) !important;
  border-color: var(--app-border) !important;
  color: #f1f5f9 !important;
}
.selectize-dropdown .active {
  background-color: rgba(34, 211, 238, 0.2) !important;
}

/* Buttons — keep semantic colors, tweak contrast on dark */
.btn-default {
  background: rgba(255, 255, 255, 0.08);
  border-color: var(--app-border);
  color: #e2e8f0;
}
.btn-default:hover {
  background: rgba(255, 255, 255, 0.12);
  color: #fff;
}

/* Tables */
.table {
  color: var(--app-text);
  background: transparent;
}
.table > thead > tr > th {
  border-bottom-color: var(--app-border);
  color: #cbd5e1;
}
.table > tbody > tr > td {
  border-top-color: var(--app-border);
}
.table-striped > tbody > tr:nth-of-type(odd) {
  background-color: rgba(255, 255, 255, 0.03);
}

hr {
  border-top-color: var(--app-border);
}

/* Verbatim / pre */
pre, .shiny-text-output {
  color: #cbd5e1 !important;
  background: rgba(0, 0, 0, 0.35) !important;
  border: 1px solid var(--app-border) !important;
  border-radius: 8px;
}

/* Slider */
.irs-line, .irs-bar-edge {
  background: rgba(255, 255, 255, 0.1) !important;
  border-color: var(--app-border) !important;
}
.irs-bar {
  background: linear-gradient(90deg, #0e7490, var(--app-accent)) !important;
  border-color: transparent !important;
}
.irs-from, .irs-to, .irs-single, .irs-min, .irs-max {
  color: var(--app-muted) !important;
}
.irs-handle > i:first-child {
  background-color: var(--app-accent) !important;
}

/* Date inputs */
.datepicker {
  color-scheme: dark;
}
"
