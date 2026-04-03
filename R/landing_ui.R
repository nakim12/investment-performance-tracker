# Landing page markup and styles (Home tab).
# ShaderToy: set `landing_shadertoy_embed_url` in R/config.R (iframe src from embed dialog).

landing_head_extras <- function() {
  tagList(
    tags$link(
      rel  = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=DM+Sans:ital,opsz,wght@0,9..40,400;0,9..40,500;0,9..40,600;0,9..40,700&",
        "family=JetBrains+Mono:wght@400;500&display=swap"
      )
    ),
    tags$style(HTML(landing_page_css)),
    tags$script(HTML(landing_page_js))
  )
}

landing_page_js <- "
(function () {
  var SCROLL_PILL = 64;
  function getNavbar() {
    return document.querySelector('nav.navbar') ||
      document.querySelector('.navbar.navbar-default') ||
      document.querySelector('.navbar-static-top');
  }
  function setLandingMode(on) {
    document.body.classList.toggle('landing-mode', !!on);
    if (!on) {
      var nav = getNavbar();
      if (nav) nav.classList.remove('landing-nav-pill');
    }
    syncNavScroll();
  }
  function syncNavScroll() {
    var nav = getNavbar();
    if (!nav || !document.body.classList.contains('landing-mode')) return;
    if (window.scrollY >= SCROLL_PILL) nav.classList.add('landing-nav-pill');
    else nav.classList.remove('landing-nav-pill');
  }
  function onInputChanged(e) {
    if (e.name !== 'main_nav') return;
    setLandingMode(e.value === 'Home');
  }
  if (window.jQuery && window.Shiny) {
    Shiny.addCustomMessageHandler('setLandingBody', function (msg) {
      setLandingMode(!!msg.home);
    });
    $(document).on('shiny:inputchanged', onInputChanged);
  }
  window.addEventListener('scroll', syncNavScroll, { passive: true });
  document.addEventListener('DOMContentLoaded', function () {
    setTimeout(syncNavScroll, 100);
  });
})();
"

landing_page_css <- "
/* ========== Home tab: page shell (no white below content) ========== */
body.landing-mode {
  background-color: #04070d !important;
}
body.landing-mode > .container-fluid {
  background-color: #04070d;
  padding-left: 0;
  padding-right: 0;
  max-width: 100%;
}
body.landing-mode > .container-fluid > .navbar {
  margin-bottom: 0;
}
body.landing-mode > .container-fluid > .tab-content {
  background-color: #04070d !important;
  border: none !important;
  padding: 0 !important;
  margin: 0 !important;
}
body.landing-mode > .container-fluid > .tab-content > .tab-pane {
  background-color: #04070d !important;
  padding: 0 !important;
  margin: 0 !important;
  border: none !important;
}

/* ========== Navbar: transparent on Home, pill after scroll ========== */
body.landing-mode nav.navbar,
body.landing-mode .navbar.navbar-default.navbar-static-top {
  transition: background 0.35s ease, box-shadow 0.35s ease, border-radius 0.4s cubic-bezier(0.4, 0, 0.2, 1),
    max-width 0.4s cubic-bezier(0.4, 0, 0.2, 1), margin 0.4s cubic-bezier(0.4, 0, 0.2, 1),
    padding 0.35s ease, border-color 0.35s ease;
  background: rgba(4, 7, 13, 0.35) !important;
  border-color: rgba(255, 255, 255, 0.06) !important;
  border-width: 0 0 1px 0;
  border-style: solid;
  border-radius: 0;
  max-width: 100%;
  margin-left: auto;
  margin-right: auto;
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
}
body.landing-mode nav.navbar.landing-nav-pill,
body.landing-mode .navbar.navbar-default.navbar-static-top.landing-nav-pill {
  max-width: min(920px, calc(100vw - 28px));
  margin-top: 10px;
  margin-bottom: 0;
  border-radius: 999px;
  border: 1px solid rgba(255, 255, 255, 0.1) !important;
  background: rgba(6, 10, 18, 0.78) !important;
  box-shadow: 0 12px 40px rgba(0, 0, 0, 0.45);
  backdrop-filter: blur(18px);
  -webkit-backdrop-filter: blur(18px);
}
body.landing-mode .navbar-default .navbar-brand,
body.landing-mode .navbar-default .navbar-nav > li > a {
  color: rgba(241, 245, 249, 0.92) !important;
}
body.landing-mode .navbar-default .navbar-nav > .active > a,
body.landing-mode .navbar-default .navbar-nav > .active > a:hover,
body.landing-mode .navbar-default .navbar-nav > .active > a:focus {
  background: rgba(34, 211, 238, 0.12) !important;
  color: #5eead4 !important;
  border-radius: 999px;
}
body.landing-mode .navbar-default .navbar-nav > li > a:hover {
  color: #fff !important;
}
body.landing-mode .navbar-default .navbar-toggle {
  border-color: rgba(255, 255, 255, 0.2);
}
body.landing-mode .navbar-default .navbar-toggle .icon-bar {
  background-color: rgba(255, 255, 255, 0.75);
}
/* Bootstrap 5 / bslib */
body.landing-mode .navbar .nav-link {
  color: rgba(241, 245, 249, 0.88) !important;
}
body.landing-mode .navbar .nav-link:hover {
  color: #fff !important;
}
body.landing-mode .navbar .nav-link.active {
  color: #5eead4 !important;
}

/* ========== Landing root: full scroll height, base color ========== */
.landing-root {
  font-family: 'DM Sans', system-ui, -apple-system, sans-serif;
  color: #e8eef4;
  position: relative;
  isolation: isolate;
  min-height: calc(100vh - 52px);
  margin: 0;
  padding: 0;
  overflow-x: hidden;
  background-color: #04070d;
}
.landing-bg-layer {
  position: absolute;
  left: 0;
  right: 0;
  top: 0;
  bottom: 0;
  min-height: 100%;
  z-index: 0;
  pointer-events: none;
}
.landing-shader-iframe {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  border: 0;
  pointer-events: none;
  opacity: 0.5;
  filter: saturate(1.1) contrast(1.05);
}
.landing-bg-vignette {
  position: absolute;
  inset: 0;
  background:
    linear-gradient(180deg, rgba(4, 7, 13, 0.2) 0%, transparent 35%, transparent 65%, rgba(4, 7, 13, 0.95) 100%),
    radial-gradient(ellipse 90% 60% at 50% 25%, transparent 0%, rgba(4, 7, 13, 0.75) 70%, #04070d 100%);
  pointer-events: none;
}
.landing-bg-fallback {
  position: absolute;
  inset: 0;
  background: linear-gradient(165deg, #04070d 0%, #0a1524 40%, #07120f 70%, #04070d 100%);
}
.landing-bg-fallback::before {
  content: '';
  position: absolute;
  inset: -60%;
  background:
    radial-gradient(ellipse 45% 38% at 25% 15%, rgba(34, 211, 238, 0.14) 0%, transparent 55%),
    radial-gradient(ellipse 40% 45% at 80% 55%, rgba(52, 211, 153, 0.09) 0%, transparent 52%),
    radial-gradient(ellipse 35% 40% at 45% 88%, rgba(99, 102, 241, 0.07) 0%, transparent 48%);
  animation: landing-orbit 24s ease-in-out infinite;
}
.landing-bg-fallback::after {
  content: '';
  position: absolute;
  inset: 0;
  background-image: url(\"data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.04'/%3E%3C/svg%3E\");
  opacity: 0.4;
  mix-blend-mode: overlay;
  pointer-events: none;
}
@keyframes landing-orbit {
  0%, 100% { transform: translate(0, 0) rotate(0deg); }
  33% { transform: translate(5%, 4%) rotate(4deg); }
  66% { transform: translate(-4%, 3%) rotate(-3deg); }
}

/* Moving grid + scan shimmer (full page height) */
.landing-grid-sheen {
  position: absolute;
  inset: 0;
  min-height: 100%;
  z-index: 0;
  pointer-events: none;
  background-image:
    linear-gradient(rgba(34, 211, 238, 0.03) 1px, transparent 1px),
    linear-gradient(90deg, rgba(34, 211, 238, 0.03) 1px, transparent 1px);
  background-size: 48px 48px;
  animation: landing-grid-drift 40s linear infinite;
  mask-image: linear-gradient(180deg, black 0%, black 70%, transparent 100%);
  -webkit-mask-image: linear-gradient(180deg, black 0%, black 70%, transparent 100%);
}
@keyframes landing-grid-drift {
  0% { background-position: 0 0, 0 0; }
  100% { background-position: 48px 48px, 48px 48px; }
}
.landing-sheen-bar {
  position: absolute;
  left: -20%;
  width: 50%;
  height: 120%;
  top: -10%;
  background: linear-gradient(105deg, transparent 0%, rgba(34, 211, 238, 0.04) 45%, rgba(255,255,255,0.02) 50%, transparent 70%);
  animation: landing-sheen-move 14s ease-in-out infinite;
  z-index: 0;
  pointer-events: none;
}
@keyframes landing-sheen-move {
  0%, 100% { transform: translateX(0) rotate(12deg); opacity: 0.5; }
  50% { transform: translateX(180%) rotate(12deg); opacity: 0.85; }
}

/* Ambient orbs (scroll with page, span full content) */
.landing-motion-orbs {
  position: absolute;
  left: 0;
  right: 0;
  top: 0;
  bottom: 0;
  min-height: 100%;
  z-index: 0;
  pointer-events: none;
  overflow: hidden;
}
.landing-orb {
  position: absolute;
  border-radius: 50%;
  filter: blur(72px);
  opacity: 0.45;
  animation: landing-orb-float 18s ease-in-out infinite;
}
.landing-orb--1 {
  width: 320px; height: 320px;
  background: rgba(34, 211, 238, 0.35);
  top: 8%; left: -8%;
  animation-delay: 0s;
}
.landing-orb--2 {
  width: 280px; height: 280px;
  background: rgba(52, 211, 153, 0.22);
  top: 35%; right: -10%;
  animation-delay: -4s;
  animation-duration: 22s;
}
.landing-orb--3 {
  width: 240px; height: 240px;
  background: rgba(99, 102, 241, 0.2);
  top: 58%; left: 20%;
  animation-delay: -8s;
  animation-duration: 20s;
}
.landing-orb--4 {
  width: 200px; height: 200px;
  background: rgba(34, 211, 238, 0.15);
  bottom: 12%; right: 25%;
  animation-delay: -2s;
  animation-duration: 25s;
}
.landing-orb--5 {
  width: 360px; height: 360px;
  background: rgba(14, 165, 233, 0.12);
  bottom: -5%; left: 40%;
  animation-delay: -10s;
  animation-duration: 28s;
}
@keyframes landing-orb-float {
  0%, 100% { transform: translate(0, 0) scale(1); }
  33% { transform: translate(30px, -24px) scale(1.06); }
  66% { transform: translate(-20px, 18px) scale(0.96); }
}

.landing-inner {
  position: relative;
  z-index: 2;
  max-width: 1120px;
  margin: 0 auto;
  padding: 2.75rem 1.5rem 5rem;
}
.landing-eyebrow {
  font-family: 'JetBrains Mono', ui-monospace, monospace;
  font-size: 0.72rem;
  letter-spacing: 0.22em;
  text-transform: uppercase;
  color: rgba(34, 211, 238, 0.85);
  margin-bottom: 1rem;
}
.landing-hero h1 {
  font-size: clamp(2.1rem, 4.5vw, 3.25rem);
  font-weight: 700;
  line-height: 1.12;
  letter-spacing: -0.03em;
  margin: 0 0 1rem 0;
  color: #f4f8fc;
}
.landing-hero h1 .landing-accent {
  color: #5eead4;
  font-weight: 600;
}
.landing-lead {
  font-size: 1.125rem;
  line-height: 1.65;
  color: rgba(200, 214, 226, 0.88);
  max-width: 36rem;
  margin-bottom: 1.75rem;
}
.landing-cta-row {
  display: flex;
  flex-wrap: wrap;
  gap: 0.75rem;
  align-items: center;
  margin-bottom: 2.75rem;
}
.landing-btn-primary {
  font-family: inherit;
  font-weight: 600;
  font-size: 0.95rem;
  padding: 0.65rem 1.35rem;
  border-radius: 999px;
  border: none;
  background: linear-gradient(135deg, #22d3ee 0%, #14b8a6 100%);
  color: #041016 !important;
  box-shadow: 0 4px 24px rgba(34, 211, 238, 0.25);
}
.landing-btn-primary:hover {
  filter: brightness(1.08);
  color: #041016 !important;
}
.landing-btn-ghost {
  font-family: inherit;
  font-weight: 500;
  font-size: 0.95rem;
  padding: 0.65rem 1.35rem;
  border-radius: 999px;
  border: 1px solid rgba(255,255,255,0.18);
  background: rgba(255,255,255,0.04);
  color: #e8eef4 !important;
}
.landing-btn-ghost:hover {
  background: rgba(255,255,255,0.09);
  color: #fff !important;
}

.landing-section-title {
  font-size: 0.7rem;
  font-family: 'JetBrains Mono', monospace;
  letter-spacing: 0.2em;
  text-transform: uppercase;
  color: rgba(148, 163, 184, 0.9);
  margin-bottom: 0.5rem;
}
.landing-section-head {
  margin-bottom: 2rem;
}
.landing-section-head h2 {
  font-size: clamp(1.5rem, 3vw, 2rem);
  font-weight: 600;
  color: #f1f5f9;
  margin: 0;
  letter-spacing: -0.02em;
}
.landing-divider {
  height: 1px;
  margin: 2.5rem 0 2.25rem;
  background: linear-gradient(90deg, transparent, rgba(34, 211, 238, 0.35), rgba(52, 211, 153, 0.25), transparent);
  background-size: 200% 100%;
  animation: landing-divider-shine 8s ease infinite;
  border: none;
  opacity: 0.85;
}
@keyframes landing-divider-shine {
  0%, 100% { background-position: 0% 50%; }
  50% { background-position: 100% 50%; }
}

.landing-steps {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
  gap: 1.25rem;
  margin-bottom: 0;
}
.landing-step {
  position: relative;
  padding: 1.35rem 1.25rem;
  border-radius: 16px;
  background: rgba(255,255,255,0.035);
  border: 1px solid rgba(255,255,255,0.09);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  overflow: hidden;
  transition: border-color 0.3s ease, box-shadow 0.3s ease;
}
.landing-step::before {
  content: '';
  position: absolute;
  inset: -1px;
  background: linear-gradient(135deg, rgba(34, 211, 238, 0.15), transparent 40%, rgba(52, 211, 153, 0.08));
  opacity: 0;
  transition: opacity 0.35s ease;
  pointer-events: none;
  border-radius: 16px;
}
.landing-step:hover::before {
  opacity: 1;
}
.landing-step:hover {
  border-color: rgba(34, 211, 238, 0.2);
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.25);
}
.landing-step-num {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  color: #22d3ee;
  margin-bottom: 0.5rem;
  position: relative;
  z-index: 1;
}
.landing-step h3 {
  font-size: 1.05rem;
  font-weight: 600;
  margin: 0 0 0.5rem 0;
  color: #f8fafc;
  position: relative;
  z-index: 1;
}
.landing-step p {
  margin: 0;
  font-size: 0.9rem;
  line-height: 1.55;
  color: rgba(186, 200, 216, 0.9);
  position: relative;
  z-index: 1;
}

.landing-band {
  position: relative;
  margin: 2.5rem -1.5rem 2.5rem;
  padding: 2.25rem 1.5rem;
  border-radius: 20px;
  border: 1px solid rgba(255, 255, 255, 0.06);
  background: linear-gradient(180deg, rgba(255,255,255,0.03) 0%, rgba(0,0,0,0.15) 100%);
  overflow: hidden;
}
.landing-band::after {
  content: '';
  position: absolute;
  width: 200px;
  height: 200px;
  border-radius: 50%;
  background: rgba(34, 211, 238, 0.08);
  filter: blur(60px);
  top: -80px;
  right: -40px;
  animation: landing-band-glow 10s ease-in-out infinite;
  pointer-events: none;
}
@keyframes landing-band-glow {
  0%, 100% { transform: translate(0, 0); opacity: 0.6; }
  50% { transform: translate(-20px, 30px); opacity: 1; }
}
.landing-band-inner {
  position: relative;
  z-index: 1;
}

.landing-features {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 1rem;
  margin-bottom: 0;
}
.landing-feature {
  position: relative;
  padding: 1.2rem 1.1rem;
  border-radius: 14px;
  border: 1px solid rgba(255,255,255,0.07);
  background: rgba(0,0,0,0.22);
  overflow: hidden;
  transition: transform 0.25s ease, border-color 0.25s ease;
}
.landing-feature::before {
  content: '';
  position: absolute;
  top: 0; left: 0; right: 0;
  height: 2px;
  background: linear-gradient(90deg, transparent, rgba(34, 211, 238, 0.5), transparent);
  transform: translateX(-100%);
  animation: landing-feature-scan 6s ease-in-out infinite;
}
.landing-feature:nth-child(2)::before { animation-delay: -1.5s; }
.landing-feature:nth-child(3)::before { animation-delay: -3s; }
.landing-feature:nth-child(4)::before { animation-delay: -4.5s; }
@keyframes landing-feature-scan {
  0%, 100% { transform: translateX(-100%); }
  50% { transform: translateX(100%); }
}
.landing-feature:hover {
  transform: translateY(-3px);
  border-color: rgba(255, 255, 255, 0.12);
}
.landing-feature h4 {
  font-size: 0.95rem;
  font-weight: 600;
  margin: 0 0 0.4rem 0;
  color: #e2e8f0;
}
.landing-feature p {
  margin: 0;
  font-size: 0.82rem;
  line-height: 1.5;
  color: rgba(148, 163, 184, 0.95);
}

.landing-footer-block {
  margin-top: 3rem;
  padding-top: 2rem;
}
.landing-disclaimer {
  font-size: 0.8rem;
  line-height: 1.55;
  color: rgba(148, 163, 184, 0.88);
  padding: 1.1rem 1.2rem;
  border-radius: 12px;
  border: 1px solid rgba(251, 191, 36, 0.22);
  background: rgba(251, 191, 36, 0.05);
  max-width: 42rem;
}
.landing-shader-hint {
  margin-top: 1.25rem;
  font-size: 0.75rem;
  color: rgba(148, 163, 184, 0.6);
  font-family: 'JetBrains Mono', monospace;
}

@media (max-width: 600px) {
  .landing-inner { padding: 2rem 1rem 4rem; }
  .landing-band { margin-left: -1rem; margin-right: -1rem; }
}
"

landing_panel <- function() {
  use_shader <- is.character(landing_shadertoy_embed_url) &&
    nzchar(landing_shadertoy_embed_url)

  bg_layer <- if (use_shader) {
    tags$div(
      class = "landing-bg-layer",
      tags$iframe(
        class   = "landing-shader-iframe",
        src     = landing_shadertoy_embed_url,
        title   = "Background shader",
        width   = "100%",
        height  = "100%",
        `frameborder` = "0"
      ),
      tags$div(class = "landing-bg-vignette"),
      tags$div(class = "landing-grid-sheen"),
      tags$div(class = "landing-sheen-bar")
    )
  } else {
    tags$div(
      class = "landing-bg-layer landing-bg-fallback",
      tags$div(class = "landing-grid-sheen"),
      tags$div(class = "landing-sheen-bar")
    )
  }

  motion_orbs <- tags$div(
    class = "landing-motion-orbs",
    tags$div(class = "landing-orb landing-orb--1"),
    tags$div(class = "landing-orb landing-orb--2"),
    tags$div(class = "landing-orb landing-orb--3"),
    tags$div(class = "landing-orb landing-orb--4"),
    tags$div(class = "landing-orb landing-orb--5")
  )

  tags$div(
    class = "landing-root",
    bg_layer,
    motion_orbs,
    tags$div(
      class = "landing-inner",
      tags$section(
        class = "landing-hero",
        tags$p(class = "landing-eyebrow", "Portfolio intelligence lab"),
        tags$h1(
          HTML("Decisions first,<br><span class=\"landing-accent\">clarity always</span>")
        ),
        tags$p(
          class = "landing-lead",
          "Build a weighted portfolio, benchmark it, and read risk the way ",
          "self-directed investors actually need: concentration, drawdowns, ",
          "rolling context, and plain-language insight—not a wall of unrelated tickers."
        ),
        tags$div(
          class = "landing-cta-row",
          actionButton("landing_start", "Start with your portfolio",
            class = "btn landing-btn-primary"),
          actionButton("landing_sample", "Try a sample portfolio",
            class = "btn landing-btn-ghost")
        )
      ),

      tags$hr(class = "landing-divider"),

      tags$div(
        class = "landing-section-head",
        tags$p(class = "landing-section-title", "How it works"),
        tags$h2("Three steps. One workflow.")
      ),
      tags$div(
        class = "landing-steps",
        tags$div(
          class = "landing-step",
          tags$div(class = "landing-step-num", "01 — Build"),
          tags$h3("Define the portfolio"),
          tags$p(
            "Enter tickers and weights (or load a template), pick a benchmark ",
            "and history window. Everything downstream keys off this single definition."
          )
        ),
        tags$div(
          class = "landing-step",
          tags$div(class = "landing-step-num", "02 — Diagnose"),
          tags$h3("See risk in context"),
          tags$p(
            "Volatility, Sharpe, drawdowns, beta, contribution to risk, ",
            "correlations, sector mix, and rolling metrics versus your benchmark."
          )
        ),
        tags$div(
          class = "landing-step",
          tags$div(class = "landing-step-num", "03 — Explore"),
          tags$h3("Drill down when you want"),
          tags$p(
            "Performance paths, price trends, forecasts, and ticker-level risk ",
            "charts stay available as supporting evidence—not the main story."
          )
        )
      ),

      tags$div(
        class = "landing-band",
        tags$div(
          class = "landing-band-inner",
          tags$div(
            class = "landing-section-head",
            style = "margin-bottom: 1.5rem;",
            tags$p(class = "landing-section-title", "Capabilities"),
            tags$h2("Built for investors who read numbers.")
          ),
          tags$div(
            class = "landing-features",
            tags$div(
              class = "landing-feature",
              tags$h4("Portfolio-native metrics"),
              tags$p("Weighted returns and covariance-aware risk—not isolated per-symbol summaries.")
            ),
            tags$div(
              class = "landing-feature",
              tags$h4("Benchmark grounding"),
              tags$p("Cumulative paths, excess return, beta, and tracking-style context vs SPY, QQQ, and peers.")
            ),
            tags$div(
              class = "landing-feature",
              tags$h4("Concentration signals"),
              tags$p("Sector weights, HHI-style concentration, and which names drive variance.")
            ),
            tags$div(
              class = "landing-feature",
              tags$h4("Transparent limits"),
              tags$p("Yahoo Finance data, historical windows, models as exploration—no black-box advice.")
            )
          )
        )
      ),

      tags$div(
        class = "landing-footer-block",
        tags$div(
          class = "landing-disclaimer",
          tags$strong("Educational use only. "),
          "This tool does not provide personalized investment, tax, or legal advice. ",
          "Past performance and backtests are not guarantees of future results."
        ),
        if (!use_shader) {
          tags$p(
            class = "landing-shader-hint",
            "Tip: paste a Shadertoy embed URL into ",
            tags$code("landing_shadertoy_embed_url"),
            " in R/config.R to replace the animated gradient background."
          )
        } else {
          NULL
        }
      )
    )
  )
}
