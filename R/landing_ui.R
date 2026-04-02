# Landing page markup and styles (Home tab).
# ShaderToy: set `landing_shadertoy_embed_url` in R/config.R to the embed URL from
# Shadertoy's "Embed" dialog (iframe src). Leave "" for the built-in gradient fallback.

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
    tags$style(HTML(landing_page_css))
  )
}

landing_page_css <- "
/* --- Landing (Home) tab: dark, full-bleed hero + sections --- */
.landing-root {
  font-family: 'DM Sans', system-ui, -apple-system, sans-serif;
  color: #e8eef4;
  position: relative;
  min-height: calc(100vh - 56px);
  margin: -15px -15px 0 -15px;
  overflow-x: hidden;
}
.landing-bg-layer {
  position: absolute;
  inset: 0;
  z-index: 0;
}
.landing-shader-iframe {
  position: absolute;
  inset: 0;
  width: 100%;
  height: 100%;
  border: 0;
  pointer-events: none;
  opacity: 0.55;
  filter: saturate(1.1) contrast(1.05);
}
.landing-bg-vignette {
  position: absolute;
  inset: 0;
  background: radial-gradient(ellipse 80% 70% at 50% 40%, transparent 0%, rgba(5,8,14,0.85) 70%, #05080e 100%);
  pointer-events: none;
}
.landing-bg-fallback {
  position: absolute;
  inset: 0;
  background: linear-gradient(135deg, #05080e 0%, #0a1628 35%, #0c1f18 65%, #050a12 100%);
}
.landing-bg-fallback::before {
  content: '';
  position: absolute;
  inset: -50%;
  background:
    radial-gradient(ellipse 50% 40% at 30% 20%, rgba(34, 211, 238, 0.12) 0%, transparent 55%),
    radial-gradient(ellipse 45% 50% at 75% 60%, rgba(52, 211, 153, 0.08) 0%, transparent 50%),
    radial-gradient(ellipse 40% 35% at 50% 90%, rgba(99, 102, 241, 0.06) 0%, transparent 45%);
  animation: landing-orbit 22s ease-in-out infinite;
}
.landing-bg-fallback::after {
  content: '';
  position: absolute;
  inset: 0;
  background-image: url(\"data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.04'/%3E%3C/svg%3E\");
  opacity: 0.35;
  mix-blend-mode: overlay;
  pointer-events: none;
}
@keyframes landing-orbit {
  0%, 100% { transform: translate(0, 0) rotate(0deg); }
  33% { transform: translate(4%, 3%) rotate(3deg); }
  66% { transform: translate(-3%, 2%) rotate(-2deg); }
}
.landing-inner {
  position: relative;
  z-index: 1;
  max-width: 1120px;
  margin: 0 auto;
  padding: 3rem 1.5rem 4rem;
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
  margin-bottom: 2.5rem;
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
.landing-section h2 {
  font-size: clamp(1.5rem, 3vw, 2rem);
  font-weight: 600;
  color: #f1f5f9;
  margin: 0 0 2rem 0;
  letter-spacing: -0.02em;
}
.landing-steps {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
  gap: 1.25rem;
  margin-bottom: 3.5rem;
}
.landing-step {
  padding: 1.35rem 1.25rem;
  border-radius: 14px;
  background: rgba(255,255,255,0.03);
  border: 1px solid rgba(255,255,255,0.08);
  backdrop-filter: blur(10px);
}
.landing-step-num {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.75rem;
  color: #22d3ee;
  margin-bottom: 0.5rem;
}
.landing-step h3 {
  font-size: 1.05rem;
  font-weight: 600;
  margin: 0 0 0.5rem 0;
  color: #f8fafc;
}
.landing-step p {
  margin: 0;
  font-size: 0.9rem;
  line-height: 1.55;
  color: rgba(186, 200, 216, 0.9);
}
.landing-features {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 1rem;
  margin-bottom: 2.5rem;
}
.landing-feature {
  padding: 1.15rem 1.1rem;
  border-radius: 12px;
  border: 1px solid rgba(255,255,255,0.06);
  background: rgba(0,0,0,0.2);
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
.landing-disclaimer {
  font-size: 0.8rem;
  line-height: 1.55;
  color: rgba(148, 163, 184, 0.85);
  padding: 1rem 1.15rem;
  border-radius: 10px;
  border: 1px solid rgba(251, 191, 36, 0.2);
  background: rgba(251, 191, 36, 0.06);
  max-width: 42rem;
}
.landing-shader-hint {
  margin-top: 1.25rem;
  font-size: 0.75rem;
  color: rgba(148, 163, 184, 0.65);
  font-family: 'JetBrains Mono', monospace;
}
@media (max-width: 600px) {
  .landing-inner { padding: 2rem 1rem 3rem; }
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
        `allowfullscreen` = NA,
        `frameborder`     = "0"
      ),
      tags$div(class = "landing-bg-vignette")
    )
  } else {
    tags$div(class = "landing-bg-layer landing-bg-fallback")
  }

  tags$div(
    class = "landing-root",
    bg_layer,
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

      tags$p(class = "landing-section-title", "How it works"),
      tags$h2("Three steps. One workflow."),
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

      tags$p(class = "landing-section-title", "Capabilities"),
      tags$h2("Built for investors who read numbers."),
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
      ),

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
}
