# Landing page (Home): single viewport on desktop, matches global dark theme.
# ShaderToy: `landing_shadertoy_embed_url` in R/config.R

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
  if (window.jQuery && window.Shiny) {
    Shiny.addCustomMessageHandler('setHomeTab', function (msg) {
      document.body.classList.toggle('app-home-tab', !!msg.home);
    });
    $(document).on('shiny:inputchanged', function (e) {
      if (e.name !== 'main_nav') return;
      document.body.classList.toggle('app-home-tab', e.value === 'Home');
    });
  }
})();
"

landing_page_css <- "
/* Home tab: no page scroll; content fits the viewport (desktop) */
body.app-home-tab {
  overflow: hidden !important;
  height: 100vh !important;
  max-height: 100vh !important;
}
body.app-home-tab > .container-fluid {
  height: 100vh;
  max-height: 100vh;
  display: flex;
  flex-direction: column;
  overflow: hidden;
}
body.app-home-tab > .container-fluid > .tab-content {
  flex: 1 1 auto;
  min-height: 0;
  overflow: hidden !important;
  padding: 0 !important;
  margin: 0 !important;
}
body.app-home-tab > .container-fluid > .tab-content > .tab-pane.active {
  height: 100%;
  max-height: 100%;
  overflow: hidden !important;
  padding: 0 !important;
  margin: 0 !important;
}

/* Landing shell */
.landing-root {
  font-family: 'DM Sans', system-ui, -apple-system, sans-serif;
  color: #e8eef4;
  position: relative;
  isolation: isolate;
  height: 100%;
  max-height: 100%;
  margin: 0;
  padding: 0;
  overflow: hidden;
  background-color: #04070d;
}
.landing-bg-layer {
  position: absolute;
  inset: 0;
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
  opacity: 0.48;
  filter: saturate(1.05) contrast(1.03);
}
.landing-bg-vignette {
  position: absolute;
  inset: 0;
  background: radial-gradient(ellipse 85% 65% at 50% 30%, transparent 0%, rgba(4, 7, 13, 0.7) 65%, #04070d 100%);
  pointer-events: none;
}
.landing-bg-fallback {
  position: absolute;
  inset: 0;
  background: linear-gradient(165deg, #04070d 0%, #0a1524 45%, #07120f 100%);
}
.landing-bg-fallback::before {
  content: '';
  position: absolute;
  inset: -40%;
  background:
    radial-gradient(ellipse 40% 35% at 20% 25%, rgba(34, 211, 238, 0.12) 0%, transparent 55%),
    radial-gradient(ellipse 38% 40% at 85% 50%, rgba(52, 211, 153, 0.07) 0%, transparent 50%),
    radial-gradient(ellipse 35% 30% at 45% 85%, rgba(99, 102, 241, 0.06) 0%, transparent 48%);
  animation: landing-orbit 20s ease-in-out infinite;
}
.landing-bg-fallback::after {
  content: '';
  position: absolute;
  inset: 0;
  background-image: url(\"data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.035'/%3E%3C/svg%3E\");
  opacity: 0.4;
  mix-blend-mode: overlay;
  pointer-events: none;
}
@keyframes landing-orbit {
  0%, 100% { transform: translate(0, 0) rotate(0deg); }
  50% { transform: translate(3%, 2%) rotate(2deg); }
}

.landing-grid-sheen {
  position: absolute;
  inset: 0;
  z-index: 0;
  pointer-events: none;
  background-image:
    linear-gradient(rgba(34, 211, 238, 0.028) 1px, transparent 1px),
    linear-gradient(90deg, rgba(34, 211, 238, 0.028) 1px, transparent 1px);
  background-size: 40px 40px;
  animation: landing-grid-drift 36s linear infinite;
}
@keyframes landing-grid-drift {
  0% { background-position: 0 0, 0 0; }
  100% { background-position: 40px 40px, 40px 40px; }
}
.landing-sheen-bar {
  position: absolute;
  left: -15%;
  width: 45%;
  height: 100%;
  top: 0;
  background: linear-gradient(100deg, transparent 0%, rgba(34, 211, 238, 0.035) 45%, transparent 70%);
  animation: landing-sheen-move 12s ease-in-out infinite;
  z-index: 0;
  pointer-events: none;
}
@keyframes landing-sheen-move {
  0%, 100% { transform: translateX(0); opacity: 0.4; }
  50% { transform: translateX(220%); opacity: 0.75; }
}

.landing-motion-orbs {
  position: absolute;
  inset: 0;
  z-index: 0;
  pointer-events: none;
  overflow: hidden;
}
.landing-orb {
  position: absolute;
  border-radius: 50%;
  filter: blur(64px);
  opacity: 0.4;
  animation: landing-orb-float 16s ease-in-out infinite;
}
.landing-orb--1 {
  width: 240px; height: 240px;
  background: rgba(34, 211, 238, 0.3);
  top: -5%; left: -5%;
}
.landing-orb--2 {
  width: 200px; height: 200px;
  background: rgba(52, 211, 153, 0.18);
  top: 40%; right: -8%;
  animation-delay: -5s;
  animation-duration: 19s;
}
.landing-orb--3 {
  width: 280px; height: 280px;
  background: rgba(99, 102, 241, 0.12);
  bottom: -10%; left: 35%;
  animation-delay: -8s;
  animation-duration: 22s;
}
@keyframes landing-orb-float {
  0%, 100% { transform: translate(0, 0) scale(1); }
  50% { transform: translate(12px, -16px) scale(1.04); }
}

/* Content: fills viewport, two columns */
.landing-inner {
  position: relative;
  z-index: 2;
  height: 100%;
  max-height: 100%;
  padding: clamp(0.75rem, 2vh, 1.25rem) clamp(1rem, 3vw, 1.75rem);
  box-sizing: border-box;
  display: flex;
  flex-direction: column;
  min-height: 0;
}
.landing-main-grid {
  flex: 1 1 auto;
  min-height: 0;
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-template-rows: 1fr;
  gap: clamp(0.75rem, 2vw, 1.5rem);
  align-items: center;
}
.landing-hero-col {
  display: flex;
  flex-direction: column;
  justify-content: center;
  min-width: 0;
}
.landing-side-col {
  display: flex;
  flex-direction: column;
  gap: 0.65rem;
  min-width: 0;
  min-height: 0;
  overflow: hidden;
  justify-content: center;
}
.landing-bottom-row {
  flex-shrink: 0;
  display: flex;
  flex-wrap: wrap;
  align-items: flex-start;
  gap: 0.65rem 1rem;
  padding-top: 0.5rem;
  margin-top: 0.25rem;
  border-top: 1px solid rgba(255, 255, 255, 0.08);
}

.landing-eyebrow {
  font-family: 'JetBrains Mono', ui-monospace, monospace;
  font-size: 0.65rem;
  letter-spacing: 0.2em;
  text-transform: uppercase;
  color: rgba(34, 211, 238, 0.85);
  margin-bottom: 0.35rem;
}
.landing-hero h1 {
  font-size: clamp(1.65rem, 3.2vw, 2.35rem);
  font-weight: 700;
  line-height: 1.1;
  letter-spacing: -0.03em;
  margin: 0 0 0.5rem 0;
  color: #f4f8fc;
}
.landing-hero h1 .landing-accent {
  color: #5eead4;
  font-weight: 600;
}
.landing-lead {
  font-size: clamp(0.85rem, 1.35vw, 0.98rem);
  line-height: 1.5;
  color: rgba(200, 214, 226, 0.88);
  margin: 0 0 0.75rem 0;
  max-width: 28rem;
}
.landing-cta-row {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  align-items: center;
}
.landing-btn-primary {
  font-family: inherit;
  font-weight: 600;
  font-size: 0.85rem;
  padding: 0.5rem 1.1rem;
  border-radius: 999px;
  border: none;
  background: linear-gradient(135deg, #22d3ee 0%, #14b8a6 100%);
  color: #041016 !important;
  box-shadow: 0 4px 20px rgba(34, 211, 238, 0.22);
}
.landing-btn-primary:hover { filter: brightness(1.08); color: #041016 !important; }
.landing-btn-ghost {
  font-family: inherit;
  font-weight: 500;
  font-size: 0.85rem;
  padding: 0.5rem 1.1rem;
  border-radius: 999px;
  border: 1px solid rgba(255,255,255,0.16);
  background: rgba(255,255,255,0.04);
  color: #e8eef4 !important;
}
.landing-btn-ghost:hover {
  background: rgba(255,255,255,0.08);
  color: #fff !important;
}

.landing-micro-title {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.6rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: rgba(148, 163, 184, 0.9);
  margin: 0 0 0.35rem 0;
}
.landing-steps-compact {
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}
.landing-step-mini {
  padding: 0.45rem 0.55rem;
  border-radius: 10px;
  background: rgba(255,255,255,0.04);
  border: 1px solid rgba(255,255,255,0.08);
}
.landing-step-mini-num {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.65rem;
  color: #22d3ee;
  margin-bottom: 0.15rem;
}
.landing-step-mini h3 {
  font-size: 0.78rem;
  font-weight: 600;
  margin: 0 0 0.15rem 0;
  color: #f1f5f9;
}
.landing-step-mini p {
  margin: 0;
  font-size: 0.68rem;
  line-height: 1.4;
  color: rgba(186, 200, 216, 0.88);
}

.landing-features-compact {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 0.4rem;
}
.landing-feature-mini {
  padding: 0.45rem 0.5rem;
  border-radius: 8px;
  border: 1px solid rgba(255,255,255,0.06);
  background: rgba(0,0,0,0.22);
}
.landing-feature-mini h4 {
  font-size: 0.72rem;
  font-weight: 600;
  margin: 0 0 0.15rem 0;
  color: #e2e8f0;
}
.landing-feature-mini p {
  margin: 0;
  font-size: 0.62rem;
  line-height: 1.35;
  color: rgba(148, 163, 184, 0.92);
}

.landing-disclaimer {
  flex: 1 1 220px;
  font-size: 0.68rem;
  line-height: 1.45;
  color: rgba(148, 163, 184, 0.9);
  padding: 0.5rem 0.65rem;
  border-radius: 8px;
  border: 1px solid rgba(251, 191, 36, 0.2);
  background: rgba(251, 191, 36, 0.05);
  min-width: 0;
}
.landing-shader-hint {
  font-size: 0.62rem;
  color: rgba(148, 163, 184, 0.55);
  font-family: 'JetBrains Mono', monospace;
  max-width: 16rem;
  margin: 0;
  align-self: center;
}

/* Wrap grid + bottom row */
.landing-stack {
  display: flex;
  flex-direction: column;
  height: 100%;
  max-height: 100%;
  min-height: 0;
}
.landing-stack .landing-main-grid { flex: 1; min-height: 0; }

@media (max-width: 900px) {
  body.app-home-tab {
    overflow: auto !important;
    height: auto !important;
    max-height: none !important;
  }
  body.app-home-tab > .container-fluid {
    height: auto;
    max-height: none;
    overflow: visible;
  }
  body.app-home-tab > .container-fluid > .tab-content {
    overflow: visible !important;
    min-height: auto;
  }
  body.app-home-tab > .container-fluid > .tab-content > .tab-pane.active {
    height: auto;
    max-height: none;
    overflow: visible !important;
  }
  .landing-root {
    height: auto;
    min-height: calc(100vh - 52px);
    overflow: visible;
  }
  .landing-inner {
    height: auto;
    max-height: none;
    overflow: visible;
    padding-bottom: 2rem;
  }
  .landing-main-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
  .landing-features-compact {
    grid-template-columns: 1fr;
  }
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
    tags$div(class = "landing-orb landing-orb--3")
  )

  tags$div(
    class = "landing-root",
    bg_layer,
    motion_orbs,
    tags$div(
      class = "landing-inner",
      tags$div(
        class = "landing-stack",
        tags$div(
          class = "landing-main-grid",
          tags$div(
            class = "landing-hero-col",
            tags$section(
              class = "landing-hero",
              tags$p(class = "landing-eyebrow", "Portfolio intelligence lab"),
              tags$h1(
                HTML("Decisions first,<br><span class=\"landing-accent\">clarity always</span>")
              ),
              tags$p(
                class = "landing-lead",
                "Weighted portfolios, benchmark context, and risk diagnostics ",
                "with plain-language insight—built for self-directed investors who want rigor without noise."
              ),
              tags$div(
                class = "landing-cta-row",
                actionButton("landing_start", "Start with your portfolio",
                  class = "btn landing-btn-primary"),
                actionButton("landing_sample", "Try a sample portfolio",
                  class = "btn landing-btn-ghost")
              )
            )
          ),
          tags$div(
            class = "landing-side-col",
            tags$p(class = "landing-micro-title", "How it works"),
            tags$div(
              class = "landing-steps-compact",
              tags$div(
                class = "landing-step-mini",
                tags$div(class = "landing-step-mini-num", "01 — Build"),
                tags$h3("Define holdings & benchmark"),
                tags$p("Tickers, weights, template or custom, plus history window.")
              ),
              tags$div(
                class = "landing-step-mini",
                tags$div(class = "landing-step-mini-num", "02 — Diagnose"),
                tags$h3("Risk in context"),
                tags$p("Volatility, Sharpe, drawdowns, beta, contributions, correlations.")
              ),
              tags$div(
                class = "landing-step-mini",
                tags$div(class = "landing-step-mini-num", "03 — Explore"),
                tags$h3("Evidence on demand"),
                tags$p("Performance, trends, forecasts—supporting views, not the main story.")
              )
            ),
            tags$p(class = "landing-micro-title", "Capabilities"),
            tags$div(
              class = "landing-features-compact",
              tags$div(
                class = "landing-feature-mini",
                tags$h4("Portfolio-first"),
                tags$p("Weighted returns & covariance-aware risk.")
              ),
              tags$div(
                class = "landing-feature-mini",
                tags$h4("Vs benchmark"),
                tags$p("Paths, excess return, beta, rolling view.")
              ),
              tags$div(
                class = "landing-feature-mini",
                tags$h4("Concentration"),
                tags$p("Sectors, HHI, what drives variance.")
              ),
              tags$div(
                class = "landing-feature-mini",
                tags$h4("Transparent"),
                tags$p("Yahoo data, clear limits, no black-box advice.")
              )
            )
          )
        ),
        tags$div(
          class = "landing-bottom-row",
          tags$div(
            class = "landing-disclaimer",
            tags$strong("Educational use only. "),
            "Not personalized investment, tax, or legal advice. Past performance is not a guarantee of future results."
          ),
          if (!use_shader) {
            tags$p(
              class = "landing-shader-hint",
              "Shader background: set ",
              tags$code("landing_shadertoy_embed_url"),
              " in R/config.R"
            )
          } else {
            NULL
          }
        )
      )
    )
  )
}
