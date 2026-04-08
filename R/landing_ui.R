# Landing page (Home): full-width narrative layout; flex fills viewport height.
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
/* Home tab: viewport-bound; scroll only inside landing if needed */
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
  display: flex;
  flex-direction: column;
  min-height: 0;
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
  opacity: 0.5;
  filter: saturate(1.05) contrast(1.03);
}
.landing-bg-vignette {
  position: absolute;
  inset: 0;
  background:
    linear-gradient(180deg, rgba(4, 7, 13, 0.15) 0%, transparent 40%, transparent 60%, rgba(4, 7, 13, 0.88) 100%),
    radial-gradient(ellipse 90% 55% at 50% 25%, transparent 0%, rgba(4, 7, 13, 0.65) 70%, #04070d 100%);
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
  inset: -50%;
  background:
    radial-gradient(ellipse 45% 38% at 25% 18%, rgba(34, 211, 238, 0.13) 0%, transparent 55%),
    radial-gradient(ellipse 42% 45% at 80% 52%, rgba(52, 211, 153, 0.08) 0%, transparent 50%),
    radial-gradient(ellipse 38% 35% at 48% 88%, rgba(99, 102, 241, 0.07) 0%, transparent 46%);
  animation: landing-orbit 22s ease-in-out infinite;
}
.landing-bg-fallback::after {
  content: '';
  position: absolute;
  inset: 0;
  background-image: url(\"data:image/svg+xml,%3Csvg viewBox='0 0 256 256' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='n'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.85' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23n)' opacity='0.04'/%3E%3C/svg%3E\");
  opacity: 0.38;
  mix-blend-mode: overlay;
  pointer-events: none;
}
@keyframes landing-orbit {
  0%, 100% { transform: translate(0, 0) rotate(0deg); }
  33% { transform: translate(4%, 3%) rotate(3deg); }
  66% { transform: translate(-3%, 2%) rotate(-2deg); }
}

.landing-grid-sheen {
  position: absolute;
  inset: 0;
  z-index: 0;
  pointer-events: none;
  background-image:
    linear-gradient(rgba(34, 211, 238, 0.032) 1px, transparent 1px),
    linear-gradient(90deg, rgba(34, 211, 238, 0.032) 1px, transparent 1px);
  background-size: 48px 48px;
  animation: landing-grid-drift 40s linear infinite;
}
@keyframes landing-grid-drift {
  0% { background-position: 0 0, 0 0; }
  100% { background-position: 48px 48px, 48px 48px; }
}
.landing-sheen-bar {
  position: absolute;
  left: -20%;
  width: 50%;
  height: 100%;
  top: 0;
  background: linear-gradient(105deg, transparent 0%, rgba(34, 211, 238, 0.04) 45%, rgba(255,255,255,0.02) 50%, transparent 72%);
  animation: landing-sheen-move 14s ease-in-out infinite;
  z-index: 0;
  pointer-events: none;
}
@keyframes landing-sheen-move {
  0%, 100% { transform: translateX(0); opacity: 0.45; }
  50% { transform: translateX(200%); opacity: 0.85; }
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
  filter: blur(72px);
  opacity: 0.42;
  animation: landing-orb-float 18s ease-in-out infinite;
}
.landing-orb--1 {
  width: 300px; height: 300px;
  background: rgba(34, 211, 238, 0.32);
  top: 5%; left: -6%;
}
.landing-orb--2 {
  width: 260px; height: 260px;
  background: rgba(52, 211, 153, 0.2);
  top: 38%; right: -8%;
  animation-delay: -4s;
  animation-duration: 22s;
}
.landing-orb--3 {
  width: 220px; height: 220px;
  background: rgba(99, 102, 241, 0.18);
  top: 62%; left: 18%;
  animation-delay: -8s;
  animation-duration: 20s;
}
.landing-orb--4 {
  width: 200px; height: 200px;
  background: rgba(34, 211, 238, 0.14);
  bottom: 8%; right: 22%;
  animation-delay: -2s;
  animation-duration: 24s;
}
.landing-orb--5 {
  width: 340px; height: 340px;
  background: rgba(14, 165, 233, 0.1);
  bottom: -8%; left: 38%;
  animation-delay: -10s;
  animation-duration: 26s;
}
@keyframes landing-orb-float {
  0%, 100% { transform: translate(0, 0) scale(1); }
  33% { transform: translate(24px, -20px) scale(1.05); }
  66% { transform: translate(-16px, 14px) scale(0.97); }
}

/* Content column: fills height; scroll inside if viewport is short */
.landing-inner {
  position: relative;
  z-index: 2;
  flex: 1 1 auto;
  min-height: 0;
  width: 100%;
  max-width: 1180px;
  margin: 0 auto;
  padding: clamp(0.6rem, 1.8vh, 1.1rem) clamp(1rem, 3vw, 2rem);
  box-sizing: border-box;
  overflow-x: hidden;
  overflow-y: auto;
  overscroll-behavior: contain;
  display: flex;
  flex-direction: column;
  gap: clamp(0.4rem, 1.2vh, 0.85rem);
}

/* Grows to absorb vertical space so sections stretch */
.landing-fill {
  flex: 1 1 auto;
  min-height: 0;
  display: flex;
  flex-direction: column;
  gap: clamp(0.45rem, 1.2vh, 0.9rem);
}

.landing-eyebrow {
  font-family: 'JetBrains Mono', ui-monospace, monospace;
  font-size: 0.72rem;
  letter-spacing: 0.2em;
  text-transform: uppercase;
  color: rgba(34, 211, 238, 0.88);
  margin-bottom: 0.25rem;
}
.landing-hero h1 {
  font-size: clamp(1.85rem, 4vw, 3rem);
  font-weight: 700;
  line-height: 1.1;
  letter-spacing: -0.03em;
  margin: 0 0 0.45rem 0;
  color: #f4f8fc;
}
.landing-hero h1 .landing-accent {
  color: #5eead4;
  font-weight: 600;
}
.landing-lead {
  font-size: clamp(0.95rem, 1.5vw, 1.15rem);
  line-height: 1.55;
  color: rgba(200, 214, 226, 0.9);
  margin: 0 0 0.65rem 0;
  max-width: 40rem;
}
.landing-cta-row {
  display: flex;
  flex-wrap: wrap;
  gap: 0.65rem;
  align-items: center;
}
.landing-btn-primary {
  font-family: inherit;
  font-weight: 600;
  font-size: 0.95rem;
  padding: 0.62rem 1.35rem;
  border-radius: 999px;
  border: none;
  background: linear-gradient(135deg, #22d3ee 0%, #14b8a6 100%);
  color: #041016 !important;
  box-shadow: 0 4px 24px rgba(34, 211, 238, 0.25);
}
.landing-btn-primary:hover { filter: brightness(1.08); color: #041016 !important; }
.landing-btn-ghost {
  font-family: inherit;
  font-weight: 500;
  font-size: 0.95rem;
  padding: 0.62rem 1.35rem;
  border-radius: 999px;
  border: 1px solid rgba(255,255,255,0.18);
  background: rgba(255,255,255,0.045);
  color: #e8eef4 !important;
}
.landing-btn-ghost:hover {
  background: rgba(255,255,255,0.1);
  color: #fff !important;
}

.landing-divider {
  height: 2px;
  margin: 0.1rem 0;
  flex-shrink: 0;
  border: none;
  background: linear-gradient(90deg, transparent, rgba(34, 211, 238, 0.4), rgba(52, 211, 153, 0.28), transparent);
  background-size: 200% 100%;
  animation: landing-divider-shine 8s ease infinite;
  opacity: 0.9;
}
@keyframes landing-divider-shine {
  0%, 100% { background-position: 0% 50%; }
  50% { background-position: 100% 50%; }
}

.landing-section-title {
  font-size: 0.7rem;
  font-family: 'JetBrains Mono', monospace;
  letter-spacing: 0.2em;
  text-transform: uppercase;
  color: rgba(148, 163, 184, 0.92);
  margin: 0 0 0.2rem 0;
}
.landing-section-head h2 {
  font-size: clamp(1.35rem, 2.6vw, 1.85rem);
  font-weight: 600;
  color: #f1f5f9;
  margin: 0 0 0.35rem 0;
  letter-spacing: -0.02em;
}

/* Steps: full width, share extra vertical space */
.landing-steps-wrap {
  flex: 1 1 0;
  min-height: 0;
  display: flex;
  flex-direction: column;
}
.landing-steps {
  flex: 1 1 auto;
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: clamp(0.55rem, 1.5vw, 1rem);
  align-items: stretch;
  min-height: min(28vh, 220px);
}
.landing-step {
  position: relative;
  padding: clamp(0.65rem, 1.5vh, 1.1rem) clamp(0.75rem, 1.8vw, 1.15rem);
  border-radius: 14px;
  background: rgba(255,255,255,0.04);
  border: 1px solid rgba(255,255,255,0.09);
  backdrop-filter: blur(12px);
  -webkit-backdrop-filter: blur(12px);
  overflow: hidden;
  display: flex;
  flex-direction: column;
  transition: border-color 0.3s ease, box-shadow 0.3s ease;
}
.landing-step::before {
  content: '';
  position: absolute;
  inset: -1px;
  background: linear-gradient(135deg, rgba(34, 211, 238, 0.12), transparent 45%, rgba(52, 211, 153, 0.06));
  opacity: 0;
  transition: opacity 0.35s ease;
  pointer-events: none;
  border-radius: 14px;
}
.landing-step:hover::before { opacity: 1; }
.landing-step:hover {
  border-color: rgba(34, 211, 238, 0.22);
  box-shadow: 0 8px 28px rgba(0, 0, 0, 0.3);
}
.landing-step-num {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.74rem;
  color: #22d3ee;
  margin-bottom: 0.35rem;
  position: relative;
  z-index: 1;
}
.landing-step h3 {
  font-size: clamp(0.95rem, 1.4vw, 1.08rem);
  font-weight: 600;
  margin: 0 0 0.4rem 0;
  color: #f8fafc;
  position: relative;
  z-index: 1;
}
.landing-step p {
  margin: 0;
  flex: 1 1 auto;
  font-size: clamp(0.8rem, 1.15vw, 0.9rem);
  line-height: 1.5;
  color: rgba(186, 200, 216, 0.92);
  position: relative;
  z-index: 1;
}

/* Capabilities band: flexes to fill remaining height */
.landing-band {
  position: relative;
  flex: 1 1 0;
  min-height: min(32vh, 260px);
  margin: 0;
  padding: clamp(0.65rem, 1.6vh, 1.1rem) clamp(0.85rem, 2vw, 1.35rem);
  border-radius: 16px;
  border: 1px solid rgba(255, 255, 255, 0.07);
  background: linear-gradient(180deg, rgba(255,255,255,0.04) 0%, rgba(0,0,0,0.2) 100%);
  overflow: hidden;
  display: flex;
  flex-direction: column;
}
.landing-band::after {
  content: '';
  position: absolute;
  width: 220px;
  height: 220px;
  border-radius: 50%;
  background: rgba(34, 211, 238, 0.09);
  filter: blur(64px);
  top: -60px;
  right: -30px;
  animation: landing-band-glow 10s ease-in-out infinite;
  pointer-events: none;
}
@keyframes landing-band-glow {
  0%, 100% { transform: translate(0, 0); opacity: 0.55; }
  50% { transform: translate(-16px, 22px); opacity: 1; }
}
.landing-band-inner {
  position: relative;
  z-index: 1;
  flex: 1 1 auto;
  min-height: 0;
  display: flex;
  flex-direction: column;
  gap: 0.45rem;
}
.landing-features {
  flex: 1 1 auto;
  display: grid;
  grid-template-columns: repeat(4, 1fr);
  gap: clamp(0.5rem, 1.2vw, 0.85rem);
  align-items: stretch;
  min-height: 0;
}
.landing-feature {
  position: relative;
  padding: clamp(0.55rem, 1.2vh, 0.85rem) clamp(0.6rem, 1.2vw, 0.9rem);
  border-radius: 12px;
  border: 1px solid rgba(255,255,255,0.07);
  background: rgba(0,0,0,0.22);
  overflow: hidden;
  display: flex;
  flex-direction: column;
  transition: transform 0.25s ease, border-color 0.25s ease;
}
.landing-feature::before {
  content: '';
  position: absolute;
  top: 0; left: 0; right: 0;
  height: 2px;
  background: linear-gradient(90deg, transparent, rgba(34, 211, 238, 0.45), transparent);
  transform: translateX(-100%);
  animation: landing-feature-scan 7s ease-in-out infinite;
}
.landing-feature:nth-child(2)::before { animation-delay: -1.75s; }
.landing-feature:nth-child(3)::before { animation-delay: -3.5s; }
.landing-feature:nth-child(4)::before { animation-delay: -5.25s; }
@keyframes landing-feature-scan {
  0%, 100% { transform: translateX(-100%); }
  50% { transform: translateX(100%); }
}
.landing-feature:hover {
  transform: translateY(-2px);
  border-color: rgba(255, 255, 255, 0.12);
}
.landing-feature h4 {
  font-size: clamp(0.82rem, 1.2vw, 0.95rem);
  font-weight: 600;
  margin: 0 0 0.3rem 0;
  color: #e2e8f0;
}
.landing-feature p {
  margin: 0;
  flex: 1 1 auto;
  font-size: clamp(0.72rem, 1.05vw, 0.82rem);
  line-height: 1.45;
  color: rgba(148, 163, 184, 0.95);
}

.landing-footer-block {
  flex-shrink: 0;
  display: flex;
  flex-wrap: wrap;
  align-items: flex-start;
  gap: 0.75rem 1.25rem;
  padding-top: 0.35rem;
}
.landing-disclaimer {
  flex: 1 1 280px;
  font-size: 0.78rem;
  line-height: 1.5;
  color: rgba(148, 163, 184, 0.9);
  padding: 0.65rem 0.85rem;
  border-radius: 10px;
  border: 1px solid rgba(251, 191, 36, 0.22);
  background: rgba(251, 191, 36, 0.06);
  min-width: 0;
}
.landing-shader-hint {
  font-size: 0.72rem;
  color: rgba(148, 163, 184, 0.6);
  font-family: 'JetBrains Mono', monospace;
  margin: 0;
  align-self: center;
  max-width: 22rem;
}

@media (max-width: 992px) {
  .landing-steps {
    grid-template-columns: 1fr;
    min-height: 0;
  }
  .landing-features {
    grid-template-columns: repeat(2, 1fr);
    min-height: 0;
  }
  .landing-band { min-height: 0; }
}

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
  body.app-home-tab > .container-fluid > .tab-content,
  body.app-home-tab > .container-fluid > .tab-content > .tab-pane.active {
    overflow: visible !important;
    height: auto;
    max-height: none;
  }
  .landing-root {
    height: auto;
    min-height: calc(100vh - 52px);
    overflow: visible;
  }
  .landing-inner {
    overflow: visible;
    padding-bottom: 2rem;
  }
  .landing-fill { flex: none; }
  .landing-steps-wrap,
  .landing-band { flex: none; min-height: 0; }
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
        tags$p(class = "landing-eyebrow", "Axis"),
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
        class = "landing-fill",
        tags$div(
          class = "landing-steps-wrap",
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
                "Performance paths, scenario replays, Holdings Explorer (trends and forecasts), and ticker-level risk ",
                "charts stay available as supporting evidence—not the main story."
              )
            )
          )
        ),

        tags$div(
          class = "landing-band",
          tags$div(
            class = "landing-band-inner",
            tags$div(
              class = "landing-section-head",
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
              ),
              tags$div(
                class = "landing-feature",
                tags$h4("Scenarios & stress"),
                tags$p("Replay known rough patches on your actual weights, then layer a simple daily return shock to see a mechanical what-if path.")
              )
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
        tags$div(
          class = "landing-footer-actions",
          style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: center; margin-top: 16px;",
          actionButton("landing_methodology", "Methodology",
            class = "btn btn-sm landing-btn-ghost"),
          actionButton("landing_scenarios", "Scenarios",
            class = "btn btn-sm landing-btn-ghost")
        ),
        if (!use_shader) {
          tags$p(
            class = "landing-shader-hint",
            "Tip: paste a Shadertoy embed URL into ",
            tags$code("landing_shadertoy_embed_url"),
            " in R/config.R for a custom shader background."
          )
        } else {
          NULL
        }
      )
    )
  )
}
