# Shiny entry point. R/ helpers are sourced from global.R before this file runs.

shinyApp(ui = app_ui(), server = app_server)
