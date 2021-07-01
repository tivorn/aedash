library(shiny)
library(bs4Dash)
library(fresh)
library(dplyr)
library(plotly)
library(reactable)

theme <- create_theme(
  bs4dash_status(
    primary = "#FF6F0A"
  )
)

about_text <- "Este painel foi desenvolvido como projeto final do curso de Dashboard em Shiny da Curso-R, embora os dados sejam oficiais obtidos a partir do Sistema de Agravos e Notificações, devem ser analisados com cautela. "

header = dashboardHeader(
  header = img(src = "logo.svg")
)

sidebar = dashboardSidebar(
  collapsed = TRUE,
  skin = "light",
  elevation = 1,
  sidebarMenu(
    menuItem(
      icon = icon("table"),
      "Visão geral",
      tabName = "menu_overview_data"
    ),
    menuItem(
      icon = icon("chart-line"),
      "Séries temporais",
      tabName =  "menu_time_series_data"
    ),
    menuItem(
      icon = icon("info-circle"),
      "Sobre",
      tabName = "menu_info"
    )
  )
)

body = dashboardBody(
  use_waiter(),
  tabItems(
    timeseries_page_ui("timeseries"),
    overview_page_ui("overview"),
    
    tabItem(
      tabName = "menu_info",
      box(
        title = "Sobre",
        p(about_text)
      )
    )
  )
)

ui <- dashboardPage(
  freshTheme = theme,
  header,
  sidebar,
  body
)

  server <- function(input, output, session) {
  
    timeseries_page_server("timeseries")
    
    overview_page_server("overview")
}

shinyApp(ui, server)