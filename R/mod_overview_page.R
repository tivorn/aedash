source("R/utils/wrang_overview_data.R")

tooltip <- tippy::tippy(icon('question-circle'),"Dados entre 2013 e 2019")

overview_page_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "menu_overview_data",
      fluidRow(
        column(
          width = 5,
          box(
            title = strong("RESUMO DE CASOS", tooltip),
            status = "success",
            id = "cases_summary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              column(
                uiOutput(
                  ns("ui_confirmed_cases")
                ),
                span("Casos confirmados"),
                width = 6
              ),
              column(
                uiOutput(
                  ns("ui_recovered_cases")
                ),
                span("Casos recuperados"),
                width = 6
              )
            ),
            fluidRow(
              column(
                uiOutput(
                  ns("ui_suspected_cases"),
                ),
                span("Casos suspeitos"),
                width = 6
              ),
              column(
                uiOutput(
                  ns("ui_case_incidence")
                ),
                span("Incidência média de casos*"),
                width = 6
              ),
            )
          )
        ),
        column(
          width = 5,
          box(
            title = strong("RESUMO DE ÓBITOS", tooltip),
            status = "success",
            id = "deaths_summary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              column(
                uiOutput(
                  ns("ui_reported_deaths")
                ),
                span("Óbitos reportados"),
                width = 6
              ),
              column(
                uiOutput(
                  ns("ui_letality")
                ),
                span("Letalidade média*"),
                width = 6
              )
            ),
            fluidRow(
              column(
                uiOutput(
                  ns("ui_mortality"),
                ),
                span("Mortalidade média*"),
                width = 6
              ),
              column(
                width = 6
              ),
            )
          )
        )
      ),
      box(
        title = strong("SÍNTESE DE CASOS E ÓBITOS", tooltip),
        status = "success",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 10,
        fluidRow(
          reactableOutput(
            ns("table_dengue_measuraments_by_region"),
            width = "100%"
          ),
          width = 12
        )
      )
      
    )
}

overview_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$ui_confirmed_cases <- renderUI({
      total_confirmed_cases <- sum(dengue_ce$confirmed_cases)
      
      h3(total_confirmed_cases)
    })
    
    output$ui_recovered_cases <- renderUI({
      total_recovered_cases <- sum(dengue_ce$recovered_cases,
                                   na.rm = TRUE)
      
      h3(total_recovered_cases)
    })
    
    output$ui_suspected_cases <- renderUI({
      total_suspected_cases <- sum(dengue_ce$suspected_cases,
                                   na.rm = TRUE)
      
      h3(total_suspected_cases)
    })
    
    output$ui_case_incidence <- renderUI({
      mean_case_incidence <- dengue_measurements %>%
        filter(measurement == "case_incidence") %>%
        pull(mean) %>%
        ceiling()
      
      h3(mean_case_incidence)
    })
    
    output$ui_reported_deaths <- renderUI({
      total_reported_deaths <- sum(dengue_ce$reported_deaths,
                                   na.rm = TRUE)
      
      h3(total_reported_deaths)
    })
    
    output$ui_mortality <- renderUI({
      mean_mortality <- dengue_measurements %>%
        filter(measurement == "mortality") %>%
        pull(mean) %>%
        scales::comma(accuracy = 0.01)
      
      h3(mean_mortality)
    })
    
    output$ui_letality <- renderUI({
      mean_letality <- dengue_measurements %>%
        filter(measurement == "letality") %>%
        pull(mean) %>%
        scales::percent(accuracy = 0.01)
      
      h3(mean_letality)
    })
    
    output$table_dengue_measuraments_by_region <- renderReactable({
      reactable(dengue_measurements_by_region,
                filterable = TRUE,
                columns = list(
                  nome_mesorregiao.x = colDef(name = "Mesorregião"),
                  nome_municipio = colDef(name = "Município"),
                  nome_microrregiao.x = colDef(name = "Microrregião"),
                  total_confirmed_cases = colDef(name = "Casos",
                                                 filterable = FALSE),
                  total_reported_deaths = colDef(name = "Óbitos",
                                                 filterable = FALSE),
                  case_incidence_mean = colDef(name = "Incidência média/100 mil hab.",
                                               filterable = FALSE),
                  mortality_mean = colDef(name = "Mortalidade média/100 mil hab.",
                                          filterable = FALSE)
                )
      )
    })
    
  })
}