
dengue_ce <- here::here("data", "dengue_ce.csv") %>%
  readr::read_csv()

timeseries_page_ui <- function(id) {
  ns <- NS(id)
  
  distinct_mesoregion <- dengue_ce %>%
    distinct(nome_mesorregiao) %>%
    pull()
  
  tabItem(
    tabName =  "menu_time_series_data",
      
      box(
        title = "Filtros",
        fluidRow(
          column(
            dateRangeInput(
              ns("dt_notific"),
              "Período",
              start = "2017-01-31",
              end = "2017-07-12",
              language = "pt-BR",
              separator = "até"
            ),
            selectInput(
              ns("select_mesoregion"),
              label = "Mesorregião",
              choices = distinct_mesoregion,
              selected = "Metropolitana de Fortaleza"
            ),
            width = 4
          ),
          
          column(
            selectInput(
              ns("select_microregion"),
              label = "Microrregião",
              choices = NULL
            ),
            selectInput(
              ns("select_county"),
              label = "Município",
              choices = NULL
            ),
            width = 4
          )
        ),
        width = 12
      ),
    
    fluidRow(
      box(
        title = strong("Casos confirmados"),
        plotlyOutput(ns("confirmed_cases")),
        width = 6
      ),
      box(
        title = strong("Casos reportados"),
        plotlyOutput(ns("reported_cases")),
        width = 6
      ),
      width = 12
    ),
    fluidRow(
      box(
        title = strong("Mortes reportadas"),
        plotlyOutput(ns("reported_deaths")),
        width = 6
      ),
      box(
        title = strong("Casos discartados"),
        plotlyOutput(ns("discarted_cases")),
        width = 6
      ),
      width = 12
    ),
    fluidRow(
      box(
        title = strong("Casos recuperados"),
        plotlyOutput(ns("recovered_cases")),
        width = 6
      ),
      box(
        title = strong("Casos suspeitos"),
        plotlyOutput(ns("suspected_cases")),
        width = 6
      ),
      width = 12
    )
  )

}

timeseries_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    mesoregion <- reactive({
      dengue_ce %>%
        filter(nome_mesorregiao == input$select_mesoregion)
    })
    
    observeEvent(
      mesoregion(),{
        choices <- unique(mesoregion()$nome_microrregiao)
        updateSelectInput(
          inputId = "select_microregion", 
          choices = choices
        )
      }
    )
    
    microregion <- reactive({
      dengue_ce %>%
        filter(nome_microrregiao == input$select_microregion)
    })
    
    observeEvent(
      microregion(), {
        choices <- unique(microregion()$nome_municipio)
        updateSelectInput(
          inputId = "select_county",
          choices = choices
        )
      }
    )
    
    plot_timeseries <- function(var,
                                location_input,
                                timestamp_input) {
      
      var <- enquo(var)
      
      yaxis_title <- switch(as_label(var),
                            "confirmed_cases" = "Casos confirmados",
                            "reported_deaths" = "Mortes reportadas",
                            "reported_cases" = "Casos reportados",
                            "recovered_cases" = "Casos recuperados",
                            "suspected_cases" = "Casos suspeitos",
                            "discarted_cases" = "Casos discartados")
      
      fig <- dengue_ce %>%
        filter(nome_municipio == location_input,
               dt_notific >= timestamp_input[1],
               dt_notific <= timestamp_input[2]) %>%
        group_by(nome_municipio, dt_notific) %>%
        summarise(dengue_var = sum(!!var, na.rm = TRUE)) %>%
        plot_ly() %>%
        add_trace(
          x = ~dt_notific,
          y = ~dengue_var,
          type = 'scatter',
          mode = 'lines',
          line = list(
            color = '#038927'
          )
        ) %>%
        layout(
          xaxis = list(title = "<b>Período</b>"),
          yaxis = list(title = glue::glue("<b>{yaxis_title}</b>"))
        )
      
      return(fig)
    }
    
    output$confirmed_cases <- renderPlotly({
      plot_timeseries(var = confirmed_cases, 
                      input$select_county,
                      input$dt_notific)
    })
    
    output$reported_cases <- renderPlotly({
      plot_timeseries(reported_cases,
                      input$select_county,
                      input$dt_notific)
    })
    
    output$reported_deaths <- renderPlotly({
      plot_timeseries(reported_deaths,
                      input$select_county,
                      input$dt_notific)
    })
    
    output$discarted_cases <- renderPlotly({
      plot_timeseries(discarted_cases, 
                      input$select_county,
                      input$dt_notific)
    })
    
    output$recovered_cases <- renderPlotly({
      plot_timeseries(recovered_cases,
                      input$select_county,
                      input$dt_notific)
    })
    
    output$suspected_cases <- renderPlotly({
      plot_timeseries(suspected_cases,
                      input$select_county,
                      input$dt_notific)
    })
    
  })
}