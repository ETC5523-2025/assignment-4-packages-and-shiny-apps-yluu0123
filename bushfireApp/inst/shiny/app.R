library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

data <- bushfireApp::temp_data

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Global Temperature Anomaly Explorer"),

  sidebarLayout(
    sidebarPanel(
      helpText(
        "Fields:",
        "country = region (e.g. World, Australia)",
        "year = calendar year",
        "temp_anomaly = °C above/below 1861–1890 baseline"
      ),
      selectInput(
        "country",
        "Select country:",
        choices = unique(data$country),
        selected = "World"
      ),
      sliderInput(
        "year_range",
        "Select Year Range:",
        min = min(data$year),
        max = max(data$year),
        value = c(1900, max(data$year)),
        sep = ""
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Data Table", DTOutput("table")),
        tabPanel("How to interpret",
                 div(
                   p("The line shows how much warmer or cooler each year was compared to",
                     "the 1861–1890 average temperature."),
                   p("Values above 0°C mean warmer-than-baseline years (more warming).",
                     "Values below 0°C mean cooler-than-baseline years.")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  filtered <- reactive({
    data %>%
      filter(
        country == input$country,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })

  output$plot <- renderPlot({
    ggplot(filtered(), aes(x = year, y = temp_anomaly)) +
      geom_line(linewidth = 1, color = "#E41A1C") +
      geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
      labs(
        x = "Year",
        y = "Temperature anomaly (°C vs 1861–1890)",
        title = paste("Temperature anomaly for", input$country)
      ) +
      theme_minimal(base_size = 13)
  })

  output$table <- renderDT({
    datatable(
      filtered(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)
