library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(DT)

data_all <- bushfireApp::temp_data

ui <- navbarPage(
  title = div(
    style = "display:flex; flex-direction:column;",
    div("Global Temperature Anomaly Explorer",
        style = "font-size: 1.4rem; font-weight:600; line-height:1.2;"),
    div("How has temperature changed compared to the 1861–1890 baseline?",
        style = "font-size:0.9rem; font-weight:400; color:#0b1b34;")
  ),
  theme = bs_theme(version = 5, primary = "#0072B2", base_font = font_google("Inter")),
  collapsible = TRUE,

  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 4,
        div(style = "background-color:#ffffff;border-radius:12px;box-shadow:0 4px 12px rgba(0,0,0,0.07);padding:16px;margin-bottom:16px;",
            h4("Inputs", style="font-weight:600; font-size:1.05rem; margin-bottom:1rem;"),
            selectInput("country","Select country / region:", choices = sort(unique(data_all$country)), selected = "World"),
            sliderInput("year_range","Select Year Range:", min=min(data_all$year), max=max(data_all$year),
                        value=c(min(data_all$year), max(data_all$year)), sep = "")
        ),
        div(style = "background-color:#ffffff;border-radius:12px;box-shadow:0 4px 12px rgba(0,0,0,0.07);padding:16px;margin-bottom:16px;",
            h4("Field definitions", style="font-weight:600; font-size:1rem; margin-bottom:1rem;"),
            tags$p(tags$b("country ="), " geographic region (e.g. 'World', 'Northern Hemisphere')."),
            tags$p(tags$b("year ="), " calendar year of observation."),
            tags$p(tags$b("temp_anomaly ="), " °C vs 1861–1890 baseline; + hotter, – cooler.")
        )
      ),
      column(
        width = 8,
        div(style="background-color:#ffffff;border-radius:12px;box-shadow:0 4px 12px rgba(0,0,0,0.07);padding:16px;margin-bottom:16px;",
            tabsetPanel(id = "main_tabs",
                        tabPanel("Plot",
                                 h4("Temperature anomaly over time", style="font-weight:600; font-size:1rem; margin-bottom:1rem;"),
                                 plotOutput("line_plot", height = "400px"),
                                 tags$p(style="font-size:0.9rem; color:#555; margin-top:1rem;",
                                        "How to interpret: Values above 0 are hotter than the 1861–1890 average; an upward trend suggests warming.")
                        ),
                        tabPanel("Data Table",
                                 tags$p(style="font-size:0.9rem; color:#555;",
                                        "Filtered data for the selected country / region and year range."),
                                 DTOutput("data_table")
                        ),
                        tabPanel("How to interpret",
                                 tags$ul(
                                   tags$li("Upward trend = warming over time."),
                                   tags$li("Points above 0 = hotter than 1861–1890 baseline."),
                                   tags$li("Recent years consistently higher = modern warming evidence.")
                                 )
                        )
            )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$country, input$year_range)
    data_all |>
      filter(country == input$country,
             between(year, input$year_range[1], input$year_range[2])) |>
      arrange(year)
  })

  output$line_plot <- renderPlot({
    df <- filtered_data()
    ggplot(df, aes(year, temp_anomaly)) +
      geom_line(color = "#FF8C00", linewidth = 1) +
      geom_point(color = "#FF8C00", size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste0("Temperature anomaly for ", input$country, " (", min(df$year), "–", max(df$year), ")"),
           x = "Year", y = "Temperature anomaly (°C vs 1861–1890)") +
      theme_minimal(base_family = "Inter") +
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.background = element_rect(fill = "white", color = NA),
            panel.grid.minor = element_blank())
  })

  output$data_table <- renderDT({
    df <- filtered_data()[, c("country","year","temp_anomaly")]
    datatable(df, rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

shinyApp(ui, server)
