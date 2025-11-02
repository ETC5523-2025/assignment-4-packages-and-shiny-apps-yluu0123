library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(DT)


data_all <- bushfireApp::temp_data

country_choices <- sort(unique(data_all$country))

# UI
ui <- navbarPage(
  title = div(
    style = "display:flex; flex-direction:column;",
    div("Global Temperature Anomaly Explorer",
        style = "font-size: 1.4rem; font-weight:600; line-height:1.2;"),
    div("How has temperature changed compared to the 1861–1890 baseline?",
        style = "font-size:0.9rem; font-weight:400; color:#ffffff; opacity:0.9;")
  ),
  theme = bs_theme(
    version = 5,
    primary = "#0072B2",
    base_font = font_google("Inter")
  ),
  collapsible = TRUE,

  tabPanel(
    title = NULL,
    fluidRow(
      column(
        width = 4,
        div(
          style = "
            background-color:#ffffff;
            border-radius:12px;
            box-shadow:0 4px 12px rgba(0,0,0,0.07);
            padding:16px;
            margin-bottom:16px;
          ",
          h4("Inputs",
             style="font-weight:600; font-size:1.05rem; margin-bottom:1rem; display:flex; justify-content:space-between;"),

          selectInput(
            inputId = "country",
            label   = "Select country / region:",
            choices = sort(unique(bushfireApp::temp_data$country)),
            selected = "World"
          ),

          sliderInput(
            inputId = "year_range",
            label   = "Select Year Range:",
            min     = min(bushfireApp::temp_data$year),
            max     = max(bushfireApp::temp_data$year),
            value   = c(min(bushfireApp::temp_data$year),
                        max(bushfireApp::temp_data$year)),
            sep     = ""
          )
        ),


        div(
          style = "
            background-color:#ffffff;
            border-radius:12px;
            box-shadow:0 4px 12px rgba(0,0,0,0.07);
            padding:16px;
            margin-bottom:16px;
          ",
          h4("Field definitions",
             style="font-weight:600; font-size:1rem; margin-bottom:1rem;"),

          tags$p(
            tags$b("country ="),
            " geographic region (e.g. 'World', 'Northern Hemisphere')."
          ),
          tags$p(
            tags$b("year ="),
            " calendar year of observation."
          ),
          tags$p(
            tags$b("temp_anomaly ="),
            " how many °C warmer (+) or cooler (–) that year was compared to the 1861–1890 average baseline."
          ),
          tags$p(
            style = "font-size:0.9rem; color:#555;",
            "Example: +1.2 means this year was about 1.2°C hotter than the 1861–1890 baseline."
          )
        )
      ),


      column(
        width = 8,
        div(
          style="
            background-color:#ffffff;
            border-radius:12px;
            box-shadow:0 4px 12px rgba(0,0,0,0.07);
            padding:16px;
            margin-bottom:16px;
          ",

          tabsetPanel(
            id = "main_tabs",
            tabPanel(
              title = "Plot",
              h4("Temperature anomaly over time",
                 style="font-weight:600; font-size:1rem; margin-bottom:1rem;"),


              plotOutput("line_plot", height = "400px"),

              tags$p(
                style="font-size:0.9rem; color:#555; margin-top:1rem;",
                "How to interpret: Values above 0 mean that year was hotter than the 1861–1890 average. ",
                "An upward trend suggests warming over time."
              )
            ),

            tabPanel(
              title = "Data Table",
              tags$p(
                style="font-size:0.9rem; color:#555;",
                "Filtered data for your selected country / region and year range."
              ),


              DTOutput("data_table")
            ),

            tabPanel(
              title = "How to interpret",
              tags$ul(
                tags$li("If the line trends upward, it means warming over time."),
                tags$li("Points above 0 are hotter than the 1861–1890 baseline."),
                tags$li("Recent years are consistently higher → evidence of modern warming.")
              )
            )
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {

  # 1. reactive for Plot tab
  filtered_data_plot <- reactive({
    req(input$country, input$year_range)
    data_all %>%
      filter(
        country == input$country,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })

  output$line_plot <- renderPlot({
    df <- filtered_data_plot()

    ggplot(df, aes(x = year, y = temp_anomaly)) +
      geom_line(color = "#FF8C00", linewidth = 1) +
      geom_point(color = "#FF8C00", size = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title = paste0(
          "Temperature anomaly for ",
          input$country,
          " (", min(df$year), "–", max(df$year), ")"
        ),
        x = "Year",
        y = "Temperature anomaly (°C vs 1861–1890)"
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        plot.title = element_text(
          size = 14, face = "bold", hjust = 0.5
        ),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank()
      )
  })

  # 2. reactive for Data Table tab
  filtered_data_table <- reactive({
    req(input$country_table, input$year_range_table)
    data_all %>%
      filter(
        country == input$country_table,
        year >= input$year_range_table[1],
        year <= input$year_range_table[2]
      ) %>%
      arrange(year)
  })

  output$data_table <- renderDT({
    df_tbl <- filtered_data_table() %>%
      select(country, year, temp_anomaly)

    datatable(
      df_tbl,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
}


shinyApp(ui, server)
