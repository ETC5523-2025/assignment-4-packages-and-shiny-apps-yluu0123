library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(DT)


data_all <- bushfireApp::temp_data

country_choices <- sort(unique(data_all$country))

# UI
ui <- bslib::page_navbar(

  title = tags$div(
    tags$h2(
      "Global Temperature Anomaly Explorer",
      style = "color:#FFFFFF; font-weight:700; margin:0;"
    ),
    tags$p(
      "How has temperature changed compared to the 1861–1890 baseline?",
      style = "color:#FFFFFF; font-size:1rem; margin:0;"
    )
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#0077CC",  # navbar blue
    base_font = font_google("Inter")
  ),


  nav_panel(
    "Plot",
    layout_sidebar(
      sidebar = sidebar(
        style = "background-color:#ffffff;
                 border-radius:12px;
                 box-shadow:0 4px 12px rgba(0,0,0,0.07);
                 padding:16px;",
        width = 300,

        h4("Inputs", style = "font-weight:600; font-size:1.05rem;"),

        selectInput(
          inputId = "country",
          label   = "Select country / region:",
          choices = country_choices,
          selected = "World"
        ),

        sliderInput(
          inputId = "year_range",
          label   = "Select Year Range:",
          min     = min(data_all$year),
          max     = max(data_all$year),
          value   = c(min(data_all$year), max(data_all$year)),
          sep     = ""
        ),

        div(
          style = "background-color:#ffffff;
                   border-radius:12px;
                   box-shadow:0 4px 12px rgba(0,0,0,0.07);
                   padding:16px;
                   margin-top:20px;",
          h4("Field definitions",
             style="font-weight:600; font-size:1.05rem;"),

          p("country = geographic region (e.g. 'World', 'Northern Hemisphere')."),
          p("year = calendar year of observation."),
          p("temp_anomaly = how many °C warmer (+) or cooler (–) that year was compared to the 1861–1890 average baseline."),
          p("Example: +1.2 means this year was about 1.2°C hotter than the 1861–1890 baseline.")
        )
      ),

      main = card(
        card_header("Temperature anomaly over time"),
        plotOutput("line_plot", height = "500px")
      )
    )
  ),

  nav_panel(
    "Data Table",
    layout_sidebar(
      sidebar = sidebar(
        style = "background-color:#ffffff;
                 border-radius:12px;
                 box-shadow:0 4px 12px rgba(0,0,0,0.07);
                 padding:16px;",
        width = 300,

        h4("Filters", style = "font-weight:600; font-size:1.05rem;"),

        selectInput(
          inputId = "country_table",
          label   = "Select country / region:",
          choices = country_choices,
          selected = "World"
        ),

        sliderInput(
          inputId = "year_range_table",
          label   = "Select Year Range:",
          min     = min(data_all$year),
          max     = max(data_all$year),
          value   = c(min(data_all$year), max(data_all$year)),
          sep     = ""
        ),

        p(
          "This table shows the raw values used in the plot.
           You can sort columns, and use the search box."
        )
      ),

      main = card(
        card_header("Filtered data"),
        DTOutput("data_table")
      )
    )
  ),

  nav_panel(
    "How to interpret",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("How to read the plot"),
        p("Each point shows how much warmer (positive) or cooler (negative) that year was compared to the average temperature in 1861–1890."),
        p("If the line is mostly above 0 after ~1950 and keeps going higher, that means that region is consistently hotter than the historical baseline."),
        p("If the slope is getting steeper in recent decades, it suggests faster warming in recent years.")
      ),
      card(
        card_header("What story should users notice?"),
        p("1. Temperatures in all regions are now almost always above the 1861–1890 baseline."),
        p("2. The increase accelerates after ~1980, which is consistent with reports on global warming."),
        p("3. This helps communicate that 'this is not random fluctuation' but a long-term warming pattern.")
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


shinyApp(ui = ui, server = server)
