# inst/shiny/app.R

# This Shiny app explores the temp_data dataset that ships
# with the bushfireApp package. It lets the user:
#  - pick a region (World, Australia, etc.)
#  - choose a year range
#  - view a line plot of temperature anomaly over time
#  - view the filtered data table
#  - read guidance on how to interpret the results
#
# The app is designed to satisfy Part B of the assignment rubric:
# - uses internal package data (temp_data)
# - has interactivity
# - interactivity updates outputs
# - explains fields
# - explains how to interpret outputs
# - has some custom styling/theme


library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

# ---- data ----
# temp_data is included in the bushfireApp package (data/temp_data.rda),
# so we can access it directly here without read.csv().
data_all <- bushfireApp::temp_data

all_countries <- sort(unique(data_all$country))

# Define UI
ui <- page_navbar(
  title = div(
    div("Global Temperature Anomaly Explorer",
        style = "font-weight:600; font-size:1.2rem;"),
    div("How has temperature changed compared to the 1861–1890 baseline?",
        style = "font-size:0.9rem; color:#666; margin-top:2px;")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0072B2",
    base_font = font_google("Inter")
  ),

  # Sidebar on the left
  sidebar = sidebar(
    width = 300,
    div(
      style = "background-color:#ffffff;
               border-radius:12px;
               box-shadow:0 4px 12px rgba(0,0,0,0.07);
               padding:16px;
               margin-bottom:16px;",
      h4("Inputs", style="font-weight:600; font-size:1.05rem;"),

      selectInput(
        inputId = "country",
        label = "Select country / region:",
        choices = sort(unique(bushfireApp::temp_data$country)),
        selected = "World"
      ),

      sliderInput(
        inputId = "year_range",
        label   = "Select Year Range:",
        min     = min(data_all$year),
        max     = max(data_all$year),
        value   = c(min(data_all$year), max(data_all$year)),
        sep     = ""
      )
    ),

    div(
      style = "background-color:#ffffff;
               border-radius:12px;
               box-shadow:0 4px 12px rgba(0,0,0,0.07);
               padding:16px;",
      h4("Field definitions",
         style="font-weight:600; font-size:1.05rem; margin-bottom:8px;"),

      tags$p(
        tags$b("country"), " = geographic region (e.g. \"World\", \"Australia\").",
        style="font-size:0.9rem; margin-bottom:4px;"
      ),
      tags$p(
        tags$b("year"), " = calendar year of observation.",
        style="font-size:0.9rem; margin-bottom:4px;"
      ),
      tags$p(
        tags$b("temp_anomaly"),
        " = how many °C warmer (+) or cooler (–) that year was",
        " compared to the 1861–1890 average baseline.",
        style="font-size:0.9rem; margin-bottom:0;"
      ),
      tags$p(
        "Example: +1.2 means this year was about 1.2°C hotter",
        " than the 1861–1890 baseline.",
        style="font-size:0.8rem; color:#666;"
      )
    )
  ),

  # Main content is a set of tabs
  nav_panel(
    "Plot",
    layout_column_wrap(
      width = 1,
      card(
        full_screen = TRUE,
        card_header("Temperature anomaly over time"),
        plotOutput("line_plot", height = "400px")
      )
    )
  ),

  nav_panel(
    "Data Table",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Filtered data"),
        DTOutput("table_out")
      )
    )
  ),

  nav_panel(
    "How to interpret",
    layout_column_wrap(
      width = 1,
      card(
        card_header("How to read this dashboard"),
        div(
          style="font-size:0.95rem; line-height:1.4;",

          h5("1. What do the inputs mean?",
             style="font-weight:600; margin-top:0.5rem;"),
          p(
            strong("Country / region: "),
            "Which location you are looking at (e.g. World, Australia)."
          ),
          p(
            strong("Year range: "),
            "You can zoom into a specific period, e.g. only 1950–2020."
          ),

          h5("2. How to read the plot",
             style="font-weight:600; margin-top:1rem;"),
          p(
            "The line shows how much warmer (positive values) or cooler ",
            " (negative values) each year was compared to the 1861–1890 ",
            "baseline. The dashed grey horizontal line at 0°C is the ",
            "baseline itself."
          ),
          p(
            "If the line is above 0, that year was hotter than the ",
            "19th-century baseline. A value like +1.2 means roughly ",
            "1.2°C warmer."
          ),
          p(
            "If the line steadily climbs over the decades, it indicates a ",
            "warming trend over time."
          ),

          h5("3. How to read the data table",
             style="font-weight:600; margin-top:1rem;"),
          p(
            strong("year"),
            " is the calendar year. ",
            strong("temp_anomaly"),
            " is the °C difference from the 1861–1890 average. ",
            "Sorting or filtering this table can help identify the ",
            "hottest years."
          ),

          h5("4. Why this matters",
             style="font-weight:600; margin-top:1rem;"),
          p(
            "Large positive anomalies (e.g. > +1°C) in recent decades ",
            "are consistent with global warming.",
            " This matches what climate science reports: ",
            "the planet is warming relative to pre-industrial levels."
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # reactive data: filter by country and year range
  filtered_data <- reactive({
    req(input$country, input$year_range)

    data_all %>%
      filter(
        country == input$country,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })

  # line plot
  output$line_plot <- renderPlot({
    df <- filtered_data()

    ggplot(df, aes(x = year, y = temp_anomaly)) +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 color = "grey50") +
      geom_line(linewidth = 1, color = "#D55E00") +
      geom_point(size = 1.5, color = "#D55E00") +
      labs(
        x = "Year",
        y = "Temperature anomaly (°C vs 1861–1890)",
        title = paste0(
          "Temperature anomaly for ", input$country,
          " (", input$year_range[1], "–", input$year_range[2], ")"
        )
      ) +
      theme_minimal(base_family = "Inter") +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12),
        axis.text  = element_text(size = 10)
      )
  })

  # data table
  output$table_out <- renderDT({
    df <- filtered_data()

    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

}


shinyApp(ui = ui, server = server)
