library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(bslib)

source("R/data_api.R")
source("R/summary.R")

# --------------------------------------------------
# Choices
# --------------------------------------------------
country_choices <- c(
  "China" = "CN",
  "India" = "IN",
  "United States" = "US",
  "Brazil" = "BR",
  "South Africa" = "ZA",
  "Japan" = "JP",
  "United Kingdom" = "GB",
  "Germany" = "DE"
)

indicator_choices <- c(
  "Life Expectancy at Birth" = "SP.DYN.LE00.IN",
  "Under-5 Mortality Rate" = "SH.DYN.MORT",
  "Health Expenditure per Capita" = "SH.XPD.CHEX.PC.CD"
)

# --------------------------------------------------
# UI
# --------------------------------------------------
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    primary = "#2563EB",
    bg = "#F5F7FA",
    fg = "#111827",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #F5F7FA;
      }

      .app-header {
        padding: 22px 28px 6px 28px;
      }

      .app-title {
        font-size: 2.6rem;
        font-weight: 800;
        letter-spacing: -0.02em;
        margin-bottom: 0.35rem;
        color: #111827;
      }

      .app-subtitle {
        font-size: 1.05rem;
        color: #6B7280;
        margin-bottom: 0.4rem;
      }

      .control-label {
        font-weight: 600;
        color: #1F2937;
      }

      .card-title-custom {
        font-size: 1.15rem;
        font-weight: 700;
        color: #111827;
      }

      .summary-box {
        font-size: 1.05rem;
        line-height: 1.8;
        color: #1F2937;
        white-space: pre-wrap;
        min-height: 120px;
      }

      .small-note {
        color: #6B7280;
        font-size: 0.92rem;
        margin-top: 8px;
      }

      .btn-primary {
        width: 100%;
        font-weight: 600;
        border-radius: 10px;
        padding-top: 10px;
        padding-bottom: 10px;
      }

      .form-select, .form-control {
        border-radius: 10px !important;
      }

      .card {
        border: none !important;
        border-radius: 18px !important;
        box-shadow: 0 8px 24px rgba(15, 23, 42, 0.08) !important;
      }
    "))
  ),
  
  div(
    class = "app-header",
    div(class = "app-title", "Global Health Indicator Explorer"),
    div(
      class = "app-subtitle",
      "Explore World Bank health indicators with interactive visualization and AI-generated summaries."
    )
  ),
  
  layout_sidebar(
    fillable = TRUE,
    
    sidebar = card(
      card_header(
        tags$span(class = "card-title-custom", "Controls")
      ),
      card_body(
        selectInput(
          "country",
          "Select country:",
          choices = country_choices,
          selected = "CN"
        ),
        selectInput(
          "indicator",
          "Select indicator:",
          choices = indicator_choices,
          selected = "SP.DYN.LE00.IN"
        ),
        sliderInput(
          "year_range",
          "Select year range:",
          min = 2000,
          max = 2023,
          value = c(2000, 2020),
          sep = ""
        ),
        actionButton("generate_btn", "Generate Summary", class = "btn-primary"),
        div(
          class = "small-note",
          "The plot updates automatically. The AI summary updates only when you click the button."
        )
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header(
        tags$span(class = "card-title-custom", "Trend Visualization")
      ),
      card_body(
        plotOutput("trend_plot", height = "440px")
      )
    ),
    
    card(
      card_header(
        tags$span(class = "card-title-custom", "AI Summary")
      ),
      card_body(
        uiOutput("summary_text"),
        div(
          class = "small-note",
          "Generated from the selected country, indicator, and year range."
        )
      )
    )
  )
)

# --------------------------------------------------
# Server
# --------------------------------------------------
server <- function(input, output, session) {
  
  wb_data <- reactive({
    df <- get_wb_data(input$country, input$indicator)
    
    if (nrow(df) == 0) {
      return(data.frame())
    }
    
    df %>%
      filter(
        year >= input$year_range[1],
        year <= input$year_range[2],
        !is.na(value)
      ) %>%
      arrange(year)
  })
  
  output$trend_plot <- renderPlot({
    df <- wb_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = year, y = value)) +
      geom_line(linewidth = 1.2, color = "#2563EB") +
      geom_point(size = 2.4, color = "#1D4ED8") +
      labs(
        title = paste(df$indicator[1], "in", df$country[1]),
        x = "Year",
        y = "Value"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        plot.title = element_text(face = "bold", size = 16, color = "#111827"),
        axis.title = element_text(face = "bold", color = "#374151"),
        axis.text = element_text(color = "#4B5563"),
        panel.grid.minor = element_blank()
      )
  })
  
  summary_text_val <- reactiveVal(
    "Click “Generate Summary” to create an AI-generated interpretation of the selected trend."
  )
  
  observeEvent(input$generate_btn, {
    df <- wb_data()
    
    if (nrow(df) <= 1) {
      summary_text_val("Not enough data available for summary generation.")
      return()
    }
    
    summary_text_val("Generating summary...")
    
    result <- tryCatch({
      generate_summary_llm(df)
    }, error = function(e) {
      generate_summary_rule(df)
    })
    
    summary_text_val(result)
  })
  
  output$summary_text <- renderUI({
    div(
      class = "summary-box",
      summary_text_val()
    )
  })
}

# --------------------------------------------------
# Run app
# --------------------------------------------------
shinyApp(ui = ui, server = server)