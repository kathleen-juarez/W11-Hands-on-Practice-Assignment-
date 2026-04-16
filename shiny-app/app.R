

library(shiny)
library(tidyverse)

# load data
df <- read_csv("Traffic acquisition-Session primary channel group (Default Channel Group)(Sheet1)(2).csv")

# clean data
df_clean <- df %>%
  rename(channel = `...1`) %>%
  filter(!is.na(channel)) %>%
  mutate(
    sessions         = readr::parse_number(Sessions),
    engaged_sessions = readr::parse_number(`Engaged sessions`),
    form_starts      = readr::parse_number(`Event count (form_start)`),
    high_intent_rate = form_starts / sessions
  ) %>%
  filter(!is.na(sessions), sessions >= 2)

ui <- fluidPage(
  
  titlePanel("Traffic Channel Engagement"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "metric",
        label   = "Select a metric:",
        choices = c("sessions", "engaged_sessions", "form_starts", "high_intent_rate")
      ),
      
      checkboxGroupInput(
        inputId  = "channels",
        label    = "Select channels to include:",
        choices  = c("Organic Social", "Direct", "Organic Search", "Unassigned", "Paid Search", "Referral"),
        selected = c("Organic Social", "Direct", "Organic Search", "Unassigned", "Paid Search", "Referral")
      )
    ),
    
    mainPanel(
      
      textOutput("title"),
      plotOutput("bar_chart"),
      plotOutput("scatter_plot")
    )
  )
)

server <- function(input, output) {
  
  output$title <- renderText(
    paste("Showing:", input$metric)
  )
  
  output$bar_chart <- renderPlot({
    df_clean %>%
      filter(channel %in% input$channels) %>%
      ggplot(aes(x = reorder(channel, .data[[input$metric]]), y = .data[[input$metric]], fill = channel)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(
        title = paste(input$metric, "by channel"),
        x     = "Channel",
        y     = input$metric
      ) +
      theme_minimal()
  })
  
  output$scatter_plot <- renderPlot({
    df_clean %>%
      filter(channel %in% input$channels) %>%
      ggplot(aes(x = sessions, y = high_intent_rate, color = channel)) +
      geom_point(size = 5) +
      geom_text(aes(label = channel), vjust = -1, show.legend = FALSE) +
      labs(
        title = "Sessions vs. High-Intent Rate",
        x     = "Sessions",
        y     = "High-Intent Rate"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
}

shinyApp(ui = ui, server = server)

library(rsconnect)
rsconnect::deployApp(
  appDir  = '~/Desktop/CEP-dashboard/CEP_shiny-app',
  appName = 'channel-engagement-explorer'
)