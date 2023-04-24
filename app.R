library(shiny)
library(ggplot2)
library(plotly)
source("r_scripts/dataviz_functions_app.R")

df <- read.csv("performances.csv")
df_puste <- read.csv("performances_puste.csv")
df_psss <- read.csv("avg_psss_counts.csv")

ui <- fluidPage(
  titlePanel("Performance Analyzer"),
  theme = shinythemes::shinytheme('readable'),

  navbarPage(
    title = "Page",

    tabPanel("Sample Size",
             mainPanel(
               plotOutput("sample_size_check")
             )),

    tabPanel("Type I error",
             sidebarPanel(
               selectInput("k", "Number of studies", c(15, 30, 70))
             ),
             mainPanel(
               plotOutput("type1")
             )),

    tabPanel("Power Selection 1",
             sidebarPanel(
               selectInput("k1", "Number of studies", c(15, 30, 70)),
               selectInput("selection1", "Selection Type", c("PB No, ORB Strong", "PB No, ORB Moderate",
                                                                "PB Strong, ORB No", "PB Moderate, ORB No",
                                                                "PB Strong, ORB Strong", "PB Moderate, ORB Moderate",
                                                                "PB Strong, ORB Moderate", "PB Moderate, ORB Strong"))
             ),
             mainPanel(
               plotOutput("power_selection1"),
               plotOutput("selection1check")
             )),

    tabPanel("Power Selection 2",
             sidebarPanel(
               selectInput("k2", "Number of studies", c(15, 30, 70)),
               selectInput("psss2", "Primary studies sample size", c("small", "medium", "large"))
             ),
             mainPanel(
               plotOutput("power_selection2"),
               plotOutput("selection2check")
             )),

    tabPanel("Compare Selections",
             sidebarPanel(
               selectInput("k3", "Number of studies", c(15, 30, 70)),
               selectInput("psss3", "Primary studies sample size", c("small", "medium", "large")),
               selectInput("selection3", "Selection Type", c("PB No, ORB Strong", "PB No, ORB Moderate",
                                                             "PB Strong, ORB No", "PB Moderate, ORB No",
                                                             "PB Strong, ORB Strong", "PB Moderate, ORB Moderate",
                                                             "PB Strong, ORB Moderate", "PB Moderate, ORB Strong"))
             ),
             mainPanel(
               "Selection 1:",
               plotOutput("power_perc_selection1"),
               "Selection 2:",
               plotOutput("power_perc_selection2")
             ))
  )
)


server <- function(input, output, session){

  output$sample_size_check <- renderPlot({
    histogram_by_pss(df = df_psss)
  })

  output$type1 <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k1, bias_type = "None")
  })

  output$selection1check <- renderPlot({
    viz_percent_out_includded_by_bt(df = df)
  })

  output$power_selection1 <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k1, bias_type = input$selection1)
  })

  output$selection2check <- renderPlot({
    viz_percent_out_includded_by_bt_puste(df = df_puste)
  })

  output$power_selection2 <- renderPlot({
    viz_rejection_rate_puste(df = df_puste, num_studies = input$k2, ss = input$psss2)
  })

  output$power_perc_selection1 <- renderPlot({
    viz_power_perc_selected(df = df_puste, num_studies = input$k3, ss = input$psss3, bias_type = input$selection3)
  })

  output$power_perc_selection2 <- renderPlot({
    viz_power_perc_selected_puste(df = df_puste, num_studies = input$k3, ss = input$psss3)
  })

}




shinyApp(ui = ui, server = server)
