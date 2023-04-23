library(shiny)
library(ggplot2)
library(plotly)
source("r_scripts/dataviz_functions_app.R")

df <- read.csv("performances.csv")

ui <- fluidPage(
  titlePanel("Performance Analyzer"),

  sidebarLayout(

    sidebarPanel(
      selectInput("k", "Number of studies in unbiased meta-analysis", c(15, 30, 70)),
      selectInput("selectionFern", "Selection Fern", c("None",
                                              "PB No, ORB Strong", "PB No, ORB Moderate",
                                              "PB Strong, ORB No", "PB Moderate, ORB No",
                                              "PB Strong, ORB Strong", "PB Moderate, ORB Moderate",
                                              "PB Strong, ORB Moderate", "PB Moderate, ORB Strong")),
      selectInput("selectionPuste", "Selection Puste", c("ORB 80% Not Sig. selected", "ORB 60% Not Sig. selected",
                                                         "ORB 40% Not Sig. selected", "ORB 20% Not Sig. selected",
                                                         "ORB 0% Not Sig. selected"))


    ),

    mainPanel(
      plotOutput("rejection_rate"),
      plotOutput("per_out_select")
    )
  )
)


server <- function(input, output, session){
  output$rejection_rate <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k, bias_type = input$selectionFern)
  })

  output$per_out_select <- renderPlot({
    viz_per_out_selected(df = df, num_studies = input$k, bias_type = input$selectionFern)
  })

}



# rejection_rates <- function(){
#   viz_rejection_rate(df = df, num_studies = input$k, bias_type = input$selectionFern)
# }
#
# per_out_selected <- function(){
#   viz_per_out_selected(df = df, num_studies = input$k, bias_type = input$selectionFern)
# }
#
# output$plot_rejection_rates <- plotly::renderPlotly({
#   rejection_rates()
# })
#
# output$plot_per_out_selected <- plotly::renderPlotly({
#   viz_per_out_selected()
# })
#
# plotly::plotlyOutput("plot_rejection_rates"),
# plotly::plotlyOutput("plot_per_out_selected")


shinyApp(ui = ui, server = server)
