# Load shiny and functions
library(shiny)
source("r_scripts/dataviz_functions_app.R")

# Read data
df <- read.csv("performances.csv")
df_puste <- read.csv("performances_puste.csv")
df_psss <- read.csv("avg_psss_counts.csv")


ui <- fluidPage(
  titlePanel("Performance Analyzer"),
  theme = shinythemes::shinytheme('readable'),

  tabsetPanel(
    tabPanel("Descriptives",
             sidebarLayout(sidebarPanel(),
                            mainPanel(
                              "Primary studies sample size",
                              tableOutput("table_ss_check")
                            ))
    )
  )
)

server <- function(input, output, session){

  output$table_ss_check <- renderTable({
    table_psss(df = df_psss)
  })

}

shinyApp(ui, server)
