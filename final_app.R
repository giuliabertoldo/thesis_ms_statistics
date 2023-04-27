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
                              tableOutput("table_ss_check"),

                              "Percentage of outcomes excluded from the original dataset",
                              tableOutput("table_perc_out_excl"),
                              plotOutput("hist_perc_out_excl"),
                              tableOutput("table_perc_out_excluded_by_d"),
                              plotOutput("viz_hist_perc_excluded_by_d"),
                              plotOutput("hist_perc_out_excluded_by_psss")
                            ))
            ),
    tabPanel("Type I error - M.Egger",
             sidebarLayout(sidebarPanel(
                              selectInput("k_t1e_me", "Number of studies", c(15, 30, 70)),
                              selectInput("bt_t1e_me", "Selection Bias Type", c("None"))
                          ),
                            mainPanel(
                              "Type I error of Multilevel Egger's regression test.",
                              plotOutput("t1e_me_plot1")
                            ))

             )
  )
)

server <- function(input, output, session){

  output$table_ss_check <- renderTable({
    table_psss(df = df_psss)
  })

  output$table_perc_out_excl <- renderTable({
    table_perc_out_excluded_by_bt(df = df)
  })

  output$hist_perc_out_excl <- renderPlot({
    viz_hist_perc_excluded(df = df)
  })

  output$table_perc_out_excluded_by_d <- renderTable({
    table_perc_out_excluded_by_bt_delta(df = df)
  })

  output$viz_hist_perc_excluded_by_d <- renderPlot({
    viz_hist_perc_excluded_by_d(df = df)
  })

  output$hist_perc_out_excluded_by_psss <- renderPlot({
    viz_hist_perc_excluded_by_psss(df = df)
  })

  output$t1e_me_plot1 <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k_t1e_me, bias_type = input$bt_t1e_me)
  })
}

shinyApp(ui, server)
