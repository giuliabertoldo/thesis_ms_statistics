library(shiny)

source("r_scripts/dataviz_functions_app.R")

df <- read.csv("performances.csv")
df_puste <- read.csv("performances_puste.csv")
df_psss <- read.csv("avg_psss_counts.csv")

ui <- fluidPage(
  titlePanel("Performance Analyzer"),
  theme = shinythemes::shinytheme('readable'),

  navbarPage(
    title = "Page",

    tabPanel("Descriptives",
             sidebarPanel(

             ),

             mainPanel(
               "Primary studies sample size",
               tableOutput("table_ss_check"),

               "Percentage of outcomes excluded from the original dataset",
               tableOutput("table_perc_out_excl"),
               plotOutput("hist_perc_out_excl"),
               tableOutput("table_perc_out_excluded_by_d"),
               plotOutput("hist_perc_out_excluded_by_d"),
               plotOutput("hist_perc_out_excluded_by_psss")


             )),

    tabPanel("Type I error",
             sidebarPanel(
               selectInput("k0", "Number of studies", c(15, 30, 70)),
               selectInput("selection0", "Selection Bias Type", c("None"))
             ),
             mainPanel(
               "Type I error of Multilevel Egger's regression test.",
               plotOutput("type1"),
               DT::dataTableOutput("type1table")

             )),

    tabPanel("Power",
             sidebarPanel(
               selectInput("k1", "Number of studies", c(15, 30, 70)),
               selectInput("selection1", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                                "PB Strong", "PB Moderate"))
             ),
             mainPanel(
               "Power of Multilevel Egger's regression test.",
               plotOutput("power_selection1"),
               DT::dataTableOutput("powertable")
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
               selectInput("selection3", "Selection Bias Type", c("PB No, ORB Strong", "PB No, ORB Moderate",
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

  output$hist_perc_out_excluded_by_d <- renderPlot({
    viz_hist_perc_excluded_by_d(df=df)
  })

  output$hist_perc_out_excluded_by_psss <- renderPlot({
    viz_hist_perc_excluded_by_psss(df = df)
  })

  output$type1 <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k0, bias_type = input$selection0)
  })

  output$type1table <- DT::renderDataTable({
    table_rejection_rate(df = df, num_studies = input$k0, bias_type = input$selection0)
  })


  output$power_selection1 <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k1, bias_type = input$selection1)
  })

  output$powertable <- DT::renderDataTable({
    table_rejection_rate(df = df, num_studies = input$k1, bias_type = input$selection1)
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
