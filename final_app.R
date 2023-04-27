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
                              plotOutput("t1e_me_plot1"),
                              DT::dataTableOutput("t1e_me_table1")
                            ))

             ),
    tabPanel("Power - M.Egger",
             sidebarLayout(sidebarPanel(
                              selectInput("k_pw_me", "Number of studies", c(15, 30, 70)),
                              selectInput("bt_pw_me", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                                "PB Strong", "PB Moderate"))
                          ),
                           mainPanel(
                             "Power of Multilevel Egger's regression test.",
                             plotOutput("pw_me_plot1"),
                             DT::dataTableOutput("pw_me_table1")
                           )
                           )

            ),
    tabPanel("Power - M.Egger - Selec.Pu.",
             sidebarLayout(sidebarPanel(
                              selectInput("k_pw_me_sp", "Number of studies", c(15, 30, 70)),
                              selectInput("pc_pw_me_sp", "Probability of censoring non-significant results", c(1, 0.8, 0.6, 0.4, 0.2))
                          ),
                           mainPanel(
                             plotOutput("pw_me_sp_plot1"),
                             DT::dataTableOutput("pw_me_sp_table1")
                           ))
                          ),

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

  output$t1e_me_table1 <- DT::renderDataTable({
    table_rejection_rate(df = df, num_studies = input$k_t1e_me, bias_type = input$bt_t1e_me)
  })

  output$pw_me_plot1 <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k_pw_me, bias_type = input$bt_pw_me)
  })

  output$pw_me_table1 <- DT::renderDataTable({
    table_rejection_rate(df = df, num_studies = input$k_pw_me, bias_type = input$bt_pw_me)
  })

  output$pw_me_sp_plot1 <- renderPlot({
    viz_rejection_rate_puste(df = df_puste, num_studies = input$k_pw_me_sp, prob_cens = input$pc_pw_me_sp)
  })

  output$pw_me_sp_table1 <- DT::renderDataTable({
    table_rejection_rate_puste(df = df_puste, num_studies = input$k_pw_me_sp, prob_cens = input$pc_pw_me_sp)
  })

}

shinyApp(ui, server)
