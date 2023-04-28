# Load shiny and functions
library(shiny)
source("r_scripts/dataviz_functions_app.R")

# Read data
df <- read.csv("performances.csv")
df_puste <- read.csv("performances_puste.csv")
df_psss <- read.csv("avg_psss_counts.csv")
df_conv <- read.csv("convergence.csv")
df_sub <- read.csv("performances_sub.csv")

ui <- fluidPage(
  titlePanel("Performance Analyzer"),
  theme = shinythemes::shinytheme('readable'),

  tabsetPanel(
    tabPanel("Descriptives",

             h4("Primary studies sample size"),
             DT::dataTableOutput("table_ss_check"),

             h4("Percentage of outcomes excluded from the original dataset"),
             DT::dataTableOutput("table_perc_out_excl"),
             plotOutput("hist_perc_out_excl"),
             DT::dataTableOutput("table_perc_out_excluded_by_d"),
             plotOutput("viz_hist_perc_excluded_by_d"),
             plotOutput("hist_perc_out_excluded_by_psss"),

             h4("Percentage of non-convergence of the four models by condition."),
             DT::dataTableOutput("conv_table1")

    ),

    tabPanel("Type I error - M.Egger",
             sidebarLayout(sidebarPanel(
               selectInput("k_t1e_me", "Number of studies", c(15, 30, 70)),
               selectInput("bt_t1e_me", "Selection Bias Type", c("None"))
             ),
             mainPanel(
               h4("Type I error of Multilevel Egger's regression test."),
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

               h4("Power of Multilevel Egger's regression test."),
               h4("Conditions with less than 1% non-covergence."),
               plotOutput("pw_me_plot2"),
               DT::dataTableOutput("pw_me_table2"),


               h4("Power of Multilevel Egger's regression test."),
               h4("All conditions"),
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
               h4("Power of Multilevel Egger's regression test using the selection mechanism in Pustejovsky & Rodgers."),
               plotOutput("pw_me_sp_plot1"),
               DT::dataTableOutput("pw_me_sp_table1")
             ))
    ),

    tabPanel("M.PET-PEESE: PET Int.",
            sidebarLayout(sidebarPanel(
                            selectInput("k_rr_pet_int", "Number of studies", c(15, 30, 70)),
                            selectInput("bt_rr_pet_int", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                               "PB Strong", "PB Moderate"))
                          ),
                          mainPanel(

                            h4("Rejection rate of intercept in Multilevel PET / Multilevel Egger's regression test"),
                            h4("Conditions with less than 1% non-covergence."),
                            plotOutput("viz_pet_int2"),
                            DT::dataTableOutput("table_pet_int2"),


                            h4("Rejection rate of intercept in Multilevel PET / Multilevel Egger's regression test"),
                            h4("All conditions"),
                            plotOutput("viz_pet_int"),
                            DT::dataTableOutput("table_pet_int")
                          ))

    ),

    tabPanel("M.PET-PEESE: Adj. Estim.",
             sidebarLayout(sidebarPanel(
                             selectInput("k_adj_est", "Number of studies", c(15, 30, 70)),
                             selectInput("bt_adj_est", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                                     "PB Strong", "PB Moderate"))
                          ),
                           mainPanel(
                             h5("RMSE Adjusted Estimate"),
                             plotOutput("viz_rmse_adj_est"),
                             DT::dataTableOutput("table_rmse_adj_est"),
                             h5("Bias Adjusted Estimate"),
                             plotOutput("viz_bias_adj_est"),
                             DT::dataTableOutput("table_bias_adj_est")
                           ))),

    tabPanel("M.PET-PEESE - Selec.Pu.",
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 h4("Power Multilevel PET intercept: Conditions with at least 80% in Multilevel Egger's regression test"),
                 p("Conditions descriptions:"),
                 p("- Selection mechanims from Pustejovsky & Rodgers (2018)"),
                 p("- Probability of censoring nonsignificant results: 1"),
                 p("- Number of studies in meta-analytic dataset: 70"),
                 p("- Within and between study variance: small"),
                 p("- Population SMD of 0.5 and all primary studies sample sizes"),
                 p("- Population SMD of 0.8 and small primary study sample size"),
                 DT::dataTableOutput("pw_pet_int_table1"),

                 h4("MSE adjusted estimate: Conditions with at least 80% in PET intercept"),
                 p("Conditions description:"),
                 p("- Population SMD of 0.5 and primary study sample size medium or large"),
                 p("- Population SMD of 0.8 and small primary study sample size"),
                 DT::dataTableOutput("mse_adj_table1")

                )))
  )
)

server <- function(input, output, session){

  output$table_ss_check <- DT::renderDataTable({
    table_psss(df = df_psss)
  })

  output$table_perc_out_excl <- DT::renderDataTable({
    table_perc_out_excluded_by_bt(df = df)
  })

  output$hist_perc_out_excl <- renderPlot({
    viz_hist_perc_excluded(df = df)
  })

  output$table_perc_out_excluded_by_d <- DT::renderDataTable({
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

  output$pw_me_plot2 <- renderPlot({
    viz_rejection_rate(df = df_sub, num_studies = input$k_pw_me, bias_type = input$bt_pw_me)
  })

  output$pw_me_table1 <- DT::renderDataTable({
    table_rejection_rate(df = df, num_studies = input$k_pw_me, bias_type = input$bt_pw_me)
  })

  output$pw_me_table2 <- DT::renderDataTable({
    table_rejection_rate(df = df_sub, num_studies = input$k_pw_me, bias_type = input$bt_pw_me)
  })


  output$pw_me_sp_plot1 <- renderPlot({
    viz_rejection_rate_puste(df = df_puste, num_studies = input$k_pw_me_sp, prob_cens = input$pc_pw_me_sp)
  })

  output$pw_me_sp_table1 <- DT::renderDataTable({
    table_rejection_rate_puste(df = df_puste, num_studies = input$k_pw_me_sp, prob_cens = input$pc_pw_me_sp)
  })

  output$pw_pet_int_table1 <- DT::renderDataTable({
    table_pet_int_subset(df = df_puste)
  })

  output$mse_adj_table1 <- DT::renderDataTable({
    table_mse_subset(df = df_puste)
  })

  output$viz_pet_int <- renderPlot({
    viz_rr_pet_int(df = df, num_studies = input$k_rr_pet_int, bias_type = input$bt_rr_pet_int)
  })

  output$viz_pet_int2 <- renderPlot({
    viz_rr_pet_int(df = df_sub, num_studies = input$k_rr_pet_int, bias_type = input$bt_rr_pet_int)
  })

  output$table_pet_int <- DT::renderDataTable({
    table_rr_pet_int(df = df, num_studies = input$k_rr_pet_int, bias_type = input$bt_rr_pet_int)
  })

  output$table_pet_int2 <- DT::renderDataTable({
    table_rr_pet_int(df = df_sub, num_studies = input$k_rr_pet_int, bias_type = input$bt_rr_pet_int)
  })

  output$viz_rmse_adj_est <- renderPlot({
    viz_adj_est_rmse(df = df, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$table_rmse_adj_est <- DT::renderDataTable({
    table_adj_est_rmse(df = df, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$viz_bias_adj_est <- renderPlot({
    viz_adj_est_bias(df = df, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$table_bias_adj_est <- DT::renderDataTable({
    table_adj_est_bias(df = df, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$conv_table1 <- DT::renderDataTable({
    table_perc_non_conv(df = df_conv)
  })
}

shinyApp(ui, server)
