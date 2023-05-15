# Load shiny and functions
library(shiny)
source("r_scripts/dataviz_functions_app.R")

# Read data
df <- read.csv("performances.csv")
df_psss <- read.csv("avg_psss_counts.csv")
df_conv <- read.csv("convergence.csv")
df_sub <- read.csv("performances_sub.csv")
df_est_check <- read.csv("estimates_check.csv")

ui <- fluidPage(
  titlePanel("Publication Bias in Meta-analysis of Dependent Standardized Mean Differences"),
  theme = shinythemes::shinytheme('readable'),

  tabsetPanel(

    tabPanel("Descriptives",
             sidebarLayout(sidebarPanel(

               h4("Select condition:"),

               selectInput("k_viz_est", "Number of studies", c(15, 30, 70)),
               selectInput("bt_viz_est", "Selection Bias Type", c("None","ORB Strong", "ORB Moderate",
                                                                  "PB Strong", "PB Moderate")),
               selectInput("d_viz_est", "True SMD", c(0, 0.2, 0.5, 0.8)),
               selectInput("su_sv_viz_est", "Within & Between study variance", c("Small", "Medium", "Large")),
               selectInput("p_viz_est", "Primary studies sample size", c("Small", "Medium", "Large"))
             ),
             mainPanel(
               h4("Descriptives"),
               tableOutput("descriptives_table"),
               br(),
               h4("Estimates from Multilevel Egger's Regression Test: SMD vs. Transformed SMD"),
               plotOutput("viz_estimates_megger"),
               br(),
               h4("Estimates from Multilevel PEESE: SMD vs. Transformed SMD"),
               plotOutput("viz_estimates_peese"),
               br(),
               h4("Effective number of outcomes and number of studies for all conditions"),
               DT::dataTableOutput("overall_table_descr")

             ))

    ),

    tabPanel("Non-convergence",

             br(),
             h4("Percentage of non-convergence of the four models by condition"),
             br(),
             DT::dataTableOutput("conv_table1")

    ),

    tabPanel("M.Egger: Type I",
             sidebarLayout(sidebarPanel(
               h4("Select condition:"),
               selectInput("k_t1e_me", "Number of studies", c(15, 30, 70)),
               selectInput("bt_t1e_me", "Selection Bias Type", c("None"))
             ),
             mainPanel(
               br(),
               h4("Type I error of Multilevel Egger's regression test"),
               h5("Conditions with less than 1% non-covergence"),
               plotOutput("t1e_me_plot2"),
               br(),
               DT::dataTableOutput("t1e_me_table2"),
               br(),

               h4("Type I error of Multilevel Egger's regression test"),
               h5("All conditions"),
               plotOutput("t1e_me_plot1"),
               br(),
               DT::dataTableOutput("t1e_me_table1")
             ))

    ),
    tabPanel("M.Egger: Power",
             sidebarLayout(sidebarPanel(
               h4("Select condition:"),
               selectInput("k_pw_me", "Number of studies", c(15, 30, 70)),
               selectInput("bt_pw_me", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                                "PB Strong", "PB Moderate"))
             ),
             mainPanel(
              br(),
               h4("Power of Multilevel Egger's regression test"),
               h5("Conditions with less than 1% non-covergence"),
               plotOutput("pw_me_plot2"),
              br(),
               DT::dataTableOutput("pw_me_table2"),
              br(),


               h4("Power of Multilevel Egger's regression test"),
               h5("All conditions"),
               plotOutput("pw_me_plot1"),
              br(),
               DT::dataTableOutput("pw_me_table1")
             )
             )

    ),

    tabPanel("M.PET-PEESE: Type I / Power",
            sidebarLayout(sidebarPanel(
                            h4("Select condition:"),
                            selectInput("k_rr_pet_int", "Number of studies", c(15, 30, 70)),
                            selectInput("bt_rr_pet_int", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                               "PB Strong", "PB Moderate"))
                          ),
                          mainPanel(
                            br(),
                            h4("Rejection rate of intercept in Multilevel PET / Multilevel Egger's regression test"),
                            h5("Conditions with less than 1% non-covergence"),
                            plotOutput("viz_pet_int2"),
                            br(),
                            DT::dataTableOutput("table_pet_int2"),
                            br(),


                            h4("Rejection rate of intercept in Multilevel PET / Multilevel Egger's regression test"),
                            h5("All conditions"),
                            plotOutput("viz_pet_int"),
                            br(),
                            DT::dataTableOutput("table_pet_int")
                          ))

    ),

    tabPanel("M.PET-PEESE: Bias",
             sidebarLayout(sidebarPanel(
                            h4("Select condition:"),
                             selectInput("k_adj_est", "Number of studies", c(15, 30, 70)),
                             selectInput("bt_adj_est", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                                     "PB Strong", "PB Moderate"))
                          ),
                           mainPanel(
                             br(),
                             h4("Bias Adjusted Estimate"),
                             h5("Conditions with less than 1% non-covergence."),
                             plotOutput("viz_bias_adj_est2"),
                             br(),
                             DT::dataTableOutput("table_bias_adj_est2"),
                             br(),

                             h4("Bias Adjusted Estimate"),
                             h5("All conditions"),
                             plotOutput("viz_bias_adj_est"),
                             br(),
                             DT::dataTableOutput("table_bias_adj_est")
                           ))),

    tabPanel("M.PET-PEESE: RMSE",
             sidebarLayout(sidebarPanel(
               h4("Select condition:"),
               selectInput("k_adj_est_rmse", "Number of studies", c(15, 30, 70)),
               selectInput("bt_adj_est_rmse", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                                  "PB Strong", "PB Moderate"))
             ),
             mainPanel(
               br(),
               h4("RMSE Adjusted Estimate"),
               h5("Conditions with less than 1% non-covergence."),
               plotOutput("viz_rmse_adj_est2"),
               br(),
               DT::dataTableOutput("table_rmse_adj_est2"),
               br(),

               h4("RMSE Adjusted Estimate"),
               h5("All conditions"),
               plotOutput("viz_rmse_adj_est"),
               br(),
               DT::dataTableOutput("table_rmse_adj_est")
             ))),

    tabPanel("Comparison M.PET vs. M.PEESE: Bias",
             sidebarLayout(sidebarPanel(
                             h4("Select condition:"),
                             selectInput("k_pet_peese", "Number of studies", c(15, 30, 70)),
                             selectInput("bt_pet_peese", "Selection Bias Type", c("ORB Strong", "ORB Moderate",
                                                                                "PB Strong", "PB Moderate")),
                             selectInput("model_smd_smdtr", "Effect Size", c("SMD", "Transformed SMD"))
                          ),
                           mainPanel(
                             h4("Bias intercept: Multilevel PET vs. Multilevel PEESE"),
                             h5("Conditions with less than 1% non-covergence."),
                             plotOutput("pet_peese_viz"),
                             br(),
                             DT::dataTableOutput("pet_peese_table"),
                             br(),
                             h4("Bias intercept: PET vs. PEESE"),
                             h5("All conditions"),
                             plotOutput("pet_peese_viz_all"),
                             br(),
                             DT::dataTableOutput("pet_peese_table_all"),
                             br()

                           ))

    )
  )
)

server <- function(input, output, session){

  output$t1e_me_plot1 <- renderPlot({
    viz_rejection_rate(df = df, num_studies = input$k_t1e_me, bias_type = input$bt_t1e_me)
  })

  output$t1e_me_table1 <- DT::renderDataTable({
    table_rejection_rate(df = df, num_studies = input$k_t1e_me, bias_type = input$bt_t1e_me)
  })

  output$t1e_me_plot2 <- renderPlot({
    viz_rejection_rate(df = df_sub, num_studies = input$k_t1e_me, bias_type = input$bt_t1e_me)
  })

  output$t1e_me_table2 <- DT::renderDataTable({
    table_rejection_rate(df = df_sub, num_studies = input$k_t1e_me, bias_type = input$bt_t1e_me)
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
    viz_adj_est_rmse(df = df, num_studies = input$k_adj_est_rmse, bias_type = input$bt_adj_est_rmse)
  })

  output$viz_rmse_adj_est2 <- renderPlot({
    viz_adj_est_rmse(df = df_sub, num_studies = input$k_adj_est_rmse, bias_type = input$bt_adj_est_rmse)
  })

  output$table_rmse_adj_est <- DT::renderDataTable({
    table_adj_est_rmse(df = df, num_studies = input$k_adj_est_rmse, bias_type = input$bt_adj_est_rmse)
  })

  output$table_rmse_adj_est2 <- DT::renderDataTable({
    table_adj_est_rmse(df = df_sub, num_studies = input$k_adj_est_rmse, bias_type = input$bt_adj_est_rmse)
  })

  output$viz_bias_adj_est <- renderPlot({
    viz_adj_est_bias(df = df, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$table_bias_adj_est <- DT::renderDataTable({
    table_adj_est_bias(df = df, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$viz_bias_adj_est2 <- renderPlot({
    viz_adj_est_bias(df = df_sub, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$table_bias_adj_est2 <- DT::renderDataTable({
    table_adj_est_bias(df = df_sub, num_studies = input$k_adj_est, bias_type = input$bt_adj_est)
  })

  output$conv_table1 <- DT::renderDataTable({
    table_perc_non_conv(df = df_conv)
  })

  output$viz_estimates_megger <- renderPlot({
    viz_hist_estimates_megger(bt = input$bt_viz_est, k = input$k_viz_est, d = input$d_viz_est, su_sv = input$su_sv_viz_est, p = input$p_viz_est)
  })

  output$viz_estimates_peese <- renderPlot({
    viz_hist_estimates_peese(bt = input$bt_viz_est, k = input$k_viz_est, d = input$d_viz_est, su_sv = input$su_sv_viz_est, p = input$p_viz_est)
  })

  output$descriptives_table <- renderTable({
    table_descriptives(df = df, bias_type = input$bt_viz_est, num_studies = input$k_viz_est, d = input$d_viz_est, su_sv = input$su_sv_viz_est, p = input$p_viz_est)
  })

  output$pet_peese_viz <- renderPlot({
    viz_compare_pet_peese_estimate(df = df_sub, num_studies = input$k_pet_peese, bias_type = input$bt_pet_peese, smd_stsmd = input$model_smd_smdtr)
  })

  output$pet_peese_table <- DT::renderDataTable({
    table_compare_pet_peese_estimate(df = df_sub, num_studies = input$k_pet_peese, bias_type = input$bt_pet_peese, smd_stsmd = input$model_smd_smdtr)
  })

  output$pet_peese_viz_all <- renderPlot({
    viz_compare_pet_peese_estimate(df = df, num_studies = input$k_pet_peese, bias_type = input$bt_pet_peese, smd_stsmd = input$model_smd_smdtr)
  })

  output$pet_peese_table_all <- DT::renderDataTable({
    table_compare_pet_peese_estimate(df = df, num_studies = input$k_pet_peese, bias_type = input$bt_pet_peese, smd_stsmd = input$model_smd_smdtr)
  })

  output$overall_table_descr <- DT::renderDataTable({
    overall_table_descriptives(df = df)
  })


}

shinyApp(ui, server)
