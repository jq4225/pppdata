library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(readxl)

zip_regressors <- read_excel('Zipregressors.xlsx', sheet = "Sheet1")

zip_regression <- read_excel('zip_table.xlsx', sheet = "Sheet2")

county_regression <- read_excel('zip_table.xlsx', sheet = "Sheet3")

county_regressors <- read_excel('Zipregressors.xlsx', sheet = "Sheet2")

race_days <- readRDS('race_days2.rds') %>%
  mutate(state = toupper(state))

descriptives_orig <- read_excel('descriptives.xlsx', sheet = "Sheet1")

descriptives_sample <- read_excel('descriptives.xlsx', sheet = "Sheet2")


# Define UI 

ui <<- navbarPage("What Determines Paycheck Protection Program Waiting Times?",
                  theme = shinytheme("lumen"),
                  tabPanel("Descriptives",
                           mainPanel(
                             h2("Descriptives"),
                             p("The heatmaps provide a visual distribution of the locations of my loan samples.
                               Despite excluding large parts of the original loan data due to lack of bank lending information,
                               both the original loan locations and my sample are distributed similarly. I also provide
                               a table of descriptive statistics showing how loan originating ZIP codes average with respect to 
                               various demographic variables."),
                             
                             tabsetPanel(type = "tabs",
                                         tabPanel("Full Loan Location Heatmap", img(src = "original_heatmap.png")),
                                         tabPanel("Sample Loan Location Heatmap", img(src = "new_heatmap.png")),
                                         tabPanel("Waiting Times by Race", 
                                                  fluidRow(
                                                    p("Nationally, there is a slightly positive correlation between
                                                      black populations and wait times without any controls. You
                                                      can see state-level visualizations here."),
                                                    selectizeInput("stateInput", "State",
                                                                   choices = state.abb,  
                                                                   selected ="AK", multiple = FALSE),
                                                    column(6, plotOutput("statePlot")),
                                                    column(6, plotOutput("natPlot"))
                                                  )),
                                         tabPanel("Sample Descriptives",
                                                  fluidRow(
                                                    p("See the Regressions tab for a full explanation of what each
                                                      regressor variable means."),
                                                    column(6, gt_output('descriptives_orig')),
                                                    column(6, gt_output('descriptives_sample'))
                                                  ))
                           ))),
                  tabPanel("Regressions",
                           mainPanel(
                             h2("Regression Results"),
                             p("Click the tabs to view the results of regressions on the ZIP and county level. The residuals tab
                               provides a visual indicator of heteroskedasticity in the sample. To correct for this, all standard errors
                               are heteroskedasticity- and clustering-robust."),
                             tabsetPanel(type = "tabs",
                                         tabPanel("ZIP Code Regressions", gt_output('zip_regression')),
                                         tabPanel("County Regressions", gt_output('county_regression')),
                                         tabPanel("Regression Coefficient Definitions",
                                                  fluidRow(
                                                    column(6, gt_output('zip_defns')),
                                                    column(6, gt_output('county_defns'))
                                                  )),
                                         tabPanel("Residuals", 
                                                  fluidRow(
                                                    img(src = "residuals_zipregression.png"),
                                                    p("This displays a plot of the residuals against the outcome variable
                                                      for the ZIP code regression, Model 4. We can see that there is
                                                      some more propensity for error at the upper ranges
                                                      of the days_approved variable. The big gap in the days_approved
                                                      is when Congress ran out of money for the program and it had to be renewed
                                                      several weeks later, rather than a problem with my data cleaning."))))
                           )),
                  tabPanel("About",
                           mainPanel(
                               h2("About"),
                               tags$a(href="https://github.com/jq4225/pppdata", 
                                      "Github repo here!"),
                               
                               p("The COVID-19 pandemic triggered an unprecedented economic shock. To 
                                 help small businesses without access to public financial markets survive,
                                 Congress passed the CARES Act in March 2020, which included a small business
                                 lending program called the Paycheck Protection Program. Here, I analyze
                                 the SBA's dataset on all PPP loans to find whether background demographic
                                 factors like race predict decreases in loan time."),
                               
                               p("I am using a couple main sources of data for my project.
                                  First, I'm using the American Community Survey's 2018 projections,
                                  which include racial, demographic, and income breakdowns for 
                                 every ZIP code in the US."),
                               
                               p("I'm also using an FDIC list of all the 
                                 banking locations in the US broken down by ZIP code. Finally,
                                 I'm using the SBA's dataset on Paycheck Protection Loans of more 
                                 than $150,000 given this year, as of August 8th."),
                               
                               p("PPP program data itself comes from the Small Business Administration."),
                               
                               p("Additional data comes from the 2014 ZIP Code Business Survey 
                                 and hand-coded data on bank policies, collected manually."),
                               
                               p("All COVID case data comes from the New York Times, and the 
                                 data on COVID policy responses comes from the Kaiser Family Foundation."),
                               
                               p("Crime rate and inequality data are sourced from both the ACS and the
                                 County Health Rankings project.")
                           ))
)

# Define server logic 
server <- function(input, output) {
    
    
    output$natPlot <- renderPlot({
      race_days %>%
        group_by(cuts) %>%
        summarize(mean_days = mean(days_to_approval), .groups = "drop") %>%
        ggplot(aes(x = cuts, y = mean_days)) +
        geom_col(fill = "darkblue") +
        geom_hline(yintercept = 35.59, color = "red",
                   linetype = "dashed") +
        scale_x_discrete(name = "Black Percent",
                         labels = c("0-10%",
                                    "10-20%",
                                    "20-30%",
                                    "30-40%",
                                    "40-50%",
                                    "50-60%",
                                    "60-70%",
                                    "70-80%",
                                    "80-90%",
                                    "90-100%")) +
        labs(title = "Mean Loan Waiting Time, National",
             subtitle = 
               "Dashed line indicates national average.",
             y = "Mean Waiting Time (days)")
    })
    
    output$statePlot <- renderPlot({
      race_days %>%
        filter(state == input$stateInput) %>%
        group_by(cuts) %>%
        summarize(mean_days = mean(days_to_approval), .groups = "drop") %>%
        ggplot(aes(x = cuts, y = mean_days)) +
          geom_col(fill = "darkblue") +
          geom_hline(yintercept = 35.59, color = "red",
                     linetype = "dashed") +
          scale_x_discrete(name = "Black Percent",
                           labels = c("0-10%",
                                      "10-20%",
                                      "20-30%",
                                      "30-40%",
                                      "40-50%",
                                      "50-60%",
                                      "60-70%",
                                      "70-80%",
                                      "80-90%",
                                      "90-100%")) +
        labs(title = paste("Mean Loan Waiting Time,", input$stateInput),
             subtitle = 
             "Dashed line indicates national average.",
             y = "Mean Waiting Time (days)")
    })
    
    output$descriptives_orig <- render_gt({
      descriptives_orig %>%
        gt() %>%
        tab_header(title = "Complete Dataset Descriptives") %>%
        fmt_number(columns = 3:4, decimals = 2)
    })
    
    output$descriptives_sample <- render_gt({
      descriptives_sample %>%
        gt() %>%
        tab_header(title = "Final Sample Descriptives") %>%
        fmt_number(columns = 3:4, decimals = 2)
    })
    
    output$zip_regression <- render_gt({
      zip_regression %>%
        gt() %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05")
    })
    
    output$county_regression <- render_gt({
      county_regression %>%
        gt() %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05")
    })
    
    output$zip_defns <- render_gt({
      zip_regressors %>%
        gt() %>%
        tab_header(title = "ZIP Code Regressors")

    })
    
    output$county_defns <- render_gt({
      county_regressors %>%
        gt() %>%
        tab_header(title = "County Regressors")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
