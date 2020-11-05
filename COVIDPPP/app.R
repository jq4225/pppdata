library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(readxl)

#ppp <- readRDS('ppp_allvars_1028.rds')

#linear_reg <- readRDS('all_linear.rds')

zip_regressors <- read_excel('Zipregressors.xlsx', sheet = "Sheet1")

county_regressors <- read_excel('Zipregressors.xlsx', sheet = "Sheet2")


# Define UI for application that draws a histogram

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
                                         tabPanel("Sample Descriptives")
                           ))),
                  tabPanel("Regressions",
                           mainPanel(
                             h2("Regression Results"),
                             p("Click the tabs to view the results of regressions on the ZIP and county level. The residuals tab
                               provides a visual indicator of heteroskedasticity in the sample. To correct for this, all standard errors
                               are heteroskedasticity- and clustering-robust."),
                             tabsetPanel(type = "tabs",
                                         tabPanel("ZIP Code Regressions"),
                                         tabPanel("County Regressions"),
                                         tabPanel("Regression Coefficient Definitions",
                                                  fluidRow(
                                                    column(6, gt_output('zip_defns')),
                                                    column(6, gt_output('county_defns'))
                                                  )),
                                         tabPanel("Residuals", img(src = "residuals_zipregression.png")))
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
