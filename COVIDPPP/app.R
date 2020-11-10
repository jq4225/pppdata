library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(readxl)

# Reading in files here: most of these are excel files converted from 
# htmlreg and other functions b/c I made tables for the papers I'm writing
# before I did anything with this.

zip_regressors <- read_excel('regressors.xlsx', sheet = "Sheet1")

zip_regression <- read_excel('zip_table.xlsx', sheet = "Sheet2")

county_regression <- read_excel('zip_table.xlsx', sheet = "Sheet3")

county_regressors <- read_excel('regressors.xlsx', sheet = "Sheet2")

race_days <- readRDS('race_days2.rds') %>%
  mutate(state = toupper(state))

descriptives_orig <- read_excel('descriptives.xlsx', sheet = "Sheet1")

descriptives_sample <- read_excel('descriptives.xlsx', sheet = "Sheet2")


# Define UI 

ui <- navbarPage("What Determines Paycheck Protection Program Waiting Times?",
                  theme = shinytheme("sandstone"),
                  tabPanel("Introduction",
                           mainPanel(
                             fluidRow(
                             h2("Introduction"),
                             p("The COVID-19 pandemic triggered an unprecedented economic shock. Social distancing measures
                               such as stay-at-home orders and mandatory business closures, combined with the fear of the
                               pandemic, cratered consumption, especially harming small businesses with no access
                               to public financial markets. According to some estimates, over 40 percent of small
                               businesses closed because of the pandemic, many of whom had saved just two weeks' of
                               operating costs. Without a safety net for small businesses, US policymakers passed the
                               Coronavirus Aid, Relief, and Economic Security (CARES) Act, which included a novel
                               Paycheck Protection Program (PPP) loan program."),
                             p("However, substantial evidence indicates that borrowers from minority communities
                               face systematic discrimination when accessing loans. For example, black borrowers are more likely
                               to be turned down for mortgages with equivalent credit scores. Here, I try to estimate
                               how much coming from a locality with higher minority populations influences
                               waiting times for loans. I aggregate lending by both ZIP and county and match loan-level
                               data with demographics, such as political preferences, income and race."),
                             p("Unlike other loan programs, the PPP is government guaranteed. Banks face no credit risk
                               in lending, and anyone should qualify for a loan if they run a qualifying small business. 
                               Therefore, differences in loan waiting times should be a reflection of taste-based discrimination,
                               rather than statistical justifications based on racial disparities in default rates or 
                               socioeconomic status."),
                             p("I use ordinary least squares (OLS) regression, controlling for a variety of background
                               variables, to test the impact of increasing the percentage of racial minorities in a local
                               population on loan waiting times, defined as the time between loan applications opening
                               and the loan approval date.")
                           ))),
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
                                                    p("These are descriptive statistics for the loans aggregated by ZIP code.
                                                    See the Regressions tab for a full explanation of what each
                                                      regressor variable means."),
                                                    column(6, gt_output('descriptives_orig')),
                                                    column(6, gt_output('descriptives_sample'))
                                                  ))
                           ))),
                  tabPanel("Results",
                           mainPanel(
                             h2("Regression Results"),
                             p("Click the tabs to view the results of regressions on the ZIP and county level. The residuals tab
                               provides a visual indicator of heteroskedasticity in the sample. To correct for this, all standard errors
                               are heteroskedasticity- and clustering-robust."),
                             tabsetPanel(type = "tabs",
                                         tabPanel("ZIP Code Regressions"),
                                         tabPanel("County Regressions"),
                                         tabPanel("Full Regression Tables", 
                                                  fluidRow(
                                                    p("These are the full regression tables for the ZIP and county level. You can see
                                                      which variables are defined as economic, loan-specific, etc. by looking at the 
                                                      regressor definitions."),
                                                    column(6, gt_output('zip_regression')),
                                                    column(6, gt_output('county_regression'))
                                                  )),
                                         tabPanel("Regression Coefficient Definitions",
                                                  fluidRow(
                                                    column(6, gt_output('zip_defns')),
                                                    column(6, gt_output('county_defns'))
                                                  )),
                                         tabPanel("Residuals", 
                                                  fluidRow(
                                                    img(src = "residuals_abs.png"),
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
                               fluidRow(
                                 column(6,
                                        list(
                                          h3("Me"),
                                          p("Hi, my name's Justin. I'm a junior in Winthrop House studying Applied
                                            Math and Economics with a secondary in Government. 
                                            This is a final project for Gov 50, a data science course at Harvard University."),
                                          p("If you'd like to get in touch with me, you can reach me at ",
                                            a(href = "mailto: qij@college.harvard.edu", "qij@college.harvard.edu", .noWS = "outside"),
                                            " or my ", a(href = "https://www.linkedin.com/in/justin-qi/", "LinkedIn", .noWS = "outside"), 
                                            ". Please contact me for access to the original data files, which
                                            were too large to upload to Github. Thanks for stopping by!"))
                                        ),
                                 
                                 # Just listing sources for everything I've done here. 
                                 
                                 column(6, 
                                        list(
                                          h3("Data"),
                                          tags$a(href="https://github.com/jq4225/pppdata", 
                                                 "Github repo here!"),
                                          p(strong("Loan level data: "), "All loan microdata came from the SBA's website, with 
                                            loans through August 8, 2020, when the PPP program closed"),
                                          
                                          p(strong("Demographic data: "), "Most demographic data (median income, race, 
                                            marital status, education, and income inequality) came from the 2018
                                            American Community Survey 5-year esimates."),
                                          
                                          p(strong("Political preferences: "), "On the ZIP code level, I used data from the
                                                   Cook Political Report's Partisan Voting Index, reported on the Congressional
                                                   District level and crosswalked over to ZIP codes. On the county level,
                                                   I used the MIT Election Data Science Lab's dataset of the percent of each
                                                   county's voters that voted Republican in the 2016 presidential election."),
                                          
                                          p(strong("Business data: "), "Data on aggregate business activity (business counts and payroll) 
                                          came from the 2014 and 2016 ZIP Code Business Patterns Survey and the 2016 County Business
                                            Patterns Survey by the Census Bureau."),
                                          
                                          p(strong("GDP: "), "County GDP statistics came from the Bureau of Economic Analysis's public
                                            figures on estimated 2017 county GDP."),
                                          
                                          p(strong("COVID-19 cases and deaths: "), "COVID case data came from the New York Times' 
                                            county-level tracking of cases and deaths. On the ZIP level, I weighted ZIP codes by the
                                            proportion of residential addresses in each county to calculate estimates for cases."),
                                          
                                          p(strong("COVID-19 policy responses: "), "State policy responses to COVID-19 and their
                                            implementation/rollback dates came from the Kaiser Family Foundation's State Policy
                                            Tracker, last updated on July 30, 2020."),
                                          
                                          p(strong("Bank lending policies: "), "Bank lending policies were hand collected from
                                            individual bank websites and lending associations that provided lists of different
                                            banks' requirements for accessing a PPP loan."),
                                          
                                          p(strong("Unemployment: "), "Unemployment data came from the Bureau of Labor Statistics' 
                                            monthly county-level unemployment figures. As with COVID data, I created estimates
                                            for ZIP code unemployment rates by weighting these figures by the proportion of residential
                                            addresses of each ZIP code that reside in a particular county."),
                                          
                                          p(strong("Population density: "), "Rural/urban ZIP code data came from the Health Resources
                                            and Services Administration. Population density data came from the 2010 US Census."),
                                          
                                          p(strong("Crime: "), "Crime rates were sourced from the County Health Rankings report as of
                                            2019. These statistics were aggregated from the FBI Uniform Crime Report.")
                                        ))
                               )
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
             y = "Mean Waiting Time (days)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
             y = "Mean Waiting Time (days)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$descriptives_orig <- render_gt({
      descriptives_orig %>%
        gt() %>%
        tab_header(title = "Complete Dataset Descriptives")
    })
    
    output$descriptives_sample <- render_gt({
      descriptives_sample %>%
        gt() %>%
        tab_header(title = "Final Sample Descriptives")
    })
    
    output$zip_regression <- render_gt({
      zip_regression %>%
        gt() %>%
        tab_header(title = "ZIP Codes") %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05")
    })
    
    output$county_regression <- render_gt({
      county_regression %>%
        gt() %>%
        tab_header(title = "Counties") %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05")
    })
    
    output$zip_defns <- render_gt({
      zip_regressors %>%
        gt() %>%
        tab_header(title = "ZIP Code Regressors") %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(
            columns = vars(`Variable name`),
            rows = `Variable name` %in% c("Demographic Variables",
                                          "Loan-specific Variables",
                                          "Economic Variables",
                                          "COVID-19 Variables"))

        )
    })
    
    output$county_defns <- render_gt({
      county_regressors %>%
        gt() %>%
        tab_header(title = "County Regressors") %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_style(
          style = cell_text(weight = "bold"),
          locations = cells_body(
            columns = vars(`Variable name`),
            rows = `Variable name` %in% c("Demographic Variables",
                                          "Loan-specific Variables",
                                          "Economic Variables",
                                          "COVID-19 Variables"))
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
