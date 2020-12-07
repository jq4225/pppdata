library(shiny)
library(tidyverse)
library(shinythemes)
library(gt)
library(readxl)
library(openintro)
library(usmap)
library(gganimate)
library(gifski)

# Transformr is a requirement for doing the fancy animated graphs for 
# polygonal shapes, which is why it's included.

library(transformr)

# Reading in files here: most of these are excel files converted from 
# htmlreg and other functions b/c I made tables for the papers I'm writing
# before I did anything with this.

zip_regressors <- read_excel('regressors.xlsx', sheet = "Sheet1")

zip_regression <- read_excel('zip_table.xlsx', sheet = "zip")

zip_simple <- read_excel('simple_regression2.xlsx', sheet = "zip2")

zip_simple_check <- read_excel('simple_regression2.xlsx', sheet = "zip")

county_regression <- read_excel('zip_table.xlsx', sheet = "countybiz")

county_regressors <- read_excel('regressors.xlsx', sheet = "Sheet2")

county_simple <- read_excel('simple_regression2.xlsx', sheet = "countybiz")

race_days <- readRDS('race_days3.rds')

descriptives_orig <- read_excel('descriptives.xlsx', sheet = "Sheet1")

descriptives_sample <- read_excel('descriptives.xlsx', sheet = "Sheet2")

zip_banks <- read_excel('bankbybank.xlsx', sheet = "zip")

county_banks <- read_excel('bankbybank.xlsx', sheet = "county")

zip_race_graph <- readRDS('marginalrace.rds')

county_race_graph <- readRDS('marg_race_biz.rds')

zip_racesq_graph <- readRDS('marginal_race_sq_zip.rds')

county_racesq_graph <- readRDS('marg_race_biz_sq.rds')

national_banks_zip <- readRDS('national_bank_graph.rds')

national_banks_county <- readRDS('national_bank_graph_county.rds')

state_cases <- readRDS('state_cases.rds')

unemploy <- readRDS('unemploy.rds') %>%
  mutate(month = as.integer(month)) %>%
  filter(month < 12)


# Define UI 

ui <- navbarPage("Equitable Lending? Don't Bank on It: Racial Disparities in the Paycheck Protection Program",
                  theme = shinytheme("sandstone"),
                  tabPanel("Introduction",
                           mainPanel(
                             h2("Introduction"),
                             p("The COVID-19 pandemic triggered an unprecedented economic shock. Social distancing measures
                               such as stay-at-home orders and mandatory business closures, combined with the fear of the
                               pandemic, cratered consumption, especially harming small businesses with no access
                               to public financial markets. The pandemic shows little signs of abating absent a vaccine,
                               with many states reporting renewed waves of cases in recent weeks."),
                             
                             # This is just a simple plot of COVID unemployment over time, animated! I include a tab for
                             # non-animated graphs of cases and deaths (which could've been animated but would just
                             # not be super educational if they were)
                             
                             tabsetPanel(type = "tabs",
                                         tabPanel("Cases and Deaths",
                                                  fluidRow(
                                                    selectizeInput("stateInput1", "State",
                                                                   choices = state.abb,  
                                                                   selected ="AL", multiple = FALSE),
                                                    column(6, plotOutput("covidCases")),
                                                    column(6, plotOutput("covidDeaths"))
                                                  )),
                                         tabPanel("Unemployment",
                                                  fluidRow(
                                                    selectizeInput("stateInput2", "State",
                                                                   choices = state.abb,  
                                                                   selected ="AL", multiple = FALSE),
                                                    p("Please allow some time for the graphics to load."),
                                                    imageOutput("unemploy")
                                                  ))

                             ),
                             

                             
                             p("According to some estimates, over 40 percent of small
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
                               population on loan waiting times, defined as the business days between loan applications opening
                               and the loan approval date."),
                             p("Later, I also attempt to disaggregate these results by individual banks and bank type, using the 
                               bank names provided in the SBA's dataset."),
                             
                             # Here, reference the mathjax object in the server part to see the actual code for the equations.
                             
                             p("The basic OLS setup is as follows in the ZIP code case: "),
                             uiOutput('OLS1'),
                             p(),
                             p("Later, I add in additional interaction terms between race and other demographic variables."),
                             
                             p("To check a more precise measurement of applicant race, I then aggregate these loans on the county level
                               and regress waiting times against the share of businesses owned by minorities."),
                             uiOutput('OLS2'),
                             p(),
                             p("This provides a slightly better proxy for what race business owner applicants to the PPP will be,
                               at the expense of having larger intra-county variation in individual applicant race.")
                           )),
                 
                  tabPanel("Descriptives",
                           mainPanel(
                             h2("Descriptives"),
                             p("The heatmaps provide a visual distribution of the locations of my loan samples.
                               Despite excluding large parts of the original loan data due to lack of bank lending information,
                               both the original loan locations and my sample are distributed similarly. I also provide
                               a table of descriptive statistics showing how loan originating ZIP codes average with respect to 
                               various demographic variables."),
                             
                             tabsetPanel(type = "tabs",
                                         
                                         # Heatmaps are jpg's because they take a very long time to render.
                                         
                                         tabPanel("Full Loan Location Heatmap", img(src = "original_heatmap.jpg", height="75%", width="75%")),
                                         tabPanel("Sample Loan Location Heatmap", img(src = "new_heatmap.jpg", height="75%", width="75%")),
                                         
                                         # This is unadjusted calculations of average waiting times in different states, faceted
                                         # by minority percentages. Two tabs -- one is the national plot displaying averages at every value
                                         # nationally, one is a state plot that also includes the national averages at every value for comparison.
                                         
                                         tabPanel("Waiting Times by Race", 
                                                  fluidRow(
                                                    p("Nationally, there is a slightly positive correlation between
                                                      minority populations and wait times without any controls. You
                                                      can see state-level visualizations here."),
                                                    selectizeInput("stateInput3", "State",
                                                                   choices = state.abb,  
                                                                   selected ="AK", multiple = FALSE),
                                                    column(6, plotOutput("statePlot")),
                                                    column(6, plotOutput("natPlot"))
                                                  )),
                                         
                                         # Sample descriptive! Just displaying tables for average values of variables.
                                         
                                         tabPanel("Sample Descriptives",
                                                  fluidRow(
                                                    p("These are descriptive statistics for the loans aggregated by ZIP code.
                                                    See the Results tab for a full explanation of what each
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
                                         tabPanel("ZIP Codes",
                                                  
                                                  # Very sadly I couldn't find a way to include fixed effects in ggeffects -- I don't have the 
                                                  # RAM for it :( The two plots are different specifications for the ggeffect input.
                                                  
                                                  
                                                  fluidRow(
                                                    column(6, plotOutput('marginalRace1')),
                                                    column(6, plotOutput('marginalRace2')),
                                                    p("As you can see, there is a positive and significant relationship between the percentage of
                                                      minorities in a community and the predicted waiting times, holding constant other demographic,
                                                      economic, loan-specific, and COVID-19-related variables. This relationship disappears once we
                                                      include additional interaction terms, but this is likely due to the high correlation between them.
                                                      Here, I also present supplemental regression results replacing ZIP-level estimates for COVID-19 case/death,
                                                      unemployment, and crime statistics with original county data to correct for measurement error. These
                                                      results are robust to multiple robustness checks not included here, including negative binomial regression.
                                                      The rightmost regression table below shows one of the robustness checks, where I replaced data that I had to
                                                      create ZIP-level estimates for (COVID cases/deaths, crime rates, and unemployment) with the original county-level
                                                      data, to correct for any problems with my estimations."),
                                                    p("Note that due to computational limitations, the graphics show predicted waiting times without
                                                      fixed effects in the regression. While the results with and without fixed effects are qualitatively similar,
                                                      the fixed effects regressions have lower coefficients on the minority percentage variable."),
                                                    
                                                    # These are the abbreviated regression tables -- I have another tab for the full ones.
                                                    
                                                    column(6, gt_output('zip_simple')),
                                                    column(6, gt_output("zip_simple_check"))
                                                  )),
                                         
                                         # Same limitations for the plots on the county levels.
                                         
                                         tabPanel("Counties",
                                                  fluidRow(
                                                    column(6, plotOutput('marginalRace3')),
                                                    column(6, plotOutput('marginalRace4')),
                                                    p("On the county leve, the relationship between race and waiting times becomes insignificant after COVID-19 variables
                                                      are included. However, including additional interaction terms suggests that the relationship between race
                                                      and waiting times is significant and nonlinear, as you can see in the abbreviated regression table below. It appears that
                                                      the effect of having a higher minority percentage tapers off at higher levels of minority populations.
                                                      While the standard errors appear quite large, this is likely because county racial breakdowns are just worse
                                                      predictors of individual race than ZIP-code level racial breakdowns, since there is more intra-county variation.
                                                      This means that the ZIP code-level results are likely more reliable."),
                                                    p("Note that due to computational limitations, the graphics show predicted waiting times without
                                                      fixed effects in the regression. While the results with and without fixed effects are qualitatively similar,
                                                      the fixed effects regressions have lower coefficients on the minority percentage variable."),
                                                    
                                                    # Again, this is the abbreviated table.
                                                    
                                                    gt_output('county_simple')
                                                  )),
                                         tabPanel("Full Tables", 
                                                  fluidRow(
                                                    p("These are the full regression tables for the ZIP and county level. You can see
                                                      which variables are defined as economic, loan-specific, etc. by looking at the 
                                                      regressor definitions."),
                                                    column(6, gt_output('zip_regression')),
                                                    column(6, gt_output('county_regression'))
                                                  )),
                                         tabPanel("Bank Type Comparisons",
                                                  
                                                  # This tab indicates the advantage of going to a national bank for your loan in racial
                                                  # disparities in waiting times. I build in a ggeffects plot here as well!
                                                  
                                                  fluidRow(
                                                    p("I also try to disaggregate racial disparities by type of bank. Here, I provide
                                                      regressions for a couple of the country's largest banks to see if applying to
                                                      these banks is positively associated with more racial disparities. I also break
                                                      loan applications down by two types of banks I can easily identify in the dataset:
                                                      large nationally chartered, which are identified by the words 'national association' or
                                                      'n.a.' in their names in the datset, and credit unions. All of these regressions
                                                      exclude interaction terms between minority_percent and other variables, but
                                                      include all of the background variables, as in Model 4 in the ZIP and county 
                                                      regressions."),
                                                    
                                                    # The ZIP and county differences here are actually fairly similar, unlike when we
                                                    # only look at minority population.
                                                    
                                                    column(6, plotOutput('zip_banks_graph')),
                                                    column(6, plotOutput("county_banks_graph")),
                                                    p("As you can see, aggregating by both ZIPs and counties indicates that large, national
                                                      banks are likely to grant loans to high-minority localities faster, holding constant 
                                                      all of the variables identified in Model 4. The absolute advantage of going to a
                                                      national bank is larger at higher minority percentages. The abbreviated regression
                                                      tables for both individual banks and bank types are shown below: the 'bank' indicator
                                                      refers to a variable that takes the value 1 when the lending institution is the bank indicated
                                                      in the column title and 0 otherwise -- so, its definition is different in each model."),
                                                    
                                                    # Only the abbreviated regression tables here again.
                                                    
                                                    column(6, gt_output('zip_banks')),
                                                    column(6, gt_output('county_banks'))
                                                  )),
                                         
                                         
                                         # Simple table of what each coefficient means / how it was calculated.
                                         
                                         tabPanel("Coefficient Definitions",
                                                  p("On the county level, the bank-disaggregated regressions use minority population percent, while
                                                    the aggregate analysis uses the percent of local businesses owned by racial minorities -- this is 
                                                    why both variables are displayed."),
                                                  fluidRow(
                                                    column(6, gt_output('zip_defns')),
                                                    column(6, gt_output('county_defns'))
                                                  )),
                                         
                                         # This graph is slightly outdated because it doesn't include fixed effects, but the general idea
                                         # is the same -- I'll update the graph soon.
                                         
                                         tabPanel("Residuals", 
                                                  fluidRow(
                                                    img(src = "residuals_abs.jpg", width = "50%", height = "50%"),
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
                               
                               # Me being arrogant
                                 
                                          h3("Me"),
                                          p("Hi, my name's Justin. I'm a junior in Winthrop House studying Applied
                                            Math and Economics with a secondary in Government. 
                                            This is a final project for Gov 50, a data science course at Harvard University."),
                                          p("If you'd like to get in touch with me, you can reach me at ",
                                            a(href = "mailto: qij@college.harvard.edu", "qij@college.harvard.edu", .noWS = "outside"),
                                            " or my ", a(href = "https://www.linkedin.com/in/justin-qi/", "LinkedIn", .noWS = "outside"), 
                                            ". Please contact me for access to the original data files, which
                                            were too large to upload to Github, or for the paper describing these results in more detail,
                                            which includes all of the robustness checks and additional extensions exploring the role of
                                            corporate social responsibility agreements. 
                                            Thanks for stopping by!"),
                                        
                                 
                                 # Just listing sources for everything I've done here. 
                                 
                                 
                                          h3("Data"),
                                          tags$a(href="https://github.com/jq4225/pppdata", 
                                                 "Github repo here!"),
                                          p(strong("Loan level data: "), "All loan microdata came from the SBA's website, with 
                                            loans through August 8, 2020, when the PPP program closed"),
                                          
                                          p(strong("Demographic data: "), "Most demographic data (median income, race, 
                                            marital status, education, and income inequality) came from the 2018
                                            American Community Survey 5-year esimates."),
                                          
                                          p(strong("Business ownership data: "), "Data on the proportion of local businesses
                                            owned by minorities came from the 2017 Annual Business Survey."),
                                          
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
                               )
                           )
)

# Define server logic 

server <- function(input, output) {
  
  
  # COVID cases and deaths plots from the intro panel -- not much to 
  # explain here, it just displays the total cases/deaths by date.
  
    output$covidCases <- renderPlot({
      state_cases %>%
        filter(state == abbr2state(input$stateInput1)) %>%
        ggplot(aes(x = date, y = cases)) +
          geom_line(color = "dodgerblue", size = 1) +
          labs(x = "Month", y = "Cases",
             title =  paste("Total COVID-19 Cases,",
                            abbr2state(input$stateInput1))) +
        theme(legend.position = "none") +

        # I feel like there's a better way to do this but I didn't find it

        scale_x_date(breaks = c(as.Date("2020/02/01"),
                                as.Date("2020/03/01"),
                                as.Date("2020/04/01"),
                                as.Date("2020/05/01"),
                                as.Date("2020/06/01"),
                                as.Date("2020/07/01"),
                                as.Date("2020/08/01"),
                                as.Date("2020/09/01"),
                                as.Date("2020/10/01"),
                                as.Date("2020/11/01")),
                     labels = c("Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug",
                                "Sept", "Oct", "Nov")) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                           label = scales::comma) +
        theme_classic()
    })

    output$covidDeaths <- renderPlot({
      state_cases %>%
        filter(state == abbr2state(input$stateInput1)) %>%
        ggplot(aes(x = date, y = deaths)) +
        geom_line(color = "firebrick4", size = 1) +
        labs(x = "Month", y = "Deaths",
             title =  paste("Total COVID-19 Deaths,",
                            abbr2state(input$stateInput1)),
             caption = "Source: New York Times") +
        theme(legend.position = "none") +
        scale_x_date(breaks = c(as.Date("2020/02/01"),
                                as.Date("2020/03/01"),
                                as.Date("2020/04/01"),
                                as.Date("2020/05/01"),
                                as.Date("2020/06/01"),
                                as.Date("2020/07/01"),
                                as.Date("2020/08/01"),
                                as.Date("2020/09/01"),
                                as.Date("2020/10/01"),
                                as.Date("2020/11/01")),
                     labels = c("Feb", "Mar", "Apr",
                                "May", "Jun", "Jul", "Aug",
                                "Sept", "Oct", "Nov")) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                           label = scales::comma) +
        theme_classic()

    })
    
    # Animated plot of unemployment rates by county (aka FIPS codes) -- I 
    # set some custom options for the gif rendering to make it run a little
    # bit faster, but it's still quite slow. Note that these graphs only have
    # unemployment from April to August b/c that's the relevant dates for PPP.
    
    output$unemploy <- renderImage({
      outfile <- tempfile(fileext='.gif')
      
      plot1 <- plot_usmap(regions = "counties", color = "grey50", 
                          data = unemploy,
                          values = "unemployment_rate_percent",
                          include = input$stateInput2) + 
        theme(panel.background = element_rect(color = "black", fill = "white"),
              legend.position = "right") +
        scale_fill_distiller(type = "seq",
                             palette = "YlGnBu",
                             na.value = "grey50",
                             direction = 1,
                             name = "Unemployment Rate (Percent)",
                             n.breaks = 5) + 
        transition_states(month) + 
        
        # This makes a smooth transition since my data is all month to month.
        
        shadow_wake(wake_length = 0.05, alpha = FALSE) + 
        labs(title = 
               paste("Unemployment Rate in", abbr2state(input$stateInput2),
                     "{closest_state}/2020, by county"),
             caption = "Source: Bureau of Labor Statistics")
      
      # Displaying animated plots in Shiny is easiest done by saving them as 
      # gifs and then rendering the gifs, which is why this is render_image
      # not render_plot
      
      anim_save("unemploy.gif", animate(plot1, nframes = 30,
                                        detail = 2,
                                        fps = 4,
                                        width = 6,
                                        height = 6,
                                        res = 150,
                                        units = "in"))

      list(src = "unemploy.gif",
           contentType = 'image/gif',
           width = "450px",
           height = "100%"
      )}, deleteFile = TRUE)
    
    
    # Equation LaTeX here! -- both setups are similar just with differnet
    # variables.
    
    output$OLS1 <- renderUI({
      withMathJax(
        "$$y_i = \\alpha + \\beta minority_{ZIP} + X'_i \\gamma + \\epsilon_i$$
        Where \\(y_i\\) is the number of business days until the loan is approved, 
        \\(\\alpha\\) are county and lender fixed effects, 
        \\(minority_{ZIP}\\) is the percent of the ZIP code's population that is non-white, 
        and \\(X_i\\) is a vector of controls (e.g. loan size, local demographics).")
    })
    
    output$OLS2 <- renderUI({
      withMathJax(
        "$$y_i = \\alpha + \\beta percentminoritybiz_{FIPS} + X'_i \\gamma + \\epsilon_i$$
        Where \\(y_i\\) is the number of business days until the loan is approved, 
        \\(\\alpha\\) are lender and state fixed effects, 
        \\(percentminoritybiz_{FIPS}\\) is the percent of the county's businesses owned by racial minorities, 
        and \\(X_i\\) is a vector of controls (e.g. loan size, local demographics).")
    })
    
    # Descriptive plots on the waiting times tab of the Descriptives tab. 
    # These just display average waiting times by racial percentages for
    # each state, unadjusted for anything or controls.
    
    output$natPlot <- renderPlot({
      race_days %>%
        filter(state == "National") %>%
        ggplot(aes(x = cuts, y = mean_days)) +
        geom_col(fill = "lightblue3") +
        geom_hline(yintercept = 24.73, color = "red",
                   linetype = "dashed") +
        scale_x_discrete(name = "Non-white as Percent of Population",
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
             y = "Mean Waiting Time (business days)",
             caption = "Source: Small Business Administration") + 
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$statePlot <- renderPlot({
      race_days %>%
        filter(state %in% c(input$stateInput3, "National")) %>%
        group_by(state, cuts) %>%
        ggplot(aes(x = cuts, y = mean_days, fill = state)) +
          geom_col(position = "dodge") +
          scale_x_discrete(name = "Non-white as Percent of Population",
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
        scale_fill_manual(name = "Region", values = c("coral", "lightblue3"),
                          labels = c(paste(abbr2state(input$stateInput3)),
                                     "National")) +
        labs(title = paste("Mean Loan Waiting Time,", 
                           abbr2state(input$stateInput3), 
                           "vs. National Average"),
             y = "Mean Waiting Time (business days)") +
        theme_classic() +
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
    
    # These ones are produced by ggeffects, plotted 95% CI from "stata" SEs. 
    # They don't include fixed effects due to limitations of the lm_robust
    # function and the inability to use ggeffect with any other regression
    # functions that allow for fixed effects.
    
    output$marginalRace1 <- renderPlot({
      zip_race_graph %>%
        ggplot(aes(x = x, y = predicted)) +
          geom_line(color = "darkblue") +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
          labs(x = "Minority Proportion of Population (Percent)", 
               y = "Predicted Wait Time (Business Days)",
               title = "Linear Predicted Effect of Racial Minority Presence on Wait Times
               (Grouping by ZIP Codes)") +
          xlim(0, 100) +
          theme_classic()
    })
    
    output$marginalRace2 <- renderPlot({
      zip_racesq_graph %>%
        ggplot(aes(x = x, y = predicted)) +
        geom_line(color = "darkblue") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
        labs(x = "Minority Proportion of Population (Percent)", 
             y = "Predicted Wait Time (Business Days)",
             title = "Quadratic Predicted Effect of Racial Minority Presence on Wait Times
               (Grouping by ZIP Codes)") +
        xlim(0, 100) +
        theme_classic()
    })
    
    # Here's all the regression tables. -- no explanation really needed, 
    # I add in an explanation of the star notation in each one and tell
    # them to keep the empty cells empty.
    
    output$zip_simple <- render_gt({
      zip_simple %>%
        gt() %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05") %>%
        tab_header(title = "Including ZIP-level Estimates")
    })
    
    output$zip_simple_check <- render_gt({
      zip_simple_check %>%
        gt() %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05") %>%
        tab_header(title = "Using Original County Data (where necessary)")
    })
    
    output$marginalRace3 <- renderPlot({
      county_race_graph %>%
        ggplot(aes(x = x, y = predicted)) +
        geom_line(color = "darkblue") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
        labs(x = "Percent of Businesses Owned by Minorities", 
             y = "Predicted Wait Time (Business Days)",
             title = "Linear Predicted Effect of Racial Minority Businesses on Wait Times
               (Grouping by Counties)") +
        xlim(0, 75) +
        theme_classic()
    })
    
    # The marginal race graphs again are self explanatory -- I graph with a 
    # ribbon to display the 95% CI.
    
    output$marginalRace4 <- renderPlot({
      county_racesq_graph %>%
        ggplot(aes(x = x, y = predicted)) +
        geom_line(color = "darkblue") +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
        labs(x = "Percent of Businesses Owned by Minorities", 
             y = "Predicted Wait Time (Business Days)",
             title = "Quadratic Predicted Effect of Racial Minority Businesses on Wait Times
               (Grouping by Counties)") +
        xlim(0, 75) +
        theme_classic()
    })
    
    output$county_simple <- render_gt({
      county_simple %>%
        gt() %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05")
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
    
    # The banking graphs show up as bar graphs just because I've gotten
    # the ggeffect for two, rather than 1 variable.
    
    output$zip_banks_graph <- renderPlot({
      national_banks_zip %>%
        mutate(x = as.factor(x)) %>%
        ggplot(aes(x = x, y = predicted, fill = group)) + 
          geom_col(position = "dodge") +
          labs(x = "National Bank", y = "Predicted Wait Time (Days)",
               title = "ZIP Codes") +
          scale_fill_brewer(palette = "Pastel1", name = "Minority Percent",
                            labels = c("25th Percentile", "50th Percentile",
                                       "75th Percentile")) +
          scale_x_discrete(labels = c("False", "True")) +
          theme_classic()
    })
    
    output$zip_banks <- render_gt({
      zip_banks %>%
        gt() %>%
        tab_header(title = "ZIP Codes") %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05")
    })
    
    # More ggeffects graph.
    
    output$county_banks_graph <- renderPlot({
      national_banks_county %>%
        mutate(x = as.factor(x)) %>%
        ggplot(aes(x = x, y = predicted, fill = group)) + 
        geom_col(position = "dodge") +
        labs(x = "National Bank", y = "Predicted Wait Time (Days)",
             title = "Counties") +
        scale_fill_brewer(palette = "Pastel1", name = "Minority Percent",
                          labels = c("25th Percentile", "50th Percentile",
                                     "75th Percentile")) +
        scale_x_discrete(labels = c("False", "True")) +
        theme_classic()
    })
    
    output$county_banks <- render_gt({
      county_banks %>%
        gt() %>%
        tab_header(title = "Counties") %>%
        fmt_missing(columns = everything(), missing_text = "") %>%
        tab_source_note(source_note = "***p < 0.001; **p < 0.01; *p < 0.05")
    })
    
    # Regression coef definitions -- just gt'ing the tables I created.
    
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
