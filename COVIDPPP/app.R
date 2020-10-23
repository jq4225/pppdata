#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)

ppp <- readRDS('ppp_allvars_1020_lockdowns.rds')

# Define UI for application that draws a histogram

ui <<- navbarPage("What Determines Paycheck Protection Program Waiting Times?",
                  theme = shinytheme("lumen"),
                  tabPanel("Descriptives",
                           plotOutput("distPlot")),
                  tabPanel("Regressions",
                           tableOutput('table')),
                  tabPanel("About",
                           mainPanel(
                               h2("About"),
                               tags$a(href="https://github.com/jq4225/pppdata", 
                                      "Github repo here!"),
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
                                 data on COVID policy responses comes from the Kaiser Family Foundation.")
                           ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        ppp %>%
            ggplot(aes(x = black_percent, y = days_to_approval)) + geom_jitter()
    })
    
    output$table <- renderTable(ppp)
}

# Run the application 
shinyApp(ui = ui, server = server)
