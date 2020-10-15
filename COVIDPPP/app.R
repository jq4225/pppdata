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
ppp <- readRDS('ppp_preliminary.rds')
cut_ppp <- readRDS('cut_ppp.rds')

# Define UI for application that draws a histogram

ui <<- navbarPage("Gov 50: Justin Qi Milestone",
                  tabPanel("Data",
                           plotOutput("distPlot")),
                  tabPanel("About",
                           mainPanel(
                               h2("About"),
                               tags$a(href="https://github.com/jq4225/pppdata", 
                                      "Github repo here!"),
                               p("I am using two main sources of data for my project.
                                  First, I'm using the American Community Survey's 2018 projections,
                                  which include racial, demographic, and income breakdowns for 
                                 every ZIP code in the US. I'm also using an FDIC list of all the 
                                 banking locations in the US broken down by ZIP code. Finally,
                                 I'm using the SBA's dataset on Paycheck Protection Loans of more 
                                 than $150,000 given this year, as of August 8th.")
                           ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        cut_ppp %>%
            drop_na(cuts) %>%
            ggplot(aes(x = cuts, y = mean_days)) + geom_col() +
              labs(x = "Proportion of Black/African American, Percent",
                   y = "Average Wait Time for PPP Loan (days)")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
