# PPP Disparity Estimation

This  is really messy, but you can run the files in the order of the numbers - i.e. 1, 2, 3, 4, 5. When there are multiple files of the same number, this indicates multiple ways the code in that file was implemented -- open each file to read the comments within. Specifically, for file 5, 5_correctedcovid.rmd implements a new way to calculate COVID caes and deaths, while the nobanks.rmd file is for recreating the original dataset without the observations dropped by including bank lending policies.  

Note that none of the .rds files are in the GitHub due to  size limitations, including the ones needed for the Shiny App (though, of course, the published Shiny App uses the files). 

The raw_data folder is not backed up to GitHub because of size limitations, but contains all of the raw data used. In general, however, data sources are as follows:

All demographic data (e.g. race, median income, high school graduation, marital status, Gini index): 2018 American Community Survey 5-year estimates

Business data: ZIP code is from 2014 ZBP Survey by the Census Bureau, County level is from County Business Patterns survey in 2016

Bank locations: FDIC survey on bank branch locations, updated as of October 1.

Loan-level data: SBA PPP website

COVID data: New York Times

Unemployment data: Bureau of Labor Statistics

State policy dummies: Kaiser Family Foundation state policy tracker

Violent crime rates: County Health Rankings, data aggregated from the FBI

Political leanings: ZIP code level from the Cook Partisan Voting Index, county level from the MIT Election Data Science Lab

Rurality: ZIP code level from the Health Resources and Services Administration, population density on county level from the 2010 Census, cleaned by ykzeng (https://github.com/ykzeng/covid-19/tree/master/data)

Crosswalk tables were from the University of Missouri's Census Data Center and the Department of Housing and Urban Development.

You can find a full version of the folder & raw data here: https://drive.google.com/drive/folders/1dkYQuBmXw8ABQvfe1N6oZR3m6xAomRWv?usp=sharing

Many of the RDS files referenced, especially in the files running regressions, are only available in the GDrive folder and not in Github.
