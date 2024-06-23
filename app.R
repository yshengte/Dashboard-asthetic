library(shiny)
library(rsconnect)
library(shinymanager)
library(keyring)
library(readxl)


# source("recruitment_plots.R")
# source("coupon_summary.R")
# source("propensity_score.R")
source("recruitment_plots(updated).R")


# setwd("O:/PATH/Data/Dashboard/src")
##runApp() # Run app locally

# rsconnect::setAccountInfo(name='rdsdashboard',
#                           token='C73557407A6495A13180EA2C838567A4',
#                           secret='oU5fLBZGzgNb5vkRQCdHWmtA8dwt96IMrzqz5P/z')
# 
# rsconnect::deployApp(appName = "PATH",account = "rdsdashboard")
# 
# # Date of last deployment to web: 9/21/2023, by Zhaoyu
# 
# 
# 
# rsconnect::showLogs()
# 
# rsconnect::configureApp("PATH", size="large")