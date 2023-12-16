# install.packages(c("shiny","shinydashboard","shinyjs","tidyverse","rvest",
#                    "tidyquant","dygraphs","xts","DT","xml2","RCurl","lubridate"))

# Loading all the packages needed
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(rvest)

# Loading all the necessary source files
source("base/login/login.R")
source("base/home/home.R")
source("base/home/feeds.R")
# Plots
source("base/gemini/plots/geyser1.R")
source("base/gemini/plots/geyser2.R")
# DataTable
source("base/gemini/datatable/datatable1.R")
source("base/gemini/datatable/datatable2.R")
# TimeSeries
source("base/gemini/timeseries/timeseries1.R")
source("base/gemini/timeseries/timeseries2.R")
# Webscrape
source("base/gemini/webscrape/webscrape1.R")
source("base/gemini/webscrape/webscrape2.R")
# Apollo
source("base/apollo/yourtab.R")
# Source your files here.

# Sets the time zone
# Find your time zone here https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
# The function OlsonNames() also has a list of time zones
Sys.setenv(TZ="America/New_York")
