library(MISTEuv)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(dataRetrieval)
library(zoo)
library(lubridate)
library(gam)
library(mgcv)

options(DT.options = list(pageLength = 7, language = list(search = 'Filter:')))

source("R/getRegDat.R")

source("R/getEstDat.R")

source("R/getBestOffset.R")

source("R/allMISTEdat.R")

source("R/applySmooth.R")

source("R/MISTEuvGUI.R")