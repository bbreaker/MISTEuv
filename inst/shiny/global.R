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

source("MISTEuv/R/getRegDat.R")

source("MISTEuv/R/getEstDat.R")

source("MISTEuv/R/getBestOffset.R")

source("MISTEuv/R/allMISTEdat.R")

source("MISTEuv/R/applySmooth.R")

source("MISTEuv/R/MISTEuvGUI.R")