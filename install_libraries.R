cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/github/of-dollars-and-data")
# source(file.path(paste0(getwd(),"/header.R")))

########################## Install All in Libraries ########################## #

pkgs <- c("tidyverse", "stringr", "lubridate",
          "ggjoy", "ggrepel", "scales", "grid", "gridExtra",
          "RColorBrewer", "quadprog", "fTrading", "quantmod",
          "gtable", "reshape2", "MASS", "gdata", "readxl",
          "Quandl", "stats", "viridis", "jpeg", "httr",
          "BenfordTests", "FinCal", "gganimate", "tidylog",
          "MLmetrics", "caret", "PerformanceAnalytics", "Hmisc",
          "survey", "mitools", "lemon")

install.packages(pkgs, repos = "http://cran.us.r-project.org")

# ############################  End  ################################## #