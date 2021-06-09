cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/github/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(zoo)
library(lemon)
library(tidyverse)
library(dplyr)

folder_name <- "dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

print(importdir)

# S&P 500 Index
spx <- read.csv(paste0(importdir, "HistoricalPricesSPX.csv"), strip.white=TRUE) %>%
  rename(date_raw=Date,
         close=Close) %>%
  mutate(
    mm = sapply(strsplit(date_raw, "/"), function(x) x[[1]][1]),
    yy = sapply(strsplit(date_raw, "/"), tail, 1),
    yymm = paste0(yy, "-", mm),
    date = as.Date(date_raw, format = "%m/%d/%y"),
    year = format(as.Date(date), "%Y"),
    month = format(as.Date(date), "%m")) %>%
  group_by(yymm) %>%
  filter(date == min(date)) %>%
  slice(1) %>% # filter down to the first trading day of each month
  ungroup() %>%
  select(date, year, month, close) %>%
  arrange(date)

print(head(spx))
print(tail(spx))

spx_df = as.data.frame(spx)

# Bond ETF
shy <- read.csv(paste0(importdir, "HistoricalPricesSHY.csv"), strip.white=TRUE) %>%
  rename(date_raw=Date,
         close=Close) %>%
  mutate(
    mm = sapply(strsplit(date_raw, "/"), function(x) x[[1]][1]),
    yy = sapply(strsplit(date_raw, "/"), tail, 1),
    yymm = paste0(yy, "-", mm),
    date = as.Date(date_raw, format = "%m/%d/%y"),
    year = format(as.Date(date), "%Y"),
    month = format(as.Date(date), "%m")) %>%
  group_by(yymm) %>%
  filter(date == min(date)) %>%
  slice(1) %>% # filter down to the first trading day of each month
  ungroup() %>%
  select(date, year, month, close) %>%
  arrange(date)

print(head(shy))
print(tail(shy))

shy_df = as.data.frame(shy)

# Merge
df <- merge(spx_df, shy_df, by=c('date', 'year', 'month'), all.x=FALSE, all.y=TRUE)
print(head(df))
print(tail(df))

names(df)[4] = 'spx'
names(df)[5] = 'shy'
