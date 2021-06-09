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

spx_df$return_ls = NA
spx_df$return_dca = NA

print(head(spx_df))
print(tail(spx_df))

n_years = 5

# run investment
for(i in 1:nrow(spx_df)){
  current_year = as.integer(spx$year[i])
  current_month = spx$month[i]
  end_year = as.character(current_year + n_years)

  # Calculate lump sum investment
  end_row = which(spx$year == end_year & spx$month == current_month)
  # if end_row is null, continue / break
  if(length(end_row) == 0){
    next
  }
  end_val = spx$close[end_row[1]]
  print(paste0("end val: ", end_val))

  return_perc = (as.double(end_val) / as.double(spx$close[i]))
  print(paste0("return: ", return_perc))

  # TODO might make sense to store this on end_val instead?
  spx_df$return_ls[i] = 12000 * return_perc


  # Calculate DCA investment starting today
  # TODO
}
print(head(spx_df))
print(tail(spx_df))

# lump sum is the simple % of 120 months later
# DCA is the weighted return of investing $1k/month for 12 consecutive months,
# and what you get 120 months after the beginning of the period


