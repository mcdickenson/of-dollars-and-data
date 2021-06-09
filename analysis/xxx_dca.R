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

qqq <- read.csv(paste0(importdir, "HistoricalPricesQQQ.csv"), strip.white=TRUE) %>%
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

print(head(qqq))
print(tail(qqq))

qqq_df = as.data.frame(qqq)


# Three strategies
# LS: sell all immediately ($12,000)
# DCA: sell a variable number of shares each month to reach a target dollar amount ($1,000/month)
# Fixed: sell a fixed number of shares each month (1/12 of what $12,000 buys at the beginning of the period)
# Notes: uses QQQ data to target Nasdaq, because it's a tech index
qqq_df$return_ls = NA
qqq_df$return_dca = NA
qqq_df$return_fixed = NA

print(head(qqq_df))
print(tail(qqq_df))

n_months = 12
dollar_amount = 12000
dollar_amount_per_month = dollar_amount / n_months

# run investment
for(i in 1:nrow(qqq_df)){
  current_year = as.integer(qqq$year[i])
  current_month = qqq$month[i]
  current_price = qqq$close[i]
  # end_year = as.character(current_year + n_years)


  # Strategy 1: lump-sum sale
  qqq_df$return_ls = dollar_amount

  # Strategy 2: DCA
  n_shares_dca = dollar_amount / qqq$close[i]
  shares_remaining = n_shares_dca
  offset = 0
  qqq_df$return_dca[i] = 0
  while(shares_remaining >= 1.0 & (i + offset < nrow(qqq_df))) {
    # get a month's price
    price_this_month = qqq$close[i+offset]
    # figure out how many to sell
    shares_to_sell = round(1000.0 / price_this_month)
    shares_to_sell = min(shares_to_sell, shares_remaining)
    print(paste0("dca selling ", shares_to_sell, " at $", price_this_month))
    qqq_df$return_dca[i] = qqq_df$return_dca[i] + (shares_to_sell * price_this_month)
    shares_remaining = shares_remaining - shares_to_sell
    offset = offset + 1
  }

  # Strategy 3: Fixed number of shares
  n_shares_fixed = dollar_amount / qqq$close[i] # starting number
  shares_remaining = n_shares_fixed
  offset = 0
  qqq_df$return_fixed[i] = 0
  while(shares_remaining >= 1.0 & (i + offset < nrow(qqq_df))) {
    # get a month's price
    price_this_month = qqq$close[i+offset]
    # figure out how many to sell
    shares_to_sell = round(n_shares_fixed / n_months)
    shares_to_sell = min(shares_to_sell, shares_remaining)
    # sell them
    print(paste0("fixed selling ", shares_to_sell, " at $", price_this_month))
    qqq_df$return_fixed[i] = qqq_df$return_fixed[i] + (shares_to_sell * price_this_month)
    shares_remaining = shares_remaining - shares_to_sell
    offset = offset + 1
  }

}
print(head(qqq_df))
print(tail(qqq_df))



