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

folder_name <- "rebalancing"
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
df = df[2:nrow(df), ] # mismatch at very beginning
names(df)[4] = 'spx'
names(df)[5] = 'shy'
print(head(df))
print(tail(df))

# Run rebalancing strategy
# 1. invest at the beginning of the period
# 2. rebalance annually for desired mix (80/20?)
# 3. compare that to never rebalancing
# what time horizon? 5 or 10 years?

portfolio_initial_value = 10000
percent_stocks = 0.8
percent_bonds = 1.0 - percent_stocks

# no rebalancing
dollars_stocks = portfolio_initial_value * percent_stocks
count_stocks = round(dollars_stocks / df$spx[1])
df$count_stocks_norebalance = count_stocks
dollars_bonds = portfolio_initial_value - (count_stocks * df$spx[1])
count_bonds = round(dollars_bonds / df$shy[1])
df$count_bonds_norebalance = count_bonds
df$value_norebalance = (df$spx * df$count_stocks_norebalance) + (df$shy * df$count_bonds_norebalance)
print(head(df))
print(tail(df))
# todo calculate percent value to see how thrown off things get

df$value_rebalance = 0
df$count_stocks_rebalance = 0
df$count_stocks_rebalance[1] = count_stocks
df$count_bonds_rebalance = 0
df$count_bonds_rebalance[1] = count_bonds

# run investment
for(i in 2:nrow(df)){
  if(i %% 6 == 0){
    # time for a rebalance
    current_stocks = df$count_stocks_rebalance[i-1]
    current_bonds = df$count_bonds_rebalance[i-1]
    current_value = (df$spx[i] * current_stocks) + (df$shy[i] * current_bonds)
    dollars_stocks = current_value * percent_stocks
    count_stocks = round(dollars_stocks / df$spx[i])
    df$count_stocks_rebalance[i] = count_stocks
    dollars_bonds = current_value - (count_stocks * df$spx[i])
    count_bonds = round(dollars_bonds / df$shy[i])
    df$count_bonds_rebalance[i] = count_bonds
  } else {
    df$count_stocks_rebalance[i] = df$count_stocks_rebalance[i-1]
    df$count_bonds_rebalance[i] = df$count_bonds_rebalance[i-1]
  }
}
df$value_rebalance = (df$spx * df$count_stocks_rebalance) + (df$shy * df$count_bonds_rebalance)
print(head(df))
print(tail(df))

print(summary(df))

# Plot the results
to_plot <- df %>%
            select(date, contains("value")) %>%
            gather(-date, key=key, value=value) %>%
            mutate(key = case_when(
              key == "value_norebalance" ~ "No Rebalancing",
              key == "value_rebalance" ~ "Rebalance Twice Per Year",
              TRUE ~ "Error"
            ))

file_path <- paste0(out_path, "/rebalancing_strategies.png")
source_string <- paste0("Source: Wall Street Journal historical prices.")
note_string <- str_wrap("Note: All calculations use the first closing price of the month.",width = 85)

text_labels <- data.frame()

text_labels[1, "date"] <- as.Date("2012-08-01", "%Y-%m-%d")
text_labels[1, "value"] <- 10000
text_labels[1, "label"] <- "No Rebalancing"

text_labels[2, "date"] <- as.Date("2005-08-01", "%Y-%m-%d")
text_labels[2, "value"] <- 10000
text_labels[2, "label"] <- "Rebalance Twice Per Year"

plot <- ggplot(to_plot, aes(x = date, y = value, col = key)) +
  geom_line() +
  geom_text(data=text_labels, aes(x=date, y=value, col = label, label = label)) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(guide = FALSE, values = c("red", "blue")) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Rebalancing")) +
  labs(x = "Year" , y = "Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")