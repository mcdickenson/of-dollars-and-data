cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/github/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)


folder_name <- "options"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #


df <- data.frame(year = seq(0, 1, 1))
print(df)

starting_amount = 1.0

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "high"] <- starting_amount
    df[i, "mid"] <- starting_amount
    df[i, "low"] <- starting_amount
  } else{
    df[i, "high"] <- df[(i-1), "high"] * 2.0
    df[i, "mid"] <- df[(i-1), "mid"]
    df[i, "low"] <- df[(i-1), "low"] * 0.25
  }
}

print(df)

to_plot <- df %>%
            select(year, "high", "mid", "low") %>%
            gather(-year, key=key, value=value) %>%
            mutate(key = case_when(
              key == "high" ~ "In the Money",
              key == "mid" ~ "Strike Price",
              key == "low" ~ "Underwater",
              TRUE ~ "Error"
            ))
print("to plot:")
print(to_plot)

file_path <- paste0(out_path, "/options.png")
note_string <- "Note here"

text_labels <- data.frame()

text_labels[1, "year"] <- 0.5
text_labels[1, "value"] <- 1.75
text_labels[1, "label"] <- "In the Money"

text_labels[2, "year"] <- 0.5
text_labels[2, "value"] <- 1.1
text_labels[2, "label"] <- "Strike Price"

text_labels[3, "year"] <- 0.5
text_labels[3, "value"] <- .5
text_labels[3, "label"] <- "Underwater"

ids = c("In the Money", "Strike Price", "Underwater")
fills = c("red", "pink", "orange")

fills_df = data.frame(
  id = ids,
  fills = fills
)

print("coords:")
coords_df = data.frame(
  id = rep(ids, each=3),
  x = c(0, 1, 1, 0, 1, 1, 0, 1, 1),
  y = c(1, 2, 1, 0, 0, 0, 1, 0.25, 1)
)

print(coords_df)

polys = merge(fills_df, coords_df, by=c("id"))

plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
  geom_polygon(data=polys, show.legend=FALSE, aes(x=x, y=y, fill=fills, col=NULL)) +
  geom_text(data=text_labels, aes(x=year, y=value, col = label, label = label)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(guide = FALSE, values = c("blue", "black", "red")) +
  of_dollars_and_data_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle(paste0("Intrinsic Value of Stock Options")) +
  labs(x = "Time" , y = paste0("Stock Price"),
       caption = NULL)

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

