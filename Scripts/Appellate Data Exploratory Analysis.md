---
title: "Appellate EDA"
author: "Michael Sullivan"
date: "2025-01-11"
output:
  html_document: default
  pdf_document: default
---
```{r}
library(here)
library(ggplot2)
library(dplyr)

data_path <- here("cta96.csv")
data <- read.csv(data_path)

#Exploring the data
print(colnames(data))

duplicate_casenums <- data$casenum[duplicated(data$casenum)]
print(duplicate_casenums)

years <- sort(unique(data$year))
num_years <- length(years)
year_counts <- rep(0,num_years)
for (year in years) {
  print(paste(year, sum(data$year == year), sep = ":"))
}

months <- c(1:12)
for (month in months) {
  print(paste(month, sum(data$month == month), sep = ":"))
}

#Coding the various outcomes for the decision of the appellate court
affirmed <- c(1,8)
reversed <- c(2,3,4,7)
indeterminate <- c(0,5,6,9,10)

year_percent_affirmed <- rep(0,num_years)
year_percent_reversed <- rep(0,num_years)
year_percent_indeterminate <- rep(0,num_years)
for (i in seq_along(years)) {
  year_percent_affirmed[i] <- nrow(data[data$year == years[i] & data$treat %in% affirmed, ]) / sum(data$year == years[i])
  year_percent_reversed[i] <- nrow(data[data$year == years[i] & data$treat %in% reversed, ]) / sum(data$year == years[i])
  year_percent_indeterminate[i] <- nrow(data[data$year == years[i] & data$treat %in% indeterminate, ]) / sum(data$year == years[i])
}


grouped_treat_data_long <- data.frame(
  years = rep(years, 3),
  values = c(year_percent_affirmed, year_percent_reversed, year_percent_indeterminate),
  group = rep(c("Affirm", "Reverse", "Indeterminate"), each = length(years))
)

ggplot(grouped_treat_data_long, aes(x = years, y = values, color = group)) +
  geom_point(size = 2) +                     # Points for both sets
  ggtitle("Proportion of cases by result") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "green", "red")) +  # Set custom colors
  theme_minimal()

individual_treat_data <- data.frame(
  years = years,
  matrix(ncol = 11, nrow = length(years))
)
colnames(individual_treat_data)[2:12] <- 0:10

for (i in 0:10) {
  for (j in seq_along(individual_treat_data$years)) {
    year <- individual_treat_data$years[j]
    individual_treat_data[j,as.character(i)] <- nrow(data[data$year == year & data$treat == i, ]) / sum(data$year == year)
  }
}

individual_treat_data_long <- data.frame(
  years = rep(years, 11),
  values = unlist(individual_treat_data[,2:12]),
  group = rep(c(0:10), each = length(years))
)

filtered_data <- individual_treat_data_long %>%
  filter(group == 1 | group == 8)

ggplot(filtered_data, aes(x = years, y = values, color = factor(group))) +
  geom_point(size = 2) +                     # Points for both sets
  ggtitle("Proportion of cases by result") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "red")) +  # Set custom colors
  theme_minimal()

filtered_data <- individual_treat_data_long %>%
  filter(group == 2 | group == 3|group == 4|group == 7)

ggplot(filtered_data, aes(x = years, y = values, color = factor(group))) +
  geom_point(size = 2) +                     # Points for both sets
  ggtitle("Proportion of cases by result") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +  # Set custom colors
  theme_minimal()

filtered_data <- individual_treat_data_long %>%
  filter(group == 5 | group == 6|group == 9|group == 10)

ggplot(filtered_data, aes(x = years, y = values, color = factor(group))) +
  geom_point(size = 2) +                     # Points for both sets
  ggtitle("Proportion of cases by result") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +  # Set custom colors
  theme_minimal()