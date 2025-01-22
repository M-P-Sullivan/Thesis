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
  group = rep(c("Affirmed", "Reversed", "Indeterminate"), each = length(years))
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

grouped_circuit_treat_data_long <- data.frame(
  year = rep(years, each = 3*12),
  circuit = rep(c(0:11), length(years)*3),
  group = rep(c("Affirmed", "Reversed", "Indeterminate"), each = 12, times = length(years)),
  treat_percent = rep(0, 3*12*length(years))
)

data_list <- list(
  "Affirmed" = affirmed,
  "Reversed" = reversed,
  "Indeterminate" = indeterminate
)

# Function to retrieve the vector dynamically
get_vector <- function(key) {
  if (key %in% names(data_list)) {
    return(data_list[[key]])
  } else {
    stop("Key not found in the list.")
  }
}

for (i in seq_len(nrow(grouped_circuit_treat_data_long))) {
  circuit_year_total = nrow(data[data$year == grouped_circuit_treat_data_long$year[i]
                                 & data$circuit == grouped_circuit_treat_data_long$circuit[i], ])
  circuit_year_group = nrow(data[data$treat %in% get_vector(grouped_circuit_treat_data_long$group[i])
                                 & data$year == grouped_circuit_treat_data_long$year[i]
                                 & data$circuit == grouped_circuit_treat_data_long$circuit[i], ])
  grouped_circuit_treat_data_long$treat_percent[i] <- circuit_year_group / circuit_year_total
}

for (i in 0:11) {
  filtered_data <- grouped_circuit_treat_data_long %>%
    filter(circuit == i)
  
  p <- ggplot(filtered_data, aes(x = year, y = treat_percent, color = group, shape = factor(circuit))) +
    geom_point(size = 2) +                     # Points for both sets
    ggtitle("Proportion of cases by result") +
    xlab("Year") +
    ylab("Values") +
    scale_color_manual(values = c("blue", "red", "green")) +  # Set custom colors
    theme_minimal() +
    guides(color = guide_legend(order = 1),  # Ensure color legend comes first
         shape = guide_legend(order = 2))
  
  print(p)
}

