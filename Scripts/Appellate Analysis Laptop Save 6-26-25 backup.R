### Packages----------

# install.packages("ranger")
# install.packages("haven")
# install.packages("here")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("ranger")
# install.packages("na.tools")

library(ranger)
library(haven)
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(na.tools)

### Initial data ingestion and exploration----------

data_path <- here("cta96.csv")
data <- read.csv(data_path)

print(colnames(data))

duplicate_casenums <- data$casenum[duplicated(data$casenum)]
print(duplicate_casenums)

years <- sort(unique(data$year))
num_years <- length(years)
year_counts <- rep(0,num_years)
for (year in years) {
  print(paste(year, sum(data$year == year), sep = ":"))
}

### Exploring overall decision results--------

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
  geom_point(size = 2) +                     
  ggtitle("Proportion of cases by result") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "green", "red")) +  
  theme_minimal()

### Exploring decision results more granularly-------

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
  filter(group == 5 | group == 6|group == 9|group == 10 | group == 0)

ggplot(filtered_data, aes(x = years, y = values, color = factor(group))) +
  geom_point(size = 2) +                     # Points for both sets
  ggtitle("Proportion of cases by result") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "red", "green", "purple", "orange")) +  # Set custom colors
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
    geom_smooth(method = "loess", se = FALSE) +
    xlab("Year") +
    ylab("Values") +
    scale_color_manual(values = c("blue", "green", "red")) +  # Set custom colors
    theme_minimal() +
    guides(color = guide_legend(order = 1),  # Ensure color legend comes first
         shape = guide_legend(order = 2))
  
  print(p)
}

### Producing weighted analysis of decision patterns--------

weights <- read.csv(here("data_weights.csv"))
new_row <- data.frame(Year=76,Circuit=9,Circuit_Cases=(655+596)/2)
weights <- rbind(weights,new_row)
weights$year <- weights$Year+1900
weights$circuit <- weights$Circuit
weights <- weights %>%
  group_by(year) %>%
  mutate(year_circuit_cases = sum(Circuit_Cases)) %>%
  ungroup()
weights$sample_cases <- apply(weights, 1, function(row) {
  nrow(filter(data, year == row["year"], circuit == row["Circuit"]))
  })
weights$year_sample_cases <- apply(weights, 1, function(row) {
  nrow(filter(data, year == row["year"]))
  })
weights$circuit_proportion <- weights$Circuit_Cases/weights$year_circuit_cases
weights$sample_proportion <- weights$sample_cases/weights$year_sample_cases
weights$weighting <- weights$circuit_proportion/weights$sample_proportion

data <- read.csv(data_path)
data <- left_join(data, weights, by=c("year", "circuit"))

weighted_year_percent_affirmed <- rep(0,num_years)
weighted_year_percent_reversed <- rep(0,num_years)
weighted_year_percent_indeterminate <- rep(0,num_years)

data[data$year == 1926 & data$treat %in% affirmed, ]$weighting
sum(data[data$year == 1926 & data$treat %in% affirmed, ]$weighting) / sum(data[data$year == 1926,]$weighting)


for (i in seq_along(years)) {
  weighted_year_percent_affirmed[i] <- sum(data[data$year == years[i] & data$treat %in% affirmed, ]$weighting) / sum(data[data$year == years[i],]$weighting)
  weighted_year_percent_reversed[i] <- sum(data[data$year == years[i] & data$treat %in% reversed, ]$weighting) / sum(data[data$year == years[i],]$weighting)
  weighted_year_percent_indeterminate[i] <- sum(data[data$year == years[i] & data$treat %in% indeterminate, ]$weighting) / sum(data[data$year == years[i],]$weighting)
  }


grouped_treat_data_long_weighted <- data.frame(
  years = rep(years, 3),
  values = c(weighted_year_percent_affirmed, weighted_year_percent_reversed, weighted_year_percent_indeterminate),
  group = rep(c("Affirmed", "Reversed", "Indeterminate"), each = length(years))
)

ggplot(grouped_treat_data_long_weighted, aes(x = years, y = values, color = group)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Proportion of cases by result - Weighted") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme_minimal()

### Analyzing slices of the data to understand the discontinuity------

six_cir_slice <- data[data$Circuit==6,]
write.csv(six_cir_slice, "Six_circuit_slice.csv", row.names=FALSE)

table(six_cir_slice$origin, six_cir_slice$year)
table(six_cir_slice$source, six_cir_slice$year)
table(six_cir_slice$casetyp1, six_cir_slice$year)
table(six_cir_slice$geniss, six_cir_slice$year)

#Plots the frequency or proportion of one of the variables by year
plot_categorical_by_year <- function(df, varname, year_var = "year", proportion = FALSE, title_prefix = "") {
  var_sym <- sym(varname)
  year_sym <- sym(year_var)
  
  freq_data <- df %>%
    count(!!var_sym, !!year_sym, name = "count") %>%
    mutate(
      !!year_var := as.numeric(as.character(!!year_sym)),
      !!var_sym := as.factor(!!var_sym)
    )
  
  if (proportion) {
    freq_data <- freq_data %>%
      group_by(!!year_sym) %>%
      mutate(count = count / sum(count)) %>%
      ungroup()
    y_label <- "Proportion"
    title_type <- "Proportions"
  } else {
    y_label <- "Frequency"
    title_type <- "Frequencies"
  }
  
  ggplot(freq_data, aes(x = !!year_sym, y = count, color = !!var_sym, group = !!var_sym)) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette = "Set1", name = varname) +
    ggtitle(paste0(title_prefix, varname, " ", title_type, " by Year")) +
    xlab("Year") +
    ylab(y_label) +
    theme_minimal()
}

### Plotting wide array of variables to see which if any spike or dip around 1970
plot_categorical_by_year(data[data$Circuit==3,], "geniss", proportion = FALSE)
plot_categorical_by_year(data[data$Circuit==3,], "geniss", proportion = TRUE)
plot_categorical_by_year(data[data$Circuit==3,], "source")
plot_categorical_by_year(data[data$Circuit==3,], "casetyp1")
plot_categorical_by_year(data[data$Circuit==3,], "origin")

### Some trends to look at
plot_categorical_by_year(data[data$treat==1,], "geniss", proportion = TRUE)      #Some promise here
plot_categorical_by_year(data[data$treat==1,], "geniss", proportion = FALSE)      #Some promise here
plot_categorical_by_year(data[data$treat==1,], "source", proportion = TRUE)      #Some promise here
plot_categorical_by_year(data[data$treat==1,], "source", proportion = FALSE)      #Some promise here
plot_categorical_by_year(data, "typeiss", proportion = TRUE)
plot_categorical_by_year(data, "typeiss", proportion = FALSE)


plot_categorical_by_year(data[data$treat==1,], "casetyp1")
plot_categorical_by_year(data[data$treat==1,], "origin")

plot_categorical_by_year(data, "geniss")
plot_categorical_by_year(data, "source")
plot_categorical_by_year(data, "casetyp1")
plot_categorical_by_year(data, "origin")

plot_categorical_by_year(data, "applfrom")

#No clear trends below
plot_categorical_by_year(data, "juris")
plot_categorical_by_year(data, "statecl")
plot_categorical_by_year(data, "standing")
plot_categorical_by_year(data, "mootness")
plot_categorical_by_year(data, "exhaust")
plot_categorical_by_year(data, "timely")
plot_categorical_by_year(data, "immunity")
plot_categorical_by_year(data, "frivol")
plot_categorical_by_year(data, "polquest")
plot_categorical_by_year(data, "oththres")
plot_categorical_by_year(data, "frivapp")
plot_categorical_by_year(data, "othappth")

### Using a function to plot the affirm/reverse rate for subsets of the data

#Plotting decision rates by category
plot_treatment_trends <- function(df, filter_expr = NULL, title_suffix = "", loess = TRUE) {
  affirmed <- c(1, 8)
  reversed <- c(2, 3, 4, 7)
  indeterminate <- c(0, 5, 6, 9, 10)
  
  if (!is.null(filter_expr)) {
    df <- df %>% filter(!!rlang::parse_expr(filter_expr))
  }
  
  years <- sort(unique(df$year))
  num_years <- length(years)
  
  year_percent_affirmed <- sapply(years, function(y) {
    sum(df$year == y & df$treat %in% affirmed) / sum(df$year == y)
  })
  year_percent_reversed <- sapply(years, function(y) {
    sum(df$year == y & df$treat %in% reversed) / sum(df$year == y)
  })
  year_percent_indeterminate <- sapply(years, function(y) {
    sum(df$year == y & df$treat %in% indeterminate) / sum(df$year == y)
  })
  
  grouped_df <- data.frame(
    years = rep(years, 3),
    values = c(year_percent_affirmed, year_percent_reversed, year_percent_indeterminate),
    group = rep(c("Affirmed", "Reversed", "Indeterminate"), each = length(years))
  )
  
  p <- ggplot(grouped_df, aes(x = years, y = values, color = group)) +
    geom_point(size = 2) +
    ggtitle(paste("Proportion of cases by result", title_suffix)) +
    xlab("Year") +
    ylab("Values") +
    scale_color_manual(values = c("blue", "green", "red")) +
    theme_minimal()
  
  if (loess) {
    p <- p + geom_smooth(method = "loess", se = FALSE)
  }
  
  return(p)
}

plot_treatment_trends(data, 'circuit == 6', title_suffix = "- 6th Circuit")

plot_treatment_trends(data, 'typeiss == 1', title_suffix = "- Criminal and Prisoner Petitions")
plot_treatment_trends(data, 'typeiss == 2', title_suffix = "- Civil - Government")
plot_treatment_trends(data, 'typeiss == 4', title_suffix = "- Civil - Private")

plot_treatment_trends(data, 'geniss == 1', title_suffix = "- Criminal")
plot_treatment_trends(data, 'geniss == 2', title_suffix = "- Civil Rights")
plot_treatment_trends(data, 'geniss == 7', title_suffix = "- Economic activity and regulation")

plot_treatment_trends(data, 'source == 1', title_suffix = "- Source - Federal District Court")
plot_treatment_trends(data, 'source == 6', title_suffix = "- Source - Federal Administrative Agency")

plot_treatment_trends(data, 'applfrom == 1', title_suffix = "- Trial")
plot_treatment_trends(data, 'applfrom == 2', title_suffix = "- Injunction")
plot_treatment_trends(data, 'applfrom == 3', title_suffix = "- Summary Judgment")
plot_treatment_trends(data, 'applfrom == 5', title_suffix = "- Dismissal")

### Count version
plot_multiple_counts_vs_total <- function(df, filter_exprs, var_labels = NULL) {
  df$year <- as.numeric(df$year)
  
  if (is.null(var_labels)) {
    var_labels <- filter_exprs
  }
  
  subset_list <- lapply(seq_along(filter_exprs), function(i) {
    expr <- filter_exprs[i]
    label <- var_labels[i]
    
    df %>%
      filter(!!rlang::parse_expr(expr)) %>%
      count(year, name = "count") %>%
      mutate(type = label)
  })
  
  subset_data <- bind_rows(subset_list)
  
  total_data <- df %>%
    count(year, name = "count") %>%
    mutate(type = "Total")
  
  combined <- bind_rows(subset_data, total_data)
  
  ggplot(combined, aes(x = year, y = count, color = type)) +
    geom_line() +
    geom_point() +
    ggtitle("Yearly Counts: Subsets vs Total") +
    xlab("Year") +
    ylab("Number of Cases") +
    theme_minimal()
}

plot_multiple_counts_vs_total(
  data,
  filter_exprs = c("geniss == 1", "geniss == 2", "geniss == 7"),
  var_labels = c("geniss 1", "geniss 2", "geniss 7")
)

plot_multiple_counts_vs_total(
  data,
  filter_exprs = c("treat == 1", "treat == 2", "treat == 3"),
  var_labels = c("treat 1", "treat 2", "treat 3")
)

plot_multiple_counts_vs_total(
  data,
  filter_exprs = c("source == 1", "source == 6"),
  var_labels = c("District Court", "FAA")
)

plot_multiple_counts_vs_total(
  data,
  filter_exprs = c("casetyp1 == 105", "casetyp1 == 107"),
  var_labels = c("Robbery", "Auto Theft")
)


### Proportion Version
plot_multiple_proportions <- function(df, filter_exprs, var_labels = NULL) {
  df$year <- as.numeric(df$year)
  
  if (is.null(var_labels)) {
    var_labels <- filter_exprs
  }
  
  total_counts <- df %>%
    count(year, name = "total")
  
  proportion_list <- lapply(seq_along(filter_exprs), function(i) {
    expr <- filter_exprs[i]
    label <- var_labels[i]
    
    subset_counts <- df %>%
      filter(!!rlang::parse_expr(expr)) %>%
      count(year, name = "subset")
    
    merged <- left_join(subset_counts, total_counts, by = "year") %>%
      mutate(proportion = subset / total, type = label)
    
    merged %>%
      select(year, proportion, type)
  })
  
  proportion_data <- bind_rows(proportion_list)
  
  ggplot(proportion_data, aes(x = year, y = proportion, color = type)) +
    geom_line() +
    geom_point() +
    ggtitle("Proportion of Cases per Year by Filter") +
    xlab("Year") +
    ylab("Proportion") +
    theme_minimal()
}

plot_multiple_proportions(
  data,
  filter_exprs = c("treat == 1", "treat == 2", "treat == 3"),
  var_labels = c("treat 1", "treat 2", "treat 3")
)

### Ranger ----------------------------

#?ranger

# Map typeiss codes to labels
typeiss_labels <- c(
  `1` = "Criminal and Prisoner Petitions",
  `2` = "Civil - Government",
  `4` = "Civil - Private"
)

# Filter and label data
data_typeiss <- data %>%
  filter(typeiss %in% c(1, 2, 4)) %>%
  mutate(typeiss_label = recode(as.character(typeiss), !!!typeiss_labels))

# Get total cases per year
total_per_year <- data %>%
  count(year, name = "total")

# Get counts per typeiss per year
typeiss_per_year <- data_typeiss %>%
  count(year, typeiss_label, name = "count")

# Merge and compute proportions
typeiss_proportions <- left_join(typeiss_per_year, total_per_year, by = "year") %>%
  mutate(proportion = count / total)

# Plot
ggplot(typeiss_proportions, aes(x = year, y = proportion, color = typeiss_label)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Case Types by Year",
    x = "Year",
    y = "Proportion",
    color = "Case Type"
  ) +
  theme_minimal()

###
# Define labels
applfrom_labels <- c(
  `1` = "Trial",
  `2` = "Injunction",
  `3` = "Summary Judgment",
  `5` = "Dismissal"
)

# Categorize and label
data_applfrom <- data %>%
  mutate(
    decade = floor(year / 10) * 10,
    applfrom_group = case_when(
      applfrom %in% c(1, 2, 3, 5) ~ as.character(applfrom),
      TRUE ~ "Other"
    ),
    applfrom_label = recode(applfrom_group, !!!applfrom_labels, Other = "Other")
  )

# Count and calculate proportions
applfrom_summary <- data_applfrom %>%
  group_by(decade, applfrom_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(decade) %>%
  mutate(proportion = count / sum(count))

# Plot
ggplot(applfrom_summary, aes(x = factor(decade), y = proportion, fill = applfrom_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Appeals by Type and Decade",
    x = "Decade",
    y = "Proportion",
    fill = "Appeal From"
  ) +
  theme_minimal()

###
# Filter only "Other" applfrom values
other_applfrom_data <- data %>%
  filter(!(applfrom %in% c(1, 2, 3, 5))) %>%
  mutate(
    decade = floor(year / 10) * 10,
    applfrom_label = paste0("applfrom ", applfrom)
  )

# Count and calculate proportions
other_applfrom_summary <- other_applfrom_data %>%
  group_by(decade, applfrom_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(decade) %>%
  mutate(proportion = count / sum(count))

# Plot
ggplot(other_applfrom_summary, aes(x = factor(decade), y = proportion, fill = applfrom_label)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of 'Other' Appeals by applfrom Code and Decade",
    x = "Decade",
    y = "Proportion",
    fill = "applfrom Value"
  ) +
  theme_minimal()

###
# Group values of 'initiate' variable
data_initiate <- data %>%
  mutate(
    initiate_group = case_when(
      initiate == 1 ~ "Plaintiff",
      initiate == 2 ~ "Defendant",
      TRUE ~ "Other"
    )
  )

# Count and compute proportions per year
initiate_summary <- data_initiate %>%
  group_by(year, initiate_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(proportion = count / sum(count))

# Plot
ggplot(initiate_summary, aes(x = year, y = proportion, color = initiate_group)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Cases by Initiating Party",
    x = "Year",
    y = "Proportion",
    color = "Initiated By"
  ) +
  theme_minimal()

plot_treatment_trends(data, 'initiate == 1', title_suffix = "- Initiated by Plaintiff", loess = TRUE)
plot_treatment_trends(data, 'initiate == 2', title_suffix = "- Initiated by Defendant", loess = TRUE)

sum(data$distjudg == 99999)/length(data$distjudg)

data$distjudg[0:5]

# Calculate percentage by year
distjudg_summary <- data %>%
  group_by(year) %>%
  summarise(
    total = n(),
    missing_99999 = sum(distjudg == 99999),
    proportion = missing_99999 / total
  )

# Plot
ggplot(distjudg_summary, aes(x = year, y = proportion)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Cases with distjudg == 99999 by Year",
    x = "Year",
    y = "Proportion"
  ) +
  theme_minimal()

### Reading in judge databases

dist_judges <- read_dta("auburn_district_w_songer_codes.dta")
test_judges <- read_dta("auburn_district_w_songer_codes.dta")

judge_case_counts <- data %>%
  filter(distjudg != 99999) %>%
  count(distjudg, name = "n_cases")

# Count how many judges heard n cases
case_distribution <- judge_case_counts %>%
  count(n_cases, name = "num_judges")

# Multiply to get number of cases in data heard by judges with n cases
case_distribution <- case_distribution %>%
  mutate(num_cases = n_cases * num_judges)

# Plot
ggplot(case_distribution, aes(x = n_cases, y = num_cases)) +
  geom_point() +
  scale_y_continuous() +
  scale_x_continuous() +
  labs(
    title = "Total Cases by Judges with n Cases (Excluding distjudg == 99999)",
    x = "Number of Cases Heard by Judge (n)",
    y = "Total Cases Heard by Judges with n Cases"
  ) +
  theme_minimal()

# Create a vector of known district judge codes from dist_judges
known_judges <- unique(dist_judges$songer_code)

# Add indicator to data
data <- data %>%
  mutate(known_judge = distjudg %in% known_judges)

# Overall proportion
overall_prop <- mean(data$known_judge, na.rm = TRUE)
print(paste("Overall proportion of cases with known district judges:", round(overall_prop, 4)))

# Proportion over time
judge_coverage_by_year <- data %>%
  group_by(year) %>%
  summarise(prop_known = mean(known_judge, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(judge_coverage_by_year, aes(x = year, y = prop_known)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Cases Heard by Judges in dist_judges Dataset",
    x = "Year",
    y = "Proportion"
  ) +
  theme_minimal()

# Get vector of known judges
known_judges <- unique(dist_judges$songer_code)

# Filter to valid (non-99999) judge IDs
valid_judges <- data %>%
  filter(distjudg != 99999) %>%
  distinct(distjudg)

# Compute proportion that are in the other dataset
matched_prop <- mean(valid_judges$distjudg %in% known_judges)

print(paste("Proportion of valid judge IDs found in dist_judges:", round(matched_prop, 4)))

# Define decision categories
affirmed <- c(1, 8)
reversed <- c(2, 3, 4, 7)
indeterminate <- c(0, 5, 6, 9, 10)

# Filter data to valid judge IDs
valid_data <- data %>% filter(distjudg != 99999)

# Summarize case counts by judge
judge_case_summary <- valid_data %>%
  group_by(distjudg) %>%
  summarise(
    total_cases = n(),
    affirmed_cases = sum(treat %in% affirmed),
    reversed_cases = sum(treat %in% reversed),
    indeterminate_cases = sum(treat %in% indeterminate),
    affirm_rate = affirmed_cases / total_cases,
    .groups = "drop"
  )

# Merge into dist_judges using songer_code
dist_judges <- dist_judges %>%
  left_join(judge_case_summary, by = c("songer_code" = "distjudg"))


############ Fixing stata dates

test_judges$monl <- test_judges$monl + 3653
test_judges$yearl <- test_judges$yearl + 3653

dist_judges <- dist_judges %>%
  mutate(
    monl = as.numeric(monl) + 3653,
    yearl = as.numeric(yearl) + 3653,
    amon = as.numeric(amon) + 3653,
    ayear = as.numeric(ayear) + 3653
  )

dist_judges <- dist_judges %>%
  mutate(
    valid_dates = monl != 99 & yearl != 9999 & amon != 99 & ayear != 9999,
    tenure_years = ifelse(
      valid_dates,
      (yearl + (monl - 1) / 12) - (ayear + (amon - 1) / 12),
      NA_real_
    ),
    tenure_floor = floor(tenure_years)
  )

# Compute average affirm rate by floored tenure
avg_affirm_by_tenure <- dist_judges %>%
  filter(!is.na(affirm_rate), !is.na(tenure_floor)) %>%
  group_by(tenure_floor) %>%
  summarise(avg_affirm = mean(affirm_rate), .groups = "drop")

# Plot
ggplot(avg_affirm_by_tenure, aes(x = tenure_floor, y = avg_affirm)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Average Affirm Rate by Judge Tenure",
    x = "Tenure (Years, Rounded Down)",
    y = "Average Affirm Rate"
  ) +
  theme_minimal()

model <- lm(affirm_rate ~ tenure_years, data = dist_judges)
summary(model)

dist_judges %>%
  count(songer_code) %>%
  filter(n > 1)

dist_judges %>%
  filter(songer_code %in% c(10505, 10632, 11226, 11251, 20341, 20579, 20854)) %>%
  arrange(songer_code)

dist_judges_dedup <- dist_judges %>%
  filter(!is.na(songer_code)) %>%
  group_by(songer_code) %>%
  slice(1) %>%
  ungroup()

# Prepare judge appointment and retirement dates from deduplicated data
judge_dates_dedup <- dist_judges_dedup %>%
  filter(amon != 99, ayear != 9999, monl != 99, yearl != 9999) %>%
  mutate(
    appoint_date = as.Date(paste(ayear, amon, 1, sep = "-")),
    retire_date = as.Date(paste(yearl, monl, 1, sep = "-")),
    tenure_years = as.numeric(difftime(retire_date, appoint_date, units = "days")) / 365.25,
    tenure_floor = floor(tenure_years)
  ) %>%
  select(songer_code, appoint_date, retire_date, tenure_years, tenure_floor)

data <- data %>%
  select(-matches("appoint_date|retire_date|tenure_years|tenure_floor"))

data <- data %>%
  left_join(judge_dates_dedup, by = c("distjudg" = "songer_code"))

# Compute tenure at time of case
data <- data %>%
  mutate(
    case_date = as.Date(paste(year, month, day, sep = "-")),
    case_tenure_years = as.numeric(difftime(case_date, appoint_date, units = "days")) / 365.25,
    case_tenure_years = ifelse(case_tenure_years < 0, NA, case_tenure_years),
    case_tenure_floor = floor(case_tenure_years)
  )

affirmed <- c(1, 8)
reversed <- c(2, 3, 4, 7)

data <- data %>%
  mutate(
    result = case_when(
      treat %in% affirmed ~ "Affirm",
      treat %in% reversed ~ "Reverse",
      TRUE ~ "Other"
    ),
    Affirmed = result == "Affirm"
  )

# Calculate average affirmation rate by case-time judge tenure
affirm_by_case_tenure <- data %>%
  filter(!is.na(case_tenure_floor)) %>%
  group_by(case_tenure_floor) %>%
  summarise(
    affirm_rate = mean(Affirmed, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(affirm_by_case_tenure, aes(x = case_tenure_floor, y = affirm_rate)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Affirmation Rate by Judge Tenure at Time of Case",
    x = "Judge Tenure at Time of Case (Years, Rounded Down)",
    y = "Affirmation Rate"
  ) +
  theme_minimal()

# Count number of cases by judge tenure at time of case
case_counts_by_tenure <- data %>%
  filter(!is.na(case_tenure_floor)) %>%
  group_by(case_tenure_floor) %>%
  summarise(num_cases = n(), .groups = "drop")

# Plot
ggplot(case_counts_by_tenure, aes(x = case_tenure_floor, y = num_cases)) +
  geom_point(size = 2) +
  geom_line() +
  labs(
    title = "Number of Cases by Judge Tenure at Time of Case",
    x = "Judge Tenure at Time of Case (Years, Rounded Down)",
    y = "Number of Cases"
  ) +
  theme_minimal()

# Calculate proportion of affirmed cases by judge tenure at time of case (rounded down), limited to <= 25 years
affirm_rate_by_case_tenure <- data %>%
  filter(!is.na(case_tenure_floor), case_tenure_floor <= 25) %>%
  group_by(case_tenure_floor) %>%
  summarise(affirm_rate = mean(Affirmed, na.rm = TRUE), .groups = "drop")

# Plot
ggplot(affirm_rate_by_case_tenure, aes(x = case_tenure_floor, y = affirm_rate)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Affirm Rate by Judge Tenure at Time of Case (≤ 25 Years)",
    x = "Judge Tenure at Time of Case (Years, Rounded Down)",
    y = "Affirm Rate"
  ) +
  theme_minimal()

# Run regression of affirmation rate on judge tenure at time of case (rounded down), limited to <= 25 years
regression_model <- lm(Affirmed ~ case_tenure_floor, data = data %>% filter(!is.na(Affirmed), !is.na(case_tenure_floor), case_tenure_floor <= 25))

# Display summary
summary(regression_model)

# Run regression of affirmation rate on judge tenure at time of case (rounded down), using all available data
regression_model_full <- lm(Affirmed ~ case_tenure_floor, data = data %>% filter(!is.na(Affirmed), !is.na(case_tenure_floor)))

# Display summary
summary(regression_model_full)

# Compute average judge tenure at time of case by year
avg_case_tenure_by_year <- data %>%
  filter(!is.na(case_tenure_years)) %>%
  group_by(year) %>%
  summarise(avg_case_tenure = mean(case_tenure_years), .groups = "drop")

# Plot
ggplot(avg_case_tenure_by_year, aes(x = year, y = avg_case_tenure)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Average Judge Tenure at Time of Case by Year",
    x = "Year",
    y = "Average Tenure (Years)"
  ) +
  theme_minimal()

###

# Remove rows with NA in the outcome or predictors
data_clean <- data %>%
  select(-distjudg) %>%  # optionally remove IDs
  filter(!is.na(Affirmed)) %>%
  drop_na()

# Fit the model
model <- ranger(
  formula = Affirmed ~ .,
  data = data_clean,
  importance = "impurity",
  probability = TRUE
)

# View variable importance
importance(model)

# Reasonable predictors
vars_to_convert <- c(
  "circuit", "state", "district", "origin", "source", "applfrom",
  "adminrev", "priorpub", "dissent", "concur", "sanction",
  "initiate", "interven", "casetyp1", "geniss", "juris", "statecl", "standing", 
  "mootness", "immunity", "timely", "frivol", "polquest", "oththres", "frivapp",
  "othappth", "typeiss", "procedur", "direct1", "unan", "signed", "constit", "fedlaw"
)

data <- data %>%
  mutate(across(all_of(vars_to_convert), ~ if (is.numeric(.)) as.factor(.) else .))

vars_to_use <- c(
  "year", "month", "circuit", "state", "district", "origin", "source", "applfrom",
  "adminrev", "priorpub", "majvotes", "dissent", "concur", "sanction",
  "initiate", "interven", "casetyp1", "geniss", "juris", "statecl", "standing", 
  "mootness", "immunity", "timely", "frivol", "polquest", "oththres", "frivapp",
  "othappth", "typeiss", "direct1", "unan", "signed"
)

# Filter and clean
data_clean <- data %>%
  select(all_of(c("Affirmed", vars_to_use))) %>%
  filter(!is.na(Affirmed)) %>%
  drop_na()

# Convert character columns to factors
data_clean <- data_clean %>%
  mutate(across(where(is.character), as.factor))

# Fit model
model <- ranger(
  Affirmed ~ .,
  data = data_clean,
  importance = "impurity",
  probability = TRUE
)

# Show variable importance
options(scipen = 999)
importance(model)
options(scipen = 0)

str(data)

table(data$initiate, data$Affirmed)
prop.table(table(data$initiate, data$Affirmed), margin = 1)

data %>%
  filter(!is.na(initiate), !is.na(Affirmed)) %>%
  group_by(initiate, Affirmed) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(initiate) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(initiate), y = prop, fill = Affirmed)) +
  geom_col(position = "fill") +
  labs(
    title = "Proportion of Affirmed Outcomes by Initiate Category",
    x = "Initiate Code",
    y = "Proportion",
    fill = "Affirmed"
  ) +
  theme_minimal()


grouped_treat_data_long <- data %>%
  filter(known_judge == TRUE) %>%
  mutate(
    Affirm = result == "Affirm",
    Reverse = result == "Reverse",
    Other = result == "Other"
  ) %>%
  group_by(year) %>%
  summarise(
    Affirm = mean(Affirm, na.rm = TRUE),
    Reverse = mean(Reverse, na.rm = TRUE),
    Other = mean(Other, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(years = year) %>%
  pivot_longer(
    cols = c("Affirm", "Reverse", "Other"),
    names_to = "group",
    values_to = "values"
  )

# Plot
ggplot(grouped_treat_data_long, aes(x = years, y = values, color = group)) +
  geom_point(size = 2) +
  ggtitle("Proportion of Cases by Result (Known Judges Only)") +
  xlab("Year") +
  ylab("Values") +
  scale_color_manual(values = c("blue", "green", "red")) +
  theme_minimal()

affirm_only <- grouped_treat_data_long %>%
  filter(group == "Affirm")

# Plot
ggplot(affirm_only, aes(x = years, y = values)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  ggtitle("Proportion of Cases Affirmed (Known Judges Only)") +
  xlab("Year") +
  ylab("Proportion Affirmed") +
  theme_minimal()

# Categorize judge tenure into bins
data <- data %>%
  mutate(
    tenure_bin = case_when(
      is.na(case_tenure_years) ~ NA_character_,
      case_tenure_years < 5 ~ "0–5 years",
      case_tenure_years < 10 ~ "5–10 years",
      TRUE ~ "10+ years"
    )
  )

# Calculate affirm rate by year and tenure bin for known judges
affirm_by_year_tenure <- data %>%
  filter(known_judge == TRUE, !is.na(tenure_bin), !is.na(Affirmed)) %>%
  group_by(year, tenure_bin) %>%
  summarise(affirm_rate = mean(Affirmed), .groups = "drop")

# Plot
ggplot(affirm_by_year_tenure, aes(x = year, y = affirm_rate, color = tenure_bin)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Affirm Rate Over Time by Judge Tenure at Case (Known Judges)",
    x = "Year",
    y = "Affirm Rate",
    color = "Judge Tenure"
  ) +
  scale_color_manual(values = c("0–5 years" = "red", "5–10 years" = "orange", "10+ years" = "blue")) +
  theme_minimal()

##### APPELLATE JUDGE DATA -----------------