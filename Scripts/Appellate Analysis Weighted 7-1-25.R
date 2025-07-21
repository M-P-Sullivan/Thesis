### Setup and Packages --------------------------------------------

# Install if needed:
# install.packages(c("dplyr", "ggplot2", "haven", "here", "tidyr", "readr"))

library(dplyr)
library(ggplot2)
library(haven)
library(here)
library(tidyr)
library(readr)
library(broom)
library(rlang)
library(purrr)
library(patchwork)
library(scales)

### Load Case-Level Data ------------------------------------------

# Load appellate case data
case_data <- read_csv(here("cta96.csv"))

# Load circuit-year weighting data
weights <- read_csv(here("data_weights.csv"))

# Fix 1976 6th Circuit special row
weights <- bind_rows(
  weights,
  data.frame(Year = 76, Circuit = 9, Circuit_Cases = (655 + 596) / 2)
)

# Convert to usable columns
weights <- weights %>%
  mutate(
    year = Year + 1900,
    circuit = Circuit
  )

### Merge Weights Into Case Data ----------------------------------

# Calculate circuit-year total population proportions
weights <- weights %>%
  group_by(year) %>%
  mutate(
    year_circuit_cases = sum(Circuit_Cases),
    circuit_proportion = Circuit_Cases / year_circuit_cases
  ) %>%
  ungroup()

# Count actual observed sample cases from case_data
sample_counts <- case_data %>%
  count(year, circuit, name = "sample_cases")

year_totals <- case_data %>%
  count(year, name = "year_sample_cases")

# Join into weights and calculate final weights
weights <- weights %>%
  left_join(sample_counts, by = c("year", "circuit")) %>%
  left_join(year_totals, by = "year") %>%
  mutate(
    sample_proportion = sample_cases / year_sample_cases,
    weighting = circuit_proportion / sample_proportion
  )

# Merge weights into case-level data
case_data <- left_join(case_data, weights, by = c("year", "circuit"))

# Drop rows with missing weights
case_data <- case_data %>%
  filter(!is.na(weighting))

affirmed <- c(1, 8)
reversed <- c(2, 3, 4, 7)

case_data <- case_data %>%
  mutate(
    lower_direct1 = case_when(
      direct1 == 3 & treat %in% affirmed ~ 3,
      direct1 == 1 & treat %in% reversed ~ 3,
      direct1 == 1 & treat %in% affirmed ~ 1,
      direct1 == 3 & treat %in% reversed ~ 1,
      TRUE ~ 0
    )
  )

### Functions -----------------------------------------------------

plot_weighted_outcomes <- function(df, 
                                   filter_expr = NULL, 
                                   title_suffix = "", 
                                   loess = TRUE) {
  # Decision categories
  affirmed <- c(1, 8)
  reversed <- c(2, 3, 4, 7)
  indeterminate <- c(0, 5, 6, 9, 10)
  
  # Apply optional filter
  if (!is.null(filter_expr)) {
    df <- df %>% filter(!!rlang::parse_expr(filter_expr))
  }
  
  # Remove rows without weights
  df <- df %>% filter(!is.na(weighting))
  
  # Define outcome groups
  df <- df %>%
    mutate(
      outcome_group = case_when(
        treat %in% affirmed ~ "Affirmed",
        treat %in% reversed ~ "Reversed",
        treat %in% indeterminate ~ "Indeterminate",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(outcome_group))
  
  # Summarize weighted proportions by year and group
  outcome_summary <- df %>%
    group_by(year, outcome_group) %>%
    summarise(weighted_sum = sum(weighting), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
      total = sum(weighted_sum),
      proportion = weighted_sum / total
    ) %>%
    ungroup() %>%
    rename(group = outcome_group, values = proportion)
  
  # Plot
  p <- ggplot(outcome_summary, aes(x = year, y = values, color = group)) +
    geom_point(size = 2) +
    labs(
      title = paste("Weighted Outcome Rates by Year", title_suffix),
      x = "Year",
      y = "Proportion",
      color = "Outcome"
    ) +
    scale_color_manual(values = c("Affirmed" = "blue", "Reversed" = "red", "Indeterminate" = "green")) +
    theme_minimal()
  
  if (loess) {
    p <- p + geom_smooth(method = "loess", se = FALSE)
  }
  
  return(p)
}

plot_weighted_proportions <- function(df, 
                                      varname, 
                                      groupings, 
                                      group_labels, 
                                      title = NULL,
                                      chart_type = c("scatter", "area"),
                                      manual_colors = NULL) {
  var_sym <- rlang::sym(varname)
  chart_type <- match.arg(chart_type)
  
  title <- if (is.null(title)) paste("Weighted Proportion of", varname, "by Year") else title
  
  # Build group formulas
  formulas <- purrr::map2(groupings, group_labels, ~ rlang::expr(!!var_sym %in% !!.x ~ !!.y))
  formulas <- append(formulas, list(rlang::expr(TRUE ~ "Other")))
  
  # Assign group
  df_grouped <- df %>%
    filter(!is.na(weighting)) %>%
    mutate(group = dplyr::case_when(!!!formulas))
  
  # Compute weighted proportions
  prop_data <- df_grouped %>%
    group_by(year, group) %>%
    summarise(weighted_sum = sum(weighting), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
      total = sum(weighted_sum),
      proportion = weighted_sum / total
    ) %>%
    ungroup()
  
  # Initialize plot
  p <- ggplot(prop_data, aes(x = year, y = proportion, fill = group, color = group))
  
  if (chart_type == "area") {
    p <- p +
      geom_area(position = "stack", alpha = 0.8, color = NA)
    if (!is.null(manual_colors)) {
      p <- p + scale_fill_manual(values = manual_colors)
    } else {
      p <- p + scale_fill_brewer(palette = "Set2")
    }
    p <- p + guides(color = "none")
    
  } else if (chart_type == "scatter") {
    p <- p +
      geom_point(size = 2) +
      geom_line()
    if (!is.null(manual_colors)) {
      p <- p + scale_color_manual(values = manual_colors)
    } else {
      p <- p + scale_color_brewer(palette = "Set2")
    }
    p <- p + guides(fill = "none")
  }
  
  p +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = title,
      x = "Year",
      y = "Proportion",
      fill = varname,
      color = varname
    ) +
    theme_minimal()
}






### Proportions

# Stacked area chart
plot_weighted_proportions(
  case_data,
  varname = "typeiss",
  groupings = list(c(1), c(2), c(4)),
  group_labels = c("Criminal", "Civil - Government", "Civil - Private"),
  chart_type = "area"
)

# Scatter + line version
plot_weighted_proportions(
  case_data,
  varname = "typeiss",
  groupings = list(c(1), c(2), c(4)),
  group_labels = c("Criminal", "Civil - Government", "Civil - Private"),
  chart_type = "scatter"
)

plot_weighted_proportions(
  case_data,
  varname = "origin",
  groupings = list(
    c(1, 2),   # District Court
    c(3)       # State Court
  ),
  group_labels = c("District Court", "State Court"),
  title = "Proportion of Cases by Origin (Weighted)",
  chart_type = "area"
)

plot_weighted_proportions(
  case_data,
  varname = "origin",
  groupings = list(
    c(1, 2),  # District Court
    c(3),     # State Court
    c(6)      # Agency 
  ),
  group_labels = c("District Court", "State Court", "Agency"),
  title = "Proportion of Cases by Origin (Weighted)",
  chart_type = "scatter"
)

plot_weighted_proportions(
  case_data,
  varname = "source",
  groupings = list(
    c(1, 2),  # District Court
    c(3),     # State Court
    c(6)      # Agency 
  ),
  group_labels = c("District Court", "State Court", "Agency"),
  title = "Proportion of Cases by Source (Weighted)",
  chart_type = "scatter"
)

plot_weighted_proportions(
  case_data,
  varname = "habeas",
  groupings = list(
    c(1),       # State
    c(2, 3)     # Federal
  ),
  group_labels = c("State Habeas", "Federal Habeas"),
  title = "Proportion of Habeas Cases Over Time (Weighted)",
  chart_type = "area"
)

plot_weighted_proportions(
  case_data,
  varname = "typeiss",
  groupings = list(
    c(1),   # Criminal
    c(2),   # Civil - Government
    c(3)    # Civil - Private
  ),
  group_labels = c("Criminal", "Civil - Government", "Civil - Private"),
  title = "Proportion of Case Types Over Time (Weighted)",
  chart_type = "area"
)

plot_weighted_proportions(
  case_data,
  varname = "direct1",
  groupings = list(c(3), c(2), c(1)),
  group_labels = c("Liberal", "mixed", "Conservative"),
  title = "Proportion of Liberal vs Conservative Decisions (Weighted)",
  chart_type = "scatter",
  manual_colors = c("Liberal" = "blue", "Conservative" = "red", "mixed" = "gray", "Other" = "black")
)


plot_weighted_proportions(
  case_data,
  varname = "lower_direct1",
  groupings = list(
    c(3),   # Liberal lower court
    c(1)    # Conservative lower court
  ),
  group_labels = c("Liberal Lower Court", "Conservative Lower Court"),
  title = "Proportion of Lower Court Direction (Weighted)",
  manual_colors = c("Liberal Lower Court" = "blue", "Conservative Lower Court" = "red", "Other" = "gray")
)


plot_weighted_proportions(
  case_data,
  varname = "geniss",
  groupings = list(
    c(1),   # Criminal
    c(2),   # Civil Rights
    c(7)    # Economic Activity
  ),
  group_labels = c("Criminal", "Civil Rights", "Economic Activity"),
  title = "Proportion of General Issue Categories Over Time (Weighted)",
  chart_type = "scatter"
)


### Outcomes

plot_weighted_outcomes(case_data)

for (c in sort(unique(case_data$circuit))) {
  p <- plot_weighted_outcomes(
    case_data,
    filter_expr = paste0("circuit == ", c),
    title_suffix = paste("- Circuit", c),
    loess = TRUE
  )
  print(p)
}

plot_weighted_outcomes(case_data, filter_expr = paste0("typeiss == ", 1), title_suffix = "Criminal")
plot_weighted_outcomes(case_data, filter_expr = paste0("typeiss == ", 2), title_suffix = "Civil - Government")
plot_weighted_outcomes(case_data, filter_expr = paste0("typeiss == ", 4), title_suffix = "Civil - Private")
plot_weighted_outcomes(
  case_data,
  filter_expr = "!(typeiss %in% c(1, 2, 4))",
  title_suffix = "Other"
)

plot_weighted_outcomes(case_data, filter_expr = paste0("direct1 == ", 3), title_suffix = "Liberal Outcomes")
plot_weighted_outcomes(case_data, filter_expr = paste0("direct1 == ", 1), title_suffix = "Conservative Outcomes")


### Means, SDs and t-tests for Proportions----------------

## Set up data frame with proportion descriptives

# Initialize empty results data frame
proportion_summary <- tibble(
  variable = character(),
  category = character(),
  mean_prop_pre = numeric(),
  sd_prop_pre = numeric(),
  mean_prop_post = numeric(),
  sd_prop_post = numeric(),
  t_statistic = numeric(),
  p_value = numeric()
)

# Function to compute summary and append to proportion_summary
summarize_group_proportions <- function(df, varname, groupings, group_labels) {
  var_sym <- sym(varname)
  
  # Build case_when expressions
  group_exprs <- map2(groupings, group_labels, ~ expr(!!var_sym %in% !!.x ~ !!.y))
  group_exprs <- append(group_exprs, list(expr(TRUE ~ "Other")))
  
  df_grouped <- df %>%
    filter(!is.na(weighting)) %>%
    mutate(group = case_when(!!!group_exprs)) %>%
    group_by(year, group) %>%
    summarise(weighted_sum = sum(weighting), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
      total = sum(weighted_sum),
      proportion = weighted_sum / total
    ) %>%
    ungroup()
  
  # Pre/post-1970 summary
  summary_stats <- df_grouped %>%
    mutate(period = ifelse(year < 1970, "pre", "post")) %>%
    group_by(group, period) %>%
    summarise(
      mean_prop = mean(proportion),
      sd_prop = sd(proportion),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = period, values_from = c(mean_prop, sd_prop))
  
  # T-tests
  ttests <- df_grouped %>%
    mutate(period = ifelse(year < 1970, "pre", "post")) %>%
    group_by(group) %>%
    summarise(ttest = list(t.test(proportion ~ period)), .groups = "drop") %>%
    mutate(
      t_statistic = map_dbl(ttest, ~ .x$statistic),
      p_value = map_dbl(ttest, ~ .x$p.value)
    ) %>%
    select(-ttest)
  
  # Combine and return
  out <- left_join(summary_stats, ttests, by = "group") %>%
    mutate(variable = varname) %>%
    select(variable, category = group,
           mean_prop_pre, sd_prop_pre,
           mean_prop_post, sd_prop_post,
           t_statistic, p_value)
  
  return(out)
}

# First variable: typeiss
proportion_summary <- bind_rows(
  proportion_summary,
  summarize_group_proportions(
    case_data,
    varname = "typeiss",
    groupings = list(c(1), c(2), c(3)),
    group_labels = c("Criminal", "Civil - Gov", "Civil - Private")
  )
)


proportion_summary <- bind_rows(
  proportion_summary,
  summarize_group_proportions(
    case_data,
    varname = "geniss",
    groupings = list(c(1), c(2), c(7)),
    group_labels = c("Criminal", "Civil Rights", "Economic Activity")
  )
)

#Circuit 11 excluded because no pre-data breaks the function
case_data_no_11 <- case_data %>% filter(circuit != 11)
circuit_vals <- 0:10
circuit_labels <- ifelse(circuit_vals == 0, "DC", as.character(circuit_vals))

proportion_summary <- bind_rows(
  proportion_summary,
  summarize_group_proportions(
    case_data_no_11,
    varname = "circuit",
    groupings = lapply(circuit_vals, function(x) c(x)),
    group_labels = circuit_labels
  )
)

# Circuit 11 added manually
c11_post <- case_data %>%
  filter(circuit == 11, year >= 1970, !is.na(weighting)) %>%
  count(year, wt = weighting, name = "weighted_count") %>%
  summarise(
    mean_prop_post = mean(weighted_count / sum(weighted_count)),
    sd_prop_post = sd(weighted_count / sum(weighted_count))
  ) %>%
  mutate(
    variable = "circuit",
    category = "11",
    mean_prop_pre = NA,
    sd_prop_pre = NA,
    t_statistic = NA,
    p_value = NA
  ) %>%
  select(variable, category, mean_prop_pre, sd_prop_pre,
         mean_prop_post, sd_prop_post, t_statistic, p_value)

proportion_summary <- bind_rows(proportion_summary, c11_post)

proportion_summary <- bind_rows(
  proportion_summary,
  summarize_group_proportions(
    case_data,
    varname = "origin",
    groupings = list(c(1, 2), c(3), c(6)),
    group_labels = c("District Court", "State Court", "Agency")
  )
)

proportion_summary <- bind_rows(
  proportion_summary,
  summarize_group_proportions(
    case_data,
    varname = "direct1",
    groupings = list(c(3), c(1), c(2)),
    group_labels = c("Liberal", "Conservative", "mixed")
  )
)

## Producing a word-friendly version

write.csv(proportion_summary, "proportion_summary.csv", row.names = FALSE)


### Means, SDs and t-tests for Affirm Rate----------------

## Initialize Affirm Rate Summary DataFrame 

affirm_summary <- tibble(
  variable = character(),
  category = character(),
  mean_affirm_pre = numeric(),
  sd_affirm_pre = numeric(),
  mean_affirm_post = numeric(),
  sd_affirm_post = numeric(),
  t_statistic = numeric(),
  p_value = numeric()
)

summarize_group_affirm <- function(df, varname, groupings, group_labels) {
  var_sym <- sym(varname)
  
  # Build case_when expressions for group labeling
  group_exprs <- map2(groupings, group_labels, ~ expr(!!var_sym %in% !!.x ~ !!.y))
  group_exprs <- append(group_exprs, list(expr(TRUE ~ "Other")))
  
  df_grouped <- df %>%
    mutate(affirmed = treat %in% c(1, 8)) %>%
    filter(!is.na(weighting)) %>%
    mutate(group = case_when(!!!group_exprs)) %>%
    group_by(year, group) %>%
    summarise(
      affirm_rate = sum(weighting[affirmed]) / sum(weighting),
      .groups = "drop"
    ) %>%
    mutate(period = ifelse(year < 1970, "pre", "post"))
  
  # Means and SDs
  mean_sd <- df_grouped %>%
    group_by(group, period) %>%
    summarise(
      mean_affirm = mean(affirm_rate),
      sd_affirm = sd(affirm_rate),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = period, values_from = c(mean_affirm, sd_affirm))
  
  # T-tests
  ttests <- df_grouped %>%
    group_by(group) %>%
    summarise(ttest = list(t.test(affirm_rate ~ period)), .groups = "drop") %>%
    mutate(
      t_statistic = map_dbl(ttest, ~ .x$statistic),
      p_value = map_dbl(ttest, ~ .x$p.value)
    ) %>%
    select(-ttest)
  
  # Combine
  out <- left_join(mean_sd, ttests, by = "group") %>%
    mutate(variable = varname) %>%
    select(variable, category = group,
           mean_affirm_pre, sd_affirm_pre,
           mean_affirm_post, sd_affirm_post,
           t_statistic, p_value)
  
  return(out)
}

## Function to compute affirm rate summary statistics
summarize_group_affirm <- function(df, varname, groupings, group_labels) {
  var_sym <- sym(varname)
  
  # Build case_when expressions for group labeling
  group_exprs <- map2(groupings, group_labels, ~ expr(!!var_sym %in% !!.x ~ !!.y))
  group_exprs <- append(group_exprs, list(expr(TRUE ~ "Other")))
  
  df_grouped <- df %>%
    mutate(affirmed = treat %in% c(1, 8)) %>%
    filter(!is.na(weighting)) %>%
    mutate(group = case_when(!!!group_exprs)) %>%
    group_by(year, group) %>%
    summarise(
      affirm_rate = sum(weighting[affirmed]) / sum(weighting),
      .groups = "drop"
    ) %>%
    mutate(period = ifelse(year < 1970, "pre", "post"))
  
  # Means and SDs
  mean_sd <- df_grouped %>%
    group_by(group, period) %>%
    summarise(
      mean_affirm = mean(affirm_rate),
      sd_affirm = sd(affirm_rate),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = period, values_from = c(mean_affirm, sd_affirm))
  
  # T-tests
  ttests <- df_grouped %>%
    group_by(group) %>%
    summarise(ttest = list(t.test(affirm_rate ~ period)), .groups = "drop") %>%
    mutate(
      t_statistic = map_dbl(ttest, ~ .x$statistic),
      p_value = map_dbl(ttest, ~ .x$p.value)
    ) %>%
    select(-ttest)
  
  # Combine
  out <- left_join(mean_sd, ttests, by = "group") %>%
    mutate(variable = varname) %>%
    select(variable, category = group,
           mean_affirm_pre, sd_affirm_pre,
           mean_affirm_post, sd_affirm_post,
           t_statistic, p_value)
  
  return(out)
}

## Adding aggregate statistics

# Compute weighted affirm rate by year for all cases
affirm_df_agg <- case_data %>%
  mutate(affirmed = treat %in% c(1, 8)) %>%
  filter(!is.na(weighting)) %>%
  group_by(year) %>%
  summarise(
    affirm_rate = sum(weighting[affirmed]) / sum(weighting),
    .groups = "drop"
  ) %>%
  mutate(period = ifelse(year < 1970, "pre", "post"))

# Compute means and SDs
mean_sd_agg <- affirm_df_agg %>%
  group_by(period) %>%
  summarise(
    mean_affirm = mean(affirm_rate),
    sd_affirm = sd(affirm_rate),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = period, values_from = c(mean_affirm, sd_affirm))

# Run t-test
ttest_agg <- t.test(affirm_rate ~ period, data = affirm_df_agg)

# Append to affirm_summary
affirm_summary <- bind_rows(
  affirm_summary,
  tibble(
    variable = "aggregate",
    category = "all",
    mean_affirm_pre = mean_sd_agg$mean_affirm_pre,
    sd_affirm_pre = mean_sd_agg$sd_affirm_pre,
    mean_affirm_post = mean_sd_agg$mean_affirm_post,
    sd_affirm_post = mean_sd_agg$sd_affirm_post,
    t_statistic = ttest_agg$statistic,
    p_value = ttest_agg$p.value
  )
)

## Adding statistics by variable

# typeiss
affirm_summary <- bind_rows(
  affirm_summary,
  summarize_group_affirm(
    case_data,
    varname = "typeiss",
    groupings = list(c(1), c(2), c(3)),
    group_labels = c("Criminal", "Civil - Gov", "Civil - Private")
  )
)

# geniss
affirm_summary <- bind_rows(
  affirm_summary,
  summarize_group_affirm(
    case_data,
    varname = "geniss",
    groupings = list(c(1), c(2), c(7)),
    group_labels = c("Criminal", "Civil Rights", "Economic Activity")
  )
)

# circuit (excluding 11)
circuit_vals <- 0:10
circuit_labels <- ifelse(circuit_vals == 0, "DC", as.character(circuit_vals))
case_data_no_11 <- case_data %>% filter(circuit != 11)

affirm_summary <- bind_rows(
  affirm_summary,
  summarize_group_affirm(
    case_data_no_11,
    varname = "circuit",
    groupings = lapply(circuit_vals, function(x) c(x)),
    group_labels = circuit_labels
  )
)

# Circuit 11 (post only, manually)
c11_post_affirm <- case_data %>%
  filter(circuit == 11, year >= 1970, !is.na(weighting)) %>%
  mutate(affirmed = treat %in% c(1, 8)) %>%
  group_by(year) %>%
  summarise(
    affirm_rate = sum(weighting[affirmed]) / sum(weighting),
    .groups = "drop"
  ) %>%
  summarise(
    mean_affirm_post = mean(affirm_rate),
    sd_affirm_post = sd(affirm_rate)
  ) %>%
  mutate(
    variable = "circuit",
    category = "11",
    mean_affirm_pre = NA,
    sd_affirm_pre = NA,
    t_statistic = NA,
    p_value = NA
  ) %>%
  select(variable, category, mean_affirm_pre, sd_affirm_pre,
         mean_affirm_post, sd_affirm_post, t_statistic, p_value)

affirm_summary <- bind_rows(affirm_summary, c11_post_affirm)

# origin
affirm_summary <- bind_rows(
  affirm_summary,
  summarize_group_affirm(
    case_data,
    varname = "origin",
    groupings = list(c(1, 2), c(3), c(6)),
    group_labels = c("District Court", "State Court", "Agency")
  )
)

# direct1
affirm_summary <- bind_rows(
  affirm_summary,
  summarize_group_affirm(
    case_data,
    varname = "direct1",
    groupings = list(c(3), c(1), c(2)),
    group_labels = c("Liberal", "Conservative", "mixed")
  )
)

## Print to CSV for use in Word
write.csv(affirm_summary, "affirm_summary.csv", row.names = FALSE)

### Scatterplots for display-------------------------------


affirmed <- c(1, 8)
reversed <- c(2, 3, 4, 7)
indeterminate <- c(0, 5, 6, 9, 10)

# Get the full range of years to standardize x-axis
year_range <- range(case_data$year, na.rm = TRUE)

# Create outcome plots by circuit
circuit_outcome_plots <- lapply(0:11, function(c) {
  df <- case_data %>%
    filter(circuit == c, !is.na(weighting)) %>%
    mutate(outcome_group = case_when(
      treat %in% affirmed ~ "Affirmed",
      treat %in% reversed ~ "Reversed",
      treat %in% indeterminate ~ "Indeterminate",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(outcome_group)) %>%
    group_by(year, outcome_group) %>%
    summarise(weighted_sum = sum(weighting), .groups = "drop") %>%
    group_by(year) %>%
    mutate(
      total = sum(weighted_sum),
      proportion = weighted_sum / total
    ) %>%
    ungroup()
  
  if (nrow(df) == 0) {
    return(
      ggplot() +
        theme_void() +
        labs(title = paste0("Circuit ", ifelse(c == 0, "DC", c)))
    )
  }
  
  ggplot(df, aes(x = year, y = proportion, color = outcome_group)) +
    geom_point(size = 1.5) +
    geom_smooth(se = FALSE, method = "loess") +
    scale_color_manual(
      values = c("Affirmed" = "blue", "Reversed" = "red", "Indeterminate" = "green"),
      name = "Outcome"
    ) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    scale_x_continuous(limits = year_range) +
    labs(
      title = paste0("Circuit ", ifelse(c == 0, "DC", c)),
      x = "Year", y = "Proportion"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right")
})

# Display each plot individually
for (p in circuit_outcome_plots) {
  print(p)
}

### District court judge data -------------------------------

# Read and deduplicate judge data
dist_judges <- read_dta("auburn_district_w_songer_codes.dta")

dist_judges_dedup <- dist_judges %>%
  filter(!is.na(songer_code)) %>%
  group_by(songer_code) %>%
  slice(1) %>%
  ungroup()

dist_judges_dedup <- dist_judges_dedup %>%
  mutate(
    judge_party = case_when(
      party == 1 ~ "dem",
      party == 0 ~ "rep",
      TRUE ~ "other"
    )
  )


# Join into appellate case data (without modifying date fields)
case_data_with_judges <- case_data %>%
  left_join(dist_judges_dedup, by = c("distjudg" = "songer_code"))

library(scales)

# Drop missing weights or party info
plot_data <- case_data_with_judges %>%
  filter(!is.na(weighting), !is.na(judge_party)) %>%
  group_by(year, judge_party) %>%
  summarise(weighted_sum = sum(weighting), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    total = sum(weighted_sum),
    proportion = weighted_sum / total
  ) %>%
  ungroup()

# Plot
ggplot(plot_data, aes(x = year, y = proportion, color = judge_party)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_manual(
    values = c("dem" = "blue", "rep" = "red", "other" = "gray"),
    name = "Judge Party"
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportion of Cases by District Judge Party (Weighted)",
    x = "Year",
    y = "Proportion"
  ) +
  theme_minimal()

### Appellate court judge data -------------------------------

app_judges <- read_dta("auburn_appct_stata.dta")
