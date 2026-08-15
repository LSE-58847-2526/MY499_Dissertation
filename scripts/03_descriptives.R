# ==============================================================================
# 03_descriptives.R
#
# London School of Economics and Political Science - Department of Methodology
# MSc in Social Research Methods
# MY499 Dissertation - "Seeing themselves on the ballot"
# Candidate number: 58847
#
# Purpose: Present descriptive statistics (figures and tables)
# ==============================================================================

# Loading packages -------------------------------------------------------------

library(tidyverse)
library(here)

# Reading the main panel -------------------------------------------------------

panel_main <- read_csv(here::here("output", "panel_main.csv"))

# Creating a folder for figures ------------------------------------------------

dir.create(here::here("output", "figures"), showWarnings = FALSE)

# A minimal serif theme, consistent across figures -----------------------------

theme_thesis <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 11, hjust = 0),
    strip.text = element_text(size = 11)
  )

# ------------------------------------------------------------------------------
# TABLE 1: Descriptive statistics (2015 and 2021 elections)
# ------------------------------------------------------------------------------

# Mean, median, SD, min and max for each analysis variable.

# Creating a vector to select and order the table rows -------------------------

vars_main <- c(
  "youth_turnout_18to29", "youth_cand_share",
  "youth_share", "total_electors", "log_total_electors"
)

table_1 <- panel_main %>%
  select(all_of(vars_main)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(variable = factor(variable, levels = vars_main)) %>%
  arrange(variable)

print(table_1)

# ------------------------------------------------------------------------------
# TABLE 2: Mean of main variables across elections (2015-2021)
# ------------------------------------------------------------------------------

# District-level mean of each variable, by election year.

table_2 <- panel_main %>%
  select(
    year, youth_turnout_18to29, youth_cand_share,
    youth_share, total_electors, log_total_electors
  ) %>%
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  group_by(variable, year) %>%
  summarise(Mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = Mean) %>%
  mutate(variable = factor(variable,
    levels = c(
      "youth_turnout_18to29", "youth_cand_share",
      "youth_share", "total_electors", "log_total_electors"
    )
  )) %>%
  arrange(variable)

print(table_2)

# ------------------------------------------------------------------------------
# Voter vs candidate contrast (same age range, 18-29)
# Uses the 18-29 threshold for both, so the comparison is consistent across ages
# ------------------------------------------------------------------------------

youth_contrast <- panel_main %>%
  summarise(
    youth_voter_share = mean(youth_share, na.rm = TRUE),
    youth_candidate_share_29 = mean(n_le29 / n_candidates, na.rm = TRUE)
  )

print(youth_contrast)

# ------------------------------------------------------------------------------
# FIGURE 1: Box plots of youth turnout by election
# ------------------------------------------------------------------------------

fig_1 <- panel_main %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = youth_turnout_18to29, fill = year)) +
  geom_boxplot(width = 0.5, outlier.size = 1, show.legend = FALSE) +
  scale_fill_manual(values = c("2015" = "#6b8cae", "2021" = "#c17b7b")) +
  labs(x = "Election year", y = "Youth turnout") +
  theme_thesis

ggsave(here::here("output", "figures", "fig_1_boxplot_turnout.png"),
  fig_1,
  width = 5, height = 4, dpi = 150
)

# Box plot statistics (youth turnout by election) ------------------------------

boxplot_stats <- panel_main %>%
  group_by(year) %>%
  summarise(
    min = min(youth_turnout_18to29, na.rm = TRUE),
    Q1 = quantile(youth_turnout_18to29, 0.25, na.rm = TRUE),
    median = median(youth_turnout_18to29, na.rm = TRUE),
    Q3 = quantile(youth_turnout_18to29, 0.75, na.rm = TRUE),
    max = max(youth_turnout_18to29, na.rm = TRUE),
    IQR = IQR(youth_turnout_18to29, na.rm = TRUE),
    .groups = "drop"
  )

print(boxplot_stats)

# ------------------------------------------------------------------------------
# FIGURE 2: Young-candidate share vs youth turnout, by election
# ------------------------------------------------------------------------------

# Scatter with an OLS fit line in each year.

fig_2 <- panel_main %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = youth_cand_share, y = youth_turnout_18to29)) +
  geom_point(aes(colour = year), alpha = 0.45, size = 1.6, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 0.6) +
  scale_colour_manual(values = c("2015" = "#6b8cae", "2021" = "#c17b7b")) +
  facet_wrap(~year) +
  labs(x = "Young-candidate share", y = "Youth turnout") +
  theme_thesis

ggsave(here::here("output", "figures", "fig_2_scatter.png"),
  fig_2,
  width = 9, height = 4, dpi = 150
)

# ------------------------------------------------------------------------------
# Pearson correlation R-squared of the cross-sectional association, by year
# ------------------------------------------------------------------------------

r2_by_year <- panel_main %>%
  group_by(year) %>%
  summarise(
    pearson_r = cor(youth_cand_share, youth_turnout_18to29),
    r_squared = cor(youth_cand_share, youth_turnout_18to29)^2,
    .groups = "drop"
  )

print(r2_by_year)

# ------------------------------------------------------------------------------
# FIGURE 3: Mean youth turnout by department, 2015 vs 2021
# ------------------------------------------------------------------------------

# District-level mean of youth turnout, aggregated by department and year.
# Bars ordered by 2021 turnout. Asuncion (the capital) marked with (*).

dept_turnout <- panel_main %>%
  mutate(
    department = str_to_title(str_to_lower(department)),
    department = if_else(department == "Capital", "Asunción (*)", department)
  ) %>%
  group_by(department, year) %>%
  summarise(
    mean_turnout = mean(youth_turnout_18to29, na.rm = TRUE),
    .groups = "drop"
  )

# Order departments by 2021 turnout (descending) -------------------------------

dept_order <- dept_turnout %>%
  filter(year == 2021) %>%
  arrange(desc(mean_turnout)) %>%
  pull(department)

dept_turnout <- dept_turnout %>%
  mutate(
    department = factor(department, levels = dept_order),
    year = factor(year)
  )

fig_3 <- ggplot(dept_turnout, aes(x = department, y = mean_turnout, fill = year)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c("2015" = "#6b8cae", "2021" = "#c17b7b")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = NULL, y = "Youth turnout", fill = "Election year") +
  theme_thesis +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.major.x = element_blank()
  )

ggsave(here::here("output", "figures", "fig_3_dept_turnout.png"),
       fig_3,
       width = 9, height = 5, dpi = 150
)

# ------------------------------------------------------------------------------
# Distribution of district electorate, raw vs log (APPENDIX E)
# ------------------------------------------------------------------------------

elect_long <- panel_main %>%
  transmute(
    `Raw` = total_electors,
    `Log` = log_total_electors
  ) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "value") %>%
  mutate(scale = factor(scale, levels = c("Raw", "Log")))

fig_appendix_elect <- ggplot(elect_long, aes(x = value)) +
  geom_histogram(
    bins = 30, fill = "#6b8cae", colour = "white", linewidth = 0.3
  ) +
  facet_wrap(~scale, scales = "free_x") +
  scale_x_continuous(labels = scales::label_comma()) +
  labs(x = NULL, y = "Districts") +
  theme_thesis

ggsave(here::here("output", "figures", "appendix_hist_elect_log.png"),
  fig_appendix_elect,
  width = 9, height = 3.8, dpi = 150
)

# ============================ End of script ===================================