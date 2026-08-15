# ==============================================================================
# 04_analysis.R
#
# London School of Economics and Political Science - Department of Methodology
# MSc in Social Research Methods
# MY499 Dissertation - "Seeing themselves on the ballot"
# Candidate number: 58847
#
# Purpose: Estimate the effect of the reform on youth turnout
# ==============================================================================

# Loading packages -------------------------------------------------------------

library(tidyverse)
library(fixest)
library(here)
library(multcomp)

# ------------------------------------------------------------------------------
# Reading data produced by 02_clean_data.R
# ------------------------------------------------------------------------------

analytical_data <- read_csv(here::here("output", "analytical_data.csv"))
panel_main <- read_csv(here::here("output", "panel_main.csv"))
panel_placebo <- read_csv(here::here("output", "panel_placebo.csv"))

# Same 11 clusters as in 02_clean_data.R; re-declared for the robustness
# and threshold checks.

crosswalk_main <- tribble(
  ~member_id, ~cluster_id,
  "14_12", "14_12", "14_13", "14_12",
  "9_12", "9_12", "9_13", "9_12",
  "2_22", "2_22", "2_11", "2_22",
  "1_11", "1_11", "1_3", "1_11",
  "14_14", "14_14", "14_0", "14_14",
  "14_20", "14_20", "14_18", "14_20", "14_11", "14_20",
  "1_12", "1_12", "1_0", "1_12", "1_8", "1_12",
  "13_6", "13_6", "13_0", "13_6",
  "15_2", "15_2", "15_9", "15_2",
  "2_33", "2_33", "2_29", "2_33", "2_9", "2_33", "2_3", "2_33",
  "17_4", "17_4", "17_3", "17_4"
)

# ==============================================================================
# 1. MAIN SPECIFICATION
# ==============================================================================

# 'unit' is the harmonised panel unit (constant 2015-2021 boundaries).
# District FE (unit) + explicit Post.
# SEs clustered by unit.

model_main <- feols(
  youth_turnout_18to29 ~ youth_cand_share + post + youth_cand_share:post +
    youth_share + log_total_electors | unit,
  data = panel_main, cluster = ~unit
)
summary(model_main)
confint(model_main)

# Post-reform marginal effect: test whether the slope (β₁ + β₃) equals zero ----

post_slope_test <- summary(
  glht(model_main, linfct = "youth_cand_share + youth_cand_share:post = 0")
)

print(post_slope_test)

# 95% confidence interval for the post-reform slope

post_slope_ci <- confint(
  glht(model_main, linfct = "youth_cand_share + youth_cand_share:post = 0")
)

print(post_slope_ci)

# Baseline (no controls) -------------------------------------------------------

# Robustness check: the interaction is stable without controls.

model_baseline <- feols(
  youth_turnout_18to29 ~ youth_cand_share + post + youth_cand_share:post | unit,
  data = panel_main, cluster = ~unit
)

etable(model_baseline, model_main,
  keep = c("youth_cand_share", "post", "youth_share", "log_total_electors"),
  se.below = TRUE
)

# ------------------------------------------------------------------------------
# FIGURE 4: Estimated relationship before and after the reform
# ------------------------------------------------------------------------------

# Each line shows how youth turnout relates to the young-candidate share,
# before (2015) and after (2021) the reform, based on the main model.

theme_thesis <- theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 11, hjust = 0),
    legend.position = "bottom"
  )

b1 <- coef(model_main)["youth_cand_share"]
b3 <- coef(model_main)["youth_cand_share:post"]

xseq <- seq(min(panel_main$youth_cand_share),
  max(panel_main$youth_cand_share),
  length.out = 100
)
mean_y <- mean(panel_main$youth_turnout_18to29)
mean_x <- mean(panel_main$youth_cand_share)

line_df <- bind_rows(
  tibble(year = "2015", x = xseq, y = mean_y + b1 * (xseq - mean_x)),
  tibble(year = "2021", x = xseq, y = mean_y + (b1 + b3) * (xseq - mean_x))
)

fig_4 <- ggplot() +
  geom_point(
    data = mutate(panel_main, year = factor(year)),
    aes(x = youth_cand_share, y = youth_turnout_18to29, colour = year),
    alpha = 0.30, size = 1.3
  ) +
  geom_line(data = line_df, aes(x = x, y = y, colour = year), linewidth = 1) +
  scale_colour_manual(values = c("2015" = "#6b8cae", "2021" = "#c17b7b")) +
  labs(
    x = "Young-candidate share",
    y = "Youth turnout (18-29)",
    colour = NULL
  ) +
  theme_thesis

dir.create(here::here("output", "figures"), showWarnings = FALSE)
ggsave(here::here("output", "figures", "fig_4_main_result.png"),
  fig_4,
  width = 6, height = 4.5, dpi = 150
)

# ==============================================================================
# 2. ROBUSTNESS: Main / Robustness 1 / Robustness 2
# ==============================================================================

# Main = crosswalk (panel_main, already built).
# Robustness 1 = drop new districts (children).
# Robustness 2 = drop children and parents districts.

children_ids <- unique(crosswalk_main$cluster_id)
parents_ids <- setdiff(crosswalk_main$member_id, crosswalk_main$cluster_id)

# Helper: Build a balanced panel for 2015-2021, dropping given districts.

build_rc <- function(drop_ids) {
  analytical_data %>%
    filter(year %in% c(2015, 2021), !district_id %in% drop_ids) %>%
    transmute(
      unit = district_id, year,
      post = if_else(year == 2021, 1L, 0L),
      youth_cand_share, youth_turnout_18to29, youth_share, log_total_electors
    ) %>%
    group_by(unit) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
}

panel_rc1 <- build_rc(children_ids)
panel_rc2 <- build_rc(c(children_ids, parents_ids))

f_rc <- youth_turnout_18to29 ~ youth_cand_share + post + youth_cand_share:post +
  youth_share + log_total_electors | unit

model_rc1 <- feols(f_rc, data = panel_rc1, cluster = ~unit)
model_rc2 <- feols(f_rc, data = panel_rc2, cluster = ~unit)

etable(model_main, model_rc1, model_rc2,
  keep = "%post",
  se.below = TRUE
)

# ==============================================================================
# 3. ROBUSTNESS: AGE-THRESHOLD
# ==============================================================================

# The threshold counts are aggregated by cluster and each share is recomputed
# from the summed counts, consistent with the main model.

thresholds <- 29:35

panel_thr <- analytical_data %>%
  filter(year %in% c(2015, 2021)) %>%
  left_join(crosswalk_main, by = c("district_id" = "member_id")) %>%
  mutate(unit = coalesce(cluster_id, district_id)) %>%
  group_by(unit, year) %>%
  summarise(
    n_candidates = sum(n_candidates, na.rm = TRUE),
    young_18to29_participation = sum(young_18to29_participation, na.rm = TRUE),
    young_18to29_register = sum(young_18to29_register, na.rm = TRUE),
    total_electors = sum(total_electors, na.rm = TRUE),
    across(all_of(paste0("n_le", thresholds)), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    youth_turnout_18to29 = young_18to29_participation / young_18to29_register,
    youth_share          = young_18to29_register / total_electors,
    log_total_electors   = log(total_electors),
    post                 = if_else(year == 2021, 1L, 0L)
  ) %>%
  group_by(unit) %>%
  filter(n_distinct(year) == 2) %>%
  ungroup()

# Recompute each threshold share from the summed counts

for (t in thresholds) {
  panel_thr[[paste0("share_le", t)]] <-
    panel_thr[[paste0("n_le", t)]] / panel_thr$n_candidates
}

# Run the model for each threshold, collect the interaction + 95% CI

thr_results <- map_dfr(thresholds, function(t) {
  share_var <- paste0("share_le", t)
  fml <- as.formula(paste0(
    "youth_turnout_18to29 ~ ", share_var, " + post + ", share_var, ":post",
    " + youth_share + log_total_electors | unit"
  ))
  m <- feols(fml, data = panel_thr, cluster = ~unit)
  ic <- paste0(share_var, ":post")
  ci <- confint(m)[ic, ]
  tibble(
    cutoff = t,
    beta3 = coef(m)[ic],
    se = se(m)[ic],
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    p = pvalue(m)[ic]
  )
})

print(thr_results)

# ==============================================================================
# 4. PLACEBO TESTS
# ==============================================================================

# ------- Placebo population test: pre-reform window 2006-2010 -----------------

model_placebo_pop <- feols(
  youth_turnout_18to29 ~ youth_cand_share + post + youth_cand_share:post +
    youth_share + log_total_electors | unit,
  data = panel_placebo, cluster = ~unit
)
summary(model_placebo_pop)
confint(model_placebo_pop)["youth_cand_share:post", ]

# -------- Placebo treatment test: mayoral contest -----------------------------

# The reform did not change the mayoral ballot. Harmonise mayoral counts over
# the main window; the panel also carries the council (actual) treatment so the
# conditioned specification can include it.

panel_mayor <- analytical_data %>%
  filter(year %in% c(2015, 2021)) %>%
  left_join(crosswalk_main, by = c("district_id" = "member_id")) %>%
  mutate(unit = coalesce(cluster_id, district_id)) %>%
  group_by(unit, year) %>%
  summarise(
    # --- council (actual treatment) counts ---
    n_young = sum(n_young, na.rm = TRUE),
    n_candidates = sum(n_candidates, na.rm = TRUE),
    # --- mayor (placebo treatment) counts ---
    n_young_mayor = sum(n_young_mayor, na.rm = TRUE),
    n_candidates_mayor = sum(n_candidates_mayor, na.rm = TRUE),
    # --- turnout / electorate ---
    young_18to29_participation = sum(young_18to29_participation, na.rm = TRUE),
    young_18to29_register = sum(young_18to29_register, na.rm = TRUE),
    total_electors = sum(total_electors, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    youth_cand_share       = n_young / n_candidates, # <-- council (actual)
    youth_cand_share_mayor = n_young_mayor / n_candidates_mayor, # <-- mayor (placebo)
    youth_turnout_18to29   = young_18to29_participation / young_18to29_register,
    youth_share            = young_18to29_register / total_electors,
    log_total_electors     = log(total_electors),
    post                   = if_else(year == 2021, 1L, 0L)
  ) %>%
  group_by(unit) %>%
  filter(n_distinct(year) == 2) %>%
  ungroup()

# Unconditioned specification (robustness check; no actual treatment included)
model_placebo_mayor <- feols(
  youth_turnout_18to29 ~ youth_cand_share_mayor + post +
    post:youth_cand_share_mayor + youth_share + log_total_electors | unit,
  data = panel_mayor, cluster = ~unit
)
summary(model_placebo_mayor)
confint(model_placebo_mayor)["youth_cand_share_mayor:post", ]

# Conditioned specification (main), following Eggers, Tuñon and Dafoe (2024,
# p. 1115) on including the real treatment (youth_cand_share)
model_placebo_mayor_cond <- feols(
  youth_turnout_18to29 ~ youth_cand_share_mayor + post + post:youth_cand_share_mayor +
    youth_cand_share + youth_cand_share:post +
    youth_share + log_total_electors | unit,
  data = panel_mayor, cluster = ~unit
)
summary(model_placebo_mayor_cond)
# 95% CIs for the two interactions of interest
confint(model_placebo_mayor_cond)[c("youth_cand_share_mayor:post",
                                    "post:youth_cand_share"), ]

# Side-by-side comparison of the mayoral placebo interaction across both specs
etable(model_placebo_mayor, model_placebo_mayor_cond,
  keep = c("%youth_cand_share_mayor:post", "%youth_cand_share:post"),
  se.below = TRUE
)

# ------------------------------------------------------------------------------
# FIGURE 5: Coefficient plot — reform effect across specifications
# ------------------------------------------------------------------------------

# Interaction estimate (beta3 = reform effect) and its 95% CI across
# specifications: the main model, two district-robustness checks, and the two
# placebo tests. Shows the effect is stable and positive across robustness
# checks and null in the placebos.

coef_plot_data <- tribble(
  ~model,               ~model_obj,                     ~coef_name,
  "Main model",         list(model_main),               "youth_cand_share:post",
  "Robustness 1",       list(model_rc1),                "youth_cand_share:post",
  "Robustness 2",       list(model_rc2),                "youth_cand_share:post",
  "Population placebo", list(model_placebo_pop),        "youth_cand_share:post",
  "Treatment placebo",  list(model_placebo_mayor_cond), "youth_cand_share_mayor:post"
) %>%
  mutate(
    beta    = map2_dbl(model_obj, coef_name, ~ coef(.x[[1]])[.y]),
    ci_low  = map2_dbl(model_obj, coef_name, ~ confint(.x[[1]])[.y, 1]),
    ci_high = map2_dbl(model_obj, coef_name, ~ confint(.x[[1]])[.y, 2]),
    type    = c("Main", "Robustness", "Robustness", "Placebo", "Placebo"),
    model   = factor(model, levels = rev(model))
  )

fig_5 <- ggplot(coef_plot_data, aes(x = beta, y = model)) +
  geom_vline(
    xintercept = 0, linetype = "solid", colour = "grey40",
    linewidth = 0.4
  ) +
  geom_pointrange(
    aes(
      xmin = ci_low, xmax = ci_high,
      colour = type, shape = type
    ),
    linewidth = 0.6, size = 0.7
  ) +
  geom_text(aes(label = sprintf("%.3f", beta)),
    vjust = -1.1, size = 2.8, colour = "grey20"
  ) +
  scale_colour_manual(values = c(
    "Main" = "#3d5a73",
    "Robustness" = "#6b8cae",
    "Placebo" = "#c17b7b"
  )) +
  scale_shape_manual(values = c("Main" = 16, "Robustness" = 16, "Placebo" = 17)) +
  labs(x = "Estimated interaction coefficient", y = NULL) +
  theme_minimal(base_size = 11, base_family = "serif") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(here::here("output", "figures", "fig_5_coefficient_plot.png"),
  fig_5,
  width = 7, height = 4, dpi = 300
)

# ------------------------------------------------------------------------------
# FIGURE 6: Interaction model graph (Appendix F)
# ------------------------------------------------------------------------------

b1 <- coef(model_main)["youth_cand_share"]
b3 <- coef(model_main)["youth_cand_share:post"]
my <- mean(panel_main$youth_turnout_18to29)
mx <- mean(panel_main$youth_cand_share)
xs <- seq(min(panel_main$youth_cand_share),
  max(panel_main$youth_cand_share),
  length.out = 100
)
line_df <- bind_rows(
  tibble(Period = "Pre-reform (2015)", x = xs, y = my + b1 * (xs - mx)),
  tibble(Period = "Post-reform (2021)", x = xs, y = my + (b1 + b3) * (xs - mx))
)
# Annotation positions (right end of each line)
xend <- max(xs)
lab_df <- tibble(
  Period = c("Pre-reform (2015)", "Post-reform (2021)"),
  x = xend,
  y = c(
    my + b1 * (xend - mx) - 0.008,
    my + (b1 + b3) * (xend - mx) + 0.004
  ),
  slp = c(
    sprintf("beta[1] == %.3f", b1),
    sprintf("beta[1] + beta[3] == %.3f", b1 + b3)
  )
)
fig_6 <- ggplot(line_df, aes(x, y, colour = Period)) +
  geom_line(linewidth = 1.2) +
  geom_text(
    data = lab_df, aes(x, y, label = slp),
    parse = TRUE, hjust = 1, vjust = -0.5, size = 3.2, show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("Pre-reform (2015)" = "#3d5a73", "Post-reform (2021)" = "#c17b7b"),
    name = NULL
  ) +
  labs(x = "Young-candidate share", y = "Youth turnout (18-29)") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "serif") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 60, 10, 10)
  )

print(fig_6)

ggsave(here::here("output", "figures", "fig_6_brambor.png"),
  fig_6,
  width = 8, height = 4.5, dpi = 150
)

# ==============================================================================
# 5. ROBUSTNESS: LOCALISED ELECTORAL VIOLENCE (Appendix G)
# ==============================================================================

# Interact the post-reform period with an indicator for the ten departments
# that experienced electoral violence in 2021.

violent_depts <- c(
  "BOQUERON", "CORDILLERA", "CAAGUAZU", "GUAIRA", "ÑEEMBUCU",
  "ITAPUA", "SAN PEDRO", "AMAMBAY", "CANINDEYU", "ALTO PARANA"
)

panel_viol <- panel_main %>%
  mutate(
    violent = as.integer(toupper(department) %in% violent_depts),
    viol_x_post = violent * post
  )

# Verify: districts in violence-affected departments vs those not affected

panel_viol %>%
  distinct(unit, violent) %>%
  count(violent)

# Electoral violence model

model_viol <- feols(
  youth_turnout_18to29 ~ youth_cand_share + post + youth_cand_share:post +
    viol_x_post + youth_share + log_total_electors | unit,
  data = panel_viol, cluster = ~unit
)

summary(model_viol)

etable(model_main, model_viol,
  keep = c("%youth_cand_share:post", "viol_x_post"),
  se.below = TRUE
)

# ============================ End of script ===================================