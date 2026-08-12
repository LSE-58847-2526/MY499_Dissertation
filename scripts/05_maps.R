# ==============================================================================
# 05_maps.R
#
# London School of Economics and Political Science - Department of Methodology
# MSc in Social Research Methods
# MY499 Dissertation - "Seeing themselves on the ballot"
# Candidate number: 58847
#
# Purpose: Create district-level maps of the main variables
# ==============================================================================

# Loading packages -------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)

# Use 'select' and 'filter' from dplyr package to avoid namespace conflicts

select <- dplyr::select
filter <- dplyr::filter

# ------------------------------------------------------------------------------
# STEP 1: Loading shapefile (INE, 2022)
# ------------------------------------------------------------------------------

districts <- st_read("data/maps/Distritos_Paraguay_INE_2022.shp")

# Cleaning names with helper function ------------------------------------------

clean_name <- function(x) {
  x <- toupper(x) # to uppercase
  x <- iconv(x, to = "ASCII//TRANSLIT") # remove accents
  x <- gsub("[^A-Z0-9 ]", "", x) # keep only letters, numbers, spaces
  x <- trimws(x) # remove extra spaces
  x <- gsub("\\s+", " ", x) # collapse multiple spaces
  x
}

# ------------------------------------------------------------------------------
# STEP 2: Building the shapefile key (department + name)
# ------------------------------------------------------------------------------

districts <- districts %>%
  mutate(
    name_key  = clean_name(DIST_DESC_),
    match_key = paste(DPTO, name_key, sep = "_")
  )

# Fixing department numbering from Occidental Region (Chaco, Paraguay) ---------

districts <- districts %>%
  mutate(
    DPTO_fix = case_when(
      DPTO == "16" ~ "17", # Boquerón: 16 in shapefile, 17 in TSJE files
      DPTO == "17" ~ "16", # Alto Paraguay: 17 in shapefile, 16 in TSJE files
      TRUE ~ DPTO
    ),
    match_key = paste(DPTO_fix, name_key, sep = "_")
  )

# Fixing name mismatches (shapefile key -> data key) ---------------------------

key_fixes <- c(
  "02_SAN PEDRO DEL YCUAMANDYYU"         = "02_SAN PEDRO DEL YCUAMANDIYU",
  "04_DOCTOR BOTTRELL"                   = "04_DOCTOR BOTRELL",
  "04_GRAL EUGENIO A GARAY"              = "04_GENERAL EUGENIO ALEJANDRINO GARAY",
  "04_MBOCAYATY"                         = "04_MBOCAYATY DEL GUAIRA",
  "04_YATAITY"                           = "04_YATAITY DEL GUAIRA",
  "05_DR CECILIO BAEZ"                   = "05_DOCTOR CECILIO BAEZ",
  "05_DR J EULOGIO ESTIGARRIBIA"         = "05_DOCTOR JOSE EULOGIO ESTIGARRIBIA",
  "05_DR JUAN MANUEL FRUTOS"             = "05_DOCTOR JUAN MANUEL FRUTOS",
  "05_YBYRAROBANA"                       = "05_YBYRAROVANA",
  "06_DR MOISES S BERTONI"               = "06_DOCTOR SANTIAGO MOISES BERTONI",
  "06_YEGROS"                            = "06_FULGENCIO YEGROS",
  "06_GRAL HIGINIO MORINIGO"             = "06_GENERAL HIGINIO MORINIGO",
  "08_SAN JUAN BAUTISTA DE LAS MISIONES" = "08_SAN JUAN BAUTISTA",
  "09_CABALLERO"                         = "09_GENERAL BERNARDINO CABALLERO",
  "09_ROQUE GONZALEZ DE SANTA CRUZ"      = "09_SAN ROQUE GONZALEZ DE SANTACRUZ",
  "10_DR RAUL PENA"                      = "10_DOCTOR RAUL PENA",
  "10_JUAN E OLEARY"                     = "10_JUAN EMILIO OLEARY",
  "11_J AUGUSTO SALDIVAR"                = "11_JULIAN AUGUSTO SALDIVAR",
  "12_GUAZUCUA"                          = "12_GUAZU CUA",
  "12_YEGROS"                            = "12_FULGENCIO YEGROS",
  "12_GRAL JOSE EDUVIGIS DIAZ"           = "12_GENERAL JOSE EDUVIGIS DIAZ",
  "14_YBY PYTA"                          = "14_YBYPYTA",
  "14_YBYRAROBANA"                       = "14_YBYRAROVANA",
  "14_LA PALOMA DEL ESPIRITU SANTO"      = "14_LA PALOMA",
  "14_VILLA CURUGUATY"                   = "14_SAN ISIDRO DEL CURUGUATY",
  "10_DR JUAN LEON MALLORQUIN"           = "10_DOCTOR JUAN LEON MALLORQUIN",
  "15_TTE 1 MANUEL IRALA FERNANDEZ"      = "15_TENIENTE IRALA FERNANDEZ"
)
districts <- districts %>%
  mutate(match_key = ifelse(match_key %in% names(key_fixes),
    key_fixes[match_key], match_key
  ))

# Fixing San Juan (Ñeembucú) using pattern (handles the 'Ñ' case) --------------
districts <- districts %>%
  mutate(match_key = ifelse(
    grepl("SAN JUAN BAUTISTA DE", DIST_DESC_) & DPTO == "12",
    "12_SAN JUAN BAUTISTA",
    match_key
  ))

# ------------------------------------------------------------------------------
# STEP 3: Bridge from name to district_id
# ------------------------------------------------------------------------------

# analytical_data links each cleaned name to its official district_id.

bridge <- analytical_data %>%
  distinct(district_id, district, department_id) %>%
  mutate(
    dep       = str_pad(department_id, 2, pad = "0"),
    name_key  = clean_name(district),
    match_key = paste(dep, name_key, sep = "_")
  ) %>%
  select(match_key, district_id)

districts <- districts %>%
  left_join(bridge, by = "match_key")

# Treatment of districts created after 2021 elections --------------------------

# In 2021, Nueva Asunción and Itacuá territories belonged to Villa Hayes and
# San Alfredo. I assign them to their parent district_id.

districts <- districts %>%
  mutate(
    district_id = case_when(
      grepl("NUEVA ASUNCI", DIST_DESC_) ~ "15_0",
      grepl("ITACU", DIST_DESC_) & DPTO == "01" ~ "1_8",
      TRUE ~ district_id
    )
  )

# ------------------------------------------------------------------------------
# STEP 4: Applying the crosswalk to get the 246 harmonised units
# ------------------------------------------------------------------------------

# Same 11 clusters as in 02_clean_data.R; re-declared to harmonise the maps.

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

districts <- districts %>%
  left_join(crosswalk_main, by = c("district_id" = "member_id")) %>%
  mutate(unit = coalesce(cluster_id, district_id))

# ------------------------------------------------------------------------------
# STEP 5: Merging the polygons that belong to the same unit
# ------------------------------------------------------------------------------

districts_h <- districts %>%
  group_by(unit) %>%
  summarise(.groups = "drop")

# ==============================================================================
# MAP 1: Change in youth turnout (2021 - 2015)
# ==============================================================================

# turnout in each year, then the difference per unit

turnout_15 <- panel_main %>%
  filter(year == 2015) %>%
  select(unit, turnout_2015 = youth_turnout_18to29)
turnout_21 <- panel_main %>%
  filter(year == 2021) %>%
  select(unit, turnout_2021 = youth_turnout_18to29)
change_df <- turnout_15 %>%
  left_join(turnout_21, by = "unit") %>%
  mutate(change = turnout_2021 - turnout_2015)
map_change <- districts_h %>%
  left_join(change_df, by = "unit")
cat("Change map - unmatched:", sum(is.na(map_change$change)), "\n")

map_plot_change <- ggplot(map_change) +
  geom_sf(aes(fill = change), color = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    name = "Change in\nyouth turnout\n(2015 to 2021)",
    low = "#B30000", # red = decrease
    mid = "#FFFFFF", # white = no change
    high = "#0038A8", # blue = increase
    midpoint = 0,
    limits = c(-0.28, 0.28),
    labels = scales::percent_format(accuracy = 1),
    na.value = "grey90"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(color = "black")
  )

ggsave("output/figures/fig_3_turnout_change.png",
  map_plot_change,
  width = 8, height = 8, dpi = 300, bg = "white"
)

# ==============================================================================
# MAP 2: Young-candidate share (2015)
# ==============================================================================

data_2015 <- panel_main %>%
  filter(year == 2015) %>%
  select(unit, youth_cand_share)
map_2015 <- districts_h %>%
  left_join(data_2015, by = "unit")
cat(
  "Candidate share 2015 - unmatched:",
  sum(is.na(map_2015$youth_cand_share)), "\n"
)

map_plot_2015 <- ggplot(map_2015) +
  geom_sf(aes(fill = youth_cand_share), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    name = "Young candidate\nshare (2015)",
    low = "#B3C6E7", high = "#0038A8",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.6), na.value = "grey90"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(color = "black")
  )

ggsave("output/figures/fig_d1_cand_share_2015.png",
  map_plot_2015,
  width = 8, height = 8, dpi = 300, bg = "white"
)

# ==============================================================================
# MAP 3: Young-candidate share (2021)
# ==============================================================================

data_2021 <- panel_main %>%
  filter(year == 2021) %>%
  select(unit, youth_cand_share)
map_2021 <- districts_h %>%
  left_join(data_2021, by = "unit")
cat(
  "Candidate share 2021 - unmatched:",
  sum(is.na(map_2021$youth_cand_share)), "\n"
)

map_plot_2021 <- ggplot(map_2021) +
  geom_sf(aes(fill = youth_cand_share), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    name = "Young candidate\nshare (2021)",
    low = "#B3C6E7", high = "#0038A8",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.6), na.value = "grey90"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(color = "black")
  )

ggsave("output/figures/fig_d1_cand_share_2021.png",
  map_plot_2021,
  width = 8, height = 8, dpi = 300, bg = "white"
)

# ==============================================================================
# MAP 4: Youth turnout (2015)
# ==============================================================================

turnout_2015 <- panel_main %>%
  filter(year == 2015) %>%
  select(unit, youth_turnout_18to29)
map_turnout_2015 <- districts_h %>%
  left_join(turnout_2015, by = "unit")
cat(
  "Youth turnout 2015 - unmatched:",
  sum(is.na(map_turnout_2015$youth_turnout_18to29)), "\n"
)

map_plot_turnout_2015 <- ggplot(map_turnout_2015) +
  geom_sf(aes(fill = youth_turnout_18to29), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    name = "Youth turnout\n(2015)",
    low = "#F5A9A9", high = "#B30000",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.3, 0.9), na.value = "grey90"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(color = "black")
  )

ggsave("output/figures/fig_d2_turnout_2015.png",
  map_plot_turnout_2015,
  width = 8, height = 8, dpi = 300, bg = "white"
)

# ==============================================================================
# MAP 5: Youth turnout (2021)
# ==============================================================================

turnout_2021 <- panel_main %>%
  filter(year == 2021) %>%
  select(unit, youth_turnout_18to29)
map_turnout_2021 <- districts_h %>%
  left_join(turnout_2021, by = "unit")
cat(
  "Youth turnout 2021 - unmatched:",
  sum(is.na(map_turnout_2021$youth_turnout_18to29)), "\n"
)

map_plot_turnout_2021 <- ggplot(map_turnout_2021) +
  geom_sf(aes(fill = youth_turnout_18to29), color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    name = "Youth turnout\n(2021)",
    low = "#F5A9A9", high = "#B30000",
    labels = scales::percent_format(accuracy = 1),
    limits = c(0.3, 0.9), na.value = "grey90"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    text = element_text(color = "black")
  )

ggsave("output/figures/fig_d2_turnout_2021.png",
  map_plot_turnout_2021,
  width = 8, height = 8, dpi = 300, bg = "white"
)

# ==============================================================================
# Highest and lowest values by year (referenced in Appendix D)
# ==============================================================================

# Young-candidate share: highest and lowest per election -----------------------

for (yr in c(2015, 2021)) {
  cat("\n=== Young-candidate share,", yr, "===\n")
  d <- panel_main %>%
    filter(year == yr) %>%
    select(district, youth_cand_share) %>%
    arrange(desc(youth_cand_share))

  cat("Highest:\n")
  print(head(d, 3))
  cat("Lowest:\n")
  print(tail(d, 3))
}

# Youth turnout: highest and lowest per year -----------------------------------

for (yr in c(2015, 2021)) {
  cat("\n=== Youth turnout,", yr, "===\n")
  d <- panel_main %>%
    filter(year == yr) %>%
    select(district, youth_turnout_18to29) %>%
    arrange(desc(youth_turnout_18to29))

  cat("Highest:\n")
  print(head(d, 3))
  cat("Lowest:\n")
  print(tail(d, 3))
}

# ============================ End of script ===================================
