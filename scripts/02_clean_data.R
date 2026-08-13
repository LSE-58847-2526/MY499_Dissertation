# ==============================================================================
# 02_clean_data.R
#
# London School of Economics and Political Science - Department of Methodology
# MSc in Social Research Methods
# MY499 Dissertation - "Seeing themselves on the ballot"
# Candidate number: 58847
#
# Purpose: Clean data and build district-year level variables
# ==============================================================================

# Defining the project root ----------------------------------------------------

here::i_am("scripts/02_clean_data.R")

# ------------------------------------------------------------------------------
# STEP 1: Setting up data and project structure 
# ------------------------------------------------------------------------------

# Loading raw data -------------------------------------------------------------

source(here::here("scripts", "01_load_data.R"))

# Renaming electoral register database labels from Spanish to English ----------

register_raw <- register_raw %>%
  mutate(pad_part_abs = recode(pad_part_abs,
                               "padrón"        = "register",
                               "participación" = "participation",
                               "abstención"    = "abstention"))

register_raw <- register_raw %>%
  rename(
    r18to24fp = r18a24fp, r18to24mp = r18a24mp,
    r18to24fe = r18a24fe, r18to24me = r18a24me,
    r25to29fp = r25a29fp, r25to29mp = r25a29mp,
    r25to29fe = r25a29fe, r25to29me = r25a29me,
    electors = electores
  )

register_raw <- register_raw %>%
  rename(
    election_year   = año,
    election_type   = tipo_eleccion,
    department_id   = dep,
    department_name = depdes,
    district_id     = dis,
    district_name   = disdes,
    measure_type    = pad_part_abs
  )

# Renaming candidates database labels from Spanish to English ------------------

candidates_raw <- candidates_raw %>%
  rename(
    election_year    = año,
    election_type    = tipo_eleccion,
    candidate_office = cand_desc,
    department_id    = dep,
    department_name  = depdes,
    district_id      = dis,
    district_name    = disdes,
    candidate_status = tit_sup,
    surname          = apellido,
    first_name       = nombre,
    sex              = sexo,
    candidate_age    = edad
  )

# Creating Output folder -------------------------------------------------------

dir.create(here::here("output"), showWarnings = FALSE)

# ------------------------------------------------------------------------------
# STEP 2: Filtering electoral register data for municipal elections
# ------------------------------------------------------------------------------

# Creating a vector of municipal elections -------------------------------------

municipal_years <- c(2006, 2010, 2015, 2021)

# Filtering electoral register file by election type and year ------------------

register_municipal <- register_raw %>%
  filter(election_type == "municipales",
         election_year %in% municipal_years)       

# ------------------------------------------------------------------------------
# STEP 3: Building youth counts at district-year level
# ------------------------------------------------------------------------------

# Aggregate young electoral counts by district-year.

# The electorate includes foreign residents with permanent status, who hold
# voting rights in municipal elections (Art. 120, 1992 Constitution). Counts
# sum Paraguayan (p) and foreign (e) electors.

youth_by_district <- register_municipal %>%
  mutate(
    young_18to24 = r18to24fp + r18to24mp + r18to24fe + r18to24me,
    young_25to29 = r25to29fp + r25to29mp + r25to29fe + r25to29me,
    young_18to29 = young_18to24 + young_25to29,
    young_female_18to29 = r18to24fp + r25to29fp + r18to24fe + r25to29fe,
    young_male_18to29 = r18to24mp + r25to29mp + r18to24me + r25to29me
  ) %>%
  group_by(election_year, department_id, district_id, measure_type) %>%
  # Sum across all polling tables within each district
  summarise(
    young_18to24 = sum(young_18to24),
    young_25to29 = sum(young_25to29),
    young_18to29 = sum(young_18to29),
    young_female_18to29 = sum(young_female_18to29),
    young_male_18to29 = sum(young_male_18to29),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# STEP 4: Reshaping to wide format and computing youth turnout
# ------------------------------------------------------------------------------

# Use the pivot_wider function to reshape from long to wide format 
# so that register, participation, and abstention become separate columns
# for each district-year.

youth_turnout <- youth_by_district %>%
  pivot_wider(  
    names_from = measure_type,
    values_from = c(young_18to24, young_25to29, young_18to29,
                    young_female_18to29, young_male_18to29)
  ) %>%
  mutate(
    youth_turnout_18to29 = young_18to29_participation / young_18to29_register,
    youth_turnout_18to24 = young_18to24_participation / young_18to24_register,
    youth_turnout_25to29 = young_25to29_participation / young_25to29_register,
    youth_turnout_female = young_female_18to29_participation / 
      young_female_18to29_register,
    youth_turnout_male = young_male_18to29_participation / 
      young_male_18to29_register
  )

# ------------------------------------------------------------------------------
# STEP 5: Matching department and district codes to names
# ------------------------------------------------------------------------------

# Build a dictionary of department-district codes and names.
# Names are labels only. All analysis uses numeric IDs.

district_names <- register_municipal %>%
  count(department_id, district_id, department_name, district_name) %>%
  group_by(department_id, district_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(department_id, district_id, department_name, district_name)

# Join department and district names back to youth turnout

youth_turnout <- youth_turnout %>%
  left_join(district_names, by = c("department_id", "district_id")) %>%
  relocate(department_name, district_name, .after = district_id)

# ------------------------------------------------------------------------------
# STEP 6: Building candidate variables at district-year level
# ------------------------------------------------------------------------------

# Main independent variable: Council candidates (JUNTA MUNICIPAL)
# Robustness: Mayor candidates (INTENDENTE)

# Filter candidates: municipal elections, council, main candidates only --------

candidates_council_main <- candidates_raw %>%
  filter(
    election_type == "municipales",
    election_year %in% municipal_years,
    candidate_office == "JUNTA MUNICIPAL",
    candidate_status == 1
  )

# Recovering missing candidate ages using information provided by the TSJE -----

# Eight titular council candidates had candidate_age = 0. The birth dates were   
# obtained through a formal request to the TSJE (Law No. 5282/2014). The
# completed age of each candidate on the election date was calculated 
# from those birth dates. To avoid publishing personal birth data, they were
# matched by location (year, department, district) in the tibble below 
# rather than by name.

# Asuncion 2021 (0, 0) is the only district with two different candidates.

tsje_ages <- tibble::tribble(
  ~election_year, ~department_id, ~district_id, ~age_rank, ~reconstructed_age,
  2006L,          9L,             23L,          1L,        44L,
  2006L,          9L,             27L,          1L,        34L,
  2021L,          0L,             0L,           1L,        32L,
  2021L,          0L,             0L,           2L,        70L,
  2021L,          2L,             22L,          1L,        39L,
  2021L,          3L,             35L,          1L,        32L,
  2021L,          4L,             21L,          1L,        68L,
  2021L,          8L,             7L,           1L,        58L
)

# Verify that the tibble has 8 rows and the expected ages, and that there are
# 8 age-0 records with a sex recorded (real candidates)

stopifnot(
  nrow(tsje_ages) == 8,
  identical(
    sort(as.integer(tsje_ages$reconstructed_age)),
    sort(c(44L, 34L, 32L, 70L, 39L, 32L, 68L, 58L))
  ),
  sum(candidates_council_main$candidate_age == 0 &
        !is.na(candidates_council_main$sex), na.rm = TRUE) == 8
)

# Assign a rank to the age-0 candidate records within each district 

candidates_council_main <- candidates_council_main %>%
  group_by(election_year, department_id, district_id) %>%
  mutate(
    age_rank = if_else(
      candidate_age == 0 & !is.na(sex),
      cumsum(candidate_age == 0 & !is.na(sex)),
      NA_integer_
    )
  ) %>%
  ungroup()

# Apply the recovered ages by location + within-district rank

candidates_council_main <- candidates_council_main %>%
  left_join(
    tsje_ages,
    by = c("election_year", "department_id", "district_id", "age_rank")
  )

# All eight records must match, and all matched records must have been age 0

stopifnot(
  sum(!is.na(candidates_council_main$reconstructed_age)) == 8,
  all(
    candidates_council_main$candidate_age[
      !is.na(candidates_council_main$reconstructed_age)
    ] == 0
  )
)

# Replace age for those eight records

candidates_council_main <- candidates_council_main %>%
  mutate(
    candidate_age = if_else(
      candidate_age == 0 & !is.na(reconstructed_age),
      as.numeric(reconstructed_age),
      as.numeric(candidate_age)
    )
  ) %>%
  select(-age_rank, -reconstructed_age)

# Removing age-0 records (placeholders) before computing shares ----------------

# 163 empty rows (with no name, sex or age) in the pre-reform municipal 
# elections (2006, 2010, 2015). Before Law No. 6318/2019, party lists did not
# need a full slate of candidates, so unfilled positions appear as blank rows.
# (Appendix B)

stopifnot(
  sum(candidates_council_main$candidate_age == 0, na.rm = TRUE) == 163
)

# Drops all 163 age-zero placeholder records 

candidates_council_main <- candidates_council_main %>%
  filter(candidate_age != 0)   

# Verify that no age-zero records remain

stopifnot(
  sum(candidates_council_main$candidate_age == 0, na.rm = TRUE) == 0
)

# Verify that every council candidate must have a valid, positive age after 
# cleaning to ensure that n_candidates (count) and youth share (X) have the 
# same denominator.

stopifnot(
  !anyNA(candidates_council_main$candidate_age),
  all(candidates_council_main$candidate_age > 0)
)

# Build candidate shares at district-election level ----------------------------

candidate_shares <- candidates_council_main %>%
  mutate(
    is_young  = candidate_age <= 35,
    is_young_30 = candidate_age <= 30,
    is_female = sex == "F",
    is_young_female = (candidate_age <= 35) & (sex == "F"),
    is_young_male   = (candidate_age <= 35) & (sex == "M")
  ) %>%
  group_by(
    election_year,
    department_id,
    district_id
  ) %>%
  summarise(
    n_candidates       = n(),
    
    # Numerators (counts)
    n_young            = sum(is_young, na.rm = TRUE),
    n_young_30         = sum(is_young_30, na.rm = TRUE),
    n_young_female     = sum(is_young_female, na.rm = TRUE),
    n_young_male       = sum(is_young_male, na.rm = TRUE),
    n_female           = sum(is_female, na.rm = TRUE),
    
    # Shares (proportions)
    youth_cand_share   = mean(is_young, na.rm = TRUE),
    youth_cand_share_30 = mean(is_young_30, na.rm = TRUE),
    young_female_share = mean(is_young_female, na.rm = TRUE),
    young_male_share   = mean(is_young_male, na.rm = TRUE),
    female_cand_share  = mean(is_female, na.rm = TRUE),
    mean_cand_age      = mean(candidate_age, na.rm = TRUE),
    
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# STEP 7: Building young-candidate counts at multiple age thresholds
# ------------------------------------------------------------------------------

# Count of council candidates at or below each age cutoff (29 to 35).
# Shares are computed after territorial harmonisation.

youth_share_thresholds <- candidates_council_main %>%
  group_by(election_year, department_id, district_id) %>%
  summarise(
    n_le29 = sum(candidate_age <= 29),
    n_le30 = sum(candidate_age <= 30),
    n_le31 = sum(candidate_age <= 31),
    n_le32 = sum(candidate_age <= 32),
    n_le33 = sum(candidate_age <= 33),
    n_le34 = sum(candidate_age <= 34),
    n_le35 = sum(candidate_age <= 35),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# STEP 8: Building candidate share for mayoral elections 
# ------------------------------------------------------------------------------

candidates_mayor <- candidates_raw %>%
  filter(
    election_type == "municipales",
    election_year %in% municipal_years,
    candidate_office == "INTENDENTE",
    candidate_status == 1
  )

# Verify that mayoral candidates have no missing or zero ages

stopifnot(
  !anyNA(candidates_mayor$candidate_age),
  all(candidates_mayor$candidate_age > 0)
)

candidate_shares_mayor <- candidates_mayor %>%
  mutate(
    is_young  = candidate_age <= 35,
    is_female = sex == "F"
  ) %>%
  group_by(
    election_year,
    department_id,
    district_id
  ) %>%
  summarise(
    n_candidates      = n(),
    # count, needed for harmonisation
    n_young           = sum(is_young,  na.rm = TRUE),  
    n_female          = sum(is_female, na.rm = TRUE),
    youth_cand_share  = n_young / n_candidates,
    female_cand_share = n_female / n_candidates,
    mean_cand_age     = mean(candidate_age, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# STEP 9: Building control variables (youth share and electorate size)
# ------------------------------------------------------------------------------

youth_share <- register_municipal %>%
  filter(measure_type == "register") %>%
  group_by(
    election_year,
    department_id,
    district_id
  ) %>%
  summarise(
    total_electors = sum(electors),             # total electors 
    log_total_electors = log(sum(electors)),    # log (total electorate)
    youth_share    = sum(
      r18to24fp + r18to24mp + r25to29fp + r25to29mp +
        r18to24fe + r18to24me + r25to29fe + r25to29me) / sum(electors),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# STEP 10: Merge turnout + council candidate data
# ------------------------------------------------------------------------------

analytical_data <- youth_turnout %>%
  select(
    election_year, department_id, district_id, department_name, 
    district_name, youth_turnout_18to29, youth_turnout_female, 
    youth_turnout_male, young_18to29_register, young_18to29_participation
  ) %>%
  inner_join(
    candidate_shares,       
    by = c("election_year", "department_id", "district_id")
  ) %>%
  left_join(
    candidate_shares_mayor,  
    by = c("election_year", "department_id", "district_id"),
    suffix = c("", "_mayor")
  ) %>%
  left_join(
    youth_share,             
    by = c("election_year", "department_id", "district_id")
  ) %>%
  left_join(
    youth_share_thresholds,  
    by = c("election_year", "department_id", "district_id")
  ) %>%
  rename(
    year = election_year, 
    department = department_name, 
    district = district_name
  )

# ------------------------------------------------------------------------------
# STEP 11: Building unique district ID (for fixed effects)
# ------------------------------------------------------------------------------

# District codes repeat across departments, so department and district codes
# are combined into a single unique identifier.

analytical_data <- analytical_data %>%
  mutate(district_id = paste(department_id, district_id, sep = "_"))

# ------------------------------------------------------------------------------
# STEP 12: Fix missing and invalid turnout values (two district cases)
# ------------------------------------------------------------------------------

# Case 1. District: Nueva Esperanza (Canindeyú), year 2006.

# No municipal elections were held in this district in 2006 (TSJE, 2007, p. 59).
# It therefore fielded no candidates, and its participation figure is not a
# valid turnout value. The inner_join with candidate data removes it, so it
# does not enter the analysis.

# Verify that Nueva Esperanza in year 2006 (dep 14, dis 11) is the only
# district-election present in youth_turnout but absent from candidate_shares.

missing_candidate_data <- youth_turnout %>%
  anti_join(
    candidate_shares,
    by = c("election_year", "department_id", "district_id")
  )

stopifnot(
  nrow(missing_candidate_data) == 1,
  missing_candidate_data$election_year == 2006,
  missing_candidate_data$department_id == 14,
  missing_candidate_data$district_id   == 11
)

# Case 2. District: Francisco Caballero Álvarez (Canindeyú), year 2010.

# The district's polling tables were not processed in the register dataset.
# The election did take place but the participation count by age group 
# is not available and cannot be recovered. Coded as NA.

fca_row <- analytical_data$district_id == "14_3" & analytical_data$year == 2010
analytical_data[fca_row, c("young_18to29_participation", "youth_turnout_18to29",
                           "youth_turnout_female", "youth_turnout_male")] <- NA

# ------------------------------------------------------------------------------
# STEP 13: Building lagged youth turnout 
# ------------------------------------------------------------------------------

# A lagged-turnout control was considered but not included in the final design.

# ------------------------------------------------------------------------------
# STEP 14: Changing district names in data base
# ------------------------------------------------------------------------------

# Verify that all districts resolve to a single standardised name and that all
# merges and fixed effects use numeric IDs (district_id), so the encoding 
# choice does not affect any results.

analytical_data <- analytical_data %>%
  mutate(district = case_when(
    district == "1RO. DE MARZO" ~ "PRIMERO DE MARZO",
    district == "1°. DE MARZO" ~ "PRIMERO DE MARZO",
    district == "ABAÌ" ~ "ABAI",
    district == "BELÈN" ~ "BELEN",
    district == "BELLA  VISTA" ~ "BELLA VISTA",
    district == "CAACUPÉ" ~ "CAACUPE",
    district == "CAAZAPÀ" ~ "CAAZAPA",
    district == "CAP. M. JOSE TROCHE" ~ "CAPITAN MAURICIO JOSE TROCHE",
    district == "CAP. MAURICIO JOSE TROCHE" ~ "CAPITAN MAURICIO JOSE TROCHE",
    district == "CAPIIVARY" ~ "CAPIIBARY",
    district == "DOMINGO M. DE IRALA" ~ "DOMINGO MARTINEZ DE IRALA",
    district == "DR. BOTRELL" ~ "DOCTOR BOTRELL",
    district == "DR. CECILIO BAEZ" ~ "DOCTOR CECILIO BAEZ",
    district == "DR. J. L. MALLORQUIN" ~ "DOCTOR JUAN LEON MALLORQUIN",
    district == "DR. JUAN M. FRUTOS" ~ "DOCTOR JUAN MANUEL FRUTOS",
    district == "DR. MOISES BERTONI" ~ "DOCTOR SANTIAGO MOISES BERTONI",
    district == "DR.J. E. ESTIGARRIBIA" ~ "DOCTOR JOSE EULOGIO ESTIGARRIBIA",
    district == "DR.J. E.ESTIGARRIBIA" ~ "DOCTOR JOSE EULOGIO ESTIGARRIBIA",
    district == "DR. RAUL PEÑA" ~ "DOCTOR RAUL PEÑA",
    district == "FRANCISCO C. ALVAREZ" ~ "FRANCISCO CABALLERO ALVAREZ",
    district == "FORTIN JOSE FALCON" ~ "JOSE FALCON",
    district == "FRANCISCO S.LOPEZ" ~ "MARISCAL FRANCISCO SOLANO LOPEZ",
    district == "FRANCISCO SOLANO LOPEZ" ~ "MARISCAL FRANCISCO SOLANO LOPEZ",
    district == "GRAL. B. CABALLERO" ~ "GENERAL BERNARDINO CABALLERO",
    district == "GRAL. BERNARDINO CABALLERO" ~ "GENERAL BERNARDINO CABALLERO",
    district == "GRAL. E. A. GARAY" ~ "GENERAL EUGENIO ALEJANDRINO GARAY",
    district == "GRAL. F. RESQUIN" ~ "GENERAL FRANCISCO ISIDORO RESQUIN",
    district == "GRAL. JOSE E. DIAZ" ~ "GENERAL JOSE EDUVIGIS DIAZ",
    district == "GRAL. JOSE M. BRUGUE" ~ "GENERAL JOSE MARIA BRUGUEZ",
    district == "GRAL. JOSE M. BRUGUEZ" ~ "GENERAL JOSE MARIA BRUGUEZ",
    district == "GRAL.ELIZARDO AQUINO" ~ "GENERAL ELIZARDO AQUINO",
    district == "GRAL.MORINIGO" ~ "GENERAL HIGINIO MORINIGO",
    district == "GRAL. MORINIGO" ~ "GENERAL HIGINIO MORINIGO",
    district == "ITAC. DEL ROSARIO" ~ "ITACURUBI DEL ROSARIO",
    district == "ITAC.DE LA CORDILLER" ~ "ITACURUBI DE LA CORDILLERA",
    district == "JOSE A. FASSARDI" ~ "JOSE FASSARDI",
    district == "JUAN E. O'LEARY" ~ "JUAN EMILIO O'LEARY",
    district == "J. AUGUSTO SALDIVAR" ~ "JULIAN AUGUSTO SALDIVAR",
    district == "MARIANO R. ALONSO" ~ "MARIANO ROQUE ALONSO",
    district == "MAYOR J. MARTINEZ" ~ "MAYOR JOSE DEJESUS MARTINEZ",
    district == "MAYOR OTAÑO" ~ "MAYOR JULIO DIONISIO OTAÑO",
    district == "MCAL.ESTIGARRIBIA" ~ "MARISCAL JOSE FELIX ESTIGARRIBIA",
    district == "PEDRO J. CABALLERO" ~ "PEDRO JUAN CABALLERO",
    district == "PTO. ADELA" ~ "PUERTO ADELA",
    district == "PTO. PINASCO" ~ "PUERTO PINASCO",
    district == "R.I. 3 CORRALES" ~ "RI 3 CORRALES",
    district == "SAN ISIDRO CURUGUATY" ~ "SAN ISIDRO DEL CURUGUATY",
    district == "SAN J.DE LOS ARROYOS" ~ "SAN JOSE DE LOS ARROYOS",
    district == "SAN PEDRO DEL Y." ~ "SAN PEDRO DEL YCUAMANDIYU",
    district == "SAN RAFAEL DEL PARAN" ~ "SAN RAFAEL DEL PARANA",
    district == "SAN ROQUE GONZALEZ" ~ "SAN ROQUE GONZALEZ DE SANTACRUZ",
    district == "SGTO.JOSE FELIX LOPEZ" ~ "SARGENTO JOSE FELIX LOPEZ",
    district == "STA. ROSA DEL MONDAY" ~ "SANTA ROSA DEL MONDAY",
    district == "STA.ROSA DEL MBUTUY" ~ "SANTA ROSA DEL MBUTUY",
    district == "TTE. ESTEBAN MARTINEZ" ~ "TENIENTE ESTEBAN MARTINEZ",
    district == "TTE ESTEBAN MARTINEZ" ~ "TENIENTE ESTEBAN MARTINEZ",
    district == "TTE.IRALA FERNANDEZ" ~ "TENIENTE IRALA FERNANDEZ",
    district == "TTE. IRALA FERNANDEZ" ~ "TENIENTE IRALA FERNANDEZ",
    district == "YAVEVYRY" ~ "YABEBYRY",
    district == "YBY PYTA" ~ "YBYPYTA",
    district == "YBYYAU" ~ "YBY YAU",
    district == "YBY YA'U" ~ "YBY YAU",
    district == "YVYRAROBANA" ~ "YBYRAROBANA",
    district == "YPE JHU" ~ "YPEJHU",
    TRUE ~ district
  ))

# ------------------------------------------------------------------------------
# STEP 15: Exporting final analytical dataset
# ------------------------------------------------------------------------------

# These guards fail loudly if a future change breaks the data.

stopifnot(
  nrow(analytical_data) == 979,                           # expected rows
  n_distinct(analytical_data$district_id) == 261,         # expected districts
  all(analytical_data$youth_turnout_18to29 <= 1, na.rm = TRUE),
  all(analytical_data$youth_turnout_18to29 >= 0, na.rm = TRUE),
  all(analytical_data$youth_share <= 1, na.rm = TRUE),           
  all(analytical_data$youth_share >= 0, na.rm = TRUE),
  all(analytical_data$youth_cand_share <= 1, na.rm = TRUE),
  all(analytical_data$mean_cand_age > 0, na.rm = TRUE)    # no age-zero
)

write_csv(analytical_data, here::here("output", "analytical_data.csv"))

# ==============================================================================
# CROSSWALK: harmonising district boundaries within each comparison window
# ==============================================================================

# Some districts were created from existing ones between elections. Because the 
# design compares each district with itself over time (district FE), each unit
# must cover the same territory in both windows. Therefore, each new district
# ("child") is folded back into the district it was created from 
# ("parent"), summing count variables and recomputing rates.
#
# The windows are harmonised separately, as each one uses its own borders:
#   Main window    (2015 -> 2021): fold districts first appearing in 2021.
#   Placebo window (2006 -> 2010): fold districts first appearing in 2010.
# analytical_data (four years) is left untouched; the crosswalk produces two
# new balanced panels: panel_main and panel_placebo.

# --------------- Name lookup for harmonised units -----------------------------

# Lookup table mapping each district_id to its district and department name,
# in order to label the harmonised panels.

unit_names <- analytical_data %>%
  distinct(district_id, district, department)

# ------------------------------------------------------------------------------
# STEP 16: Defining cluster maps (member district_id -> cluster id)
# ------------------------------------------------------------------------------

# Each district is mapped to a cluster id (the id of the head district).
# Every member of the cluster shares that same cluster id, in order to group
# them together. 

# --- Main window (2015 -> 2021): 11 clusters ----------------------------------

# The cluster head (first id) is the child; its parent(s) fold into it.
# Single-parent = one parent; multi-parent = several parents.

crosswalk_main <- tribble(
  ~member_id, ~cluster_id,
  # Maracaná + San Isidro del Curuguaty (single)
  "14_12","14_12",  "14_13","14_12",
  # María Antonia + Mbuyapey (single)
  "9_12","9_12",    "9_13","9_12",
  # San Vicente Pancholo + General Francisco Isidoro Resquín (single)
  "2_22","2_22",    "2_11","2_22",
  # Arroyito + Horqueta (single)
  "1_11","1_11",    "1_3","1_11",
  # Puerto Adela + Salto del Guairá (single)
  "14_14","14_14",  "14_0","14_14",
  # Laurel + Yvyrarobana + Nueva Esperanza (multi)
  "14_20","14_20",  "14_18","14_20",  "14_11","14_20",
  # Paso Horqueta + Concepción + San Alfredo (multi)
  "1_12","1_12",    "1_0","1_12",     "1_8","1_12",
  # Cerro Corá + Pedro Juan Caballero (single)
  "13_6","13_6",    "13_0","13_6",
  # Campo Aceval + Teniente Irala Fernández (single)
  "15_2","15_2",    "15_9","15_2",
  # San José del Rosario + Villa del Rosario + General Elizardo Aquino + 
  # Antequera (multi)
  "2_33","2_33",    "2_29","2_33",    "2_9","2_33",   "2_3","2_33",
  # Boquerón + Mariscal José Félix Estigarribia (single)
  "17_4","17_4",    "17_3","17_4"
)

# --- Placebo window (2006 -> 2010): 6 clusters --------------------------------

# The cluster head (first id) is the parent; children fold into it.
# Villa Hayes is the parent of two children (15_11 and 15_4).

crosswalk_placebo <- tribble(
  ~member_id, ~cluster_id,
  # Villa Hayes (single) + Teniente Esteban Martínez + General José María Bruguez 
  "15_0","15_0",    "15_11","15_0",   "15_4","15_0",
  # Raúl Arsenio Oviedo (single) + Tembiapora 
  "5_23","5_23",    "5_34","5_23",
  # Coronel Martínez (single) + Tebicuary 
  "4_5","4_5",      "4_30","4_5",
  # Puerto Casado (single) + Carmelo Peralta 
  "16_1","16_1",    "16_2","16_1",
  # Concepción (single) + San Carlos del Apa 
  "1_0","1_0",      "1_6","1_0",
  # Horqueta (single) + Azotey 
  "1_3","1_3",      "1_2","1_3"
)

# Validate the crosswalk maps before applying the ------------------------------

# (1) No district maps to more than one cluster (disjoint clusters).
# (2) Every mapped district exists in its comparison window.
# Non-overlapping mappings ensure that no district contributes to more than one 
# harmonised territorial unit.

stopifnot(
  !anyDuplicated(crosswalk_main$member_id),
  !anyDuplicated(crosswalk_placebo$member_id),
  all(crosswalk_main$member_id %in%
        analytical_data$district_id[analytical_data$year %in% c(2015, 2021)]),
  all(crosswalk_placebo$member_id %in%
        analytical_data$district_id[analytical_data$year %in% c(2006, 2010)])
)

# ------------------------------------------------------------------------------
# STEP 17: Defining the harmonisation function
# ------------------------------------------------------------------------------

# Takes the two years of a window and a crosswalk, assigns each district to 
# its cluster (districts not in the map keep their own id), sums the count 
# variables per cluster-year, and recomputes rates/shares/logs from the summed 
# counts. Finally, keeps only balanced units (present in both window years).

# Sum counts only when all component values are observed;
# otherwise the harmonised unit is set to NA.

sum_strict <- function(x) {
  if (any(is.na(x))) NA_real_ else sum(x)
}

harmonise_window <- function(data, years, cwalk) {
  data %>%
    filter(year %in% years) %>%
    left_join(cwalk, by = c("district_id" = "member_id")) %>%
    mutate(unit = coalesce(cluster_id, district_id)) %>%
    group_by(unit, year) %>%
    summarise(
      # council candidates 
      n_young                    = sum_strict(n_young),
      n_candidates               = sum_strict(n_candidates),
      # turnout and electorate
      young_18to29_participation = sum_strict(young_18to29_participation),
      young_18to29_register      = sum_strict(young_18to29_register),
      total_electors             = sum_strict(total_electors),
      # mayoral candidates (for the placebo)
      n_young_mayor              = sum_strict(n_young_mayor),
      n_candidates_mayor         = sum_strict(n_candidates_mayor),
      # age-threshold counts
      n_le29 = sum_strict(n_le29), n_le30 = sum_strict(n_le30),
      n_le31 = sum_strict(n_le31), n_le32 = sum_strict(n_le32),
      n_le33 = sum_strict(n_le33), n_le34 = sum_strict(n_le34),
      n_le35 = sum_strict(n_le35),
      .groups = "drop"
    ) %>%
    mutate(
      youth_cand_share       = n_young / n_candidates,
      youth_turnout_18to29   = young_18to29_participation / young_18to29_register,
      youth_share            = young_18to29_register / total_electors,
      log_total_electors     = log(total_electors),
      post                   = if_else(year == max(years), 1L, 0L),
      # mayor share (recomputed from aggregated counts)
      youth_cand_share_mayor = n_young_mayor / n_candidates_mayor,
      # threshold shares (recomputed from aggregated counts)
      youth_cand_share_le29 = n_le29 / n_candidates,
      youth_cand_share_le30 = n_le30 / n_candidates,
      youth_cand_share_le31 = n_le31 / n_candidates,
      youth_cand_share_le32 = n_le32 / n_candidates,
      youth_cand_share_le33 = n_le33 / n_candidates,
      youth_cand_share_le34 = n_le34 / n_candidates,
      youth_cand_share_le35 = n_le35 / n_candidates
    ) %>%
    filter(
      is.finite(youth_cand_share),
      is.finite(youth_turnout_18to29),
      is.finite(youth_share),
      is.finite(log_total_electors)
    ) %>%
    left_join(unit_names, by = c("unit" = "district_id")) %>%
    relocate(unit, district, department, year) %>%
    group_by(unit) %>%
    filter(n_distinct(year) == 2) %>%
    ungroup()
}

# ------------------------------------------------------------------------------
# STEP 18: Building the harmonised panels
# ------------------------------------------------------------------------------

# Main model panel (2015 -> 2021): 246 units x 2 = 492 rows

panel_main <- harmonise_window(
  analytical_data, c(2015, 2021), crosswalk_main)

# Placebo panel (2006 -> 2010): 229 units x 2 = 458 rows

panel_placebo <- harmonise_window(
  analytical_data, c(2006, 2010), crosswalk_placebo)

# Verify exact panel sizes (documents the dimensions after harmonisation).
stopifnot(
  nrow(panel_main)             == 492,
  n_distinct(panel_main$unit)  == 246,
  nrow(panel_placebo)          == 458,
  n_distinct(panel_placebo$unit) == 229
)

# Verify that both panels are balanced, complete, and unique (no missing values
# or duplicated outcomes)

stopifnot(
  all(count(panel_main, unit)$n == 2),
  all(count(panel_placebo, unit)$n == 2),
  !anyNA(panel_main$youth_turnout_18to29),
  !anyNA(panel_placebo$youth_turnout_18to29),
  !anyDuplicated(panel_main[c("unit", "year")]),
  !anyDuplicated(panel_placebo[c("unit", "year")])
)

# ------------------------------------------------------------------------------
# STEP 19: Exporting harmonised panels
# ------------------------------------------------------------------------------

write_csv(panel_main,    here::here("output", "panel_main.csv"))
write_csv(panel_placebo, here::here("output", "panel_placebo.csv"))

# ============================ End of script ===================================