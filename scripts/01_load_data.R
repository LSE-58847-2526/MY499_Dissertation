# ==============================================================================
# 01_load_data.R
#
# London School of Economics and Political Science - Department of Methodology
# MSc in Social Research Methods
# MY499 Dissertation - "Seeing themselves on the ballot"
# Candidate number: 58847
#
# Purpose: Import and validate raw TSJE electoral databases
# ==============================================================================

# Defining the project root ----------------------------------------------------

here::i_am("scripts/01_load_data.R")

# Loading packages -------------------------------------------------------------

library(tidyverse)   # data management 
library(here)        # data portability
library(janitor)     # data cleaning

# Path to each file ------------------------------------------------------------

register_file <- here(
  "data", "raw_data",
  "08.-Padron-particip-abstenc-mesa-1996-a-2023-actualizado.csv"
)
candidates_file <- here(
  "data", "raw_data",
  "candidatos-1998-a-2023-municipales-y-generales.csv"
)

# Encoding ---------------------------------------------------------------------

# Raw TSJE files are encoded in Latin1. Files are imported using the 
# corresponding locale to ensure that Spanish and Guaraní characters such
# "ñ, á, é, í, ó, ú" are read correctly.

# Validating input files -------------------------------------------------------

stopifnot(
  file.exists(register_file),
  file.exists(candidates_file)
)

# Read the electoral register database -----------------------------------------

register_raw <- read_csv2(
  register_file, locale = locale(encoding = "Latin1"))

# Read the candidates database -------------------------------------------------

candidates_raw <- read_csv2(
  candidates_file, locale = locale(encoding = "Latin1"))

# Validating data import -------------------------------------------------------

stopifnot(
  nrow(problems(register_raw)) == 0,
  nrow(problems(candidates_raw)) == 0,
  sum(duplicated(register_raw)) == 0,
  sum(duplicated(candidates_raw)) == 0
)

# ============================ End of script ===================================