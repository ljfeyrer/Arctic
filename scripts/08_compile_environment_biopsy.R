#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: Compile the normalized yearly Environment and Biopsy tables
# into one multi-year file per data type. Unlike the sightings side, each
# year has exactly one source (the Tarajoq form export) for Environment and
# Biopsy, so there is no cross-source duplicate to resolve here -- this is a
# simple stack across years, not a compile/dedup pair like
# write_compiled_dedup() produces for sightings.
#
# Biopsy keeps both Hit and Miss records (with an outcome column) rather
# than filtering to Hit-only the way the old per-season script did; filter
# at analysis time if only successful biopsies are wanted.
#
# This step must run after 01_normalize_yearly_form_data.R.
#
# Inputs (never modified):
#   input/normalized/<year>/environment/environment_<year>_standardized.csv
#   input/normalized/<year>/biopsy/biopsy_<year>_standardized.csv
#
# Outputs:
#   output/Environment/environment_compiled.csv
#   output/Biopsy/biopsy_compiled.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

# No --year argument: unlike the per-year sightings outputs, this script's
# output is always the single file covering every available year, so
# restricting to one year would silently overwrite the multi-year compiled
# file with a partial one. Re-run with no arguments whenever a year is
# added or re-normalized.
normalized_root <- "input/normalized"
years <- list.dirs(normalized_root, recursive = FALSE, full.names = FALSE) %>%
  str_subset("^[0-9]{4}$") %>%
  sort()
if (!length(years)) stop("No year folders found under ", normalized_root)

compile_type <- function(type, folder, out_dir) {
  files <- file.path(normalized_root, years, folder, paste0(folder, "_", years, "_standardized.csv"))
  present <- file.exists(files)
  if (!any(present)) {
    message(type, ": no normalized files found under input/normalized/<year>/", folder, "/")
    return(invisible())
  }
  combined <- map_dfr(files[present], read_csv, col_types = cols(.default = col_character()), show_col_types = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0(folder, "_compiled.csv"))
  write_csv(combined, out_file, na = "")
  message(
    type, ": ", sum(present), " year(s) (", paste(years[present], collapse = ", "),
    "), ", nrow(combined), " rows -> ", out_file
  )
}

compile_type("Environment", "environment", "output/Environment")
compile_type("Biopsy", "biopsy", "output/Biopsy")
