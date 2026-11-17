# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: Shared helpers for the Arctic NBW normalization/compilation/QA pipeline.
# Sourced by scripts/00_export_yearly_form_sheets.R through
# scripts/07_crossref_compiled_against_reference_dbs.R (all pipeline stages
# except 02_normalize_2023_field_note_sightings.R, which doesn't need these),
# by scripts/QA/qa_*.R (all except qa_coordinate_note_review.R), and by
# scripts/collate_photo_id_encounters.R.
# Keep this file dependency-light (readr/dplyr only) and free of
# project-specific logic -- only genuinely shared, source-agnostic
# text/number/geometry utilities belong here.

# ---- CLI ARGUMENTS --------------------------------------------------------

# Reads a --name=value command line argument. Returns `default` when the
# flag is absent.
get_arg <- function(name, default = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0) {
    return(default)
  }
  sub(prefix, "", hit[[1]], fixed = TRUE)
}

# ---- TEXT / NUMBER ---------------------------------------------------------

# Trims text and converts common missing-value placeholders (including the
# "d d" / "d  d" unset-coordinate placeholder used in several form exports)
# to NA.
na_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "na", "n/a", "d  d", "d d", "#N/A")] <- NA_character_
  x
}

as_num <- function(x) {
  x <- na_text(x)
  suppressWarnings(readr::parse_number(x, na = c("", "NA", "N/A")))
}

# Returns the first candidate column that exists in `data`, by name order.
get_col <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  na_text(data[[hit[[1]]]])
}

# Like get_col(), but when several candidate columns exist, picks the first
# one that actually has at least one non-missing value instead of just the
# first one present.
get_col_best <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0) {
    return(rep(NA_character_, nrow(data)))
  }
  values <- lapply(hit, function(name) na_text(data[[name]]))
  usable <- vapply(values, function(x) any(!is.na(x)), logical(1))
  if (any(usable)) values[[which(usable)[1]]] else values[[1]]
}

# ---- GEOMETRY ---------------------------------------------------------------

# Great-circle distance in meters (WGS84-like spherical Earth radius).
haversine_m <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  phi1 <- lat1 * rad
  phi2 <- lat2 * rad
  dphi <- (lat2 - lat1) * rad
  dlambda <- (lon2 - lon1) * rad
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  6371008.8 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

# ---- COMPILED/DEDUPLICATED OUTPUT -------------------------------------------
# Shared by the compile-style scripts (03_compile_normalized_tarajoq_sightings.R,
# 06_merge_annual_sightings.R), which each compute a `dedup_group_id` and
# `retained` column the same way: when nothing was flagged as a duplicate for
# a year, write one "<prefix>_<year>_compiled_deduplicated.csv" instead of two
# identical-looking files; when something was removed, write the full
# "<prefix>_<year>_compiled.csv" audit trail alongside the retained-only
# "<prefix>_<year>_deduplicated.csv".

write_compiled_dedup <- function(data, dir, prefix, year) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (all(is.na(data$dedup_group_id))) {
    readr::write_csv(data, file.path(dir, paste0(prefix, "_", year, "_compiled_deduplicated.csv")), na = "")
  } else {
    readr::write_csv(data, file.path(dir, paste0(prefix, "_", year, "_compiled.csv")), na = "")
    readr::write_csv(
      dplyr::filter(data, retained),
      file.path(dir, paste0(prefix, "_", year, "_deduplicated.csv")),
      na = ""
    )
  }
}

# Finds whichever of the two possible outputs of write_compiled_dedup() exists
# for one year: the single compiled_deduplicated file, or (when duplicates
# were found and removed) the deduplicated file. Returns NA_character_ if
# neither exists.
resolve_dedup_file <- function(dir, prefix, year) {
  candidates <- file.path(dir, paste0(
    prefix, "_", year, c("_compiled_deduplicated.csv", "_deduplicated.csv")
  ))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

# ---- ENCOUNTER GROUPING -----------------------------------------------------

# Assigns a sequential encounter number to events sorted ascending by
# `datetime`, starting a new encounter whenever the gap since the previous
# event exceeds `gap_minutes` (an NA gap, including at the first event,
# also starts a new encounter).
group_into_encounters <- function(datetime, gap_minutes = 30) {
  gap <- as.numeric(difftime(datetime, dplyr::lag(datetime), units = "mins"))
  new_encounter <- seq_along(datetime) == 1 | gap > gap_minutes
  cumsum(new_encounter)
}
