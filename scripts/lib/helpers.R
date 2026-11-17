# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-16
# Description: Shared helpers for the Arctic NBW normalization/compilation/QA pipeline.
# Sourced by scripts/00-04_*.R, scripts/QA/qa_*.R,
# scripts/crossref_compiled_nbw_dfo_db.R, and scripts/collate_photo_id_encounters.R.
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
