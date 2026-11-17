#!/usr/bin/env Rscript

# Author: Laura Joan Feyrer
# Date created: 2026-11-17
# Date updated: 2026-11-17
# Description: Runs the full NBW sightings pipeline, in dependency order,
# from the original input files through to the merged sightings output,
# the cross-reference against external databases, and the maps.
#
# Each step runs as its own `Rscript` process rather than being source()'d
# into this one -- several of the scripts define same-named top-level
# helpers/variables (parse_datetime, years, etc.), so sourcing them all into
# one shared session would let those collide. This mirrors exactly how each
# script is already run by hand.
#
# Run from the repository root: Rscript scripts/run_pipeline.R
# Stops immediately if any required step fails. The few purely-QA/review
# steps at the end (which nothing downstream reads) are run but do not stop
# the pipeline if they fail -- a problem there is worth knowing about, not
# worth losing the rest of the run over.
#
# Every step discovers its own years, so there is nothing to update here
# when a new season's data arrives.

run <- function(script, ..., required = TRUE) {
  args <- c(...)
  message("\n==> ", script, if (length(args)) paste0(" ", paste(args, collapse = " ")) else "")
  status <- system2("Rscript", c(script, args))
  if (status != 0) {
    msg <- paste0(script, " failed (exit ", status, ")")
    if (required) stop(msg) else message("Continuing despite failure: ", msg)
  }
}

# ---- 00/01: digital tablet form years --------------------------------------
run("scripts/00_export_yearly_form_sheets.R")
run("scripts/01_normalize_yearly_form_data.R")
run("scripts/01_normalize_station_data.R")
run("scripts/01_normalize_track_data.R")

# ---- 02: everything else that feeds a year's normalized sightings ----------
# Order among these doesn't matter to each other, only that 01_normalize_
# station_data.R (above) runs first for the two that look up station fixes.
run("scripts/02_normalize_2017_paper_form_data.R")
run("scripts/02_normalize_naw_sightings.R")
run("scripts/02b_normalize_naw_tagbiopsy_sightings.R")
run("scripts/02_normalize_tarajoq_nonform_sightings.R")
run("scripts/02_normalize_2023_field_note_sightings.R")
run("scripts/02_normalize_2023_leg2_photoid.R")

# ---- 04/05: corrections and ship-track backfill on the Tarajoq form table ----
run("scripts/04_correct_tarajoq_form_coordinates.R")
run("scripts/05_backfill_missing_tarajoq_coordinates.R")

# ---- 03: compile + dedup Tarajoq sightings ----------------------------------
run("scripts/03_compile_normalized_tarajoq_sightings.R")

# ---- 06: merge Tarajoq + NAW sightings, draw the maps -----------------------
run("scripts/06_merge_annual_sightings.R")

# ---- 07: cross-reference the merge against external reference databases ----
run("scripts/07_crossref_compiled_against_reference_dbs.R")

# ---- 08: compile the Environment/Biopsy tables ------------------------------
run("scripts/08_compile_environment_biopsy.R")

# ---- 09 + QA: review-only, nothing downstream depends on these -------------
run("scripts/02_normalize_lv_encounters.R", required = FALSE)
run("scripts/09_compare_lv_encounters_to_cetaceans.R", required = FALSE)
run("scripts/QA/qa_cetaceans_vs_normalized.R", required = FALSE)
run("scripts/QA/qa_coordinate_note_review.R", required = FALSE)
run("scripts/QA/qa_sightings_source_coverage.R", required = FALSE)
run("scripts/QA/qa_tarajoq_compiled_vs_original.R", required = FALSE)

message("\nPipeline complete.")
