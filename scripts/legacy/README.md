# Archived per-season scripts

The scripts in this folder are the pre-pipeline workflow: a new copy was made
and hand-edited for each field season (`datasheet_importer_2024.R` deleted,
`datasheet_importer_2025.R` created, etc. -- see `git log --follow`). They are
kept here for provenance and are no longer run. New seasons should use
`scripts/00_export_yearly_form_sheets.R` through `scripts/05_backfill_missing_tarajoq_coordinates.R`
instead, which take `--year=YYYY` and do not need to be copied or hand-edited.

This note records what each archived script did, where its logic landed in
the current pipeline, and what was deliberately left behind.

## datasheet_importer_2025.R

Imported Cetaceans/Environment/Biopsy field-form exports and backfilled
missing GPS from the ship track.

- **Ported:** the `"d  d"` blank-coordinate placeholder, species name
  cleanup (`nbw`/`northen bottlenose` -> canonical NBW), Excel-serial /
  slash / ISO date parsing, and the `H`/`M`/`E`-prefixed biopsy date format
  are all in `scripts/01_normalize_yearly_form_data.R`.
- **Ported and fixed:** this script read biopsy subsample ids and outcomes
  only from the newer `Subsample_B_no`/`Biopsy_outcome` columns. Older
  exports (2024 and earlier) use `Sample_no`/`Sample_success` instead, and
  the standardized 2024 biopsy table was silently losing the subsample id
  as a result (`Ha2024-10-27-dsbb01A&B` was being dropped). This is now
  fixed in `01_normalize_yearly_form_data.R`: `sample_b_id` falls back to
  `Sample_no`, and `outcome` is canonicalized to `Hit`/`Miss` from
  `Sample_success` free text (using the same `hit|tissue` keyword rule this
  script used), with the original text kept in `outcome_raw`.
- **Ported:** the duplicate-`Subsample_B_no` check (this script flagged
  duplicates to a review CSV instead of silently keeping one copy). Now
  written per year to
  `output/Sightings_QA/biopsy_duplicate_sample_ids_<year>.csv` whenever any
  are found. Re-running the pipeline surfaced a real one: 2025 has several
  `Subsample_B_no` values (e.g. `BIOP-BB-2025-9`) repeated across rows with
  slightly different timestamps -- needs a look before relying on 2025
  biopsy counts.
- **Ported as a new pipeline step:** this script's ship-track backfill
  (`backfill_coordinates()`, a floor-to-the-minute join) for sightings,
  environment, and biopsy records with no recorded position. The
  normalization layer (`01_normalize_yearly_form_data.R`) deliberately
  leaves missing coordinates as `NA`, per
  `input/normalized/README.md`'s "later, traceable enrichment step" design,
  so this is now its own step: `scripts/05_backfill_missing_tarajoq_coordinates.R`.
  It fills from the nearest ship-track fix only within `--max-gap-minutes`
  (default 30, vs. this script's effectively 0-tolerance same-minute join),
  records the gap and method for every row, and writes a separate
  `_filled.csv` plus an audit CSV rather than overwriting the standardized
  table. It is **not** wired into the compiler yet -- see
  "What was not ported" below.
- **Not ported:** the one-off hardcoded 2025 fix
  (`StartPos == "d  d" -> "66d50.77 59d11.03"`). It didn't need porting: it
  is the same record as `2025_sighting_7`, which
  `scripts/04_correct_tarajoq_form_coordinates.R` already fills from the
  explicit form note ("same position as the first environment") in a
  documented, traceable way.

## time0_2025.r

Ship-track import (TimeZero CSV + GPX), one fix per UTC minute, >12h gap QA.

- **Ported:** all of it, into `scripts/01_normalize_track_data.R` --
  headerless `TK,...` export detection, per-minute dedup, the >12h gap
  report, and the own-ship CSV/GPX combination. The year-specific
  `SURVEY_WINDOW` date filter this script needed editing every season was
  replaced with a `--min-latitude` filter (default 60°N), which needs no
  per-year editing.
- **Not ported (intentional, not a gap):** KML import and the optional UTM
  shapefile export. `01_normalize_track_data.R`'s own header comment defers
  "shapefiles and map products" to the output stage; nothing currently
  needs them. Add if a future season brings KML track data.

## baseMap_arctic_2025.R

Static ggplot basemap (bathymetry + EEZ + ship track + NBW sightings/
stations) for one season's figure.

- **Nothing to port into the normalization pipeline** -- it only consumes
  already-cleaned outputs (`output/Cetaceans/...`, `output/Track/ship_track.shp`,
  a station CSV) to draw a figure; it has no cleaning logic of its own. Its
  station DMS parsing is the same pattern already generalized in
  `01_normalize_station_data.R`.
- Archived as-is. If yearly maps are wanted again, it needs a `--year` flag
  and the hardcoded `~/CODE/shapefiles/` path fixed before reuse.

## CleanData_Arctic2023.R

An early, pre-form-app 2023 script: ship track cleanup, Tarajoq Cetaceans
sightings, and a separate Lightroom "List View" sightings export, joined
into one 2023 sightings file.

- **Already superseded, not re-ported:** its single-record coordinate fix
  (Nov 10, 2023, filled from station notes) and its per-record Min/Best/Max
  fixes are the same underlying problem already resolved, with more rigor
  and full documentation, by the 2023 field-note reconciliation recorded in
  `output/Sightings_QA/sightings_review_checklist.md` ("Review 2023 field-note
  station matches" -- status: resolved) and
  `scripts/02_normalize_2023_field_note_sightings.R`.
- **Not in scope here:** the "remove erroneous pic from the harbour on
  Nov 4, 2023" exclusion and the List View (Lightroom catalogue) sightings
  join belong to the individual photo-ID catalogue thread
  (`scripts/photoid/00_clean_arctic_lv.R` / `Arctic_NBW_PhotoID_Report.Rmd`), which is a
  different data product (resighting/association analysis) and wasn't part
  of this pass. Worth checking whether that Nov 4 exclusion is still applied
  there before trusting 2023 photo-ID counts.
- Archived as-is.

## collate_photo_id_encounters.R

Grouped one year's List View (LV) photo export into 30-minute encounters and
summarised them against that year's Leg 2 sightings file.

- **Could no longer run:** its inputs (`input/data/<year>/LV_<year>.csv`,
  `input/sightings/derived/CetaceanSightings_leg2_<year>.csv`) are folders
  from before the `input/orig_data/<year>/` reorganisation, and nothing read
  its `output/PhotoID/photo_id_<year>/` output. The 2022 and 2024 outputs it
  left are kept under `output/PhotoID/archive/legacy_2026-11-16/`.
- **Replaced by:** `scripts/02_normalize_lv_encounters.R`, which does the
  encounter grouping for every year with an export under
  `input/orig_data/<year>/LV/` (the folder formerly named `photo_id/`), and
  `scripts/09_compare_lv_encounters_to_cetaceans.R`, which compares those
  encounters with the compiled Cetaceans sightings and lists the ones with
  no sighting within 1 hour and 5 km.
- **Not ported:** the per-year encounter map and the "late November" date
  summary, which were specific to the 2022 Leg 2 review.

## Outstanding from this pass

- `output/Sightings_QA/biopsy_duplicate_sample_ids_2025.csv` -- real
  duplicate `Subsample_B_no` values in 2025, needs review (see above).
- ~~Decide whether the compiler should use the `_filled` files.~~ Done
  2026-11-17: `03_compile_normalized_tarajoq_sightings.R` now compiles the
  `_filled` form file when present, after the 2023 form times were found to
  be local time (UTC-2) and corrected in `01_normalize_yearly_form_data.R`.
