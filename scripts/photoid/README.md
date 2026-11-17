# Arctic Northern Bottlenose Whale Photo-ID Catalogue Analysis

**Author:** Laura Joan Feyrer
**Date Created:** 2026-04-29
**Last Updated:** 2026-11-16

## Overview

This workflow analyzes the Arctic Northern Bottlenose Whale photo-identification catalogue, focusing on resighting patterns, spatial distribution, and individual associations. The analysis distinguishes between the reliable period (2017 onwards) and earlier datasets (ASO and LAB, 2003-2016), which are out of scope for this analysis.

This is a specialized, Arctic-only branch of catalogue cleaning that is also done -- more thoroughly, for every photo-ID catalogue, not just Arctic -- in the separate `~/CODE/PhotoID` project (`01_clean_lv_2025.R` -> `02_QA_Sex_determination.r` -> `03_finalize_LV.r`). The two have already diverged; this analysis re-derives its own cleaning from a separate Arctic-only export rather than consuming that project's finalized output. Folding the two together (or moving this analysis into `~/CODE/PhotoID`, or a new project) is a bigger change, deliberately deferred -- likely worth revisiting once this data also feeds a mark-recapture analysis.

## Files Included

1. **00_clean_arctic_lv.R** - Step 1: cleaning, quality rating, encounter grouping
2. **01_analyze_arctic_lv_resights.R** - Step 2: resighting and within-day association analysis
3. **02_map_arctic_lv_sightings.R** - Saves one map of all catalogue photo locations to `output/FIGS/arctic_photoid_sightings_map.png`, without knitting the report
4. **Arctic_NBW_PhotoID_Report.Rmd** - RMarkdown report with tables and maps
5. **README.md** - This file

The report and the map script draw on `scripts/lib/arctic_basemap.R` (bathymetry, marine refuges, EEZ, land on a fixed extent), which the single-panel field sightings map in `scripts/06_merge_annual_sightings.R` also uses, so the photo-ID and field sightings maps line up.

Cleaning is split from the resight/association analysis so the catalogue can be re-cleaned (e.g. after a new LV export) without recomputing everything downstream by hand, and so the resight/association/mapping piece can move on its own if this analysis is ever relocated.

## Requirements

### R Packages

Install required packages using:

```r
install.packages("pacman")
pacman::p_load(dplyr, here, tidyverse, stringr, readr, lubridate, sf,
               rnaturalearth, rnaturalearthdata, viridis, ggspatial,
               purrr, knitr, kableExtra, patchwork, igraph, ggraph, tidygraph)
```

Both R scripts also source `scripts/lib/helpers.R` for the shared 30-minute encounter-grouping rule and haversine distance function used elsewhere in this repo.

### Input Data

Place the CSV export in the project's `INPUT` folder (this is the same folder as `input/` -- macOS treats the two names as identical):
- `input/catalogue_files/LV_Arctic_2017_2024.csv`

Or modify the file path near the top of `00_clean_arctic_lv.R`.

## Workflow

### Step 1: Run the Cleaning Script

```r
source("00_clean_arctic_lv.R")
```

This script will:
- Clean and standardize ID, date, and location variables
- Classify photo quality (star ratings) and side (Left/Right)
- Group photos into encounters (30-minute windows)
- Identify quality IDs (3*+ photos)
- Save cleaned data to `output/PhotoID/`

### Step 2: Run the Resight/Association Analysis

```r
source("01_analyze_arctic_lv_resights.R")
```

This script will:
- Build the per-encounter sightings table
- Calculate all resighting pairs (within-day, within-month, within-year, between-year)
- Calculate distances and time intervals between resightings
- Identify within-day associations (IDs seen together)
- Save processed data to `output/PhotoID/`

### Step 3: Generate the Report

Option A - In RStudio:
1. Open `Arctic_NBW_PhotoID_Report.Rmd`
2. Click "Knit" button
3. HTML report will be generated in the same directory

Option B - From R console:
```r
rmarkdown::render("Arctic_NBW_PhotoID_Report.Rmd")
```

Requires `pandoc` for the HTML render; `knitr::purl()` + `source()` on the result will run all the report's R code without it, useful for checking the analysis still runs after a change.

The report includes:
- Overall catalogue statistics
- Quality IDs by year
- Resighting analysis (within-day, within-month, within-year, between-year)
- Spatial maps (all sightings, by year, resight locations)
- Within-day associations (including network graphs)
- Summary statistics

## Output Files

Both R scripts write to `output/PhotoID/`. The yearly LV photo encounters compared against the Cetaceans sightings are a separate product under `output/LV_encounters/` (`scripts/02_normalize_lv_encounters.R`, `scripts/09_compare_lv_encounters_to_cetaceans.R`), not part of this catalogue analysis.

### Data Files (.rds and .csv)
- `LV_ARC_encounters.rds` / `LV_ARC_reliable.rds` - Cleaned dataset with encounter grouping (step 1)
- `quality_ids_arctic.rds` - Summary of IDs with 3*+ photos (step 1)
- `quality_ids_by_year.csv` - Annual summary of quality IDs (step 1)
- `sightings_arctic.rds` - Encounter-level sighting data (step 2)
- `resights_encounter_level.rds` - All pairwise resights, full encounter-level detail (step 2)
- `resights_categorized.rds` - Date-level deduplicated resights, for summary tables (step 2)
- `within_day_resights.csv`, `between_day_resights.csv`, `between_week_resights.csv`, `between_year_resights.csv` (step 2)
- `association_frequency.csv` - IDs seen together (step 2)

### Report
- `Arctic_NBW_PhotoID_Report.html` - Interactive HTML report with maps and tables

Set the `ARCTIC_PHOTOID_DATA_DIR` environment variable to point the report at a different data folder, and `ARCTIC_BASEMAP_DIR` to point its maps at a different shapefile folder than the default `~/CODE/shapefiles`.

## Key Analytical Features

### Encounter Grouping
Photos are grouped into "encounters" using a 30-minute threshold (the same rule, and the same `group_into_encounters()` helper, used elsewhere in this repo for ship-based photo-ID encounters). Photos of the same ID taken within 30 minutes are considered the same encounter. This allows differentiation between:
- Multiple photos of the same encounter (expected)
- Multiple encounters of the same individual on the same day (interesting!)

### Resighting Categories
- **Within-Day:** Different encounters on the same day
- **Within-Month:** Within about 4 weeks
- **Within-Year:** Within about a year
- **Between-Year:** More than about a year apart

### Distance Calculations
For resights with GPS coordinates, `01_analyze_arctic_lv_resights.R` calculates:
- Straight-line distance between sighting locations (km), via the shared `haversine_m()` helper
- Time between sightings (hours for within-day, days for longer intervals)

### Associations
IDs photographed in the same encounter (within 30 minutes) are considered "associated" for that encounter.

## Data Quality Notes

### Reliable Period (2017-2024)
Years 2017 onwards have consistent effort and photo quality. These are the most reliable for resighting analysis.

### Early Period (2003-2016)
Not currently included in this analysis -- `00_clean_arctic_lv.R` filters to `YEAR >= 2017`. The ASO (2004-2017, at-sea observer) and LAB (2003, Labrador field season) datasets have lower photo quality and inconsistent effort.

### Quality Thresholds
- **3*** photos are considered reliable for identification
- **4**** photos are highest quality
- Analysis focuses on IDs with at least one 3*+ photo

## Customization

### Adjust Encounter Threshold
Change the `gap_minutes` argument to `group_into_encounters()` in `00_clean_arctic_lv.R` (default 30).

### Adjust Reliable Year
Change `YEAR_START` near the top of `00_clean_arctic_lv.R` (default 2017).

### Modify Maps
Map extent is fixed (`xlims`/`ylims` in the Rmd's `map-prep` chunk) rather than computed from the data, so it stays stable across report re-runs; edit those two vectors to change it.

## Troubleshooting

### "File not found" / "Missing cleaned data" errors
- Check that the input CSV is at `input/catalogue_files/LV_Arctic_2017_2024.csv`
- Run the scripts in order: `00_clean_arctic_lv.R`, then `01_analyze_arctic_lv_resights.R`, then knit the Rmd
- Make sure the `here` package is properly initialized (open the project via `Arctic.Rproj`)

### No resights found
- This can happen if IDs don't have multiple sightings
- Check that encounter grouping is working (review `LV_ARC_encounters.rds`)
- Verify date/time parsing succeeded

### Map rendering issues
- Ensure `sf`, `terra`, and `rnaturalearth` packages are installed
- Confirm the shapefiles under `ARCTIC_BASEMAP_DIR` (default `~/CODE/shapefiles`) exist: GEBCO bathymetry, Canada EEZ, and world coastline

## Contact

For questions about the analysis or to report issues, contact Laura Joan Feyrer.
