# Author: Laura Joan Feyrer
# Date created: 2026-11-16
# Date updated: 2026-11-17
# Description: TODO - describe what this script does
#
# !/usr/bin/env Rscript

# Export field-form sheets from the original yearly Excel files.
#
# This is the reusable first step of the yearly workflow. It does not alter the
# original workbooks. It reads all matching workbooks, combines each selected
# sheet in memory, removes exact duplicate rows, and writes one compiled CSV
# under input/orig_data/<year>/forms_exports/<sheet>/.
# GPS processing is deliberately separate and optional.
#
# --year=all (the default) runs every year that has at least one
# forms_original/formdat*.xlsx file -- i.e. every digital-tablet-form year.
# Years with no formdat*.xlsx (paper-form years like 2017, NAW-only years)
# are not discovered by this script and are never silently skipped: there is
# simply nothing here for them to export. A problem with one year in --all
# mode is reported and that year is skipped rather than stopping every other
# year's export; --year=YYYY for one specific year still stops on error.
#
# Examples:
#   Rscript scripts/export_yearly_form_sheets.R --year=all
#   Rscript scripts/export_yearly_form_sheets.R --year 2023
#   Rscript scripts/export_yearly_form_sheets.R --year 2024 \
#     --sheets Cetaceans,Environment,Biopsy

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
  library(dplyr)
  library(here)
})

source(here::here("scripts", "lib", "helpers.R"))

year_arg <- get_arg("year", "all")
project_dir <- here::here()

sheets <- strsplit(
  get_arg("sheets", "Cetaceans,Environment,Biopsy"),
  ",",
  fixed = TRUE
)[[1]] |>
  trimws()
sheets <- sheets[nzchar(sheets)]

# Years with at least one forms_original/formdat*.xlsx file, i.e. years that
# actually used the digital tablet form (as opposed to a paper-form year or a
# year with no Tarajoq forms at all).
discover_tablet_form_years <- function() {
  orig_root <- file.path(project_dir, "input", "orig_data")
  candidate_years <- list.dirs(orig_root, recursive = FALSE, full.names = FALSE)
  candidate_years <- candidate_years[grepl("^[0-9]{4}$", candidate_years)]
  has_formdat <- vapply(candidate_years, function(y) {
    length(list.files(
      file.path(orig_root, y, "forms_original"),
      pattern = "^formdat.*[.]xlsx$", recursive = TRUE, ignore.case = TRUE
    )) > 0
  }, logical(1))
  sort(candidate_years[has_formdat])
}

if (identical(year_arg, "all")) {
  years <- discover_tablet_form_years()
  if (!length(years)) stop("No years with forms_original/formdat*.xlsx found under input/orig_data")
} else {
  if (!grepl("^[0-9]{4}$", year_arg)) stop("Provide a four-digit year or --year=all, e.g. --year=2023")
  years <- year_arg
}

export_year <- function(year) {
  original_dir <- file.path(project_dir, "input", "orig_data", year, "forms_original")
  export_dir <- file.path(project_dir, "input", "orig_data", year, "forms_exports")

  if (!dir.exists(original_dir)) {
    stop("Original forms folder does not exist: ", original_dir)
  }

  # Field exports use formdat*.xlsx. Codebooks and form-definition workbooks
  # are intentionally excluded so they are not mistaken for observations.
  workbooks <- list.files(
    original_dir,
    pattern = "^formdat.*[.]xlsx$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  workbooks <- workbooks[!grepl("(^|/)(~\\$|forms_code)(/|$)", workbooks)]

  # Keep only field workbooks for the requested year. This prevents legacy
  # files that happen to be stored in the same yearly folder from being
  # re-exported.
  year_code <- substr(year, 3, 4)
  workbooks <- workbooks[grepl(
    paste0("^formdat", year_code),
    basename(workbooks),
    ignore.case = TRUE
  )]

  if (length(workbooks) == 0) {
    stop("No formdat*.xlsx files found under: ", original_dir)
  }

  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  summary_rows <- list()
  sheet_records <- list()
  selected_sheet_count <- 0L

  for (workbook in sort(workbooks)) {
    available <- readxl::excel_sheets(workbook)
    selected <- intersect(sheets, available)

    if (length(selected) == 0) {
      message("Skipping (none of the requested sheets found): ", basename(workbook))
      next
    }

    for (sheet in selected) {
      # Read all cells as text so Excel datetime cells and text-formatted dates
      # can be combined safely across workbooks with different schemas.
      data <- readxl::read_xlsx(
        workbook,
        sheet = sheet,
        col_types = "text",
        .name_repair = "unique"
      )
      data <- data %>% filter(if_any(everything(), ~ !is.na(.x) & as.character(.x) != ""))

      # Keep every source workbook. Files with the same formdat stem may contain
      # additional rows, so deduplication happens only after all copies are read.
      sheet_records[[sheet]] <- append(
        sheet_records[[sheet]],
        list(data %>% mutate(
          .source_workbook = basename(workbook),
          .source_row = seq_len(nrow(data))
        ))
      )

      selected_sheet_count <- selected_sheet_count + 1L
      message("Read ", basename(workbook), " / ", sheet, " (", nrow(data), " rows)")
    }
  }

  if (selected_sheet_count == 0L) {
    stop("No requested sheets were found in the field workbooks.")
  }

  audit_dir <- file.path(project_dir, "output", "Sightings_QA")
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

  # Compile each requested sheet across all source workbooks, then remove only
  # exact duplicate rows. Source workbook and row numbers are retained so that a
  # record can always be traced back to the original workbook.
  for (sheet in names(sheet_records)) {
    records <- bind_rows(sheet_records[[sheet]])
    provenance_cols <- c(".source_workbook", ".source_row")
    data_cols <- setdiff(names(records), provenance_cols)

    compiled <- records %>%
      group_by(across(all_of(data_cols))) %>%
      summarise(
        source_workbook = paste(unique(.source_workbook), collapse = "; "),
        source_rows = paste(unique(.source_row), collapse = "; "),
        .groups = "drop"
      )

    compiled_file <- file.path(
      export_dir, sheet,
      paste0(tolower(sheet), "_", year, "_compiled_deduplicated.csv")
    )
    write_csv(compiled, compiled_file, na = "")

    summary_rows[[length(summary_rows) + 1L]] <- tibble(
      year = year,
      stage = "compiled_deduplicated",
      source_workbook = paste(unique(records$.source_workbook), collapse = "; "),
      source_sheet = sheet,
      output_file = normalizePath(compiled_file, winslash = "/", mustWork = FALSE),
      rows_written = nrow(compiled),
      columns_written = ncol(compiled)
    )
    message(
      "Compiled ", sheet, ": ", nrow(records),
      " source rows -> ", nrow(compiled), " unique rows"
    )
  }

  audit_file <- file.path(audit_dir, paste0("form_sheet_export_audit_", year, ".csv"))
  write_csv(bind_rows(summary_rows), audit_file, na = "")

  message("Wrote compiled, exact-deduplicated sheets for ", year, ".")
  message("Audit written to: ", audit_file)
}

if (identical(year_arg, "all")) {
  for (year in years) {
    tryCatch(
      export_year(year),
      error = function(e) message("Skipping ", year, ": ", conditionMessage(e))
    )
  }
} else {
  export_year(years)
}
