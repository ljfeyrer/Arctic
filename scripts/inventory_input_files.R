# Inventory original and intermediate input files before reorganizing the project.
# This script does not move, edit, or delete source files.

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(stringr))

input_root <- "input"
output_dir <- "output/Sightings_QA"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(
  input_root,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE,
  include.dirs = FALSE
)
files <- files[!dir.exists(files)]
files <- files[!grepl("[.]DS_Store$|~\\$", files, ignore.case = TRUE)]

inventory <- tibble(
  current_path = normalizePath(files, winslash = "/", mustWork = FALSE),
  relative_path = str_remove(files, paste0("^", fixed(input_root), "/?")),
  extension = str_to_lower(tools::file_ext(files)),
  size_bytes = file.info(files)$size
) %>%
  mutate(
    top_level = str_extract(relative_path, "^[^/]+"),
    year = str_extract(relative_path, "20(22|23|24|25|26)"),
    layer = case_when(
      str_detect(relative_path, "^orig_data/[0-9]{4}/forms_exports/") ~ "source_form_exports",
      str_starts(relative_path, "orig_data/") ~ "original_yearly_data",
      str_starts(relative_path, "sightings/") ~ "curated_sightings_inputs",
      str_starts(relative_path, "basemaps") ~ "reference_basemap",
      TRUE ~ "other_input"
    ),
    data_role = case_when(
      str_detect(str_to_lower(relative_path), "gps|gpx|track|time0|_trk|_wpt|_rte") ~ "track_or_navigation",
      str_detect(str_to_lower(relative_path), "station|stns|trawl") ~ "station_reference",
      str_detect(str_to_lower(relative_path), "cetacean|nbw|sighting") ~ "sightings_or_cetaceans",
      str_detect(str_to_lower(relative_path), "biopsy") ~ "biopsy",
      str_detect(str_to_lower(relative_path), "environment|env") ~ "environment",
      TRUE ~ "other"
    )
  ) %>%
  arrange(year, layer, relative_path)

write_csv(inventory, file.path(output_dir, "input_file_inventory.csv"), na = "")
print(inventory %>% count(year, layer, data_role, sort = TRUE), n = 100)
message("Inventory written to: ", file.path(output_dir, "input_file_inventory.csv"))
