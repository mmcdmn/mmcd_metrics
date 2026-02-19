# =============================================================================
# Scan all breeding site .gpkg files 
# ONLY facilities: Sr (Sr_Rosemount), E (East_Oakdale), Sj (Sj_Jordan)
# Files without ALL core columns are EXCLUDED from analysis, except "sample" and "drone" which are flagged but allowed missing
# Reports: unique facility/zone/foreman values, non-core columns, qualifying files
# =============================================================================

library(sf)

# CHANGE THIS to your local path where the breeding site .gpkg files are stored
base_dir <- "C:/Users/datatech/Documents/breeding site files"

# Find all gpkg files, skip old/Archive/to_upload and all_air_ba
all_gpkg <- list.files(base_dir, pattern = "\\.gpkg$", recursive = TRUE, full.names = TRUE)
skip_pattern <- "(old|Archive|to_upload|all_air_ba)"
gpkg_files <- all_gpkg[!grepl(skip_pattern, all_gpkg, ignore.case = TRUE)]

# ONLY include files from Sr_Rosemount, East_Oakdale, Sj_Jordan
# These are folders whose prefix (before the _) is Sr, East, Sj
facility_pattern <- "^(Sr_|East_|Sj_)"
gpkg_files <- gpkg_files[grepl(facility_pattern, basename(dirname(gpkg_files)))]

cat(sprintf("Found %d .gpkg files (Sr, E, Sj only)\n\n", length(gpkg_files)))

# UPDATED core columns - now includes facility, zone, foreman
# "hard" core = MUST have these to qualify at all
# "soft" core = drone, sample — include files missing these but FLAG them loudly
hard_core_cols <- c("sitecode", "priority", "acres", "type", "air_gnd",
                    "culex", "spr_aedes", "coq_pert", "prehatch",
                    "remarks", "facility", "zone", "foreman")
soft_core_cols <- c("drone", "sample")
core_cols <- c(hard_core_cols, soft_core_cols)

# Structural columns to always exclude
structural_cols <- c("geom", "geometry", "fid", "gid", "id", "site",
                     "sectcode", "page")

# QGIS junk prefix to exclude
qgis_junk_prefix <- "auxiliary_storage_"

# Read column names and sample data from each file
file_info <- list()

for (f in gpkg_files) {
  short <- gsub(paste0(base_dir, "/"), "", f)
  tryCatch({
    layers <- st_layers(f)
    layer_name <- layers$name[1]
    
    # Read schema only
    d <- st_read(f, layer = layer_name, query = paste0("SELECT * FROM \"", layer_name, "\" LIMIT 0"), quiet = TRUE)
    cols_raw <- names(d)
    cols_lower <- tolower(cols_raw)
    
    # Check if ALL hard core columns are present (drone/sample are soft — allowed missing)
    has_hard_core <- all(hard_core_cols %in% cols_lower)
    missing_soft <- setdiff(soft_core_cols, cols_lower)
    
    file_info[[short]] <- list(
      cols_raw = cols_raw,
      cols_lower = cols_lower,
      has_hard_core = has_hard_core,
      missing_soft = missing_soft,
      path = f,
      layer = layer_name
    )
  }, error = function(e) {
    cat(sprintf("  ERROR reading %s: %s\n", short, e$message))
  })
}

# Split: qualifying = has all hard core (may be missing drone/sample)
# excluded = missing hard core columns
qualifying <- file_info[sapply(file_info, function(x) x$has_hard_core)]
excluded <- file_info[sapply(file_info, function(x) !x$has_hard_core)]

# Further split qualifying: fully complete vs missing soft core
needs_columns <- qualifying[sapply(qualifying, function(x) length(x$missing_soft) > 0)]

cat(sprintf("=== FILES WITH ALL HARD CORE COLUMNS: %d / %d ===\n", length(qualifying), length(file_info)))
for (name in names(qualifying)) {
  if (length(qualifying[[name]]$missing_soft) > 0) {
    cat(sprintf("  PASS*: %-55s (%d cols)  *** MISSING: %s ***\n", name, 
                length(qualifying[[name]]$cols_lower),
                paste(qualifying[[name]]$missing_soft, collapse = ", ")))
  } else {
    cat(sprintf("  PASS:  %s (%d cols)\n", name, length(qualifying[[name]]$cols_lower)))
  }
}

if (length(needs_columns) > 0) {
  cat("\n")
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  cat("!!!  WARNING: THE FOLLOWING FILES ARE MISSING SOFT-CORE COLUMNS     !!!\n")
  cat("!!!  These columns NEED TO BE ADDED to these files:                 !!!\n")
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
  for (name in names(needs_columns)) {
    cat(sprintf("  >>> %-55s NEEDS: %s\n", name,
                paste(needs_columns[[name]]$missing_soft, collapse = ", ")))
  }
  cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
}

cat(sprintf("\n=== FILES EXCLUDED (missing hard core columns): %d ===\n", length(excluded)))
for (name in names(excluded)) {
  missing <- setdiff(hard_core_cols, excluded[[name]]$cols_lower)
  cat(sprintf("  SKIP: %-55s missing: %s\n", name, paste(missing, collapse = ", ")))
}

# ============================================================================
# UNIQUE VALUES: facility, zone, foreman from qualifying files
# ============================================================================
cat("\n=== UNIQUE VALUES FROM QUALIFYING FILES ===\n")

all_facilities <- c()
all_zones <- c()
all_foremen <- c()

for (name in names(qualifying)) {
  tryCatch({
    info <- qualifying[[name]]
    # Read just the 3 columns we care about
    d <- st_read(info$path, layer = info$layer, quiet = TRUE)
    d_names_lower <- tolower(names(d))
    names(d) <- d_names_lower
    
    if ("facility" %in% d_names_lower) all_facilities <- c(all_facilities, unique(as.character(d$facility)))
    if ("zone" %in% d_names_lower) all_zones <- c(all_zones, unique(as.character(d$zone)))
    if ("foreman" %in% d_names_lower) all_foremen <- c(all_foremen, unique(as.character(d$foreman)))
  }, error = function(e) {
    cat(sprintf("  Error reading data from %s: %s\n", name, e$message))
  })
}

# Clean up and deduplicate
all_facilities <- sort(unique(all_facilities[!is.na(all_facilities) & all_facilities != ""]))
all_zones <- sort(unique(all_zones[!is.na(all_zones) & all_zones != ""]))
all_foremen <- sort(unique(all_foremen[!is.na(all_foremen) & all_foremen != ""]))

cat(sprintf("\nUnique FACILITY values (%d):\n", length(all_facilities)))
cat(paste("  ", all_facilities, collapse = "\n"), "\n")

cat(sprintf("\nUnique ZONE values (%d):\n", length(all_zones)))
cat(paste("  ", all_zones, collapse = "\n"), "\n")

cat(sprintf("\nUnique FOREMAN values (%d):\n", length(all_foremen)))
cat(paste("  ", all_foremen, collapse = "\n"), "\n")

# ============================================================================
# COLUMN ANALYSIS on qualifying files only
# ============================================================================
cat("\n=== COLUMN ANALYSIS (qualifying files only) ===\n\n")

all_col_sets <- lapply(qualifying, function(x) x$cols_lower)
all_unique_cols <- sort(unique(unlist(all_col_sets)))

# Filter out structural and QGIS junk
meaningful_cols <- all_unique_cols[
  !(all_unique_cols %in% structural_cols) & 
  !grepl(paste0("^", qgis_junk_prefix), all_unique_cols) &
  !(all_unique_cols %in% c("layer", "path", "x", "y", "rotation"))
]

# Count how many qualifying files have each column
col_counts <- sapply(meaningful_cols, function(col) {
  sum(sapply(all_col_sets, function(s) col %in% s))
})

total <- length(qualifying)

cat("--- CORE columns (in definition) ---\n")
for (col in core_cols) {
  cnt <- if (col %in% names(col_counts)) col_counts[col] else 0
  cat(sprintf("  %-25s  %d/%d files  CORE\n", col, cnt, total))
}

cat("\n--- NON-CORE columns present in qualifying files ---\n")
non_core <- setdiff(meaningful_cols, core_cols)
non_core_counts <- col_counts[non_core]
non_core_counts <- sort(non_core_counts, decreasing = TRUE)

for (col in names(non_core_counts)) {
  pct <- round(non_core_counts[col] / total * 100)
  cat(sprintf("  %-25s  %d/%d files  (%d%%)\n", col, non_core_counts[col], total, pct))
}

cat("\n--- Summary ---\n")
cat(sprintf("Qualifying files:        %d\n", total))
cat(sprintf("  - Fully complete:      %d\n", total - length(needs_columns)))
cat(sprintf("  - Missing drone/sample:%d  *** NEED COLUMNS ADDED ***\n", length(needs_columns)))
cat(sprintf("Excluded files:          %d\n", length(excluded)))
cat(sprintf("Core columns (hard):     %d\n", length(hard_core_cols)))
cat(sprintf("Core columns (soft):     %d  (drone, sample)\n", length(soft_core_cols)))
cat(sprintf("Non-core meaningful:     %d\n", length(non_core)))
cat(sprintf("Total unique facilities: %d\n", length(all_facilities)))
cat(sprintf("Total unique zones:      %d\n", length(all_zones)))
cat(sprintf("Total unique foremen:    %d\n", length(all_foremen)))
