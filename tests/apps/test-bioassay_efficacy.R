# =============================================================================
# TEST: bioassay efficacy tab (control_efficacy app)
# =============================================================================
# Covers action='P' bioassay scoring: emergence inhibition, the pending vs
# zero-emergence distinction, dominant-genus assignment with an Unknown
# fallback, and material effect-window classification.
#
# load_bioassay_data() itself is SQL and is not unit-testable without a
# database, so the post-processing is exercised through the pure helper
# derive_bioassay_metrics().
# =============================================================================

get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

root <- get_project_root()

source(file.path(root, "tests/stubs/stub_bioassay.R"), local = TRUE)

test_that("all bioassay source files load correctly", {
  skip_if_not_installed("RPostgres")
  expect_no_error({
    source(file.path(root, "shared/app_libraries.R"), local = TRUE)
    source(file.path(root, "shared/server_utilities.R"), local = TRUE)
    source(file.path(root, "shared/db_helpers.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/data_functions.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/bioassay_data_functions.R"), local = TRUE)
    source(file.path(root, "apps/control_efficacy/display_functions.R"), local = TRUE)
  })
})

if (!requireNamespace("RPostgres", quietly = TRUE)) {
  skip("RPostgres is not installed")
}

source(file.path(root, "shared/app_libraries.R"), local = TRUE)
source(file.path(root, "shared/server_utilities.R"), local = TRUE)
source(file.path(root, "shared/db_helpers.R"), local = TRUE)
source(file.path(root, "apps/control_efficacy/data_functions.R"), local = TRUE)
source(file.path(root, "apps/control_efficacy/bioassay_data_functions.R"), local = TRUE)

# No database in tests: derive_bioassay_metrics() takes thresholds directly.
get_db_connection <- function() NULL
safe_disconnect <- function(conn) invisible(NULL)

derived <- derive_bioassay_metrics(
  get_stub_bioassay_rows(),
  genus_tbl  = get_stub_bioassay_species(),
  thresholds = get_stub_bioassay_thresholds()
)
expected <- get_stub_bioassay_expected()

# =============================================================================
# MATERIAL WINDOW CLASSIFICATION
# =============================================================================

test_that("classify_material_window separates within, past, and untreated", {
  bio <- as.Date(c("2025-06-10", "2025-06-10", "2025-06-10", "2025-06-10"))
  exp <- as.Date(c("2025-06-15", "2025-06-10", "2025-06-01", NA))

  expect_equal(
    classify_material_window(bio, exp),
    c("Within window", "Within window", "Past expiry", "Untreated")
  )
})

test_that("classify_material_window handles an empty vector", {
  expect_equal(
    classify_material_window(as.Date(character(0)), as.Date(character(0))),
    character(0)
  )
})

test_that("material window and days past expiry are derived correctly", {
  expect_equal(derived$material_window, expected$material_window)

  # Row 6: treated 2024-06-01 with effect_days 14 -> expiry 2024-06-15,
  # bioassay taken 2024-07-25 = 40 days past expiry.
  row6 <- derived[derived$pkey_pg == 9006, ]
  expect_equal(row6$days_past_expiry, 40L)
  expect_equal(row6$material_window, "Past expiry")

  # Rows inside the window carry no days-past value.
  expect_true(all(is.na(derived$days_past_expiry[derived$material_window != "Past expiry"])))
})

test_that("missing effect_days falls back to 14 days", {
  # Row 8 has effect_days NA and a treatment on 2024-07-29; the 14-day
  # default puts expiry at 2024-08-12, so the 2024-07-30 bioassay is inside.
  row8 <- derived[derived$pkey_pg == 9008, ]
  expect_equal(row8$prior_trt_effect_days, BIOASSAY_DEFAULT_EFFECT_DAYS)
  expect_equal(row8$material_window, "Within window")
})

test_that("action '1' (Inspect + Treat ground) counts as a treatment", {
  row9 <- derived[derived$pkey_pg == 9009, ]

  # The bug this guards: '1' was excluded from the treatment match, so these
  # reported "Untreated" despite a treatment inside the effect window.
  expect_equal(row9$trt_type, "Ground")
  expect_equal(row9$material_window, "Within window")
  expect_false(is.na(row9$prior_trt_date))
})

test_that("action '6' (Treat Catch Basins) counts as a treatment", {
  row10 <- derived[derived$pkey_pg == 9010, ]

  expect_equal(row10$trt_type, "Catch Basin")
  expect_equal(row10$material_window, "Within window")
  expect_false(is.na(row10$prior_trt_date))
})

test_that("BIOASSAY_TRT_ACTIONS is the single source of truth for the match", {
  # The constant used to be dead code while the SQL hardcoded its own list.
  expect_setequal(BIOASSAY_TRT_ACTIONS, c("A", "D", "3", "1", "6"))
})

test_that("an untreated bioassay still carries a season", {
  row7 <- derived[derived$pkey_pg == 9007, ]
  expect_equal(row7$material_window, "Untreated")
  expect_true(is.na(row7$prior_trt_date))
  expect_false(is.na(row7$season))
  expect_equal(row7$trt_type, "None")
})

# =============================================================================
# EMERGENCE INHIBITION
# =============================================================================

test_that("emergence inhibition matches 100 * (1 - emerged / pupae)", {
  expect_equal(derived$pct_inhibition, expected$pct_inhibition)
})

test_that("a pending bioassay scores NA, never 100 percent", {
  row2 <- derived[derived$pkey_pg == 9002, ]

  expect_true(row2$is_pending)
  expect_true(is.na(row2$pct_inhibition))
  # The failure mode this guards: treating a missing emergence count as zero
  # emergence would report perfect control for unfinished work.
  expect_false(isTRUE(row2$pct_inhibition == 100))
  expect_equal(row2$pupaecnt, 50L)
  expect_true(is.na(row2$emergedcnt))
})

test_that("a zero-pupae bioassay is neither pending nor scored", {
  row3 <- derived[derived$pkey_pg == 9003, ]

  expect_true(row3$is_zero_pupae)
  expect_false(row3$is_pending)
  expect_true(is.na(row3$pct_inhibition))
})

test_that("zero emergence scores 100 percent inhibition", {
  row8 <- derived[derived$pkey_pg == 9008, ]
  expect_equal(row8$pct_inhibition, 100)
  expect_false(row8$is_pending)
})

test_that("pending and zero-pupae flags are mutually exclusive", {
  expect_equal(derived$is_pending, expected$is_pending)
  expect_equal(derived$is_zero_pupae, expected$is_zero_pupae)
  expect_false(any(derived$is_pending & derived$is_zero_pupae))
})

# =============================================================================
# GENUS ASSIGNMENT
# =============================================================================

test_that("dominant genus is taken from the larval sample", {
  expect_equal(derived$genus, expected$genus)
  expect_equal(derived$has_species, expected$has_species)

  # Row 1 is 80% Aedes / 20% Culex -> Aedes wins.
  expect_equal(derived$genus[derived$pkey_pg == 9001], "Aedes")
  # Row 7 is 30% Aedes / 70% Culex -> Culex wins.
  expect_equal(derived$genus[derived$pkey_pg == 9007], "Culex")
})

test_that("a bioassay with no species data is kept as Unknown", {
  rows <- get_stub_bioassay_rows()
  row4 <- derived[derived$pkey_pg == 9004, ]

  expect_equal(row4$genus, "Unknown")
  expect_false(row4$has_species)
  expect_true(is.na(row4$genus_pct))
  # Kept, not dropped: the row count must survive the species join.
  expect_equal(nrow(derived), nrow(rows))
})

test_that("every bioassay stays a single row", {
  # Guards against reintroducing the checkback loader's per-genus fan-out,
  # which would double-count a pupae cup across Aedes and Culex.
  expect_equal(nrow(derived), length(unique(derived$pkey_pg)))
  expect_false(any(duplicated(derived$pkey_pg)))
})

test_that("season and treatment type are derived from the bioassay date", {
  expect_equal(derived$season, expected$season)
  expect_equal(derived$trt_type, expected$trt_type)
})

test_that("derive_bioassay_metrics tolerates no species table", {
  bare <- derive_bioassay_metrics(
    get_stub_bioassay_rows(),
    genus_tbl  = NULL,
    thresholds = get_stub_bioassay_thresholds()
  )
  expect_true(all(bare$genus == "Unknown"))
  expect_false(any(bare$has_species))
  expect_equal(nrow(bare), nrow(get_stub_bioassay_rows()))
})

# =============================================================================
# FILTERING
# =============================================================================

all_seasons <- c("Spring", "Summer")
all_types   <- c("Air", "Ground", "Drone", "Catch Basin", "None")
all_windows <- c("Within window", "Past expiry", "Untreated")

test_that("filter_bioassay_data returns everything when nothing is narrowed", {
  out <- filter_bioassay_data(derived, seasons = all_seasons,
                              trt_types = all_types, windows = all_windows)
  expect_equal(nrow(out), nrow(derived))
})

test_that("filter_bioassay_data retains pending rows for the pending list", {
  out <- filter_bioassay_data(derived, seasons = all_seasons,
                              trt_types = all_types, windows = all_windows)
  expect_true(any(out$is_pending))
})

test_that("an empty checkbox group yields no data", {
  expect_null(filter_bioassay_data(derived, seasons = NULL,
                                   trt_types = all_types, windows = all_windows))
  expect_null(filter_bioassay_data(derived, seasons = all_seasons,
                                   trt_types = character(0), windows = all_windows))
  expect_null(filter_bioassay_data(derived, seasons = all_seasons,
                                   trt_types = all_types, windows = NULL))
})

test_that("genus and window filters narrow correctly", {
  aedes <- filter_bioassay_data(derived, genus = "Aedes", seasons = all_seasons,
                                trt_types = all_types, windows = all_windows)
  expect_true(all(aedes$genus == "Aedes"))

  unknown <- filter_bioassay_data(derived, genus = "Unknown", seasons = all_seasons,
                                  trt_types = all_types, windows = all_windows)
  expect_equal(nrow(unknown), 1)
  expect_equal(unknown$pkey_pg, 9004)

  past <- filter_bioassay_data(derived, seasons = all_seasons, trt_types = all_types,
                               windows = "Past expiry")
  expect_equal(past$pkey_pg, 9006)
})

test_that("material and facility filters narrow correctly", {
  meth <- filter_bioassay_data(derived, seasons = all_seasons, trt_types = all_types,
                               windows = all_windows, allowed_matcodes = "22")
  expect_true(all(meth$prior_trt_material == "22"))
  expect_equal(sort(meth$pkey_pg), c(9006, 9008))

  sj <- filter_bioassay_data(derived, seasons = all_seasons, trt_types = all_types,
                             windows = all_windows, facility = "Sj")
  expect_true(all(sj$facility == "Sj"))
})

test_that("filter_bioassay_data handles NULL and empty input", {
  expect_null(filter_bioassay_data(NULL))
  expect_null(filter_bioassay_data(derived[0, ], seasons = all_seasons,
                                   trt_types = all_types, windows = all_windows))
})

# =============================================================================
# SUMMARY COUNTS
# =============================================================================

test_that("summarize_bioassay_pending counts each state once", {
  s <- summarize_bioassay_pending(derived)

  expect_equal(s$total, 10L)
  expect_equal(s$pending, 1L)
  expect_equal(s$zero_pupae, 1L)
  expect_equal(s$scored, sum(!is.na(expected$pct_inhibition)))
  expect_equal(s$past_expiry, 1L)
  expect_equal(s$untreated, 1L)
  expect_equal(s$within_window + s$past_expiry + s$untreated, s$total)
})

test_that("summarize_bioassay_pending zeroes out on empty input", {
  s <- summarize_bioassay_pending(NULL)
  expect_equal(s$total, 0L)
  expect_equal(s$pending, 0L)
  expect_equal(s$scored, 0L)
})
