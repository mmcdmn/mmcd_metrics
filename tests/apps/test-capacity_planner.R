# =============================================================================
# TEST: capacity_planner app (routed drive times)
# =============================================================================
# Pure-function tests for the routed facility -> section drive-time lookup and
# how it feeds the What-If crew estimate. No database needed.
# =============================================================================

get_project_root <- function() {
  if (file.exists("apps")) return(".")
  if (file.exists("../../apps")) return("../..")
  stop("Cannot find project root")
}

load_capacity_fns <- function(env) {
  root <- get_project_root()
  source(file.path(root, "apps/capacity_planner/data_functions.R"), local = env)
  source(file.path(root, "apps/capacity_planner/display_functions.R"), local = env)
}

stub_sections <- function() {
  data.frame(
    facility = c("Sr", "Sr", "Sr"), sectcode = c("190101-", "190102-", "190103-"),
    ground_sites = c(10, 20, 30), air_sites = c(1, 2, 3), miles = c(10, 20, 5),
    stringsAsFactors = FALSE)
}

stub_routed_csv <- function() {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    facility = c("Sr", "Sr"), sectcode = c("190101-", "190102-"),
    drive_min = c(24, 40), drive_miles = c(14, 26), snap_m = c(10, 2500),
    snap_flag = c(FALSE, TRUE), built = "2026-10-26", osm_extract = "test"),
    f, row.names = FALSE)
  f
}

test_that("load_routed_drivetimes reads the CSV and renames columns", {
  env <- new.env(); load_capacity_fns(env)
  r <- env$load_routed_drivetimes(stub_routed_csv())
  expect_equal(names(r), c("facility", "sectcode", "drive_min_routed",
                           "drive_miles_routed", "snap_m", "snap_flag"))
  expect_equal(nrow(r), 2)
  expect_equal(r$drive_min_routed, c(24, 40))
})

test_that("load_routed_drivetimes returns NULL for a missing or malformed file", {
  env <- new.env(); load_capacity_fns(env)
  expect_null(env$load_routed_drivetimes(file.path(tempdir(), "does_not_exist.csv")))
  bad <- tempfile(fileext = ".csv"); write.csv(data.frame(a = 1), bad, row.names = FALSE)
  expect_null(env$load_routed_drivetimes(bad))
})

test_that("attach_routed_drivetimes matches on facility + sectcode, NA when no row", {
  env <- new.env(); load_capacity_fns(env)
  s <- env$attach_routed_drivetimes(stub_sections(), env$load_routed_drivetimes(stub_routed_csv()))
  expect_equal(s$drive_min_routed, c(24, 40, NA))
  expect_equal(s$drive_miles_routed, c(14, 26, NA))
  s0 <- env$attach_routed_drivetimes(stub_sections(), NULL)
  expect_true(all(is.na(s0$drive_min_routed)))
})

test_that("section_drive_minutes: routed, estimate, fallback and speed scaling", {
  env <- new.env(); load_capacity_fns(env)
  s <- env$attach_routed_drivetimes(stub_sections(), env$load_routed_drivetimes(stub_routed_csv()))
  est <- env$est_drive_minutes(s$miles, 45, 1.3)
  expect_equal(env$section_drive_minutes(s, "estimate", 45, 1.3), est)
  routed <- env$section_drive_minutes(s, "routed", 45, 1.3, 100)
  expect_equal(routed[1:2], c(24, 40))
  expect_equal(routed[3], est[3])                       # no routed row -> estimate
  slower <- env$section_drive_minutes(s, "routed", 45, 1.3, 80)
  expect_equal(slower[1:2], c(30, 50))                  # 80% field speed -> minutes / 0.8
  expect_equal(env$section_drive_minutes(s, "routed", 45, 1.3, NA), routed)
  expect_equal(length(env$section_drive_minutes(s[0, ], "routed")), 0)
})

test_that("estimate_crews uses supplied drive_minutes for travel hours", {
  env <- new.env(); load_capacity_fns(env)
  rates <- c(gnd_trt = 1)
  hpj <- c(gnd_trt = 1)
  s <- stub_sections()
  with_est <- env$estimate_crews(s, rates, hpj, hrs_per_crew = 100, trips_per_section = 2,
                                 avg_mph = 45, circuity = 1.3)
  routed <- env$estimate_crews(s, rates, hpj, hrs_per_crew = 100, trips_per_section = 2,
                               drive_minutes = c(60, 120, 30))
  # travel = trips x 2 x one-way hours, summed: 2 x 2 x (1 + 2 + 0.5) = 14
  expect_equal(routed$totals$travel_hours, 14)
  expect_equal(with_est$totals$travel_hours,
               round(sum(2 * 2 * env$est_drive_minutes(s$miles, 45, 1.3) / 60), 1))
  expect_equal(env$estimate_crews(s, rates, hpj, hrs_per_crew = 100, trips_per_section = 0,
                                  drive_minutes = c(60, 120, 30))$totals$travel_hours, 0)
})

test_that("routed_summary reports circuity and implied mph per facility", {
  env <- new.env(); load_capacity_fns(env)
  s <- env$attach_routed_drivetimes(stub_sections(), env$load_routed_drivetimes(stub_routed_csv()))
  r <- env$routed_summary(s)
  expect_equal(r$Facility[1], "DISTRICT")
  expect_true("Sr" %in% r$Facility)
  expect_equal(r$Circuity[r$Facility == "Sr"], round(median(c(14 / 10, 26 / 20)), 2))
  expect_equal(nrow(env$routed_summary(env$attach_routed_drivetimes(stub_sections(), NULL))), 0)
})

test_that("the committed routed lookup (if present) is complete and sane", {
  root <- get_project_root()
  path <- file.path(root, "apps/capacity_planner/data/facility_section_drivetime.csv")
  skip_if_not(file.exists(path), "routed lookup not built")
  d <- read.csv(path, stringsAsFactors = FALSE)
  expect_true(all(c("facility", "sectcode", "drive_min", "drive_miles", "snap_m") %in% names(d)))
  expect_false(any(duplicated(paste(d$facility, d$sectcode))))
  expect_true(all(d$drive_min > 0, na.rm = TRUE))
  expect_lt(mean(is.na(d$drive_min)), 0.01)
})

test_that("whatif_compute works for every staffing driver, including 'sections' (regression)", {
  env <- new.env(); load_capacity_fns(env)
  sec <- data.frame(
    facility = "Sr", fosarea = rep(c("1901", "1902"), each = 4), zone = rep(c("1", "1", "2", "2"), 2),
    sectcode = paste0("19", 1:8), ground_sites = 10, ground_acres = 5, air_sites = 2, air_acres = 20,
    prehatch_sites = 1, prehatch_acres = 1, wet_cb = 3, miles = 8, stringsAsFactors = FALSE)
  cap <- data.frame(fosarea = c("1901", "1902"), fos_name = c("A", "B"), facility = "Sr",
                    facility_name = "South Rosemount", n_techs = 5,
                    drone_offload_pct = 0, drone_air_offload_pct = 0, stringsAsFactors = FALSE)
  # sections 3 and 4 (P2 of FOS 1901) are promoted
  for (m in c("ground_sites", "ground_acres", "air_sites", "air_acres",
              "prehatch_sites", "prehatch_acres", "sections", "wet_cb")) {
    r <- env$whatif_compute(sec, c("193", "194"), cap, metric = m, per_tech = 1)
    expect_equal(r$totals$metric, m, info = m)
    expect_gt(r$totals$added_load, 0, label = paste("added_load for", m))
  }
  # "sections" counts rows: 2 P1 sections -> 4 after promoting 2 P2 sections in FOS 1901
  r <- env$whatif_compute(sec, c("193", "194"), cap, metric = "sections", per_tech = 1)
  expect_equal(r$totals$added_load, 2)
})

# ---- Estimate A travel-time ratio -------------------------------------------
travel_fixture <- function() {
  # FOS 1901: 2 current P1 sections (20 min and 30 min away -> avg 25) and
  # 2 P2 sections that could be promoted: one 50 min away, one 100 min away.
  sec <- data.frame(
    facility = "Sr", fosarea = "1901", zone = c("1", "1", "2", "2"),
    sectcode = c("s1", "s2", "s3", "s4"), ground_sites = c(100, 100, 100, 100),
    miles = c(10, 15, 25, 50), stringsAsFactors = FALSE)
  cap <- data.frame(fosarea = "1901", fos_name = "A", facility = "Sr",
                    facility_name = "South Rosemount", n_techs = 5,
                    drone_offload_pct = 0, drone_air_offload_pct = 0, stringsAsFactors = FALSE)
  list(sec = sec, cap = cap, dm = c(20, 30, 50, 100))
}

test_that("travel_adjust = FALSE (or no drive minutes) reproduces the unadjusted numbers", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  base <- env$whatif_compute(x$sec, c("s3", "s4"), x$cap, "ground_sites", per_tech = 100)
  off  <- env$whatif_compute(x$sec, c("s3", "s4"), x$cap, "ground_sites", per_tech = 100,
                             drive_minutes = x$dm, travel_adjust = FALSE)
  expect_equal(off$totals$added_techs, base$totals$added_techs)
  expect_equal(base$by_fos$needed_new, 4)            # ceiling(400 / 100)
  expect_false(isTRUE(off$totals$travel_adjusted))
  expect_equal(off$by_fos$travel_ratio, 1)
})

test_that("promoted sections farther than the current P1 average need more techs", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  # avg P1 = 25 min; promoted s3 + s4 average 75 min -> ratio 25/75 = 1/3
  r <- env$whatif_compute(x$sec, c("s3", "s4"), x$cap, "ground_sites", per_tech = 100,
                          drive_minutes = x$dm, travel_adjust = TRUE)
  b <- r$by_fos
  expect_true(r$totals$travel_adjusted)
  expect_equal(b$med_p1_min, 25); expect_equal(b$med_promoted_min, 75)
  expect_equal(b$travel_ratio, 1 / 3, tolerance = 1e-9)
  # s = 20%: factor = 1 / (0.8 + 0.2 x 3) = 1/1.4; needed_new = ceiling(2 + 200/(100/1.4)) = 5
  expect_equal(b$capacity_factor, 1 / 1.4, tolerance = 1e-9)
  expect_equal(b$needed_new, 5)
  expect_equal(b$added_techs, 3)
  expect_gt(r$totals$added_techs, env$whatif_compute(x$sec, c("s3", "s4"), x$cap,
                                                     "ground_sites", per_tech = 100)$totals$added_techs)
})

test_that("closer-than-average promoted sections raise capacity; ratio 1 when not computable", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  near <- env$whatif_compute(x$sec, "s3", x$cap, "ground_sites", per_tech = 100,
                             drive_minutes = c(20, 30, 10, 100), travel_adjust = TRUE)
  expect_equal(near$by_fos$travel_ratio, 2.5)         # 25 / 10
  expect_equal(near$by_fos$needed_new, 3)             # factor 1/(0.8+0.08); ceiling(2.88) = 3
  none <- env$whatif_compute(x$sec, character(0), x$cap, "ground_sites", per_tech = 100,
                             drive_minutes = x$dm, travel_adjust = TRUE)
  expect_equal(none$by_fos$travel_ratio, 1)            # nothing promoted
  na <- env$whatif_compute(x$sec, "s3", x$cap, "ground_sites", per_tech = 100,
                           drive_minutes = c(NA, NA, 50, 100), travel_adjust = TRUE)
  expect_equal(na$by_fos$travel_ratio, 1)              # no P1 times -> no adjustment
  wrong_len <- env$whatif_compute(x$sec, "s3", x$cap, "ground_sites", per_tech = 100,
                                  drive_minutes = c(1, 2), travel_adjust = TRUE)
  expect_false(wrong_len$totals$travel_adjusted)       # misaligned vector is ignored
})

test_that("drone offload is applied before the travel ratio", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  x$cap$drone_offload_pct <- 50
  r <- env$whatif_compute(x$sec, c("s3", "s4"), x$cap, "ground_sites", per_tech = 100,
                          drive_minutes = x$dm, travel_adjust = TRUE)
  # loads halve: cur 100, added 100; factor 1/1.4 -> ceiling(1 + 100/(100/1.4)) = 3; now = 1
  expect_equal(r$by_fos$needed_new, 3)
  expect_equal(r$by_fos$added_techs, 2)
})

test_that("travel share bounds the capacity factor and is editable", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  # promoted sections 100x closer: factor can't pass 1 / (1 - s) = 1.25 at s = 20
  dm <- c(20, 30, 0.25, 0.25)
  r <- env$whatif_compute(x$sec, c("s3", "s4"), x$cap, "ground_sites", per_tech = 100,
                          drive_minutes = dm, travel_adjust = TRUE)
  expect_lt(r$by_fos$capacity_factor, 1.25)
  expect_gt(r$by_fos$capacity_factor, 1.2)
  # s = 0: travel does not change capacity at all; s = 100 is capped at 99
  r0 <- env$whatif_compute(x$sec, c("s3", "s4"), x$cap, "ground_sites", per_tech = 100,
                           drive_minutes = x$dm, travel_adjust = TRUE, travel_share = 0)
  expect_equal(r0$by_fos$capacity_factor, 1)
  expect_equal(r0$by_fos$needed_new, 4)
  r1 <- env$whatif_compute(x$sec, c("s3", "s4"), x$cap, "ground_sites", per_tech = 100,
                           drive_minutes = x$dm, travel_adjust = TRUE, travel_share = 100)
  expect_equal(r1$by_fos$capacity_factor, 1 / (0.01 + 0.99 * 3), tolerance = 1e-9)
})

test_that("typical drive time is the median, so one far section does not skew it", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  x$sec$zone <- c("1", "1", "1", "2")
  x$sec$sectcode <- c("s1", "s2", "s3", "s4")
  # P1 times 20, 30, 400 -> median 30 (mean would be 150)
  r <- env$whatif_compute(x$sec, "s4", x$cap, "ground_sites", per_tech = 100,
                          drive_minutes = c(20, 30, 400, 30), travel_adjust = TRUE)
  expect_equal(r$by_fos$med_p1_min, 30)
  expect_equal(r$by_fos$travel_ratio, 1)
})

# ---- Phone layout: capacity_dt pins the label columns ------------------------
test_that("capacity_dt freezes leading columns, scrolls sideways and keeps caller options", {
  skip_if_not_installed("DT")
  env <- new.env(); load_capacity_fns(env)
  d <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  w <- env$capacity_dt(d, left = 2, rownames = FALSE, options = list(dom = "t", pageLength = 5))
  expect_equal(w$x$options$fixedColumns$leftColumns, 2)
  expect_true(w$x$options$scrollX)
  expect_equal(w$x$options$dom, "t")
  expect_equal(w$x$options$pageLength, 5)
  expect_true("FixedColumns" %in% unlist(w$x$extensions))
  # nothing to pin when the table has no more columns than `left`
  one <- env$capacity_dt(data.frame(Note = "x"), rownames = FALSE)
  expect_null(one$x$options$fixedColumns)
  expect_true(one$x$options$scrollX)
})

# ---- What-If map accessibility (the shared a11y test only parses R, not the map's JS) ----
test_that("map selection has a keyboard route, a real button and an accessible name", {
  app_src <- paste(readLines(file.path(dirname(test_path()), "..", "..", "apps", "capacity_planner", "app.R"),
                             warn = FALSE), collapse = " ")
  has <- function(x) grepl(x, app_src, fixed = TRUE)
  # keyboard-operable picker bound to the same selection as the map
  expect_true(has('type="checkbox" class="wi-sel"'))
  expect_true(has("aria-label=\"Select section"))
  expect_true(has("observeEvent(input$wi_toggle"))
  expect_false(has("wi_pick"))
  # map is a labelled figure and the selection count is announced
  expect_true(has('leaflet::leafletOutput("wi_map", height = 440),'))
  expect_true(has("a11y_figure("))
  expect_true(has('role = "status"'))
  # the Box select control is a <button> with pressed state, never a link
  expect_true(has("L.DomUtil.create('button'"))
  expect_true(has("aria-pressed"))
  expect_false(has("L.DomUtil.create('a'"))
  # scrolling tables are keyboard reachable and caption text meets contrast
  expect_true(has("tabindex: 0, role: 'region'"))
  expect_true(has("caption { color:#555; }"))
  # one box per tap so page scrolling comes back
  expect_true(has("setBox(false)"))
})
