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
                           "drive_miles_routed", "snap_m", "snap_flag", "straight_miles"))
  expect_equal(nrow(r), 2)
  expect_equal(r$drive_min_routed, c(24, 40))
})

test_that("load_routed_drivetimes returns NULL for a missing or malformed file", {
  env <- new.env(); load_capacity_fns(env)
  expect_null(env$load_routed_drivetimes(file.path(tempdir(), "does_not_exist.csv")))
  bad <- tempfile(fileext = ".csv"); write.csv(data.frame(a = 1), bad, row.names = FALSE)
  expect_null(env$load_routed_drivetimes(bad))
})

test_that("facility office coordinates are valid and used for straight-line distance", {
  env <- new.env(); load_capacity_fns(env)
  root <- get_project_root()
  locations <- env$load_facility_locations(
    file.path(root, "apps/capacity_planner/data/facility_locations.csv")
  )
  expect_equal(sort(locations$facility), c("E", "N", "Sj", "Sr", "Wm", "Wp"))
  sr <- locations[locations$facility == "Sr", ]
  expect_equal(sr$lon, -93.1205639)
  expect_equal(sr$lat, 44.7183211)
  expect_equal(round(env$.haversine_miles(sr$lon, sr$lat, -92.84310, 44.73851), 1), 13.7)
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
  # every facility x section pair, so a section can be moved to any facility
  expect_true("straight_miles" %in% names(d))
  expect_equal(nrow(d), length(unique(d$facility)) * length(unique(d$sectcode)))
  expect_equal(sort(unique(table(d$sectcode))), length(unique(d$facility)))
  expect_false(any(is.na(d$straight_miles)))
})

# ---- Scenarios: promote and move sections between facilities ----------------
# Two facilities. Sr: s1, s2 (P1) and s3, s4 (P2). Sj: j1 (P1) and j2 (P2).
scenario_fixture <- function() {
  sec <- data.frame(
    facility = c("Sr", "Sr", "Sr", "Sr", "Sj", "Sj"),
    fosarea = c("1901", "1901", "1901", "1901", "7001", "7001"),
    zone = c("1", "1", "2", "2", "1", "2"),
    sectcode = c("s1", "s2", "s3", "s4", "j1", "j2"),
    ground_sites = c(100, 100, 100, 100, 50, 80), ground_acres = c(10, 10, 10, 10, 5, 8),
    air_sites = c(1, 2, 3, 4, 5, 6), air_acres = c(10, 20, 30, 40, 50, 60),
    prehatch_sites = 1, prehatch_acres = 1, wet_cb = c(1, 1, 1, 1, 1, 1),
    miles = c(10, 15, 25, 50, 12, 30), drive_min_routed = c(20, 30, 50, 100, 25, 60),
    drive_miles_routed = c(8, 12, 20, 40, 10, 25), snap_m = 0, snap_flag = FALSE,
    stringsAsFactors = FALSE)
  # every facility x section pair, as the committed lookup has (Sj is farther from Sr's sections)
  grid <- expand.grid(facility = c("Sr", "Sj"), sectcode = sec$sectcode, stringsAsFactors = FALSE)
  home <- setNames(sec$facility, sec$sectcode)
  grid$drive_min_routed <- ifelse(grid$facility == home[grid$sectcode], 20, 70)
  grid$drive_miles_routed <- ifelse(grid$facility == home[grid$sectcode], 8, 30)
  grid$snap_m <- 0; grid$snap_flag <- FALSE
  grid$straight_miles <- ifelse(grid$facility == home[grid$sectcode], 9, 33)
  staff <- data.frame(facility = c("Sr", "Sj"), crews = c(1, 1), techs_per_crew = c(5, 4),
                      drone_techs = 0, drone_offload_pct = 0, drone_air_offload_pct = 0,
                      stringsAsFactors = FALSE)
  list(sec = sec, times = grid, staff = staff)
}
chg <- function(code, fac = NA_character_, zone = NA_character_) {
  data.frame(sectcode = code, to_facility = fac, to_zone = zone, stringsAsFactors = FALSE)
}

test_that("scenario_apply: no changes leaves the table as is, with from-columns", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, NULL, x$times)
  expect_equal(a$facility, x$sec$facility)
  expect_equal(a$zone, x$sec$zone)
  expect_false(any(a$moved)); expect_false(any(a$promoted)); expect_false(any(a$time_missing))
})

test_that("scenario_apply: promote, move and move + promote, with the rules", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  ch <- rbind(chg("s3", NA, "1"),           # promote P2 -> P1, same facility
              chg("s1", "Sj", NA),          # move a P1 section, stays P1
              chg("s4", "Sj", "1"),         # move + promote
              chg("j1", NA, "2"),           # demotion is ignored
              chg("s2", "Nowhere", NA),     # unknown facility ignored
              chg("j2", "Sj", NA))          # no-op
  a <- env$scenario_apply(x$sec, ch, x$times)
  g <- function(code, col) a[[col]][a$sectcode == code]
  expect_equal(c(g("s3", "facility"), g("s3", "zone")), c("Sr", "1"))
  expect_true(g("s3", "promoted")); expect_false(g("s3", "moved"))
  expect_equal(c(g("s1", "facility"), g("s1", "zone")), c("Sj", "1"))
  expect_true(g("s1", "moved")); expect_false(g("s1", "promoted"))
  expect_equal(c(g("s4", "facility"), g("s4", "zone")), c("Sj", "1"))
  expect_true(g("s4", "moved") && g("s4", "promoted"))
  expect_equal(g("j1", "zone"), "1"); expect_false(g("j1", "promoted"))
  expect_equal(g("s2", "facility"), "Sr")
  expect_false(g("j2", "moved"))
  # the last change for a section wins
  b <- env$scenario_apply(x$sec, rbind(chg("s1", "Sj", NA), chg("s1", "Sr", NA)), x$times)
  expect_equal(b$facility[b$sectcode == "s1"], "Sr")
})

test_that("scenario_apply re-keys a moved section's distance and drive time to the new facility", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, chg("s1", "Sj", NA), x$times)
  expect_equal(a$drive_min_routed[a$sectcode == "s1"], 70)
  expect_equal(a$miles[a$sectcode == "s1"], 33)
  expect_equal(a$drive_min_routed[a$sectcode == "s2"], 30)    # untouched
  none <- env$scenario_apply(x$sec, chg("s1", "Sj", NA), NULL)
  expect_true(none$time_missing[none$sectcode == "s1"])
  expect_true(is.na(none$miles[none$sectcode == "s1"]))
})

test_that("scenario_compare: moves conserve district totals, promotion shifts P2 -> P1", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  mv <- env$scenario_apply(x$sec, chg("s1", "Sj", NA), x$times)
  p1 <- env$scenario_compare(mv, "1", c("ground_sites", "sections"))
  d <- p1[p1$key == "DISTRICT", ]; sr <- p1[p1$key == "Sr", ]; sj <- p1[p1$key == "Sj", ]
  expect_equal(d$ground_sites_chg, 0)                            # a move changes no district total
  expect_equal(sr$ground_sites_chg, -100); expect_equal(sj$ground_sites_chg, 100)
  expect_equal(sr$sections_chg, -1); expect_equal(sj$sections_chg, 1)
  expect_equal(p1$key[1], "DISTRICT")
  pr <- env$scenario_apply(x$sec, chg(c("s3", "s4"), NA, "1"), x$times)
  p1 <- env$scenario_compare(pr, "1", "ground_sites")
  p2 <- env$scenario_compare(pr, "2", "ground_sites")
  expect_equal(p1$ground_sites_chg[p1$key == "DISTRICT"], 200)
  expect_equal(p2$ground_sites_chg[p2$key == "DISTRICT"], -200)
  both <- env$scenario_compare(pr, "both", "ground_sites")
  expect_equal(both$ground_sites_chg[both$key == "DISTRICT"], 0)
})

test_that("load per tech and crew after a move comes from staffing_baseline()", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, chg("s1", "Sj", NA), x$times)
  now <- env$staffing_baseline(env$scenario_cap(transform(a, facility = facility_from, zone = zone_from)), x$staff)
  aft <- env$staffing_baseline(env$scenario_cap(a), x$staff)
  expect_equal(now$ground_sites_per_tech[now$facility == "Sr"], 40)   # 200 sites / 5 techs
  expect_equal(aft$ground_sites_per_tech[aft$facility == "Sr"], 20)   # 100 / 5
  expect_equal(aft$ground_sites_per_tech[aft$facility == "Sj"], 37.5) # 150 / 4
})

test_that("scenario_apply on a subset of rows needs the full facility list to accept a move", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  sub <- x$sec[x$sec$sectcode == "s3", ]            # only Sr rows: Sj is not in this table
  ch <- chg("s3", "Sj", "1")
  expect_equal(env$scenario_apply(sub, ch, x$times)$facility, "Sr")      # destination unknown here
  a <- env$scenario_apply(sub, ch, x$times, facilities = unique(x$sec$facility))
  expect_equal(a$facility, "Sj"); expect_true(a$moved); expect_true(a$promoted)
})

# ---- Selecting / deselecting sections (click, box, checkbox) -----------------
none <- function() chg(character(0), character(0), character(0))
tog <- function(x, cur, codes, dest = "keep", mode = "toggle") {
  env <- new.env(); load_capacity_fns(env)
  env$scenario_toggle(x$sec, cur, codes, dest, mode)
}

test_that("selecting a P2 section promotes it and selecting it again puts it back to P2", {
  x <- scenario_fixture()
  on <- tog(x, none(), "s3")
  expect_equal(on$changes$sectcode, "s3"); expect_equal(on$changes$to_zone, "1")
  env <- new.env(); load_capacity_fns(env)
  a <- env$scenario_apply(x$sec, on$changes, x$times)
  expect_equal(a$zone[a$sectcode == "s3"], "1")
  off <- tog(x, on$changes, "s3")
  expect_equal(nrow(off$changes), 0)
  a <- env$scenario_apply(x$sec, off$changes, x$times)
  expect_equal(a$zone[a$sectcode == "s3"], "2")
  expect_false(any(a$promoted))
})

test_that("a move + promote toggles off with the same destination, re-moves with another", {
  x <- scenario_fixture()
  on <- tog(x, none(), "s3", "Sj")                        # Sr P2 -> Sj P1
  expect_equal(c(on$changes$to_facility, on$changes$to_zone), c("Sj", "1"))
  expect_equal(nrow(tog(x, on$changes, "s3", "Sj")$changes), 0)        # same destination: undo
  stay <- tog(x, on$changes, "s3", "keep")                             # keep: back to Sr, still promoted
  expect_true(is.na(stay$changes$to_facility)); expect_equal(stay$changes$to_zone, "1")
  re <- tog(x, on$changes, "s3", "Sr")                                 # other destination: re-move
  expect_equal(nrow(re$changes), 1)
  expect_true(is.na(re$changes$to_facility)); expect_equal(re$changes$to_zone, "1")
})

test_that("a P1 section can only be moved; with 'keep' nothing is applied and it is reported", {
  x <- scenario_fixture()
  k <- tog(x, none(), "s1")
  expect_equal(nrow(k$changes), 0); expect_equal(k$skipped, 1L)
  mv <- tog(x, none(), "s1", "Sj")
  expect_equal(mv$changes$to_facility, "Sj"); expect_true(is.na(mv$changes$to_zone))
  expect_equal(nrow(tog(x, mv$changes, "s1", "Sj")$changes), 0)        # again: back to Sr
  expect_equal(nrow(tog(x, mv$changes, "s1", "keep")$changes), 0)      # P1 + keep: nothing to apply, restores
  expect_equal(tog(x, none(), "j1", "Sj")$skipped, 1L)                 # already at Sj: no-op
})

test_that("a box adds what is missing, and undoes everything when all of it is already in", {
  x <- scenario_fixture()
  one <- tog(x, none(), "s3")$changes
  box <- tog(x, one, c("s3", "s4"))                                    # s3 in, s4 not: add s4
  expect_equal(sort(box$changes$sectcode), c("s3", "s4"))
  all_off <- tog(x, box$changes, c("s3", "s4"))                        # both in: undo both
  expect_equal(nrow(all_off$changes), 0)
  # P1 sections in the box (nothing to do with 'keep') do not stop a box of promoted ones undoing
  mixed <- tog(x, box$changes, c("s1", "s3", "s4"))
  expect_equal(nrow(mixed$changes), 0)
})

test_that("set and off always apply / undo, and leave other sections alone", {
  x <- scenario_fixture()
  cur <- tog(x, none(), "s3")$changes
  s <- tog(x, cur, c("s3", "s4"), mode = "set")
  expect_equal(sort(s$changes$sectcode), c("s3", "s4"))                # s3 stays promoted
  o <- tog(x, s$changes, "s3", mode = "off")
  expect_equal(o$changes$sectcode, "s4")
  expect_equal(nrow(tog(x, cur, "nope")$changes), 1)                   # unknown code ignored
})

test_that("scenario_staffing works for every driver and counts 'sections' as rows", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, chg(c("s3", "s4"), NA, "1"), x$times)
  for (m in env$SCENARIO_METRICS) {
    r <- env$scenario_staffing(a, x$staff, metric = m, per_tech = 1)
    expect_equal(r$totals$metric, m, info = m)
    expect_gt(r$totals$added_load, 0, label = paste("added_load for", m))
  }
  r <- env$scenario_staffing(a, x$staff, metric = "sections", per_tech = 1)
  expect_equal(r$totals$added_load, 2)
})

test_that("a facility giving sections away frees techs (negative change), the receiver needs more", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, chg(c("s1", "s2"), "Sj", NA), x$times)   # all of Sr's P1 -> Sj
  r <- env$scenario_staffing(a, x$staff, "ground_sites", per_tech = 50)
  sr <- r$by_facility[r$by_facility$facility == "Sr", ]
  sj <- r$by_facility[r$by_facility$facility == "Sj", ]
  expect_equal(sr$removed_load, 200); expect_equal(sr$added_load, 0)
  expect_equal(sr$techs_change, -4)                       # ceil(200/50) -> 0
  expect_equal(sj$added_load, 200)
  expect_equal(sj$techs_change, 4)                        # ceil(50/50)=1 -> ceil(250/50)=5
  expect_equal(r$totals$techs_freed, 4); expect_equal(r$totals$techs_added, 4)
  expect_equal(r$totals$techs_change, 0)
})

# ---- Estimate A travel-time ratio (promotion on one facility) ---------------
travel_fixture <- function() {
  # Sr: 2 current P1 sections (20 and 30 min away -> median 25) and 2 P2 sections that
  # could be promoted: one 50 min away, one 100 min away.
  x <- scenario_fixture()
  x$sec <- x$sec[x$sec$facility == "Sr", ]
  x$staff <- x$staff[x$staff$facility == "Sr", ]
  x$dm <- c(20, 30, 50, 100)
  x
}
tstaff <- function(a, x, dm_b = x$dm, dm_a = x$dm, ...) {
  env <- new.env(); load_capacity_fns(env)
  env$scenario_staffing(a, x$staff, "ground_sites", per_tech = 100, dm_before = dm_b, dm_after = dm_a, ...)
}

test_that("travel_adjust = FALSE (or no drive minutes) reproduces the unadjusted numbers", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  a <- env$scenario_apply(x$sec, chg(c("s3", "s4"), NA, "1"), x$times)
  base <- env$scenario_staffing(a, x$staff, "ground_sites", per_tech = 100)
  off <- tstaff(a, x, travel_adjust = FALSE)
  expect_equal(off$totals$techs_change, base$totals$techs_change)
  expect_equal(base$by_fos$needed_new, 4)            # ceiling(400 / 100)
  expect_false(isTRUE(off$totals$travel_adjusted))
  expect_equal(off$by_fos$travel_ratio, 1)
})

test_that("promoted sections farther than the current P1 median need more techs", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  a <- env$scenario_apply(x$sec, chg(c("s3", "s4"), NA, "1"), x$times)
  r <- tstaff(a, x, travel_adjust = TRUE)
  b <- r$by_fos
  expect_true(r$totals$travel_adjusted)
  expect_equal(b$med_p1_min, 25); expect_equal(b$med_added_min, 75)
  expect_equal(b$travel_ratio, 3, tolerance = 1e-9)        # added 75 / current 25: 3x the driving
  # s = 20%: factor = 1 / (0.8 + 0.2 x 3) = 1/1.4; needed_new = ceiling(2 + 200/(100/1.4)) = 5
  expect_equal(b$capacity_factor, 1 / 1.4, tolerance = 1e-9)
  expect_equal(b$needed_new, 5)
  expect_equal(b$techs_change, 3)
})

test_that("closer-than-median added sections raise capacity; ratio 1 when not computable", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  a <- env$scenario_apply(x$sec, chg("s3", NA, "1"), x$times)
  near <- tstaff(a, x, dm_b = c(20, 30, 10, 100), dm_a = c(20, 30, 10, 100), travel_adjust = TRUE)
  expect_equal(near$by_fos$travel_ratio, 0.4)         # added 10 / current 25: closer, below 1
  expect_equal(near$by_fos$needed_new, 3)             # factor 1/(0.8+0.08); ceiling(2.88) = 3
  none <- env$scenario_apply(x$sec, NULL, x$times)
  expect_equal(tstaff(none, x, travel_adjust = TRUE)$by_fos$travel_ratio, 1)  # nothing added
  expect_equal(tstaff(a, x, dm_b = c(NA, NA, 50, 100), dm_a = c(NA, NA, 50, 100),
                      travel_adjust = TRUE)$by_fos$travel_ratio, 1)           # no P1 times
  wrong_len <- tstaff(a, x, dm_b = c(1, 2), dm_a = c(1, 2), travel_adjust = TRUE)
  expect_false(wrong_len$totals$travel_adjusted)                                   # misaligned vector ignored
})

test_that("a moved-in section is timed from the NEW facility", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, chg("s3", "Sj", "1"), x$times)       # Sr P2 -> Sj P1
  dm_b <- env$section_drive_minutes(transform(a, drive_min_routed = x$sec$drive_min_routed), "routed")
  dm_a <- env$section_drive_minutes(a, "routed")
  r <- env$scenario_staffing(a, x$staff, "ground_sites", per_tech = 40,
                             dm_before = dm_b, dm_after = dm_a, travel_adjust = TRUE)
  sj <- r$by_fos[r$by_fos$facility == "Sj" & r$by_fos$fosarea == "unassigned", ]
  expect_equal(nrow(sj), 1)
  expect_equal(sj$med_added_min, 70)                       # routed Sj -> s3, not Sr's 50
  expect_equal(sj$med_p1_min, 25)                          # baseline = Sj's own P1 median (j1)
  expect_gt(sj$travel_ratio, 1)                            # 70 / 25: farther than Sj's P1
})

test_that("drone offload is applied before the travel ratio", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  x$staff$drone_offload_pct <- 50
  a <- env$scenario_apply(x$sec, chg(c("s3", "s4"), NA, "1"), x$times)
  r <- tstaff(a, x, travel_adjust = TRUE)
  # loads halve: cur 100, added 100; factor 1/1.4 -> ceiling(1 + 100/(100/1.4)) = 3; now = 1
  expect_equal(r$by_fos$needed_new, 3)
  expect_equal(r$by_fos$techs_change, 2)
})

test_that("travel share bounds the capacity factor and is editable", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  a <- env$scenario_apply(x$sec, chg(c("s3", "s4"), NA, "1"), x$times)
  dm <- c(20, 30, 0.25, 0.25)   # promoted sections 100x closer: factor can't pass 1 / (1 - s) = 1.25
  r <- tstaff(a, x, dm_b = dm, dm_a = dm, travel_adjust = TRUE)
  expect_lt(r$by_fos$capacity_factor, 1.25)
  expect_gt(r$by_fos$capacity_factor, 1.2)
  r0 <- tstaff(a, x, travel_adjust = TRUE, travel_share = 0)
  expect_equal(r0$by_fos$capacity_factor, 1)
  expect_equal(r0$by_fos$needed_new, 4)
  r1 <- tstaff(a, x, travel_adjust = TRUE, travel_share = 100)
  expect_equal(r1$by_fos$capacity_factor, 1 / (0.01 + 0.99 * 3), tolerance = 1e-9)
})

test_that("typical drive time is the median, so one far section does not skew it", {
  env <- new.env(); load_capacity_fns(env); x <- travel_fixture()
  x$sec$zone <- c("1", "1", "1", "2")
  a <- env$scenario_apply(x$sec, chg("s4", NA, "1"), x$times)
  r <- tstaff(a, x, dm_b = c(20, 30, 400, 30), dm_a = c(20, 30, 400, 30), travel_adjust = TRUE)
  expect_equal(r$by_fos$med_p1_min, 30)               # mean would be 150
  expect_equal(r$by_fos$travel_ratio, 1)
})

test_that("moved-in sections land in an 'unassigned' row; FOS areas keep their own", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, rbind(chg("s3", NA, "1"), chg("s4", "Sj", "1")), x$times)
  expect_equal(a$fos_new[a$sectcode == "s3"], "1901")          # promoted in place: same FOS area
  expect_true(is.na(a$fos_new[a$sectcode == "s4"]))            # moved to Sj: no FOS area there
  r <- env$scenario_staffing(a, x$staff, "ground_sites", per_tech = 100)
  f <- r$by_fos
  sr <- f[f$facility == "Sr" & f$fosarea == "1901", ]
  un <- f[f$facility == "Sj" & f$fosarea == "unassigned", ]
  expect_equal(sr$added_load, 100)                             # s3 (promoted in place)
  expect_equal(sr$techs_change, 1)                             # ceil(300/100) - ceil(200/100)
  expect_equal(un$added_load, 100)                             # s4 (moved in and promoted)
  expect_equal(un$n_techs, 0); expect_equal(un$needed_now, 0)
  expect_equal(un$needed_new, 1)                               # a new crew's worth of work
  expect_equal(f$fosarea[f$facility == "Sj"], c("7001", "unassigned"))   # unassigned listed last
  # the facility rows are the sums of their FOS rows
  fac <- r$by_facility
  expect_equal(fac$techs_change[fac$facility == "Sj"], 1)
  expect_equal(fac$added_load[fac$facility == "Sr"], 100)
  expect_equal(r$totals$techs_change, sum(f$techs_change))
})

test_that("scenario_compare breaks a facility down by FOS area and shows drive time", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, rbind(chg("s3", NA, "1"), chg("s4", "Sj", "1")), x$times)
  dm_b <- x$sec$drive_min_routed
  dm_a <- env$section_drive_minutes(a, "routed")
  fac <- env$scenario_compare(a, "1", c("ground_sites", "sections"), dm_before = dm_b, dm_after = dm_a)
  expect_equal(fac$ground_sites_chg[fac$key == "Sr"], 100)       # +s3 (s4 was P2, left P2 not P1)
  expect_equal(fac$ground_sites_chg[fac$key == "Sj"], 100)
  expect_equal(fac$p1_min_before[fac$key == "Sr"], 25)           # median(20, 30)
  expect_equal(fac$added_min[fac$key == "Sr"], 50)               # s3
  expect_equal(fac$added_min[fac$key == "Sj"], 70)               # s4 timed from Sj
  fos <- env$scenario_compare(a, "1", c("ground_sites", "sections"), facility = "Sj",
                              dm_before = dm_b, dm_after = dm_a)
  expect_equal(fos$key, c("ALL", "7001", "unassigned"))
  expect_equal(fos$label[3], "Unassigned (moved in)")
  expect_equal(fos$ground_sites_chg, c(100, 0, 100))
  expect_equal(fos$sections_chg, c(1, 0, 1))
  expect_equal(fos$added_min[3], 70)
  # the unassigned row has no P1 of its own: it is compared with the facility's median P1 time
  expect_equal(fos$p1_min_before[3], fos$p1_min_before[1]); expect_equal(fos$p1_min_before[1], 25)
  # the displayed travel ratio (added / current) is the one Estimate A uses for that row
  r <- env$scenario_staffing(a, x$staff, "ground_sites", per_tech = 100,
                             dm_before = dm_b, dm_after = dm_a, travel_adjust = TRUE)
  un <- r$by_fos[r$by_fos$facility == "Sj" & r$by_fos$fosarea == "unassigned", ]
  expect_equal(un$travel_ratio, fos$added_min[3] / fos$p1_min_before[3])
  sr <- env$scenario_compare(a, "1", "ground_sites", facility = "Sr")
  expect_equal(sr$key, c("ALL", "1901"))
  expect_equal(sr$ground_sites_chg, c(100, 100))
})

test_that("scenario_crews: added hours minus removed hours, per facility", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  rates <- list(DISTRICT = c(gnd_trt = 1)); hpj <- c(gnd_trt = 1)
  a <- env$scenario_apply(x$sec, chg(c("s3", "s1"), c("Sj", "Sj"), c("1", NA)), x$times)
  # s3: Sr P2 -> Sj P1 (added at Sj, 100 sites); s1: Sr P1 -> Sj P1 (removed from Sr, added to Sj, 100 sites)
  r <- env$scenario_crews(a, rates, hpj, hrs_per_crew = 100, trips_per_section = 0)
  sj <- r$by_facility[r$by_facility$facility == "Sj", ]
  sr <- r$by_facility[r$by_facility$facility == "Sr", ]
  expect_equal(sj$added_hours, 200); expect_equal(sj$removed_hours, 0)
  expect_equal(sr$added_hours, 0); expect_equal(sr$removed_hours, 100)
  expect_equal(r$totals$crews, 1)                           # (200 - 100) / 100
  # travel uses each side's own drive time
  t1 <- env$scenario_crews(a, rates, hpj, hrs_per_crew = 100, trips_per_section = 1,
                           dm_before = rep(60, 6), dm_after = rep(120, 6))
  expect_equal(t1$totals$travel_hours, 2 * 2 * 2 - 1 * 2 * 1)   # added 2 sections x 2h x 2 trips... minus removed 1
  nothing <- env$scenario_crews(env$scenario_apply(x$sec, NULL, x$times), rates, hpj)
  expect_equal(nrow(nothing$by_facility), 0); expect_equal(nothing$totals$crews, 0)
})

test_that("scenario_drive_summary gives the median P1 drive minutes before and after, DISTRICT first", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, chg("s3", "Sj", "1"), x$times)       # Sr P2 -> Sj P1 (70 min from Sj)
  dm_b <- x$sec$drive_min_routed
  dm_a <- env$section_drive_minutes(a, "routed")
  d <- env$scenario_drive_summary(a, dm_b, dm_a)
  expect_equal(d$facility[1], "DISTRICT")
  expect_equal(d$p1_median_min_before[d$facility == "Sj"], 25)        # j1
  expect_equal(d$p1_median_min_after[d$facility == "Sj"], 47.5)       # median(25, 70)
  expect_equal(d$p1_median_min_before[d$facility == "Sr"], 25)        # median(20, 30)
  expect_equal(d$p1_median_min_after[d$facility == "Sr"], 25)
})

# ---- CSV downloads -------------------------------------------------------------
test_that("What-If CSV exports hold plain numbers with snake_case headers", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  a <- env$scenario_apply(x$sec, rbind(chg("s3", NA, "1"), chg("s4", "Sj", "1")), x$times)
  dm_b <- x$sec$drive_min_routed; dm_a <- env$section_drive_minutes(a, "routed")
  cmp <- env$scenario_compare(a, "1", env$SCENARIO_METRICS, dm_before = dm_b, dm_after = dm_a)
  e <- env$scenario_export_compare(cmp, "facility")
  expect_equal(e$facility[1], "DISTRICT")
  expect_true(all(c("ground_sites_p1_before", "ground_sites_p1_after", "ground_sites_p1_change",
                    "median_drive_min_current_p1", "median_drive_min_added", "travel_ratio") %in% names(e)))
  expect_true(all(vapply(e[-1], is.numeric, logical(1))))
  expect_equal(e$ground_sites_p1_change[e$facility == "Sj"], 100)
  expect_equal(e$travel_ratio[e$facility == "Sj"], round(70 / 25, 3))     # added / current
  # the shown numbers, not "+100" text
  expect_false(any(grepl("[+,]", unlist(e[-1]))))

  f <- env$scenario_export_fos(a, dm_b, dm_a)
  expect_true(all(c("facility", "fos_area", "fos_name") %in% names(f)))
  expect_false("ALL" %in% f$fos_area)
  expect_true("unassigned" %in% f$fos_area[f$facility == "Sj"])

  st <- env$scenario_staffing(a, x$staff, "ground_sites", per_tech = 100,
                              dm_before = dm_b, dm_after = dm_a, travel_adjust = TRUE)
  s <- env$scenario_export_staffing(st)
  expect_equal(nrow(s), nrow(st$by_fos))
  expect_true(all(c("driver", "load_per_tech", "travel_ratio", "capacity_factor",
                    "techs_change") %in% names(s)))
  expect_equal(unique(s$driver), "ground_sites"); expect_equal(unique(s$load_per_tech), 100)
  expect_equal(sum(s$techs_change), st$totals$techs_change)

  now <- a; now$facility <- a$facility_from; now$zone <- a$zone_from
  l <- env$scenario_export_load(env$staffing_baseline(env$scenario_cap(now), x$staff),
                                env$staffing_baseline(env$scenario_cap(a), x$staff))
  expect_true(all(c("facility", "ground_sites_per_tech_now", "ground_sites_per_tech_after",
                    "ground_acres_per_crew_now", "ground_acres_per_crew_after") %in% names(l)))
})

test_that("the changed-sections export lists promoted / moved sections, header-only when none", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  none <- env$scenario_apply(x$sec, NULL, x$times)
  e0 <- env$scenario_export_changes(none)
  expect_equal(nrow(e0), 0)
  expect_true(all(c("sectcode", "facility_from", "facility_to", "zone_from", "zone_to") %in% names(e0)))
  a <- env$scenario_apply(x$sec, rbind(chg("s3", NA, "1"), chg("s4", "Sj", "1")), x$times)
  dm_b <- x$sec$drive_min_routed; dm_a <- env$section_drive_minutes(a, "routed")
  e <- env$scenario_export_changes(a, dm_b, dm_a)
  expect_equal(e$sectcode, c("s3", "s4"))
  expect_equal(e$zone_from, c("P2", "P2")); expect_equal(e$zone_to, c("P1", "P1"))
  expect_equal(e$facility_to, c("Sr", "Sj")); expect_equal(e$moved, c(FALSE, TRUE))
  expect_equal(e$drive_min_from, c(50, 100)); expect_equal(e$drive_min_to, c(50, 70))
  expect_identical(names(env$scenario_export_changes(NULL)), names(e0))
})

test_that("every results table has a CSV download wired in app.R", {
  app_src <- paste(readLines(file.path(dirname(test_path()), "..", "..", "apps", "capacity_planner", "app.R"),
                             warn = FALSE), collapse = "
")
  ids <- c("cap_download", "dl_avg", "dl_drive", "dl_staff", "dl_baseline", "dl_techs",
           "dl_wi_sections", "dl_wi_changes", "dl_wi_fos", "dl_wi_pertech", "dl_wi_a",
           "dl_wi_b_job", "dl_wi_b_fac")
  for (id in ids) {
    expect_true(grepl(sprintf('"%s"', id), app_src, fixed = TRUE), info = paste("button", id))
  }
  for (id in setdiff(ids, "cap_download")) {
    expect_true(grepl(sprintf('csv_out("%s"', id), app_src, fixed = TRUE), info = paste("handler", id))
  }
  expect_true(grepl("output$cap_download <- downloadHandler", app_src, fixed = TRUE))
})

# ---- Sharing a scenario as a link ------------------------------------------------
share_codes <- function() {
  root <- get_project_root()
  d <- read.csv(file.path(root, "apps/capacity_planner/data/facility_section_drivetime.csv"),
                stringsAsFactors = FALSE)
  unique(d$sectcode)
}

test_that("scenario_pack / scenario_unpack round-trip every destination group", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  ch <- rbind(chg("s3", NA, "1"),            # promote in place
              chg("s1", "Sj", NA),           # move only
              chg("s4", "Sj", "1"),          # move + promote
              chg("j2", NA, "1"))            # promote in place (same group as s3)
  p <- env$scenario_pack(ch, x$sec$sectcode)
  expect_equal(length(p$g), 3)                                   # three distinct destinations
  u <- env$scenario_unpack(p, x$sec$sectcode)
  expect_true(u$ok)
  got <- u$changes[order(u$changes$sectcode), ]; want <- ch[order(ch$sectcode), ]
  expect_equal(got$sectcode, want$sectcode)
  expect_equal(got$to_facility, want$to_facility)
  expect_equal(got$to_zone, want$to_zone)
  # the unpacked scenario applies exactly like the original
  a1 <- env$scenario_apply(x$sec, ch, x$times); a2 <- env$scenario_apply(x$sec, u$changes, x$times)
  expect_equal(a1$facility, a2$facility); expect_equal(a1$zone, a2$zone)
  # empty scenario and sections that no longer exist
  e <- env$scenario_unpack(env$scenario_pack(chg(character(0), character(0), character(0)),
                                             x$sec$sectcode), x$sec$sectcode)
  expect_true(e$ok); expect_equal(nrow(e$changes), 0)
  gone <- env$scenario_pack(chg("zzz", "Sj", "1"), x$sec$sectcode)
  expect_equal(length(gone$g), 0)
})

test_that("a link made against a different section list is refused, not guessed at", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  p <- env$scenario_pack(chg("s3", NA, "1"), x$sec$sectcode)
  fewer <- env$scenario_unpack(p, x$sec$sectcode[-1])
  expect_false(fewer$ok); expect_equal(fewer$reason, "sections_changed"); expect_equal(nrow(fewer$changes), 0)
  renamed <- env$scenario_unpack(p, c(x$sec$sectcode[-1], "new1"))        # same count, different list
  expect_false(renamed$ok)
  expect_false(env$scenario_unpack(list(), x$sec$sectcode)$ok)
})

test_that("a share token round-trips settings, staffing and map view", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  settings <- list(wi_travel = FALSE, wi_travel_share = 25, wi_metric = "ground_sites",
                   wi_per_tech = NA, wi_drill = "", wi_move_to = "Sj")
  st <- env$scenario_share_state(chg(c("s3", "s4"), c(NA, "Sj"), c("1", "1")), x$sec, settings,
                                 x$staff, list(lat = 44.95123, lng = -93.2, z = 10.5),
                                 as.Date("2026-11-05"))
  tok <- env$share_token_encode(st)
  expect_match(tok, "^[A-Za-z0-9_-]+$")
  y <- env$share_token_decode(tok)
  expect_equal(y$t, "2026-11-05")
  expect_equal(y$d$p1, 3); expect_equal(y$d$p2, 3)               # data stamp (zone counts)
  # empty / NA settings are dropped, the rest become deep-link text
  s <- env$share_state_settings(y, c("wi_travel", "wi_travel_share", "wi_metric", "wi_per_tech",
                                     "wi_drill", "wi_move_to"))
  expect_equal(s, list(wi_travel = "false", wi_travel_share = "25", wi_metric = "ground_sites",
                       wi_move_to = "Sj"))
  # only allow-listed ids come back
  expect_equal(names(env$share_state_settings(y, "wi_travel")), "wi_travel")
  expect_equal(length(env$share_state_settings(y, character(0))), 0)
  # staffing numbers survive bit-for-bit whatever the JSON writer does (the image's jsonlite
  # keeps 15 digits): 20/3 techs per crew must not come back as 6.66666666666667
  set.seed(11)
  odd <- x$staff; odd$techs_per_crew <- c(20 / 3, 40 / 7); odd$drone_offload_pct <- runif(2, 0, 100)
  z <- env$share_token_decode(env$share_token_encode(
    env$scenario_share_state(chg("s3", NA, "1"), x$sec, list(wi_travel_share = 25), odd)))
  back2 <- env$share_state_staff(z, x$staff)
  expect_identical(back2$techs_per_crew, odd$techs_per_crew)
  expect_identical(back2$drone_offload_pct, odd$drone_offload_pct)
  expect_equal(7 * back2$techs_per_crew[2], 40)
  # staffing table: whole table saved, applied by facility
  cur <- x$staff; cur$techs_per_crew <- c(1, 1)
  back <- env$share_state_staff(y, cur)
  expect_equal(back$techs_per_crew, x$staff$techs_per_crew)
  expect_equal(back$facility, cur$facility)
  v <- env$share_state_view(y)
  expect_equal(v$lat, 44.9512); expect_equal(v$z, 10.5)
  expect_true(env$scenario_unpack(y, x$sec$sectcode)$ok)
})

test_that("share token validation: bad tokens are NULL, bad values are ignored", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  tok <- env$share_token_encode(env$scenario_share_state(chg("s3", NA, "1"), x$sec, list(wi_travel = TRUE)))
  bad_tokens <- list(NULL, NA, "", "abc", "not base64!", strrep("A", 13000), "eJwVyTsKgDAQQMG7vHoD",
                     substr(tok, 1, nchar(tok) - 12), paste0(substr(tok, 1, 40), "!!"),
                     c(tok, tok), 12)
  for (i in seq_along(bad_tokens)) {
    expect_null(suppressWarnings(env$share_token_decode(bad_tokens[[i]])), info = paste("bad token", i))
  }
  # valid gzip + JSON of another shape / version is refused
  other <- function(x) env$.b64url_enc(memCompress(charToRaw(as.character(jsonlite::toJSON(x, auto_unbox = TRUE))), "gzip"))
  expect_null(env$share_token_decode(other(list(v = 99, a = 1))))
  expect_null(env$share_token_decode(other(list(a = 1))))
  expect_null(env$share_token_decode(other(1:3)))
  expect_null(env$share_token_decode(env$.b64url_enc(memCompress(charToRaw("{not json"), "gzip"))))
  # values: unknown facilities, negative / non-finite numbers, offload > 100, map out of range
  y <- list(v = 1L, o = list(wi_travel = TRUE, evil = "x", wi_metric = list("a", "b"), wi_crew_hours = 2184),
            st = list(list(f = "Sr", c = -3, t = 6, d = "x", o = 150, a = 20),
                      list(f = "Nowhere", c = 9, t = 9, d = 9, o = 9, a = 9)),
            m = list(lat = 200, lng = 0, z = 5))
  s <- env$share_state_settings(y, c("wi_travel", "wi_metric", "wi_crew_hours"))
  expect_equal(s, list(wi_travel = "true", wi_crew_hours = "2184"))
  back <- env$share_state_staff(y, x$staff)
  expect_equal(back$crews, x$staff$crews)                              # -3 refused
  expect_equal(back$techs_per_crew, c(6, x$staff$techs_per_crew[2]))   # 6 accepted for Sr
  expect_equal(back$drone_offload_pct, x$staff$drone_offload_pct)      # 150 refused
  expect_equal(back$drone_air_offload_pct[1], 20)
  expect_null(env$share_state_view(y))
  expect_null(env$share_state_view(list()))
})

test_that("share links stay short: a bitmap per destination, measured on the real section list", {
  env <- new.env(); load_capacity_fns(env); codes <- share_codes()
  sec <- data.frame(sectcode = codes, zone = "2", stringsAsFactors = FALSE)
  len <- function(codes_sel, fac = "Sj", zone = "1") {
    nchar(env$share_token_encode(env$scenario_share_state(
      data.frame(sectcode = codes_sel, to_facility = fac, to_zone = zone, stringsAsFactors = FALSE),
      sec, list(wi_travel = TRUE))))
  }
  set.seed(7)
  expect_lt(len(codes[1]), 250)
  expect_lt(len(codes[800:1099]), 300)                            # a box: 300 neighbouring sections
  expect_lt(len(sample(codes, 300)), 700)                         # 300 scattered sections
  expect_lt(len(codes), 300)                                      # every section
  expect_false(identical(env$share_token_encode(list(v = 1L, a = 1)),
                         env$share_token_encode(list(v = 1L, a = 2))))
  # the real section list survives a pack / unpack of the scattered scenario
  pick <- sample(codes, 300)
  u <- env$scenario_unpack(env$scenario_pack(
    data.frame(sectcode = pick, to_facility = "Sj", to_zone = "1", stringsAsFactors = FALSE), codes), codes)
  expect_true(u$ok); expect_setequal(u$changes$sectcode, pick)
})

test_that("the app wires sharing: button, restore hook, banner, spec, map view", {
  app_src <- paste(readLines(file.path(dirname(test_path()), "..", "..", "apps", "capacity_planner", "app.R"),
                             warn = FALSE), collapse = "\n")
  has <- function(x) grepl(x, app_src, fixed = TRUE)
  expect_true(has('actionButton("wi_share"'))
  expect_true(has("observeEvent(input$wi_share"))
  expect_true(has('id = "wi_share_url"')); expect_true(has('`for` = "wi_share_url"'))
  expect_true(has("expand_state = function(state) expand_share(state)"))
  expect_true(has('uiOutput("wi_shared_note")'))
  expect_true(has("scenario_unpack(x, b$sections$sectcode)"))
  expect_true(has("leaflet::setView(rv$lng, rv$lat, rv$z)"))
  expect_true(has("wi_share_link"))
  expect_true(has('SHARE_SETTING_IDS <- setdiff(grep("^wi_", names(DEEPLINK_SPEC), value = TRUE),'))
  expect_true(has('c("wi_techs_per_crew", "wi_drone_offload", "wi_drone_air_offload"))'))
  # every wi_ input the UI creates is a deep-link param, so a share link can never miss one
  m <- gregexpr('(numericInput|selectInput|radioButtons|checkboxInput)\\("(wi_[a-z_]+)"', app_src, perl = TRUE)
  ui_ids <- unique(sub('^.*\\("', "", sub('"$', "", regmatches(app_src, m)[[1]])))
  expect_gt(length(ui_ids), 15)
  spec_ids <- unique(sub('^\\s*([a-z_]+)\\s*=\\s*list\\(input.*$', "\\1",
                         grep("^\\s*wi_[a-z_]+\\s*=\\s*list\\(input", strsplit(app_src, "\n")[[1]], value = TRUE)))
  expect_equal(setdiff(ui_ids, spec_ids), character(0))
})

test_that("staff_apply_edit writes the named facility only and ignores display rounding", {
  env <- new.env(); load_capacity_fns(env); x <- scenario_fixture()
  s <- x$staff; s$techs_per_crew <- c(40 / 7, 6)                 # Sr 5.714..., Sj 6
  # the page shows 5.71 for Sr; an echo of that tagged Sr is within rounding: nothing changes
  expect_identical(env$staff_apply_edit(s, "Sr", "techs_per_crew", 5.71, tol = 0.005), s)
  # a real edit changes that facility and no other
  b <- env$staff_apply_edit(s, "Sj", "techs_per_crew", 7)
  expect_equal(b$techs_per_crew, c(40 / 7, 7))
  # the same stale 5.71 tagged for Sr can never reach Sj
  expect_equal(env$staff_apply_edit(s, "Sr", "techs_per_crew", 5.71)$techs_per_crew[2], 6)
  # clamped, and bad input is ignored
  expect_equal(env$staff_apply_edit(s, "Sj", "drone_offload_pct", 150, 0, 100)$drone_offload_pct[2], 100)
  expect_equal(env$staff_apply_edit(s, "Sj", "techs_per_crew", -4, 0, Inf)$techs_per_crew[2], 0)
  expect_identical(env$staff_apply_edit(s, "Nowhere", "techs_per_crew", 9), s)
  expect_identical(env$staff_apply_edit(s, "Sj", "techs_per_crew", NA_real_), s)
  expect_identical(env$staff_apply_edit(s, "Sj", "techs_per_crew", Inf), s)
  expect_identical(env$staff_apply_edit(s, "Sj", "bogus", 1), s)
  expect_identical(env$staff_apply_edit(s, NULL, "techs_per_crew", 1), s)
  expect_identical(env$staff_apply_edit(s, "Sj", "techs_per_crew", "7"), s)
})

test_that("the shared staffing inputs are written through tagged edits, not read directly", {
  app_src <- paste(readLines(file.path(dirname(test_path()), "..", "..", "apps", "capacity_planner", "app.R"),
                             warn = FALSE), collapse = "\n")
  has <- function(x) grepl(x, app_src, fixed = TRUE)
  expect_true(has("observeEvent(input$wi_staff_edit"))
  expect_true(has("#wi_techs_per_crew, #wi_drone_offload, #wi_drone_air_offload"))
  expect_true(has("fac !== window.wiStaffFac"))                 # edit only while the boxes match
  expect_true(has("window.wiStaffPushing"))                     # server pushes are not edits
  expect_true(has('session$sendCustomMessage("wi_staff_set"'))
  expect_true(has("wi_staff_set"))
  expect_true(has("staff_apply_edit("))
  expect_false(has('updateNumericInput(session, "wi_techs_per_crew"'))
  # reading the shared inputs to write staff() is what let a stale value bounce between facilities
  expect_false(has("observeEvent(input$wi_techs_per_crew"))
  expect_false(has("observeEvent(input$wi_drone_offload"))
  expect_false(has("observeEvent(input$wi_drone_air_offload"))
})

# ---- Shared cache for the heavy queries -----------------------------------------------------
fake_store <- function() {
  e <- new.env()
  list(env = e,
       get = function(k) e[[k]],
       set = function(k, v, ttl) { e[[k]] <- v; e$.last_ttl <- ttl; TRUE })
}

test_that("capacity_cached: a hit never calls the loader; a miss loads and stores", {
  env <- new.env(); load_capacity_fns(env)
  st <- fake_store(); calls <- 0
  loader <- function() { calls <<- calls + 1; data.frame(a = 1:3) }
  r1 <- env$capacity_cached("sections", loader, store = st, ttl = 60, fingerprint = "fp")
  expect_false(r1$cached); expect_equal(calls, 1); expect_equal(r1$data$a, 1:3)
  expect_s3_class(r1$at, "POSIXct")
  expect_equal(st$env$.last_ttl, 60)
  r2 <- env$capacity_cached("sections", loader, store = st, ttl = 60, fingerprint = "fp")
  expect_true(r2$cached); expect_equal(calls, 1)                    # no second query
  expect_equal(r2$data, r1$data); expect_equal(r2$at, r1$at)        # same data, same "as of" time
  # the key names the data set, the cache version and the fingerprint
  expect_match(setdiff(ls(st$env, all.names = FALSE), ".last_ttl"),
               "^capacity:sections:v[0-9]+:fp$")
  # other data sets and other fingerprints are separate entries
  env$capacity_cached("priority", loader, store = st, fingerprint = "fp")
  env$capacity_cached("sections", loader, store = st, fingerprint = "other")
  expect_equal(calls, 3)
})

test_that("capacity_cached: refresh bypasses the cache and replaces what everyone sees", {
  env <- new.env(); load_capacity_fns(env)
  st <- fake_store(); n <- 0
  loader <- function() { n <<- n + 1; data.frame(v = n) }
  env$capacity_cached("sections", loader, store = st, fingerprint = "fp")
  r <- env$capacity_cached("sections", loader, store = st, fingerprint = "fp", refresh = TRUE)
  expect_false(r$cached); expect_equal(r$data$v, 2)
  again <- env$capacity_cached("sections", loader, store = st, fingerprint = "fp")
  expect_true(again$cached); expect_equal(again$data$v, 2)           # the refreshed value is shared
})

test_that("capacity_cached: an empty or failed load is returned but never stored", {
  env <- new.env(); load_capacity_fns(env)
  st <- fake_store(); calls <- 0
  empty <- function() { calls <<- calls + 1; data.frame() }
  r <- env$capacity_cached("sections", empty, store = st, fingerprint = "fp")
  expect_equal(nrow(r$data), 0); expect_false(r$cached)
  env$capacity_cached("sections", empty, store = st, fingerprint = "fp")
  expect_equal(calls, 2)                                             # tried again, not served from cache
  expect_equal(length(setdiff(ls(st$env), ".last_ttl")), 0)
  expect_null(env$capacity_cached("sections", function() NULL, store = st, fingerprint = "fp")$data)
})

test_that("capacity_cached: no Redis, or a broken one, just loads", {
  env <- new.env(); load_capacity_fns(env)
  loader <- function() data.frame(a = 1)
  expect_equal(env$capacity_cached("sections", loader, store = NULL, fingerprint = "fp")$data$a, 1)
  bad <- list(get = function(k) stop("redis down"), set = function(k, v, ttl) stop("redis down"))
  r <- env$capacity_cached("sections", loader, store = bad, fingerprint = "fp")
  expect_equal(r$data$a, 1); expect_false(r$cached)
  # a stored value of the wrong shape is ignored, not trusted
  junk <- list(get = function(k) "not a cache entry", set = function(k, v, ttl) TRUE)
  expect_false(env$capacity_cached("sections", loader, store = junk, fingerprint = "fp")$cached)
})

test_that("the cache fingerprint follows the RA exclusion file, and the TTL has a safe default", {
  env <- new.env(); load_capacity_fns(env)
  f <- tempfile(fileext = ".csv")
  writeLines(c("sitecode", "190101-001"), f)
  a <- env$capacity_cache_fingerprint(f)
  expect_identical(env$capacity_cache_fingerprint(f), a)
  writeLines(c("sitecode", "190101-001", "190101-002"), f)
  b <- env$capacity_cache_fingerprint(f)
  expect_false(identical(a, b))                                      # edited file: new key, no stale counts
  expect_false(identical(env$capacity_cache_fingerprint(tempfile()), a))   # missing file is its own state
  expect_match(a, "^[0-9a-f]{8}$")
  ttl <- env$capacity_cache_ttl()
  expect_true(is.integer(ttl) && ttl > 0)
})

test_that("files are read once per process and again when their content changes", {
  env <- new.env(); load_capacity_fns(env)
  f <- tempfile(fileext = ".csv")
  writeLines(c("sitecode", "A-1", "A-2"), f)
  s1 <- env$ra_exclusion_sql("b", f)
  expect_match(s1, "A-1,A-2", fixed = TRUE)
  expect_identical(env$ra_exclusion_sql("b", f), s1)
  expect_match(env$ra_exclusion_sql("x", f), "x.sitecode", fixed = TRUE)   # alias still applied per call
  writeLines(c("sitecode", "B-9"), f)
  expect_match(env$ra_exclusion_sql("b", f), "B-9", fixed = TRUE)
  expect_false(grepl("A-1", env$ra_exclusion_sql("b", f), fixed = TRUE))
  # routed drive times: the same parsed object comes back until the file changes
  csv <- stub_routed_csv()
  r1 <- env$load_routed_drivetimes(csv); r2 <- env$load_routed_drivetimes(csv)
  expect_identical(r1, r2)
  d <- read.csv(csv, stringsAsFactors = FALSE); d$drive_min[1] <- 99
  write.csv(d, csv, row.names = FALSE)
  expect_equal(env$load_routed_drivetimes(csv)$drive_min_routed[1], 99)
})

test_that("the app, start-up and config are wired for the shared cache", {
  root <- file.path(dirname(test_path()), "..", "..")
  app_src <- paste(readLines(file.path(root, "apps", "capacity_planner", "app.R"), warn = FALSE), collapse = "\n")
  has <- function(x) grepl(x, app_src, fixed = TRUE)
  expect_true(has('capacity_cached("sections", load_capacity_sections, refresh = refresh)'))
  expect_true(has('capacity_cached("priority", load_capacity_by_priority, refresh = refresh)'))
  expect_true(has('capacity_cached("geo", load_section_geo, ttl = 7L * 86400L)'))
  expect_false(has("geo <- load_section_geo()"))
  expect_true(has('uiOutput("data_asof")'))
  expect_true(has('actionButton("data_refresh"'))
  expect_true(has("observeEvent(input$data_refresh"))
  expect_true(has("time.local-time"))
  # the heavy loaders are no longer called straight from the page
  expect_false(has("sections <- tryCatch(load_capacity_sections()"))
  startup <- paste(readLines(file.path(root, "startup.sh"), warn = FALSE), collapse = "\n")
  expect_true(grepl("warm_capacity_cache()", startup, fixed = TRUE))
  expect_equal(length(gregexpr("\n *warm_capacity_planner *\r?\n", startup)[[1]]), 2)   # both modes
  cfg <- paste(readLines(file.path(root, "config", "app_config.yaml"), warn = FALSE), collapse = "\n")
  expect_true(grepl("capacity_planner: 86400", cfg, fixed = TRUE))
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
  expect_true(has("aria-label=\"Promote section"))
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
  # a clicked (selected) table row keeps readable text: DataTables' default is white on mid blue
  expect_true(has("html:root { --dt-row-selected: 205, 222, 250; --dt-row-selected-text: 31, 41, 51;"))
})

test_that("What-If page: no priority filter, no section 5, per-tech above the estimates, instant map", {
  app_src <- paste(readLines(file.path(dirname(test_path()), "..", "..", "apps", "capacity_planner", "app.R"),
                             warn = FALSE), collapse = "
")
  has <- function(x) grepl(x, app_src, fixed = TRUE)
  pos <- function(x) regexpr(x, app_src, fixed = TRUE)[1]
  # every section is always shown
  expect_false(has("wi_show_zone")); expect_false(has("Show priority"))
  expect_false(has("wi_show_fac")); expect_false(has("wi_selectall"))
  # the changed-sections list (old section 5) is gone
  expect_false(has('DTOutput("wi_changes")')); expect_false(has("output$wi_changes <-"))
  expect_false(has("Sections this scenario changes"))
  # load per tech and per crew sits above the staffing estimates; drive time + factor in What changed
  expect_gt(pos('h4("4. Staffing estimates")'), pos('h4("3. Load per tech and per crew'))
  expect_gt(pos('h4("3. Load per tech and per crew'), pos('h4("2. What changed")'))
  expect_gt(pos('DT::DTOutput("wi_pertech")'), pos('h4("2. What changed")'))
  expect_lt(pos('DT::DTOutput("wi_pertech")'), pos('DT::DTOutput("wi_a_table")'))
  expect_true(has('"Median drive min to current P1"'))
  expect_true(has('"Median drive min to added sections"'))
  expect_true(has('"Travel ratio (added / current)"'))
  expect_false(has("current / added"))
  expect_false(has('"Capacity factor"')); expect_false(has("Drive min, added"))
  # Estimate A: just the techs (no "n more needed, n freed", no repeated load columns)
  expect_false(has("more needed")); expect_false(has('"P1 now"')); expect_false(has('"P1 after"'))
  # the map is drawn once and restyled in place, not redrawn on every change
  expect_true(has("wi_restyle")); expect_true(has("setStyle({color: ol[i], fillColor: fill[i]"))
  expect_true(has("observeEvent(changes(), restyle_sync()"))
  expect_false(has("fillOpacity = 0.9"))       # leaflet hover highlight would reset the fill
})

# ---- Prehatch means GROUND prehatch ----------------------------------------------------------
test_that("prehatch counts are ground prehatch only: air sites with a prehatch type are excluded", {
  env <- new.env(); load_capacity_fns(env)
  # the definition: ground AND a prehatch type
  expect_match(env$.ground_prehatch_sql, "b.air_gnd = 'G'", fixed = TRUE)
  expect_match(env$.ground_prehatch_sql, "'PREHATCH','BRIQUET','PELLET','PRE1ONLY'", fixed = TRUE)
  # both heavy queries filter every prehatch count / acre sum through it, never by type alone
  root <- file.path(dirname(test_path()), "..", "..")
  src <- paste(readLines(file.path(root, "apps", "capacity_planner", "data_functions.R"), warn = FALSE),
               collapse = "\n")
  expect_equal(length(gregexpr("FILTER (WHERE %s)", src, fixed = TRUE)[[1]]), 4)   # 2 queries x (sites, acres)
  expect_false(grepl("FILTER (WHERE b.prehatch IN", src, fixed = TRUE))
  expect_equal(length(gregexpr(".ground_prehatch_sql, .ground_prehatch_sql", src, fixed = TRUE)[[1]]), 2)
  # the cache key follows the definition, so counts cached under the old (air-inclusive) one are not served
  f <- tempfile(fileext = ".csv"); writeLines(c("sitecode", "190101-001"), f)
  fp_new <- env$capacity_cache_fingerprint(f)
  env$.ground_prehatch_sql <- env$.prehatch_in_sql
  expect_false(identical(env$capacity_cache_fingerprint(f), fp_new))
})

test_that("every prehatch label says ground prehatch", {
  env <- new.env(); load_capacity_fns(env)
  expect_equal(unname(env$CAPACITY_METRIC_LABELS[c("prehatch_sites", "prehatch_acres")]),
               c("Ground prehatch sites", "Ground prehatch acres"))
  root <- file.path(dirname(test_path()), "..", "..")
  app_src <- paste(readLines(file.path(root, "apps", "capacity_planner", "app.R"), warn = FALSE), collapse = "\n")
  # a bare "Prehatch ..." / "PH ..." label would hide that air sites are not in it
  expect_false(grepl('"(P1 )?Prehatch (sites|acres)', app_src))
  expect_false(grepl('`Prehatch acres/', app_src, fixed = TRUE))
  expect_false(grepl('"PH (sites|acres)"', app_src))
  expect_true(grepl("Ground prehatch sites/tech", app_src, fixed = TRUE))
  expect_true(grepl("Ground prehatch acres/crew", app_src, fixed = TRUE))
  # CSV of the changed sections
  expect_true(all(c("ground_prehatch_sites", "ground_prehatch_acres") %in%
                    names(env$scenario_export_changes(NULL))))
})

# ---- 2020 census residents per section ---------------------------------------------------------
pop_fixture <- function() {
  x <- scenario_fixture()
  x$sec$population <- c(1000, 2000, 3000, 4000, 500, 700)
  x$sec$land_sqmi  <- c(1, 1, 1, 2, 1, 1)
  x$sec$density    <- x$sec$population / x$sec$land_sqmi
  x
}
no_census <- function(sec) sec[, setdiff(names(sec), c("population", "land_sqmi", "density"))]

test_that("build script: blocks go to the section holding their internal point, population conserved", {
  skip_if_not_installed("sf")
  root <- file.path(dirname(test_path()), "..", "..")
  e <- new.env(); options(section_population_no_run = TRUE); on.exit(options(section_population_no_run = NULL))
  source(file.path(root, "apps", "capacity_planner", "scripts", "build_section_population.R"), local = e)
  sq <- function(x0, y0, w = 1000) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w),
                                                              c(x0, y0 + w), c(x0, y0))))
  secs <- sf::st_sf(sectcode = c("0000002", "0000001", "0000003"),     # not in sorted order on purpose
                    geometry = sf::st_sfc(sq(480000, 4980000), sq(481000, 4980000), sq(490000, 4990000),
                                          crs = 26915))
  xy <- data.frame(x = c(480500, 481500, 480200, 495000), y = c(4980500, 4980500, 4980900, 4995000))
  ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(xy, coords = c("x", "y"), crs = 26915), 4326))
  blocks <- data.frame(pop = c(100L, 50L, 25L, 7L), land_m2 = c(e$SQMI_M2, e$SQMI_M2 / 2, e$SQMI_M2 / 4, 1000),
                       lon = ll[, 1], lat = ll[, 2])
  r <- e$assign_blocks_to_sections(blocks, secs)
  d <- r$per_section
  expect_equal(d$sectcode, c("0000001", "0000002", "0000003"))          # sorted
  expect_equal(d$population, c(50L, 125L, 0L))
  expect_equal(d$blocks, c(1L, 2L, 0L))
  expect_equal(d$land_sqmi, c(0.5, 1.25, 0))
  expect_equal(d$density, c(100, 100, NA_real_))                        # no land -> NA, not 0 / Inf
  expect_equal(r$block_section, c("0000002", "0000001", "0000002", NA))
  # nothing is lost: the sections plus the blocks outside every section add up to the file
  expect_equal(sum(d$population) + sum(blocks$pop[is.na(r$block_section)]), sum(blocks$pop))
})

test_that("load_section_population / attach_section_population: lookup, fallbacks, one parse per file", {
  env <- new.env(); load_capacity_fns(env)
  f <- tempfile(fileext = ".csv")
  writeLines(c("sectcode,population,land_sqmi,density", "0100001,1200,0.8,9999", "0100002,0,0,NA"), f)
  p <- env$load_section_population(f)
  expect_equal(p$sectcode, c("0100001", "0100002"))
  expect_equal(p$density, c(1500, NA_real_))                            # recomputed from the two counts
  expect_identical(env$load_section_population(f), p)                  # memoized
  writeLines(c("sectcode,population,land_sqmi", "0100001,10,1"), f)     # edited file is re-read
  expect_equal(env$load_section_population(f)$population, 10)
  expect_null(env$load_section_population(tempfile()))                  # missing file
  writeLines(c("a,b", "1,2"), f); expect_null(env$load_section_population(f))   # wrong columns
  s <- data.frame(sectcode = c("0100001", "0999999"), stringsAsFactors = FALSE)
  writeLines(c("sectcode,population,land_sqmi", "0100001,300,2"), f)
  a <- env$attach_section_population(s, env$load_section_population(f))
  expect_equal(a$population, c(300, NA)); expect_equal(a$density, c(150, NA))   # unknown section: NA
  n <- env$attach_section_population(s, NULL)
  expect_true(all(is.na(n$population)) && all(c("population", "land_sqmi", "density") %in% names(n)))
})

test_that("the committed section_population.csv covers every section with plausible numbers", {
  root <- file.path(dirname(test_path()), "..", "..")
  d <- read.csv(file.path(root, "apps", "capacity_planner", "data", "section_population.csv"),
                stringsAsFactors = FALSE, colClasses = c(sectcode = "character"))
  expect_true(all(c("sectcode", "population", "land_sqmi", "density", "blocks") %in% names(d)))
  expect_false(anyDuplicated(d$sectcode) > 0)
  expect_true(all(nchar(d$sectcode) == 7))
  expect_true(all(d$population >= 0) && all(d$land_sqmi >= 0) && !anyNA(d$population))
  expect_equal(sum(d$population), 3163104)                              # the whole 2020 file, none lost
  times <- read.csv(file.path(root, "apps", "capacity_planner", "data", "facility_section_drivetime.csv"),
                    stringsAsFactors = FALSE, colClasses = c(sectcode = "character"))
  expect_true(all(unique(times$sectcode) %in% d$sectcode))              # every routed section has a row
  expect_true(all(is.na(d$density) | d$density <= 40000))
})

test_that("residents roll up per FOS area and are reported by What-If, but never drive Estimate A", {
  env <- new.env(); load_capacity_fns(env); x <- pop_fixture()
  cap <- env$aggregate_capacity_by_fos(x$sec, x$staff)
  p1 <- cap[cap$facility == "Sr" & cap$zone == "1", ]
  expect_equal(p1$population, 3000); expect_equal(p1$density, 1500)     # (1000+2000) / (1+1) sq mi
  expect_true(all(c("population_P1", "density_P1", "population_P2", "density_P2") %in% names(env$capacity_wide(cap))))
  # no census columns at all: still works, residents empty
  cap0 <- env$aggregate_capacity_by_fos(no_census(x$sec), x$staff)
  expect_true(all(cap0$population == 0)); expect_true(all(is.na(cap0$density)))
  w0 <- env$capacity_wide(cap0)
  expect_equal(ncol(env$drop_empty_population(w0)) + 4, ncol(w0))
  # What-If: promoting s3 (3,000 residents) adds them to Sr's P1
  a <- env$scenario_apply(x$sec, chg("s3", NA, "1"), x$times)
  cmp <- env$scenario_compare(a, "1", env$SCENARIO_CHANGE_METRICS)
  expect_equal(cmp$population_chg[cmp$key == "Sr"], 3000)
  expect_equal(cmp$population_chg[cmp$key == "DISTRICT"], 3000)
  # population is not a workload metric: Estimate A ignores it
  expect_false(any(c("population", "density") %in% env$SCENARIO_METRICS))
  expect_true("population" %in% env$SCENARIO_CHANGE_METRICS)
  s_pop <- env$scenario_staffing(a, x$staff, metric = "population", per_tech = 100)
  s_gnd <- env$scenario_staffing(a, x$staff, metric = "ground_sites", per_tech = 100)
  expect_equal(s_pop$by_fos, s_gnd$by_fos)
  # a scenario table without the column does not fail
  b <- env$scenario_apply(no_census(x$sec), chg("s3", NA, "1"), x$times)
  expect_false("population_chg" %in% names(env$scenario_compare(b, "1", env$SCENARIO_CHANGE_METRICS)))
})

test_that("CSV exports carry the residents of what changed", {
  env <- new.env(); load_capacity_fns(env); x <- pop_fixture()
  a <- env$scenario_apply(x$sec, rbind(chg("s3", NA, "1"), chg("s4", "Sj", "1")), x$times)
  e <- env$scenario_export_changes(a)
  expect_equal(e$population, c(3000, 4000)); expect_equal(e$residents_per_sq_mi, c(3000, 2000))
  cmp <- env$scenario_compare(a, "1", env$SCENARIO_CHANGE_METRICS)
  ex <- env$scenario_export_compare(cmp)
  expect_equal(ex$population_p1_change[ex$facility == "DISTRICT"], 7000)
  # no census data: empty columns rather than an error
  b <- env$scenario_apply(no_census(x$sec), chg("s3", NA, "1"), x$times)
  expect_true(is.na(env$scenario_export_changes(b)$population))
})

test_that("map shading: classes, no-data grey, legends", {
  env <- new.env(); load_capacity_fns(env)
  a <- data.frame(zone = c("1", "2", "2", "2", "2", "1"),
                  density = c(0, 299.9, 300, 1200, NA, 99999),
                  population = c(0, 10, 260, 1500, NA, 9000), stringsAsFactors = FALSE)
  d <- env$shade_fill(a, "density")
  expect_equal(d[1], env$SHADE_COLORS[1]); expect_equal(d[2], env$SHADE_COLORS[1])
  expect_equal(d[3], env$SHADE_COLORS[2])                               # 300 starts the second class
  expect_equal(d[4], env$SHADE_COLORS[4])                               # 1,000 to 2,500
  expect_equal(d[5], env$SHADE_NO_DATA); expect_equal(d[6], env$SHADE_COLORS[9])
  r <- env$shade_fill(a, "residents")
  expect_equal(r[3], env$SHADE_COLORS[2]); expect_equal(r[4], env$SHADE_COLORS[4]); expect_equal(r[6], env$SHADE_COLORS[9])
  expect_null(env$shade_fill(a, "facility"))
  expect_null(env$shade_fill(a, "hotspot"))                             # the removed mode is not a mode
  od <- env$shade_opacity(a, "density")
  expect_true(all(od[c(1, 6)] > od[c(2, 3)]))                           # P1 stronger than P2
  expect_equal(env$shade_opacity(a, "facility"), ifelse(a$zone == "1", 0.75, 0.15))   # the original look
  # a table with no census columns gets the no-data color, not an error
  expect_equal(env$shade_fill(data.frame(zone = "1"), "density"), env$SHADE_NO_DATA)
  # legends: one color per label, no-data entry, readable class names
  for (m in c("density", "residents")) {
    lg <- env$shade_legend(m)
    expect_equal(length(lg$colors), length(lg$labels)); expect_true(nzchar(lg$title))
  }
  expect_equal(env$shade_legend("density")$labels[c(1, 2, 9, 10)],
               c("Under 300", "300 to 600", "7,500 or more", "No land / no data"))
  expect_null(env$shade_legend("facility"))
  expect_equal(unname(env$SHADE_MODES), c("facility", "density", "residents"))
  expect_null(env$shade_legend("hotspot"))
  expect_false(exists("SHADE_HOTSPOT_DENSITY", envir = env)); expect_false(exists("shade_dense", envir = env))
})

test_that("drop_empty_population removes empty residents columns only", {
  env <- new.env(); load_capacity_fns(env)
  df <- data.frame(ground_sites_P1 = 1:2, population_P1 = c(NA, 0), density_P1 = c(NA, NA),
                   population_P2 = c(5, 6), stringsAsFactors = FALSE)
  expect_equal(names(env$drop_empty_population(df)), c("ground_sites_P1", "population_P2"))
  expect_equal(names(env$drop_empty_population(data.frame(a = 1))), "a")
})

test_that("the app wires residents shading and keeps residents out of the estimate choices", {
  root <- file.path(dirname(test_path()), "..", "..")
  app_src <- paste(readLines(file.path(root, "apps", "capacity_planner", "app.R"), warn = FALSE), collapse = "\n")
  has <- function(x) grepl(x, app_src, fixed = TRUE)
  expect_true(has('radioButtons("wi_shade"'))
  expect_true(has('wi_shade       = list(input = "wi_shade",     type = "radio")'))   # deep link + shared link
  expect_true(has("observeEvent(input$wi_shade"))
  expect_true(has("attach_section_population(sections)"))
  expect_true(has('layerId = "wi_legend"'))
  # Estimate A driver and the metric choices must not offer residents
  m <- regmatches(app_src, regexpr("METRIC_CHOICES <- c\\([^)]*\\)", app_src))
  expect_false(grepl("population|density", m))
  expect_true(has("SCENARIO_CHANGE_METRICS"))
  expect_false(grepl("scenario_staffing\\([^)]*SCENARIO_CHANGE_METRICS", app_src))
})

test_that("shaded-mode outlines are thin and keep the facility color", {
  env <- new.env(); load_capacity_fns(env)
  a <- data.frame(zone = c("1", "2", "2", "1"), density = c(5000, 5000, 200, NA), stringsAsFactors = FALSE)
  fc <- c("#111111", "#222222", "#333333", "#444444")
  o <- env$shade_outline(a, "density", fc)
  expect_equal(o$ol, fc)                                              # facility stays on the outline
  expect_equal(o$w, c(1, 0.5, 0.5, 1)); expect_equal(o$dash, c("", "4 3", "4 3", ""))
  expect_lt(max(o$w), 2)                                              # thin: the fill must show at district zoom
})
