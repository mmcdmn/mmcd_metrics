# =============================================================================
# STUB DATA FOR BIOASSAY EFFICACY TESTING
# =============================================================================
# Stub data for the bioassay (action='P') tab of the control_efficacy app.
# Each row is a deliberate scenario with a known expected outcome; see
# get_stub_bioassay_expected() for the contract the tests assert against.
# =============================================================================

#' Bioassay rows as load_bioassay_data() receives them from SQL
#'
#' Shaped like the LATERAL query output: one row per bioassay, with the
#' already-matched prior treatment attached.
get_stub_bioassay_rows <- function() {
  data.frame(
    pkey_pg  = 9001:9010,
    sitecode = paste0("BIO-", sprintf("%03d", 1:10)),
    inspdate = as.Date(c(
      "2024-05-10",  # 1 scored, within window, Aedes
      "2024-07-15",  # 2 PENDING - emergedcnt NA
      "2024-07-20",  # 3 zero pupae
      "2024-07-22",  # 4 no species data -> Unknown
      "2024-05-12",  # 5 within window (5 days after trt, effect_days 21)
      "2024-07-25",  # 6 past expiry (effect_days 14, lands 40 days past expiry)
      "2024-07-28",  # 7 untreated - no prior treatment
      "2024-07-30",  # 8 scored, zero emergence -> 100%
      "2024-07-26",  # 9 action '1' Inspect + Treat (Ground)
      "2024-07-27"   # 10 action '6' Treat Catch Basins
    )),
    pupaecnt   = c(100L, 50L,  0L, 80L, 40L, 60L, 25L, 10L, 20L, 10L),
    emergedcnt = c( 10L,  NA, NA, 20L,  4L, 48L,  5L,  0L,  5L,  2L),
    sampnum_yr = c("24-70001", "24-70002", "24-70003", NA_character_,
                   "24-70005", "24-70006", "24-70007", "24-70008",
                   "24-70009", "24-70010"),
    numdip   = c(2, 3, 0, 1, 2, 4, 1, 1, 2, 1),
    facility = c("N", "N", "N", "Sj", "N", "N", "Sj", "N", "N", "E"),
    zone     = rep("1", 10),
    prior_trt_date = as.Date(c(
      "2024-05-05",  # 1 within (effect 21)
      "2024-07-10",  # 2
      "2024-07-15",  # 3
      "2024-07-18",  # 4
      "2024-05-07",  # 5 within: +5 days, effect 21
      "2024-06-01",  # 6 past expiry: effect 14 -> 40 days past
      NA,            # 7 untreated
      "2024-07-29",  # 8 effect_days NA -> 14-day default
      "2024-07-20",  # 9 within (effect 21)
      "2024-07-25"   # 10 within (effect 30)
    )),
    prior_trt_material    = c("19", "19", "19", "19", "19", "22",
                              NA_character_, "22", "19", "G2"),
    prior_trt_mattype     = c(rep("Bti_Granule", 5), "Methoprene_Pellet",
                              NA_character_, "Methoprene_Pellet",
                              "Bti_Granule", "Alt_P35"),
    prior_trt_effect_days = c(21L, 21L, 21L, 21L, 21L, 14L, NA, NA, 21L, 30L),
    trt_action            = c("A", "A", "3", "D", "A", "3",
                              NA_character_, "A", "1", "6"),
    tdose                 = c(rep(5, 6), NA, 5, 5, 2.5),
    dosage_label          = c(rep("5 lb / acre", 6), NA_character_,
                              "5 lb / acre", "5 lb / acre", "2.5 lb / acre"),
    active_ingredient     = c(rep("Bti", 5), "Methoprene", NA_character_,
                              "Methoprene", "Bti", "Methoprene"),
    stringsAsFactors = FALSE
  )
}


#' Species composition keyed on sampnum_yr
#'
#' Row 4's sample is deliberately absent so the Unknown-genus fallback is
#' exercised. Row 7 is a Culex-dominant mix to check the argmax tie-break.
get_stub_bioassay_species <- function() {
  data.frame(
    sampnum_yr = c("24-70001", "24-70001",
                   "24-70002",
                   "24-70003",
                   "24-70005",
                   "24-70006",
                   "24-70007", "24-70007",
                   "24-70008",
                   "24-70009",
                   "24-70010"),
    genus = c("Aedes", "Culex",
              "Aedes",
              "Culex",
              "Aedes",
              "Culex",
              "Aedes", "Culex",
              "Aedes",
              "Aedes",
              "Culex"),
    total_pct = c(80, 20,
                  100,
                  100,
                  95,
                  100,
                  30, 70,
                  100,
                  100,
                  100),
    stringsAsFactors = FALSE
  )
}


#' Spring/summer thresholds so season does not need a database
get_stub_bioassay_thresholds <- function() {
  data.frame(
    year = c(2024, 2025),
    date_start = as.Date(c("2024-06-15", "2025-06-15")),
    stringsAsFactors = FALSE
  )
}


#' Expected outcomes per pkey_pg, in the same order as get_stub_bioassay_rows()
get_stub_bioassay_expected <- function() {
  data.frame(
    pkey_pg = 9001:9010,
    pct_inhibition = c(90, NA, NA, 75, 90, 20, 80, 100, 75, 80),
    is_pending     = c(FALSE, TRUE, FALSE, FALSE, FALSE,
                       FALSE, FALSE, FALSE, FALSE, FALSE),
    is_zero_pupae  = c(FALSE, FALSE, TRUE, FALSE, FALSE,
                       FALSE, FALSE, FALSE, FALSE, FALSE),
    genus          = c("Aedes", "Aedes", "Culex", "Unknown", "Aedes",
                       "Culex", "Culex", "Aedes", "Aedes", "Culex"),
    has_species    = c(TRUE, TRUE, TRUE, FALSE, TRUE,
                       TRUE, TRUE, TRUE, TRUE, TRUE),
    season         = c("Spring", "Summer", "Summer", "Summer", "Spring",
                       "Summer", "Summer", "Summer", "Summer", "Summer"),
    trt_type       = c("Air", "Air", "Ground", "Drone", "Air",
                       "Ground", "None", "Air", "Ground", "Catch Basin"),
    material_window = c("Within window", "Within window", "Within window",
                        "Within window", "Within window", "Past expiry",
                        "Untreated", "Within window", "Within window",
                        "Within window"),
    stringsAsFactors = FALSE
  )
}
