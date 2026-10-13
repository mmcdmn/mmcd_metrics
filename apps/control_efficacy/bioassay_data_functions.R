# bioassay_data_functions.R
# Bioassay (action='P') emergence-inhibition analysis.
# Depends on: data_functions.R (get_matcodes_by_ingredient, load_dosage_options)
#

library(DBI)
library(RPostgres)

# Treatment actions a bioassay can be attributed to. '1' (Inspect + Treat,
# ground) and '6' (Treat Catch Basins) carry a matcode and are real treatments;
# omitting them made most bioassays report "Untreated".
BIOASSAY_TRT_ACTIONS <- c("A", "D", "3", "1", "6")
BIOASSAY_DEFAULT_EFFECT_DAYS <- 14L


#' Classify a bioassay against its prior treatment's effective window
#'
#' @param bio_date Date vector of bioassay inspection dates
#' @param prior_trt_expiry Date vector of treatment expiry; NA when untreated
#' @return Character vector: "Untreated", "Within window", or "Past expiry"
classify_material_window <- function(bio_date, prior_trt_expiry) {
  n <- length(bio_date)
  if (n == 0) return(character(0))

  out <- rep("Past expiry", n)
  out[is.na(prior_trt_expiry)] <- "Untreated"

  within <- !is.na(prior_trt_expiry) & !is.na(bio_date) & bio_date <= prior_trt_expiry
  out[within] <- "Within window"
  out
}


#' get genus, season, inhibition, and material window for bioassay rows
#'
#'
#' @param bio Data frame with at least: inspdate, pupaecnt, emergedcnt,
#'   sampnum_yr, trt_action, prior_trt_date, prior_trt_effect_days
#' @param genus_tbl Data frame of sampnum_yr / genus / total_pct, or NULL
#' @param thresholds Spring threshold table (year, date_start); NULL to look up
#' @return bio with the derived columns added
derive_bioassay_metrics <- function(bio, genus_tbl = NULL, thresholds = NULL) {

  if (is.null(bio) || nrow(bio) == 0) return(bio)

  bio$inspdate       <- as.Date(bio$inspdate)
  bio$prior_trt_date <- as.Date(bio$prior_trt_date)
  bio$pupaecnt       <- as.integer(bio$pupaecnt)
  bio$emergedcnt     <- as.integer(bio$emergedcnt)
  bio$genus       <- "Unknown"
  bio$genus_pct   <- NA_real_
  bio$has_species <- FALSE

  if (!is.null(genus_tbl) && nrow(genus_tbl) > 0) {
    dom <- genus_tbl[order(genus_tbl$sampnum_yr, -genus_tbl$total_pct, genus_tbl$genus), ]
    dom <- dom[!duplicated(dom$sampnum_yr), ]
    idx <- match(bio$sampnum_yr, dom$sampnum_yr)
    hit <- !is.na(idx)
    bio$genus[hit]       <- dom$genus[idx[hit]]
    bio$genus_pct[hit]   <- dom$total_pct[idx[hit]]
    bio$has_species[hit] <- TRUE
  }

  bio$year   <- as.integer(format(bio$inspdate, "%Y"))
  bio$season <- NA_character_

  if (is.null(thresholds)) thresholds <- get_spring_date_thresholds()
  if (!is.null(thresholds) && nrow(thresholds) > 0) {
    t_idx <- match(bio$year, as.integer(thresholds$year))
    thr   <- thresholds$date_start[t_idx]
    bio$season <- ifelse(
      is.na(thr) | is.na(bio$inspdate), NA_character_,
      ifelse(bio$inspdate < thr, "Spring", "Summer")
    )
  }

  bio$trt_type <- dplyr::case_when(
    bio$trt_action == "A" ~ "Air",
    bio$trt_action == "D" ~ "Drone",
    bio$trt_action == "3" ~ "Ground",
    bio$trt_action == "1" ~ "Ground",
    bio$trt_action == "6" ~ "Catch Basin",
    TRUE ~ "None"
  )

  # Emergence inhibition. Pending (emergedcnt NULL) and zero-pupae rows are
  # kept but score NA -- a pending bioassay is not 100% inhibition.
  bio$is_pending    <- !is.na(bio$pupaecnt) & bio$pupaecnt > 0 & is.na(bio$emergedcnt)
  bio$is_zero_pupae <- !is.na(bio$pupaecnt) & bio$pupaecnt == 0

  scoreable <- !is.na(bio$pupaecnt) & bio$pupaecnt > 0 & !is.na(bio$emergedcnt)
  bio$pct_inhibition <- NA_real_
  bio$pct_inhibition[scoreable] <-
    100 * (1 - bio$emergedcnt[scoreable] / bio$pupaecnt[scoreable])

  # Material effect window
  bio$prior_trt_effect_days <- as.integer(bio$prior_trt_effect_days)
  has_trt <- !is.na(bio$prior_trt_date)
  bio$prior_trt_effect_days[has_trt & is.na(bio$prior_trt_effect_days)] <-
    BIOASSAY_DEFAULT_EFFECT_DAYS

  bio$prior_trt_expiry <- as.Date(NA)
  bio$prior_trt_expiry[has_trt] <-
    bio$prior_trt_date[has_trt] + bio$prior_trt_effect_days[has_trt]

  bio$days_from_trt    <- as.integer(bio$inspdate - bio$prior_trt_date)
  bio$material_window  <- classify_material_window(bio$inspdate, bio$prior_trt_expiry)
  bio$days_past_expiry <- NA_integer_
  past <- bio$material_window == "Past expiry"
  bio$days_past_expiry[past] <-
    as.integer(bio$inspdate[past] - bio$prior_trt_expiry[past])

  rownames(bio) <- NULL
  bio
}


#' Load bioassay data with emergence inhibition, genus, and material window
#'
#' @param start_year Integer start year
#' @param end_year Integer end year
#' @param facility_filter Facility code ("all" for no filter)
#' @return Data frame, exactly one row per bioassay, or NULL
load_bioassay_data <- function(start_year, end_year, facility_filter = "all") {

  con <- get_db_connection()
  if (is.null(con)) return(NULL)

  start_date <- paste0(start_year, "-01-01")
  end_date   <- paste0(end_year,   "-12-31")

  tryCatch({
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    need_archive <- as.integer(start_year) < current_year
    need_current <- as.integer(end_year)  >= current_year

    # A bioassay early in a year can be covered by a treatment from the previous year
    trt_lookback <- as.character(as.Date(start_date) - 120)

    bio_tpl <- "
      SELECT inspdate, insptime, sitecode, pupaecnt, emergedcnt,
             sampnum_yr, numdip, facility, pkey_pg
      FROM %s
      WHERE action = 'P'
        AND inspdate >= '%s' AND inspdate <= '%s'
    "

    trt_tpl <- paste0("
      SELECT sitecode, inspdate, insptime, matcode, action
      FROM %s
      ", build_sql_in_clause("action", BIOASSAY_TRT_ACTIONS, prefix = "WHERE "), "
        AND matcode IS NOT NULL AND matcode <> ''
        AND inspdate >= '%s' AND inspdate <= '%s'
    ")

    bio_parts <- character(0)
    trt_parts <- character(0)
    if (need_current) {
      bio_parts <- c(bio_parts, sprintf(bio_tpl, "dblarv_insptrt_current", start_date, end_date))
      trt_parts <- c(trt_parts, sprintf(trt_tpl, "dblarv_insptrt_current", trt_lookback, end_date))
    }
    if (need_archive) {
      bio_parts <- c(bio_parts, sprintf(bio_tpl, "dblarv_insptrt_archive", start_date, end_date))
      trt_parts <- c(trt_parts, sprintf(trt_tpl, "dblarv_insptrt_archive", trt_lookback, end_date))
    }

    # picks the single most recent treatment at or before each bioassay.
    # Same-day treatments count unless both timestamps exist and the treatment
    # landed after the pupae were collected.
    query <- sprintf("
      WITH all_trts AS (%s)
      SELECT
        b.pkey_pg,
        b.sitecode,
        b.inspdate,
        b.pupaecnt,
        b.emergedcnt,
        b.sampnum_yr,
        b.numdip,
        COALESCE(gis.facility, b.facility) AS facility,
        gis.zone,
        pt.prior_trt_date,
        pt.prior_trt_material,
        pt.prior_trt_mattype,
        pt.prior_trt_effect_days,
        pt.trt_action
      FROM (%s) b
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(b.sitecode, 7)
      LEFT JOIN LATERAL (
        SELECT t.inspdate     AS prior_trt_date,
               t.matcode      AS prior_trt_material,
               mt.mattype     AS prior_trt_mattype,
               mt.effect_days AS prior_trt_effect_days,
               t.action       AS trt_action
        FROM all_trts t
        LEFT JOIN mattype_list_targetdose mt ON t.matcode = mt.matcode
        WHERE t.sitecode = b.sitecode
          AND t.inspdate <= b.inspdate
          AND (t.inspdate < b.inspdate
               OR t.insptime IS NULL
               OR b.insptime IS NULL
               OR t.insptime <= b.insptime)
        ORDER BY t.inspdate DESC, t.insptime DESC NULLS LAST
        LIMIT 1
      ) pt ON TRUE
    ",
      paste(trt_parts, collapse = " UNION ALL "),
      paste(bio_parts, collapse = " UNION ALL ")
    )

    bio <- dbGetQuery(con, query)
    if (is.null(bio) || nrow(bio) == 0) {
      safe_disconnect(con)
      message("[bioassay] No action='P' records in range")
      return(NULL)
    }
    message(sprintf("[bioassay] %d bioassays", nrow(bio)))

    # -------------------------------------------------------------------
    # Material details for the matched treatment
    # -------------------------------------------------------------------
    bio$tdose             <- NA_real_
    bio$dosage_label      <- NA_character_
    bio$active_ingredient <- NA_character_

    mat_codes <- unique(bio$prior_trt_material[!is.na(bio$prior_trt_material)])
    if (length(mat_codes) > 0) {
      mat_list <- paste0("'", paste(mat_codes, collapse = "','"), "'")
      mat_details <- dbGetQuery(con, sprintf("
        SELECT DISTINCT t.matcode, t.tdose, t.unit, t.area, m.active_ingredient
        FROM mattype_list_targetdose t
        JOIN mattype_list m ON t.mattype = m.mattype
        WHERE t.matcode IN (%s)
      ", mat_list))

      if (!is.null(mat_details) && nrow(mat_details) > 0) {
        mat_details$dosage_label <- paste(mat_details$tdose, mat_details$unit, "/", mat_details$area)
        # A matcode can carry several target doses; collapse to one so the
        # one-row-per-bioassay contract holds.
        mat_details <- mat_details[!duplicated(mat_details$matcode), ]
        idx <- match(bio$prior_trt_material, mat_details$matcode)
        hit <- !is.na(idx)
        bio$tdose[hit]             <- mat_details$tdose[idx[hit]]
        bio$dosage_label[hit]      <- mat_details$dosage_label[idx[hit]]
        bio$active_ingredient[hit] <- mat_details$active_ingredient[idx[hit]]
      }
    }

    # -------------------------------------------------------------------
    # Genus from the bioassay's own larval sample
    # -------------------------------------------------------------------
    genus_tbl <- NULL
    samples <- unique(bio$sampnum_yr[!is.na(bio$sampnum_yr) & bio$sampnum_yr != ""])

    if (length(samples) > 0) {
      sample_list <- paste0("'", paste(samples, collapse = "','"), "'")
      genus_tbl <- dbGetQuery(con, sprintf("
        SELECT sampnum_yr, genus, SUM(per) AS total_pct
        FROM (
          SELECT spp.sampnum_yr, spec.genus, spp.per
          FROM dblarv_species_current spp
          JOIN lookup_specieslist spec ON spp.spp = CAST(spec.sppcode AS VARCHAR)
          WHERE spp.sampnum_yr IN (%s) AND spec.genus IN ('Aedes','Culex') AND spp.per IS NOT NULL
          UNION ALL
          SELECT spp.sampnum_yr, spec.genus, spp.per
          FROM dblarv_species_archive spp
          JOIN lookup_specieslist spec ON spp.spp = CAST(spec.sppcode AS VARCHAR)
          WHERE spp.sampnum_yr IN (%s) AND spec.genus IN ('Aedes','Culex') AND spp.per IS NOT NULL
        ) sub
        GROUP BY sampnum_yr, genus
      ", sample_list, sample_list))
    }

    safe_disconnect(con)

    bio <- derive_bioassay_metrics(bio, genus_tbl = genus_tbl)

    if (facility_filter != "all") {
      bio <- bio[!is.na(bio$facility) & bio$facility == facility_filter, ]
      if (nrow(bio) == 0) return(NULL)
    }

    message(sprintf(
      "[bioassay] Final: %d rows | scored=%d pending=%d zero_pupae=%d | within=%d past_expiry=%d untreated=%d",
      nrow(bio),
      sum(!is.na(bio$pct_inhibition)),
      sum(bio$is_pending, na.rm = TRUE),
      sum(bio$is_zero_pupae, na.rm = TRUE),
      sum(bio$material_window == "Within window"),
      sum(bio$material_window == "Past expiry"),
      sum(bio$material_window == "Untreated")
    ))

    rownames(bio) <- NULL
    return(bio)

  }, error = function(e) {
    message("Error loading bioassay data: ", e$message)
    if (exists("con") && !is.null(con)) {
      try(safe_disconnect(con), silent = TRUE)
    }
    return(NULL)
  })
}


#' Filter bioassay data for display
#'
#' @param bio_data Data frame from load_bioassay_data()
#' @param genus "Both" for no filter, else "Aedes"/"Culex"/"Unknown"
#' @param allowed_matcodes Character vector of matcodes, or NULL for no filter
#' @return Filtered data frame, or NULL when nothing matches
filter_bioassay_data <- function(bio_data,
                                 genus = "Both",
                                 seasons = NULL,
                                 trt_types = NULL,
                                 windows = NULL,
                                 allowed_matcodes = NULL,
                                 dosage = "all",
                                 facility = "all") {

  if (is.null(bio_data) || nrow(bio_data) == 0) return(NULL)
  d <- bio_data

  if (length(seasons) == 0) return(NULL)
  d <- d[!is.na(d$season) & d$season %in% seasons, ]

  if (length(trt_types) == 0) return(NULL)
  d <- d[d$trt_type %in% trt_types, ]

  if (length(windows) == 0) return(NULL)
  d <- d[d$material_window %in% windows, ]

  if (!is.null(genus) && genus != "Both") {
    d <- d[d$genus == genus, ]
  }

  if (!is.null(allowed_matcodes)) {
    d <- d[!is.na(d$prior_trt_material) & d$prior_trt_material %in% allowed_matcodes, ]
  }

  if (!is.null(dosage) && dosage != "all") {
    d <- d[!is.na(d$dosage_label) & d$dosage_label == dosage, ]
  }

  if (!is.null(facility) && facility != "all") {
    d <- d[!is.na(d$facility) & d$facility == facility, ]
  }

  if (nrow(d) == 0) return(NULL)
  rownames(d) <- NULL
  d
}


#' Summarize bioassay completion and material-window counts
#'
#' @param bio_data Data frame from load_bioassay_data()
#' @return Named list of integer counts
summarize_bioassay_pending <- function(bio_data) {
  if (is.null(bio_data) || nrow(bio_data) == 0) {
    return(list(total = 0L, scored = 0L, pending = 0L, zero_pupae = 0L,
                within_window = 0L, past_expiry = 0L, untreated = 0L))
  }

  list(
    total         = nrow(bio_data),
    scored        = sum(!is.na(bio_data$pct_inhibition)),
    pending       = sum(bio_data$is_pending, na.rm = TRUE),
    zero_pupae    = sum(bio_data$is_zero_pupae, na.rm = TRUE),
    within_window = sum(bio_data$material_window == "Within window"),
    past_expiry   = sum(bio_data$material_window == "Past expiry"),
    untreated     = sum(bio_data$material_window == "Untreated")
  )
}
