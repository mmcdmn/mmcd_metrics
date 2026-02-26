# =============================================================================
# EFFICACY TAB: Genus-level % reduction analysis
# Depends on: data_functions.R (get_matcodes_by_ingredient, load_dosage_options)
# =============================================================================

#' Load complete efficacy data for % reduction box plots
#'
#' Pipeline:
#'   1. SQL: Bulk-fetch post-treatment checks + all records for those sites
#'   2. R:   Vectorized matching via dplyr (no loops)
#'   3. SQL: Bulk-load species genus percentages
#'   4. R:   Compute genus dip counts, % reduction, season
#'
#' @param start_year Integer start year (e.g. 2020)
#' @param end_year Integer end year (e.g. 2025)
#' @param bti_only Logical. If TRUE, filter treatments to Bti larvicide matcodes.
#' @param use_mullas Logical. If TRUE, use Mulla's formula.
#' @return Data frame (long format) one row per genus per checkback
load_efficacy_data <- function(start_year, end_year, bti_only = FALSE, use_mullas = FALSE) {
  
  con <- get_db_connection()
  if (is.null(con)) return(NULL)
  
  start_date <- paste0(start_year, "-01-01")
  end_date   <- paste0(end_year,   "-12-31")
  
  tryCatch({
    # -------------------------------------------------------------------
    # BTI MATCODE FILTER 
    # -------------------------------------------------------------------
    bti_codes <- NULL
    if (bti_only) {
      bti_codes <- get_matcodes_by_ingredient(con, "Bti")
      if (length(bti_codes) == 0) {
        message("[efficacy] No Bti matcodes found")
        safe_disconnect(con)
        return(NULL)
      }
      message(sprintf("[efficacy] Bti filter: %d matcodes", length(bti_codes)))
    }
    
    wider_start <- as.character(as.Date(start_date) - 90)
    
    # -------------------------------------------------------------------
    # STEP 1a: Get all post-treatment checks (always union current+archive)
    # -------------------------------------------------------------------
    post_sql <- sprintf("
      SELECT pkey_pg, inspdate AS post_date, insptime AS post_time,
             sitecode, numdip AS post_numdip, sampnum_yr AS post_sampnum_yr,
             action AS post_action,
             COALESCE(gis.facility, insp.facility) AS facility, insp.acres
      FROM dblarv_insptrt_current insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
      WHERE insp.posttrt_p IS NOT NULL
        AND insp.inspdate >= '%s' AND insp.inspdate <= '%s'
      UNION ALL
      SELECT pkey_pg, inspdate, insptime,
             sitecode, numdip, sampnum_yr,
             action,
             COALESCE(gis.facility, insp.facility), insp.acres
      FROM dblarv_insptrt_archive insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
      WHERE insp.posttrt_p IS NOT NULL
        AND insp.inspdate >= '%s' AND insp.inspdate <= '%s'
    ", start_date, end_date, start_date, end_date)
    
    post_checks <- dbGetQuery(con, post_sql)
    if (is.null(post_checks) || nrow(post_checks) == 0) {
      safe_disconnect(con)
      return(NULL)
    }
    post_checks$post_date <- as.Date(post_checks$post_date)
    message(sprintf("[efficacy] %d post-treatment checks", nrow(post_checks)))
    
    # -------------------------------------------------------------------
    # STEP 1b: Get ALL records for these sites in wider window
    # -------------------------------------------------------------------
    sites <- unique(post_checks$sitecode)
    site_list <- paste0("'", paste(sites, collapse = "','"), "'")
    
    all_sql <- sprintf("
      SELECT insp.pkey_pg, insp.inspdate, insp.insptime, insp.sitecode, insp.numdip, insp.sampnum_yr,
             insp.presamp_yr, insp.action, insp.matcode, insp.mattype, insp.posttrt_p,
             COALESCE(gis.facility, insp.facility) AS facility, insp.acres,
             COALESCE(mt.effect_days, 14) AS effect_days
      FROM dblarv_insptrt_current insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
      LEFT JOIN mattype_list_targetdose mt ON insp.matcode = mt.matcode
      WHERE insp.sitecode IN (%s)
        AND insp.inspdate >= '%s' AND insp.inspdate <= '%s'
      UNION ALL
      SELECT insp.pkey_pg, insp.inspdate, insp.insptime, insp.sitecode, insp.numdip, insp.sampnum_yr,
             insp.presamp_yr, insp.action, insp.matcode, insp.mattype, insp.posttrt_p,
             COALESCE(gis.facility, insp.facility), insp.acres,
             COALESCE(mt.effect_days, 14)
      FROM dblarv_insptrt_archive insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
      LEFT JOIN mattype_list_targetdose mt ON insp.matcode = mt.matcode
      WHERE insp.sitecode IN (%s)
        AND insp.inspdate >= '%s' AND insp.inspdate <= '%s'
    ", site_list, wider_start, end_date, site_list, wider_start, end_date)
    
    all_records <- dbGetQuery(con, all_sql)
    all_records$inspdate <- as.Date(all_records$inspdate)
    
    # -------------------------------------------------------------------
    # STEP 2: Vectorized R matching (no loops)
    #
    # For each post-check, find the most recent treatment and pre-check
    # at the same site before the post-check date/time.
    #
    # Strategy: sort all treatments desc by date/time, then use a
    # non-equi merge approach via dplyr.
    # -------------------------------------------------------------------
    treatments_all <- all_records %>%
      filter(action %in% c('A', 'D', '3')) %>%
      arrange(sitecode, desc(inspdate), desc(insptime))
    
    # NOTE: Bti filter is applied AFTER matching, not here.
    # Filtering treatments before matching causes post-checks to silently drop
    # when the most recent treatment isn't Bti (even if an earlier Bti treatment exists).
    
    pre_checks_all <- all_records %>%
      filter(action %in% c('4', '2', '1'), is.na(posttrt_p)) %>%
      arrange(sitecode, desc(inspdate), desc(insptime))
    
    # Give post_checks a unique row_id to track through the join
    post_checks$row_id <- seq_len(nrow(post_checks))
    
    # For treatment matching: find most recent treatment before each post-check
    # Use a rolling join approach in base R for speed
    matched_trts <- vector("list", nrow(post_checks))
    
    # Split data by sitecode for fast lookup
    trt_by_site <- split(treatments_all, treatments_all$sitecode)
    pre_by_site <- split(pre_checks_all, pre_checks_all$sitecode)
    
    for (i in seq_len(nrow(post_checks))) {
      pc <- post_checks[i, ]
      site <- pc$sitecode
      
      # Find most recent treatment before this post-check at this site
      site_trts <- trt_by_site[[site]]
      if (is.null(site_trts) || nrow(site_trts) == 0) next
      
      # Already sorted desc — filter to before post_date/time
      before_mask <- site_trts$inspdate < pc$post_date |
                    (site_trts$inspdate == pc$post_date & site_trts$insptime < pc$post_time)
      if (!any(before_mask)) next
      trt <- site_trts[which(before_mask)[1], ]
      
      # Find most recent pre-check before post-check
      site_pres <- pre_by_site[[site]]
      pre_date <- as.Date(NA)
      pre_numdip <- NA_real_
      pre_sampnum_yr <- NA_character_
      pre_time_val <- "00:00:00"
      
      if (!is.null(site_pres) && nrow(site_pres) > 0) {
        before_mask_pre <- site_pres$inspdate < pc$post_date |
                          (site_pres$inspdate == pc$post_date & site_pres$insptime < pc$post_time)
        if (any(before_mask_pre)) {
          pre <- site_pres[which(before_mask_pre)[1], ]
          pre_date <- pre$inspdate
          pre_numdip <- pre$numdip
          pre_sampnum_yr <- pre$sampnum_yr
          pre_time_val <- if (!is.na(pre$insptime) && pre$insptime != "") pre$insptime else "00:00:00"
        }
      }
      
      matched_trts[[i]] <- data.frame(
        row_id = pc$row_id,
        trt_date = trt$inspdate,
        trt_time = ifelse(is.na(trt$insptime) || trt$insptime == "", "00:00:00", trt$insptime),
        trt_action = trt$action,
        trt_matcode = trt$matcode,
        trt_mattype = trt$mattype,
        trt_acres = trt$acres,
        trt_effect_days = trt$effect_days,
        pre_date = pre_date,
        pre_time = pre_time_val,
        pre_numdip = pre_numdip,
        pre_sampnum_yr = pre_sampnum_yr,
        stringsAsFactors = FALSE
      )
    }
    
    matched_trts <- matched_trts[!sapply(matched_trts, is.null)]
    if (length(matched_trts) == 0) {
      safe_disconnect(con)
      return(NULL)
    }
    
    trt_df <- do.call(rbind, matched_trts)
    
    # Join back to post_checks
    matched <- merge(post_checks, trt_df, by = "row_id")
    matched$acres <- ifelse(!is.na(matched$trt_acres), matched$trt_acres, matched$acres)
    matched$days_from_trt <- as.numeric(matched$post_date - matched$trt_date)
    matched$year <- as.integer(format(matched$trt_date, "%Y"))
    matched$row_id <- NULL
    matched$trt_acres <- NULL
    
    message(sprintf("[efficacy] Matched %d post-checks to treatments", nrow(matched)))
    
    # -------------------------------------------------------------------
    # STEP 2a-post: Apply Bti filter AFTER matching
    # Each post-check is matched to its most recent treatment. Now we filter
    # to keep only rows where that matched treatment used a Bti matcode.
    # -------------------------------------------------------------------
    if (bti_only && !is.null(bti_codes)) {
      n_before <- nrow(matched)
      matched <- matched[matched$trt_matcode %in% bti_codes, ]
      message(sprintf("[efficacy] Bti post-filter: %d -> %d rows (removed %d non-Bti)",
                      n_before, nrow(matched), n_before - nrow(matched)))
      if (nrow(matched) == 0) {
        safe_disconnect(con)
        return(NULL)
      }
    }
    
    # -------------------------------------------------------------------
    # STEP 2b: Look up material details (active_ingredient, dosage)
    # -------------------------------------------------------------------
    mat_codes_unique <- unique(matched$trt_matcode[!is.na(matched$trt_matcode)])
    if (length(mat_codes_unique) > 0) {
      mat_list <- paste0("'", paste(mat_codes_unique, collapse = "','"), "'")
      mat_detail_sql <- sprintf("
        SELECT DISTINCT t.matcode, t.tdose, t.unit, t.area, m.active_ingredient
        FROM mattype_list_targetdose t
        JOIN mattype_list m ON t.mattype = m.mattype
        WHERE t.matcode IN (%s)
      ", mat_list)
      mat_details <- dbGetQuery(con, mat_detail_sql)
      if (!is.null(mat_details) && nrow(mat_details) > 0) {
        mat_details$dosage_label <- paste(mat_details$tdose, mat_details$unit, "/", mat_details$area)
        matched <- merge(matched, mat_details[, c("matcode", "tdose", "dosage_label", "active_ingredient")],
                         by.x = "trt_matcode", by.y = "matcode", all.x = TRUE)
      } else {
        matched$tdose <- NA_real_
        matched$dosage_label <- NA_character_
        matched$active_ingredient <- NA_character_
      }
    } else {
      matched$tdose <- NA_real_
      matched$dosage_label <- NA_character_
      matched$active_ingredient <- NA_character_
    }
    
    # -------------------------------------------------------------------
    # STEP 3: Bulk-load species genus percentages
    # -------------------------------------------------------------------
    all_samples <- unique(c(matched$post_sampnum_yr, matched$pre_sampnum_yr))
    all_samples <- all_samples[!is.na(all_samples) & all_samples != ""]
    
    genus_data <- NULL
    if (length(all_samples) > 0) {
      sample_list <- paste0("'", paste(all_samples, collapse = "','"), "'")
      
      spp_sql <- sprintf("
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
      ", sample_list, sample_list)
      
      genus_data <- dbGetQuery(con, spp_sql)
      if (!is.null(genus_data) && nrow(genus_data) > 0) {
        message(sprintf("[efficacy] Species: %d sample-genus combos", nrow(genus_data)))
      }
    }
    
    safe_disconnect(con)
    
    # -------------------------------------------------------------------
    # STEP 4: Season, treatment type, and control checkback classification
    # (moved before genus loop so control data is available for Mulla's)
    # -------------------------------------------------------------------
    spring_thresholds <- get_spring_date_thresholds()
    thr_year <- spring_thresholds$year
    thr_date <- spring_thresholds$date_start
    
    matched$season <- vapply(seq_len(nrow(matched)), function(i) {
      idx <- match(matched$year[i], thr_year)
      if (is.na(idx) || is.na(matched$trt_date[i])) return(NA_character_)
      if (matched$trt_date[i] < thr_date[idx]) "Spring" else "Summer"
    }, character(1))
    
    matched$trt_type <- dplyr::case_when(
      matched$trt_action == "A" ~ "Air",
      matched$trt_action == "D" ~ "Drone",
      matched$trt_action == "3" ~ "Ground",
      TRUE ~ "Other"
    )
    
    # Flag control checkbacks and invalid checkbacks:
    # Control: no treatments between pre and post (by timestamp)
    # Invalid: TWO OR MORE treatments between the pre-inspection and post-inspection
    #   This means the site was treated multiple times, so we can't attribute
    #   the checkback result to a single treatment.
    trt_datetime <- paste(matched$trt_date, matched$trt_time)
    pre_datetime <- ifelse(is.na(matched$pre_date), NA_character_,
                           paste(matched$pre_date, matched$pre_time))
    
    # Control = treatment datetime is at or before pre-inspection datetime
    # (meaning no treatment happened between the pre-inspection and the post-check)
    matched$is_control <- !is.na(matched$pre_date) &
                (is.na(matched$trt_date) | trt_datetime <= pre_datetime)
    
    # Invalid = TWO OR MORE treatments between pre-inspection and post-inspection.
    # One treatment between pre and post is expected (that's the treatment we're
    # measuring efficacy for). Two or more means we can't attribute the result
    # to a single treatment.
    # trt_by_site is available from the matching loop above.
    invalid_info <- vapply(seq_len(nrow(matched)), function(i) {
      if (is.na(matched$pre_date[i]) || is.na(matched$post_date[i])) return(NA_character_)
      
      site <- matched$sitecode[i]
      pre_date_val <- matched$pre_date[i]
      post_date_val <- matched$post_date[i]
      site_trts <- trt_by_site[[site]]
      if (is.null(site_trts) || nrow(site_trts) == 0) return(NA_character_)
      
      # Count treatments strictly between pre and post dates
      pre_time_val <- matched$pre_time[i]
      post_time_val <- if (!is.na(matched$post_time[i]) && matched$post_time[i] != "") matched$post_time[i] else "23:59:59"
      pre_dt_full <- paste(pre_date_val, pre_time_val)
      post_dt_full <- paste(post_date_val, post_time_val)
      trt_times_local <- ifelse(is.na(site_trts$insptime) | site_trts$insptime == "", "00:00:00", site_trts$insptime)
      trt_dts_local <- paste(site_trts$inspdate, trt_times_local)
      between_mask <- trt_dts_local > pre_dt_full & trt_dts_local < post_dt_full
      n_between <- sum(between_mask)
      
      if (n_between < 2) return(NA_character_)
      
      # Return info: number of treatments between pre and post
      between_trts <- site_trts[between_mask, ]
      trt_dates_str <- paste(between_trts$inspdate, collapse = ", ")
      sprintf("%d|%s", n_between, trt_dates_str)
    }, character(1))
    
    matched$is_invalid <- !is.na(invalid_info)
    # If a row is invalid, it can't also be a control — invalid takes priority
    matched$is_control[matched$is_invalid] <- FALSE
    matched$invalid_reason <- NA_character_
    matched$prior_trt_date <- as.Date(NA)
    matched$prior_trt_material <- NA_character_
    matched$prior_trt_effect_days <- NA_integer_
    matched$prior_trt_expiry <- as.Date(NA)
    matched$prior_trt_days_remaining <- NA_integer_
    if (any(matched$is_invalid)) {
      valid_idx <- which(!is.na(invalid_info))
      parts <- strsplit(invalid_info[valid_idx], "\\|")
      for (j in seq_along(valid_idx)) {
        p <- parts[[j]]
        matched$invalid_reason[valid_idx[j]] <- sprintf(
          "%s treatments between pre and post inspections (dates: %s)",
          p[1], p[2])
      }
    }
    
    n_control <- sum(matched$is_control, na.rm = TRUE)
    n_invalid <- sum(matched$is_invalid, na.rm = TRUE)
    n_valid <- sum(!matched$is_control & !matched$is_invalid, na.rm = TRUE)
    message(sprintf("[efficacy] Control: %d, Invalid (2+ treatments between pre/post): %d, Valid: %d",
                    n_control, n_invalid, n_valid))
    
    # -------------------------------------------------------------------
    # STEP 5: Compute genus dip counts + % reduction (vectorized)
    # When use_mullas=TRUE and control data exists, apply full Mulla's:
    #   % Reduction = 100 - (T2/T1) x (C1/C2) x 100
    # Where T=treated, C=control, 1=pre, 2=post
    # C1/C2 corrects for natural population change between observations.
    # -------------------------------------------------------------------
    result_list <- vector("list", 2)
    names(result_list) <- c("Aedes", "Culex")
    
    for (g in c("Aedes", "Culex")) {
      gm <- matched
      
      if (!is.null(genus_data) && nrow(genus_data) > 0) {
        g_sub <- genus_data[genus_data$genus == g, c("sampnum_yr", "total_pct")]
        
        gm <- merge(gm, g_sub, by.x = "pre_sampnum_yr", by.y = "sampnum_yr", all.x = TRUE)
        names(gm)[names(gm) == "total_pct"] <- "pre_genus_pct"
        
        gm <- merge(gm, g_sub, by.x = "post_sampnum_yr", by.y = "sampnum_yr", all.x = TRUE)
        names(gm)[names(gm) == "total_pct"] <- "post_genus_pct"
      } else {
        gm$pre_genus_pct  <- NA_real_
        gm$post_genus_pct <- NA_real_
      }
      
      gm$genus <- g
      gm$pre_genus_pct[is.na(gm$pre_genus_pct)]   <- 0
      gm$post_genus_pct[is.na(gm$post_genus_pct)] <- 0
      gm$pre_genus_dips  <- gm$pre_numdip  * gm$pre_genus_pct  / 100
      gm$post_genus_dips <- gm$post_numdip * gm$post_genus_pct / 100
      
      valid <- !is.na(gm$pre_genus_dips) & gm$pre_genus_dips > 0
      gm$pct_reduction <- NA_real_
      gm$n_controls <- NA_integer_
      gm$control_ratio <- NA_real_
      
      if (use_mullas) {
        # Compute standard reduction for ALL rows first (simplified Mulla's)
        gm$pct_reduction[valid] <-
          100 - (gm$post_genus_dips[valid] / gm$pre_genus_dips[valid]) * 100
        
        # If control data exists, compute correction factor and apply to treated rows
        control_mask <- gm$is_control == TRUE
        control_rows <- gm[control_mask & valid & gm$post_genus_dips > 0, ]
        
        if (nrow(control_rows) > 0) {
          # Compute C1/C2 ratio per season (natural population change factor)
          control_summary <- control_rows %>%
            mutate(ratio = pre_genus_dips / post_genus_dips) %>%
            group_by(season) %>%
            summarise(avg_control_ratio = mean(ratio, na.rm = TRUE),
                      n_control = n(), .groups = 'drop')
          
          # Apply full Mulla's formula to treated rows by season
          for (s in unique(control_summary$season)) {
            if (is.na(s)) next
            season_ctrl <- control_summary[control_summary$season == s, ]
            treated_season <- !control_mask & !gm$is_invalid & valid & gm$season == s & !is.na(gm$season)
            
            gm$pct_reduction[treated_season] <-
              100 - (gm$post_genus_dips[treated_season] /
                     gm$pre_genus_dips[treated_season]) *
                    season_ctrl$avg_control_ratio * 100
            
            gm$n_controls[treated_season] <- season_ctrl$n_control
            gm$control_ratio[treated_season] <- round(season_ctrl$avg_control_ratio, 4)
          }
          
          message(sprintf("[efficacy] Mulla's control correction applied for genus %s: %d control obs across %d seasons",
                          g, nrow(control_rows), nrow(control_summary)))
        } else {
          message(sprintf("[efficacy] No control data for genus %s - using simplified Mulla's", g))
        }
      } else {
        gm$pct_reduction[valid] <-
          (gm$pre_genus_dips[valid] - gm$post_genus_dips[valid]) /
           gm$pre_genus_dips[valid] * 100
      }
      
      result_list[[g]] <- gm
    }
    
    result <- do.call(rbind, result_list)
    rownames(result) <- NULL
    
    message(sprintf("[efficacy] Final: %d rows (%d Aedes, %d Culex, %d control, %d invalid) | Bti=%s Mulla=%s",
                    nrow(result),
                    sum(result$genus == "Aedes"),
                    sum(result$genus == "Culex"),
                    sum(result$is_control, na.rm = TRUE),
                    sum(result$is_invalid, na.rm = TRUE),
                    bti_only, use_mullas))
    
    return(result)
    
  }, error = function(e) {
    message("Error loading efficacy data: ", e$message)
    if (exists("con") && !is.null(con)) {
      try(safe_disconnect(con), silent = TRUE)
    }
    return(NULL)
  })
}
