# =============================================================================
# EFFICACY TAB: Genus-level % reduction analysis
# =============================================================================

#' Get Bti larvicide matcodes by joining mattype_list_targetdose → mattype_list
#'
#' @param con Database connection
#' @return Character vector of matcodes
get_bti_matcodes <- function(con) {
  bti <- dbGetQuery(con, "
    SELECT DISTINCT t.matcode
    FROM mattype_list_targetdose t
    JOIN mattype_list m ON t.mattype = m.mattype
    WHERE m.active_ingredient ILIKE '%Bti%'
      AND m.physinv_list = '(1) Larvicide'
  ")
  if (is.null(bti) || nrow(bti) == 0) return(character(0))
  return(bti$matcode)
}

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
    # BTI MATCODE FILTER (optional)
    # -------------------------------------------------------------------
    bti_codes <- NULL
    if (bti_only) {
      bti_codes <- get_bti_matcodes(con)
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
      SELECT pkey_pg, inspdate, insptime, sitecode, numdip, sampnum_yr,
             presamp_yr, action, matcode, mattype, posttrt_p,
             COALESCE(gis.facility, insp.facility) AS facility, insp.acres
      FROM dblarv_insptrt_current insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
      WHERE insp.sitecode IN (%s)
        AND insp.inspdate >= '%s' AND insp.inspdate <= '%s'
      UNION ALL
      SELECT pkey_pg, inspdate, insptime, sitecode, numdip, sampnum_yr,
             presamp_yr, action, matcode, mattype, posttrt_p,
             COALESCE(gis.facility, insp.facility), insp.acres
      FROM dblarv_insptrt_archive insp
      LEFT JOIN gis_sectcode gis ON gis.sectcode = LEFT(insp.sitecode, 7)
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
    
    # Optionally filter to Bti matcodes
    if (bti_only && !is.null(bti_codes)) {
      treatments_all <- treatments_all %>% filter(matcode %in% bti_codes)
    }
    
    pre_checks_all <- all_records %>%
      filter(action %in% c('4', '2', '1')) %>%
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
      
      if (!is.null(site_pres) && nrow(site_pres) > 0) {
        before_mask_pre <- site_pres$inspdate < pc$post_date |
                          (site_pres$inspdate == pc$post_date & site_pres$insptime < pc$post_time)
        if (any(before_mask_pre)) {
          pre <- site_pres[which(before_mask_pre)[1], ]
          pre_date <- pre$inspdate
          pre_numdip <- pre$numdip
          pre_sampnum_yr <- pre$sampnum_yr
        }
      }
      
      matched_trts[[i]] <- data.frame(
        row_id = pc$row_id,
        trt_date = trt$inspdate,
        trt_action = trt$action,
        trt_matcode = trt$matcode,
        trt_mattype = trt$mattype,
        trt_acres = trt$acres,
        pre_date = pre_date,
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
    # STEP 4: Compute genus dip counts + % reduction (vectorized)
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
      
      if (use_mullas) {
        gm$pct_reduction[valid] <-
          100 - (gm$post_genus_dips[valid] / gm$pre_genus_dips[valid]) * 100
      } else {
        gm$pct_reduction[valid] <-
          (gm$pre_genus_dips[valid] - gm$post_genus_dips[valid]) /
           gm$pre_genus_dips[valid] * 100
      }
      
      result_list[[g]] <- gm
    }
    
    result <- do.call(rbind, result_list)
    rownames(result) <- NULL
    
    # -------------------------------------------------------------------
    # STEP 5: Season classification
    # -------------------------------------------------------------------
    spring_thresholds <- get_spring_date_thresholds()
    thr_year <- spring_thresholds$year
    thr_date <- spring_thresholds$date_start
    
    result$season <- vapply(seq_len(nrow(result)), function(i) {
      idx <- match(result$year[i], thr_year)
      if (is.na(idx) || is.na(result$trt_date[i])) return(NA_character_)
      if (result$trt_date[i] < thr_date[idx]) "Spring" else "Summer"
    }, character(1))
    
    result$trt_type <- dplyr::case_when(
      result$trt_action == "A" ~ "Air",
      result$trt_action == "D" ~ "Drone",
      result$trt_action == "3" ~ "Ground",
      TRUE ~ "Other"
    )
    
    message(sprintf("[efficacy] Final: %d rows (%d Aedes, %d Culex) | Bti=%s Mulla=%s",
                    nrow(result),
                    sum(result$genus == "Aedes"),
                    sum(result$genus == "Culex"),
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
