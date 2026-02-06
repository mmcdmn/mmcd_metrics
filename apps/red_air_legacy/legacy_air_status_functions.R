
# Function to get air sites data with status calculation
get_air_sites_data <- function(analysis_date, lookback_period, rain_threshold, treatment_threshold, facility_filter, priority_filter, zone_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    lookback_days <- as.numeric(lookback_period)
    rain_threshold <- as.numeric(rain_threshold)
    treatment_threshold <- as.numeric(treatment_threshold)
    
    # Build facility filter - handle empty or NULL values
    facility_condition <- if (is.null(facility_filter) || length(facility_filter) == 0) {
      "AND 1=1"  # Always true condition
    } else {
      facility_list <- paste(sprintf("'%s'", facility_filter), collapse=", ")
      sprintf("AND b.facility IN (%s)", facility_list)
    }
    
    # Build priority filter - handle empty or NULL values
    priority_condition <- if (is.null(priority_filter) || length(priority_filter) == 0) {
      "AND 1=1"  # Always true condition
    } else {
      priority_list <- paste(sprintf("'%s'", priority_filter), collapse=", ")
      sprintf("AND b.priority IN (%s)", priority_list)
    }
    
    # Build zone filter - handle empty or NULL values
    zone_condition <- if (is.null(zone_filter) || length(zone_filter) == 0) {
      "AND 1=1"  # Always true condition
    } else {
      zone_list <- paste(sprintf("'%s'", zone_filter), collapse=", ")
      sprintf("AND g.zone IN (%s)", zone_list)
    }
    
    # Simple but effective query for air site status calculation
    query <- sprintf("
      WITH ActiveAirSites AS (
        SELECT 
          b.facility,
          b.sitecode,
          b.acres,
          b.priority,
          b.prehatch,
          g.zone,
          ST_X(ST_Centroid(ST_Transform(b.geom, 4326))) as longitude,
          ST_Y(ST_Centroid(ST_Transform(b.geom, 4326))) as latitude
        FROM loc_breeding_sites b
        LEFT JOIN public.gis_sectcode g ON g.sectcode = left(b.sitecode, 7)
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          %s
          %s
          %s
      ),
      
      RainfallData AS (
        SELECT 
          a.sitecode,
          COALESCE(SUM(r.rain_inches), 0) as total_rainfall,
          -- Check if any 3 consecutive days in the last 14 days had enough rainfall
          CASE 
            WHEN EXISTS (
              SELECT 1 
              FROM generate_series(0, 14 - %.0f) AS start_offset  -- Sliding window for consecutive days
              WHERE (
                SELECT COALESCE(SUM(rain_inches), 0)
                FROM nws_precip_site_history 
                WHERE sitecode = a.sitecode 
                  AND date >= '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day'
                  AND date < '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day' + INTERVAL '%.0f days'
              ) >= %f
            ) THEN 
              json_build_object('triggered', true, 'total_rain', COALESCE(SUM(r.rain_inches), 0))::text
            ELSE 
              json_build_object('triggered', false)::text
          END as triggering_period,
          CASE 
            WHEN EXISTS (
              SELECT 1 
              FROM generate_series(0, 14 - %.0f) AS start_offset  -- Sliding window for consecutive days
              WHERE (
                SELECT COALESCE(SUM(rain_inches), 0)
                FROM nws_precip_site_history 
                WHERE sitecode = a.sitecode 
                  AND date >= '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day'
                  AND date < '%s'::date - INTERVAL '14 days' + start_offset * INTERVAL '1 day' + INTERVAL '%.0f days'
              ) >= %f
            ) THEN true
            ELSE false
          END as has_triggering_rainfall
        FROM ActiveAirSites a
        LEFT JOIN nws_precip_site_history r ON a.sitecode = r.sitecode
          AND r.date >= '%s'::date - INTERVAL '14 days'
          AND r.date <= '%s'::date
        GROUP BY a.sitecode
      ),
      
      LastInspections AS (
        SELECT 
          sitecode,
          inspdate as last_inspection_date,
          numdip as last_larvae_count,
          ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY inspdate DESC) as rn
        FROM (
          SELECT sitecode, inspdate, numdip FROM dblarv_insptrt_current WHERE action IN ('1', '2', '4') AND inspdate <= '%s'
          UNION ALL
          SELECT sitecode, inspdate, numdip FROM dblarv_insptrt_archive WHERE action IN ('1', '2', '4') AND inspdate <= '%s'
        ) inspections
      ),
      
      RecentInspections AS (
        SELECT 
          sitecode,
          last_inspection_date,
          last_larvae_count
        FROM LastInspections
        WHERE rn = 1
      ),
      
      TreatmentEffects AS (
        SELECT 
          t.sitecode,
          MAX(t.inspdate) as last_treatment_date,
          FIRST_VALUE(t.mattype) OVER (PARTITION BY t.sitecode ORDER BY t.inspdate DESC) as last_treatment_material,
          CASE 
            WHEN MAX(m.effect_days) IS NOT NULL 
            THEN MAX(t.inspdate) + INTERVAL '1 day' * MAX(m.effect_days)
            ELSE NULL
          END as treatment_expiry
        FROM (
          SELECT sitecode, inspdate, mattype
          FROM public.dblarv_insptrt_archive
          WHERE action IN ('3', 'A', 'D')
            AND inspdate <= '%s'::date
          UNION ALL
          SELECT sitecode, inspdate, mattype
          FROM public.dblarv_insptrt_current
          WHERE action IN ('3', 'A', 'D')
            AND inspdate <= '%s'::date
        ) t
        LEFT JOIN public.mattype_list m ON t.mattype = m.mattype
        GROUP BY t.sitecode, t.mattype, t.inspdate
      ),
      
      LastTreatmentInfo AS (
        SELECT 
          sitecode,
          last_treatment_date,
          last_treatment_material,
          treatment_expiry,
          ROW_NUMBER() OVER (PARTITION BY sitecode ORDER BY last_treatment_date DESC) as rn
        FROM TreatmentEffects
      ),
      
      FinalTreatmentInfo AS (
        SELECT 
          sitecode,
          last_treatment_date,
          last_treatment_material,
          treatment_expiry
        FROM LastTreatmentInfo
        WHERE rn = 1
      )
      
      SELECT 
        a.sitecode,
        a.facility,
        a.priority,
        a.zone,
        a.acres,
        a.longitude,
        a.latitude,
        COALESCE(r.total_rainfall, 0) as total_rainfall,
        r.triggering_period,
        r.has_triggering_rainfall,
        i.last_inspection_date,
        i.last_larvae_count,
        t.last_treatment_date,
        t.last_treatment_material,
        t.treatment_expiry,
        CASE 
          -- Priority 1: Active Treatment (treatment applied and still effective)
          WHEN t.treatment_expiry IS NOT NULL AND '%s'::date <= t.treatment_expiry THEN 'Active Treatment'
          
          -- Priority 2: Treatment Expired -> Unknown (once treatment expires, go back to unknown)
          WHEN t.treatment_expiry IS NOT NULL AND '%s'::date > t.treatment_expiry THEN 'Unknown'
          
          -- Priority 3: Has triggering rainfall -> ALWAYS Needs Inspection
          WHEN r.has_triggering_rainfall = true THEN 'Needs Inspection'
          
          -- Priority 4: No triggering rainfall but has inspection -> use inspection results  
          WHEN r.has_triggering_rainfall = false AND i.last_inspection_date IS NOT NULL THEN
            CASE 
              WHEN COALESCE(i.last_larvae_count, 0) >= %.0f THEN 'Needs Treatment'
              ELSE 'Under Threshold'
            END
          
          -- Default: Unknown (no triggering rainfall, no recent inspection)
          ELSE 'Unknown'
        END as site_status
      FROM ActiveAirSites a
      LEFT JOIN RainfallData r ON a.sitecode = r.sitecode
      LEFT JOIN RecentInspections i ON a.sitecode = i.sitecode
      LEFT JOIN FinalTreatmentInfo t ON a.sitecode = t.sitecode
      ORDER BY a.facility, a.sitecode
    ",
    analysis_date,          # For enddate filter
    facility_condition,
    priority_condition, 
    zone_condition,
    lookback_days,          # For generate_series limit 1
    analysis_date,          # For consecutive period start 1
    analysis_date,          # For consecutive period start 2
    lookback_days,          # For consecutive period length 1
    rain_threshold,         # For consecutive threshold check 1
    lookback_days,          # For generate_series limit 2
    analysis_date,          # For consecutive period start 3
    analysis_date,          # For consecutive period start 4
    lookback_days,          # For consecutive period length 2
    rain_threshold,         # For consecutive threshold check 2
    analysis_date,          # For rainfall period start
    analysis_date,          # For rainfall period end
    analysis_date,          # For inspection date filter 1
    analysis_date,          # For inspection date filter 2
    analysis_date,          # For treatment date filter 1
    analysis_date,          # For treatment date filter 2
    analysis_date,          # For treatment expiry check
    analysis_date,          # For treatment expired check
    treatment_threshold     # For larvae count threshold
    )
    
    result <- dbGetQuery(con, query)
    safe_disconnect(con)
    
    return(result)
    
  }, error = function(e) {
    if (exists("con") && !is.null(con)) {
      safe_disconnect(con)
    }
    warning(paste("Error getting air sites data:", e$message))
    return(data.frame())
  })
}
