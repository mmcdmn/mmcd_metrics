#!/usr/bin/env Rscript

# Debug the sprintf query construction
source('shared/db_helpers.R')

# Set up test values
analysis_date <- Sys.Date()
lookback_period <- "3"
rain_threshold <- "1.0"
treatment_threshold <- "1"
facility_filter <- "all"
priority_filter <- "all"  
zone_filter <- NULL

lookback_days <- as.numeric(lookback_period)
rain_threshold <- as.numeric(rain_threshold)
treatment_threshold <- as.numeric(treatment_threshold)

# Build conditions exactly like the function
facility_condition <- if (facility_filter == "all") {
  ""
} else {
  sprintf("AND b.facility = '%s'", facility_filter)
}

priority_condition <- if (priority_filter == "all") {
  ""
} else {
  sprintf("AND b.priority = '%s'", priority_filter)
}

zone_condition <- if (is.null(zone_filter) || length(zone_filter) == 0) {
  ""
} else {
  zone_list <- paste(sprintf("'%s'", zone_filter), collapse=", ")
  sprintf("AND g.zone IN (%s)", zone_list)
}

cat('Conditions:\n')
cat('- facility_condition: "', facility_condition, '"\n', sep='')
cat('- priority_condition: "', priority_condition, '"\n', sep='')
cat('- zone_condition: "', zone_condition, '"\n', sep='')
cat('- analysis_date:', analysis_date, '\n')
cat('- lookback_days:', lookback_days, '\n')
cat('- rain_threshold:', rain_threshold, '\n')
cat('- treatment_threshold:', treatment_threshold, '\n')

# Count the arguments
args <- list(
  facility_condition,
  priority_condition, 
  zone_condition,
  analysis_date,
  lookback_days,
  analysis_date,
  analysis_date,
  rain_threshold,
  treatment_threshold
)

cat('\nNumber of sprintf arguments:', length(args), '\n')

# Now let's count placeholders in the raw query
query_template <- "
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
        LEFT JOIN public.gis_sectcode g ON LEFT(b.sitecode, 6) || '-' = g.sectcode
          OR LEFT(b.sitecode, 6) || 'N' = g.sectcode
        WHERE (b.enddate IS NULL OR b.enddate > '%s')
          AND b.air_gnd = 'A'
          AND b.geom IS NOT NULL
          %s
          %s
          %s
      ),
      
      RainfallData AS (
        SELECT 
          b.sitecode,
          COALESCE(SUM(r.rainfall), 0) as total_rainfall
        FROM ActiveAirSites b
        LEFT JOIN public.nws_precip_site_history r ON b.sitecode = r.sitecode
        WHERE r.date >= '%s'::date - INTERVAL '%d days'
          AND r.date <= '%s'::date
        GROUP BY b.sitecode
      ),
      
      LastInspections AS (
        SELECT 
          a.sitecode,
          a.inspdate as last_inspection_date,
          a.numdip as last_larvae_count,
          ROW_NUMBER() OVER (PARTITION BY a.sitecode ORDER BY a.inspdate DESC) as rn
        FROM public.dblarv_insptrt_archive a
        WHERE a.action IN ('1', '2', '4')
        UNION ALL
        SELECT 
          c.sitecode,
          c.inspdate as last_inspection_date,
          c.numdip as last_larvae_count,
          ROW_NUMBER() OVER (PARTITION BY c.sitecode ORDER BY c.inspdate DESC) as rn
        FROM public.dblarv_insptrt_current c
        WHERE c.action IN ('1', '2', '4')
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
          CASE 
            WHEN MAX(m.effect_days) IS NOT NULL 
            THEN MAX(t.trtdate) + INTERVAL '1 day' * MAX(m.effect_days)
            ELSE NULL
          END as treatment_expiry
        FROM (
          SELECT sitecode, trtdate, materusd
          FROM public.dblarv_insptrt_archive
          WHERE action IN ('3', 'A', 'D')
          UNION ALL
          SELECT sitecode, trtdate, materusd
          FROM public.dblarv_insptrt_current
          WHERE action IN ('3', 'A', 'D')
        ) t
        LEFT JOIN public.mattype_list m ON t.materusd = m.material
        GROUP BY t.sitecode
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
        i.last_inspection_date,
        i.last_larvae_count,
        t.treatment_expiry,
        CASE 
          WHEN r.total_rainfall < %f THEN 'Below Threshold'
          WHEN t.treatment_expiry IS NOT NULL AND '%s'::date <= t.treatment_expiry THEN 'Active Treatment'
          WHEN i.last_inspection_date IS NULL THEN 'Needs Inspection' 
          WHEN t.treatment_expiry IS NOT NULL AND '%s'::date > t.treatment_expiry AND i.last_inspection_date < t.treatment_expiry THEN 'Needs Re-inspection'
          WHEN COALESCE(i.last_larvae_count, 0) >= %d THEN 'Needs Treatment' 
          ELSE 'Under Threshold'
        END as site_status
      FROM ActiveAirSites a
      LEFT JOIN RainfallData r ON a.sitecode = r.sitecode
      LEFT JOIN RecentInspections i ON a.sitecode = i.sitecode
      LEFT JOIN TreatmentEffects t ON a.sitecode = t.sitecode
      ORDER BY a.facility, a.sitecode
    "

# Count placeholders
placeholders <- gregexpr("%[sf]|%d", query_template)[[1]]
cat('Number of placeholders in query:', length(placeholders[placeholders > 0]), '\n')