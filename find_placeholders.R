#!/usr/bin/env Rscript

# Find the exact placeholders
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

# Find all placeholders with their positions
lines <- strsplit(query_template, "\n")[[1]]
placeholders <- c()
for(i in seq_along(lines)) {
  matches <- gregexpr("%[sdf]", lines[i])[[1]]
  if(matches[1] > 0) {
    for(match in matches) {
      placeholder_text <- substr(lines[i], match, match+1)
      placeholders <- c(placeholders, paste0("Line ", i, ": ", placeholder_text, " in '", trimws(lines[i]), "'"))
    }
  }
}

cat("All placeholders found:\n")
for(i in seq_along(placeholders)) {
  cat(i, ".", placeholders[i], "\n")
}

cat("\nArguments provided:\n")
cat("1. facility_condition\n")
cat("2. priority_condition\n") 
cat("3. zone_condition\n")
cat("4. analysis_date\n")
cat("5. lookback_days\n")
cat("6. analysis_date\n")
cat("7. analysis_date\n")
cat("8. rain_threshold\n")
cat("9. treatment_threshold\n")