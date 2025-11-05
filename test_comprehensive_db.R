#!/usr/bin/env Rscript

# Comprehensive database testing for red_air logic
source('shared/db_helpers.R')

con <- get_db_connection()
if(!is.null(con)) {
  cat('=== TESTING AIR SITES ===\n')
  
  # 1. Count air sites by priority
  result1 <- dbGetQuery(con, "
    SELECT priority, COUNT(*) as count 
    FROM loc_breeding_sites 
    WHERE air_gnd = 'A' AND (enddate IS NULL OR enddate > CURRENT_DATE)
    GROUP BY priority 
    ORDER BY priority
  ")
  cat('Air sites by priority:\n')
  print(result1)
  
  # 2. Sample air sites
  result2 <- dbGetQuery(con, "
    SELECT sitecode, facility, priority, acres, startdate
    FROM loc_breeding_sites 
    WHERE air_gnd = 'A' AND (enddate IS NULL OR enddate > CURRENT_DATE)
    LIMIT 5
  ")
  cat('\nSample air sites:\n')
  print(result2)
  
  cat('\n=== TESTING PRECIPITATION DATA ===\n')
  
  # 3. Recent precipitation data
  result3 <- dbGetQuery(con, "
    SELECT sitecode, date, rain_inches
    FROM public.nws_precip_site_history 
    WHERE date >= CURRENT_DATE - INTERVAL '7 days'
      AND rain_inches > 0
    ORDER BY date DESC
    LIMIT 10
  ")
  cat('Recent precipitation (last 7 days):\n')
  print(result3)
  
  cat('\n=== TESTING INSPECTION DATA ===\n')
  
  # 4. Recent inspections (action 1,2,4)
  result4 <- dbGetQuery(con, "
    SELECT sitecode, inspdate, action, numdip, facility
    FROM public.dblarv_insptrt_current 
    WHERE action IN ('1', '2', '4')
      AND inspdate >= CURRENT_DATE - INTERVAL '30 days'
    ORDER BY inspdate DESC
    LIMIT 10
  ")
  cat('Recent inspections (last 30 days):\n')
  print(result4)
  
  # 5. Recent treatments (action 3,A,D)
  result5 <- dbGetQuery(con, "
    SELECT sitecode, inspdate, action, matcode, mattype, facility
    FROM public.dblarv_insptrt_current 
    WHERE action IN ('3', 'A', 'D')
      AND inspdate >= CURRENT_DATE - INTERVAL '30 days'
    ORDER BY inspdate DESC
    LIMIT 10
  ")
  cat('Recent treatments (last 30 days):\n')
  print(result5)
  
  cat('\n=== TESTING MATERIAL TYPES ===\n')
  
  # 6. Material types with effect days
  result6 <- dbGetQuery(con, "
    SELECT mattype, effect_days, active_ingredient, formulation
    FROM public.mattype_list 
    WHERE effect_days IS NOT NULL
    ORDER BY mattype
    LIMIT 10
  ")
  cat('Material types with effect days:\n')
  print(result6)
  
  dbDisconnect(con)
} else {
  cat('Database connection failed\n')
}