#!/usr/bin/env Rscript

# Test the correct precipitation tables
source('shared/db_helpers.R')

con <- get_db_connection()
if(!is.null(con)) {
  cat('Testing precipitation tables...\n')
  
  # Test nws_precip_for_site
  cat('=== nws_precip_for_site ===\n')
  result1 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.nws_precip_for_site")
  cat('Total records in nws_precip_for_site:', result1$count, '\n')
  
  # Check date range
  result2 <- dbGetQuery(con, "SELECT MIN(date) as min_date, MAX(date) as max_date FROM public.nws_precip_for_site")
  cat('Date range:', result2$min_date, 'to', result2$max_date, '\n')
  
  # Check recent data (last 30 days)
  result3 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.nws_precip_for_site WHERE date >= CURRENT_DATE - INTERVAL '30 days'")
  cat('Records in last 30 days:', result3$count, '\n')
  
  cat('\n=== nws_precip_site_history ===\n')
  result4 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.nws_precip_site_history")
  cat('Total records in nws_precip_site_history:', result4$count, '\n')
  
  # Check date range
  result5 <- dbGetQuery(con, "SELECT MIN(date) as min_date, MAX(date) as max_date FROM public.nws_precip_site_history")
  cat('Date range:', result5$min_date, 'to', result5$max_date, '\n')
  
  # Check recent data (last 30 days)
  result6 <- dbGetQuery(con, "SELECT COUNT(*) as count FROM public.nws_precip_site_history WHERE date >= CURRENT_DATE - INTERVAL '30 days'")
  cat('Records in last 30 days:', result6$count, '\n')
  
  # Test which table has recent data for specific sites
  cat('\n=== Sample air sites with recent precipitation ===\n')
  sample_query <- "
    SELECT b.sitecode 
    FROM loc_breeding_sites b 
    WHERE b.air_gnd = 'A' 
      AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
    LIMIT 5
  "
  air_sites <- dbGetQuery(con, sample_query)
  
  if(nrow(air_sites) > 0) {
    test_site <- air_sites$sitecode[1]
    cat('Testing with site:', test_site, '\n')
    
    # Test precipitation data for this site
    precip_query1 <- sprintf("SELECT COUNT(*) as count FROM public.nws_precip_for_site WHERE sitecode = '%s' AND date >= CURRENT_DATE - INTERVAL '7 days'", test_site)
    precip1 <- dbGetQuery(con, precip_query1)
    cat('Recent precip records in nws_precip_for_site:', precip1$count, '\n')
    
    precip_query2 <- sprintf("SELECT COUNT(*) as count FROM public.nws_precip_site_history WHERE sitecode = '%s' AND date >= CURRENT_DATE - INTERVAL '7 days'", test_site)
    precip2 <- dbGetQuery(con, precip_query2)
    cat('Recent precip records in nws_precip_site_history:', precip2$count, '\n')
  }
  
  dbDisconnect(con)
} else {
  cat('Database connection failed\n')
}