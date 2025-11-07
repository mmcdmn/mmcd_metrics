library(DBI)
library(sf)
library(dplyr)

source("../../shared/db_helpers.R")

con <- get_db_connection()

# Test trap query - should get ~474 rows
cat("Testing trap query...\n")

trap_q <- "
WITH trap_data AS (
  SELECT t.ainspecnum, t.facility, t.x, t.y, t.survtype
  FROM public.dbadult_insp_current t
  WHERE t.inspdate::date <= '2025-11-07'::date 
    AND t.x IS NOT NULL AND t.y IS NOT NULL
    AND t.survtype IN ('4','5','6')
)
SELECT td.ainspecnum, td.facility, td.x as lon, td.y as lat, td.survtype,
       COALESCE(SUM(s.cnt), 0) as species_count
FROM trap_data td
LEFT JOIN public.dbadult_species_current s ON td.ainspecnum = s.ainspecnum
GROUP BY td.ainspecnum, td.facility, td.x, td.y, td.survtype
"

traps <- dbGetQuery(con, trap_q)
cat(sprintf("Original query returned %d rows\n", nrow(traps)))
cat(sprintf("First 10 rows:\n"))
print(head(traps, 10))

# Check for duplicates at same location
cat("\n\nChecking for duplicate locations...\n")
dup_check <- traps %>% 
  group_by(lon, lat) %>% 
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)
cat(sprintf("Found %d locations with multiple traps\n", nrow(dup_check)))
if (nrow(dup_check) > 0) {
  print(head(dup_check, 5))
}

# Test distance calculation
cat("\n\nTesting distance calculation...\n")
traps_sf <- st_as_sf(traps, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
traps_m <- st_transform(traps_sf, 3857)

# Test with first 5 traps
test_dists <- as.numeric(st_distance(traps_m[1, ], traps_m[1:5, ]))
cat("Distances from trap 1 to first 5 traps:\n")
print(test_dists)

# Check species counts
cat("\n\nSpecies count distribution:\n")
print(summary(traps$species_count))
cat(sprintf("Traps with count > 0: %d\n", sum(traps$species_count > 0)))
cat(sprintf("Traps with count = 0: %d\n", sum(traps$species_count == 0)))

dbDisconnect(con)
cat("\nTest complete!\n")
