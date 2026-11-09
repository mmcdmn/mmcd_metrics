# Build data/section_population.csv: 2020 census population per capacity-planner section.
#
# The census file (extra/qgis/2020CensusBlockAnalysis.gpkg, 47,031 blocks, ~74 MB) is far too
# slow and large to load in the app, and the app's database connection is read-only. So this
# script reads ONLY the block attributes (no polygons), puts every block in the section that
# contains its census internal point, adds the blocks up per section and writes a small tracked
# CSV (~3,200 rows). The app reads that CSV (load_section_population) and never touches the
# geopackage.
#
#   sectcode    7-digit section code (gis_sectcode.sectcode)
#   population  2020 residents of the blocks assigned to the section
#   land_sqmi   land area of those blocks, square miles (ALAND20)
#   density     population / land_sqmi (residents per square mile); NA when there is no land
#   blocks      number of census blocks assigned to the section
#
# A block sits in exactly one section (the one holding its internal point), so population is
# conserved; blocks whose internal point is in no section (outside the district's sections) are
# listed and left out. Blocks on a shared section edge go to the lowest sectcode.
#
# Section outlines come from ONE database query (cache it with SECTION_GEOM_RDS to rerun
# without touching the database).
#
# Run from the project root:
#   Rscript apps/capacity_planner/scripts/build_section_population.R [path/to/blocks.gpkg]
#   SECTION_GEOM_RDS=path/to/sections.rds Rscript ...   (reuse / create the section-outline cache)

SQMI_M2 <- 2589988.11   # square metres in a square mile (ALAND20 is in square metres)

#' Read the census block attributes (no geometry) from the geopackage.
#' The two "2020BlockswPop" layers hold the same blocks (they only differ in their QGIS style),
#' so the first one is used.
#' @return data.frame: geoid, pop, land_m2, lon, lat
read_census_blocks <- function(gpkg) {
  if (!file.exists(gpkg)) stop("Census geopackage not found: ", gpkg)
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tb <- DBI::dbGetQuery(con, "SELECT table_name FROM gpkg_contents
                              WHERE data_type = 'features' AND table_name LIKE '%2020BlockswPop%'
                              ORDER BY table_name")$table_name
  if (length(tb) == 0) stop("No '2020BlockswPop' layer in ", gpkg)
  b <- DBI::dbGetQuery(con, sprintf(
    'SELECT GEOID20, POPTOTAL, ALAND20, INTPTLAT20, INTPTLON20 FROM "%s"', tb[1]))
  data.frame(geoid = as.character(b$GEOID20), pop = as.integer(b$POPTOTAL),
             land_m2 = as.numeric(b$ALAND20),
             lon = as.numeric(b$INTPTLON20), lat = as.numeric(b$INTPTLAT20),
             stringsAsFactors = FALSE)
}

#' Put every block in the section that contains its internal point and add them up.
#'
#' @param blocks   data.frame: pop, land_m2, lon, lat (WGS84 degrees); optional geoid
#' @param sections sf polygons (any CRS) with a `sectcode` column
#' @return list(per_section = data.frame(sectcode, population, land_sqmi, density, blocks),
#'              block_section = character (sectcode per block, NA = outside every section),
#'              n_edge = blocks that sat on a shared edge)
assign_blocks_to_sections <- function(blocks, sections) {
  sections <- sections[order(as.character(sections$sectcode)), ]
  pts <- sf::st_transform(sf::st_as_sf(blocks[, c("lon", "lat")], coords = c("lon", "lat"),
                                       crs = 4326), sf::st_crs(sections))
  hit <- sf::st_intersects(pts, sections)
  first <- vapply(hit, function(h) if (length(h) == 0) NA_integer_ else h[1], integer(1))
  sc <- as.character(sections$sectcode)
  block_section <- ifelse(is.na(first), NA_character_, sc[first])

  pop  <- tapply(blocks$pop[!is.na(first)], block_section[!is.na(first)], sum)
  land <- tapply(blocks$land_m2[!is.na(first)], block_section[!is.na(first)], sum)
  n    <- tapply(rep(1L, sum(!is.na(first))), block_section[!is.na(first)], sum)
  out <- data.frame(sectcode = sc, stringsAsFactors = FALSE)
  out$population <- as.integer(ifelse(is.na(pop[sc]), 0L, pop[sc]))
  out$land_sqmi  <- as.numeric(round(ifelse(is.na(land[sc]), 0, land[sc]) / SQMI_M2, 4))
  out$density    <- ifelse(out$land_sqmi > 0, round(out$population / out$land_sqmi, 1), NA_real_)
  out$blocks     <- as.integer(ifelse(is.na(n[sc]), 0L, n[sc]))
  rownames(out) <- NULL
  list(per_section = out, block_section = block_section, n_edge = sum(lengths(hit) > 1))
}

build_section_population <- function(gpkg = "extra/qgis/2020CensusBlockAnalysis.gpkg",
                                     out = "apps/capacity_planner/data/section_population.csv",
                                     geom_cache = Sys.getenv("SECTION_GEOM_RDS", "")) {
  suppressPackageStartupMessages({ library(sf) })
  blocks <- read_census_blocks(gpkg)
  cat(sprintf("Census blocks: %s, residents: %s\n", format(nrow(blocks), big.mark = ","),
              format(sum(blocks$pop), big.mark = ",")))

  if (nzchar(geom_cache) && file.exists(geom_cache)) {
    secs <- readRDS(geom_cache); cat("Section outlines from cache:", geom_cache, "\n")
  } else {
    suppressPackageStartupMessages(library(DBI))
    source("shared/db_helpers.R")
    con <- get_db_connection()
    on.exit(safe_disconnect(con), add = TRUE)
    secs <- dbGetQuery(con, "
      SELECT sc.sectcode, ST_AsText(ST_Transform(sc.the_geom, 26915)) AS wkt
      FROM gis_sectcode sc
      WHERE sc.the_geom IS NOT NULL AND sc.fosarea IS NOT NULL AND sc.fosarea <> ''
        AND sc.zone IN ('1', '2')
      ORDER BY sc.sectcode")
    if (nzchar(geom_cache)) saveRDS(secs, geom_cache)
  }
  sections <- sf::st_make_valid(sf::st_as_sf(secs, wkt = "wkt", crs = 26915))
  cat(sprintf("Sections: %d\n", nrow(sections)))

  r <- assign_blocks_to_sections(blocks, sections)
  d <- r$per_section
  outside <- is.na(r$block_section)

  # ---- checks -------------------------------------------------------------------------------
  stopifnot(sum(d$population) + sum(blocks$pop[outside]) == sum(blocks$pop),
            !anyNA(d$population), all(d$population >= 0), !anyDuplicated(d$sectcode))
  cty <- substr(blocks$geoid[!outside], 3, 5)
  cat(sprintf("Residents in sections: %s of %s (%.1f%%); blocks outside every section: %d (%s residents)\n",
              format(sum(d$population), big.mark = ","), format(sum(blocks$pop), big.mark = ","),
              100 * sum(d$population) / sum(blocks$pop), sum(outside),
              format(sum(blocks$pop[outside]), big.mark = ",")))
  cat(sprintf("Sections with no block: %d; with no residents: %d; blocks on a shared edge: %d\n",
              sum(d$blocks == 0), sum(d$population == 0), r$n_edge))
  cat("Residents by county code:\n"); print(sort(tapply(blocks$pop[!outside], cty, sum), decreasing = TRUE))
  cat("Section density (residents / sq mi) quantiles:\n")
  print(round(quantile(d$density, c(0, .25, .5, .75, .9, 1), na.rm = TRUE)))

  d$built <- as.character(Sys.Date())
  d$source <- "2020 Census blocks (POPTOTAL), block internal point in section"
  utils::write.csv(d, out, row.names = FALSE)
  cat(sprintf("Wrote %d sections -> %s\n", nrow(d), out))
  invisible(d)
}

if (!isTRUE(getOption("section_population_no_run")) && !interactive()) {
  if (!file.exists("shared/db_helpers.R")) stop("Run from the project root (mmcd_metrics/).")
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a) >= 1) build_section_population(gpkg = a[1]) else build_section_population()
}
