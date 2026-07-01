# =============================================================================
# API Routes — Reference lookups
# =============================================================================
# Small, cross-cutting reference data that isn't tied to one app: town/township
# name<->code mapping, etc. Mounted under /v1/public/reference/...
# NO new SQL beyond reading existing lookup tables.
# =============================================================================

source("/srv/shiny-server/shared/db_helpers.R")
source("/srv/shiny-server/shared/app_libraries.R")
source("/srv/api/api_helpers.R")

#* Get the township/city list — 4-digit town code + city name.
#* A "town" is the first 4 digits of a sitecode/sectcode, shared across all site
#* types (ground/air/drone/struct/cb). Use to resolve a place name like "Eagan" to
#* its code (1904) for the `town` filter on data endpoints.
#* @get /towns
#* @serializer json
function(req, res) {
  tryCatch({
    lkp <- get_town_lookup()
    if (is.null(lkp) || nrow(lkp) == 0) return(list())
    lapply(seq_len(nrow(lkp)), function(i) {
      list(towncode = as.character(lkp$towncode4[i]), city = as.character(lkp$city[i]))
    })
  }, error = function(e) api_error(res, 400, e$message))
}
