# Facilities Overview - Data Functions
# =============================================================================
# Sources the unified functions from district_overview.
# Facilities overview uses the SAME data, just aggregated by facility.
# =============================================================================

# Source the unified data functions from district_overview
# This gives us: load_metric_data, load_data_by_zone, load_data_by_facility
source("../district_overview/data_functions.R")

# All the functions we need are now available:
# - load_metric_data(metric, ...) - core unified loader
# - load_data_by_facility(metric, ...) - aggregates by facility
# - load_data_by_zone(metric, ...) - aggregates by zone
# - load_*_overview() - backward compat wrappers
# - get_facility_order(), order_facilities() - facility ordering
