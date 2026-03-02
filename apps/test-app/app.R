# =============================================================================
# ADMIN UTILITIES — System Dashboard
# =============================================================================
# Consolidated admin interface for cache management, runtime monitoring,
# configuration review, and color reference.
# =============================================================================

source("../../shared/app_libraries.R")

# Additional packages needed by admin dashboard
suppressPackageStartupMessages({
  library(shinydashboard)
  library(shinyjs)
  library(jsonlite)
})

# Null-coalescing operator
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b

source("../../shared/db_helpers.R")
source("../../shared/cache_utilities.R")
source("../../shared/config.R")

# =============================================================================
# HELPER: format TTL for display
# =============================================================================
format_ttl <- function(seconds) {
  if (is.null(seconds) || is.na(seconds)) return("?")
  seconds <- as.numeric(seconds)
  if (seconds >= 86400) return(paste0(seconds / 86400, " days"))
  if (seconds >= 3600) return(paste0(seconds / 3600, " hours"))
  if (seconds >= 60) return(paste0(seconds / 60, " min"))
  paste0(seconds, " sec")
}

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "MMCD Admin"),
  
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "tabs",
      menuItem("System Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Cache Manager", tabName = "cache", icon = icon("database")),
      menuItem("Runtime & Routing", tabName = "runtime", icon = icon("server")),
      menuItem("App Config", tabName = "config", icon = icon("cog")),
      menuItem("Metric Registry", tabName = "registry", icon = icon("list")),
      hr(style = "margin: 5px 15px; border-color: #444;"),
      menuItem("Color Reference", tabName = "colors", icon = icon("palette")),
      hr(style = "margin: 5px 15px; border-color: #444;"),
      # Theme selector at bottom
      div(style = "padding: 10px 15px;",
        selectInput("color_theme", 
                    label = tags$span(style = "color: #aaa; font-size: 11px;", "Color Theme"),
                    choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                    selected = "MMCD",
                    width = "100%")
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    # Compact styling
    tags$head(tags$style(HTML("
      .small-box { min-height: 80px; }
      .small-box .inner h3 { font-size: 24px; }
      .small-box .inner p { font-size: 12px; }
      .box { margin-bottom: 10px; }
      .info-box { min-height: 65px; }
      .nav-tabs-custom { margin-bottom: 10px; }
      .content-wrapper { background-color: #f4f6f9; }
      pre { font-size: 12px; max-height: 400px; overflow-y: auto; }
      .dataTables_wrapper { font-size: 13px; }
      .config-section { margin-bottom: 15px; padding: 10px; background: #f8f9fa; 
                         border-radius: 4px; border-left: 3px solid #3c8dbc; }
      .config-section h5 { margin-top: 0; color: #3c8dbc; }
      .color-swatch { display: inline-block; width: 20px; height: 20px; 
                       border: 1px solid #ddd; border-radius: 3px; 
                       vertical-align: middle; margin-right: 5px; }
    "))),
    
    tabItems(
      # =====================================================================
      # SYSTEM OVERVIEW TAB
      # =====================================================================
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("ovRedisStatus", width = 3),
          valueBoxOutput("ovCacheKeys", width = 3),
          valueBoxOutput("ovWorkerMode", width = 3),
          valueBoxOutput("ovPlatform", width = 3)
        ),
        fluidRow(
          box(title = "Cache Summary", status = "primary", solidHeader = TRUE, width = 6,
            verbatimTextOutput("ovCacheSummary")
          ),
          box(title = "Environment", status = "info", solidHeader = TRUE, width = 6,
            verbatimTextOutput("ovEnvironment")
          )
        ),
        fluidRow(
          box(title = "Config Source", status = "success", solidHeader = TRUE, width = 12,
            verbatimTextOutput("ovConfigSource")
          )
        )
      ),
      
      # =====================================================================
      # CACHE MANAGER TAB
      # =====================================================================
      tabItem(tabName = "cache",
        tabBox(
          title = NULL, width = 12,
          # --- Historical Cache Sub-tab ---
          tabPanel("Historical Cache",
            icon = icon("history"),
            fluidRow(
              column(8,
                DTOutput("cacheStatusTable"),
                br(),
                verbatimTextOutput("cacheInfo")
              ),
              column(4,
                div(class = "config-section",
                  h5("Cache Actions"),
                  uiOutput("metricCheckboxes"),
                  hr(),
                  fluidRow(
                    column(6,
                      actionButton("regenerateSelected", "Regen Selected", 
                                   icon = icon("sync"), class = "btn-primary btn-sm btn-block"),
                      br(),
                      actionButton("clearSelected", "Clear Selected", 
                                   icon = icon("trash"), class = "btn-warning btn-sm btn-block")
                    ),
                    column(6,
                      actionButton("regenerateAll", "Regen All", 
                                   icon = icon("sync-alt"), class = "btn-success btn-sm btn-block"),
                      br(),
                      actionButton("clearAll", "Clear All", 
                                   icon = icon("trash-alt"), class = "btn-danger btn-sm btn-block")
                    )
                  ),
                  br(),
                  actionButton("refreshStatus", "Refresh Status", 
                               icon = icon("refresh"), class = "btn-info btn-sm btn-block")
                )
              )
            ),
            fluidRow(
              box(title = "Operation Log", solidHeader = TRUE, 
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                verbatimTextOutput("cacheLog")
              )
            )
          ),
          
          # --- Lookup Cache Sub-tab ---
          tabPanel("Lookup Cache",
            icon = icon("table"),
            fluidRow(
              column(6,
                DTOutput("lookupCacheStatus"),
                br(),
                actionButton("refreshLookupCaches", "Refresh Lookups", 
                             icon = icon("database"), class = "btn-info btn-sm"),
                actionButton("clearLookupCaches", "Clear Lookups", 
                             icon = icon("eraser"), class = "btn-warning btn-sm")
              ),
              column(6,
                div(class = "config-section",
                  h5("Lookup Tables"),
                  tags$ul(
                    tags$li(tags$b("facilities:"), " Facility codes and names"),
                    tags$li(tags$b("foremen:"), " Field supervisor employee data"),
                    tags$li(tags$b("species:"), " Mosquito species codes"),
                    tags$li(tags$b("structure_types:"), " Structure type codes"),
                    tags$li(tags$b("spring_thresholds:"), " ACT4-P1 spring date thresholds")
                  ),
                  tags$p(class = "text-muted", style = "font-size: 11px;",
                    "Stored in Redis (shared across workers) with 5-min in-memory L1 layer."
                  )
                ),
                verbatimTextOutput("lookupCacheLog")
              )
            )
          ),
          
          # --- Tiered Cache Sub-tab ---
          tabPanel("Tiered Cache",
            icon = icon("layer-group"),
            fluidRow(
              column(8,
                verbatimTextOutput("tieredCacheStatus")
              ),
              column(4,
                div(class = "config-section",
                  h5("Short-lived (2 min)"),
                  actionButton("clearDbCache", "DB Queries", 
                               icon = icon("database"), class = "btn-warning btn-sm btn-block"),
                  actionButton("clearChartCache", "Charts", 
                               icon = icon("chart-bar"), class = "btn-warning btn-sm btn-block"),
                  actionButton("clearStatCache", "Stat Boxes", 
                               icon = icon("calculator"), class = "btn-warning btn-sm btn-block")
                ),
                br(),
                div(class = "config-section",
                  h5("Long-lived (7d / 24h)"),
                  actionButton("clearFosCache", "FOS Drill-Down", 
                               icon = icon("users"), class = "btn-danger btn-sm btn-block"),
                  actionButton("clearColorCache", "Color Maps", 
                               icon = icon("palette"), class = "btn-danger btn-sm btn-block"),
                  actionButton("clearFacHistCache", "Facility Historical", 
                               icon = icon("building"), class = "btn-danger btn-sm btn-block")
                ),
                br(),
                fluidRow(
                  column(6, actionButton("clearAllTieredCache", "Clear ALL", 
                                         icon = icon("trash-alt"), class = "btn-danger btn-sm btn-block")),
                  column(6, actionButton("refreshTieredStatus", "Refresh", 
                                         icon = icon("sync"), class = "btn-info btn-sm btn-block"))
                )
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # RUNTIME & ROUTING TAB
      # =====================================================================
      tabItem(tabName = "runtime",
        tabBox(
          title = NULL, width = 12,
          # --- Environment Sub-tab ---
          tabPanel("Environment",
            icon = icon("info-circle"),
            fluidRow(
              column(6,
                box(title = "Runtime Environment", status = "primary", 
                    solidHeader = TRUE, width = 12,
                  verbatimTextOutput("runtimeInfo")
                )
              ),
              column(6,
                box(title = "Redis / Cache Backend", status = "info", 
                    solidHeader = TRUE, width = 12,
                  verbatimTextOutput("redisStatus"),
                  verbatimTextOutput("cacheBackendInfo")
                )
              )
            ),
            fluidRow(
              box(title = "Worker Session", status = "warning", solidHeader = TRUE, 
                  width = 12, collapsible = TRUE,
                verbatimTextOutput("workerInfo")
              )
            )
          ),
          
          # --- Load Balancing Sub-tab ---
          tabPanel("Load Balancing",
            icon = icon("balance-scale"),
            fluidRow(
              column(6,
                box(title = "Worker Load (Live)", status = "success", 
                    solidHeader = TRUE, width = 12,
                  actionButton("refreshDR", "Refresh", icon = icon("sync"), 
                               class = "btn-sm btn-info"),
                  br(), br(),
                  DTOutput("drWorkerLoad")
                )
              ),
              column(6,
                box(title = "Routing Configuration", status = "info", 
                    solidHeader = TRUE, width = 12,
                  verbatimTextOutput("drConfig")
                )
              )
            ),
            fluidRow(
              box(title = "Active Route Mappings", status = "warning", 
                  solidHeader = TRUE, width = 12, collapsible = TRUE,
                DTOutput("drRouteMappings")
              )
            ),
            fluidRow(
              box(title = "Routing Decision Log", status = "primary", 
                  solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                DTOutput("drRouteLog")
              )
            )
          ),
          
          # --- Access Log Sub-tab ---
          tabPanel("Access Log",
            icon = icon("file-alt"),
            fluidRow(
              box(title = "nginx Access Log", status = "info", 
                  solidHeader = TRUE, width = 12,
                actionButton("refreshLB", "Refresh", icon = icon("sync"), 
                             class = "btn-sm btn-info"),
                br(), br(),
                DTOutput("lbRecent")
              )
            ),
            fluidRow(
              box(title = "Sessions by Worker", status = "info", 
                  solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                DTOutput("lbSessions")
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # APP CONFIG TAB
      # =====================================================================
      tabItem(tabName = "config",
        fluidRow(
          box(title = "Configuration Source", status = "success", 
              solidHeader = TRUE, width = 12,
            verbatimTextOutput("configSource"),
            actionButton("reloadConfig", "Reload Config", 
                         icon = icon("refresh"), class = "btn-sm btn-info")
          )
        ),
        fluidRow(
          box(title = "Cache TTLs", status = "primary", solidHeader = TRUE, 
              width = 6, collapsible = TRUE,
            DTOutput("configCacheTTLs")
          ),
          box(title = "Value Box Thresholds", status = "warning", solidHeader = TRUE, 
              width = 6, collapsible = TRUE,
            DTOutput("configThresholds")
          )
        ),
        fluidRow(
          box(title = "Status Colors", status = "danger", solidHeader = TRUE, 
              width = 4, collapsible = TRUE,
            uiOutput("configStatusColors")
          ),
          box(title = "Wiki Links", status = "info", solidHeader = TRUE, 
              width = 4, collapsible = TRUE,
            DTOutput("configWikiLinks")
          ),
          box(title = "Display & Runtime", status = "info", solidHeader = TRUE, 
              width = 4, collapsible = TRUE,
            DTOutput("configDisplaySettings")
          )
        ),
        fluidRow(
          box(title = "Raw YAML", solidHeader = TRUE, 
              width = 12, collapsible = TRUE, collapsed = TRUE,
            verbatimTextOutput("configRawYAML")
          )
        )
      ),
      
      # =====================================================================
      # METRIC REGISTRY TAB
      # =====================================================================
      tabItem(tabName = "registry",
        fluidRow(
          box(title = "Registered Metrics", status = "primary", 
              solidHeader = TRUE, width = 12,
            p(class = "text-muted", "All metrics from apps/overview/metric_registry.R"),
            DTOutput("registryTable")
          )
        )
      ),
      
      # =====================================================================
      # COLOR REFERENCE TAB (consolidated)
      # =====================================================================
      tabItem(tabName = "colors",
        tabBox(
          title = NULL, width = 12,
          tabPanel("Facilities & FOS",
            icon = icon("building"),
            fluidRow(
              column(6, h4("Facilities"), DTOutput("facilityInfo")),
              column(6, h4("Field Operations Supervisors"), DTOutput("fosInfo"))
            )
          ),
          tabPanel("Status Colors",
            icon = icon("traffic-light"),
            fluidRow(
              column(6, h4("Hex Colors (Charts)"), DTOutput("statusColors")),
              column(6, h4("Shiny Colors (ValueBoxes)"), DTOutput("shinyColors"))
            ),
            fluidRow(
              column(12, h4("Color Mapping (Visualizations)"), DTOutput("statusColorMap"))
            )
          ),
          tabPanel("Treatment Plans",
            icon = icon("spray-can"),
            fluidRow(
              column(6, h4("By Code"), DTOutput("treatmentColorsCodes")),
              column(6, h4("By Name"), DTOutput("treatmentColorsNames"))
            ),
            fluidRow(
              column(12, h4("Plan Types"), DTOutput("treatmentPlanTypes"))
            )
          ),
          tabPanel("Mosquito Species",
            icon = icon("bug"),
            DTOutput("mosquitoColors")
          ),
          tabPanel("Theme Preview",
            icon = icon("swatchbook"),
            fluidRow(column(12, uiOutput("themeInfo"))),
            fluidRow(
              column(4, h4("Primary"), DTOutput("themePrimaryColors")),
              column(4, h4("Sequential"), DTOutput("themeSequentialColors")),
              column(4, h4("Diverging"), DTOutput("themeDivergingColors"))
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # ===========================================================================
  # SYSTEM OVERVIEW
  # ===========================================================================
  
  get_platform <- function() {
    plat <- Sys.getenv("MMCD_PLATFORM", "")
    if (nchar(plat) > 0) return(plat)
    is_ecs <- nchar(Sys.getenv("ECS_TASK_ARN", "")) > 0 ||
              nchar(Sys.getenv("ECS_CONTAINER_METADATA_URI_V4", "")) > 0
    if (is_ecs) return("ECS/Fargate")
    hostname <- tryCatch(Sys.info()[["nodename"]], error = function(e) "")
    is_aws <- grepl("compute\\.internal|ec2|apprunner", hostname, ignore.case = TRUE) ||
              nchar(Sys.getenv("AWS_DEFAULT_REGION", "")) > 0
    if (is_aws) return("AWS (unknown)")
    "Local/Docker"
  }
  
  output$ovRedisStatus <- renderValueBox({
    redis_ok <- tryCatch(
      system("redis-cli -h 127.0.0.1 ping > /dev/null 2>&1", intern = FALSE) == 0,
      error = function(e) FALSE
    )
    valueBox(
      if (redis_ok) "Online" else "Offline",
      "Redis", icon = icon("database"),
      color = if (redis_ok) "green" else "red"
    )
  })
  
  output$ovCacheKeys <- renderValueBox({
    total <- tryCatch(length(redis_keys("*")), error = function(e) 0)
    valueBox(total, "Redis Keys", icon = icon("key"), color = "blue")
  })
  
  output$ovWorkerMode <- renderValueBox({
    # Load container .env to pick up ENABLE_NGINX and SHINY_WORKERS
    env_path <- "/srv/shiny-server/.env"
    if (file.exists(env_path)) readRenviron(env_path)
    enable_nginx <- Sys.getenv("ENABLE_NGINX", "false") == "true"
    workers <- as.integer(Sys.getenv("SHINY_WORKERS", "3"))
    valueBox(
      if (enable_nginx) paste0(workers, " Workers") else "Single",
      "Worker Mode", icon = icon("server"),
      color = if (enable_nginx) "green" else "yellow"
    )
  })
  
  output$ovPlatform <- renderValueBox({
    valueBox(get_platform(), "Platform", icon = icon("cloud"), color = "purple")
  })
  
  output$ovCacheSummary <- renderPrint({
    hist_total <- tryCatch(length(redis_keys("mmcd:historical:*")), error = function(e) 0)
    lookup_total <- tryCatch(length(redis_keys("mmcd:lookup:*")), error = function(e) 0)
    app_total <- tryCatch(length(redis_keys("mmcd:app:*")), error = function(e) 0)
    total <- tryCatch(length(redis_keys("*")), error = function(e) 0)
    
    cfg <- get_app_config()
    ttl <- cfg$cache$ttl
    
    cat("Cache Tiers:\n")
    cat(sprintf("  Historical averages:  %4d keys  (TTL: %s)\n", 
                hist_total, format_ttl(ttl$historical_averages)))
    cat(sprintf("  Lookup tables:        %4d keys  (TTL: %s)\n", 
                lookup_total, format_ttl(ttl$lookup_tables)))
    cat(sprintf("  App-level cache:      %4d keys  (TTL: varies)\n", app_total))
    cat(sprintf("  Total Redis keys:     %4d\n", total))
  })
  
  output$ovEnvironment <- renderPrint({
    cat(sprintf("Platform:    %s\n", get_platform()))
    cat(sprintf("Host:        %s\n", Sys.info()[["nodename"]]))
    cat(sprintf("PID:         %d\n", Sys.getpid()))
    cat(sprintf("R Version:   %s\n", R.version.string))
    cat(sprintf("Time:        %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
    cfg_src <- tryCatch(get_config_source(), error = function(e) "(unavailable)")
    cat(sprintf("Config:      %s\n", cfg_src))
  })
  
  output$ovConfigSource <- renderPrint({
    tryCatch(print_config_summary(), error = function(e) cat("Config unavailable:", e$message))
  })
  
  # ===========================================================================
  # CACHE MANAGER
  # ===========================================================================
  
  cache_log <- reactiveVal("")
  cache_trigger <- reactiveVal(0)
  
  log_message <- function(msg) {
    current <- cache_log()
    timestamp <- format(Sys.time(), "%H:%M:%S")
    cache_log(paste0(current, "[", timestamp, "] ", msg, "\n"))
  }
  
  output$metricCheckboxes <- renderUI({
    cache_trigger()
    metrics <- get_cacheable_metrics()
    if (length(metrics) == 0) return(p("No cacheable metrics"))
    checkboxGroupInput("selectedMetrics", "Metrics:", 
                       choices = metrics, selected = metrics)
  })
  
  output$cacheStatusTable <- renderDT({
    cache_trigger()
    status_df <- tryCatch(get_cache_status(), error = function(e) {
      data.frame(metric_id = character(), status = character(), 
                 rows_5yr = integer(), rows_10yr = integer(), 
                 last_updated = character())
    })
    if (nrow(status_df) == 0) return(data.frame(Message = "No cache data available"))
    
    registry <- tryCatch(get_metric_registry(), error = function(e) list())
    data1_labels <- sapply(status_df$metric_id, function(mid) {
      if (isTRUE(registry[[mid]]$historical_type == "yearly_grouped")) "Facilities" 
      else "5yr Avg"
    })
    data2_labels <- sapply(status_df$metric_id, function(mid) {
      if (isTRUE(registry[[mid]]$historical_type == "yearly_grouped")) "District" 
      else "10yr Avg"
    })
    
    data.frame(
      Metric = status_df$metric_id,
      Status = ifelse(status_df$status == "Complete", 
                      '<span style="color:green">&#10003;</span>',
                      ifelse(status_df$status == "Partial",
                             '<span style="color:orange">&#9888;</span>',
                             '<span style="color:red">&#10007;</span>')),
      `Data 1` = paste0(data1_labels, ": ", status_df$rows_5yr),
      `Data 2` = paste0(data2_labels, ": ", status_df$rows_10yr),
      Updated = status_df$last_updated,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 15, dom = 't'))
  
  output$cacheInfo <- renderText({
    cache_trigger()
    redis_active <- exists("redis_is_active", mode = "function") && redis_is_active()
    if (!redis_active) return("Redis offline.")
    tryCatch({
      info <- redis_cache_status()
      meta <- redis_get(HISTORICAL_META_KEY)
      paste0(
        "Backend: Redis (", REDIS_CONFIG$host, ":", REDIS_CONFIG$port, ")\n",
        "Historical: ", info$historical_count, 
        " | Lookups: ", info$lookup_count, "\n",
        if (!is.null(meta$generated_date)) {
          paste0("Generated: ", meta$generated_date, 
                 " (", as.numeric(Sys.Date() - as.Date(meta$generated_date)), 
                 " days ago)")
        } else ""
      )
    }, error = function(e) paste("Error:", e$message))
  })
  
  output$cacheLog <- renderText(cache_log())
  
  # Cache action handlers
  observeEvent(input$regenerateSelected, {
    metrics <- input$selectedMetrics
    if (is.null(metrics) || length(metrics) == 0) { 
      log_message("No metrics selected"); return() 
    }
    log_message(paste("Regenerating:", paste(metrics, collapse = ", ")))
    withProgress(message = "Regenerating...", value = 0, {
      for (i in seq_along(metrics)) {
        incProgress(1/length(metrics), detail = metrics[i])
        tryCatch({
          regenerate_cache(metrics = metrics[i])
          log_message(paste("  done:", metrics[i]))
        }, error = function(e) {
          log_message(paste("  FAIL:", metrics[i], e$message))
        })
      }
    })
    cache_trigger(cache_trigger() + 1)
  })
  
  observeEvent(input$regenerateAll, {
    log_message("Full regeneration...")
    withProgress(message = "Regenerating all...", value = 0, {
      tryCatch({
        regenerate_cache()
        log_message("Complete")
      }, error = function(e) log_message(paste("Error:", e$message)))
    })
    cache_trigger(cache_trigger() + 1)
  })
  
  observeEvent(input$clearSelected, {
    metrics <- input$selectedMetrics
    if (is.null(metrics) || length(metrics) == 0) { 
      log_message("None selected"); return() 
    }
    log_message(paste("Clearing:", paste(metrics, collapse = ", ")))
    clear_cache(metrics)
    cache_trigger(cache_trigger() + 1)
  })
  
  observeEvent(input$clearAll, {
    log_message("Clearing all cache...")
    clear_cache()
    cache_trigger(cache_trigger() + 1)
  })
  
  observeEvent(input$refreshStatus, { 
    cache_trigger(cache_trigger() + 1) 
  })
  
  # --- Lookup Cache ---
  lookup_cache_log <- reactiveVal("")
  lookup_trigger <- reactiveVal(0)
  
  lookup_log_message <- function(msg) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    lookup_cache_log(paste0(lookup_cache_log(), "[", timestamp, "] ", msg, "\n"))
  }
  
  output$lookupCacheStatus <- renderDT({
    lookup_trigger()
    tryCatch({
      df <- get_lookup_cache_status()
      df$status <- ifelse(
        df$status == "Cached",
        '<span style="color:green">&#10003; Cached</span>',
        '<span style="color:gray">&#9675; Empty</span>'
      )
      df
    }, error = function(e) {
      data.frame(lookup_type = "Error", status = e$message, row_count = NA)
    })
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$lookupCacheLog <- renderText(lookup_cache_log())
  
  observeEvent(input$refreshLookupCaches, {
    lookup_log_message("Refreshing lookups...")
    withProgress(message = "Refreshing...", value = 0.5, {
      tryCatch({
        refresh_lookup_caches()
        lookup_log_message("Done")
      }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    })
    lookup_trigger(lookup_trigger() + 1)
  })
  
  observeEvent(input$clearLookupCaches, {
    lookup_log_message("Clearing lookups...")
    tryCatch({
      clear_lookup_cache_types()
      lookup_log_message("Cleared")
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    lookup_trigger(lookup_trigger() + 1)
  })
  
  # --- Tiered Cache ---
  tiered_trigger <- reactiveVal(0)
  
  observeEvent(input$clearDbCache, {
    tryCatch({
      n <- clear_cache_tier("db")
      lookup_log_message(sprintf("Cleared DB cache (%d keys)", n))
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    tiered_trigger(tiered_trigger() + 1)
  })
  
  observeEvent(input$clearChartCache, {
    tryCatch({
      n <- clear_cache_tier("chart")
      lookup_log_message(sprintf("Cleared chart cache (%d keys)", n))
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    tiered_trigger(tiered_trigger() + 1)
  })
  
  observeEvent(input$clearStatCache, {
    tryCatch({
      n <- clear_cache_tier("stat")
      lookup_log_message(sprintf("Cleared stat box cache (%d keys)", n))
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    tiered_trigger(tiered_trigger() + 1)
  })
  
  observeEvent(input$clearFosCache, {
    tryCatch({
      n <- clear_cache_tier("fos")
      lookup_log_message(sprintf("Cleared FOS cache (%d keys)", n))
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    tiered_trigger(tiered_trigger() + 1)
  })
  
  observeEvent(input$clearColorCache, {
    tryCatch({
      n <- clear_cache_tier("color")
      lookup_log_message(sprintf("Cleared color cache (%d keys)", n))
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    tiered_trigger(tiered_trigger() + 1)
  })
  
  observeEvent(input$clearFacHistCache, {
    tryCatch({
      n <- clear_cache_tier("fachist")
      lookup_log_message(sprintf("Cleared facility historical (%d keys)", n))
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    tiered_trigger(tiered_trigger() + 1)
  })
  
  observeEvent(input$clearAllTieredCache, {
    tryCatch({
      clear_cache_tier("all")
      lookup_log_message("ALL tiered caches cleared")
    }, error = function(e) lookup_log_message(paste("Error:", e$message)))
    tiered_trigger(tiered_trigger() + 1)
  })
  
  output$tieredCacheStatus <- renderText({
    tiered_trigger()
    input$refreshTieredStatus
    invalidateLater(10000)
    tryCatch({
      counts <- get_cache_tier_counts()
      redis_total <- tryCatch(length(redis_keys("*")), error = function(e) "?")
      hist_total <- tryCatch(length(redis_keys("mmcd:historical:*")), error = function(e) 0)
      lookup_total <- tryCatch(length(redis_keys("mmcd:lookup:*")), error = function(e) 0)
      fos_hist <- tryCatch(length(redis_keys("mmcd:hist_fos:*")), error = function(e) 0)
      
      cfg <- get_app_config()
      ttl <- cfg$cache$ttl
      
      paste(
        "=== App-Level Cache ===",
        sprintf("DB Queries:     %3d keys  (%s TTL)", 
                counts$db_queries, format_ttl(ttl$db_queries)),
        sprintf("Charts:         %3d keys  (%s TTL)", 
                counts$charts, format_ttl(ttl$charts)),
        sprintf("Stat Boxes:     %3d keys  (%s TTL)", 
                counts$stat_boxes, format_ttl(ttl$stat_boxes)),
        sprintf("FOS Drill-down: %3d keys  (%s TTL)", 
                counts$fos_drilldown, format_ttl(ttl$fos_drilldown)),
        sprintf("Color Maps:     %3d keys  (%s TTL)", 
                counts$color_maps, format_ttl(ttl$color_mappings)),
        sprintf("Fac Historical: %3d keys  (%s TTL)", 
                counts$fac_hist, format_ttl(ttl$facility_historical)),
        "",
        "=== Global Redis ===",
        sprintf("Historical Avg: %3d keys  (%s TTL)", 
                hist_total, format_ttl(ttl$historical_averages)),
        sprintf("FOS Hist Avg:   %3d keys  (%s TTL)", 
                fos_hist, format_ttl(ttl$fos_drilldown)),
        sprintf("Lookup Tables:  %3d keys  (%s TTL)", 
                lookup_total, format_ttl(ttl$lookup_tables)),
        sprintf("Total Redis:    %s keys", redis_total),
        "",
        paste0("Checked: ", format(Sys.time(), "%H:%M:%S")),
        sep = "\n"
      )
    }, error = function(e) paste("Error:", e$message))
  })
  
  # ===========================================================================
  # RUNTIME & ROUTING
  # ===========================================================================
  
  output$runtimeInfo <- renderPrint({
    env_path <- "/srv/shiny-server/.env"
    if (file.exists(env_path)) readRenviron(env_path)
    
    platform <- get_platform()
    enable_nginx <- Sys.getenv("ENABLE_NGINX", "false") == "true"
    shiny_workers <- as.integer(Sys.getenv("SHINY_WORKERS", "3"))
    redis_available <- tryCatch(
      system("redis-cli -h 127.0.0.1 ping > /dev/null 2>&1", intern = FALSE) == 0,
      error = function(e) FALSE
    )
    
    cat(sprintf("Platform:     %s\n", platform))
    cat(sprintf("Host:         %s\n", Sys.info()[["nodename"]]))
    cat(sprintf("PID:          %d\n", Sys.getpid()))
    cat(sprintf("R Version:    %s\n", R.version.string))
    cat(sprintf("Time:         %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
    
    if (enable_nginx) {
      cat(sprintf("Mode:         MULTI-WORKER (%d instances)\n", shiny_workers))
      cat(sprintf("Load Balancer:nginx on :3838\n"))
      cat(sprintf("Worker Ports: %s\n", 
                  paste(3839:(3839 + shiny_workers - 1), collapse = ", ")))
    } else {
      cat("Mode:         SINGLE-WORKER\n")
      cat("Port:         3838\n")
    }
    cat(sprintf("\nRedis:        %s\n", 
                if (redis_available) "Available" else "Unavailable"))
  })
  
  # Worker info
  find_worker_info <- function() {
    map_file <- "/srv/shiny-server/.worker_map"
    if (!file.exists(map_file)) 
      return(list(id = "N/A", port = "N/A", debug = "no worker map"))
    map_lines <- readLines(map_file, warn = FALSE)
    map_lines <- trimws(map_lines[nzchar(trimws(map_lines))])
    if (length(map_lines) == 0) 
      return(list(id = "N/A", port = "N/A", debug = "map empty"))
    
    map_df <- tryCatch({
      do.call(rbind, lapply(map_lines, function(line) {
        parts <- strsplit(line, "\\s+")[[1]]
        if (length(parts) >= 3) {
          data.frame(pid = as.integer(parts[1]), id = parts[2], 
                     port = parts[3], stringsAsFactors = FALSE)
        }
      }))
    }, error = function(e) NULL)
    if (is.null(map_df) || nrow(map_df) == 0) 
      return(list(id = "N/A", port = "N/A", debug = "parse error"))
    
    current_pid <- Sys.getpid()
    visited <- integer(0)
    for (step in 1:10) {
      if (current_pid %in% visited || current_pid <= 1) break
      visited <- c(visited, current_pid)
      idx <- which(map_df$pid == current_pid)
      if (length(idx) > 0) {
        return(list(id = map_df$id[idx[1]], port = map_df$port[idx[1]],
                    debug = paste("matched PID", current_pid)))
      }
      status_file <- paste0("/proc/", current_pid, "/status")
      if (!file.exists(status_file)) break
      ppid_line <- grep("^PPid:", readLines(status_file, warn = FALSE), 
                        value = TRUE)
      if (length(ppid_line) == 0) break
      current_pid <- as.integer(sub("PPid:\\s+", "", ppid_line[1]))
    }
    list(id = "not matched", port = "unknown", 
         debug = paste("walked:", paste(visited, collapse = "->")))
  }
  
  output$workerInfo <- renderPrint({
    env_path <- "/srv/shiny-server/.env"
    if (file.exists(env_path)) readRenviron(env_path)
    worker <- find_worker_info()
    cat(sprintf("Instance:       %s (port %s)\n", worker$id, worker$port))
    cat(sprintf("PID:            %d\n", Sys.getpid()))
    cat(sprintf("Detection:      %s\n", worker$debug))
    cat(sprintf("Session Host:   %s\n", 
                tryCatch(session$clientData$url_hostname, error = function(e) "unknown")))
    cat(sprintf("Remote Addr:    %s\n", 
                tryCatch(session$request$REMOTE_ADDR, error = function(e) "unknown")))
    cat(sprintf("X-Forwarded:    %s\n", 
                tryCatch(session$request$HTTP_X_FORWARDED_FOR, error = function(e) "unknown")))
  })
  
  output$redisStatus <- renderPrint({
    if (exists("redis_cache_status", mode = "function")) {
      redis_cache_status()
    } else {
      cat("redis_cache_status() not available")
    }
  })
  
  output$cacheBackendInfo <- renderPrint({
    list(
      backend = if (exists("REDIS_CONFIG")) REDIS_CONFIG$backend else "unknown",
      active = if (exists("redis_is_active", mode = "function")) redis_is_active() else FALSE,
      host = if (exists("REDIS_CONFIG")) 
               paste0(REDIS_CONFIG$host, ":", REDIS_CONFIG$port) else "unknown"
    )
  })
  
  # --- nginx log parser ---
  parse_nginx_access_log <- function(log_path, max_lines = 200) {
    if (!file.exists(log_path)) 
      return(data.frame(Message = "Log not found", stringsAsFactors = FALSE))
    if (file.access(log_path, mode = 4) != 0) 
      return(data.frame(Message = "Permission denied", stringsAsFactors = FALSE))
    
    lines <- tryCatch(readLines(log_path, warn = FALSE), 
                      error = function(e) character(0))
    if (length(lines) == 0) 
      return(data.frame(Message = "Empty log", stringsAsFactors = FALSE))
    
    lines <- tail(lines, max_lines)
    pattern <- paste0(
      "^([^ ]+) - ([^ ]+) \\[([^]]+)\\] \"([^\"]*)\" ([0-9]{3}) ([0-9]+) ",
      "\"([^\"]*)\" \"([^\"]*)\" upstream=([^ ]*) xff=\"([^\"]*)\" ",
      "req_time=([^ ]*) upstream_time=([^ ]*) upstream_status=([^ ]*)"
    )
    matches <- regexec(pattern, lines)
    parts <- regmatches(lines, matches)
    
    rows <- lapply(parts, function(m) {
      if (length(m) == 0) return(NULL)
      req_parts <- strsplit(m[5], " ")[[1]]
      data.frame(
        Time = m[4], Method = req_parts[1] %||% "",
        Path = req_parts[2] %||% m[5], Status = m[6],
        Upstream = m[10], ReqTime = m[12],
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, rows)
    if (is.null(df) || nrow(df) == 0) 
      return(data.frame(Message = "No parseable lines", stringsAsFactors = FALSE))
    df
  }
  
  output$lbRecent <- renderDT({
    input$refreshLB
    parse_nginx_access_log("/var/log/nginx/access.log", 200)
  }, options = list(pageLength = 20, order = list(list(0, "desc"))))
  
  output$lbSessions <- renderDT({
    input$refreshLB
    df <- parse_nginx_access_log("/var/log/nginx/access.log", 500)
    if (!all(c("Path", "Upstream", "Time") %in% names(df))) return(df)
    session_id <- sub(".*/session/([^/]+).*", "\\1", df$Path)
    is_session <- grepl("/session/", df$Path)
    df <- df[is_session, , drop = FALSE]
    session_id <- session_id[is_session]
    if (nrow(df) == 0) return(data.frame(Message = "No session requests"))
    aggregate(list(Sessions = session_id), list(Upstream = df$Upstream), 
              function(x) length(unique(x)))
  }, options = list(pageLength = 10))
  
  # --- Dynamic Routing ---
  get_monitor_redis <- function() {
    tryCatch(redux::hiredis(host = "127.0.0.1", port = 6379L), 
             error = function(e) NULL)
  }
  
  output$drWorkerLoad <- renderDT({
    input$refreshDR
    tryCatch({
      r <- get_monitor_redis()
      if (is.null(r)) return(data.frame(Worker = "N/A", Load = "Redis unavailable", 
                                         check.names = FALSE))
      workers_raw <- r$SMEMBERS("mmcd:workers")
      if (is.null(workers_raw) || length(workers_raw) == 0)
        return(data.frame(Worker = "N/A", Load = "No workers", check.names = FALSE))
      workers <- sort(unlist(workers_raw))
      loads <- vapply(workers, function(w) {
        val <- tryCatch(r$GET(paste0("mmcd:load:", w)), error = function(e) NULL)
        if (is.null(val)) 0L else as.integer(val)
      }, integer(1))
      data.frame(Worker = workers, `Active WS` = loads, 
                 check.names = FALSE, stringsAsFactors = FALSE)
    }, error = function(e) {
      data.frame(Worker = "Error", Load = e$message, check.names = FALSE)
    })
  }, options = list(pageLength = 10, dom = "t"))
  
  output$drConfig <- renderPrint({
    tryCatch({
      r <- get_monitor_redis()
      cfg <- get_app_config()
      route_ttl <- cfg$runtime$route_ttl_seconds %||% 600
      
      workers_file <- "/etc/nginx/workers.list"
      workers_list <- if (file.exists(workers_file)) {
        readLines(workers_file, warn = FALSE)
      } else {
        "(not found)"
      }
      
      cat("Strategy:    Least-loaded worker\n")
      cat("Route TTL:   ", route_ttl, "seconds\n")
      cat("Sticky:      Yes\n")
      cat("Fallback:    Consistent hash\n\n")
      cat("Workers:\n")
      for (w in workers_list) cat("  ", w, "\n")
      
      if (!is.null(r)) {
        routes <- tryCatch(length(r$KEYS("mmcd:route:*")), error = function(e) "?")
        cat(sprintf("\nActive Routes:  %s\n", routes))
        cat(sprintf("Workers Reg:    %s\n", 
                    tryCatch(r$SCARD("mmcd:workers"), error = function(e) "?")))
      }
    }, error = function(e) cat("Error:", e$message))
  })
  
  output$drRouteMappings <- renderDT({
    input$refreshDR
    tryCatch({
      r <- get_monitor_redis()
      if (is.null(r)) return(data.frame(`Client IP` = "N/A", Worker = "Redis unavailable", 
                                         TTL = NA, check.names = FALSE))
      keys <- tryCatch(r$KEYS("mmcd:route:*"), error = function(e) list())
      if (length(keys) == 0) return(data.frame(`Client IP` = "(none)", 
                                                Worker = "No routes", TTL = NA, 
                                                check.names = FALSE))
      rows <- lapply(keys, function(key) {
        key_str <- as.character(key)
        data.frame(
          `Client IP` = sub("^mmcd:route:", "", key_str),
          Worker = tryCatch(as.character(r$GET(key_str)), error = function(e) "?"),
          TTL = tryCatch(as.integer(r$TTL(key_str)), error = function(e) -1L),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      })
      do.call(rbind, rows)
    }, error = function(e) {
      data.frame(`Client IP` = "Error", Worker = e$message, TTL = NA, 
                 check.names = FALSE)
    })
  }, options = list(pageLength = 20))
  
  output$drRouteLog <- renderDT({
    input$refreshDR
    tryCatch({
      r <- get_monitor_redis()
      if (is.null(r)) return(data.frame(Time = "N/A", IP = "Redis unavailable", 
                                         Target = "", check.names = FALSE))
      log_raw <- tryCatch(r$LRANGE("mmcd:route_log", 0L, 49L), 
                          error = function(e) list())
      if (length(log_raw) == 0) return(data.frame(Time = "(none)", 
                                                   IP = "No decisions logged", 
                                                   Target = "", check.names = FALSE))
      rows <- lapply(log_raw, function(entry) {
        p <- tryCatch(jsonlite::fromJSON(as.character(entry)), 
                      error = function(e) NULL)
        if (is.null(p)) return(NULL)
        data.frame(
          Time = p$time_fmt %||% as.character(p$time),
          IP = p$client_ip %||% "?",
          Target = p$target %||% "?",
          Load = as.integer(p$load %||% 0),
          Reason = p$reason %||% "?",
          check.names = FALSE, stringsAsFactors = FALSE
        )
      })
      rows <- rows[!sapply(rows, is.null)]
      if (length(rows) == 0) return(data.frame(Time = "(none)", 
                                                IP = "No parseable entries", 
                                                Target = "", check.names = FALSE))
      do.call(rbind, rows)
    }, error = function(e) {
      data.frame(Time = "Error", IP = e$message, Target = "", check.names = FALSE)
    })
  }, options = list(pageLength = 20))
  
  # ===========================================================================
  # APP CONFIG TAB
  # ===========================================================================
  
  config_trigger <- reactiveVal(0)
  
  observeEvent(input$reloadConfig, {
    get_app_config(force_reload = TRUE)
    config_trigger(config_trigger() + 1)
  })
  
  output$configSource <- renderPrint({
    config_trigger()
    src <- tryCatch(get_config_source(), error = function(e) "(error)")
    cat("Config file:", src, "\n")
    cat("Loaded at:  ", format(Sys.time(), "%H:%M:%S"), "\n")
  })
  
  output$configCacheTTLs <- renderDT({
    config_trigger()
    cfg <- get_app_config()
    ttl <- cfg$cache$ttl
    data.frame(
      Setting = names(ttl),
      Seconds = as.integer(unlist(ttl)),
      Human = sapply(unlist(ttl), format_ttl),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }, options = list(pageLength = 15, dom = 't'))
  
  output$configThresholds <- renderDT({
    config_trigger()
    cfg <- get_app_config()
    rows <- list()
    
    for (nm in names(cfg$thresholds$historical)) {
      t <- cfg$thresholds$historical[[nm]]
      rows[[length(rows) + 1]] <- data.frame(
        Mode = "historical", Metric = nm, 
        Direction = t$direction %||% "",
        Good = as.character(t$good), 
        Warning = as.character(t$warning), 
        stringsAsFactors = FALSE)
    }
    for (nm in names(cfg$thresholds$fixed_pct)) {
      t <- cfg$thresholds$fixed_pct[[nm]]
      rows[[length(rows) + 1]] <- data.frame(
        Mode = "fixed_pct", Metric = nm, 
        Direction = t$direction %||% "",
        Good = as.character(t$good), 
        Warning = as.character(t$warning), 
        stringsAsFactors = FALSE)
    }
    for (nm in names(cfg$thresholds$pct_of_average)) {
      t <- cfg$thresholds$pct_of_average[[nm]]
      rows[[length(rows) + 1]] <- data.frame(
        Mode = "pct_of_avg", Metric = nm, 
        Direction = t$direction %||% "",
        Good = as.character(t$good), 
        Warning = as.character(t$warning), 
        stringsAsFactors = FALSE)
    }
    for (nm in names(cfg$thresholds$capacity)) {
      t <- cfg$thresholds$capacity[[nm]]
      rows[[length(rows) + 1]] <- data.frame(
        Mode = "capacity", Metric = nm, 
        Direction = t$direction %||% "",
        Good = as.character(t$at_capacity %||% ""), 
        Warning = as.character(t$near_capacity %||% ""), 
        stringsAsFactors = FALSE)
    }
    
    do.call(rbind, rows)
  }, options = list(pageLength = 20, dom = 't'))
  
  output$configStatusColors <- renderUI({
    config_trigger()
    cfg <- get_app_config()
    colors <- cfg$thresholds$colors
    tagList(
      lapply(names(colors), function(nm) {
        div(style = "margin: 8px 0;",
          tags$span(class = "color-swatch", 
                    style = paste0("background-color:", colors[[nm]], ";")),
          tags$b(nm), " \u2014 ", tags$code(colors[[nm]])
        )
      })
    )
  })
  
  output$configWikiLinks <- renderDT({
    config_trigger()
    cfg <- get_app_config()
    links <- cfg$wiki_links
    data.frame(
      Metric = names(links),
      Link = sapply(unlist(links), function(l) {
        if (nchar(l) > 0) l else "(not set)"
      }),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }, options = list(pageLength = 15, dom = 't'))
  
  output$configDisplaySettings <- renderDT({
    config_trigger()
    cfg <- get_app_config()
    settings <- c(cfg$display, cfg$runtime)
    data.frame(
      Setting = names(settings),
      Value = as.character(unlist(settings)),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }, options = list(pageLength = 15, dom = 't'))
  
  output$configRawYAML <- renderPrint({
    config_trigger()
    config_path <- tryCatch(get_config_source(), error = function(e) NULL)
    if (!is.null(config_path) && file.exists(config_path)) {
      cat(paste(readLines(config_path, warn = FALSE), collapse = "\n"))
    } else {
      cat("Config file not available at:", config_path %||% "(unknown)")
    }
  })
  
  # ===========================================================================
  # METRIC REGISTRY
  # ===========================================================================
  
  output$registryTable <- renderDT({
    ensure_registry_loaded()
    if (!exists("get_metric_registry", mode = "function"))
      return(data.frame(Message = "Registry not loaded"))
    
    registry <- get_metric_registry()
    df <- do.call(rbind, lapply(names(registry), function(id) {
      config <- registry[[id]]
      data.frame(
        ID = id,
        Name = config$display_name %||% id,
        Category = config$category %||% "",
        Folder = config$app_folder %||% "",
        Historical = if (isTRUE(config$historical_enabled)) "Yes" else "No",
        `Color Mode` = config$color_mode %||% "historical",
        Cacheable = if (id %in% get_cacheable_metrics()) 
          '<span style="color:green">Yes</span>' 
        else 
          '<span style="color:gray">No</span>',
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    df
  }, escape = FALSE, options = list(pageLength = 20, dom = 't'))
  
  # ===========================================================================
  # COLOR REFERENCE
  # ===========================================================================
  
  current_theme <- reactive({ input$color_theme })
  
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
  
  output$facilityInfo <- renderDT({
    facilities <- get_facility_lookup()
    if (nrow(facilities) == 0) return(NULL)
    colors <- get_facility_base_colors(theme = current_theme())
    data.frame(
      Facility = facilities$short_name,
      City = facilities$full_name,
      Color = sprintf(
        '<div style="background-color:%s;width:60px;height:18px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors[facilities$short_name]
      ),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$fosInfo <- renderDT({
    foremen <- get_foremen_lookup()
    if (nrow(foremen) == 0) return(NULL)
    colors <- get_themed_foreman_colors(theme = current_theme())
    data.frame(
      `Emp#` = foremen$emp_num,
      Name = foremen$shortname,
      Facility = foremen$facility,
      Color = sprintf(
        '<div style="background-color:%s;width:60px;height:18px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors[foremen$shortname]
      ),
      check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
    )
  }, escape = FALSE, options = list(pageLength = 10))
  
  output$statusColors <- renderDT({
    colors <- get_status_colors(theme = current_theme())
    descriptions <- get_status_descriptions()
    data.frame(
      Status = names(colors),
      Hex = as.character(colors),
      Desc = descriptions[names(colors)],
      Preview = sprintf(
        '<div style="background-color:%s;width:60px;height:18px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$shinyColors <- renderDT({
    colors <- get_shiny_colors()
    data.frame(
      Status = names(colors),
      Color = as.character(colors),
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 10, dom = 't'))
  
  output$statusColorMap <- renderDT({
    color_map <- get_status_color_map(theme = current_theme())
    data.frame(
      Status = names(color_map),
      Hex = as.character(unlist(color_map)),
      Preview = sprintf(
        '<div style="background-color:%s;width:60px;height:18px;border:1px solid #ddd;border-radius:2px;"></div>',
        unlist(color_map)
      ),
      stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$treatmentColorsCodes <- renderDT({
    colors <- get_treatment_plan_colors(use_names = FALSE)
    if (length(colors) == 0) return(data.frame(Message = "None available"))
    data.frame(
      Code = names(colors),
      Hex = as.character(colors),
      Preview = sprintf(
        '<div style="background-color:%s;width:60px;height:18px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$treatmentColorsNames <- renderDT({
    colors <- get_treatment_plan_colors(use_names = TRUE)
    if (length(colors) == 0) return(data.frame(Message = "None available"))
    data.frame(
      Name = names(colors),
      Hex = as.character(colors),
      Preview = sprintf(
        '<div style="background-color:%s;width:60px;height:18px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$treatmentPlanTypes <- renderDT({
    plan_types <- get_treatment_plan_types()
    if (nrow(plan_types) == 0) return(data.frame(Message = "None available"))
    data.frame(
      Code = plan_types$plan_code,
      Name = plan_types$plan_name,
      Description = plan_types$description,
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 10))
  
  output$mosquitoColors <- renderDT({
    colors <- get_mosquito_species_colors()
    if (length(colors) == 0) return(data.frame(Message = "None available"))
    data.frame(
      Species = names(colors),
      Hex = as.character(unlist(colors)),
      Preview = sprintf(
        '<div style="background-color:%s;width:40px;height:14px;border:1px solid #ddd;border-radius:2px;"></div>',
        unlist(colors)
      ),
      stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(
    pageLength = 20, scrollY = "400px", scrollCollapse = TRUE
  ))
  
  output$themeInfo <- renderUI({
    theme <- current_theme()
    if (!exists("get_theme_description", mode = "function")) 
      return(tags$p("Theme info not available"))
    tagList(
      tags$h4(paste("Theme:", theme)),
      tags$p(get_theme_description(theme))
    )
  })
  
  output$themePrimaryColors <- renderDT({
    if (!exists("get_theme_palette", mode = "function")) 
      return(data.frame(Message = "N/A"))
    colors <- get_theme_palette(current_theme())$primary
    data.frame(
      `#` = seq_along(colors), Hex = colors,
      Preview = sprintf(
        '<div style="background-color:%s;width:40px;height:20px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$themeSequentialColors <- renderDT({
    if (!exists("get_theme_palette", mode = "function")) 
      return(data.frame(Message = "N/A"))
    colors <- get_theme_palette(current_theme())$sequential
    data.frame(
      `#` = seq_along(colors), Hex = colors,
      Preview = sprintf(
        '<div style="background-color:%s;width:40px;height:20px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$themeDivergingColors <- renderDT({
    if (!exists("get_theme_palette", mode = "function")) 
      return(data.frame(Message = "N/A"))
    colors <- get_theme_palette(current_theme())$diverging
    data.frame(
      `#` = seq_along(colors), Hex = colors,
      Preview = sprintf(
        '<div style="background-color:%s;width:40px;height:20px;border:1px solid #ddd;border-radius:2px;"></div>',
        colors
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
}

shinyApp(ui = ui, server = server)
