# TEST APP
# Admin utilities for colors, name mapping, and cache management

library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(DT)
library(jsonlite)

# Null-coalescing operator (available in R 4.4+ but define for compatibility)
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b

source("../../shared/db_helpers.R")
source("../../shared/cache_utilities.R")

ui <- dashboardPage(
  dashboardHeader(title = "Admin Utilities"),
  
  dashboardSidebar(
    sidebarMenu(
      # Theme selector at top of sidebar
      div(style = "padding: 15px; background-color: #2c3b41;",
        selectInput("color_theme", 
                    label = tags$strong(style = "color: white;", "Color Theme:"),
                    choices = c("MMCD", "IBM", "Wong", "Tol", "Viridis", "ColorBrewer"),
                    selected = "MMCD"),
        tags$div(id = "theme_description", 
                style = "color: #bbb; font-size: 11px; margin-top: -10px;",
                "MMCD default color scheme"),
        tags$script(HTML("
          Shiny.addCustomMessageHandler('updateThemeDesc', function(message) {
            $('#theme_description').text(message);
          });
        "))
      ),
      hr(style = "margin: 5px 0; border-color: #444;"),
      menuItem("Cache Manager", tabName = "cache_manager", icon = icon("database")),
      menuItem("Runtime Info", tabName = "runtime_info", icon = icon("server")),
      menuItem("Dynamic Routing", tabName = "dynamic_routing", icon = icon("random")),
      menuItem("Metric Registry", tabName = "metric_registry", icon = icon("list")),
      hr(style = "margin: 5px 0; border-color: #444;"),
      menuItem("Facilities", tabName = "facilities", icon = icon("building")),
      menuItem("FOS", tabName = "fos", icon = icon("users")),
      menuItem("Status Colors", tabName = "status_colors", icon = icon("palette")),
      menuItem("Treatment Plan Colors", tabName = "treatment_colors", icon = icon("spray-can")),
      menuItem("Mosquito Species Colors", tabName = "mosquito_colors", icon = icon("bug")),
      menuItem("Theme Preview", tabName = "theme_preview", icon = icon("swatchbook"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Cache Manager tab (NEW)
      tabItem(tabName = "cache_manager",
        fluidRow(
          box(
            title = "Historical Cache Status",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            DTOutput("cacheStatusTable"),
            br(),
            verbatimTextOutput("cacheInfo")
          ),
          box(
            title = "Cache Actions",
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            p("Select metrics to regenerate their cache data. This pulls fresh data from the database and recalculates 5-year and 10-year averages."),
            uiOutput("metricCheckboxes"),
            hr(),
            actionButton("regenerateSelected", "Regenerate Selected", 
                         icon = icon("sync"), class = "btn-primary"),
            actionButton("regenerateAll", "Regenerate All", 
                         icon = icon("sync-alt"), class = "btn-success"),
            hr(),
            actionButton("clearSelected", "Clear Selected", 
                         icon = icon("trash"), class = "btn-warning"),
            actionButton("clearAll", "Clear All Cache", 
                         icon = icon("trash-alt"), class = "btn-danger"),
            hr(),
            actionButton("refreshStatus", "Refresh Status", icon = icon("refresh"))
          )
        ),
        fluidRow(
          box(
            title = "Cache Operation Log",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("cacheLog")
          )
        ),
        # Lookup Cache Section
        fluidRow(
          box(
            title = "Lookup Cache (In-Memory)",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            p("Cached lookup tables (facilities, foremen, species, etc.) are stored in memory to reduce database queries."),
            DTOutput("lookupCacheStatus"),
            br(),
            actionButton("refreshLookupCaches", "Refresh All Lookups", 
                         icon = icon("database"), class = "btn-info"),
            actionButton("clearLookupCaches", "Clear Lookup Cache",
                         icon = icon("eraser"), class = "btn-warning")
          ),
          box(
            title = "Lookup Cache Info",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            p(strong("Note:"), " Lookup caches are automatically populated on first access and cleared when the server restarts."),
            tags$ul(
              tags$li(strong("facilities:"), " Facility codes and names"),
              tags$li(strong("foremen:"), " Field supervisor employee data"),
              tags$li(strong("species:"), " Mosquito species codes"),
              tags$li(strong("structure_types:"), " Structure type codes"),
              tags$li(strong("spring_thresholds:"), " ACT4-P1 spring date thresholds")
            ),
            verbatimTextOutput("lookupCacheLog")
          )
        )
      ),

      # Runtime Info tab (NEW)
      tabItem(tabName = "runtime_info",
        fluidRow(
          box(
            title = "Runtime Environment",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("runtimeInfo")
          ),
          box(
            title = "Redis / Cache Status",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("redisStatus"),
            br(),
            verbatimTextOutput("cacheBackendInfo")
          )
        ),
        fluidRow(
          box(
            title = "Worker Session",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("workerInfo")
          )
        ),
        fluidRow(
          box(
            title = "Load Balancer (Recent Requests)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            actionButton("refreshLB", "Refresh", icon = icon("sync"), class = "btn-sm btn-info"),
            br(), br(),
            DTOutput("lbRecent")
          )
        ),
        fluidRow(
          box(
            title = "Load Balancer (Active Sessions by Worker)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("lbSessions")
          )
        )
      ),

      # Dynamic Routing tab
      tabItem(tabName = "dynamic_routing",
        fluidRow(
          box(
            title = "Dynamic Routing — Worker Load (Live)",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            actionButton("refreshDR", "Refresh", icon = icon("sync"), class = "btn-sm btn-info"),
            br(), br(),
            DTOutput("drWorkerLoad")
          ),
          box(
            title = "Dynamic Routing — Configuration",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("drConfig")
          )
        ),
        fluidRow(
          box(
            title = "Active Route Mappings (IP → Worker)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("drRouteMappings")
          )
        ),
        fluidRow(
          box(
            title = "Routing Decision Log (Recent)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("drRouteLog")
          )
        ),
        fluidRow(
          box(
            title = "nginx Access Log (Recent Requests)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("drAccessLog")
          )
        )
      ),
      
      # Metric Registry tab (NEW)
      tabItem(tabName = "metric_registry",
        fluidRow(
          box(
            title = "Registered Metrics",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("All metrics registered in the system. Add new metrics to apps/overview/metric_registry.R"),
            DTOutput("registryTable")
          )
        )
      ),
      
      # Facilities tab
      tabItem(tabName = "facilities",
        fluidRow(
          box(
            title = "Facility Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("facilityInfo")
          )
        )
      ),
      
      # FOS tab
      tabItem(tabName = "fos",
        fluidRow(
          box(
            title = "FOS (Field Operations Supervisors) Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("fosInfo")
          )
        )
      ),
      
      # Status Colors tab
      tabItem(tabName = "status_colors",
        fluidRow(
          box(
            title = "Status Colors (Hex for Charts)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("statusColors")
          ),
          box(
            title = "Shiny Colors (Named for ValueBoxes)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("shinyColors")
          )
        ),
        fluidRow(
          box(
            title = "Status Color Mapping (for Visualizations)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("statusColorMap")
          )
        )
      ),
      
      # Treatment Plan Colors tab
      tabItem(tabName = "treatment_colors",
        fluidRow(
          box(
            title = "Treatment Plan Colors (by Code)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("treatmentColorsCodes")
          ),
          box(
            title = "Treatment Plan Colors (by Name)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("treatmentColorsNames")
          )
        ),
        fluidRow(
          box(
            title = "Treatment Plan Types Information",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("treatmentPlanTypes")
          )
        )
      ),
      
      # Mosquito Species Colors tab
      tabItem(tabName = "mosquito_colors",
        fluidRow(
          box(
            title = "Mosquito Species Colors",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "600px",
            style = "overflow-y: auto;",
            DTOutput("mosquitoColors")
          )
        )
      ),
      
      # Theme Preview tab
      tabItem(tabName = "theme_preview",
        fluidRow(
          box(
            title = "Theme Information",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            uiOutput("themeInfo")
          )
        ),
        fluidRow(
          box(
            title = "Primary Palette Colors",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("themePrimaryColors")
          ),
          box(
            title = "Sequential Palette",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("themeSequentialColors")
          )
        ),
        fluidRow(
          box(
            title = "Diverging Palette",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("themeDivergingColors")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # ==========================================================================
  # CACHE MANAGER - Reactive values and handlers
  # ==========================================================================
  
  cache_log <- reactiveVal("")
  cache_trigger <- reactiveVal(0)
  
  log_message <- function(msg) {
    current <- cache_log()
    timestamp <- format(Sys.time(), "%H:%M:%S")
    cache_log(paste0(current, "[", timestamp, "] ", msg, "\n"))
  }
  
  # Dynamically generate checkboxes from registry
  output$metricCheckboxes <- renderUI({
    cache_trigger()  # Depend on trigger for refresh
    metrics <- get_cacheable_metrics()
    
    if (length(metrics) == 0) {
      return(p("No cacheable metrics found"))
    }
    
    checkboxGroupInput("selectedMetrics", "Select Metrics:",
                       choices = metrics,
                       selected = metrics)
  })
  
  # Cache status table
  output$cacheStatusTable <- renderDT({
    cache_trigger()  # Depend on trigger for refresh
    
    status_df <- tryCatch({
      get_cache_status()
    }, error = function(e) {
      data.frame(
        metric_id = character(),
        status = character(),
        rows_5yr = integer(),
        rows_10yr = integer(),
        last_updated = character()
      )
    })
    
    if (nrow(status_df) == 0) {
      return(data.frame(Message = "No cache data available"))
    }
    
    # Determine descriptive data labels based on metric type
    registry <- tryCatch(get_metric_registry(), error = function(e) list())
    
    data1_labels <- sapply(status_df$metric_id, function(mid) {
      config <- registry[[mid]]
      is_yearly <- isTRUE(config$historical_type == "yearly_grouped")
      if (is_yearly) "Facilities" else "5yr Avg"
    })
    data2_labels <- sapply(status_df$metric_id, function(mid) {
      config <- registry[[mid]]
      is_yearly <- isTRUE(config$historical_type == "yearly_grouped")
      if (is_yearly) "District" else "10yr Avg"
    })
    
    # Format for display
    display_df <- data.frame(
      Metric = status_df$metric_id,
      Status = ifelse(status_df$status == "Complete", 
                      '<span style="color:green">&#10003; Complete</span>',
                      ifelse(status_df$status == "Partial",
                             '<span style="color:orange">&#9888; Partial</span>',
                             '<span style="color:red">&#10007; Missing</span>')),
      `5 year Data` = paste0(data1_labels, ": ", status_df$rows_5yr, " rows"),
      `10 year Data` = paste0(data2_labels, ": ", status_df$rows_10yr, " rows"),
      `Last Updated` = status_df$last_updated,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    display_df
  }, escape = FALSE, options = list(pageLength = 20, dom = 't'))
  
  # Cache info text
  output$cacheInfo <- renderText({
    cache_trigger()
    cache_file <- get_cache_file()
    
    if (!file.exists(cache_file)) {
      return("Cache file does not exist. Click 'Regenerate All' to create.")
    }
    
    info <- file.info(cache_file)
    paste0(
      "Cache file: ", cache_file, "\n",
      "File size: ", round(info$size / 1024, 1), " KB\n",
      "Last modified: ", info$mtime
    )
  })

  # Comprehensive runtime environment diagnostics
  output$runtimeInfo <- renderPrint({
    # Ensure .env is loaded (startup.sh writes MMCD_PLATFORM, ENABLE_NGINX there)
    env_path <- "/srv/shiny-server/.env"
    if (file.exists(env_path)) readRenviron(env_path)
    
    # Read platform from startup.sh-generated .env (most reliable)
    mmcd_platform <- Sys.getenv("MMCD_PLATFORM", "")
    
    # Fallback detection if MMCD_PLATFORM wasn't set
    if (nchar(mmcd_platform) == 0) {
      hostname <- tryCatch(Sys.info()[["nodename"]], error = function(e) "")
      is_ecs <- nchar(Sys.getenv("ECS_TASK_ARN", "")) > 0 ||
                nchar(Sys.getenv("ECS_CONTAINER_METADATA_URI", "")) > 0 ||
                nchar(Sys.getenv("ECS_CONTAINER_METADATA_URI_V4", "")) > 0 ||
                grepl("AWS_ECS", Sys.getenv("AWS_EXECUTION_ENV", ""))
      is_aws <- grepl("compute\\.internal|ec2|apprunner", hostname, ignore.case = TRUE) ||
                nchar(Sys.getenv("AWS_DEFAULT_REGION", "")) > 0 ||
                nchar(Sys.getenv("AWS_REGION", "")) > 0
      
      mmcd_platform <- if (is_ecs) {
        "ECS/Fargate"
      } else if (is_aws) {
        "AWS (unknown service)"
      } else {
        "Local/Docker"
      }
    }
    
    platform <- mmcd_platform
    
    # Detect concurrency mode — default is single-worker (matches startup.sh)
    enable_nginx <- Sys.getenv("ENABLE_NGINX", "false") == "true"
    shiny_workers <- as.integer(Sys.getenv("SHINY_WORKERS", "3"))
    
    # Check if redis is embedded and working
    redis_available <- tryCatch({
      system("redis-cli -h 127.0.0.1 ping > /dev/null 2>&1", intern = FALSE) == 0
    }, error = function(e) FALSE)
    
    # Test shared cache write/read (simpler, more reliable)
    cache_test <- "NOT TESTED"
    if (redis_available && exists("redis_set", mode = "function") && exists("redis_get", mode = "function")) {
      tryCatch({
        test_key <- "runtime_info_test"
        redis_set(test_key, list(status = "working"), ttl = 5)
        Sys.sleep(0.2)
        result <- redis_get(test_key)
        cache_test <- if (!is.null(result) && result$status == "working") "WORKING" else "FAILED"
      }, error = function(e) {
        # Gracefully handle any error during test
      })
    }
    
    sep <- paste0("=", paste(rep("=", 60), collapse = ""), "=\n")
    
    cat(sep)
    cat("DEPLOYMENT ENVIRONMENT\n")
    cat(sep)
    cat("Platform:                 ", platform, "\n")
    cat("Host:                     ", Sys.info()[["nodename"]], "\n")
    cat("PID:                      ", Sys.getpid(), "\n")
    cat("User:                     ", Sys.info()[["user"]], "\n")
    cat("Time:                     ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
    cat("\n")
    
    cat(sep)
    cat("CONCURRENCY MODE\n")
    cat(sep)
    if (enable_nginx) {
      cat("Status:                   MULTI-WORKER (nginx load-balanced)\n")
      cat("Workers:                  ", shiny_workers, " Shiny Server instances\n")
      cat("Load Balancer:            nginx on :3838\n")
      cat("Worker Ports:             ", paste(3839:(3839+shiny_workers-1), collapse = ", "), "\n")
    } else {
      cat("Status:                   SINGLE-WORKER (no nginx)\n")
      cat("Workers:                  1 Shiny Server\n")
      cat("Listen Port:              3838\n")
    }
    cat("\n")
    
    cat(sep)
    cat("SHARED CACHE STATUS\n")
    cat(sep)
    cat("Redis Host:               127.0.0.1\n")
    cat("Redis Port:               6379\n")
    cat("Redis Available:          ", if (redis_available) "YES" else "NO", "\n")
    cat("Shared Cache Test:        ", cache_test, "\n")
    cat("\n")
    
    cat(sep)
    cat("SUMMARY\n")
    cat(sep)
    
    status <- "✓ READY"
    if (!enable_nginx) {
      status <- "⚠ SINGLE-WORKER MODE (not optimal for production)"
    }
    if (!redis_available) {
      status <- "✗ REDIS NOT AVAILABLE (cache will fall back to files)"
    }
    
    cat(status, "\n")
  })

  # Walk the process tree to find which shiny-server instance owns this R process
  find_worker_info <- function() {
    map_file <- "/srv/shiny-server/.worker_map"
    if (!file.exists(map_file)) {
      return(list(id = "N/A", port = "N/A", debug = "no worker map (single-worker mode?)"))
    }

    map_lines <- readLines(map_file, warn = FALSE)
    map_lines <- trimws(map_lines[nzchar(trimws(map_lines))])
    if (length(map_lines) == 0) {
      return(list(id = "N/A", port = "N/A", debug = "worker map is empty"))
    }

    # Parse: PID INSTANCE_ID PORT
    map_df <- tryCatch({
      do.call(rbind, lapply(map_lines, function(line) {
        parts <- strsplit(line, "\\s+")[[1]]
        if (length(parts) >= 3) {
          data.frame(pid = as.integer(parts[1]), id = parts[2], port = parts[3],
                     stringsAsFactors = FALSE)
        }
      }))
    }, error = function(e) NULL)

    if (is.null(map_df) || nrow(map_df) == 0) {
      return(list(id = "N/A", port = "N/A", debug = paste("parse error:", map_lines[1])))
    }

    # Walk up the process tree from this R process looking for a PID in the map
    current_pid <- Sys.getpid()
    visited <- integer(0)
    for (step in 1:10) {
      if (current_pid %in% visited || current_pid <= 1) break
      visited <- c(visited, current_pid)

      idx <- which(map_df$pid == current_pid)
      if (length(idx) > 0) {
        return(list(id = map_df$id[idx[1]], port = map_df$port[idx[1]],
                    debug = paste("matched PID", current_pid, "at step", step)))
      }

      # Read parent PID from /proc
      status_file <- paste0("/proc/", current_pid, "/status")
      if (!file.exists(status_file)) break
      status_lines <- readLines(status_file, warn = FALSE)
      ppid_line <- grep("^PPid:", status_lines, value = TRUE)
      if (length(ppid_line) == 0) break
      current_pid <- as.integer(sub("PPid:\\s+", "", ppid_line[1]))
    }

    list(id = "not matched", port = "unknown",
         debug = paste("walked:", paste(visited, collapse = "->"),
                       "| map PIDs:", paste(map_df$pid, collapse = ",")))
  }

  output$workerInfo <- renderPrint({
    env_path <- "/srv/shiny-server/.env"
    if (file.exists(env_path)) readRenviron(env_path)

    worker <- find_worker_info()
    pid <- Sys.getpid()
    session_host <- tryCatch(session$clientData$url_hostname, error = function(e) "unknown")
    session_port <- tryCatch(session$clientData$url_port, error = function(e) "unknown")
    remote_addr <- tryCatch(session$request$REMOTE_ADDR, error = function(e) "unknown")
    x_forwarded_for <- tryCatch(session$request$HTTP_X_FORWARDED_FOR, error = function(e) "unknown")

    cat("Instance ID:            ", worker$id, "\n")
    cat("Instance Port:          ", worker$port, "\n")
    cat("Process PID:            ", pid, "\n")
    cat("Worker Detection:       ", worker$debug, "\n")
    cat("Session Host:           ", session_host, "\n")
    cat("Session Port:           ", session_port, "\n")
    cat("Remote Addr:            ", remote_addr, "\n")
    cat("X-Forwarded-For:        ", x_forwarded_for, "\n")
  })

  parse_nginx_access_log <- function(log_path, max_lines = 200) {
    if (!file.exists(log_path)) {
      return(data.frame(Message = "nginx access log not found", stringsAsFactors = FALSE))
    }

    # Check if we can actually read the file
    if (file.access(log_path, mode = 4) != 0) {
      perms <- tryCatch(
        paste(system(paste("ls -la", shQuote(log_path)), intern = TRUE), collapse = " "),
        error = function(e) "unknown"
      )
      return(data.frame(
        Message = paste0("Permission denied reading nginx log. File perms: ", perms),
        stringsAsFactors = FALSE
      ))
    }

    lines <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) {
      return(character(0))
    })
    if (length(lines) == 0) {
      fsize <- file.info(log_path)$size
      return(data.frame(
        Message = paste0("nginx access log has 0 parseable lines (file size: ", fsize, " bytes)"),
        stringsAsFactors = FALSE
      ))
    }

    lines <- tail(lines, max_lines)
    pattern <- "^([^ ]+) - ([^ ]+) \\[([^]]+)\\] \"([^\"]*)\" ([0-9]{3}) ([0-9]+) \"([^\"]*)\" \"([^\"]*)\" upstream=([^ ]*) xff=\"([^\"]*)\" req_time=([^ ]*) upstream_time=([^ ]*) upstream_status=([^ ]*)"
    matches <- regexec(pattern, lines)
    parts <- regmatches(lines, matches)

    rows <- lapply(parts, function(m) {
      if (length(m) == 0) return(NULL)
      request <- m[5]
      req_parts <- strsplit(request, " ")[[1]]
      method <- if (length(req_parts) >= 1) req_parts[1] else ""
      path <- if (length(req_parts) >= 2) req_parts[2] else request
      data.frame(
        Time = m[4],
        Method = method,
        Path = path,
        Status = m[6],
        Upstream = m[10],
        UpstreamStatus = m[14],
        ReqTime = m[12],
        UpstreamTime = m[13],
        XFF = m[11],
        RemoteAddr = m[2],
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, rows)
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(Message = "No parseable nginx log lines yet", stringsAsFactors = FALSE))
    }

    df
  }

  output$lbRecent <- renderDT({
    input$refreshLB  # reactivity trigger
    parse_nginx_access_log("/var/log/nginx/access.log", max_lines = 200)
  }, options = list(pageLength = 20, order = list(list(0, "desc"))))

  output$lbSessions <- renderDT({
    input$refreshLB  # reactivity trigger
    df <- parse_nginx_access_log("/var/log/nginx/access.log", max_lines = 500)
    if (!all(c("Path", "Upstream", "Time") %in% names(df))) {
      return(df)
    }

    session_id <- sub(".*/session/([^/]+).*", "\\1", df$Path)
    is_session <- grepl("/session/", df$Path)
    session_id <- session_id[is_session]   # filter BEFORE subsetting df
    df <- df[is_session, , drop = FALSE]

    if (nrow(df) == 0) {
      return(data.frame(Message = "No session requests found in recent logs"))
    }

    summary <- aggregate(
      x = list(Sessions = session_id),
      by = list(Upstream = df$Upstream),
      FUN = function(x) length(unique(x))
    )
    names(summary)[names(summary) == "Sessions"] <- "Unique Sessions"
    summary
  }, options = list(pageLength = 10))

  # ===========================================================================
  # Dynamic Routing monitoring (reads from Redis keys set by Lua router)
  # ===========================================================================

  # Helper: get a standalone Redis connection for monitoring queries
  get_monitor_redis <- function() {
    tryCatch({
      redux::hiredis(host = "127.0.0.1", port = 6379L)
    }, error = function(e) NULL)
  }

  # Worker Load (live active WebSocket connections per worker)
  output$drWorkerLoad <- renderDT({
    input$refreshDR
    r <- get_monitor_redis()
    if (is.null(r)) {
      return(data.frame(Message = "Redis unavailable"))
    }
    workers <- tryCatch(r$SMEMBERS("mmcd:workers"), error = function(e) list())
    if (length(workers) == 0) {
      return(data.frame(Message = "No workers registered in Redis (mmcd:workers is empty)"))
    }
    workers <- sort(unlist(workers))
    loads <- sapply(workers, function(w) {
      val <- tryCatch(r$GET(paste0("mmcd:load:", w)), error = function(e) "0")
      as.integer(val %||% "0")
    })
    data.frame(
      Worker = workers,
      `Active WebSockets` = loads,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 10, dom = "t"))

  # Configuration summary
  output$drConfig <- renderPrint({
    r <- get_monitor_redis()
    env_path <- "/srv/shiny-server/.env"
    if (file.exists(env_path)) readRenviron(env_path)

    workers_file <- "/etc/nginx/workers.list"
    workers_list <- if (file.exists(workers_file)) {
      readLines(workers_file, warn = FALSE)
    } else {
      "file not found"
    }

    route_ttl <- Sys.getenv("ROUTE_TTL_SECONDS", "600")

    cat("=== DYNAMIC ROUTING CONFIG ===\n\n")
    cat("Load Balancer:       OpenResty + Lua\n")
    cat("Routing Strategy:    Least-loaded worker assignment\n")
    cat("Route TTL:           ", route_ttl, "seconds\n")
    cat("Sticky Sessions:     Yes (for TTL duration)\n")
    cat("Fallback:            Consistent hash (if Redis down)\n\n")
    cat("Workers:\n")
    for (w in workers_list) cat("  ", w, "\n")
    cat("\nRedis Keys:\n")
    cat("  mmcd:workers       SET of worker addresses\n")
    cat("  mmcd:load:<worker> INT active WS connections\n")
    cat("  mmcd:route:<ip>    STRING worker addr (TTL=", route_ttl, "s)\n")
    cat("  mmcd:route_log     LIST last 200 routing decisions\n")

    if (!is.null(r)) {
      total_routes <- tryCatch({
        cursor <- "0"
        count <- 0
        repeat {
          result <- r$SCAN(cursor, match = "mmcd:route:*", count = 100L)
          cursor <- result[[1]]
          count <- count + length(result[[2]])
          if (cursor == "0") break
        }
        count
      }, error = function(e) "?")
      cat("\nActive Routes:      ", total_routes, "\n")

      total_workers <- tryCatch(r$SCARD("mmcd:workers"), error = function(e) "?")
      cat("Registered Workers: ", total_workers, "\n")

      log_len <- tryCatch(r$LLEN("mmcd:route_log"), error = function(e) "?")
      cat("Decision Log Size:  ", log_len, "\n")
    }
  })

  # Active Route Mappings (IP → Worker with remaining TTL)
  output$drRouteMappings <- renderDT({
    input$refreshDR
    r <- get_monitor_redis()
    if (is.null(r)) {
      return(data.frame(Message = "Redis unavailable"))
    }

    # SCAN for all mmcd:route:* keys
    routes <- list()
    cursor <- "0"
    repeat {
      result <- tryCatch(r$SCAN(cursor, match = "mmcd:route:*", count = 100L),
                         error = function(e) list("0", list()))
      cursor <- result[[1]]
      routes <- c(routes, result[[2]])
      if (cursor == "0") break
    }

    if (length(routes) == 0) {
      return(data.frame(Message = "No active route mappings"))
    }

    rows <- lapply(routes, function(key) {
      key_str <- as.character(key)
      ip <- sub("^mmcd:route:", "", key_str)
      worker <- tryCatch(as.character(r$GET(key_str)), error = function(e) "?")
      ttl <- tryCatch(as.integer(r$TTL(key_str)), error = function(e) -1L)
      data.frame(
        `Client IP` = ip,
        `Assigned Worker` = worker,
        `TTL Remaining (s)` = ttl,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }, options = list(pageLength = 20, order = list(list(2, "asc"))))

  # Routing Decision Log (from mmcd:route_log Redis list)
  output$drRouteLog <- renderDT({
    input$refreshDR
    r <- get_monitor_redis()
    if (is.null(r)) {
      return(data.frame(Message = "Redis unavailable"))
    }

    log_raw <- tryCatch(r$LRANGE("mmcd:route_log", 0L, 49L), error = function(e) list())
    if (length(log_raw) == 0) {
      return(data.frame(Message = "No routing decisions logged yet"))
    }

    rows <- lapply(log_raw, function(entry) {
      parsed <- tryCatch(jsonlite::fromJSON(as.character(entry)), error = function(e) NULL)
      if (is.null(parsed)) return(NULL)
      loads_str <- if (!is.null(parsed$all_loads)) {
        paste(names(parsed$all_loads), "=", unlist(parsed$all_loads), collapse = ", ")
      } else {
        ""
      }
      data.frame(
        Time = if (!is.null(parsed$time_fmt)) parsed$time_fmt else as.character(parsed$time),
        `Client IP` = parsed$client_ip %||% "?",
        `Assigned To` = parsed$target %||% "?",
        `Worker Load` = as.integer(parsed$load %||% 0),
        Reason = parsed$reason %||% "?",
        `All Loads` = loads_str,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    rows <- rows[!sapply(rows, is.null)]
    if (length(rows) == 0) {
      return(data.frame(Message = "No parseable routing decisions"))
    }
    do.call(rbind, rows)
  }, options = list(pageLength = 20, dom = "frtip"))

  # nginx access log for the Dynamic Routing tab (reuses parse_nginx_access_log)
  output$drAccessLog <- renderDT({
    input$refreshDR
    parse_nginx_access_log("/var/log/nginx/access.log", max_lines = 100)
  }, options = list(pageLength = 20, order = list(list(0, "desc"))))

  # Redis status / cache backend info
  output$redisStatus <- renderPrint({
    if (exists("redis_cache_status", mode = "function")) {
      redis_cache_status()
    } else {
      "redis_cache_status() not available"
    }
  })

  output$cacheBackendInfo <- renderPrint({
    list(
      cache_backend = if (exists("REDIS_CONFIG")) REDIS_CONFIG$backend else "unknown",
      redis_active = if (exists("redis_is_active", mode = "function")) redis_is_active() else FALSE,
      redis_host = if (exists("REDIS_CONFIG")) REDIS_CONFIG$host else "unknown",
      redis_port = if (exists("REDIS_CONFIG")) REDIS_CONFIG$port else NA,
      redis_db = if (exists("REDIS_CONFIG")) REDIS_CONFIG$db else NA,
      redis_prefix = if (exists("REDIS_CONFIG")) REDIS_CONFIG$prefix else "unknown"
    )
  })
  
  # Cache log output
  output$cacheLog <- renderText({
    cache_log()
  })
  
  # Regenerate selected metrics
  observeEvent(input$regenerateSelected, {
    metrics <- input$selectedMetrics
    if (is.null(metrics) || length(metrics) == 0) {
      log_message("No metrics selected")
      return()
    }
    
    log_message(paste("Starting regeneration for:", paste(metrics, collapse = ", ")))
    
    withProgress(message = "Regenerating cache...", value = 0, {
      for (i in seq_along(metrics)) {
        metric <- metrics[i]
        incProgress(1/length(metrics), detail = metric)
        
        tryCatch({
          regenerate_cache(metrics = metric)
          log_message(paste("  ✓", metric, "complete"))
        }, error = function(e) {
          log_message(paste("  ✗", metric, "failed:", e$message))
        })
      }
    })
    
    log_message("Regeneration complete")
    cache_trigger(cache_trigger() + 1)  # Trigger refresh
  })
  
  # Regenerate all metrics
  observeEvent(input$regenerateAll, {
    log_message("Starting full cache regeneration...")
    
    withProgress(message = "Regenerating all cache...", value = 0, {
      tryCatch({
        regenerate_cache()
        log_message("✓ Full regeneration complete")
      }, error = function(e) {
        log_message(paste("✗ Error:", e$message))
      })
    })
    
    cache_trigger(cache_trigger() + 1)
  })
  
  # Clear selected metrics
  observeEvent(input$clearSelected, {
    metrics <- input$selectedMetrics
    if (is.null(metrics) || length(metrics) == 0) {
      log_message("No metrics selected to clear")
      return()
    }
    
    log_message(paste("Clearing cache for:", paste(metrics, collapse = ", ")))
    clear_cache(metrics)
    log_message("Cache cleared for selected metrics")
    cache_trigger(cache_trigger() + 1)
  })
  
  # Clear all cache
  observeEvent(input$clearAll, {
    log_message("Clearing entire cache...")
    clear_cache()
    log_message("Cache cleared")
    cache_trigger(cache_trigger() + 1)
  })
  
  # Refresh status
  observeEvent(input$refreshStatus, {
    log_message("Refreshing cache status...")
    cache_trigger(cache_trigger() + 1)
  })
  
  # ==========================================================================
  # METRIC REGISTRY TABLE
  # ==========================================================================
  
  output$registryTable <- renderDT({
    # Load registry
    ensure_registry_loaded()
    
    if (!exists("get_metric_registry", mode = "function")) {
      return(data.frame(Message = "Could not load metric registry"))
    }
    
    registry <- get_metric_registry()
    
    # Build table from registry
    df <- do.call(rbind, lapply(names(registry), function(id) {
      config <- registry[[id]]
      data.frame(
        ID = id,
        Name = config$display_name %||% id,
        `App Folder` = config$app_folder %||% "",
        `Has Acres` = ifelse(isTRUE(config$has_acres), "Yes", "No"),
        `Historical` = ifelse(isTRUE(config$historical_enabled), "Yes", "No"),
        `Active Calc` = ifelse(isTRUE(config$use_active_calculation), "Yes", "No"),
        Cacheable = ifelse(id %in% get_cacheable_metrics(), 
                          '<span style="color:green">Yes</span>', 
                          '<span style="color:gray">No</span>'),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }))
    
    df
  }, escape = FALSE, options = list(pageLength = 20))
  
  # ==========================================================================
  # LOOKUP CACHE MANAGEMENT
  # ==========================================================================
  
  lookup_cache_log <- reactiveVal("")
  lookup_trigger <- reactiveVal(0)
  
  lookup_log_message <- function(msg) {
    current <- lookup_cache_log()
    timestamp <- format(Sys.time(), "%H:%M:%S")
    lookup_cache_log(paste0(current, "[", timestamp, "] ", msg, "\n"))
  }
  
  # Lookup cache status table
  output$lookupCacheStatus <- renderDT({
    lookup_trigger()  # Depend on trigger
    
    tryCatch({
      status_df <- get_lookup_cache_status()
      
      # Format status with colors
      status_df$status <- ifelse(
        status_df$status == "Cached",
        '<span style="color:green">✓ Cached</span>',
        '<span style="color:gray">○ Empty</span>'
      )
      
      status_df
    }, error = function(e) {
      data.frame(
        lookup_type = "Error",
        status = e$message,
        row_count = NA
      )
    })
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  # Lookup cache log
  output$lookupCacheLog <- renderText({
    lookup_cache_log()
  })
  
  # Refresh lookup caches
  observeEvent(input$refreshLookupCaches, {
    lookup_log_message("Refreshing all lookup caches...")
    
    withProgress(message = "Refreshing lookups...", value = 0.5, {
      tryCatch({
        refresh_lookup_caches()
        lookup_log_message("✓ All lookups refreshed from database")
      }, error = function(e) {
        lookup_log_message(paste("✗ Error:", e$message))
      })
    })
    
    lookup_trigger(lookup_trigger() + 1)
  })
  
  # Clear lookup caches
  observeEvent(input$clearLookupCaches, {
    lookup_log_message("Clearing all lookup caches...")
    
    tryCatch({
      clear_lookup_cache_types()
      lookup_log_message("✓ Lookup cache cleared - will refetch on next access")
    }, error = function(e) {
      lookup_log_message(paste("✗ Error:", e$message))
    })
    
    lookup_trigger(lookup_trigger() + 1)
  })
  
  # ==========================================================================
  # THEME & COLOR HANDLERS (existing code)
  # ==========================================================================
  
  # Reactive value for current theme
  current_theme <- reactive({
    input$color_theme
  })
  
  # Update theme description when theme changes
  observeEvent(input$color_theme, {
    if (exists("get_theme_description", mode = "function")) {
      desc <- get_theme_description(input$color_theme)
      session$sendCustomMessage("updateThemeDesc", desc)
    }
    
    # Set global option so color functions use the selected theme
    options(mmcd.color.theme = input$color_theme)
  })
  
  # Facility Information (with colors)
  output$facilityInfo <- renderDT({
    facilities <- get_facility_lookup()
    if (nrow(facilities) == 0) return(NULL)
    
    colors <- get_facility_base_colors(theme = current_theme())
    
    df <- data.frame(
      Facility = facilities$short_name,
      City = facilities$full_name,
      Color = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors[facilities$short_name]
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # FOS Information (with colors)
  output$fosInfo <- renderDT({
    foremen <- get_foremen_lookup()
    if (nrow(foremen) == 0) return(NULL)
    
    colors <- get_themed_foreman_colors(theme = current_theme())
    
    df <- data.frame(
      "Emp Num" = foremen$emp_num,
      "Shortname" = foremen$shortname,
      Facility = foremen$facility,
      Color = unname(colors[foremen$shortname]),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors[foremen$shortname]
      ),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Status Colors (Hex)
  output$statusColors <- renderDT({
    colors <- get_status_colors(theme = current_theme())
    descriptions <- get_status_descriptions()
    
    df <- data.frame(
      Status = names(colors),
      "Hex Color" = as.character(colors),
      Description = descriptions[names(colors)],
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Shiny Colors (Named)
  output$shinyColors <- renderDT({
    colors <- get_shiny_colors()
    
    df <- data.frame(
      Status = names(colors),
      "Shiny Color" = as.character(colors),
      "Use Case" = "ValueBox elements",
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Status Color Mapping
  output$statusColorMap <- renderDT({
    color_map <- get_status_color_map(theme = current_theme())
    
    df <- data.frame(
      "Status Name" = names(color_map),
      "Hex Color" = as.character(unlist(color_map)),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        unlist(color_map)
      ),
      "Use Case" = "Maps, charts, tables",
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Treatment Plan Colors (by Code)
  output$treatmentColorsCodes <- renderDT({
    colors <- get_treatment_plan_colors(use_names = FALSE)
    
    if (length(colors) == 0) {
      df <- data.frame(
        Message = "No treatment plan colors available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Plan Code" = names(colors),
      "Hex Color" = as.character(colors),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Treatment Plan Colors (by Name)
  output$treatmentColorsNames <- renderDT({
    colors <- get_treatment_plan_colors(use_names = TRUE)
    
    if (length(colors) == 0) {
      df <- data.frame(
        Message = "No treatment plan colors available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Plan Name" = names(colors),
      "Hex Color" = as.character(colors),
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 20px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Treatment Plan Types
  output$treatmentPlanTypes <- renderDT({
    plan_types <- get_treatment_plan_types()
    
    if (nrow(plan_types) == 0) {
      df <- data.frame(
        Message = "No treatment plan types available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Plan Code" = plan_types$plan_code,
      "Plan Name" = plan_types$plan_name,
      Description = plan_types$description,
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10))
  
  # Mosquito Species Colors
  output$mosquitoColors <- renderDT({
    colors <- get_mosquito_species_colors()
    
    if (length(colors) == 0) {
      df <- data.frame(
        Message = "No mosquito species colors available",
        stringsAsFactors = FALSE
      )
      return(df)
    }
    
    df <- data.frame(
      "Species Code" = names(colors),
      "Hex Color" = as.character(unlist(colors)),
      Preview = sprintf(
        '<div style="background-color: %s; width: 80px; height: 15px; border: 1px solid #ddd;"></div>',
        unlist(colors)
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 20, scrollY = "400px", scrollCollapse = TRUE))
  
  # Theme Preview Outputs
  output$themeInfo <- renderUI({
    theme <- current_theme()
    if (exists("get_theme_description", mode = "function")) {
      desc <- get_theme_description(theme)
      tagList(
        tags$h4(paste("Current Theme:", theme)),
        tags$p(desc),
        tags$p(tags$em("This theme will affect facility colors, status colors, and dynamically generated color palettes."))
      )
    } else {
      tags$p("Theme information not available. Make sure color_themes.R is loaded.")
    }
  })
  
  output$themePrimaryColors <- renderDT({
    if (!exists("get_theme_palette", mode = "function")) {
      return(data.frame(Message = "Theme palette function not available"))
    }
    
    palette <- get_theme_palette(current_theme())
    colors <- palette$primary
    
    df <- data.frame(
      "Color #" = seq_along(colors),
      "Hex Value" = colors,
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 25px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$themeSequentialColors <- renderDT({
    if (!exists("get_theme_palette", mode = "function")) {
      return(data.frame(Message = "Theme palette function not available"))
    }
    
    palette <- get_theme_palette(current_theme())
    colors <- palette$sequential
    
    df <- data.frame(
      "Color #" = seq_along(colors),
      "Hex Value" = colors,
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 25px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10, dom = 't'))
  
  output$themeDivergingColors <- renderDT({
    if (!exists("get_theme_palette", mode = "function")) {
      return(data.frame(Message = "Theme palette function not available"))
    }
    
    palette <- get_theme_palette(current_theme())
    colors <- palette$diverging
    
    df <- data.frame(
      "Color #" = seq_along(colors),
      "Hex Value" = colors,
      Preview = sprintf(
        '<div style="background-color: %s; width: 100px; height: 25px; border: 1px solid #ddd;"></div>',
        colors
      ),
      stringsAsFactors = FALSE
    )
    df
  }, escape = FALSE, options = list(pageLength = 10, dom = 't', scrollX = TRUE))
  

}

# Add custom JavaScript to update theme description
shinyApp(ui = ui, server = server)
