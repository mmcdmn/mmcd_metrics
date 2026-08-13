# =============================================================================
# SECRET REFRESH PAGE — nightly infection-rate / abundance updater
# =============================================================================
# A hidden, key-gated Shiny page that recomputes the current season's MLE + MIR
# and writes them into the six pre-calculated tables, then refreshes the
# abundance materialized view. A daily cron hits this page to keep the DB fresh
# for the app AND all external consumers (public_map, etc.).
#
# This is a STANDALONE app — it does not touch data_functions.R. It reuses the
# shared DB/Redis helpers and the validated authoritative methodology.
#
# ── WRITE ACCESS ──────────────────────────────────────────────────────────────
# The normal app connects to the READ REPLICA (mmcd_read @ rds-readonly), which
# is physically read-only. Writing requires a connection to the PRIMARY with a
# write role, supplied via DB_WRITE_* env vars. Computing (reading pools) uses
# the usual read connection; only the final writes go to the primary.
#
#   DB_WRITE_HOST / DB_WRITE_PORT / DB_WRITE_USER / DB_WRITE_PASSWORD / DB_WRITE_NAME
#   REFRESH_SECRET   required shared secret; the page refuses to run without it
#
# ── HOW IT'S TRIGGERED ────────────────────────────────────────────────────────
# Visit  /refresh_views/?key=<REFRESH_SECRET>&run=1   to run the write.
# Add    &dry=1   to compute + report WITHOUT writing (safe preview).
# A plain curl only fetches the UI shell (Shiny runs over a websocket), so the
# daily cron must load the URL with a headless browser, e.g.:
#   chromium --headless --disable-gpu --dump-dom \
#     "https://metrics.mmcd.org/refresh_views/?key=$SECRET&run=1" >/dev/null
#
# ── METHODOLOGY (authoritative — reproduces Jordan's static tables exactly) ────
#   Pool set: dbvirus_pool_test (target='WNV', result NOT NULL) -> dbvirus_pool
#             -> dbadult_insp -> viarea via loc_vectorindexareas_sections_a on
#             LEFT(sitecode,7); left(loc_code,1)='X' OR sitecode NULL => 'Out';
#             drop viarea NULL. Week via calc_week_num(inspdate). NOT survtype-limited.
#   MLE: PooledInfRate::pooledBin(x, m, pt.method='firth', scale=1); undefined
#        boundaries (all-positive) -> SQL NULL.
#   MIR: positive / mosquitoes * 1000.
# Each table is updated transactionally: DELETE WHERE yrwk LIKE '<year>%' + INSERT
# (surrogate id continues from the current max). History is never touched.
# =============================================================================

library(shiny)
library(DBI)
library(RPostgres)

source("../../shared/db_helpers.R")   # get_db_connection() [read replica], safe_disconnect(), redis

SPP_CODES <- c("33", "34", "36", "371", "372")   # matches the static _spp tables
ABUND_MV  <- "dbadult_mon_nt_co2_forvectorabundance"

# Insert column order per table (surrogate id first; the _spp MIR table's serial
# `id` is auto and therefore omitted).
COL_ORDER <- list(
  dbvirus_mle_yrwk_area     = c("mle_id", "yrwk", "viarea", "p", "lower", "upper"),
  dbvirus_mle_yrwk_area_spp = c("mle_id", "yrwk", "viarea", "spp_code", "p", "lower", "upper"),
  dbvirus_mle_yrwk          = c("mle_id", "yrwk", "p", "lower", "upper"),
  dbvirus_mir_yrwk_area     = c("mir_id", "year", "yrwk", "viarea", "positive", "total", "mosquitoes", "mir"),
  dbvirus_mir_yrwk_area_spp = c("mir_id", "year", "yrwk", "viarea", "spp_code", "positive", "total", "mosquitoes", "mir"),
  dbvirus_mir_yrwk          = c("mir_id", "year", "yrwk", "positive", "total", "mosquitoes", "mir")
)

# =============================================================================
# WRITE CONNECTION — to the PRIMARY (not the read replica)
# =============================================================================
get_write_connection <- function() {
  host <- Sys.getenv("DB_WRITE_HOST", "")
  if (!nzchar(host)) return(NULL)
  tryCatch(
    dbConnect(RPostgres::Postgres(),
      host     = host,
      port     = as.integer(Sys.getenv("DB_WRITE_PORT", "5432")),
      user     = Sys.getenv("DB_WRITE_USER"),
      password = Sys.getenv("DB_WRITE_PASSWORD"),
      dbname   = Sys.getenv("DB_WRITE_NAME", Sys.getenv("DB_NAME", "mmcd_data"))),
    error = function(e) NULL)
}

# =============================================================================
# COMPUTE (fresh, no cache)
# =============================================================================
.fmt_mle <- function(x) ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = 9))
.fmt_mir <- function(x) ifelse(is.na(x), "0",           formatC(x, format = "f", digits = 9))

.pooled_mle <- function(x, m) {
  v <- !is.na(m) & m > 0 & !is.na(x)
  if (sum(v) == 0) return(c(NA, NA, NA))
  r <- tryCatch(PooledInfRate::pooledBin(x = x[v], m = m[v], pt.method = "firth", scale = 1),
                error = function(e) NULL)
  if (is.null(r)) c(NA, NA, NA) else c(as.numeric(r$P), as.numeric(r$Lower), as.numeric(r$Upper))
}

.pull_pools_year <- function(con, year) {
  viarea <- "CASE WHEN left(i.loc_code,1)='X' OR i.sitecode IS NULL THEN 'Out' ELSE v.viareaa END"
  q <- sprintf(
    "SELECT calc_week_num(i.inspdate)::int AS yrwk, %s AS viarea,
            p.spp_code, p.count AS pool_size,
            CASE WHEN t.result='Pos' THEN 1 ELSE 0 END AS x
     FROM dbvirus_pool_test t
     JOIN dbvirus_pool p      ON p.poolnum = t.poolnum
     LEFT JOIN dbadult_insp i ON i.sampnum_yr = p.sampnum_yr
     LEFT JOIN loc_vectorindexareas_sections_a v ON v.sectcode = LEFT(i.sitecode,7)
     WHERE t.target='WNV' AND t.result IS NOT NULL AND (%s) IS NOT NULL
       AND calc_week_num(i.inspdate) >= %d AND calc_week_num(i.inspdate) < %d",
    viarea, viarea, as.integer(year) * 100L, (as.integer(year) + 1L) * 100L)
  dbGetQuery(con, q)
}

.agg_mle <- function(pools, keys) {
  g <- do.call(paste, c(pools[keys], sep = "\r"))
  do.call(rbind, lapply(split(seq_len(nrow(pools)), g), function(idx) {
    d <- pools[idx, ]; r <- .pooled_mle(d$x, d$pool_size)
    out <- d[1, keys, drop = FALSE]
    out$p <- .fmt_mle(r[1]); out$lower <- .fmt_mle(r[2]); out$upper <- .fmt_mle(r[3]); out
  }))
}

.agg_mir <- function(pools, keys) {
  g <- do.call(paste, c(pools[keys], sep = "\r"))
  do.call(rbind, lapply(split(seq_len(nrow(pools)), g), function(idx) {
    d <- pools[idx, ]; pos <- sum(d$x); tot <- nrow(d); mosq <- sum(d$pool_size)
    out <- d[1, keys, drop = FALSE]
    out$positive   <- as.character(as.integer(pos))
    out$total      <- as.character(as.integer(tot))
    out$mosquitoes <- as.character(as.integer(mosq))
    out$mir        <- .fmt_mir(if (mosq > 0) pos / mosq * 1000 else 0)
    out
  }))
}

compute_all <- function(read_con, year) {
  pools <- .pull_pools_year(read_con, year)
  if (is.null(pools) || nrow(pools) == 0) return(NULL)
  pools$yrwk_c <- as.character(pools$yrwk)
  spp <- pools[pools$spp_code %in% SPP_CODES, ]

  res <- list(
    dbvirus_mle_yrwk_area     = .agg_mle(pools, c("yrwk_c", "viarea")),
    dbvirus_mle_yrwk_area_spp = .agg_mle(spp,   c("yrwk_c", "viarea", "spp_code")),
    dbvirus_mle_yrwk          = .agg_mle(pools, c("yrwk_c")),
    dbvirus_mir_yrwk_area     = .agg_mir(pools, c("yrwk_c", "viarea")),
    dbvirus_mir_yrwk_area_spp = .agg_mir(spp,   c("yrwk_c", "viarea", "spp_code")),
    dbvirus_mir_yrwk          = .agg_mir(pools, c("yrwk_c"))
  )
  for (nm in names(res)) {
    df <- res[[nm]]
    names(df)[names(df) == "yrwk_c"] <- "yrwk"
    if (grepl("mir", nm)) df$year <- substr(df$yrwk, 1, 4)
    res[[nm]] <- df
  }
  res
}

# =============================================================================
# WRITE — transactional delete-current-year + insert
# =============================================================================
write_table <- function(wcon, name, df, year, log) {
  idcol <- if (grepl("mle", name)) "mle_id" else "mir_id"
  dbBegin(wcon)
  tryCatch({
    del   <- dbExecute(wcon, sprintf("DELETE FROM %s WHERE yrwk LIKE '%d%%'", name, as.integer(year)))
    maxid <- dbGetQuery(wcon, sprintf("SELECT COALESCE(MAX(%s::bigint),0) AS m FROM %s", idcol, name))$m
    df[[idcol]] <- as.character(seq.int(maxid + 1L, length.out = nrow(df)))
    df_out <- df[, COL_ORDER[[name]], drop = FALSE]
    for (cc in names(df_out)) if (!is.na(cc)) df_out[[cc]] <- as.character(df_out[[cc]])
    dbAppendTable(wcon, name, df_out)
    dbCommit(wcon)
    log(sprintf("  %-28s deleted %d, inserted %d", name, del, nrow(df_out)))
    TRUE
  }, error = function(e) {
    dbRollback(wcon)
    log(sprintf("  ERROR %s: %s (rolled back)", name, conditionMessage(e)))
    FALSE
  })
}

# Orchestrator. dry_run = compute + report only. Returns a character log vector.
run_refresh <- function(year = as.integer(format(Sys.Date(), "%Y")), dry_run = FALSE) {
  lines <- character(0)
  log <- function(s) { lines[[length(lines) + 1]] <<- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", s) }

  if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
    log("ABORT: PooledInfRate not installed."); return(lines)
  }
  read_con <- get_db_connection()
  if (is.null(read_con)) { log("ABORT: no read DB connection."); return(lines) }
  on.exit(safe_disconnect(read_con), add = TRUE)

  log(sprintf("Computing infection rates for %d (dry_run=%s) ...", year, dry_run))
  res <- tryCatch(compute_all(read_con, year), error = function(e) { log(paste("Compute failed:", conditionMessage(e))); NULL })
  if (is.null(res)) { log("No WNV pool data for this year — nothing to do."); return(lines) }
  for (nm in names(res)) log(sprintf("  %-28s %4d rows", nm, nrow(res[[nm]])))

  if (dry_run) { log("DRY RUN — nothing written."); return(lines) }

  wcon <- get_write_connection()
  if (is.null(wcon)) {
    log("ABORT: write DB not configured. Set DB_WRITE_HOST/USER/PASSWORD (primary, write role).")
    return(lines)
  }
  on.exit(tryCatch(dbDisconnect(wcon), error = function(e) NULL), add = TRUE)

  ok <- TRUE
  for (nm in names(res)) ok <- write_table(wcon, nm, res[[nm]], year, log) && ok

  log(sprintf("Refreshing materialized view %s ...", ABUND_MV))
  tryCatch({ dbExecute(wcon, sprintf("REFRESH MATERIALIZED VIEW %s", ABUND_MV)); log("  abundance refreshed") },
           error = function(e) { log(paste("  abundance refresh FAILED:", conditionMessage(e))); ok <<- FALSE })

  log(if (ok) "SUCCESS — refresh complete." else "COMPLETED WITH ERRORS — see above.")
  lines
}

# =============================================================================
# UI / SERVER  (key-gated)
# =============================================================================
ui <- fluidPage(
  tags$head(tags$title("MMCD — View Refresh")),
  tags$h3("MMCD Infection-Rate / Abundance Refresh"),
  uiOutput("gate"),
  tags$hr(),
  verbatimTextOutput("log", placeholder = TRUE)
)

server <- function(input, output, session) {
  status <- reactiveVal("Idle.")
  logtxt <- reactiveVal("(no run yet)")

  secret <- Sys.getenv("REFRESH_SECRET", "")
  qs <- reactive(parseQueryString(session$clientData$url_search))
  key_ok <- reactive(nzchar(secret) && identical(as.character(qs()$key %||% ""), secret))

  do_run <- function(dry) {
    if (!key_ok()) { status("DENIED — missing/invalid key."); return(invisible()) }
    status(if (dry) "Running DRY refresh..." else "Running refresh (writing)...")
    lines <- tryCatch(run_refresh(dry_run = dry),
                      error = function(e) paste("FATAL:", conditionMessage(e)))
    logtxt(paste(lines, collapse = "\n"))
    status(if (dry) "Dry run finished." else "Run finished.")
  }

  # Auto-run for the cron/headless-browser hit: ?key=...&run=1  (&dry=1 optional).
  # Guarded so it fires at most once per session (no accidental double-write).
  ran <- reactiveVal(FALSE)
  observe({
    q <- qs()
    if (!ran() && identical(as.character(q$run %||% ""), "1")) {
      ran(TRUE)
      isolate(do_run(identical(as.character(q$dry %||% ""), "1")))
    }
  })

  output$gate <- renderUI({
    if (!nzchar(secret)) {
      return(tags$p(style = "color:#b00", "REFRESH_SECRET is not set on the server — the page is disabled."))
    }
    if (!key_ok()) {
      return(tagList(
        tags$p("Provide the shared key in the URL: ", tags$code("?key=YOURKEY")),
        tags$p(tags$b("Status: "), textOutput("status_inline", inline = TRUE))
      ))
    }
    tagList(
      tags$p("Authorized. ",
             tags$b("Write target: "),
             tags$code(if (nzchar(Sys.getenv("DB_WRITE_HOST", ""))) Sys.getenv("DB_WRITE_HOST") else "NOT CONFIGURED (DB_WRITE_*)")),
      actionButton("dry",  "Dry run (no write)", class = "btn-info"),
      actionButton("run",  "Run refresh (write)", class = "btn-danger"),
      tags$p(tags$b("Status: "), textOutput("status_inline", inline = TRUE))
    )
  })

  output$status_inline <- renderText(status())
  output$log <- renderText(logtxt())

  observeEvent(input$dry, do_run(TRUE))
  observeEvent(input$run, do_run(FALSE))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

shinyApp(ui, server)
