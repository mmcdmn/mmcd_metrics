# =============================================================================
# ACCESSIBILITY HELPERS (ADA / WCAG 2.1 AA)
# =============================================================================
#
#
# See shared/SHARED_RESOURCES.md
# =============================================================================
#' Default border color for interactive control boundaries (6.42:1 on white).
A11Y_BORDER_COLOR <- "#55606B"

#' Focus ring color (6.70:1 on white).
A11Y_FOCUS_COLOR <- "#1d4ed8"


# -----------------------------------------------------------------------------
# Shared asset loading
# -----------------------------------------------------------------------------

# Locate a file in shared/assets from wherever the app runs.
.a11y_find_asset <- function(file) {
  candidates <- file.path(
    c("../../shared/assets",
      "../../../shared/assets",
      "shared/assets",
      "/srv/shiny-server/shared/assets"),
    file
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NULL)
  normalizePath(hit[1], winslash = "/")
}

#' Read a shared asset once. A missing file must never take an app down, so
#' this warns and returns "" instead of stopping; tests/apps/
#' test-accessibility.R fails CI if either asset cannot be found.
.a11y_read_asset <- function(file) {
  path <- .a11y_find_asset(file)
  if (is.null(path)) {
    warning("accessibility_helpers.R: shared/assets/", file,
            " not found - accessibility styling/behavior disabled for this app")
    return("")
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# Read at source time, once per R process.
.A11Y_CSS <- .a11y_read_asset("accessibility.css")
.A11Y_JS  <- .a11y_read_asset("accessibility.js")


# -----------------------------------------------------------------------------
# CSS / JS tags
# -----------------------------------------------------------------------------

#' Get Accessibility CSS
#' @param border_color Border color for control boundaries
#' @param focus_color Focus ring color
#' @return A <style> tag
#' @export
get_accessibility_css <- function(border_color = A11Y_BORDER_COLOR,
                                  focus_color  = A11Y_FOCUS_COLOR) {
  overrides <- character(0)
  if (!identical(border_color, A11Y_BORDER_COLOR)) {
    overrides <- c(overrides, paste0("--a11y-border: ", border_color, ";"))
  }
  if (!identical(focus_color, A11Y_FOCUS_COLOR)) {
    overrides <- c(overrides, paste0("--a11y-focus: ", focus_color, ";"))
  }
  css <- .A11Y_CSS
  if (length(overrides) > 0) {
    css <- paste0(css, "\n:root { ", paste(overrides, collapse = " "), " }\n")
  }
  shiny::tags$style(shiny::HTML(css))
}

#' Get Accessibility JS
#' @return A <script> tag
#' @export
get_accessibility_js <- function() {
  shiny::tags$script(shiny::HTML(gsub("</", "<\\/", .A11Y_JS, fixed = TRUE)))
}

#' Bundle the accessibility CSS and JS for the document head.
#'
#' @param ... Passed through to get_accessibility_css()
#' @return A tags$head() containing the shared accessibility assets
#' @export
accessibility_head <- function(...) {
  shiny::tags$head(
    get_accessibility_css(...),
    get_accessibility_js()
  )
}


# -----------------------------------------------------------------------------
# Components
# -----------------------------------------------------------------------------

#' Skips link 
#' @param target_id Fragment id to jump to
#' @param label Visible text once focused
#' @return A Shiny tag
#' @export
skip_link <- function(target_id = "main-content", label = "Skip to main content") {
  shiny::tags$a(
    href  = paste0("#", target_id),
    class = "a11y-skip-link",
    label
  )
}


#' Wrap content in a <main> 
#'
#' @param ... Content to wrap
#' @param id Element id (must match the skip link target)
#' @param label Accessible name for the  main
#' @return A Shiny tag
#' @export
main_landmark <- function(..., id = "main-content", label = "Main content") {
  shiny::tags$main(
    id           = id,
    class        = "a11y-main-landmark",
    tabindex     = "-1",
    `aria-label` = label,
    ...
  )
}


#' Accessible drop in for shiny::mainPanel()
#' @param ... Content (as for mainPanel)
#' @param width Bootstrap column width (as for mainPanel)
#' @param id Landmark id
#' @param label Accessible name for the landmark
#' @return A Shiny tag
#' @export
accessible_main_panel <- function(..., width = 8, id = "main-content",
                                  label = "Main content") {
  htmltools::tagAppendAttributes(
    shiny::mainPanel(..., width = width),
    id           = id,
    tabindex     = "-1",
    class        = "a11y-main-landmark",
    `aria-label` = label
  )
}


#' Accessible show/hide section 
#'
#' @param id Button id; the region gets id "<id>-region"
#' @param label Visible button text
#' @param ... Content of the collapsible region
#' @param icon Font Awesome icon name shown before the label, or NULL
#' @param open Start expanded?
#' @return A tagList (button + region)
#' @export
a11y_disclosure <- function(id, label, ..., icon = "circle-question", open = FALSE) {
  region_id <- paste0(id, "-region")

  icon_tag <- NULL
  if (!is.null(icon)) {
    # icon() labels itself "<name> icon"; next to visible text it is decorative.
    icon_tag <- shiny::icon(icon)
    icon_tag$attribs[names(icon_tag$attribs) %in% c("aria-label", "role")] <- NULL
    icon_tag <- htmltools::tagAppendAttributes(icon_tag, `aria-hidden` = "true")
  }

  shiny::tagList(
    shiny::tags$button(
      type                   = "button",
      id                     = id,
      class                  = "a11y-disclosure",
      `data-a11y-disclosure` = "",
      `aria-expanded`        = if (isTRUE(open)) "true" else "false",
      `aria-controls`        = region_id,
      icon_tag,
      if (!is.null(icon_tag)) " ",
      label
    ),
    shiny::tags$div(
      id     = region_id,
      hidden = if (isTRUE(open)) NULL else NA,
      ...
    )
  )
}


#' Accessible chart 
#' @param output The output UI, e.g. plotlyOutput("x") or leafletOutput("m")
#' @param label Short accessible name, e.g. "Drone treated acres by week"
#' @param caption Optional longer description (text or a textOutput())
#' @param caption_visible Show the caption on screen (default: screen readers only)
#' @return A <figure> tag
#' @export
a11y_figure <- function(output, label, caption = NULL, caption_visible = FALSE) {
  caption_tag <- NULL
  if (!is.null(caption)) {
    caption_tag <- shiny::tags$figcaption(
      class = if (isTRUE(caption_visible)) "a11y-figure-caption" else "a11y-figure-caption sr-only",
      caption
    )
  }
  shiny::tags$figure(
    class              = "a11y-figure",
    `data-a11y-label`  = label,
    `aria-label`       = label,
    output,
    caption_tag
  )
}


# -----------------------------------------------------------------------------
# Page shells
# -----------------------------------------------------------------------------

#' raise titlePanel() output to the page banner.
.a11y_promote_title <- function(children) {
  for (i in seq_along(children)) {
    child <- children[[i]]
    if (!inherits(child, "shiny.tag.list")) next
    names_in <- vapply(child, function(el) {
      if (inherits(el, "shiny.tag")) el$name else ""
    }, character(1))
    if (!("head" %in% names_in && "h2" %in% names_in)) next

    h2_pos <- which(names_in == "h2")[1]
    heading <- child[[h2_pos]]
    heading$name <- "h1"
    heading <- htmltools::tagAppendAttributes(heading, class = "h2")
    child[[h2_pos]] <- shiny::tags$header(class = "a11y-banner", heading)
    children[[i]] <- child
    break
  }
  children
}


#' Does this UI tree already contain a main landmark?
#' True for any app using mainPanel()
.a11y_has_main <- function(x) {
  if (inherits(x, "shiny.tag")) {
    if (identical(x$name, "main")) return(TRUE)
    role <- x$attribs[["role"]]
    if (!is.null(role) && any(as.character(role) == "main")) return(TRUE)
    return(.a11y_has_main(x$children))
  }
  if (is.list(x)) {
    for (child in x) if (.a11y_has_main(child)) return(TRUE)
  }
  FALSE
}

#' Put page content inside a main landmark.
.a11y_wrap_main <- function(children) {
  outside <- vapply(children, function(child) {
    if (inherits(child, "shiny.tag")) return(identical(child$name, "head"))
    if (inherits(child, "shiny.tag.list")) {
      names_in <- vapply(child, function(el) {
        if (inherits(el, "shiny.tag")) el$name else ""
      }, character(1))
      return("head" %in% names_in)   # titlePanel() output: head + banner
    }
    FALSE
  }, logical(1))
  c(children[outside], list(main_landmark(children[!outside])))
}


#' Accessible drop-in for shiny::fluidPage()

#' @param ... UI elements 
#' @param title Browser title 
#' @param theme Bootstrap theme 
#' @param lang Page 
#' @param skip_target 
#' @return A Shiny UI definition
#' @export
accessible_page <- function(..., title = NULL, theme = NULL, lang = "en",
                            skip_target = "main-content") {
  children <- .a11y_promote_title(list(...)) 
  # check if the UI tree already contains a main landmark
  if (!.a11y_has_main(children)) children <- .a11y_wrap_main(children)

  theme_fix <- if (!is.null(theme) && inherits(theme, "bs_theme")) {
    shiny::tags$head(shiny::tags$style(shiny::HTML(
      ".btn-default, .btn-default .action-label { color: #1f2933 !important; }"
    )))
  }

  do.call(shiny::fluidPage, c(
    list(accessibility_head(), theme_fix, skip_link(skip_target)),
    children,
    list(title = title, theme = theme, lang = lang)
  ))
}


#' Accessible drop-in for shinydashboard::dashboardPage()
#' @param ... Arguments passed straight through to dashboardPage()
#' @param lang Page language for WCAG 3.1.1 (default "en")
#' @param skip_target Fragment id the skip link aims at
#' @return A Shiny UI definition
#' @export
accessible_dashboard_page <- function(..., lang = "en", skip_target = "main-content") {
  if (!requireNamespace("shinydashboard", quietly = TRUE)) {
    stop("accessible_dashboard_page() requires the shinydashboard package")
  }

  page <- shiny::tagList(
    accessibility_head(),
    skip_link(skip_target),
    shinydashboard::dashboardPage(...)
  )
  attr(page, "lang") <- lang
  page
}
