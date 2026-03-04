# Air Inspection Checklist - Display Functions
# Functions for building the checklist UI elements

#' Create a stat summary box
#' @param value The value to display
#' @param label The label text
#' @param bg_color Background color
#' @return Shiny tag
create_summary_box <- function(value, label, bg_color = "#f8f9fa") {
  # Auto-detect text color: white for dark backgrounds, dark for light ones
  is_dark <- bg_color %in% c("#28a745", "#dc3545", "#007bff", "#dc2626", "#16a34a")
  text_color <- if (is_dark) "#ffffff" else "#333333"
  label_color <- if (is_dark) "rgba(255,255,255,0.85)" else "#666666"
  
  div(class = "summary-box",
      style = paste0("background-color:", bg_color, "; flex: 1; min-width: 120px;"),
      div(class = "summary-number", style = paste0("color:", text_color, ";"), value),
      div(class = "summary-label", style = paste0("color:", label_color, ";"), label)
  )
}

#' Build a bug status badge
#' @param bug_status Character: "Red Bugs", "Blue Bugs", "Pending Lab", "No Sample", "No Bugs"
#' @return HTML string for the badge
bug_badge_html <- function(bug_status) {
  if (is.na(bug_status) || bug_status == "No Sample") return("")

  badge_class <- switch(bug_status,
    "Red Bugs"    = "bug-red",
    "Blue Bugs"   = "bug-blue",
    "Pending Lab" = "bug-pending",
    "No Bugs"     = "bug-none",
    ""
  )

  if (badge_class == "") return("")
  sprintf('<span class="item-bug-badge %s">%s</span>', badge_class, bug_status)
}

#' Build the full checklist HTML grouped by FOS area then AirMap
#' Includes summary value boxes at the top showing filtered totals
#' @param data Data frame from get_checklist_data()
#' @param show_unfinished_only Logical, filter to only not-inspected sites
#' @return Shiny tagList
build_checklist_html <- function(data, show_unfinished_only = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(div(style = "text-align: center; padding: 40px; color: #999;",
               icon("clipboard-list", style = "font-size: 48px;"),
               h4("No RED air sites found for the selected filters."),
               p("Try changing facility, FOS, or lookback days.")))
  }

  # Filter if showing unfinished only
  if (show_unfinished_only) {
    data <- data[!data$was_inspected, ]
    if (nrow(data) == 0) {
      return(div(style = "text-align: center; padding: 40px; color: #28a745;",
                 icon("check-circle", style = "font-size: 48px;"),
                 h4("All sites have been inspected!"),
                 p("All RED air sites have inspections within the lookback window.")))
    }
  }

  # ---- Summary value boxes (based on currently filtered/displayed data) ----
  total_sites <- nrow(data)
  inspected_sites <- sum(data$was_inspected, na.rm = TRUE)
  not_inspected <- total_sites - inspected_sites
  pct_complete <- if (total_sites > 0) round(100 * inspected_sites / total_sites, 1) else 0
  red_bugs <- sum(data$bug_status == "Red Bugs", na.rm = TRUE)
  pending_lab <- sum(data$bug_status == "Pending Lab", na.rm = TRUE)

  pct_color <- if (pct_complete >= 90) "#28a745" else if (pct_complete >= 60) "#ffc107" else "#dc3545"

  value_boxes <- div(class = "checklist-value-boxes",
    style = "display: flex; gap: 12px; margin-bottom: 20px; flex-wrap: wrap;",
    create_summary_box(paste0(inspected_sites, " / ", total_sites),
                       paste0("Inspected (", pct_complete, "%)"), pct_color),
    create_summary_box(not_inspected, "Not Inspected",
                       if (not_inspected == 0) "#28a745" else "#f8f9fa"),
    create_summary_box(red_bugs, "Red Bugs",
                       if (red_bugs > 0) "#dc3545" else "#f8f9fa"),
    create_summary_box(pending_lab, "Pending Lab",
                       if (pending_lab > 0) "#ffc107" else "#f8f9fa")
  )

  # ---- Build checklist grouped by FOS -> AirMap -> Sites ----
  # Ensure airmap_display column exists (fallback for old data without it)
  if (!"airmap_display" %in% names(data)) {
    data$airmap_display <- "No AirMap"
  }

  fos_groups <- split(data, data$fos_display)
  fos_groups <- fos_groups[order(names(fos_groups))]

  checklist_tags <- tagList()

  for (fos_name in names(fos_groups)) {
    fos_data <- fos_groups[[fos_name]]

    # Count stats for this FOS
    fos_total <- nrow(fos_data)
    fos_done <- sum(fos_data$was_inspected, na.rm = TRUE)

    fos_header <- div(class = "checklist-header",
                      sprintf("%s  (%d / %d inspected)", fos_name, fos_done, fos_total))

    # Split by AirMap within this FOS (replaces section grouping)
    airmap_groups <- split(fos_data, fos_data$airmap_display)
    # Sort airmap groups: extract numeric part for natural ordering
    airmap_names <- names(airmap_groups)
    airmap_nums <- suppressWarnings(as.numeric(gsub("[^0-9]", "", airmap_names)))
    airmap_order <- order(airmap_nums, airmap_names, na.last = TRUE)
    airmap_groups <- airmap_groups[airmap_order]

    airmap_tags <- tagList()

    for (airmap_name in names(airmap_groups)) {
      map_data <- airmap_groups[[airmap_name]]

      map_total <- nrow(map_data)
      map_done <- sum(map_data$was_inspected, na.rm = TRUE)

      # Collapsible AirMap sub-header
      map_id <- paste0("airmap_", gsub("[^a-zA-Z0-9]", "_", paste0(fos_name, "_", airmap_name)))
      airmap_header <- div(
        class = "section-subheader airmap-toggle",
        style = "cursor: pointer; user-select: none;",
        `data-target` = map_id,
        icon("chevron-right", class = "airmap-chevron"),
        sprintf(" %s  (%d / %d)", airmap_name, map_done, map_total)
      )

      # Group by section within this AirMap
      section_groups <- split(map_data, map_data$sectcode)
      section_groups <- section_groups[order(names(section_groups))]

      section_tags <- tagList()

      for (section_name in names(section_groups)) {
        sec_data <- section_groups[[section_name]]
        sec_data <- sec_data[order(sec_data$sitecode), ]

        sec_total <- nrow(sec_data)
        sec_done <- sum(sec_data$was_inspected, na.rm = TRUE)

        section_header <- div(class = "section-inner-subheader",
                              sprintf("Section %s  (%d / %d)", section_name, sec_done, sec_total))

        items_list <- tagList()

        for (j in seq_len(nrow(sec_data))) {
          row <- sec_data[j, ]
          is_done <- isTRUE(row$was_inspected)

          if (is_done) {
            icon_html <- icon("check-circle", class = "item-done")
            # Use inspector_display (name) instead of emp number
            emp_text <- if ("inspector_display" %in% names(row) &&
                            !is.na(row$inspector_display) && row$inspector_display != "") {
              row$inspector_display
            } else if (!is.na(row$inspector_emp) && row$inspector_emp != "") {
              paste0("Emp #", row$inspector_emp)
            } else {
              ""
            }
            dip_text <- if (!is.na(row$dip_count)) {
              paste0("Dip: ", row$dip_count)
            } else {
              ""
            }
            details_parts <- c(emp_text, dip_text)
            details_parts <- details_parts[details_parts != ""]
            details_str <- paste(details_parts, collapse = " | ")

            # Add bug badge
            badge_str <- bug_badge_html(row$bug_status)

            detail_div <- div(class = "item-details",
                              HTML(paste0(details_str, badge_str)))
          } else {
            icon_html <- icon("times-circle", class = "item-not-done")
            detail_div <- div(class = "item-details", 
                              paste0(round(row$acres, 2), " acres"))
          }

          # Active treatment badge
          trt_badge <- ""
          has_trt <- isTRUE(row$has_active_treatment)
          if (has_trt) {
            mat_label <- if (!is.na(row$active_material) && row$active_material != "") {
              row$active_material
            } else {
              "Treated"
            }
            trt_badge <- sprintf('<span class="badge-treatment" title="Expires %s">%s</span>',
                                 if (!is.na(row$active_trt_expiry)) as.character(row$active_trt_expiry) else "?",
                                 paste0("\U1F6AB ", mat_label))
          }

          item_css <- paste("checklist-item", if (has_trt) "item-active-trt" else "")

          item <- div(class = item_css,
                      div(class = "item-status-icon", icon_html),
                      div(class = "item-sitecode", HTML(make_sitecode_link(row$sitecode))),
                      detail_div,
                      HTML(trt_badge))

          items_list <- tagAppendChild(items_list, item)
        }

        section_tags <- tagAppendChild(section_tags, section_header)
        section_tags <- tagAppendChild(section_tags, items_list)
      }

      # Build the collapsible container with section-grouped items
      item_container <- div(id = map_id, class = "airmap-sites", style = "display: none;",
                            section_tags)

      airmap_tags <- tagAppendChild(airmap_tags, airmap_header)
      airmap_tags <- tagAppendChild(airmap_tags, item_container)
    }

    fos_block <- div(class = "checklist-section",
                     fos_header,
                     airmap_tags)

    checklist_tags <- tagAppendChild(checklist_tags, fos_block)
  }

  # Wrap everything: value boxes + checklist
  tagList(value_boxes, checklist_tags)
}
