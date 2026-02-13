# Air Inspection Checklist - Display Functions
# Functions for building the checklist UI elements

#' Create a stat summary box
#' @param value The value to display
#' @param label The label text
#' @param bg_color Background color
#' @return Shiny tag
create_summary_box <- function(value, label, bg_color = "#f8f9fa") {
  div(class = "summary-box", style = paste0("background-color:", bg_color, ";"),
      div(class = "summary-number", value),
      div(class = "summary-label", label)
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

#' Build the full checklist HTML grouped by FOS area then section
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

  # Group by FOS area, then section
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

    # Split by section within this FOS
    section_groups <- split(fos_data, fos_data$sectcode)
    section_groups <- section_groups[order(names(section_groups))]

    section_tags <- tagList()

    for (section_name in names(section_groups)) {
      sec_data <- section_groups[[section_name]]
      sec_data <- sec_data[order(sec_data$sitecode), ]

      sec_total <- nrow(sec_data)
      sec_done <- sum(sec_data$was_inspected, na.rm = TRUE)

      section_header <- div(class = "section-subheader",
                            sprintf("Section %s  (%d / %d)", section_name, sec_done, sec_total))

      item_tags <- tagList()

      for (j in seq_len(nrow(sec_data))) {
        row <- sec_data[j, ]
        is_done <- isTRUE(row$was_inspected)

        if (is_done) {
          icon_html <- icon("check-circle", class = "item-done")
          emp_text <- if (!is.na(row$inspector_emp) && row$inspector_emp != "") {
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
                    div(class = "item-sitecode", row$sitecode),
                    detail_div,
                    HTML(trt_badge))

        item_tags <- tagAppendChild(item_tags, item)
      }

      section_tags <- tagAppendChild(section_tags, section_header)
      section_tags <- tagAppendChild(section_tags, item_tags)
    }

    fos_block <- div(class = "checklist-section",
                     fos_header,
                     section_tags)

    checklist_tags <- tagAppendChild(checklist_tags, fos_block)
  }

  return(checklist_tags)
}
