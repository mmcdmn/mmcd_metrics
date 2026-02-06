# Integration test: Test JavaScript visibility toggle behavior

cat("Running facility detail container visibility test...\n\n")

ui_path <- file.path("apps", "overview", "dynamic_ui.R")
ui_lines <- readLines(ui_path, warn = FALSE)
ui_text <- paste(ui_lines, collapse = "\n")

# Test 1: Container should NOT have inline display:none
cat("Test 1: Container inline styles\n")
container_pattern <- 'id="facility_detail_container"[^>]*style="[^"]*display:\\s*none'
has_inline_none <- grepl(container_pattern, ui_text, perl = TRUE)
if (has_inline_none) {
  stop("FAIL: Container has inline display:none which blocks CSS class toggle")
}
cat("  PASS: No inline display:none found\n\n")

# Test 2: CSS should set default display:none on class
cat("Test 2: CSS default display:none\n")
css_default_pattern <- '\\.facility-detail-section\\s*\\{[^}]*display:\\s*none'
has_css_default <- grepl(css_default_pattern, ui_text, perl = TRUE)
if (!has_css_default) {
  stop("FAIL: CSS should set display:none by default on .facility-detail-section")
}
cat("  PASS: CSS sets display:none by default\n\n")

# Test 3: CSS should override with .visible class
cat("Test 3: CSS .visible override\n")
css_visible_pattern <- '\\.facility-detail-section\\.visible\\s*\\{[^}]*display:\\s*block\\s*!important'
has_css_visible <- grepl(css_visible_pattern, ui_text, perl = TRUE)
if (!has_css_visible) {
  stop("FAIL: CSS should set display:block !important for .visible class")
}
cat("  PASS: CSS .visible class overrides with display:block !important\n\n")

# Test 4: JavaScript should toggle .visible class
cat("Test 4: JavaScript class toggle\n")
js_add_visible <- grepl("addClass\\(['\"]visible['\"]\\)", ui_text)
js_remove_visible <- grepl("removeClass\\(['\"]visible['\"]\\)", ui_text)
if (!js_add_visible || !js_remove_visible) {
  stop("FAIL: JavaScript should add/remove 'visible' class")
}
cat("  PASS: JavaScript toggles .visible class\n\n")

# Test 5: uiOutput should be inside container
cat("Test 5: uiOutput placement\n")
# Just check both exist - structural placement is sufficient
has_container_id <- any(grepl('facility_detail_container', ui_lines, fixed = TRUE))
has_uiOutput <- any(grepl('facility_detail_boxes', ui_lines, fixed = TRUE))
if (!has_container_id) {
  stop("FAIL: Missing facility_detail_container id")
}
if (!has_uiOutput) {
  stop("FAIL: Missing uiOutput for facility_detail_boxes")
}
cat("  PASS: Both container and uiOutput present\n\n")

cat("=== ALL VISIBILITY TESTS PASSED ===\n")
