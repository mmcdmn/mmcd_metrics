# Universal Documentation Sync Script
# Syncs all NOTES.md files to NOTES.html across all apps
# Run from shared/ folder: Rscript sync_all_docs.R

library(rmarkdown)
library(knitr)

# Function to get all app directories with NOTES files
get_apps_with_notes <- function() {
  base_path <- file.path("..", "apps")
  if (!dir.exists(base_path)) {
    cat("Error: apps directory not found relative to shared folder\n")
    return(character(0))
  }
  
  app_dirs <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  
  # Filter to only apps that have NOTES.md files
  apps_with_notes <- character(0)
  for (app_dir in app_dirs) {
    notes_md <- file.path(app_dir, "NOTES.md")
    if (file.exists(notes_md)) {
      apps_with_notes <- c(apps_with_notes, app_dir)
    }
  }
  
  return(apps_with_notes)
}

# Function to convert single markdown file to HTML with custom CSS
convert_md_to_html <- function(md_path, html_path, app_name) {
  cat(sprintf("  Converting %s...\n", basename(md_path)))
  
  # Read the markdown file
  if (!file.exists(md_path)) {
    cat(sprintf("    Warning: %s not found, skipping\n", md_path))
    return(FALSE)
  }
  
  md_content <- readLines(md_path, warn = FALSE)
  
  # Extract title from first H1 or use app name
  page_title <- sprintf("%s - Technical Notes", tools::toTitleCase(gsub("_", " ", app_name)))
  if (length(md_content) > 0) {
    first_h1 <- grep("^#\\s+", md_content)[1]
    if (!is.na(first_h1)) {
      page_title <- gsub("^#\\s+", "", md_content[first_h1])
    }
  }
  
  # Simple markdown to HTML conversion (without pandoc dependency)
  html_body <- convert_markdown_to_html(md_content)
  
  # Build complete HTML document
  html_content <- build_complete_html(html_body, page_title)
  
  # Write the final HTML file
  writeLines(html_content, html_path)
  
  return(TRUE)
  
  # Write the final HTML file
  writeLines(html_content, html_path)
  
  return(TRUE)
}

# Simple markdown to HTML converter (no pandoc dependency)
convert_markdown_to_html <- function(md_lines) {
  html_lines <- character(0)
  in_code_block <- FALSE
  code_block_lang <- ""
  in_list <- FALSE
  list_level <- 0
  
  for (i in seq_along(md_lines)) {
    line <- md_lines[i]
    original_line <- line
    
    # Handle code blocks
    if (grepl("^```", line)) {
      if (!in_code_block) {
        # Start code block
        in_code_block <- TRUE
        code_block_lang <- gsub("^```", "", line)
        html_lines <- c(html_lines, sprintf("<pre><code class=\"%s\">", code_block_lang))
      } else {
        # End code block
        in_code_block <- FALSE
        html_lines <- c(html_lines, "</code></pre>")
      }
      next
    }
    
    # If we're in a code block, just add the line as-is
    if (in_code_block) {
      # Escape HTML characters
      line <- gsub("&", "&amp;", line)
      line <- gsub("<", "&lt;", line)
      line <- gsub(">", "&gt;", line)
      html_lines <- c(html_lines, line)
      next
    }
    
    # Handle headers
    if (grepl("^#{1,6}\\s+", line)) {
      level <- nchar(gsub("(#+).*", "\\1", line))
      text <- gsub("^#+\\s+", "", line)
      text <- process_inline_formatting(text)
      html_lines <- c(html_lines, sprintf("<h%d>%s</h%d>", level, text, level))
      next
    }
    
    # Handle unordered lists
    if (grepl("^\\s*[-*+]\\s+", line)) {
      current_level <- (nchar(line) - nchar(ltrim(line))) %/% 2 + 1
      
      if (!in_list || current_level > list_level) {
        if (in_list && current_level > list_level) {
          html_lines <- c(html_lines, "<ul>")
        } else {
          html_lines <- c(html_lines, "<ul>")
          in_list <- TRUE
        }
        list_level <- current_level
      } else if (current_level < list_level) {
        while (list_level > current_level) {
          html_lines <- c(html_lines, "</ul>")
          list_level <- list_level - 1
        }
      }
      
      text <- gsub("^\\s*[-*+]\\s+", "", line)
      text <- process_inline_formatting(text)
      html_lines <- c(html_lines, sprintf("<li>%s</li>", text))
      next
    }
    
    # Handle ordered lists
    if (grepl("^\\s*\\d+\\.\\s+", line)) {
      if (!in_list) {
        html_lines <- c(html_lines, "<ol>")
        in_list <- TRUE
        list_level <- 1
      }
      
      text <- gsub("^\\s*\\d+\\.\\s+", "", line)
      text <- process_inline_formatting(text)
      html_lines <- c(html_lines, sprintf("<li>%s</li>", text))
      next
    }
    
    # End lists if we're not in a list item anymore
    if (in_list && !grepl("^\\s*[-*+\\d]", line) && nchar(trim(line)) > 0) {
      while (list_level > 0) {
        if (list_level > 0) {
          html_lines <- c(html_lines, "</ul>")
        }
        list_level <- list_level - 1
      }
      in_list <- FALSE
    }
    
    # Handle blockquotes
    if (grepl("^>", line)) {
      text <- gsub("^>\\s*", "", line)
      text <- process_inline_formatting(text)
      html_lines <- c(html_lines, sprintf("<blockquote>%s</blockquote>", text))
      next
    }
    
    # Handle horizontal rules
    if (grepl("^---+$|^\\*\\*\\*+$", line)) {
      html_lines <- c(html_lines, "<hr>")
      next
    }
    
    # Handle empty lines
    if (nchar(trim(line)) == 0) {
      # End lists on empty lines
      if (in_list) {
        while (list_level > 0) {
          html_lines <- c(html_lines, "</ul>")
          list_level <- list_level - 1
        }
        in_list <- FALSE
      }
      html_lines <- c(html_lines, "")
      next
    }
    
    # Regular paragraph
    if (nchar(trim(line)) > 0) {
      text <- process_inline_formatting(line)
      html_lines <- c(html_lines, sprintf("<p>%s</p>", text))
    }
  }
  
  # Close any remaining lists
  if (in_list) {
    while (list_level > 0) {
      html_lines <- c(html_lines, "</ul>")
      list_level <- list_level - 1
    }
  }
  
  return(html_lines)
}

# Process inline formatting (bold, italic, code, links)
process_inline_formatting <- function(text) {
  # Escape HTML first
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  
  # Handle inline code first (before other formatting)
  text <- gsub("`([^`]+)`", "<code>\\1</code>", text)
  
  # Handle bold
  text <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", text)
  text <- gsub("__([^_]+)__", "<strong>\\1</strong>", text)
  
  # Handle italic
  text <- gsub("\\*([^*]+)\\*", "<em>\\1</em>", text)
  text <- gsub("_([^_]+)_", "<em>\\1</em>", text)
  
  # Handle links [text](url)
  text <- gsub("\\[([^\\]]+)\\]\\(([^\\)]+)\\)", "<a href=\"\\2\">\\1</a>", text)
  
  return(text)
}

# Helper function to trim whitespace
trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}

# Helper function to trim leading whitespace only
ltrim <- function(x) {
  gsub("^\\s+", "", x)
}

# Build complete HTML document with styling
build_complete_html <- function(html_body, page_title) {
  html_doc <- c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "    <meta charset=\"UTF-8\">",
    "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
    sprintf("    <title>%s</title>", page_title),
    "    <style>",
    "        body {",
    "            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;",
    "            line-height: 1.6;",
    "            max-width: 1200px;",
    "            margin: 0 auto;",
    "            padding: 20px;",
    "            background-color: #f5f5f5;",
    "        }",
    "        .container {",
    "            background-color: white;",
    "            padding: 30px;",
    "            border-radius: 8px;",
    "            box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    "        }",
    "        h1 {",
    "            color: #2c3e50;",
    "            border-bottom: 3px solid #3498db;",
    "            padding-bottom: 10px;",
    "        }",
    "        h2 {",
    "            color: #34495e;",
    "            margin-top: 30px;",
    "            border-bottom: 2px solid #ecf0f1;",
    "            padding-bottom: 8px;",
    "        }",
    "        h3 {",
    "            color: #7f8c8d;",
    "            margin-top: 20px;",
    "        }",
    "        h4 {",
    "            color: #95a5a6;",
    "            margin-top: 15px;",
    "        }",
    "        code {",
    "            background-color: #f8f9fa;",
    "            padding: 2px 6px;",
    "            border-radius: 3px;",
    "            font-family: 'Courier New', monospace;",
    "            font-size: 0.9em;",
    "            color: #e74c3c;",
    "        }",
    "        pre {",
    "            background-color: #2c3e50;",
    "            color: #ecf0f1;",
    "            padding: 20px;",
    "            border-radius: 5px;",
    "            overflow-x: auto;",
    "            margin: 15px 0;",
    "            border: 1px solid #34495e;",
    "        }",
    "        pre code {",
    "            background-color: transparent;",
    "            color: #ecf0f1;",
    "            padding: 0;",
    "            border-radius: 0;",
    "            font-size: 0.85em;",
    "        }",
    "        ul, ol {",
    "            margin: 10px 0;",
    "            padding-left: 30px;",
    "        }",
    "        li {",
    "            margin: 5px 0;",
    "        }",
    "        blockquote {",
    "            border-left: 4px solid #3498db;",
    "            background-color: #f8f9fa;",
    "            padding: 10px 20px;",
    "            margin: 20px 0;",
    "            font-style: italic;",
    "        }",
    "        table {",
    "            width: 100%;",
    "            border-collapse: collapse;",
    "            margin: 20px 0;",
    "        }",
    "        th, td {",
    "            border: 1px solid #ddd;",
    "            padding: 12px;",
    "            text-align: left;",
    "        }",
    "        th {",
    "            background-color: #f5f5f5;",
    "            font-weight: bold;",
    "        }",
    "        hr {",
    "            border: none;",
    "            height: 2px;",
    "            background-color: #ecf0f1;",
    "            margin: 20px 0;",
    "        }",
    "        p {",
    "            margin: 10px 0;",
    "        }",
    "        a {",
    "            color: #3498db;",
    "            text-decoration: none;",
    "        }",
    "        a:hover {",
    "            text-decoration: underline;",
    "        }",
    "    </style>",
    "</head>",
    "<body>",
    "    <div class=\"container\">",
    html_body,
    "    </div>",
    "</body>",
    "</html>"
  )
  
  return(html_doc)
}

# Function to check if a file needs syncing
needs_sync <- function(md_path, html_path) {
  if (!file.exists(md_path) || !file.exists(html_path)) {
    return(TRUE)
  }
  
  md_time <- file.info(md_path)$mtime
  html_time <- file.info(html_path)$mtime
  
  return(md_time > html_time)
}

# Main sync function
sync_all_documentation <- function(force = FALSE) {
  cat("=== Universal Documentation Sync ===\n\n")
  
  # Check if we're in the shared folder
  if (!file.exists("db_helpers.R")) {
    cat("Error: This script should be run from the shared/ folder\n")
    cat("Current directory:", getwd(), "\n")
    return(FALSE)
  }
  
  apps <- get_apps_with_notes()
  
  if (length(apps) == 0) {
    cat("No apps with NOTES.md found\n")
    return(FALSE)
  }
  
  cat(sprintf("Found %d apps with NOTES files:\n", length(apps)))
  
  success_count <- 0
  skip_count <- 0
  error_count <- 0
  
  for (app_path in apps) {
    app_name <- basename(app_path)
    cat(sprintf("\n Processing %s:\n", app_name))
    
    md_path <- file.path(app_path, "NOTES.md")
    html_path <- file.path(app_path, "NOTES.html")
    
    # Check if sync is needed
    if (!force && !needs_sync(md_path, html_path)) {
      cat("   Already up to date, skipping\n")
      skip_count <- skip_count + 1
      next
    }
    
    # Convert MD to HTML
    if (convert_md_to_html(md_path, html_path, app_name)) {
      cat("   Successfully synced\n")
      success_count <- success_count + 1
    } else {
      cat("   Failed to sync\n")
      error_count <- error_count + 1
    }
  }
  
  cat("\n=== Sync Summary ===\n")
  cat(sprintf(" Success: %d apps\n", success_count))
  cat(sprintf("⏭  Skipped: %d apps (already up to date)\n", skip_count))
  cat(sprintf(" Errors:  %d apps\n", error_count))
  
  if (success_count > 0) {
    cat("\n Next steps:\n")
    cat("   1. Review the updated HTML files\n")
    cat("   2. Commit changes: git add . && git commit -m 'Sync documentation'\n")
  }
  
  return(error_count == 0)
}

# Function to list all apps and their sync status
check_sync_status <- function() {
  cat("=== Documentation Sync Status ===\n\n")
  
  apps <- get_apps_with_notes()
  
  if (length(apps) == 0) {
    cat("No apps with NOTES.md found\n")
    return()
  }
  
  for (app_path in apps) {
    app_name <- basename(app_path)
    md_path <- file.path(app_path, "NOTES.md")
    html_path <- file.path(app_path, "NOTES.html")
    
    md_exists <- file.exists(md_path)
    html_exists <- file.exists(html_path)
    needs_update <- needs_sync(md_path, html_path)
    
    status <- if (!md_exists) {
      " No NOTES.md"
    } else if (!html_exists) {
      " HTML missing"
    } else if (needs_update) {
      "  Needs sync"
    } else {
      " Up to date"
    }
    
    cat(sprintf("%-25s %s\n", app_name, status))
  }
  
  cat("\nRun sync_all_documentation() to update all files\n")
}

# Check if required packages are installed
check_dependencies <- function() {
  # We only need base R packages for our simple markdown parser
  return(FALSE)
}

# Main execution
if (!interactive()) {
  # If run as script, sync automatically
  check_dependencies()
  
  # Check for command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  force <- "--force" %in% args || "-f" %in% args
  status_only <- "--status" %in% args || "-s" %in% args
  
  if (status_only) {
    check_sync_status()
  } else {
    sync_all_documentation(force = force)
  }
} else {
  # If run interactively, provide helper functions
  check_dependencies()
  
  cat("Universal Documentation Sync loaded!\n\n")
  cat("Available functions:\n")
  cat("- sync_all_documentation(force=FALSE): Sync all NOTES.md to NOTES.html\n")
  cat("- check_sync_status(): Show status of all documentation files\n")
  cat("- get_apps_with_notes(): List all apps with NOTES files\n\n")
  
  check_sync_status()
}