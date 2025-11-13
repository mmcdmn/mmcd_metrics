#!/bin/bash
# Universal Documentation Sync - Git Bash wrapper
# Finds R installation and runs the sync script

set -e

# Function to find R installation
find_r_path() {
    # Common R installation paths on Windows
    local r_paths=(
        "/c/Program Files/R/R-4.5.2/bin"
        "/c/Program Files/R/R-4.5.1/bin"
        "/c/Program Files/R/R-4.4.1/bin"
        "/c/Program Files/R/R-4.4.0/bin"
        "/c/Program Files (x86)/R/R-4.5.2/bin"
        "/c/Program Files (x86)/R/R-4.5.1/bin"
    )
    
    # Try to find Rscript
    for path in "${r_paths[@]}"; do
        if [[ -f "$path/Rscript.exe" ]]; then
            echo "$path/Rscript.exe"
            return 0
        fi
    done
    
    # Try PATH
    if command -v Rscript >/dev/null 2>&1; then
        echo "Rscript"
        return 0
    fi
    
    return 1
}

# Check if we're in the shared folder
if [[ ! -f "db_helpers.R" ]]; then
    echo "Error: This script must be run from the shared/ folder"
    echo "Current location: $(pwd)"
    echo "Expected to find: db_helpers.R"
    exit 1
fi

# Find R
echo "🔍 Looking for R installation..."
if ! RSCRIPT_PATH=$(find_r_path); then
    echo "❌ Error: R/Rscript not found"
    echo "Please install R from https://cran.r-project.org/ or ensure it's in your PATH"
    exit 1
fi

echo "✅ Found R: $RSCRIPT_PATH"

# Display banner
echo ""
echo "🔄 Universal Documentation Sync"
echo "📁 Working directory: $(pwd)"

# Parse arguments
FORCE=""
STATUS=""
HELP=""

for arg in "$@"; do
    case $arg in
        --force|-f)
            FORCE="--force"
            ;;
        --status|-s)
            STATUS="--status"
            ;;
        --help|-h)
            HELP="--help"
            ;;
    esac
done

if [[ -n "$HELP" ]]; then
    echo "Universal Documentation Sync"
    echo "Syncs all NOTES.md files to NOTES.html across all apps"
    echo ""
    echo "Usage:"
    echo "  ./sync_all_docs.sh           # Sync only files that have changed"
    echo "  ./sync_all_docs.sh --force   # Force sync all files"
    echo "  ./sync_all_docs.sh --status  # Show sync status only"
    echo "  ./sync_all_docs.sh --help    # Show this help"
    echo ""
    echo "Examples:"
    echo "  ./sync_all_docs.sh            # Quick sync"
    echo "  ./sync_all_docs.sh --status   # Check what needs updating"
    echo "  ./sync_all_docs.sh --force    # Rebuild all HTML files"
    exit 0
fi

if [[ -n "$STATUS" ]]; then
    echo " Checking sync status..."
elif [[ -n "$FORCE" ]]; then
    echo " Force syncing all documentation..."
else
    echo " Syncing changed documentation..."
fi
echo ""

# Run the R script
set +e  # Don't exit on error, we want to capture the exit code

if [[ -n "$FORCE" ]]; then
    "$RSCRIPT_PATH" sync_all_docs.R --force
elif [[ -n "$STATUS" ]]; then
    "$RSCRIPT_PATH" sync_all_docs.R --status
else
    "$RSCRIPT_PATH" sync_all_docs.R
fi

exit_code=$?

echo ""
if [[ $exit_code -eq 0 ]]; then
    echo " Documentation sync completed successfully!"
else
    echo " Documentation sync completed with errors"
fi

exit $exit_code