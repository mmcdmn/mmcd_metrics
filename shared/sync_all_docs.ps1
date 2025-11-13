# Universal Documentation Sync
# PowerShell wrapper for sync_all_docs.R
# Run from shared/ folder

param(
    [switch]$Force,
    [switch]$Status,
    [switch]$Help
)

if ($Help) {
    Write-Host "Universal Documentation Sync" -ForegroundColor Green
    Write-Host "Syncs all NOTES.md files to NOTES.html across all apps" -ForegroundColor White
    Write-Host ""
    Write-Host "Usage:" -ForegroundColor Yellow
    Write-Host "  .\sync_all_docs.ps1           # Sync only files that have changed"
    Write-Host "  .\sync_all_docs.ps1 -Force    # Force sync all files"
    Write-Host "  .\sync_all_docs.ps1 -Status   # Show sync status only"
    Write-Host "  .\sync_all_docs.ps1 -Help     # Show this help"
    Write-Host ""
    Write-Host "Examples:" -ForegroundColor Cyan
    Write-Host "  .\sync_all_docs.ps1            # Quick sync"
    Write-Host "  .\sync_all_docs.ps1 -Status    # Check what needs updating"
    Write-Host "  .\sync_all_docs.ps1 -Force     # Rebuild all HTML files"
    exit
}

# Check if we're in the shared folder
if (!(Test-Path "db_helpers.R")) {
    Write-Host "Error: This script must be run from the shared/ folder" -ForegroundColor Red
    Write-Host "Current location: $(Get-Location)" -ForegroundColor Yellow
    Write-Host "Expected to find: db_helpers.R" -ForegroundColor Yellow
    exit 1
}

# Check if R is available
$rPath = Get-Command Rscript -ErrorAction SilentlyContinue
if (!$rPath) {
    Write-Host "Error: R/Rscript not found in PATH" -ForegroundColor Red
    Write-Host "Please install R or ensure it's in your PATH" -ForegroundColor Yellow
    exit 1
}

# Build command arguments
$rArgs = @()
if ($Force) { $rArgs += "--force" }
if ($Status) { $rArgs += "--status" }

# Display banner
Write-Host ""
Write-Host " Universal Documentation Sync" -ForegroundColor Green
Write-Host " Working directory: $(Get-Location)" -ForegroundColor Gray

if ($Status) {
    Write-Host " Checking sync status..." -ForegroundColor Yellow
} elseif ($Force) {
    Write-Host " Force syncing all documentation..." -ForegroundColor Yellow
} else {
    Write-Host " Syncing changed documentation..." -ForegroundColor Yellow
}
Write-Host ""

# Run the R script
try {
    if ($rArgs.Count -gt 0) {
        & Rscript sync_all_docs.R $rArgs
    } else {
        & Rscript sync_all_docs.R
    }
    
    $exitCode = $LASTEXITCODE
    
    if ($exitCode -eq 0) {
        Write-Host ""
        Write-Host " Documentation sync completed successfully!" -ForegroundColor Green
    } else {
        Write-Host ""
        Write-Host " Documentation sync completed with errors" -ForegroundColor Red
        exit $exitCode
    }
} catch {
    Write-Host ""
    Write-Host " Failed to run sync script: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

Write-Host ""