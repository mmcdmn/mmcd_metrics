@echo off
REM Universal Documentation Sync
REM Windows batch wrapper for sync_all_docs.R
REM Run from shared/ folder

setlocal enabledelayedexpansion

REM Check for help flag
if "%1"=="--help" goto :help
if "%1"=="-h" goto :help
if "%1"=="/?" goto :help

REM Check if we're in the shared folder
if not exist "db_helpers.R" (
    echo Error: This script must be run from the shared/ folder
    echo Current location: %CD%
    echo Expected to find: db_helpers.R
    exit /b 1
)

REM Check if R is available
Rscript --version >nul 2>&1
if errorlevel 1 (
    echo Error: R/Rscript not found in PATH
    echo Please install R or ensure it's in your PATH
    exit /b 1
)

REM Display banner
echo.
echo  Universal Documentation Sync
echo  Working directory: %CD%

REM Parse arguments and build R command
set "rargs="
set "mode=sync"

:parse_args
if "%1"=="" goto :run_sync
if "%1"=="--force" (
    set "rargs=%rargs% --force"
    set "mode=force"
    shift
    goto :parse_args
)
if "%1"=="-f" (
    set "rargs=%rargs% --force"
    set "mode=force"
    shift
    goto :parse_args
)
if "%1"=="--status" (
    set "rargs=%rargs% --status"
    set "mode=status"
    shift
    goto :parse_args
)
if "%1"=="-s" (
    set "rargs=%rargs% --status"
    set "mode=status"
    shift
    goto :parse_args
)
REM Unknown argument, pass it through
set "rargs=%rargs% %1"
shift
goto :parse_args

:run_sync
if "%mode%"=="status" (
    echo  Checking sync status...
) else if "%mode%"=="force" (
    echo  Force syncing all documentation...
) else (
    echo  Syncing changed documentation...
)
echo.

REM Run the R script
if "%rargs%"=="" (
    Rscript sync_all_docs.R
) else (
    Rscript sync_all_docs.R%rargs%
)

set "exit_code=%errorlevel%"

if %exit_code% equ 0 (
    echo.
    echo  Documentation sync completed successfully!
) else (
    echo.
    echo  Documentation sync completed with errors
)

echo.
pause
exit /b %exit_code%

:help
echo Universal Documentation Sync
echo Syncs all NOTES.md files to NOTES.html across all apps
echo.
echo Usage:
echo   sync_all_docs.bat              # Sync only files that have changed
echo   sync_all_docs.bat --force      # Force sync all files
echo   sync_all_docs.bat --status     # Show sync status only
echo   sync_all_docs.bat --help       # Show this help
echo.
echo Examples:
echo   sync_all_docs.bat              # Quick sync
echo   sync_all_docs.bat --status     # Check what needs updating
echo   sync_all_docs.bat --force      # Rebuild all HTML files
echo.
pause
exit /b 0