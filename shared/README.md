# Shared Resources

This folder contains shared utilities and resources used across all apps in the mmcd_metrics project.

## Files

### Database Helpers
- **`db_helpers.R`** - Common database connection and utility functions

### Documentation Sync System
- **`sync_all_docs.R`** - Universal documentation sync script (main logic)
- **`sync_all_docs.ps1`** - PowerShell wrapper with enhanced UI
- **`sync_all_docs.bat`** - Windows batch file for double-click execution  
- **`sync_all_docs.sh`** - Git Bash/Linux wrapper with R auto-detection
- **`README_UNIVERSAL_SYNC.md`** - Detailed documentation for the sync system

## Quick Start - Documentation Sync

The universal sync system automatically converts all `NOTES.md` files to `NOTES.html` across all apps.

### Windows (Recommended)
```powershell
# From shared/ folder
.\sync_all_docs.ps1              # Sync changed files
.\sync_all_docs.ps1 -Status      # Check what needs updating
.\sync_all_docs.ps1 -Force       # Rebuild all HTML files
```

### Alternative Methods
```bash
# Double-click sync_all_docs.bat in Windows Explorer

# Or use Git Bash
./sync_all_docs.sh --status

# Or run R directly
Rscript sync_all_docs.R

#Run on windows
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R --status
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" sync_all_docs.R --Force
```

### Current Apps with Documentation
- `cattail/` 
- `drone/`
- `ground_prehatch_progress/`
- `struct_trt/`
- `suco_history/`
- `trap_surveillance_test/`

The system automatically discovers new apps when you add `NOTES.md` files - no configuration needed!

## Requirements

- **R**: The sync system requires R to be installed
- **Windows**: PowerShell execution may need to be enabled for `.ps1` files
- **Location**: Scripts must be run from the `shared/` folder

## See Also

- `README_UNIVERSAL_SYNC.md` - Complete documentation sync system guide
- Individual app folders for app-specific documentation