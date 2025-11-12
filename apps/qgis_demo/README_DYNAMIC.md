# QGIS Demo - Dynamic Heat Map Generation

## How It Works

This app **dynamically generates QGIS projects** based on user inputs. Here's the flow:

### 1. User Selects Parameters
- Species (Aedes triseriatus, japonicus, or albopictus)
- Analysis Date
- K-Nearest Neighbors value

### 2. App Computes Vector Index
Using the same SQL and k-NN algorithm as `trap_survillance_test`:
- Queries trap inspection data from PostgreSQL
- Calculates inverse-distance weighted vector index for each section
- Returns sections with vector_index values

### 3. Dynamic QGIS Project Generation
The app creates a **new QGIS project file** on every refresh:

```r
generate_advanced_qgis_project(
  sections_data = computed_data,
  traps_data = trap_locations,
  analysis_date = user_selected_date,
  species_label = user_selected_species
)
```

This function:
- Writes computed data to a new GPKG file in `/qgis/projects/`
- Reads the `simple_template.xml` template
- Replaces placeholders with actual values:
  - `{{ANALYSIS_DATE}}` → User's date
  - `{{SPECIES_LABEL}}` → Selected species
  - `{{GPKG_PATH}}` → Path to the generated GPKG
- Saves as a new `.qgs` file with unique timestamp

### 4. QGIS Server Renders the Map
- QGIS Server reads the newly created `.qgs` project
- Applies graduated symbology (yellow → orange → red)
- Renders as WMS tiles

### 5. Display in Browser
- Shiny requests a WMS GetMap image from QGIS Server
- Layer name is dynamic: `"Simple Heat Map - {analysis_date}"`
- Image is displayed in the app

## Template File

`simple_template.xml` contains:
- Layer definitions (heat map, counties, facility areas)
- Graduated symbol styling with 4 classes
- Placeholders that get replaced at runtime

## Key Files

- `app.R` - Main Shiny app with UI and server logic
- `qgis_helpers.R` - Functions to generate dynamic QGIS projects
- `data_functions.R` - SQL queries and vector index calculation
- `simple_template.xml` - QGIS project template with placeholders

## Docker Setup

In your container:
```
/qgis/projects/                      # Dynamic projects directory
  ├── advanced_heatmap_20251112_143022.qgs
  ├── advanced_heatmap_20251112_143022_data.gpkg
  ├── advanced_heatmap_20251112_144105.qgs
  └── advanced_heatmap_20251112_144105_data.gpkg
```

Each time a user clicks "Generate", a new project is created!

## GPKG Reference Layers

The template includes static reference layers from your shared GPKG files:
- `/srv/shiny-server/shared/gpkg/7counties.gpkg` (county boundaries)
- `/srv/shiny-server/shared/gpkg/MMCD_FacilityArea.gpkg` (facility areas)

These paths are hardcoded in the template since they don't change.

## Why This Approach?

**Benefits:**
- Map updates based on live database queries
- Symbology reflects current data ranges
- Layer names show analysis parameters
- Multiple users can run different analyses simultaneously

## Testing Locally

1. Run the app in Docker
2. Select different species and dates
3. Click "Generate QGIS Cartography"
4. Each run creates a new `/qgis/projects/advanced_heatmap_*.qgs` file
5. QGIS Server renders it with professional styling

The map changes based on your inputs! 🎉
