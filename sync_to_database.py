# =============================================================================
# QGIS Python Script: Sync Any Local Layer to PostgreSQL Database
# 
# UNIVERSAL WORKFLOW:
# 1. Load your local data (any .gpkg, .shp, etc.) into QGIS via drag-drop
# 2. Load the database layer (drag the .qlr file)
# 3. Run this script
# 4. Choose which layer to upload
# 5. Script handles the rest (upsert mode)
#
# INSTALLATION:
# Save as: C:\Users\[USERNAME]\AppData\Roaming\QGIS\QGIS3\profiles\default\python\scripts\sync_to_database.py
#
# USAGE:
# Plugins > Python Console > Show Editor > Open Script > select this file > Run
# =============================================================================

from qgis.core import (
    QgsProject, QgsField, QgsFeature, QgsGeometry,
    QgsCoordinateTransform, QgsFieldConstraints, QgsExpression,
    QgsApplication
)
from qgis.utils import iface
from PyQt5.QtCore import QVariant, Qt
from PyQt5.QtWidgets import (
    QMessageBox, QInputDialog, QComboBox, QVBoxLayout, QDialog,
    QLabel, QDialogButtonBox, QLineEdit, QFormLayout, QTextEdit,
    QProgressDialog
)
import re

# --- FIND AVAILABLE LAYERS ---
project = QgsProject.instance()
all_layers = list(project.mapLayers().values())

if not all_layers:
    iface.messageBar().pushCritical("Error", "No layers loaded! Load your data and the database layer first.")
    raise Exception("No layers found")

# Separate local layers from database layer
local_layers = []
database_layer = None

for lyr in all_layers:
    if lyr.type() != 0:  # Skip non-vector layers
        continue
    
    if "loc_breeding_site_cards_sjsreast2" in lyr.name() or "database" in lyr.name().lower():
        database_layer = lyr
    elif lyr.name() != "loc_breeding_site_cards_sjsreast2":
        # Any other layer is assumed to be a data source
        local_layers.append(lyr)

if not database_layer:
    iface.messageBar().pushCritical("Error", "Could not find database layer (loc_breeding_site_cards_sjsreast2)!")
    raise Exception("Database layer not found")

if not local_layers:
    iface.messageBar().pushCritical("Error", "No local data layers found! Load your data first (BS_Brian.gpkg, etc.)")
    raise Exception("No source layers found")

# --- LET USER CHOOSE SOURCE LAYER ---
if len(local_layers) == 1:
    source_layer = local_layers[0]
    reply = QMessageBox.question(
        None, "Confirm Source Layer",
        f"Found data layer: {source_layer.name()}\n\nUse this as source?",
        QMessageBox.Yes | QMessageBox.No
    )
    if reply != QMessageBox.Yes:
        raise Exception("User cancelled")
else:
    # Multi-choice dialog
    from PyQt5.QtWidgets import QDialogButtonBox
    
    dlg = QDialog()
    dlg.setWindowTitle("Select Source Layer")
    layout = QVBoxLayout()
    layout.addWidget(QLabel("Which layer do you want to sync to the database?"))
    
    combo = QComboBox()
    for lyr in local_layers:
        combo.addItem(f"{lyr.name()} ({lyr.featureCount()} features)", lyr)
    layout.addWidget(combo)
    
    # Add OK/Cancel buttons
    buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
    buttons.accepted.connect(dlg.accept)
    buttons.rejected.connect(dlg.reject)
    layout.addWidget(buttons)
    
    dlg.setLayout(layout)
    if dlg.exec_() != QDialog.Accepted:
        raise Exception("User cancelled")
    
    source_layer = combo.currentData()

print(f"Source layer: {source_layer.name()} ({source_layer.featureCount()} features)")
print(f"Target layer: {database_layer.name()} ({database_layer.featureCount()} features)")

# --- MAP COLUMNS ---
# Build lowercase lookup for source columns
source_fields = {f.name().lower(): f.name() for f in source_layer.fields()}
target_fields = {f.name().lower(): f.name() for f in database_layer.fields()}

# Skip these (auto-generated or QGIS internal)
skip_cols = {"id", "fid"}
skip_prefixes = ("auxiliary_storage",)

# Find matching, extra, and missing columns
matching = {}  # target_name -> source_name
extra_in_source = []

for src_lower, src_name in source_fields.items():
    if src_lower in skip_cols:
        continue
    if any(src_lower.startswith(p) for p in skip_prefixes):
        continue
    if src_lower in target_fields:
        matching[target_fields[src_lower]] = src_name
    else:
        extra_in_source.append(src_name)

missing_in_source = []
for tgt_lower, tgt_name in target_fields.items():
    if tgt_lower in skip_cols or tgt_lower == "geom":
        continue
    if tgt_lower not in source_fields:
        missing_in_source.append(tgt_name)

print(f"\nMatching columns ({len(matching)}):")
for tgt, src in sorted(matching.items()):
    flag = " (case diff)" if tgt != src else ""
    print(f"  {src} -> {tgt}{flag}")

if extra_in_source:
    print(f"\nExtra columns in {source_layer.name()} ({len(extra_in_source)}):")
    for col in extra_in_source:
        print(f"  + {col}")

if missing_in_source and len(missing_in_source) <= 10:
    print(f"\nColumns in database but not in source ({len(missing_in_source)}):")
    for col in missing_in_source:
        print(f"  - {col} (will be NULL)")

# --- GET EXISTING SITECODES ---
existing_sitecodes = set()
for feat in database_layer.getFeatures():
    sc = feat["sitecode"]
    if sc:
        existing_sitecodes.add(str(sc))

print(f"\nDatabase has {len(existing_sitecodes)} existing sitecodes")

# --- DETECT NOT NULL CONSTRAINTS ---
not_null_cols = set()
for field in database_layer.fields():
    constraints = field.constraints()
    if constraints.constraints() & QgsFieldConstraints.ConstraintNotNull:
        name = field.name()
        if name.lower() not in skip_cols and name.lower() != "geom":
            not_null_cols.add(name)

if not_null_cols:
    print(f"\nDatabase NOT NULL columns: {', '.join(sorted(not_null_cols))}")
else:
    print("\n(No NOT NULL constraints detected via provider — constraints may still exist at DB level)")

# NOT NULL columns completely absent from source (will always be NULL on insert)
not_null_missing = not_null_cols & set(missing_in_source)
# NOT NULL columns present in source (may have occasional NULLs)
not_null_matched = {col for col in not_null_cols if col in matching}

# --- VALIDATE SOURCE DATA ---
print("\n=== Pre-flight Validation ===")
source_sitecodes_count = {}
no_sitecode_rows = []
null_violations = {}  # sitecode -> [col_names with NULL]

for i, src_feat in enumerate(source_layer.getFeatures()):
    sitecode = None
    for fname in ["sitecode", "Sitecode", "site_code", "Site_Code", "SiteCode"]:
        if fname in source_fields.values():
            val = src_feat[fname]
            if val is not None and not (isinstance(val, QVariant) and val.isNull()):
                sitecode = str(val)
            break
    if not sitecode:
        no_sitecode_rows.append(i)
        continue

    source_sitecodes_count[sitecode] = source_sitecodes_count.get(sitecode, 0) + 1

    # Check NULL violations for ALL features (INSERT and UPDATE)
    is_insert = sitecode not in existing_sitecodes
    violations = []
    for col in not_null_matched:
        src_name = matching[col]
        val = src_feat[src_name]
        if val is None or (isinstance(val, QVariant) and val.isNull()):
            violations.append(col)
    # For inserts, also flag NOT NULL columns missing from source entirely
    if is_insert:
        for col in not_null_missing:
            if col not in violations:
                violations.append(col)
    if violations:
        null_violations[sitecode] = list(set(violations))

    if (i + 1) % 200 == 0:
        QgsApplication.processEvents()

dup_sitecodes = {sc: cnt for sc, cnt in source_sitecodes_count.items() if cnt > 1}

# Summarize NOT NULL issues by column
problematic_cols = {}
for sc, cols in null_violations.items():
    for col in cols:
        problematic_cols[col] = problematic_cols.get(col, 0) + 1

has_issues = bool(null_violations) or bool(dup_sitecodes) or bool(no_sitecode_rows)

if has_issues:
    if null_violations:
        print(f"\n\u26a0 NOT NULL violations ({len(null_violations)} features):")
        for col, count in sorted(problematic_cols.items(), key=lambda x: -x[1]):
            tag = "(MISSING from source)" if col in not_null_missing else "(in source)"
            print(f"  Column '{col}' {tag}: {count} features with NULL")
        print("  Sample sitecodes:")
        for sc in list(null_violations.keys())[:5]:
            print(f"    {sc}: NULL in {', '.join(null_violations[sc])}")
    if dup_sitecodes:
        print(f"\n\u26a0 Duplicate sitecodes in source ({len(dup_sitecodes)}):")
        for sc, cnt in sorted(dup_sitecodes.items())[:10]:
            print(f"  {sc}: appears {cnt} times")
    if no_sitecode_rows:
        print(f"\n\u26a0 {len(no_sitecode_rows)} rows have no sitecode (will be skipped)")
else:
    print("\u2713 Pre-flight validation passed")

# --- ASK USER FOR DEFAULTS (NOT NULL columns) ---
not_null_defaults = {}
skip_null_sitecodes = set()

if problematic_cols:
    dlg = QDialog()
    dlg.setWindowTitle("Required Columns Need Values")
    main_layout = QVBoxLayout()

    info = QLabel(
        f"{len(null_violations)} features have NULL values in required "
        f"(NOT NULL) database columns.\n\n"
        "Review affected sitecodes below, then enter default values to fix.\n"
        "Leave a field blank to SKIP features with NULL in that column.\n"
    )
    info.setWordWrap(True)
    main_layout.addWidget(info)

    # Show affected sitecodes
    detail_lines = []
    for sc, cols in sorted(null_violations.items()):
        detail_lines.append(f"  {sc}: NULL in {', '.join(cols)}")
    detail_text = QTextEdit()
    detail_text.setReadOnly(True)
    detail_text.setPlainText("\n".join(detail_lines[:200]))
    if len(detail_lines) > 200:
        detail_text.append(f"\n... and {len(detail_lines) - 200} more")
    detail_text.setMaximumHeight(150)
    main_layout.addWidget(QLabel("Affected features:"))
    main_layout.addWidget(detail_text)

    main_layout.addWidget(QLabel("\nSet default values:"))
    form = QFormLayout()
    inputs = {}
    for col in sorted(problematic_cols.keys()):
        count = problematic_cols[col]
        label = col
        if col in not_null_missing:
            label += " (not in source)"
        line_edit = QLineEdit()
        line_edit.setPlaceholderText(f"{count} features affected — leave blank to skip them")
        form.addRow(label + ":", line_edit)
        inputs[col] = line_edit
    main_layout.addLayout(form)

    btn_box = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
    btn_box.accepted.connect(dlg.accept)
    btn_box.rejected.connect(dlg.reject)
    main_layout.addWidget(btn_box)

    dlg.setLayout(main_layout)
    dlg.setMinimumWidth(500)

    if dlg.exec_() != QDialog.Accepted:
        raise Exception("User cancelled to fix data")

    for col, line_edit in inputs.items():
        val = line_edit.text().strip()
        if val:
            not_null_defaults[col] = val
            print(f"  Default for '{col}': '{val}'")

    # Mark sitecodes to skip (violations in columns that have no default)
    for sc, cols in null_violations.items():
        unresolved = [c for c in cols if c not in not_null_defaults]
        if unresolved:
            skip_null_sitecodes.add(sc)
    if skip_null_sitecodes:
        print(f"  Skipping {len(skip_null_sitecodes)} features with unresolved NOT NULL violations")

elif dup_sitecodes:
    dup_detail = "\n".join(f"  {sc}: appears {cnt} times"
                           for sc, cnt in sorted(dup_sitecodes.items())[:100])
    dlg = QDialog()
    dlg.setWindowTitle("Duplicate Sitecodes Found")
    layout = QVBoxLayout()
    layout.addWidget(QLabel(
        f"{len(dup_sitecodes)} sitecodes appear multiple times in your source.\n"
        "The first occurrence will be INSERTED, subsequent ones will UPDATE it.\n\n"
        "Review the duplicates below. Cancel to go fix your data first."
    ))
    txt = QTextEdit()
    txt.setReadOnly(True)
    txt.setPlainText(dup_detail)
    txt.setMinimumHeight(200)
    layout.addWidget(txt)
    dup_btn = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
    dup_btn.accepted.connect(dlg.accept)
    dup_btn.rejected.connect(dlg.reject)
    layout.addWidget(dup_btn)
    dlg.setLayout(layout)
    dlg.setMinimumWidth(500)
    if dlg.exec_() != QDialog.Accepted:
        raise Exception("User cancelled to fix duplicate sitecodes")

# --- COORDINATE TRANSFORM ---
transform = None
if source_layer.crs() != database_layer.crs():
    transform = QgsCoordinateTransform(
        source_layer.crs(), database_layer.crs(), project
    )
    print(f"CRS transform: {source_layer.crs().authid()} -> {database_layer.crs().authid()}")

# --- SYNC LOOP (with retry on commit failures) ---
original_existing_sitecodes = set(existing_sitecodes)
retry_attempted_cols = set()

while True:
    existing_sitecodes = set(original_existing_sitecodes)

    total = source_layer.featureCount()
    print(f"\nSyncing {total} features (upsert mode)...")
    print("  New features → insert, existing → update")
    if not_null_defaults:
        print(f"  Applying defaults: {not_null_defaults}")
    if skip_null_sitecodes:
        print(f"  Skipping {len(skip_null_sitecodes)} features with NOT NULL violations")

    database_layer.startEditing()

    progress = QProgressDialog(
        "Syncing features to database...", "Cancel", 0, total, iface.mainWindow()
    )
    progress.setWindowModality(Qt.WindowModal)
    progress.setMinimumDuration(500)
    progress.setWindowTitle("Sync Progress")

    inserted = 0
    updated = 0
    skipped = 0
    errors = 0
    error_msgs = []
    cancelled = False

    for i, src_feat in enumerate(source_layer.getFeatures()):
        progress.setValue(i)
        progress.setLabelText(
            f"Processing feature {i + 1} / {total}\n"
            f"Inserted: {inserted}  Updated: {updated}  Skipped: {skipped}"
        )
        if progress.wasCanceled():
            cancelled = True
            break

        # Get sitecode from source
        sitecode = None
        for fname in ["sitecode", "Sitecode", "site_code", "Site_Code", "SiteCode"]:
            if fname in source_fields.values():
                val = src_feat[fname]
                if val is not None and not (isinstance(val, QVariant) and val.isNull()):
                    sitecode = str(val)
                break

        if not sitecode:
            skipped += 1
            if len(error_msgs) < 20:
                error_msgs.append(f"Row {i}: No sitecode found — skipped")
            continue

        # Skip features with unresolved NOT NULL violations
        if sitecode in skip_null_sitecodes:
            skipped += 1
            continue

        # Check if this sitecode already exists (in DB or freshly inserted)
        if sitecode in existing_sitecodes:
            # UPDATE existing feature
            found = False
            expr = '"sitecode" = ' + QgsExpression.quotedValue(sitecode)
            for tgt_feat in database_layer.getFeatures(expr):
                for tgt_name, src_name in matching.items():
                    idx = database_layer.fields().indexOf(tgt_name)
                    if idx >= 0 and tgt_name != "sitecode":
                        val = src_feat[src_name]
                        if (val is None or (isinstance(val, QVariant) and val.isNull())) and tgt_name in not_null_defaults:
                            val = not_null_defaults[tgt_name]
                        tgt_feat.setAttribute(idx, val)

                # Update geometry
                geom = QgsGeometry(src_feat.geometry())
                if transform:
                    geom.transform(transform)
                tgt_feat.setGeometry(geom)

                ok = database_layer.updateFeature(tgt_feat)
                if ok:
                    updated += 1
                    found = True
                else:
                    errors += 1
                    if len(error_msgs) < 20:
                        error_msgs.append(f"Row {i}: UPDATE failed for sitecode={sitecode}")
                break

            if not found:
                errors += 1
                if len(error_msgs) < 20:
                    error_msgs.append(f"Row {i}: Sitecode '{sitecode}' expected in DB but not found by query")
        else:
            # INSERT new feature
            new_feat = QgsFeature(database_layer.fields())

            geom = QgsGeometry(src_feat.geometry())
            if transform:
                geom.transform(transform)
            new_feat.setGeometry(geom)

            for tgt_name, src_name in matching.items():
                idx = database_layer.fields().indexOf(tgt_name)
                if idx >= 0:
                    val = src_feat[src_name]
                    if (val is None or (isinstance(val, QVariant) and val.isNull())) and tgt_name in not_null_defaults:
                        val = not_null_defaults[tgt_name]
                    new_feat.setAttribute(idx, val)

            # Set defaults for NOT NULL columns missing from source entirely
            for col, default_val in not_null_defaults.items():
                if col not in matching:
                    idx = database_layer.fields().indexOf(col)
                    if idx >= 0:
                        new_feat.setAttribute(idx, default_val)

            ok = database_layer.addFeature(new_feat)
            if ok:
                inserted += 1
                existing_sitecodes.add(sitecode)
            else:
                errors += 1
                if len(error_msgs) < 20:
                    error_msgs.append(f"Row {i}: INSERT failed for sitecode={sitecode}")

        if (i + 1) % 500 == 0:
            print(f"  Progress: {i + 1}/{total} "
                  f"(ins: {inserted}, upd: {updated}, skip: {skipped})")

    progress.setValue(total)
    progress.close()

    if cancelled:
        database_layer.rollBack()
        print("✗ Cancelled by user.")
        iface.messageBar().pushWarning("Cancelled", "Sync was cancelled. No changes saved.")
        break

    # --- RESULTS ---
    print(f"\nSync complete:")
    print(f"  Inserted: {inserted}")
    print(f"  Updated: {updated}")
    if skipped:
        print(f"  Skipped: {skipped}")
    if errors:
        print(f"  Errors: {errors}")

    if error_msgs:
        print("\nFirst errors:")
        for msg in error_msgs:
            print(f"  {msg}")

    # --- SAVE WITH CONFIRMATION ---
    summary = (
        f"  {inserted} new features\n"
        f"  {updated} updated features"
    )
    if skipped:
        summary += f"\n  {skipped} skipped"
    if errors:
        summary += f"\n  {errors} errors"

    reply = QMessageBox.question(
        None, "Save Changes?",
        f"Sync Summary:\n{summary}\n\nSave to database?",
        QMessageBox.Yes | QMessageBox.No
    )

    if reply != QMessageBox.Yes:
        database_layer.rollBack()
        print("✗ Changes rolled back.")
        break

    # --- TRY TO COMMIT ---
    if database_layer.commitChanges():
        print(f"\n✓ SUCCESS!")
        print(f"  Inserted: {inserted}")
        print(f"  Updated: {updated}")
        if skipped:
            print(f"  Skipped: {skipped}")
        print(f"  Total rows now: {database_layer.featureCount()}")
        iface.messageBar().pushSuccess("Done", f"{inserted} inserted + {updated} updated!")
        break

    # --- COMMIT FAILED — TRY TO HELP USER FIX IT ---
    errs = database_layer.commitErrors()
    err_text = "\n".join(errs[:15]) if errs else "Unknown error"
    print(f"\n✗ COMMIT FAILED:\n{err_text}")
    database_layer.rollBack()

    err_joined = " ".join(errs) if errs else ""

    # --- PARSE NOT NULL ERRORS ---
    null_match = re.search(
        r'null value in column "(\w+)"', err_joined, re.IGNORECASE
    )
    if null_match:
        col_name = null_match.group(1)
        print(f"  Detected NOT NULL violation on column: '{col_name}'")

        if col_name in retry_attempted_cols:
            QMessageBox.critical(
                None, "Cannot Fix Automatically",
                f"Column '{col_name}' still has NULL values even after "
                f"applying the default.\n\n"
                f"This may be a data type mismatch or a different issue.\n\n"
                f"Error:\n{err_text}"
            )
            iface.messageBar().pushCritical("Error", "Commit failed! Changes rolled back.")
            break

        # Find affected sitecodes for this column
        affected = []
        for sc, cols in null_violations.items():
            if col_name in cols:
                affected.append(sc)

        # Build a helpful dialog
        fix_dlg = QDialog()
        fix_dlg.setWindowTitle(f"Fix Required: '{col_name}' cannot be NULL")
        fix_layout = QVBoxLayout()

        fix_layout.addWidget(QLabel(
            f"The database rejected the sync because column '{col_name}' "
            f"has NULL values\nbut the database requires a value.\n\n"
            f"Enter a default value for '{col_name}' to fill all NULLs, "
            f"then the sync will retry automatically."
        ))

        if affected:
            fix_layout.addWidget(QLabel(f"\nAffected sitecodes ({len(affected)}):"))
            affected_txt = QTextEdit()
            affected_txt.setReadOnly(True)
            affected_txt.setPlainText("\n".join(affected[:200]))
            if len(affected) > 200:
                affected_txt.append(f"\n... and {len(affected) - 200} more")
            affected_txt.setMaximumHeight(120)
            fix_layout.addWidget(affected_txt)

        fix_layout.addWidget(QLabel(f"\nDefault value for '{col_name}':"))
        fix_input = QLineEdit()
        fix_input.setPlaceholderText(f"Enter a value for {col_name}")
        fix_layout.addWidget(fix_input)

        fix_btns = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        fix_btns.button(QDialogButtonBox.Ok).setText("Retry with this default")
        fix_btns.button(QDialogButtonBox.Cancel).setText("Give up")
        fix_btns.accepted.connect(fix_dlg.accept)
        fix_btns.rejected.connect(fix_dlg.reject)
        fix_layout.addWidget(fix_btns)

        fix_dlg.setLayout(fix_layout)
        fix_dlg.setMinimumWidth(500)

        if fix_dlg.exec_() == QDialog.Accepted and fix_input.text().strip():
            default_val = fix_input.text().strip()
            not_null_defaults[col_name] = default_val
            retry_attempted_cols.add(col_name)
            print(f"  Retrying with default '{col_name}' = '{default_val}'")
            continue  # RETRY the whole upsert+commit
        else:
            QMessageBox.information(
                None, "Sync Aborted",
                f"Changes have been rolled back. No data was saved.\n\n"
                f"To fix this manually:\n"
                f"1. Open your source layer's attribute table\n"
                f"2. Find rows where '{col_name}' is empty/NULL\n"
                f"3. Fill in the values\n"
                f"4. Re-run this script"
            )
            iface.messageBar().pushCritical("Error", "Commit failed! Changes rolled back.")
            break

    # --- PARSE DUPLICATE KEY ERRORS ---
    dup_match = re.search(
        r'duplicate key.*Key \(sitecode\)=\(([^)]+)\)', err_joined, re.IGNORECASE
    )
    if dup_match:
        dup_sc = dup_match.group(1)
        print(f"  Detected duplicate key for sitecode: '{dup_sc}'")
        original_existing_sitecodes.add(dup_sc)

        reply2 = QMessageBox.question(
            None, "Duplicate Sitecode — Retry?",
            f"Sitecode '{dup_sc}' already exists in the database but was\n"
            f"not detected in the initial scan.\n\n"
            f"This can happen if the database layer wasn't fully loaded.\n\n"
            f"Retry? (The sitecode will be UPDATED instead of inserted.)",
            QMessageBox.Yes | QMessageBox.No
        )
        if reply2 == QMessageBox.Yes:
            print(f"  Retrying with sitecode '{dup_sc}' marked as existing")
            continue  # RETRY
        else:
            iface.messageBar().pushCritical("Error", "Commit failed! Changes rolled back.")
            break

    # --- UNKNOWN ERROR ---
    QMessageBox.critical(
        None, "Commit Failed",
        f"Database rejected the changes. They have been rolled back.\n\n"
        f"Error:\n{err_text}"
    )
    iface.messageBar().pushCritical("Error", "Commit failed! Changes rolled back.")
    break

# --- FINAL STATUS ---
print("\n" + "=" * 60)
print("SYNC SCRIPT FINISHED")
print("=" * 60)
