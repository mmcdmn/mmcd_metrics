# Updating Configuration Files

The MMCD Metrics Dashboard uses a centralized YAML configuration file (`config/app_config.yaml`) to manage cache TTLs, value box color thresholds, wiki links, and display settings. This guide explains exactly how to update this configuration using Git.

## Configuration File Structure

The configuration is stored in a single YAML file that controls:

- **Cache TTL Settings** - How long different data is cached (in seconds)
- **Value Box Color Thresholds** - Red/yellow/green boundaries (4 different modes: historical, fixed_pct, pct_of_average, capacity)
- **Wiki Links** - External reference links for each metric
- **Display Settings** - Default UI options (zone filter, expiring days threshold)
- **Runtime Settings** - Route TTL for load balancer, max workers

File location: `config/app_config.yaml`

## How to Update the Configuration

### Step 1: Start on the Dev Branch

Open your terminal and navigate to the project directory:

```bash
cd ~/mmcd_metrics
```

Switch to the `dev` branch (where all development work happens):

```bash
git checkout dev
```

Pull the latest changes to ensure you have the most recent configuration:

```bash
git pull origin dev
```

### Step 2: Open the Configuration File

Open `config/app_config.yaml` in your text editor:

```bash
nano config/app_config.yaml
```

Or use your preferred editor (vim, VS Code, etc.):

```bash
code config/app_config.yaml
```

### Step 3: Make Your Changes

Edit the YAML file to update whatever needs changing. Common updates:

**Example 1: Change Cache TTL for historical averages**
```yaml
# Find this section and update the value (in seconds)
cache:
  ttl:
    historical_averages: 1209600  # Change this value
```

**Example 2: Update a color threshold**
```yaml
# Find the metric you want to update
thresholds:
  fixed_pct:
    air_sites:
      good: 85      # Change these percentages
      warning: 70
      direction: "higher_is_better"
```

**Example 3: Add a wiki link**
```yaml
# Find the wiki_links section
wiki_links:
  catch_basin: "https://your-wiki.com/catch-basins"  # Add or update link
```

Save your changes in the editor (`Ctrl+S` for most editors).

### Step 4: View Your Changes

Check what you've modified:

```bash
git status
```

This shows you all changed files. You should see `config/app_config.yaml` listed.

View the exact changes you made:

```bash
git diff config/app_config.yaml
```

This shows the old values (with `-` prefix) and new values (with `+` prefix). Review to make sure everything is correct.

### Step 5: Stage Your Changes

Add the configuration file to the staging area (preparing it for commit):

```bash
git add config/app_config.yaml
```

Verify it's staged:

```bash
git status
```

You should see `config/app_config.yaml` under "Changes to be committed".

### Step 6: Commit Your Changes

Create a commit with a descriptive message. The message should briefly explain what changed:

```bash
git commit -m "UPDATE: Cache TTL for historical averages to 1209600 seconds"
```

Other example commit messages:

```bash
# If you updated color thresholds
git commit -m "UPDATE: Air sites color threshold good boundary to 85%"

# If you added/updated wiki links
git commit -m "UPDATE: Added wiki links for catch basin metrics"

# If you changed multiple things
git commit -m "UPDATE: Configuration adjustments - TTLs, thresholds, and wiki links"
```

### Step 7: Push to Dev Branch

Send your changes to the remote repository on the `dev` branch:

```bash
git push origin dev
```

After pushing, the output will show something like:
```
To https://github.com/mmcdmn/mmcd_metrics.git
   abc123..def456  dev -> dev
```

This confirms your changes are now on the dev branch.


### Step 8: Merge to Main (Production)

1. Go to GitHub: [https://github.com/mmcdmn/mmcd_metrics](https://github.com/mmcdmn/mmcd_metrics)

2. Click the **"Pull requests"** tab

3. Click **"New pull request"**

4. Set:
   - Base branch: `main` (the production branch)
   - Compare branch: `dev` (your development branch with changes)

5. Review the changes shown in the PR

6. Click **"Create pull request"**

7. Add a title and description. Example:
   ```
   Title: Update configuration - Cache TTLs and color thresholds
   
   Description:
   - Increased historical averages cache TTL from 604800 to 1209600 seconds
   - Updated air sites color thresholds
   - Added new wiki links for catch basin metrics
   ```

8. Click **"Create pull request"** to submit

9. Wait for any required tests to run

10. Click **"Merge pull request"** when ready

11. Confirm the merge

Your changes are now deployed to production and will be live after the container redeploys.

## Common Configuration Changes

### Changing Cache TTL

Find the `cache.ttl` section and update values. TTLs are in seconds:

```yaml
cache:
  ttl:
    historical_averages: 1209600    # 14 days
    lookup_tables: 1209600          # 14 days
    fos_drilldown: 604800           # 7 days
    color_mappings: 604800          # 7 days
    facility_historical: 86400      # 24 hours
    general: 300                    # 5 minutes
    db_queries: 120                 # 2 minutes
    charts: 120                     # 2 minutes
    stat_boxes: 120                 # 2 minutes
```

### Updating Color Thresholds

Color modes are used by different metrics:

**Historical Mode** (comparing to historical average):
```yaml
thresholds:
  historical:
    metric_name:
      good: 0.9              # Within 90% of average
      warning: 0.8           # Within 80% of average
      direction: "higher_is_better"  # or "lower_is_better"
```

**Fixed Percentage Mode** (fixed percentage thresholds):
```yaml
thresholds:
  fixed_pct:
    metric_name:
      good: 85              # >= 85%
      warning: 70           # >= 70%
      direction: "higher_is_better"
```

**Percentage of Average Mode** (relative to average):
```yaml
thresholds:
  pct_of_average:
    metric_name:
      good: 100             # 100% of average
      warning: 80           # 80% of average
```

**Capacity Mode** (for capacity-based metrics):
```yaml
thresholds:
  capacity:
    metric_name:
      at_capacity: 95       # >= 95% = alert
      near_capacity: 80     # >= 80% = warning
```

### Adding Wiki Links

Add URLs to external documentation:

```yaml
wiki_links:
  metric_id: "https://your-wiki-url/page"
  drone: "https://wiki.example.com/drone-treatment"
  catch_basin: "https://wiki.example.com/catch-basins"
```

## Troubleshooting

### "Files changed" shows unrelated changes

Make sure you only modified `config/app_config.yaml`:

```bash
git reset
git checkout config/app_config.yaml
git add config/app_config.yaml
```

Then commit again.

### YAML syntax error when deploying

YAML is whitespace-sensitive. Common issues:
- Use **spaces only** (not tabs) for indentation
- Indent with 2 spaces per level
- Colons must have a space after them (`key: value`)
- Strings with special characters need quotes (`"value"`)

Validate your YAML before committing using an online validator: https://www.yamllint.com/

### Changes don't appear in test app

1. Make sure you pushed: `git push origin dev`
2. Container needs to restart to reload config
3. Clear browser cache (Ctrl+Shift+Delete)
4. Refresh the app page

### Can't push to dev

```bash
# Make sure you have the latest
git pull origin dev

# If there are conflicts, resolve them (ask for help if unsure)

# Then try pushing again
git push origin dev
```

## Summary

The workflow for updating configuration is:

1. `git checkout dev` → Switch to dev branch
2. `git pull origin dev` → Get latest
3. Edit `config/app_config.yaml` → Make changes
4. `git diff config/app_config.yaml` → Review changes
5. `git add config/app_config.yaml` → Stage changes
6. `git commit -m "description"` → Commit changes
7. `git push origin dev` → Push to dev
8. Test in dev environment
9. Create Pull Request on GitHub to merge dev → main
10. Approve and merge PR to deploy to production

That's it! Your configuration changes are now live.
