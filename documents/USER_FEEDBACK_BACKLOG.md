# User Feedback Backlog

This document tracks user feedback and feature requests that have not yet been implemented across the various metrics applications.

**Last Updated:** December 17, 2025

---

## Cattail Treatments

### Data Validation Issues
- **Date:** 5/8/2024
- **Issue:** North facility showed 282 sites treated and 143 inspected
- **Status:** Pending Investigation
- **Notes:** Numbers might be accurate but seemed suspicious. Need to validate data accuracy and potentially add alerts for unusual data patterns.

---

## Cattail Inspections

### Graph Interactivity
- **Issue:** Progress vs goal graphs don't support hover to get values
- **Requested Enhancement:** Add hover tooltips with percentages and facility filters
- **Priority:** Medium
- **Status:** Not Started

### Historical Comparison - UI Clarity
- **Issue:** Not clear that "Sites table view" specifically changes table content and doesn't interact with the graph
- **Requested Enhancement:** Improve labeling/instructions to clarify the relationship between table view selector and graph
- **Priority:** Low
- **Status:** Not Started

### Site Sorting Functionality
- **Issue:** Previously, sorting by number of sites showed 1 for each facility per site type. This option no longer available
- **Requested Enhancement:** Restore ability to sort by inspection/treatment type
- **Priority:** Medium
- **Status:** Not Started

### Acres Display Enhancement
- **Issue:** Acres data looks accurate but missing priority filters
- **Requested Enhancement:** Add P1 and P2 toggle filters for acres display
- **Priority:** Medium
- **Status:** Not Started

---

## SUCO (Surveillance and Unusual Capture Occurrences)

### Date Navigation UI Issues
- **Issue:** Date arrow is darker on one side, showing numbers like "20360" on light side and "20240" on dark side - unclear meaning
- **Requested Enhancement:** Clarify date navigation controls and what the numbers represent (likely years)
- **Priority:** High
- **Status:** Not Started

### Hover Information Enhancement
- **Issue:** When hovering, can see info but when multiple SUCOs share the same date, hard to tell if data is lagging or correct
- **Requested Enhancement:** Include trap number in hover tooltip
- **Priority:** High
- **Status:** Not Started

### Menu Visibility
- **Issue:** Menu for changing "number of SUCOs" to "number of something caught" is too hidden
- **Requested Enhancement:** Make this menu more prominent/discoverable
- **Priority:** Medium
- **Status:** Not Started

### Detailed Sample Tab Improvements
- **Issue:** Tab cuts off at species; unclear what first number means
- **Positive Feedback:** Users like that it gives species list, not just species count
- **Requested Enhancement:** 
  - Fix cutoff issue
  - Clarify what first number represents
- **Priority:** Medium
- **Status:** Not Started

### Summary Table Enhancement
- **Issue:** Summary table only shows species count
- **Requested Enhancement:** Add species found (species list) in addition to count
- **Priority:** Medium
- **Status:** Not Started

### Map Visualization Issues
- **Issue:** Samples are too close together/overlapping on map
- **Requested Enhancement:** 
  - Improve spacing/clustering of map points
  - Add legend/key showing how dot size relates to count
- **Priority:** High
- **Status:** Not Started

### Data Range Selection Improvements
- **Issue:** 
  - Unclear what "current data vs current + archive" means
  - Need easier date range picker
  - Want to quickly select specific years
- **Requested Enhancement:** 
  - Clarify terminology for data vs archive
  - Add quick-select options for year ranges while keeping specific date range picker
- **Priority:** High
- **Status:** Not Started

---

## Inspections (Prehatch)

### Sample Tracking Enhancement
- **Issue:** During prehatch sample history review, cannot easily see which sites have been sampled and which samples have been ID'd
- **Use Case:** Throughout the season, samples can get mixed up, overlooked, or lost. Need quick visibility into sampling and identification status
- **Requested Enhancement:** Add indicator showing:
  - Whether site has been checked/sampled
  - Whether sample has been identified
  - Note: "Site sampled but not ID'd" currently doesn't show useful information
- **Priority:** High
- **Status:** Not Started

---

## Air Sites

### Percentage Display
- **Issue:** Would be helpful to have percent red calculation
- **Requested Enhancement:** 
  - Add "percent red" metric
  - Weekly view preferred to compare spring/summer breeding sites and edit as needed
- **Priority:** High
- **Status:** Not Started

### Historical Date Range Flexibility
- **Issue:** Historical section has limited date range options
- **Requested Enhancement:** Allow any specific date range a user wants to enter for all historical data (sites or acres per facility during specific time period)
- **Priority:** Medium
- **Status:** Not Started
- **Notes:** Need to assess implementation complexity

### Manual Override Functionality
- **Issue:** Need ability to manually override site status back to "Unknown"
- **Current Behavior Questions:**
  - If more rainfall occurs, will it automatically change inspected sites to unknown if within 7 days?
  - Is this difficult due to incorporating rainfall data?
- **Requested Enhancement:** 
  - Mass manual status change to "Unknown" (unless active treatment)
  - Trigger status at 1 inch or more rainfall within 48 hours
  - Ability to manually change per township
  - Sometimes staff "poke around" before full brood inspection starts
- **Priority:** High
- **Status:** Not Started
- **Notes:** This may simplify implementation - let users manually trigger unknown status rather than automatic rainfall integration

---

## Structure Status / Structure Treatments

### Section Progress by FOS
- **Feedback:** Being able to see section progress on FOS-level basis would be neat and potentially useful for some FOS
- **Caveat:** Not all structure treatments operate section-by-section
  - Sometimes target certain types depending on operation (targeted control for Cx. vs routine operations)
  - Difficult to extract applicable management-level meaning unless known underperformance/issues
  - Inventory management and Webster records may be more appropriate for this than district-wide metrics
- **Status:** Under Consideration
- **Notes:** Feature may have limited applicability

### Five and Ten Year Averages
- **Issue:** Questions about necessity in current live tracker setup
- **Notes:** User suggests this is more appropriate in Historical Data section (see Historical Structures comments)
- **Status:** Pending Review

### Functionality - General Feedback
- Overall demonstrates structure progress well at a glance
- Helps make structure surveillance and treatment processes more uniform across FOS and potentially district-wide
- Slider to adjust expiration window is useful and displays well
- Data appears to pull accurately from database

### Historical Comparison - Date Range Controls
- **Requested Enhancement:** 
  - Ability to set mandatory start date and optional end date to confine live data to that period
  - Would help with week-to-week or month-to-month structure work comparison
  - May also address year/week toggle checkbox needs
- **Priority:** Medium
- **Status:** Not Started

### Visualization Improvements
- **Issue:** Less notable than Historical Trends since everything is clearly labeled with FOS/Facility names, but could still improve clarity
- **Requested Enhancement:** 
  - Tweak visualizations for colorblind accessibility
  - While FOS use different hues of base Facility color (appreciated design choice), consider assigning patterns to maintain visual clarity
- **Priority:** Low
- **Status:** Not Started

---

## Additional Apps - Pending Feedback

### Catch Basin Status
- *No feedback recorded yet*

### Control Efficacy
- *No feedback recorded yet*

### Drone
- *No feedback recorded yet*

### Ground Prehatch Progress
- *No feedback recorded yet*

### Mosquito Surveillance Map
- *No feedback recorded yet*

### Mosquito Monitoring
- *No feedback recorded yet*

### Section Cards
- *No feedback recorded yet*

### Trap Surveillance Test
- *No feedback recorded yet*

---
