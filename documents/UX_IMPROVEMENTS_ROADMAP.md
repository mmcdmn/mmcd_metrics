# UX Improvements Roadmap

This document outlines user experience improvements for the MMCD Metrics Dashboard, prioritized by implementation difficulty and user impact.

## Overview Dashboard UX Improvements

### 1. Loading Skeleton (Priority: HIGH - Easy/High Impact)

**Current Issue**: When data loads, users see empty space or loading spinners without context.

**Solution**: Show grey placeholder boxes that match the final layout while data loads.

**Implementation Details**:
- **Files to modify**: 
  - `apps/overview/dynamic_ui.R` - Add skeleton CSS and placeholder HTML
  - `apps/overview/dynamic_server.R` - Show skeleton on initial load, hide when data ready
- **Technical approach**:
  - CSS animations for subtle "shimmer" effect on placeholder boxes
  - Skeleton boxes match final value box dimensions and layout
  - Replace skeleton with real data when `current_data()` reactive updates
- **Code changes**:
  - Add `.skeleton-box` CSS with shimmer animation
  - Add `renderUI` for skeleton that shows initially
  - Use `conditionalPanel` to switch between skeleton and real data
- **Testing**: Verify skeleton appears on page load, disappears when data loads

### 2. Error Boundaries (Priority: HIGH - Easy/High Impact)

**Current Issue**: When database errors occur, users see grey screens or blank panels.

**Solution**: Show user-friendly error messages with retry options.

**Implementation Details**:
- **Files to modify**:
  - `apps/overview/dynamic_server.R` - Wrap reactive expressions in `tryCatch`
  - `apps/overview/dynamic_ui.R` - Add error message styling
- **Technical approach**:
  - Create `create_error_panel()` helper function
  - Add error state to main data reactives
  - Show error panel with retry button instead of empty content
- **Error scenarios to handle**:
  - Database connection failures
  - Query timeouts
  - Invalid date ranges
  - Missing historical data
- **Testing**: Simulate DB errors, verify friendly error messages appear

### 3. Offline Indicator (Priority: MEDIUM - Easy/Medium Impact)

**Current Issue**: No indication when database connectivity issues occur.

**Solution**: Show warning banner when DB connection fails.

**Implementation Details**:
- **Files to modify**:
  - `shared/db_helpers.R` - Add connection status tracking
  - `apps/overview/dynamic_ui.R` - Add offline indicator UI
  - `apps/overview/dynamic_server.R` - Monitor connection status
- **Technical approach**:
  - Track last successful DB query timestamp
  - Show amber warning banner if no successful queries in 30+ seconds
  - Auto-hide banner when connection restored
- **Design**: Fixed position amber banner at top: "⚠ Database connection issue. Data may be outdated."

### 4. Keyboard Shortcuts (Priority: MEDIUM - Medium/Medium Impact)

**Current Issue**: Users must click UI elements for common actions.

**Solution**: Add keyboard shortcuts for frequently used actions.

**Implementation Details**:
- **Files to modify**:
  - `apps/overview/dynamic_ui.R` - Add JavaScript keydown handler
- **Shortcuts to implement**:
  - `R` = Trigger refresh button
  - `Esc` = Close all open chart panels and detail sections
  - `1-9` = Jump to metric category (if multiple categories)
  - `D` = Toggle district/facilities view (if applicable)
- **Technical approach**:
  - Add `$(document).on('keydown')` handler
  - Prevent default for shortcuts that might conflict
  - Show keyboard shortcut hints on first visit (dismissible tooltip)
- **Accessibility**: Ensure shortcuts work with screen readers

### 5. Remembering Preferences (Priority: LOW - Medium/Medium Impact)

**Current Issue**: Users lose their preferred settings (date, zone) on page refresh.

**Solution**: Store user preferences in browser localStorage.

**Implementation Details**:
- **Files to modify**:
  - `apps/overview/dynamic_ui.R` - Add localStorage JavaScript helpers
  - `apps/overview/dynamic_server.R` - Load saved preferences on startup
- **Preferences to save**:
  - Last used custom date
  - Preferred zone filter (P1, P2, or both)
  - Expiring days threshold
  - Preferred overview type (district/facilities)
- **Technical approach**:
  - Save to localStorage on input change (debounced)
  - Load from localStorage on page load
  - Provide "Reset to defaults" option
- **Privacy**: Only store functional preferences, no personal data

## Implementation Priority Order

1. **Phase 1** (This Sprint):
   - ✅ Performance optimizations (completed)
   - 🔄 Loading skeleton
   - 🔄 Error boundaries

2. **Phase 2** (Next Sprint):
   - 📋 Offline indicator
   - 📋 Keyboard shortcuts

3. **Phase 3** (Future):
   - 📋 Remembering preferences
   - 📋 Additional accessibility improvements

## Success Metrics

- **Loading skeleton**: Perceived load time improvement (user feedback)
- **Error boundaries**: Reduction in user-reported "blank screen" issues
- **Offline indicator**: Faster issue resolution for connectivity problems
- **Keyboard shortcuts**: Power user adoption rate
- **Remembering preferences**: Reduced friction for repeat users

## Implementation Notes

### CSS Framework
The dashboard uses Bootstrap 4 classes. Skeleton styles should integrate with existing `.stat-box` and `.fluidRow` layouts.

### JavaScript Dependencies
- jQuery (already loaded by Shiny)
- Shiny custom message handlers
- No external libraries required

### Testing Strategy
- **Manual testing**: Load dashboard with throttled network
- **Error simulation**: Mock DB failures in test environment
- **Cross-browser**: Test in Chrome, Firefox, Safari
- **Accessibility**: Test with keyboard navigation and screen reader

### Rollback Plan
Each improvement should be feature-flagged or easily revertible:
- CSS changes: Wrap in feature classes that can be disabled
- JavaScript: Conditional loading based on configuration
- Server logic: Graceful fallbacks to existing behavior