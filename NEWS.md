# jaspDescriptives Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspDescriptives (development version)

* Invalid Plot Builder layer combinations now show an error on the affected plot only, so other plots and the plot layout still render. The reversed identity line again spans the data range.

* Fixed confidence-ellipse rank checks, Plot Builder upgrades for nested saved-variable formats, and sorting after switching an axis to scale.

* Plot Builder summarizes independent two-scale data with per-group mean/median centers (dots, crosshairs, or reference lines). Added separate mean SD, SEM, and Hotelling confidence ellipses. Position-based summaries remain available for categorical axes, single-axis plots, and repeated measures.

* Fixed Plot Builder marginal area-stack totals, count-stack axis limits under the JASP theme, and single-axis sum-stack labels. Removed spurious warnings from single-axis JASP frames and repeated-measures joint means.

* Plot Builder supports single-axis points, boxplots, violins, and summaries; categorical histogram/density ridges; grouped histogram layouts; and optional mean/median centers across both coordinates. Geometry availability and help now follow variable types.
* Plot Builder separates statistical grouping from coloring, provides explicit stack measures, and preserves shape mappings beyond six categories. Repeated-measures means and SEM/CI use subject averages within each plotted position, group, and panel, including margins; trajectories stay within other repeated-measures conditions.

* Fixed Plot Builder outline and summary-line colors, histogram count-density scaling, distribution eligibility, ellipse bounds, and negative annotation coordinates. Clarified opacity, facet ordering, missing-combination filling, and annotation spacing labels and help.

* Plot Builder now selects Grid or Rows and columns explicitly, retaining inactive assignments and migrating saved layouts.

* Plot Builder now offers explicit custom or group colors for P-value brackets, preserving group coloring in saved analyses.

* Replaced conflicting black/white outline checkboxes in Plot Builder with outline color dropdowns and made empty circles independent of outline color. Saved outline selections are migrated automatically.

* Fixed moving-average confidence bands in time-series autocorrelation plots to use only preceding positive lags. Showing zero lag no longer changes the bands at other lags.
* Time Series Descriptives autocorrelation plots offer two missing-observation choices: exclude cases listwise (default) or use the longest complete segment. Listwise exclusion adds a note explaining that retained observations are treated as consecutive steps and internal gaps are ignored; the data and other outputs are unchanged.
* Time Series Descriptives uses Time to order whole observations without adding rows for gaps. Unusable or ambiguous Time falls back to spreadsheet row order with a warning; numeric Time filters accept negative values and decimals.
* Added documentation for the boxplot jitter element in the helpfile ([PR 534](https://github.com/jasp-stats/jaspDescriptives/pull/534)).
* Added documentation for interval plots, dotplots, and pie charts in the helpfile ([PR 516](https://github.com/jasp-stats/jaspDescriptives/pull/516)).

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
