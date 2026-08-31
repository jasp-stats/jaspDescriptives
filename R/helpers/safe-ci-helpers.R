# Safe helpers to make CI tests robust to empty inputs and ggplot2 internals.

# Return the first parsed expression from text, or NULL if none/invalid.
safe_parse_first <- function(x) {
  if (is.null(x)) return(NULL)
  x_chr <- as.character(x)
  x_chr <- x_chr[!is.na(x_chr) & nzchar(x_chr)]
  if (length(x_chr) == 0) return(NULL)
  parsed <- tryCatch(parse(text = x_chr), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) == 0) return(NULL)
  parsed[[1]]
}

# Wrapper around shapiro.test that avoids errors when the input is constant or too short.
# Returns either a shapiro.test result or a consistent list with NA p-value.
safe_shapiro_test <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) <= 1) {
    structure(
      list(statistic = NA_real_,
           p.value   = NA_real_,
           method    = "Shapiro-Wilk (skipped)",
           data.name = deparse(substitute(x)),
           note      = "not enough variance or too few unique values"),
      class = "htest"
    )
  } else {
    stats::shapiro.test(x)
  }
}

# Robust axis extraction for ggplot objects. Tries getAxisInfo if available,
# otherwise falls back to ggplot_build-based extraction to be compatible
# across ggplot2 versions and scale types (e.g. ScaleContinuousDatetime).
safe_get_axis_info <- function(plot) {
  # Try existing helper (if package provides it)
  out <- tryCatch(getAxisInfo(plot), error = function(e) NULL)
  if (!is.null(out)) return(out)

  # Fallback: extract info from ggplot_build
  b <- tryCatch(ggplot2::ggplot_build(plot), error = function(e) NULL)
  if (is.null(b)) return(NULL)

  # Try to get panel params or scale entries in a few ways to support ggplot2 versions
  panel_params <- NULL
  if (!is.null(b$layout) && length(b$layout$panel_params) >= 1) {
    panel_params <- b$layout$panel_params[[1]]
  } else if (!is.null(b$layout$panel_scales_x) && length(b$layout$panel_scales_x) >= 1) {
    panel_params <- b$layout$panel_scales_x[[1]]
  }

  breaks <- NULL
  labels <- NULL
  if (!is.null(panel_params)) {
    # panel_params objects differ by ggplot2 version; attempt safe extraction
    breaks <- tryCatch({
      if (!is.null(panel_params$x) && is.function(panel_params$x$get_breaks)) {
        panel_params$x$get_breaks()
      } else if (!is.null(panel_params$breaks)) {
        panel_params$breaks
      } else NULL
    }, error = function(e) NULL)

    labels <- tryCatch({
      if (!is.null(panel_params$x) && is.function(panel_params$x$get_labels) && !is.null(breaks)) {
        panel_params$x$get_labels(breaks)
      } else if (!is.null(panel_params$labels)) {
        panel_params$labels
      } else NULL
    }, error = function(e) NULL)
  }

  list(breaks = breaks, labels = labels, panel_params = panel_params)
}
