# CI-safe helper utilities for parsing/evaluation and plotting inspection.

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

# Evaluate a text expression safely.
# - If the text can be parsed to an R expression, evaluate and return it.
# - If parsing fails, attempt to treat it as a simple numeric vector like "1,2,3" or "c(1,2)".
# - Otherwise return the original text (or NULL for empty input).
safe_eval_text <- function(txt, envir = parent.frame()) {
  if (is.null(txt)) return(NULL)

  # If already an expression or other type, return as-is
  if (is.expression(txt) || is.call(txt) || is.numeric(txt) || is.logical(txt)) {
    return(txt)
  }

  # Try to parse first
  expr <- safe_parse_first(txt)
  if (!is.null(expr)) {
    return(tryCatch(eval(expr, envir = envir), error = function(e) {
      # if eval fails, try fallback below
      NULL
    }))
  }

  # Fallback: try simple numeric vector parsing (comma-separated values)
  txt_chr <- as.character(txt)
  # Remove surrounding c( ) if present
  txt_stripped <- gsub("^\\s*c\\s*\\((.*)\\)\\s*$", "\\1", txt_chr)
  parts <- strsplit(txt_stripped, ",")[[1]]
  parts_trim <- trimws(parts)
  nums <- suppressWarnings(as.numeric(parts_trim))
  if (length(nums) > 0 && all(!is.na(nums))) return(nums)

  # Last fallback: return the string itself
  txt_chr
}

# Wrapper around shapiro.test that avoids errors when the input is constant or too short.
# Returns either a shapiro.test result (htest) or an htest-like object with NA p.value.
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
# otherwise falls back to ggplot2::ggplot_build-based extraction to be compatible
# across ggplot2 versions and scale types (e.g. ScaleContinuousDatetime).
safe_get_axis_info <- function(plot) {
  out <- tryCatch(getAxisInfo(plot), error = function(e) NULL)
  if (!is.null(out)) return(out)

  b <- tryCatch(ggplot2::ggplot_build(plot), error = function(e) NULL)
  if (is.null(b)) return(NULL)

  panel_params <- NULL
  if (!is.null(b$layout) && length(b$layout$panel_params) >= 1) {
    panel_params <- b$layout$panel_params[[1]]
  } else if (!is.null(b$layout$panel_scales_x) && length(b$layout$panel_scales_x) >= 1) {
    panel_params <- b$layout$panel_scales_x[[1]]
  }

  breaks <- NULL
  labels <- NULL
  if (!is.null(panel_params)) {
    breaks <- tryCatch({
      if (!is.null(panel_params$x) && is.function(panel_params$x$get_breaks))
        panel_params$x$get_breaks()
      else if (!is.null(panel_params$breaks))
        panel_params$breaks
      else NULL
    }, error = function(e) NULL)

    labels <- tryCatch({
      if (!is.null(panel_params$x) && is.function(panel_params$x$get_labels) && !is.null(breaks))
        panel_params$x$get_labels(breaks)
      else if (!is.null(panel_params$labels))
        panel_params$labels
      else NULL
    }, error = function(e) NULL)
  }

  list(breaks = breaks, labels = labels, panel_params = panel_params)
}
