# Select a complete calculation input without changing the prepared observation grid.
#' @export
.tsCompleteSeries <- function(y, missingValues = "omit") {
  # Older saved analyses may not yet contain the option. Use the current default.
  if (is.null(missingValues)) missingValues <- "omit"
  if (length(missingValues) != 1L || is.na(missingValues) ||
      !missingValues %in% c("omit", "error", "trim", "longest"))
    stop("Unknown missing-observation handling option.")

  y <- as.numeric(y)
  result <- list(y = numeric(), indices = integer(), error = NULL, note = NULL)
  if (any(is.infinite(y))) {
    result$error <- gettext("The series contains infinite values. Resolve these values before calculating this output.")
    return(result)
  }
  observed <- which(!is.na(y))
  if (!length(observed)) {
    result$error <- gettext("The selected series contains no observed values.")
    return(result)
  }

  indices <- seq_along(y)
  if (anyNA(y)) {
    # Retain strict handling for explicit values saved by earlier development builds.
    if (missingValues == "error") {
      result$error <- gettext("The series used for this calculation contains missing observations. Choose Omit all missing observations or Use the longest complete segment under Missing observations.")
      return(result)
    }
    if (missingValues == "omit") {
      indices <- observed
    } else if (missingValues == "trim") {
      indices <- seq.int(observed[1L], tail(observed, 1L))
      if (anyNA(y[indices])) {
        result$error <- gettext("Missing observations remain inside the series after omitting those at the ends. Choose Use the longest complete segment, or use Filter by to select a continuous range without missing observations.")
        return(result)
      }
    } else {
      runs <- rle(!is.na(y))
      ends <- cumsum(runs$lengths)
      complete <- which(runs$values)
      # which.max resolves equal-length segments in favor of the earliest one.
      chosen <- complete[which.max(runs$lengths[complete])]
      indices <- seq.int(ends[chosen] - runs$lengths[chosen] + 1L, ends[chosen])
    }
  }

  if (length(indices) < 2L) {
    result$error <- gettext("The selected observations contain fewer than two values. Select a range with more observed values.")
    return(result)
  }
  values <- y[indices]
  if (all(values == values[1L])) {
    result$error <- gettext("The selected observations contain no variation. This calculation requires varying observations.")
    return(result)
  }
  result$y <- values
  result$indices <- indices
  if (length(indices) != length(y)) {
    result$note <- if (missingValues == "omit")
      gettextf("Missing observations were omitted for this calculation (%1$s of %2$s observations retained), so the remaining observations are treated as consecutive time steps and any internal gaps are ignored.",
               length(indices), length(y))
    else
      gettextf("This calculation uses %1$s of %2$s observation steps: steps %3$s to %4$s in analysis order. The data and saved values are unchanged.",
               length(indices), length(y), indices[1L], tail(indices, 1L))
  }
  result
}
