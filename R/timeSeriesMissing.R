# Select a complete calculation input without changing the prepared observation grid.
#' @export
.tsCompleteSeries <- function(y, missingValues = "error") {
  # Older saved analyses may not yet contain the new option. Use its strict default.
  if (is.null(missingValues)) missingValues <- "error"
  if (length(missingValues) != 1L || is.na(missingValues) ||
      !missingValues %in% c("error", "trim", "longest"))
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
    if (missingValues == "error") {
      result$error <- gettext("The series used for this calculation contains missing observations. Choose a method under Missing observations. Missing values at the ends, including those introduced by differencing, can be handled with Omit missing observations at the ends.")
      return(result)
    }
    if (missingValues == "trim") {
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
    result$error <- gettext("The selected complete segment contains fewer than two observations. Select a longer complete range.")
    return(result)
  }
  values <- y[indices]
  if (all(values == values[1L])) {
    result$error <- gettext("The selected complete segment contains no variation. This calculation requires varying observations.")
    return(result)
  }
  result$y <- values
  result$indices <- indices
  if (length(indices) != length(y))
    result$note <- gettextf("This calculation uses %1$s of %2$s observation steps: steps %3$s to %4$s in analysis order. The data and saved values are unchanged.",
                           length(indices), length(y), indices[1L], tail(indices, 1L))
  result
}
