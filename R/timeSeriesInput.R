#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


# Opt-in preparation for analyses that treat Time as observation order only.
# raw contains every supplied row, ordered by usable Time before filtering;
# data contains the selected rows in that same order. Both have y, t, and
# optional xreg1, xreg2, ... columns, with original spreadsheet row names.
# Missing dependent/covariate values are retained; no gaps are inferred or filled.
# With no usable Time, t is the input row position and input order is retained.
# jaspResults may be NULL for data-only use. Visible notes describe this call;
# stale notes are removed even when the analysis is not ready or has no rows.
# Integer positions keep notes before other output: warnings, interpretation, ordering, ties.
# Existing exported Time helpers remain available with their legacy behavior.
.tsPrepareData <- function(jaspResults, dataset, options, ready, covariates = FALSE) {
  if (!is.null(jaspResults)) {
    jaspResults[["timeSortingNote"]] <- NULL
    jaspResults[["timeInterpretationNote"]] <- NULL
    jaspResults[["timeWarning"]] <- NULL
    jaspResults[["timeTiesNote"]] <- NULL
    jaspResults[["timeFilterWarning"]] <- NULL
  }

  if (!ready || is.null(options[["dependent"]]) || options[["dependent"]] == "" || nrow(dataset) == 0) {
    empty <- data.frame()
    return(list(raw = empty, data = empty))
  }

  interpreted <- list(t = seq_len(nrow(dataset)), status = "row")
  if (options[["time"]] != "")
    interpreted <- .tsInterpretTime(dataset[[options[["time"]]]])

  dat <- data.frame(y = dataset[[options[["dependent"]]]], t = interpreted$t)
  if (covariates && length(options[["covariates"]]) > 0) {
    cdat <- dataset[options[["covariates"]]]
    names(cdat) <- paste0("xreg", seq_along(options[["covariates"]]))
    dat <- cbind(dat, cdat)
  }
  # Row identity is independent of Time, including when timestamps are tied.
  attr(dat, "row.names") <- attr(dataset, "row.names")

  if (interpreted$status %in% c("invalid", "ambiguous")) {
    attr(dat, "timeFallback") <- TRUE
    if (!is.null(jaspResults)) {
      text <- if (identical(interpreted$reason, "yearMonth"))
        gettext("Warning: Time could represent decimal numbers or year.month dates, giving conflicting orders or ties. Time was ignored, and the analysis used spreadsheet row order. Use a numeric (Scale) variable for decimal indices, or dates in year-month-day order (YYYY-MM-DD). Leave Time empty to use spreadsheet row order.")
      else if (identical(interpreted$reason, "missing"))
        gettext("Warning: Time contains missing or non-finite values. Time was ignored, and the analysis used spreadsheet row order. Select a complete numeric variable or one containing complete, unambiguous dates, or leave Time empty to use spreadsheet row order.")
      else if (identical(interpreted$reason, "inconsistent"))
        gettext("Warning: Time contains inconsistent day/month ordering. Time was ignored, and the analysis used spreadsheet row order. Use dates in year-month-day order (YYYY-MM-DD), for example 2020-10-01 for 1 October 2020, or leave Time empty to use spreadsheet row order.")
      else if (interpreted$status == "ambiguous")
        gettext("Warning: The date format in Time is ambiguous. Time was ignored, and the analysis used spreadsheet row order. Use dates in year-month-day order (YYYY-MM-DD), for example 2020-10-01 for 1 October 2020, or leave Time empty to use spreadsheet row order.")
      else
        gettext("Warning: The selected Time variable contains values that cannot be interpreted as numbers or dates. Time was ignored, and the analysis used spreadsheet row order. Select a numeric variable or one containing unambiguous dates, or leave Time empty to use spreadsheet row order.")
      rows <- rownames(dataset)[head(interpreted$affectedRows, 5L)]
      if (length(rows) && length(interpreted$affectedRows) < nrow(dataset))
        text <- paste(text, gettextf("Affected spreadsheet rows (first few): %s.", paste(rows, collapse = ", ")))
      jaspResults[["timeWarning"]] <- createJaspHtml(title = gettext("Warning: Time ignored"), text = text,
        dependencies = c("dependent", "time"), position = -5)
    }
  } else if (interpreted$status %in% c("numeric", "date")) {
    if (length(interpreted$dateConventions) && !is.null(jaspResults)) {
      descriptions <- c(
        ymd = gettext("year-month-day (YYYY-MM-DD), for example 2020-01-10 as 10 January 2020"),
        dmy = gettext("day-month-year (DD-MM-YYYY), for example 10-01-2020 as 10 January 2020"),
        mdy = gettext("month-day-year (MM-DD-YYYY), for example 01-10-2020 as 10 January 2020"))
      description <- paste(descriptions[interpreted$dateConventions], collapse = "; ")
      text <- if (length(interpreted$dateConventions) == 1L)
        gettextf("Time was interpreted in %s.", description)
      else
        gettextf("Dates in Time were interpreted using these date orders: %s.", description)
      jaspResults[["timeInterpretationNote"]] <- createJaspHtml(title = gettext("Date interpretation"), text = text,
        dependencies = c("dependent", "time"), position = -3)
    }
    if (anyDuplicated(dat$t) && !is.null(jaspResults)) {
      jaspResults[["timeTiesNote"]] <- createJaspHtml(
        title = gettext("Repeated time values"),
        text = gettext("Time contains repeated values. Spreadsheet row order is retained within tied values. Tied observations are analysed as successive steps in that order."),
        dependencies = c("dependent", "time"), position = -1)
    }
    if (is.unsorted(dat$t)) {
      # The second key makes the ordering of tied values explicit and stable.
      dat <- dat[order(dat$t, seq_len(nrow(dat))), , drop = FALSE]
      if (!is.null(jaspResults)) {
        jaspResults[["timeSortingNote"]] <- createJaspHtml(
          title = gettext("Time ordering"),
          text = gettext("Observations were sorted by Time for this analysis."),
          dependencies = c("dependent", "time"), position = -2)
      }
    }
  }
  selected <- .tsFilterPreparedData(dat, options, jaspResults)
  return(list(raw = dat, data = selected))
}

# Interpret Time only to order observations, without inferring missing measurements.
# Text date bounds: Gregorian years 0001..9999, clock 00:00:00..<24:00:00;
# leap seconds, 24:00, named months, and incomplete/two-digit years are not
# accepted. Explicit numeric text is considered before any date syntax.
.tsInterpretTime <- function(time) {
  n <- length(time)
  answer <- function(t, status, rows = integer(), reason = NULL)
    list(t = t, status = status, affectedRows = as.integer(rows), reason = reason)
  fallback <- function(status, rows, reason = "unsupported")
    answer(seq_len(n), status, rows, reason)

  if (n == 0L)
    return(fallback("invalid", integer()))

  # Do not discard a known date/time class or convert numeric values through
  # character strings, which can round otherwise distinct numeric indices.
  if (inherits(time, "Date") || inherits(time, "POSIXt")) {
    bad <- which(!is.finite(as.numeric(time)))
    if (length(bad)) return(fallback("invalid", bad, "missing"))
    return(answer(if (inherits(time, "Date")) as.POSIXct(time, tz = "UTC") else as.POSIXct(time), "date"))
  }
  if (is.numeric(time) && !is.complex(time) && is.null(dim(time))) {
    bad <- which(!is.finite(time))
    if (length(bad)) return(fallback("invalid", bad, "missing"))
    return(answer(time, "numeric"))
  }
  if (!(is.character(time) || is.factor(time)) || !is.null(dim(time)))
    return(fallback("invalid", seq_len(n)))

  text <- trimws(as.character(time))
  numberPattern <- "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$"
  numericSyntax <- !is.na(text) & grepl(numberPattern, text, perl = TRUE)
  nonfiniteSyntax <- !is.na(text) & text %in% c("Inf", "+Inf", "-Inf", "Infinity", "+Infinity", "-Infinity", "NaN", "NA")
  missingSyntax <- is.na(text) | !nzchar(text)
  if (any(nonfiniteSyntax | missingSyntax))
    return(fallback("invalid", which(nonfiniteSyntax | missingSyntax), "missing"))
  if (all(numericSyntax)) {
    value <- suppressWarnings(as.numeric(text))
    bad <- which(!is.finite(value))
    if (length(bad)) return(fallback("invalid", bad, "missing"))
    # Some text exports use year.month labels. Only flag them when a valid
    # competing calendar interpretation would reverse order or merge months.
    # Native numeric columns and ordinary decimal indices stay numeric.
    if (all(grepl("^[0-9]{4}\\.[0-9]{1,2}$", text))) {
      parts <- strsplit(text, ".", fixed = TRUE)
      years <- as.integer(vapply(parts, `[[`, "", 1L))
      months <- as.integer(vapply(parts, `[[`, "", 2L))
      if (all(years >= 1L & months >= 1L & months <= 12L)) {
        byNumber <- order(value, seq_len(n))
        periodDiff <- diff((years * 12L + months)[byNumber])
        conflicts <- which(periodDiff < 0L | (diff(value[byNumber]) == 0 & periodDiff != 0L))
        if (length(conflicts))
          return(fallback("ambiguous", sort(unique(byNumber[c(conflicts, conflicts + 1L)])), "yearMonth"))
      }
    }
    return(answer(value, "numeric"))
  }

  validDate <- function(year, month, day) {
    if (year < 1L || year > 9999L || month < 1L || month > 12L || day < 1L)
      return(FALSE)
    leap <- year %% 4L == 0L && (year %% 100L != 0L || year %% 400L == 0L)
    days <- c(31L, if (leap) 29L else 28L, 31L, 30L, 31L, 30L,
              31L, 31L, 30L, 31L, 30L, 31L)
    day <= days[[month]]
  }
  # Missing year/century and short day-month text are ambiguous, not dates.
  shortDate <- function(value) {
    match <- regmatches(value, regexec("^([0-9]{1,2})[-/.]([0-9]{1,2})(?:[-/.]([0-9]{1,2}))?\\.?$", value, perl = TRUE))[[1L]]
    if (!length(match)) return(FALSE)
    a <- as.integer(match[[2L]])
    b <- as.integer(match[[3L]])
    a >= 1L && a <= 31L && b >= 1L && b <= 31L && min(a, b) <= 12L
  }

  # Capture a complete date followed by an optional complete clock and zone.
  # A zone is accepted only with a clock. Mixed zoned/unzoned input is flagged
  # ambiguous below; consistently unzoned text uses UTC, as in the old parser.
  datePattern <- paste0(
    "^([0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}|[0-9]{1,2}[-/.][0-9]{1,2}[-/.][0-9]{4})",
    "(?:[T ]([0-9]{2}):([0-9]{2})(?::([0-9]{2})(\\.[0-9]+)?)?(Z|[+-][0-9]{2}:?[0-9]{2})?)?$"
  )
  kind <- rep("invalid", n)
  year <- month <- day <- hour <- minute <- rep(NA_integer_, n)
  second <- offset <- rep(0, n)
  hasZone <- dmy <- mdy <- rep(FALSE, n)
  localFirst <- localSecond <- rep(NA_integer_, n)

  for (i in seq_len(n)) {
    if (is.na(text[[i]]) || !nzchar(text[[i]])) next
    fields <- regmatches(text[[i]], regexec(datePattern, text[[i]], perl = TRUE))[[1L]]
    if (!length(fields)) {
      if (shortDate(text[[i]])) kind[[i]] <- "ambiguous"
      next
    }
    dateText <- fields[[2L]]
    separators <- gsub("[0-9]", "", dateText)
    if (substr(separators, 1L, 1L) != substr(separators, 2L, 2L)) next
    parts <- strsplit(dateText, "[-/.]")[[1L]]
    hour[[i]] <- if (nzchar(fields[[3L]])) as.integer(fields[[3L]]) else 0L
    minute[[i]] <- if (nzchar(fields[[4L]])) as.integer(fields[[4L]]) else 0L
    second[[i]] <- if (nzchar(fields[[5L]])) as.numeric(paste0(fields[[5L]], fields[[6L]])) else 0
    if (hour[[i]] > 23L || minute[[i]] > 59L || !is.finite(second[[i]]) || second[[i]] >= 60)
      next
    zone <- fields[[7L]]
    hasZone[[i]] <- nzchar(zone)
    if (hasZone[[i]] && zone != "Z") {
      zone <- gsub(":", "", zone, fixed = TRUE)
      zoneHour <- as.integer(substr(zone, 2L, 3L))
      zoneMinute <- as.integer(substr(zone, 4L, 5L))
      if (zoneHour > 23L || zoneMinute > 59L) next
      offset[[i]] <- (zoneHour * 3600 + zoneMinute * 60) * if (substr(zone, 1L, 1L) == "-") -1 else 1
    }
    if (nchar(parts[[1L]]) == 4L) {
      year[[i]] <- as.integer(parts[[1L]])
      month[[i]] <- as.integer(parts[[2L]])
      day[[i]] <- as.integer(parts[[3L]])
      if (validDate(year[[i]], month[[i]], day[[i]])) kind[[i]] <- "ymd"
    } else {
      year[[i]] <- as.integer(parts[[3L]])
      localFirst[[i]] <- as.integer(parts[[1L]])
      localSecond[[i]] <- as.integer(parts[[2L]])
      dmy[[i]] <- validDate(year[[i]], localSecond[[i]], localFirst[[i]])
      mdy[[i]] <- validDate(year[[i]], localFirst[[i]], localSecond[[i]])
      if (dmy[[i]] || mdy[[i]]) kind[[i]] <- "local"
    }
  }

  inconsistent <- integer()
  local <- which(kind == "local")
  if (length(local)) {
    allDmy <- all(dmy[local])
    allMdy <- all(mdy[local])
    if (xor(allDmy, allMdy)) {
      month[local] <- if (allDmy) localSecond[local] else localFirst[local]
      day[local] <- if (allDmy) localFirst[local] else localSecond[local]
    } else {
      # Even 01/01/2020 alone does not establish one local convention; the
      # caller's contract requires the whole column to establish one.
      kind[local] <- if (allDmy && allMdy) "ambiguous" else "invalid"
      if (!allDmy && !allMdy) inconsistent <- local
    }
  }
  dateRows <- which(kind %in% c("ymd", "local"))
  if (length(dateRows) && any(hasZone[dateRows]) && !all(hasZone[dateRows]))
    kind[dateRows] <- "ambiguous"
  if (length(inconsistent)) return(fallback("invalid", inconsistent, "inconsistent"))
  if (any(kind == "invalid")) return(fallback("invalid", which(kind == "invalid")))
  if (any(kind == "ambiguous")) return(fallback("ambiguous", which(kind == "ambiguous")))

  dateText <- sprintf("%04d-%02d-%02d", year, month, day)
  midnight <- as.POSIXct(dateText, format = "%Y-%m-%d", tz = "UTC")
  # Calendar fields were checked independently; round-trip as well to avoid
  # accepting a platform parser's normalization or unsupported finite range.
  # Compare fields directly because formatted years can omit leading zeros.
  calendar <- as.POSIXlt(midnight, tz = "UTC")
  exact <- !is.na(midnight) & calendar$year + 1900L == year &
    calendar$mon + 1L == month & calendar$mday == day
  epoch <- as.numeric(midnight) + hour * 3600 + minute * 60 + second - offset
  lower <- as.numeric(as.POSIXct("0001-01-01", format = "%Y-%m-%d", tz = "UTC"))
  upper <- as.numeric(as.POSIXct("9999-12-31", format = "%Y-%m-%d", tz = "UTC")) + 86400
  bad <- which(!exact | !is.finite(epoch) | epoch < lower | epoch >= upper)
  if (length(bad)) return(fallback("invalid", bad))
  interpreted <- answer(as.POSIXct(epoch, origin = "1970-01-01", tz = "UTC"), "date")
  # Disclose the existing text interpretation; native date classes need no guess.
  interpreted$dateConventions <- c(if (any(kind == "ymd")) "ymd",
    if (length(local)) if (allDmy) "dmy" else "mdy")
  interpreted
}

# Select by original spreadsheet row IDs or interpreted Time values.
.tsFilterPreparedData <- function(dataset, options, jaspResults = NULL) {
  endExclusive <- FALSE
  if (!options[["filter"]])
    return(dataset)
  if (nrow(dataset) == 0)
    .quitAnalysis(gettext("The selected filter contains no observations. Adjust the filter limits."))

  if (options[["filterBy"]] == "row") {
    values <- as.numeric(rownames(dataset))
    start <- if (identical(options[["rowStart"]], "")) min(values) else suppressWarnings(as.numeric(options[["rowStart"]]))
    end <- if (identical(options[["rowEnd"]], "")) max(values) else suppressWarnings(as.numeric(options[["rowEnd"]]))
  } else {
    if (isTRUE(attr(dataset, "timeFallback")) || options[["time"]] == "")
      .quitAnalysis(gettext("Filtering by Time requires a usable numeric or date variable assigned to Time. Select an appropriate Time variable, choose Row number to filter spreadsheet rows, or disable filtering."))
    values <- dataset$t
    if (options[["filterBy"]] == "time") {
      if (inherits(values, "POSIXt"))
        .quitAnalysis(gettext("The selected Time variable contains dates. Choose Date to filter it, or assign a numeric variable to Time."))
      start <- if (identical(options[["timeStart"]], "")) min(values) else suppressWarnings(as.numeric(options[["timeStart"]]))
      end <- if (identical(options[["timeEnd"]], "")) max(values) else suppressWarnings(as.numeric(options[["timeEnd"]]))
    } else if (options[["filterBy"]] == "date") {
      if (!inherits(values, "POSIXt"))
        .quitAnalysis(gettext("Assign a date variable to Time, or choose Time index to filter the selected numeric variable."))
      start <- min(values)
      end <- max(values)
      if (!identical(options[["dateStart"]], "")) {
        parsed <- .tsInterpretTime(options[["dateStart"]])
        if (parsed$status != "date" || length(parsed$t) != 1L)
          .quitAnalysis(gettext("Enter Start in year-month-day order (YYYY-MM-DD), for example 2020-10-01 for 1 October 2020. For a specific time, use YYYY-MM-DD HH:MM:SS."))
        start <- parsed$t
      }
      if (!identical(options[["dateEnd"]], "")) {
        parsed <- .tsInterpretTime(options[["dateEnd"]])
        if (parsed$status != "date" || length(parsed$t) != 1L)
          .quitAnalysis(gettext("Enter End in year-month-day order (YYYY-MM-DD), for example 2020-10-01 for 1 October 2020. For a specific time, use YYYY-MM-DD HH:MM:SS."))
        end <- parsed$t
        # Parsed text uses UTC. A date without a clock includes the whole UTC
        # day; explicit timestamps retain their exact inclusive boundary.
        endExclusive <- !grepl("[T ]", trimws(options[["dateEnd"]]))
        if (endExclusive) end <- end + 86400
      }
    }
  }
  if (length(start) != 1L || length(end) != 1L || !is.finite(start) || !is.finite(end))
    .quitAnalysis(gettext("Filter limits must be valid, finite values."))
  if (end < start || (endExclusive && end == start))
    .quitAnalysis(gettext("The 'End' value of the filter should be larger/later than the 'Start' value."))
  selected <- values >= start & (if (endExclusive) values < end else values <= end)
  if (!any(selected)) {
    if (options[["filterBy"]] == "time") {
      bounds <- format(c(start, end, min(values), max(values)), trim = TRUE, digits = 15)
      .quitAnalysis(gettextf("No observations have Time values between %1$s and %2$s. The selected Time variable ranges from %3$s to %4$s. Adjust the limits, or choose Row number to filter spreadsheet rows.",
        bounds[1L], bounds[2L], bounds[3L], bounds[4L]))
    }
    if (options[["filterBy"]] == "date") {
      bounds <- format(range(values), "%Y-%m-%d %H:%M:%OS", tz = "UTC")
      .quitAnalysis(gettextf("No observations have dates within the selected range. The selected Time variable ranges from %1$s to %2$s (UTC). Adjust the date limits, or choose Row number to filter spreadsheet rows.",
        bounds[1L], bounds[2L]))
    }
    .quitAnalysis(gettext("The selected filter contains no observations. Adjust the filter limits."))
  }
  if (options[["filterBy"]] == "row" && options[["time"]] != "" &&
      !isTRUE(attr(dataset, "timeFallback")) && any(diff(which(selected)) != 1L) &&
      !is.null(jaspResults)) {
    jaspResults[["timeFilterWarning"]] <- createJaspHtml(
      title = gettext("Warning: Filtering"),
      text = gettext("Warning: The selected spreadsheet rows skip observations within Time order. The selected observations are analysed as successive steps. Use a Time or date filter to select a continuous block in Time order."),
      dependencies = c("dependent", "time", "filter", "filterBy", "rowStart", "rowEnd"), position = -4)
  }
  return(dataset[selected, , drop = FALSE])
}
