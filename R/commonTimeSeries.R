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

.tsReadData <- function(jaspResults, dataset, options, ready, covariates = FALSE) {
  if (!is.null(dataset)) {
    return(dataset)
  }

  if (ready) {
    yDataset <- .readDataSetToEnd(columns.as.numeric = options$dependent)
    yName <- options$dependent[1]
    y <- yDataset[, yName]
    dat <- data.frame(y)

    if (options$time == "") {
      t <- 1:nrow(yDataset)
    } else {
      tDataset <- .readDataSetToEnd(columns.as.factor = options$time)
      tName <- options$time[1]
      t <- as.character(tDataset[, tName])
    }
    dat <- cbind(dat, t)

    if (covariates) {
      if (length(options[["covariates"]]) > 0) {
        cDataset <- .readDataSetToEnd(columns.as.numeric = options$covariates)
        covariateNames <- options$covariates
        covariates <- as.data.frame(cDataset[, covariateNames])
        names(covariates) <- paste0("xreg", 1:length(covariateNames))
        dat <- cbind(dat, covariates)
      }
    }
    return(dat)
  }
}

.tsErrorHandler <- function(dataset, ready) {
  if (!ready) {
    return()
  }

  datZoo <- zoo::zoo(dataset$y, dataset$t)
  if (!zoo::is.regular(datZoo)) {
    .quitAnalysis(gettext("The time series data should be equally-spaced."))
  }

  if (any(duplicated(dataset$t))) {
    .quitAnalysis(gettext("The time variable should have unique values only."))
  }
}

.tsGuessInterval <- function(dataset) {
  allDiffInSec <- difftime(dataset$t[2:(length(dataset$t))], dataset$t[1:(length(dataset$t) - 1)], units = "secs")
  diffInSec <- as.numeric(names(which.max(table(allDiffInSec))))

  sec <- diffInSec
  min <- diffInSec / 60
  hour <- min / 60
  day <- hour / 24
  week <- day / 7
  month <- day / 30.44
  quarter <- month / 4
  year <- month / 12

  df <- data.frame(sec, min, hour, day, week, month, quarter, year)
  return(names(which.min(abs(df - 1))))
}

.tsDataFilterHandler <- function(dataset, options, ready) {
  if (options$filter & ready) {
    start <- 1
    end <- nrow(dataset)
    if (options$filterBy == "row") {
      if (options$rowStart != "") start <- options$rowStart
      if (options$rowEnd != "") end <- options$rowEnd
    }
    tryDate <- try(as.POSIXct(dataset$t, tz = "UTC"))
    if (options$filterBy == "time") {
      if (!jaspBase::isTryError(tryDate)) {
        .quitAnalysis(gettext("The 'Time' variable has a date-like format, please filter by date instead."))
      }
      start <- min(dataset$t, na.rm = TRUE)
      end <- max(dataset$t, na.rm = TRUE)
      if (options$timeStart != "") start <- options$timeStart
      if (options$timeEnd != "") end <- options$timeEnd
    }
    if (options$filterBy == "date") {
      if (jaspBase::isTryError(tryDate)) {
        .quitAnalysis(gettext("The 'Time' variable is not in a date-like format (e.g., yyyy-mm-dd hh:mm:ss). Try to filter by time index instead."))
      }
      first <- min(tryDate, na.rm = TRUE)
      last <- max(tryDate, na.rm = TRUE)
      if (options$dateStart != "") {
        start <- try(as.POSIXct(options$dateStart, tz = "UTC"))
        if (jaspBase::isTryError(start)) {
          .quitAnalysis(gettext("'Start' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)."))
        }
        if (start >= last) {
          .quitAnalysis(gettext("The 'Start' value of the filter should be before last observation."))
        }
        if (start < first) {
          start <- 1
        } else {
          start <- min(which(tryDate >= start))
        }
      }
      if (options$dateEnd != "") {
        end <- try(as.POSIXct(options$dateEnd, tz = "UTC"))
        if (jaspBase::isTryError(end)) {
          .quitAnalysis(gettext("'End' must be in a date-like format (e.g., yyyy-mm-dd hh:mm:ss)."))
        }
        if (end <= first) {
          .quitAnalysis(gettext("The 'End' value of the filter should be larger/later than the 'Start' value."))
        }
        if (end > last) {
          end <- nrow(dataset)
        } else {
          end <- max(which(tryDate <= end))
        }
      }
    }
    if (start >= nrow(dataset)) .quitAnalysis(gettext("The 'Start' value of the filter should be before last observation."))
    if (end > nrow(dataset)) end <- nrow(dataset)
    if (end <= start) {
      .quitAnalysis(gettext("The 'End' value of the filter should be larger/later than the 'Start' value."))
    }
    dataset <- dataset[start:end, ]
  }
  return(dataset)
}

.tsDataWithMissingRowsHandler <- function(dataset, options, ready) {
  # Sometimes time series data sets do not have NA's for missing data,
  # but skip rows with a column indicating the time / date
  # so e.g., when third measurement is missing at t = 3,
  # the data set goes from t = 2 on the second row, to t = 4 on the third.
  # This function imputes NA's for the missing time stamps.
  if (options$time == "") {
    return(dataset)
  }

  if (ready) {
    tryDate <- try(as.POSIXct(dataset$t, tz = "UTC"))

    if (jaspBase::isTryError(tryDate)) {
      minT <- min(dataset$t, na.rm = TRUE)
      maxT <- max(dataset$t, na.rm = TRUE)
      newT <- minT:maxT
    } else {
      dataset$t <- as.POSIXct(dataset$t, tz = "UTC")
      increment <- .tsGuessInterval(dataset)
      newT <- seq.POSIXt(min(dataset$t, na.rm = TRUE), max(dataset$t, na.rm = TRUE), by = increment)
    }
    dfNewT <- data.frame(t = newT)
    dat <- merge(dfNewT, dataset, all.x = TRUE)
    return(dat)
  }
}

.tsFillTimeSeriesPlot <- function(timeSeriesPlot, dataset, options, type, distribution, yName = NULL) {
  if (is.null(yName)) yName <- options$dependent[1]

  dat <- dataset

  p <- .tsJASPScatterPlot(dat$t, dat$y,
    yName = yName, xName = "t",
    addSmooth = FALSE, plotAbove = "none",
    plotRight = distribution,
    type = type
  )

  timeSeriesPlot$plotObject <- p
}

.tsAcfBartlett <- function(r, N, ci = 0.95) {
  # calculate confidence interval for ACF assuming moving average process
  # using Bartlett's formula
  z <- qnorm((1 + ci) / 2)

  lag <- length(r)
  df <- data.frame(r = r, se = numeric(lag))

  for (i in 1:lag) {
    df$se[i] <- z * sqrt((1 / N) * (1 + 2 * sum(r[1:i]^2)))
  }

  return(df)
}

.tsFillACF <- function(plot, type, dataset, options, zeroLag = FALSE, maxLag, ci, ciValue, ciType) {
  y <- na.omit(as.numeric(dataset$y))

  if (type == "ACF") {
    ac <- stats::acf(y, plot = FALSE, lag.max = maxLag)
  }
  if (type == "PACF") {
    ac <- stats::pacf(y, plot = FALSE, lag.max = maxLag)
    ciType <- "whiteNoise" # PACF does not have confidence interval assuming ma
  }

  dat <- data.frame(acf = ac$acf, lag = ac$lag)
  if (type == "ACF" && !zeroLag) {
    dat <- dat[-1, ]
  } # remove lag 0

  yRange <- dat$acf
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$lag)
  xBreaks <- xBreaks[!xBreaks %% 1] # keep only integers
  xMin <- min(xBreaks)
  xMax <- max(xBreaks)

  p <- ggplot2::ggplot()

  if (ci) {
    # add confidence bounds
    if (ciType == "whiteNoise") {
      clim <- qnorm((1 + ciValue) / 2) / sqrt(ac$n.used)
      dat$upper <- rep(clim, nrow(dat))
      dat$lower <- -dat$upper
    } else {
      clim <- .tsAcfBartlett(dat$acf, ac$n.used, ciValue)
      dat$upper <- clim$se
      dat$lower <- -dat$upper
    }

    yRange <- c(yRange, dat$upper, dat$lower)

    p <- p +
      ggplot2::geom_ribbon(
        data = dat,
        ggplot2::aes(x = lag, ymin = lower, ymax = upper), alpha = 0.15
      )
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  p <- p +
    ggplot2::scale_x_continuous(name = gettext("Lag"), breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = type, breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf), size = 1) +
    ggplot2::geom_segment(ggplot2::aes(x = xMin, xend = xMax, y = 0, yend = 0), alpha = 0.5) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  plot$plotObject <- p
  return()
}

# custom JASPScatterPlot function because x may be a date...
.tsJASPScatterPlot <- function(x, y, group = NULL, xName = NULL, yName = NULL,
                               addSmooth = TRUE, addSmoothCI = TRUE,
                               smoothCIValue = 0.95, forceLinearSmooth = FALSE,
                               plotAbove = c("density", "histogram", "none"),
                               plotRight = c("density", "histogram", "none"),
                               colorAreaUnderDensity = TRUE,
                               alphaAreaUnderDensity = .5,
                               showLegend = !is.null(group),
                               legendTitle = NULL,
                               type = "both",
                               ...) {

  # TODO: make actual error messages
  stopifnot(
    # is.numeric(x),
    is.numeric(y),
    is.null(group) || is.numeric(group) || is.factor(group),
    is.null(xName) || is.character(xName) || is.expression(xName),
    is.null(yName) || is.character(yName) || is.expression(yName),
    is.logical(addSmooth),
    is.logical(showLegend),
    length(x) == length(y) && (is.null(group) || length(x) == length(group))
  )
  plotRight <- match.arg(plotRight)

  tryDate <- try(as.POSIXct(x, tz = "UTC"))

  if (jaspBase::isTryError(tryDate)) {
    x <- as.numeric(x)
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(x)
    xScale <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks))
  } else {
    x <- as.POSIXct(x, tz = "UTC")
    xBreaks <- pretty(x)
    xLabels <- attr(xBreaks, "labels")
    xScale <- ggplot2::scale_x_datetime(breaks = xBreaks, labels = xLabels, limits = range(xBreaks))
  }

  df <- data.frame(x = x, y = y)
  mapping <- ggplot2::aes(x = .data$x, y = .data$y)

  geomPoint <- if (type == "points" || type == "both") {
    jaspGraphs::geom_point()
  } else {
    NULL
  }
  geomLine <- if (type == "line" || type == "both") {
    jaspGraphs::geom_line()
  } else {
    NULL
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(y)

  dots <- list(...)
  if (showLegend) {
    dots <- quantmod::setDefaults(dots, legend.position = "right")
  }

  mainPlot <- ggplot2::ggplot(df, mapping) +
    geomLine +
    geomPoint +
    ggplot2::labs(x = xName, y = yName, color = legendTitle, fill = legendTitle) +
    xScale +
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    do.call(jaspGraphs::themeJaspRaw, dots)

  gb <- ggplot2::ggplot_build(mainPlot)
  scales <- gb$layout$get_scales(1L)
  x.range <- scales$x$get_limits()
  y.range <- scales$y$get_limits()

  rightPlot <- jaspGraphs:::JASPScatterSubPlot(na.omit(y), group, plotRight, y.range, colorAreaUnderDensity, alphaAreaUnderDensity, flip = TRUE)

  plotList <- list(mainPlot = mainPlot, rightPlot = rightPlot)
  plotList <- plotList[lengths(plotList) > 0L]

  plot <- jaspGraphs:::jaspGraphsPlot$new(
    subplots     = plotList,
    plotFunction = jaspGraphs:::reDrawAlignedPlot,
    size         = 5,
    showLegend   = showLegend
  )
  return(plot)
}
