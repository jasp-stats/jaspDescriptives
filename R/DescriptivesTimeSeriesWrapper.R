#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Time Series Descriptives
#'
#' Time Series Descriptives allows the user to obtain descriptive statistics and descriptive plots for univariate time-series.
#'
#' @param acf, Plots the autocorrelation for a specified number of lags. The confidence interval may be given assuming either a white noise process, or assuming for a lag q a moving average process of order q - 1.
#'    Defaults to \code{FALSE}.
#' @param dependent, A variable that is measured repeatedly over time.
#' @param filter, Selects an inclusive range of observations for analysis. Row number refers to original spreadsheet rows, even when Time changes the analysis order. Time index filters numeric Time values, including negative values and decimals. Date filters interpreted dates and times. Date filters use UTC. A date-only End includes the entire day; an End with a time uses that exact cutoff. Time or date filtering requires a usable Time variable; otherwise, select Row number or disable filtering.
#'    Defaults to \code{FALSE}.
#' @param lagPlot, Plots the dependent variable (y-axis) against a lagged version of itself (x-axis). The lag stands for the amount of observations in between the dependent variable and the lagged version of itself. The regression line is the autoregression at the specified lag.
#'    Defaults to \code{FALSE}.
#' @param pacf, Plots the partial autocorrelation for a specified number of lags.
#'    Defaults to \code{FALSE}.
#' @param time, Optional. Complete numeric values order observations from smallest to largest, regardless of gaps. Complete, unambiguous dates and times order observations chronologically. Use 'YYYY-MM-DD' for dates or 'YYYY-MM-DD HH:MM:SS' for dates with times; seconds can be omitted. All variables stay aligned, and spreadsheet row order is unchanged. Tied values retain spreadsheet row order. If any value is missing or cannot be interpreted, or dates are ambiguous, Time is ignored for the entire analysis and a warning is shown. Leave Time empty to use spreadsheet row order. Successive observations are modeled as equally spaced steps; no rows are added for gaps in Time.
#' @param timeSeriesPlot, Plots the dependent variable (y-axis) over time (x-axis).
#'    Defaults to \code{TRUE}.
DescriptivesTimeSeries <- function(
          data = NULL,
          version = "1",
          acf = FALSE,
          acfCi = TRUE,
          acfCiLevel = 0.95,
          acfCiType = "whiteNoise",
          acfMaxLag = 10,
          acfZeroLag = FALSE,
          dateEnd = "",
          dateStart = "",
          dependent = list(types = list(), value = ""),
          descriptivesTableTransposed = FALSE,
          filter = FALSE,
          filterBy = "row",
          lagPlot = FALSE,
          lagPlotLag = 1,
          lagPlotRegressionCi = TRUE,
          lagPlotRegressionCiLevel = 0.95,
          lagPlotRegressionLine = TRUE,
          lagPlotRegressionType = "smooth",
          pacf = FALSE,
          pacfCi = TRUE,
          pacfCiLevel = 0.95,
          pacfMaxLag = 10,
          plotHeight = 320,
          plotWidth = 480,
          rowEnd = 100,
          rowStart = 1,
          time = list(types = list(), value = ""),
          timeEnd = 100,
          timeSeriesPlot = TRUE,
          timeSeriesPlotDistribution = "none",
          timeSeriesPlotType = "both",
          timeStart = 1) {

   defaultArgCalls <- formals(jaspDescriptives::DescriptivesTimeSeries)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dependent", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDescriptives", "DescriptivesTimeSeries", "DescriptivesTimeSeries.qml", options, version, TRUE))
}