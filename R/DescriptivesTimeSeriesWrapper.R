#
# Copyright (C) 2013-2022 University of Amsterdam
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

# This is a generated file. Don't change it

DescriptivesTimeSeries <- function(
          data = NULL,
          version = "0.19",
          acf = FALSE,
          acfCi = TRUE,
          acfCiLevel = 0.95,
          acfCiType = "whiteNoise",
          acfMaxLag = 10,
          acfZeroLag = FALSE,
          dateEnd = "",
          dateStart = "",
          dependent = "",
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
          time = "",
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

   optionsWithFormula <- c("dependent", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDescriptives::DescriptivesTimeSeries", data, options, version))
}
