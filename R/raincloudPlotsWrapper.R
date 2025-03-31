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

raincloudPlots <- function(
          data = NULL,
          version = "0.19.3",
          boxNudge = 0,
          boxOpacity = 0.5,
          boxOutline = "colorPalette",
          boxOutlineWidth = 1,
          boxPadding = 0.1,
          boxWidth = 0.1,
          colorAnyway = TRUE,
          colorPalette = "colorblind",
          covariate = list(types = list(), value = ""),
          covariatePalette = "viridis",
          customAxisLimits = FALSE,
          customColors = FALSE,
          customSides = FALSE,
          customizationTable = list(list(levels = "Row 0", name = "data 1", values = "R"), list(levels = "Row 0", name = "data 2", values = 0), list(levels = "Row 0", name = "data 3", values = 0), list(levels = "Row 0", name = "data 4", values = "#00A9E6")),
          dependentVariables = list(types = list(), value = list()),
          heightPlot = 350,
          horizontal = FALSE,
          jitter = FALSE,
          lowerAxisLimit = 0,
          mean = FALSE,
          meanCiWidth = 0.95,
          meanInterval = FALSE,
          meanIntervalCustom = FALSE,
          meanIntervalOption = "ci",
          meanLines = FALSE,
          meanLinesOpacity = 0.5,
          meanLinesWidth = 1,
          meanPosition = "likeBox",
          meanSize = 6,
          numberOfClouds = 1,
          observationId = list(types = list(), value = ""),
          observationIdLineOpacity = 0.25,
          observationIdLineWidth = 1,
          placeholder1 = 0,
          placeholder2 = 0,
          placeholder3 = 0,
          placeholder4 = 0,
          placeholder5 = 0,
          placeholder6 = 0,
          placeholder7 = 0,
          placeholder8 = 0,
          plotHeight = 320,
          plotWidth = 480,
          pointNudge = 0.19,
          pointOpacity = 0.5,
          pointSize = 2.5,
          pointSpread = 0.065,
          primaryFactor = list(types = list(), value = ""),
          secondaryFactor = list(types = list(), value = ""),
          showBox = TRUE,
          showCaption = TRUE,
          showLegend = FALSE,
          showPoint = TRUE,
          showVio = TRUE,
          table = FALSE,
          tableBoxStatistics = TRUE,
          upperAxisLimit = 1000,
          vioHeight = 0.7,
          vioNudge = 0.15,
          vioOpacity = 0.5,
          vioOutline = "colorPalette",
          vioOutlineWidth = 1,
          vioSmoothing = 1,
          widthPlot = 600) {

   defaultArgCalls <- formals(jaspDescriptives::raincloudPlots)
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

   optionsWithFormula <- c("boxOutline", "colorPalette", "covariate", "covariatePalette", "customizationTable", "dependentVariables", "observationId", "primaryFactor", "secondaryFactor", "vioOutline")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDescriptives", "raincloudPlots", "raincloudPlots.qml", options, version, TRUE))
}