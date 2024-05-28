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

Descriptives <- function(
          data = NULL,
          version = "0.19",
          formula = NULL,
          associationMatrixUse = "everything",
          boxPlot = FALSE,
          boxPlotBoxPlot = TRUE,
          boxPlotColourPalette = FALSE,
          boxPlotJitter = FALSE,
          boxPlotOutlierLabel = FALSE,
          boxPlotViolin = FALSE,
          ciBootstrapSamples = 1000,
          coefficientOfVariation = FALSE,
          colorPalette = "colorblind",
          correlation = FALSE,
          correlationPlots = FALSE,
          covariance = FALSE,
          customHistogramPosition = "stack",
          densityPlot = FALSE,
          densityPlotSeparate = "",
          densityPlotTransparency = 20,
          densityPlotType = "density",
          descriptivesTableTransposed = FALSE,
          distributionAndCorrelationPlotDensity = FALSE,
          distributionAndCorrelationPlotHistogramBinWidthType = "sturges",
          distributionAndCorrelationPlotHistogramManualNumberOfBins = 30,
          distributionAndCorrelationPlotRugMarks = FALSE,
          distributionPlots = FALSE,
          dotPlot = FALSE,
          frequencyTables = FALSE,
          frequencyTablesMaximumDistinctValues = 10,
          heatmapDisplayValue = FALSE,
          heatmapDisplayValueRelativeTextSize = 1,
          heatmapHorizontalAxis = "",
          heatmapLegend = FALSE,
          heatmapPlot = FALSE,
          heatmapStatisticContinuous = "mean",
          heatmapStatisticDiscrete = "mode",
          heatmapTileWidthHeightRatio = 1,
          heatmapVerticalAxis = "",
          intervalPlot = FALSE,
          iqr = FALSE,
          kurtosis = FALSE,
          likertPlot = FALSE,
          likertPlotAdjustableFontSize = "normal",
          likertPlotAssumeVariablesSameLevel = FALSE,
          mad = FALSE,
          madRobust = FALSE,
          maximum = TRUE,
          mean = TRUE,
          meanCi = FALSE,
          meanCiLevel = 0.95,
          meanCiMethod = "oneSampleTTest",
          median = FALSE,
          minimum = TRUE,
          missing = TRUE,
          mode = FALSE,
          paretoPlot = FALSE,
          paretoPlotRule = FALSE,
          paretoPlotRuleCi = 0.95,
          percentileValues = list(),
          percentiles = FALSE,
          pieChart = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          qqPlot = FALSE,
          quantilesForEqualGroups = FALSE,
          quantilesForEqualGroupsNumber = 4,
          quartiles = FALSE,
          range = FALSE,
          scatterPlot = FALSE,
          scatterPlotGraphTypeAbove = "density",
          scatterPlotGraphTypeRight = "density",
          scatterPlotLegend = TRUE,
          scatterPlotRegressionLine = TRUE,
          scatterPlotRegressionLineCi = TRUE,
          scatterPlotRegressionLineCiLevel = 0.95,
          scatterPlotRegressionLineType = "smooth",
          sd = TRUE,
          sdCi = FALSE,
          sdCiLevel = 0.95,
          seMean = FALSE,
          shapiroWilkTest = FALSE,
          skewness = FALSE,
          splitBy = "",
          statisticsValuesAreGroupMidpoints = FALSE,
          stemAndLeaf = FALSE,
          stemAndLeafScale = 1,
          sum = FALSE,
          valid = TRUE,
          variables = list(),
          variance = FALSE,
          varianceCi = FALSE,
          varianceCiLevel = 0.95) {

   defaultArgCalls <- formals(jaspDescriptives::Descriptives)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }

   optionsWithFormula <- c("associationMatrixUse", "colorPalette", "densityPlotSeparate", "distributionAndCorrelationPlotHistogramBinWidthType", "heatmapHorizontalAxis", "heatmapVerticalAxis", "likertPlotAdjustableFontSize", "meanCiMethod", "splitBy", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDescriptives::Descriptives", data, options, version))
}
