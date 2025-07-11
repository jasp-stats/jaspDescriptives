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

#' Plot Builder (beta)
#'
#' @param relativeHeight, Specify the relative heights of the columns or widths of the rows in the plot layout.
jaspPlotBuilder <- function(
          data = NULL,
          version = "0.95",
          PlotBuilderTab = list(list(accuracyCountValue = 0.1, accuracyMeanValue = 0.1, accuracyMedianValue = 0.1, accuracySumValue = 0.1, addAreaStack = FALSE, addBarStack = FALSE, addBoxplot = FALSE, addCI95ErrorBar = FALSE, addCi95Ribbon = FALSE, addCountArea = FALSE, addCountBar = FALSE, addCountDash = FALSE, addCountDot = FALSE, addCountLine = FALSE, addCountValue = FALSE, addCurveFitPlotBuilder = FALSE, addDataPoint = FALSE, addHistogram = FALSE, addIdentityLinePlotBuilder = FALSE, addMeanArea = FALSE, addMeanBar = FALSE, addMeanDash = FALSE, addMeanDot = FALSE, addMeanLine = FALSE, addMeanValue = FALSE, addMedianArea = FALSE, addMedianBar = FALSE, addMedianDash = FALSE, addMedianDot = FALSE, addMedianLine = FALSE, addMedianValue = FALSE, addRangeErrorBar = FALSE, addRangeRibbon = FALSE, addReferenceLinePlotBuilder = FALSE, addSDErrorBar = FALSE, addSEMErrorBar = FALSE, addSdRibbon = FALSE, addSemRibbon = FALSE, addSumArea = FALSE, addSumBar = FALSE, addSumDash = FALSE, addSumDot = FALSE, addSumLine = FALSE, addSumValue = FALSE, addViolin = FALSE, aggregationFun = "mean", aggregationFunY = "mean", alphaAreaStack = 0.4, alphaBarStack = 0.8, alphaBoxplotPlotBuilder = 0.8, alphaCi95Ribbon = 0.4, alphaCountArea = 1, alphaCountBar = 1, alphaCountDash = 1, alphaCountDot = 1, alphaCountLine = 1, alphaCountValue = 1, alphaHistogramPlotBuilder = 1, alphaMeanArea = 1, alphaMeanBar = 1, alphaMeanDash = 1, alphaMeanDot = 1, alphaMeanLine = 1, alphaMeanValue = 1, alphaMedianArea = 1, alphaMedianBar = 1, alphaMedianDash = 1, alphaMedianDot = 1, alphaMedianLine = 1, alphaMedianValue = 1, alphaPlotBuilder = 0.5, alphaRangeRibbon = 0.4, alphaSdRibbon = 0.4, alphaSemRibbon = 0.4, alphaSumArea = 1, alphaSumBar = 1, alphaSumDash = 1, alphaSumDot = 1, alphaSumLine = 1, alphaSumValue = 1, alphaViolinPlotBuilder = 0.8, annotationLineList = list(), annotationPlotBuilder = list(), asPercentage = FALSE, asTable = "bottom-right", asTableFacetWrap = "bottom-rightFacetWrap", axes = "margins", axisLabels = "all", barwidthCountBar = 0.8, baseFontSize = 18, betweenSubjectFactors = list(types = list(), value = list()), binsPlotBuilder = 30, blackOutlineBoxplot = TRUE, blackOutlineCI95ErrorBar = FALSE, blackOutlineCi95Ribbon = FALSE, blackOutlineCountDash = FALSE, blackOutlineCountDot = FALSE, blackOutlineCountLine = FALSE, blackOutlineCountValue = FALSE, blackOutlineCurveFit = FALSE, blackOutlineDataPoint = FALSE, blackOutlineMeanDash = FALSE, blackOutlineMeanDot = FALSE, blackOutlineMeanLine = FALSE, blackOutlineMeanValue = FALSE, blackOutlineMedianDash = FALSE, blackOutlineMedianDot = FALSE, blackOutlineMedianLine = FALSE, blackOutlineMedianValue = FALSE, blackOutlineRangeErrorBar = FALSE, blackOutlineRangeRibbon = FALSE, blackOutlineSDErrorBar = FALSE, blackOutlineSEMErrorBar = FALSE, blackOutlineSdRibbon = FALSE, blackOutlineSemRibbon = FALSE, blackOutlineSumDash = FALSE, blackOutlineSumDot = FALSE, blackOutlineSumLine = FALSE, blackOutlineSumValue = FALSE, blackOutlineViolin = FALSE, bottomMargin = 10, breakByX = "", breakByY = "", breakFromX = "", breakFromY = "", breakToX = "", breakToY = "", captionPlotBuilder = "", colSplitRM = list(types = list(), value = ""), colorByGroup = "none", colorIdentityLine = "lightgray", colorLabelRenamer = list(), colorReferenceLine = "lightgray", colorsAll = "jaspPalette", columnsvariableSplitPlotBuilder = list(types = list(), value = ""), connectRMPlotBuilder = FALSE, covariates = list(types = list(), value = list()), curvaFitMethod = "lm", customColors = "", cutShortScale = FALSE, cutShortScaleY = FALSE, dashwidthCountDash = 0.8, dashwidthMeanDash = 0.8, dashwidthMedianDash = 0.8, dodgeBoxplotPlotBuilder = 0.8, dodgeCI95ErrorBar = 0.8, dodgeCountArea = 0.8, dodgeCountBar = 0.8, dodgeCountDash = 0.8, dodgeCountDot = 0.8, dodgeCountLine = 0.8, dodgeCurveFit = 0.8, dodgeMeanArea = 0.8, dodgeMeanBar = 0.8, dodgeMeanDash = 0.8, dodgeMeanDot = 0.8, dodgeMeanLine = 0.8, dodgeMedianArea = 0.8, dodgeMedianBar = 0.8, dodgeMedianDash = 0.8, dodgeMedianDot = 0.8, dodgeMedianLine = 0.8, dodgeRangeErrorBar = 0.8, dodgeSDErrorBar = 0.8, dodgeSEMErrorBar = 0.8, dodgeSumArea = 0.8, dodgeSumBar = 0.8, dodgeSumDash = 0.8, dodgeSumDot = 0.8, dodgeSumLine = 0.8, dodgeViolinPlotBuilder = 0.8, drawQuantilesViolinPlotBuilder = "0.25, 0.5, 0.75", emptyCircles = FALSE, enableSort = FALSE, enableSortY = FALSE, fontsizeCountValue = 14, fontsizeMeanValue = 14, fontsizeMedianValue = 14, fontsizeSumValue = 14, gridVarRM = list(types = list(), value = ""), gridVariablePlotBuilder = list(types = list(), value = ""), groupVarRM = list(types = list(), value = ""), heightPlotBuilder = 300, hjustCountValue = 0.5, hjustMeanValue = 0.5, hjustMedianValue = 0.5, hjustSumValue = 0.5, isRM = "noRM", jitterhPlotBuilder = 0.3, jitterwPlotBuilder = 0.3, labelSizePValue = 4.5, labelcolor = "black", leftMargin = 10, legendPosistionPlotBuilder = "right", limitFromX = "", limitFromY = "", limitToX = "", limitToY = "", lineRMsize = 0.5, lineRMtransparency = 0.5, linewidhtReferenceLines = 1, linewidthAreaStack = 0.25, linewidthCI95ErrorBar = 1, linewidthCountDash = 1, linewidthCountLine = 1, linewidthCurveFit = 1, linewidthMeanDash = 1, linewidthMeanLine = 1, linewidthMedianDash = 1, linewidthMedianLine = 1, linewidthRangeErrorBar = 1, linewidthSDErrorBar = 1, linewidthSEMErrorBar = 1, linewidthSumDash = 1, linewidthSumLine = 0.5, linewidthViolinPlotBuilder = 0.8, margins = FALSE, ncolFacetWrap = 1, nrowFacetWrap = 1, outlierBoxplotPlotBuilder = FALSE, outlierCoefBoxplotPlotBuilder = 1.5, outlierSizeBoxplotPlotBuilder = 1, pairwiseComparisons = list(), plotStyle = "JASP", pointDodgePlotBuilder = 0.8, pointsizePlotBuilder = 3, propMode = "absolute", removeLegendTitle = FALSE, repeatedMeasuresCells = list("", ""), repeatedMeasuresFactors = list(list(levels = list("Level 1", "Level 2"), name = "RM Factor 1")), replaceNaAreaStack = FALSE, reverseAreaStack = FALSE, reverseBarStack = FALSE, reversedirectionIdentityLine = FALSE, rightMargin = 10, rotateXLabel = FALSE, rotateYLabel = FALSE, rowSplitRM = list(types = list(), value = ""), rowsvariableSplitPlotBuilder = list(types = list(), value = ""), scaleViolinPlotBuilder = "width", scales = "fixed", scalesFacetWrap = "fixed", seCurveFit = TRUE, sizeCountDot = 5, sizeMeanDot = 5, sizeMedianDot = 5, sizeSumDot = 5, sortXLabelsOrder = "Increasing", sortYLabelsOrder = "Increasing", space = "fixed", stepDistance = 0.15, stripPosition = "top", titlePlotBuilder = "", titleXPlotBuilder = "", titleYPlotBuilder = "", topMargin = 10, transparencyCI95ErrorBar = 1, transparencyCurveFit = 0.2, transparencyRangeErrorBar = 1, transparencySDErrorBar = 1, transparencySEMErrorBar = 1, trimViolinPlotBuilder = FALSE, value = "Plot 1", variableColorPlotBuilder = list(types = list(), value = ""), variableXPlotBuilder = list(types = list(), value = ""), variableYPlotBuilder = list(types = list(), value = ""), vjustCountValue = -0.5, vjustMeanValue = -0.5, vjustMedianValue = -0.5, vjustSumValue = -0.5, widthBoxplotPlotBuilder = 0.6, widthCI95ErrorBar = 0.3, widthLineBoxplotPlotBuilder = 0.8, widthMeanBar = 0.8, widthMedianBar = 0.8, widthPlotBuilder = 380, widthRangeErrorBar = 0.3, widthSDErrorBar = 0.3, widthSEMErrorBar = 0.3, widthSumBar = 0.8, widthSumDash = 0.8, widthWhiskersPlotBuilder = 0.3, xAxisLabelRenamer = list(), xAxisTitleSplit = "", xReferenceLine = "", xVarRM = list(types = list(), value = ""), yAxisLabelRenamer = list(), yAxisTitleSplit = "", yPositionPValue = 70, yReferenceLine = "")),
          columnWidthInput = "",
          fullRowSpecifications = list(),
          getCommonLegend = FALSE,
          labelDistance1 = 0.05,
          labelDistance2 = 0.95,
          labelSize = 18,
          layoutHeight = 500,
          layoutWidth = 500,
          plotHeight = 320,
          plotSpacing = 10,
          plotWidth = 480,
          relHeightWithinRowLayout = "",
          relativeHeight = "",
          rowSpecifications = list()) {

   defaultArgCalls <- formals(jaspDescriptives::jaspPlotBuilder)
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

   optionsWithFormula <- c("PlotBuilderTab", "fullRowSpecifications", "rowSpecifications")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDescriptives", "jaspPlotBuilder", "jaspPlotBuilder.qml", options, version, FALSE))
}
