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

#' Descriptive Statistics
#'
#' Descriptives allows the user to obtain basic descriptive statistics, histograms and density plots, correlation plots, boxplots, and frequency tables.
#'
#' @param associationMatrixUse, How to deal with missing values?
#' \itemize{
#'   \item \code{"everything"} (default) : use all observations, resulting in NA when there are missing values.
#'   \item \code{"complete.obs"}: missing values are handled by casewise deletion (i.e., only use rows of the data set that are complete).
#'   \item \code{"pairwise.complete.obs"}: use all complete pairs of observations on those variables. This can result in covariance or correlation matrices which are not positive semi-definite.
#' }
#' @param boxPlot, For continuous variables, displays a boxplot. Optionally, the outliers are labelled. Outliers are based on the interquartile range (IQR), i.e., [25th percentile] - 1.5 × IQR and [75th percentile] + 1.5 × IQR. Can also display in color, and has selectable boxplot, violin, and jitter elements for displaying the distribution of the data. This can be split by a categorical variable such as experimental condition.
#'    Defaults to \code{FALSE}.
#' @param ciBootstrapSamples, the number of bootstrap samples to be used.
#' @param coefficientOfVariation, The Coefficient of variation gives us the relative dispersion of the data, in contrast to the standard deviation, which gives the absolute dispersion. For this purpose, the standard deviation is divided by the mean value, so that the unit is truncated away.
#'    Defaults to \code{FALSE}.
#' @param correlation, Pearson's correlation coefficient.
#'    Defaults to \code{FALSE}.
#' @param correlationPlots, Displays a matrix of plots between continuous variables, with scatterplots between the variables in the off-diagonal entries, and histograms and density plots in the diagonal entries. The line represents the fit of a 1st, 2nd, 3rd, or 4th order polynomial (the selection is based on the Bayesian information criterion; Schwarz, 1978).
#'    Defaults to \code{FALSE}.
#' @param covariance, Covariance value.
#'    Defaults to \code{FALSE}.
#' @param customHistogramPosition, Options for separate bins of the histogram
#' \itemize{
#'   \item \code{"identity"}: Identity: Bars are layered on top of each other, with transparency often used to distinguish overlapping data.
#'   \item \code{"dodge"}: Dodge: Bars are placed side-by-side, allowing for easy comparison of different categories within each bin.
#'   \item \code{"stack"}: Stack: Bars are stacked vertically, combining counts across categories within each bin.
#' }
#' @param densityPlot, visualizes data distributions, adapting to the type of variable selected. For scale variables, it generates histograms or density plots to show continuous data distributions. For categorical variables, it creates bar plots displaying counts or proportions of each category.
#'    Defaults to \code{FALSE}.
#' @param densityPlotCategoricalType, Display counts, proportions, or conditional proportions (conditional for each category on the x-axis).
#' \itemize{
#'   \item \code{"prop"}
#'   \item \code{"count"}
#'   \item \code{"condProp"}
#' }
#' @param densityPlotType, Whether to display a density plot or histogram.
#' \itemize{
#'   \item \code{"histogram"}
#'   \item \code{"density"}
#' }
#' @param descriptivesTableTransposed, Tranposes the main table
#'    Defaults to \code{FALSE}.
#' @param distributionPlots, For continuous variables, displays a histogram and the fit of a nonparametric density estimator. For nominal and ordinal variables, displays a frequency distribution.
#'    Defaults to \code{FALSE}.
#' @param frequencyTables, Displays a frequency table for each variable.
#'    Defaults to \code{FALSE}.
#' @param iqr, Interquartile range of the data points; 75th percentile - 25th percentile.
#'    Defaults to \code{FALSE}.
#' @param kurtosis, Kurtosis of the distribution of the data points.
#'    Defaults to \code{FALSE}.
#' @param likertPlot, Displays a horizontally stacked bar chart showing the contribution of levels within a variable in percent. Order of levels depends on defined order in the JASP data table. A legend below the graph provides an overview of levels and their respective colors in the graph.<ul><li>The y-axis represents the variables used, the x-axis represents the percentages. Percentage contribution of all lower-order (below the middle level) and higher-order (above the middle level) levels are displayed on their respective side of the graph.</li><li>The graph displays percentages on the x-axis as positive in both directions. Reason for the chosen display (in two directions) is the graphs usefulness in survey research where levels often follow a Likert based order (e.g., high - low, likely - unlikely, agreement - disagreement). Therefore, the graph contains a split between levels at their median.</li><li>The number of variable levels determines the number of layers displayed. Layers represent the percentage distribution of the levels of the variable under investigation.</li><li>If the variables contain an uneven amount of levels, the middle level is displayed as a grey block in the middle of the stacked bar with its percentage contribution on top.</li></ul>
#'    Defaults to \code{FALSE}.
#' @param mad, Median absolute deviation of the data points.
#'    Defaults to \code{FALSE}.
#' @param madRobust, Median absolute deviation of the data points, adjusted by a factor for asymptotically normal consistency.
#'    Defaults to \code{FALSE}.
#' @param maximum, Maximum value of the data points.
#'    Defaults to \code{TRUE}.
#' @param meanArithmetic, Arithmetic mean of the data points.
#'    Defaults to \code{TRUE}.
#' @param meanCiLevel, width of the confidence interval.
#' @param meanCiMethod, How should the confidence interval be computed? By default, we use a `T model`, which yields results identical to a one-sample t-test. Alternative options are a normal model ($\bar{x} \pm z_{95} \times SE$), or `Bootstrap`.
#' @param meanGeometric, Geometric mean of the data points; defined only for strictly positive variables.
#'    Defaults to \code{FALSE}.
#' @param meanHarmonic, Harmonic mean of the data points; defined only for strictly positive variables.
#'    Defaults to \code{FALSE}.
#' @param median, Median of the data points.
#'    Defaults to \code{FALSE}.
#' @param minimum, Minimum value of the data points.
#'    Defaults to \code{TRUE}.
#' @param mode, Mode of the data points; if more than one mode exists, only the first is reported. For nominal and ordinal data, the mode is the most frequent observed value. For continuous data, the mode is the value with highest density estimate (see 'Distribution Plots' -> 'Display density'). If a footnote about multimodality for continuous variables is reported, we recommend visualizing the data to check for multimodality.
#'    Defaults to \code{FALSE}.
#' @param paretoPlot, Displays the counts of each factor/level within the variable in a descending order. The y-axis represents the frequency (counts as grey bars) of each factor/level, the x-axis represents the factors/levels of the variable in an ordered sequence.<br>By default, a cumulative line is drawn indicating the proportional contribution of each factor. A second vertical axis to the right side of the graph scales with this cumulative line and represents percentages to enable the description of the cumulative line.<br>If "Pareto rule" is enabled, the two new lines enable a more precise assessment of factor/level contribution to the overall contribution (in percent) by using different input numbers.
#'    Defaults to \code{FALSE}.
#' @param percentiles, Displays the xth percentile; percentile values must be separated by comma.
#'    Defaults to \code{FALSE}.
#' @param quantilesForEqualGroups, Displays the cut points that divide the data into x equal groups; default is 4 equal groups.
#'    Defaults to \code{FALSE}.
#' @param quartiles, Displays the 25th, 50th, and 75th percentiles of the data points.
#'    Defaults to \code{FALSE}.
#' @param range, Range of the data points; maximum - minimum.
#'    Defaults to \code{FALSE}.
#' @param sd, Standard deviation of the data points.
#'    Defaults to \code{TRUE}.
#' @param sdCi, a confidence interval for the standard deviation based on bootstrap samples.
#'    Defaults to \code{FALSE}.
#' @param sdCiMethod, How should the confidence interval be computed? By default, we use an analytical approach (chi-square). The alternative option is `Bootstrap`
#' @param seMean, Standard error of the arithmetic mean.
#'    Defaults to \code{FALSE}.
#' @param shapiroWilkTest, Shapiro-Wilk test
#'    Defaults to \code{FALSE}.
#' @param skewness, Skewness of the distribution of the data points.
#'    Defaults to \code{FALSE}.
#' @param splitBy, Can be split by a categorical variable such as experimental condition.
#' @param stemAndLeaf, Displays the spread of a variable.<ul><li>Stem: the first digit(s).</li><li>Leaf: the first digit after the stem.</li></ul>
#'    Defaults to \code{FALSE}.
#' @param stemAndLeafScale, The scale parameter controls how much the table is expanded. For example, scale = 2 will cause the table to be roughly twice as long as the default (scale = 1).
#' @param sum, Sum of the data points.
#'    Defaults to \code{FALSE}.
#' @param valid, Number of valid observations
#'    Defaults to \code{TRUE}.
#' @param variables, All variables of interest.
#' @param variance, Variance of the data points.
#'    Defaults to \code{FALSE}.
#' @param varianceCi, a confidence interval for the variance based on bootstrap samples.
#'    Defaults to \code{FALSE}.
#' @param varianceCiMethod, How should the confidence interval be computed? By default, we use a analytical approach (chi-square). The alternative option is `Bootstrap`
Descriptives <- function(
    data = NULL,
    version = "0.95.1",
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
    densityPlotCategoricalType = "count",
    densityPlotSeparate = list(types = list(), value = ""),
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
    heatmapHorizontalAxis = list(types = list(), value = ""),
    heatmapLegend = FALSE,
    heatmapPlot = FALSE,
    heatmapStatisticContinuous = "mean",
    heatmapStatisticDiscrete = "mode",
    heatmapTileWidthHeightRatio = 1,
    heatmapVerticalAxis = list(types = list(), value = ""),
    intervalPlot = FALSE,
    iqr = FALSE,
    kurtosis = FALSE,
    likertPlot = FALSE,
    likertPlotAdjustableFontSize = "normal",
    likertPlotAssumeVariablesSameLevel = FALSE,
    mad = FALSE,
    madRobust = FALSE,
    maximum = TRUE,
    meanArithmetic = TRUE,
    meanCi = FALSE,
    meanCiLevel = 0.95,
    meanCiMethod = "oneSampleTTest",
    meanGeometric = FALSE,
    meanHarmonic = FALSE,
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
    scatterPlotRegressionLineType = "linear",
    sd = TRUE,
    sdCi = FALSE,
    sdCiLevel = 0.95,
    sdCiMethod = "chiSquaredModel",
    seMean = FALSE,
    shapiroWilkTest = FALSE,
    skewness = FALSE,
    splitBy = list(types = list(), value = ""),
    statisticsValuesAreGroupMidpoints = FALSE,
    stemAndLeaf = FALSE,
    stemAndLeafScale = 1,
    sum = FALSE,
    valid = TRUE,
    variables = list(types = list(), value = list()),
    variance = FALSE,
    varianceCi = FALSE,
    varianceCiLevel = 0.95,
    varianceCiMethod = "chiSquaredModel") {

  defaultArgCalls <- formals(jaspDescriptives::Descriptives)
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

  if (!is.null(formula)) {
    if (!inherits(formula, "formula")) {
      formula <- as.formula(formula)
    }
    options$formula <- jaspBase::jaspFormula(formula, data)
  }
  optionsWithFormula <- c("associationMatrixUse", "colorPalette", "densityPlotSeparate", "distributionAndCorrelationPlotHistogramBinWidthType", "heatmapHorizontalAxis", "heatmapVerticalAxis", "likertPlotAdjustableFontSize", "meanCiMethod", "sdCiMethod", "splitBy", "variables", "varianceCiMethod")
  for (name in optionsWithFormula) {
    if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

  return(jaspBase::runWrappedAnalysis("jaspDescriptives", "Descriptives", "Descriptives.qml", options, version, TRUE))
}
