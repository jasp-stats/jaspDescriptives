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

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL) {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

DescriptivesInternal <- function(jaspResults, dataset, options) {
  variables <- unlist(options$variables)
  splitName <- options$splitBy
  makeSplit <- splitName != ""
  numberMissingSplitBy <- 0

  if (is.null(dataset)) {
    temp <- .descriptivesReadData(options, variables, splitName)
    dataset         <- temp[["dataset"]]
    dataset.factors <- temp[["dataset.factors"]]
    isNominalText   <- temp[["isNominalText"]]
  }

  if (makeSplit && length(variables) > 0) {
    splitFactor <- dataset[[splitName]]
    splitLevels <- levels(splitFactor)
    # remove missing values from the grouping variable
    dataset <- dataset[!is.na(splitFactor), ]
    dataset.factors <- dataset.factors[!is.na(splitFactor), ]

    numberMissingSplitBy <- sum(is.na(splitFactor))

    # Actually remove missing values from the split factor
    splitFactor <- na.omit(splitFactor)
    # create a list of datasets, one for each level
    splitDat.factors <- split(dataset.factors[variables], splitFactor)
  }

  .descriptivesDescriptivesTable(dataset, dataset.factors, isNominalText, options, jaspResults, numberMissingSplitBy = numberMissingSplitBy)

  # Covariance matrix
  if (options[["covariance"]] || options[["correlation"]]) {
    if (is.null(jaspResults[["associationMatrix"]])) {
      cName <- ifelse(options[["splitBy"]] != "", "Matrices", "Matrix")
      jaspResults[["associationMatrix"]] <- createJaspContainer(gettextf("Association %s", cName))
      jaspResults[["associationMatrix"]]$dependOn(c("splitBy", "covariance", "correlation", "variables", "associationMatrixUse"))
      # jaspResults[["stemAndLeaf"]]$position <- 11
    }

    numericOrFactorVariables <- Filter(function(var) .descriptivesIsNumericColumn(dataset.factors, var), variables)

    if (length(variables) > 0L) {
      .descriptivesCovarianceTables(
        container = jaspResults[["associationMatrix"]],
        dataset   = if (makeSplit) splitDat.factors else dataset.factors,
        variables = numericOrFactorVariables,
        options   = options
      )
    }
  }

  # Frequency table
  if (options$frequencyTables) {
    if (is.null(jaspResults[["tables"]])) {
      jaspResults[["tables"]] <- createJaspContainer(gettext("Frequency Tables"))
      jaspResults[["tables"]]$dependOn(c("frequencyTables", "splitBy", "frequencyTablesMaximumDistinctValues"))
      jaspResults[["tables"]]$position <- 3
    }

    .descriptivesFrequencyTables(dataset.factors, options, jaspResults[["tables"]])
  }

  # Correlation plot
  if (options$correlationPlots && length(variables) > 1) {
    if (is.null(jaspResults[["matrixPlot"]])) {
      if (makeSplit) {
        jaspResults[["matrixPlot"]] <- createJaspContainer(title = gettext("Correlation plots"))
        corrPlot <- jaspResults[["matrixPlot"]]
        corrPlot$dependOn(c("correlationPlots", "splitBy", "variables"))

        for (i in seq_along(splitLevels))
          corrPlot[[splitLevels[i]]] <- .descriptivesMatrixPlot(splitDat.factors[[i]], options, splitLevels[i])

      } else {
        jaspResults[["matrixPlot"]] <- .descriptivesMatrixPlot(dataset.factors, options, gettext("Correlation plot")) # Create one plot
      }

      jaspResults[["matrixPlot"]]$position <- 6
    }
  }

  # Distribution plots
  if (options$distributionPlots) {
    if (is.null(jaspResults[["distributionPlots"]])) {
      jaspResults[["distributionPlots"]] <- createJaspContainer(gettext("Distribution Plots"))
      jaspResults[["distributionPlots"]]$dependOn(c(
        "variables", "distributionPlots", "splitBy", "distributionAndCorrelationPlotHistogramBinWidthType", "distributionAndCorrelationPlotDensity",
        "distributionAndCorrelationPlotRugMarks", "distributionAndCorrelationPlotHistogramManualNumberOfBins"
      ))
      jaspResults[["distributionPlots"]]$position <- 5
    }

    distPlots <- jaspResults[["distributionPlots"]]

    for (var in variables) {
      if (is.null(distPlots[[var]])) {
        if (makeSplit) {
          distPlots[[var]] <- .descriptivesFrequencyPlots(dataset = splitDat.factors, options = options, variable = var)
        } else {
          distPlots[[var]] <- .descriptivesFrequencyPlots(dataset = dataset.factors, options = options, variable = var)
        }
      }
    }
  }

  # Box plots
  if (options$boxPlot) {
    if (is.null(jaspResults[["boxPlot"]])) {
      jaspResults[["boxPlot"]] <- createJaspContainer(gettext("Boxplots"))
      jaspResults[["boxPlot"]]$dependOn(c("boxPlot", "splitBy"))
      jaspResults[["boxPlot"]]$position <- 7
    }

    splitPlots <- jaspResults[["boxPlot"]]

    for (var in variables) {
      if (is.null(splitPlots[[var]]) && .descriptivesIsNumericColumn(dataset.factors, var)) {
        splitPlots[[var]] <- .descriptivesSplitPlot(dataset = dataset, options = options, variable = var)
        splitPlots[[var]]$dependOn(optionContainsValue = list(variables = var))
      }
    }
  }

  # QQ plots
  if (options$qqPlot) {
    if (is.null(jaspResults[["QQPlots"]])) {
      jaspResults[["QQPlots"]] <- createJaspContainer(
        if (length(variables) > 1 || length(levels(dataset[[splitName]])) > 1) # there will be more than one Q-Q Plot
          gettext("Q-Q Plots")
        else # only one Q-Q Plot
          gettext("Q-Q Plot")
      )
      jaspResults[["QQPlots"]]$dependOn(c("qqPlot", "splitBy"))
      jaspResults[["QQPlots"]]$position <- 8
    }
    QQPlots <- jaspResults[["QQPlots"]]
    if (makeSplit) {
      qqSplitFactor <- dataset[[splitName]]
      if (length(qqSplitFactor) == 0L)
        return(createJaspPlot(error = gettext("Plotting is not possible: Variable only contains NA!"), dependencies = "splitBy"))

      # gives the different split values
      qqSplitLevels <- levels(qqSplitFactor)
      # remove missing values from the grouping variable
      dataset <- dataset[!is.na(qqSplitFactor), ]
      for (var in variables) {
        if (!is.null(QQPlots[[var]]) || !.descriptivesIsNumericColumn(dataset.factors, var))
          next

        deeperQQPlots <- createJaspContainer(paste0(var))
        deeperQQPlots$dependOn(optionContainsValue = list(variables = var))
        QQPlots[[var]] <- deeperQQPlots
        # splits dataset according to split values
        qqSplitData <- split(dataset, qqSplitFactor)
        for (lev in seq_along(qqSplitLevels)) {
          QQPlots[[var]][[paste0(var, lev)]] <- .descriptivesQQPlot(dataset = qqSplitData[[lev]], options = options, qqvar = var, levelName = qqSplitLevels[lev])
        }
      }
    } else { # no split
      for (var in variables) {
        if (is.null(QQPlots[[var]]) && .descriptivesIsNumericColumn(dataset.factors, var)) {
          QQPlots[[var]] <- .descriptivesQQPlot(dataset = dataset, options = options, qqvar = var)
        }
      }
    }
  }

  # Pie charts
  if (options$pieChart) {
    if (is.null(jaspResults[["pieCharts"]])) {
      jaspResults[["pieCharts"]] <- createJaspContainer(gettext("Pie charts"))
      jaspResults[["pieCharts"]]$dependOn(c("splitBy", "pieChart", "colorPalette"))
      jaspResults[["pieCharts"]]$position <- 9
    }

    piePlots <- jaspResults[["pieCharts"]]
    jaspGraphs::setGraphOption("palette", options[["colorPalette"]])
    for (var in variables) {
      # skip non-categorical variables
      if (is.double(dataset.factors[[var]]))
        next

      if (is.null(piePlots[[var]])) {
        piePlots[[var]] <- if (makeSplit) {
          .descriptivesPieChart(dataset = splitDat.factors, options = options, variable = var)
        } else {
          .descriptivesPieChart(dataset = dataset.factors, options = options, variable = var)
        }
      }
    }
  }

  # Stem and leaf
  if (options[["stemAndLeaf"]]) {
    if (is.null(jaspResults[["stemAndLeaf"]])) {
      jaspResults[["stemAndLeaf"]] <- createJaspContainer(gettext("Stem and Leaf"))
      jaspResults[["stemAndLeaf"]]$dependOn(c("splitBy", "stemAndLeaf", "stemAndLeafScale"))
      jaspResults[["stemAndLeaf"]]$position <- 11
    }

    numericOrFactorVariables <- Filter(function(var) .descriptivesIsNumericColumn(dataset.factors, var), variables)

    if (length(variables) > 0L) {
      .descriptivesStemAndLeafTables(
        container = jaspResults[["stemAndLeaf"]],
        dataset   = if (makeSplit) splitDat.factors else dataset.factors,
        variables = numericOrFactorVariables,
        options   = options
      )
    }
  }

  # Scatter plots
  if (options[["scatterPlot"]]) {
    if (is.null(jaspResults[["scatterPlots"]])) {
      jaspResults[["scatterPlots"]] <- createJaspContainer(gettext("Scatter Plots"))
      jaspResults[["scatterPlots"]]$dependOn(c(
        "splitBy", "scatterPlot", "scatterPlotGraphTypeAbove", "scatterPlotGraphTypeRight", "scatterPlotRegressionLine",
        "scatterPlotRegressionLineCi", "scatterPlotRegressionLineCiLevel", "scatterPlotRegressionLineType", "scatterPlotLegend",
        "colorPalette"
      ))
      jaspResults[["scatterPlots"]]$position <- 10
    }
    .descriptivesScatterPlots(jaspResults[["scatterPlots"]], dataset.factors, variables, splitName, options)
  }

  # Interval plots
  if (options$intervalPlot) {
    if (is.null(jaspResults[["IntervalPlots"]])) {
      jaspResults[["IntervalPlots"]] <- createJaspContainer(gettext("Interval plots"))
      jaspResults[["IntervalPlots"]]$dependOn(c("intervalPlot", "splitBy"))
      jaspResults[["IntervalPlots"]]$position <- 12
    }

    intervalPlots <- jaspResults[["IntervalPlots"]]

    for (var in variables) {
      if (is.null(intervalPlots[[var]]) && .descriptivesIsNumericColumn(dataset.factors, var)) {
        intervalPlots[[var]] <- .descriptivesIntervalPlot(dataset = dataset, options = options, variable = var)
        intervalPlots[[var]]$dependOn(optionContainsValue = list(variables = var))
      }
    }
  }

  # Dot plots
  if (options[["dotPlot"]]) {
    if (is.null(jaspResults[["DotPlots"]])) {
      jaspResults[["DotPlots"]] <- createJaspContainer(gettext("Dot Plots"))
      jaspResults[["DotPlots"]]$dependOn(c("splitBy", "dotPlot"))
      jaspResults[["DotPlots"]]$position <- 13
    }

    dotPlots <- jaspResults[["DotPlots"]]

    for (var in variables) {
      if (is.null(dotPlots[[var]])) {
        dotPlots[[var]] <- .descriptivesDotPlots(
          dataset = if (makeSplit) splitDat.factors else dataset.factors,
          options = options,
          variable = var
        )
      }
    }
  }

  # Heatmap
  if (options[["heatmapHorizontalAxis"]] != "" && options[["heatmapVerticalAxis"]] != "") {
    if (is.null(jaspResults[["heatmaps"]])) {
      jaspResults[["heatmaps"]] <- createJaspContainer(gettext("Heatmaps"))
      jaspResults[["heatmaps"]]$dependOn(c(
        "heatmapHorizontalAxis", "heatmapVerticalAxis",
        "heatmapDisplayValue", "heatmapTileWidthHeightRatio", "heatmapLegend",
        "heatmapStatisticContinuous", "heatmapStatisticDiscrete",
        "colorPalette", "splitBy", "variables", "heatmapPlot"
      ))
      jaspResults[["heatmaps"]]$position <- 14
    }

    .descriptivesHeatmaps(jaspResults[["heatmaps"]], dataset.factors, variables, options)
  }

  # Pareto plots
  if (options[["paretoPlot"]]) {
    if (is.null(jaspResults[["paretoPlots"]])) {
      jaspResults[["paretoPlots"]] <- createJaspContainer(gettext("Pareto Plots"))
      jaspResults[["paretoPlots"]]$dependOn(c("paretoPlot", "splitBy", "paretoPlotRule", "paretoPlotRuleCi"))
      jaspResults[["paretoPlots"]]$position <- 15
    }

    parPlots <- jaspResults[["paretoPlots"]]

    for (var in variables) {
      # skip non-categorical variables
      if (is.double(dataset.factors[[var]]))
        next

      if (is.null(parPlots[[var]])) {
        parPlots[[var]] <- if (makeSplit) {
          .descriptivesParetoPlots(splitDat.factors, var, options)
        } else {
          .descriptivesParetoPlots(dataset.factors, var, options)
        }
      }
    }
  }

  # Density plots
  if (options[["densityPlot"]]) {
    if (is.null(jaspResults[["densityPlot"]])) {
      jaspResults[["densityPlot"]] <- createJaspContainer(gettext("Density Plots"))
      jaspResults[["densityPlot"]]$dependOn(c(
        "densityPlot", "densityPlotSeparate", "densityPlotType", "customHistogramPosition",
        "colorPalette", "splitBy", "variables", "densityPlotTransparency"
      ))
      jaspResults[["densityPlot"]]$position <- 17
    }

    .descriptivesDensityPlots(jaspResults[["densityPlot"]], dataset.factors, variables, options)
  }

  # Likert plots
  if (options[["likertPlot"]] && !all(lapply(dataset.factors[variables], is.double))) {
    if (is.null(jaspResults[["likertPlot"]])) {
      jaspResults[["likertPlot"]] <- createJaspContainer(gettext("Likert Plots"))
      jaspResults[["likertPlot"]]$dependOn(c(
        "likertPlot", "splitBy", "variables",
        "likertPlotAssumeVariablesSameLevel", "likertPlotAdjustableFontSize"
      ))
      jaspResults[["likertPlot"]]$position <- 16
    }

    likPlots <- jaspResults[["likertPlot"]]

    for (var in variables) {
      # exclude non-categorical variables from dataframe
      if (is.numeric(dataset.factors[[var]])) {
        if (makeSplit) {
          for (i in seq_along(splitLevels))
            splitDat.factors[[i]] <- splitDat.factors[[i]][, !names(splitDat.factors[[i]]) %in% c(var), drop = FALSE]
        } else {
          dataset.factors <- dataset.factors[, !names(dataset.factors) %in% c(var), drop = FALSE]
        }
      }
    }

    if (makeSplit) {
      for (i in seq_along(splitLevels))
        likPlots[[splitLevels[i]]] <- .descriptivesLikertPlots(splitDat.factors[[i]], splitLevels[i], options)
    } else {
      jaspResults[["likertPlot"]] <- .descriptivesLikertPlots(dataset.factors, gettext("Likert Plots"), options)
    }
  }

  return()
}

.descriptivesReadData <- function(options, variables, splitName) {

  makeSplit <- splitName != ""
  dataset <- .readDataSetToEnd(columns.as.numeric = variables, columns.as.factor = if (makeSplit) splitName)
  dataset.factors <- jaspBase::readDataSetByVariableTypes(options, "variables")
  if (makeSplit)
    dataset.factors[[splitName]] <- dataset[[splitName]]

  allMissing <- \(x) all(is.na(x))
  missingAllAsNumeric <- vapply(dataset,         allMissing, FUN.VALUE = logical(1L))
  missingAllAsIs      <- vapply(dataset.factors, allMissing, FUN.VALUE = logical(1L))
  isNominalText       <- missingAllAsNumeric & !missingAllAsIs[names(missingAllAsNumeric)]

  return(list(dataset = dataset, dataset.factors = dataset.factors, isNominalText = isNominalText))
}

.descriptivesDescriptivesTable <- function(dataset, dataset.factors, isNominalText, options, jaspResults, numberMissingSplitBy = 0) {
  if (!is.null(jaspResults[["stats"]])) {
    return()
  } # The options for this table didn't change so we don't need to rebuild it

  wantsSplit <- options$splitBy != ""
  variables <- unlist(options$variables)
  equalGroupsNo <- options$quantilesForEqualGroupsNumber
  percentilesPercentiles <- unique(options$percentileValues)
  stats <- createJaspTable(gettext("Descriptive Statistics"))
  stats$transpose <- !options[["descriptivesTableTransposed"]] # the table is transposed by default
  stats$position <- 1

  if (numberMissingSplitBy)
    stats$addFootnote(message = gettextf("Excluded %1$i rows from the analysis that correspond to the missing values of the split-by variable %2$s", numberMissingSplitBy, options$splitBy))

  stats$dependOn(c(
    "splitBy", "variables", "quantilesForEqualGroupsNumber", "percentileValues", "mode", "median", "mean",
    "seMean", "sd", "coefficientOfVariation", "variance", "skewness", "kurtosis", "shapiroWilkTest",
    "range", "iqr", "mad", "madRobust", "minimum", "maximum", "sum", "quartiles", "quantilesForEqualGroups",
    "percentiles", "descriptivesTableTransposed", "valid", "missing", "meanCi", "meanCiLevel", "meanCiMethod",
    "sdCi", "sdCiLevel", "varianceCi", "varianceCiLevel", "ciBootstrapSamples"
  ))

  if (wantsSplit) {
    stats$transposeWithOvertitle <- TRUE
    stats$addColumnInfo(name = "Variable", title = "", type = "string")
    stats$addColumnInfo(name = "Level",    title = "", type = "string")
  } else {
    stats$addColumnInfo(name = "Variable", title = "", type = "string")
  }

  formattedMeanCiPercent <- format(100 * options[["meanCiLevel"]], digits = 3, drop0trailing = TRUE)
  formattedSdCiPercent <- format(100 * options[["sdCiLevel"]], digits = 3, drop0trailing = TRUE)
  formattedVarianceCiPercent <- format(100 * options[["varianceCiLevel"]], digits = 3, drop0trailing = TRUE)

  # only add overtitle for CIs if table is transposed, else describe CIs in title
  if (options[["descriptivesTableTransposed"]]) {
    meanCiOvertitle <- gettextf("%s%% Confidence Interval Mean", formattedMeanCiPercent)
    sdCiOvertitle <- gettextf("%s%% Confidence Interval Std. Dev.", formattedSdCiPercent)
    varianceCiOvertitle <- gettextf("%s%% Confidence Interval Variance", formattedVarianceCiPercent)
    meanCiUbTitle <- gettext("Upper")
    meanCiLbTitle <- gettext("Lower")
    sdCiUbTitle <- gettext("Upper")
    sdCiLbTitle <- gettext("Lower")
    varianceCiUbTitle <- gettext("Upper")
    varianceCiLbTitle <- gettext("Lower")
  } else {
    meanCiOvertitle <- NULL
    sdCiOvertitle <- NULL
    varianceCiOvertitle <- NULL
    meanCiUbTitle <- gettextf("%s%% CI Mean Upper", formattedMeanCiPercent)
    meanCiLbTitle <- gettextf("%s%% CI Mean Lower", formattedMeanCiPercent)
    sdCiUbTitle <- gettextf("%s%% CI Std. Dev. Upper", formattedSdCiPercent)
    sdCiLbTitle <- gettextf("%s%% CI Std. Dev. Lower", formattedSdCiPercent)
    varianceCiUbTitle <- gettextf("%s%% CI Variance Upper", formattedVarianceCiPercent)
    varianceCiLbTitle <- gettextf("%s%% CI Variance Lower", formattedVarianceCiPercent)
  }

  if (options$valid)                          stats$addColumnInfo(name="Valid",                       title=gettext("Valid"),                   type="integer")
  if (options$missing)                        stats$addColumnInfo(name="Missing",                     title=gettext("Missing"),                 type="integer")
  if (options$mode)                           stats$addColumnInfo(name="Mode",                        title=gettext("Mode"),                    type="number")
  if (options$median)                         stats$addColumnInfo(name="Median",                      title=gettext("Median"),                  type="number")
  if (options$mean)                           stats$addColumnInfo(name="Mean",                        title=gettext("Mean"), 				            type="number")
  if (options$seMean)                         stats$addColumnInfo(name="Std. Error of Mean",          title=gettext("Std. Error of Mean"),      type="number")
  if (options$meanCi) {                       stats$addColumnInfo(name="MeanCIUB",                    title=meanCiUbTitle,                      type="number", overtitle = meanCiOvertitle)
                                              stats$addColumnInfo(name="MeanCILB",                    title=meanCiLbTitle,                      type="number", overtitle = meanCiOvertitle)}
  if (options$sd)                             stats$addColumnInfo(name="Std. Deviation",              title=gettext("Std. Deviation"),          type="number")
  if (options$sdCi) {                         stats$addColumnInfo(name="SdCIUB",                      title=sdCiUbTitle,                        type="number", overtitle = sdCiOvertitle)
                                              stats$addColumnInfo(name="SdCILB",                      title=sdCiLbTitle,                        type="number", overtitle = sdCiOvertitle)}
  if (options$coefficientOfVariation)         stats$addColumnInfo(name="Coefficient of Variation",    title=gettext("Coefficient of variation"),type="number")
  if (options$mad)                            stats$addColumnInfo(name="MAD",                         title=gettext("MAD"),                     type="number")
  if (options$madRobust)                      stats$addColumnInfo(name="MAD Robust",                  title=gettext("MAD robust"),              type="number")
  if (options$iqr)                            stats$addColumnInfo(name="IQR",                         title=gettext("IQR"),                     type="number")
  if (options$variance)                       stats$addColumnInfo(name="Variance",                    title=gettext("Variance"),                type="number")
  if (options$varianceCi) {                   stats$addColumnInfo(name="VarianceCIUB",                title=varianceCiUbTitle,                  type="number", overtitle = varianceCiOvertitle)
                                              stats$addColumnInfo(name="VarianceCILB",                title=varianceCiLbTitle,                  type="number", overtitle = varianceCiOvertitle)}
  if (options$skewness) {                     stats$addColumnInfo(name="Skewness",                    title=gettext("Skewness"),                type="number")
                                              stats$addColumnInfo(name="Std. Error of Skewness",      title=gettext("Std. Error of Skewness"),  type="number") }
  if (options$kurtosis) {                     stats$addColumnInfo(name="Kurtosis",                    title=gettext("Kurtosis"),                type="number")
                                              stats$addColumnInfo(name="Std. Error of Kurtosis",      title=gettext("Std. Error of Kurtosis"),  type="number") }
  if (options$shapiroWilkTest) {              stats$addColumnInfo(name="Shapiro-Wilk",                title=gettext("Shapiro-Wilk"),            type="number")
                                              stats$addColumnInfo(name="P-value of Shapiro-Wilk",     title=gettext("P-value of Shapiro-Wilk"), type="pvalue") }
  if (options$range)                          stats$addColumnInfo(name="Range",                       title=gettext("Range"),                   type="number")
  if (options$minimum)                        stats$addColumnInfo(name="Minimum",                     title=gettext("Minimum"),                 type="number")
  if (options$maximum)                        stats$addColumnInfo(name="Maximum",                     title=gettext("Maximum"),                 type="number")

  if (options$quartiles) {
                                    stats$addColumnInfo(name="q1", title=gettext("25th percentile"), type="number")
                                    stats$addColumnInfo(name="q2", title=gettext("50th percentile"), type="number")
                                    stats$addColumnInfo(name="q3", title=gettext("75th percentile"), type="number")
  }

  if (options$quantilesForEqualGroups) { # I've read that there are several ways how to estimate percentiles so it should be checked if it match the SPSS way
    tempPercentNames <- 1 / equalGroupsNo * 1:(equalGroupsNo - 1) * 100

    for (i in seq_along(tempPercentNames))
      stats$addColumnInfo(name = paste0("eg", i), title = gettextf("%gth percentile", round(tempPercentNames[i], 2)), type = "number")
  }

  if (options$percentiles) {
    for (i in percentilesPercentiles) {
      if (i >= 0 && i <= 100) {
        stats$addColumnInfo(name = paste0("pc", i), title = gettextf("%gth percentile", i), type = "number")
      } else {
        .quitAnalysis(gettext("Error in Percentiles, all values should >=0 and <=100"))
      }
    }
  }

  if (options$sum)
    stats$addColumnInfo(name = "Sum", title = gettext("Sum"), type = "number")

  jaspResults[["stats"]] <- stats

  # lets just add footnotes once instead of a gazillion times..
  shouldAddNominalTextFootnote <- FALSE
  shouldAddModeMoreThanOnceFootnote <- FALSE
  shouldAddModeContinuousTreatedAsDiscreteFootnote <- FALSE

  # Find the number of levels to loop over
  if (wantsSplit) {
    split <- dataset[[options$splitBy]]
    splitLevels <- levels(split)
    nLevels <- length(levels(split))

    for (variable in variables) {
      for (l in seq_len(nLevels)) {
        column <- (if (isNominalText[variable]) dataset.factors else dataset)[[variable]][split == splitLevels[l]]
        subReturn <- .descriptivesDescriptivesTable_subFunction(column, list(Variable = variable, Level = splitLevels[l]), options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote, shouldAddModeContinuousTreatedAsDiscreteFootnote, jaspResults)

        shouldAddNominalTextFootnote                     <- subReturn$shouldAddNominalTextFootnote
        shouldAddModeMoreThanOnceFootnote                <- subReturn$shouldAddModeMoreThanOnceFootnote
        shouldAddModeContinuousTreatedAsDiscreteFootnote <- subReturn$shouldAddModeContinuousTreatedAsDiscreteFootnote

        stats$addRows(subReturn$resultsCol, rowNames = paste0(variable, l))

        if (subReturn$shouldAddIdenticalFootnote) {
          stats$addFootnote(
            message = gettext("All values are identical"),
            colNames = c("Skewness", "Kurtosis", "Shapiro-Wilk", "P-value of Shapiro-Wilk"),
            rowNames = paste0(variable, l)
          )
        }

        if (subReturn$shouldAddExplainEmptySet) {
          stats$addFootnote(
            message = gettextf("Infimum (minimum) of an empty set is %1$s, supremum (maximum) of an empty set is %2$s.", "\u221E", "-\u221E"),
            colNames = c("Minimum", "Maximum"),
            rowNames = paste0(variable, l)
          )
        }

        if(subReturn$shouldAddModeMoreThanOnceFootnote)
          stats$addFootnote(message  = gettext("More than one mode exists. For nominal and ordinal data, the first mode is reported. For continuous data, the mode with the highest density estimate is reported but multiple modes may exist. We recommend visualizing the data to check for multimodality."),
                            colNames = "Mode",
                            rowNames = variable)

        if (subReturn$shouldAddModeContinuousTreatedAsDiscreteFootnote)
          stats$addFootnote(message  = gettext("The mode is computed assuming that variables are discrete."),
                            colNames = "Mode",
                            rowNames = variable)
      }
    }
  } else { # we dont want to split
    for (variable in variables) {
      column <- (if (isNominalText[variable]) dataset.factors else dataset)[[variable]]
      subReturn <- .descriptivesDescriptivesTable_subFunction(column, list(Variable = variable), options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote, shouldAddModeContinuousTreatedAsDiscreteFootnote, jaspResults)

      shouldAddNominalTextFootnote                     <- subReturn$shouldAddNominalTextFootnote
      shouldAddModeMoreThanOnceFootnote                <- subReturn$shouldAddModeMoreThanOnceFootnote
      shouldAddModeContinuousTreatedAsDiscreteFootnote <- subReturn$shouldAddModeContinuousTreatedAsDiscreteFootnote

      stats$addRows(subReturn$resultsCol, rowNames = variable)

      if (subReturn$shouldAddIdenticalFootnote) {
        stats$addFootnote(
          message = gettext("All values are identical"),
          colNames = c("Skewness", "Kurtosis", "Shapiro-Wilk", "P-value of Shapiro-Wilk"),
          rowNames = variable
        )
      }

      if (subReturn$shouldAddExplainEmptySet)
        stats$addFootnote(message  = gettextf("Infimum (minimum) of an empty set is %1$s, supremum (maximum) of an empty set is %2$s.", "\u221E", "-\u221E"),
                          colNames = c("Minimum", "Maximum"),
                          rowNames = variable)

      if(subReturn$shouldAddModeMoreThanOnceFootnote)
        stats$addFootnote(message  = gettext("More than one mode exists. For nominal and ordinal data, the first mode is reported. For continuous data, the mode with the highest density estimate is reported but multiple modes may exist. We recommend visualizing the data to check for multimodality."),
                          colNames = "Mode",
                          rowNames = variable)

      if (subReturn$shouldAddModeContinuousTreatedAsDiscreteFootnote)
        stats$addFootnote(message  = gettext("The mode is computed assuming that variables are discreet."),
                          colNames = "Mode",
                          rowNames = variable)
    }
  }

  return(stats)
}

.descriptivesDescriptivesTable_subFunction <- function(column, resultsCol, options, shouldAddNominalTextFootnote, shouldAddModeMoreThanOnceFootnote, shouldAddModeContinuousTreatedAsDiscreteFootnote, jaspResults) {
  equalGroupsNo          <- options$quantilesForEqualGroupsNumber
  percentilesPercentiles <- unique(options$percentileValues)

  rows       <- length(column)
  na.omitted <- na.omit(column)

  if (base::is.factor(na.omitted) && (options$mode || options$median || options$mean || options$minimum || options$seMean || options$iqr || options$mad || options$madRobust || options$kurtosis || options$shapiroWilkTest || options$skewness || options$quartiles || options$variance || options$sd || options$coefficientOfVariation || options$percentiles || options$sum || options$maximum)) {
    shouldAddNominalTextFootnote <- TRUE
  }

  shouldAddIdenticalFootnote <- all(na.omitted[1] == na.omitted) && (options$skewness || options$kurtosis || options$shapiroWilkTest)

  valid <- length(na.omitted)
  resultsCol[["Valid"]]                   <- if (options$valid)   valid
  resultsCol[["Missing"]]                 <- if (options$missing) rows - length(na.omitted)

  resultsCol[["Median"]]                  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$median,            na.omitted, median)
  resultsCol[["Mean"]]                    <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$mean,              na.omitted, mean)
  resultsCol[["Std. Error of Mean"]]      <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$seMean, na.omitted, function(param) { sd(param)/sqrt(length(param))} )
  resultsCol[["Std. Deviation"]]          <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$sd, na.omitted, sd)
  resultsCol[["Coefficient of Variation"]]<- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$coefficientOfVariation,      na.omitted, function(param) { sd(param) / mean(param)})
  resultsCol[["MAD"]]                     <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$mad,               na.omitted, function(param) { mad(param, constant = 1) } )
  resultsCol[["MAD Robust"]]              <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$madRobust,         na.omitted, mad)
  resultsCol[["IQR"]]                     <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$iqr,               na.omitted, .descriptivesIqr)
  resultsCol[["Variance"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$variance,          na.omitted, var)
  resultsCol[["Kurtosis"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$kurtosis,          na.omitted, .descriptivesKurtosis)
  resultsCol[["Std. Error of Kurtosis"]]  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$kurtosis,          na.omitted, .descriptivesSEK)
  resultsCol[["Skewness"]]                <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$skewness,          na.omitted, .descriptivesSkewness)
  resultsCol[["Std. Error of Skewness"]]  <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$skewness,          na.omitted, .descriptivesSES)
  resultsCol[["Shapiro-Wilk"]]            <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$shapiroWilkTest,   na.omitted, function(param) { res <- try(shapiro.test(param)$statistic); if(isTryError(res)) NaN else res })
  resultsCol[["P-value of Shapiro-Wilk"]] <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$shapiroWilkTest,   na.omitted, function(param) { res <- try(shapiro.test(param)$p.value);   if(isTryError(res)) NaN else res })
  resultsCol[["Range"]]                   <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$range,             na.omitted, function(param) { range(param)[2] - range(param)[1]})
  resultsCol[["Minimum"]]                 <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$minimum,           na.omitted, min)
  resultsCol[["Maximum"]]                 <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$maximum,           na.omitted, max)
  resultsCol[["Sum"]]                     <- .descriptivesDescriptivesTable_subFunction_OptionChecker(options$sum,               na.omitted, sum)

  # validator for meanCi, sdCi, and varianceCi
  ciOptionChecker <- function(fun, na.omitted, options, jaspResults, variableName) {
    if (is.factor(na.omitted)) { # show empty cells when things cannot be computed
      return(list(upper = "", lower = ""))
    } else if (length(na.omitted) == 0L) { # show NaN when things can be computed in principle but not for this variable
      return(list(upper = NaN, lower = NaN))
    } else {
      return(fun(na.omitted, options, jaspResults, variableName))
    }
  }

  if (options[["meanCi"]]) {
    variableName <- if (is.null(resultsCol[["Level"]])) resultsCol[["Variable"]] else paste0(resultsCol[["Variable"]], resultsCol[["Level"]])
    meanCiResults <- ciOptionChecker(.descriptivesMeanCI, na.omitted, options, jaspResults, variableName)
    resultsCol[["MeanCIUB"]] <- meanCiResults$upper
    resultsCol[["MeanCILB"]] <- meanCiResults$lower
  }
  if (options[["sdCi"]]) {
    variableName <- if (is.null(resultsCol[["Level"]])) resultsCol[["Variable"]] else paste0(resultsCol[["Variable"]], resultsCol[["Level"]])
    sdCiResults <- ciOptionChecker(.descriptivesSdCI, na.omitted, options, jaspResults, variableName)
    resultsCol[["SdCIUB"]] <- sdCiResults$upper
    resultsCol[["SdCILB"]] <- sdCiResults$lower
  }
  if (options[["varianceCi"]]) {
    variableName <- if (is.null(resultsCol[["Level"]])) resultsCol[["Variable"]] else paste0(resultsCol[["Variable"]], resultsCol[["Level"]])
    varianceCiResults <- ciOptionChecker(.descriptivesVarianceCI, na.omitted, options, jaspResults, variableName)
    resultsCol[["VarianceCIUB"]] <- varianceCiResults$upper
    resultsCol[["VarianceCILB"]] <- varianceCiResults$lower
  }

  # should explain supremum and infimum of an empty set?
  shouldAddExplainEmptySet <- (options$minimum || options$maximum) && valid == 0

  if (options$mode) {

    # TODO: fix this after we have mixed columns (DvdB)
    if (FALSE && is.numeric(na.omitted)) { # scale data
      temp <- .desriptivesComputeModeContinuous(na.omitted)
      mode <- temp[["xValues"]][which.max(temp[["yValues"]])]

      shouldAddModeMoreThanOnceFootnote <- temp[["numModes"]] > 1L
    } else { # ordinal, nominal, or nominal text data
      tb <- table(na.omitted)
      mode <- names(tb[tb == max(tb)])[1] # use only the first mode
      mode <- if (!is.na(suppressWarnings(as.numeric(mode)))) {
        as.numeric(mode) # the most frequent value with mixed columns we can always show this one
      } else {
        which.max(tb) # the index of the most frequent value, not great for nominal text but better than nothing?
      }

      shouldAddModeMoreThanOnceFootnote <- length(mode) > 1L
    }

    shouldAddModeContinuousTreatedAsDiscreteFootnote <- is.numeric(na.omitted)

    resultsCol[["Mode"]] <- mode[1L]

  }

  if (options$quartiles) {
    if (base::is.factor(na.omitted) == FALSE) {
      resultsCol[["q1"]] <- quantile(na.omitted, c(.25), names = FALSE)
      resultsCol[["q2"]] <- quantile(na.omitted, c(.5), names = FALSE)
      resultsCol[["q3"]] <- quantile(na.omitted, c(.75), names = FALSE)
    } else {
      resultsCol[["q1"]] <- ""
      resultsCol[["q2"]] <- ""
      resultsCol[["q3"]] <- ""
    }
  } else {
    resultsCol[["q1"]] <- NULL
    resultsCol[["q2"]] <- NULL
    resultsCol[["q3"]] <- NULL
  }

  equalGroupNames <- NULL

  if (options$quantilesForEqualGroups)
    equalGroupNames <- paste0("eg", seq(equalGroupsNo - 1))

  percentileNames <- NULL

  if (options$percentiles)
    percentileNames <- paste0("pc", percentilesPercentiles)

  for (row in names(resultsCol)) {
    if (startsWith(row, "eg") && ((row %in% equalGroupNames) == FALSE)) {
      resultsCol[[row]] <- NULL
    }

    if (startsWith(row, "pc") && ((row %in% percentileNames) == FALSE)) {
      resultsCol[[row]] <- NULL
    }
  }

  if (base::is.factor(na.omitted) == FALSE) {
    if (options$quantilesForEqualGroups) {

      for (i in seq(equalGroupsNo - 1))
        resultsCol[[paste0("eg", i)]] <- quantile(na.omitted, c(i / equalGroupsNo), names=FALSE)

    }

    if (options$percentiles) {

      for (i in percentilesPercentiles)
        resultsCol[[paste0("pc", i)]] <- quantile(na.omitted, c(i / 100), names=FALSE)

    }
  } else {
    if (options$quantilesForEqualGroups) {

      for (i in seq(equalGroupsNo - 1))
        resultsCol[[paste0("eg", i)]] <- ""

    }

    if (options$percentiles) {

      for (i in percentilesPercentiles)
        resultsCol[[paste0("pc", i)]] <- ""

    }
  }

  return(list(
    resultsCol = resultsCol,
    shouldAddNominalTextFootnote = shouldAddNominalTextFootnote,
    shouldAddModeMoreThanOnceFootnote = shouldAddModeMoreThanOnceFootnote,
    shouldAddModeContinuousTreatedAsDiscreteFootnote = shouldAddModeContinuousTreatedAsDiscreteFootnote,
    shouldAddIdenticalFootnote = shouldAddIdenticalFootnote,
    shouldAddExplainEmptySet = shouldAddExplainEmptySet
  ))
}


.descriptivesDescriptivesTable_subFunction_OptionChecker <- function(optionToCheck, na.omitted, function_to_use) {
  if (!optionToCheck)
    return(NULL)

  if (base::is.factor(na.omitted))
    return("")

  return(function_to_use(na.omitted))
}

.descriptivesFrequencyTables <- function(dataset, options, freqTabs) {
  splitName <- options$splitBy
  wantsSplit <- splitName != ""
  splitFactor <- dataset[[splitName]]
  splitLevels <- levels(splitFactor)

  omittedVariables <- character()
  maximumDistinctValues <- options[["frequencyTablesMaximumDistinctValues"]]

  for (variable in options$variables) {
    column <- dataset[[variable]]

    noDistinctObservations <- if (is.factor(column)) nlevels(column) else length(unique(column))

    if (noDistinctObservations > maximumDistinctValues) {
      omittedVariables <- c(omittedVariables, variable)
      next
    }

    if(!is.null(freqTabs[[variable]]))
      next

    freqTab <- .descriptivesFrequencyTableMeta(variable, wantsSplit, splitName)

    freqTabs[[variable]] <- freqTab

    rows <- list()

    if (wantsSplit) {
      for (lev in splitLevels) { # also loop over the levels
        t <- table(column[splitFactor == lev])
        total <- sum(t)
        alltotal <- length(column[splitFactor == lev])
        cFreq <- 0

        for (i in seq_along(names(t))) {
          row                         <- list()
          row[["factor"]]             <- lev
          row[["Level"]]              <- names(t)[i]
          row[["Frequency"]]          <- as.vector(t[i])
          cFreq                       <- cFreq + row[["Frequency"]]
          row[["Percent"]]            <- row[["Frequency"]]/alltotal*100
          row[["Valid Percent"]]      <- row[["Frequency"]]/total*100
          row[["Cumulative Percent"]] <- cFreq/total*100
          row[[".isNewGroup"]]        <- i==1
          rows[[length(rows) + 1]]    <- row
        }

        rows[[length(rows) + 1]] <- list(
          "factor"              = "",
          "Level"               = gettext("Missing"),
          "Frequency"           = alltotal - total,
          "Percent"             = (alltotal - total) / alltotal * 100,
          "Valid Percent"       = "",
          "Cumulative Percent"  = "",
          ".isNewGroup"         = FALSE
        )

        rows[[length(rows) + 1]] <- list(
          "factor"              = "",
          "Level"               = gettext("Total"),
          "Frequency"           = alltotal,
          "Percent"             = 100,
          "Valid Percent"       = "",
          "Cumulative Percent"  = "",
          ".isNewGroup"         = FALSE
        )
      }
    } else {
      t         <- table(column)
      total     <- sum(t)
      cFreq     <- 0
      alltotal  <- length(column)

      for (lev in names(t)) {
        row                         <- list()
        row[["Level"]]              <- lev
        row[["Frequency"]]          <- as.numeric(t[lev])
        cFreq                       <- cFreq + row[["Frequency"]]
        row[["Percent"]]            <- row[["Frequency"]]/alltotal*100
        row[["Valid Percent"]]      <- row[["Frequency"]]/total*100
        row[["Cumulative Percent"]] <- cFreq/total*100
        rows[[length(rows) + 1]]    <- row
      }

      rows[[length(rows) + 1]] <- list(
        "Level"               = gettext("Missing"),
        "Frequency"           = alltotal - total,
        "Percent"             = (alltotal - total) / alltotal * 100,
        "Valid Percent"       = "",
        "Cumulative Percent"  = ""
      )

      rows[[length(rows) + 1]] <- list(
        "Level"               = gettext("Total"),
        "Frequency"           = alltotal,
        "Percent"             = 100,
        "Valid Percent"       = "",
        "Cumulative Percent"  = ""
      )
    }

    freqTab$addRows(rows)
    freqTab$status <- "complete"
  }

  if (length(omittedVariables) > 0L) {
    variableFirstTable <- setdiff(options[["variables"]], omittedVariables)
    if (length(variableFirstTable) > 0L) {
      freqTabs[[variableFirstTable[[1L]]]]$addFootnote(
        sprintf(
          ngettext(
            msg1 = "%2$s has more than %1$s distinct values and is omitted.",
            msg2 = "The following variables have more than %1$s distinct values and are omitted: %2$s.",
            n = length(omittedVariables)
          ),
          maximumDistinctValues,
          paste(omittedVariables, collapse = ", ")
        )
      )
      # It's actually only the footnote that introduces this dependency...
      freqTabs$dependOn(options = "variables")
    } else {
      # all variables have more distinct values than the maximum, add a dummy table and add a footnote
      freqTab <- .descriptivesFrequencyTableMeta(NULL, wantsSplit, splitName)
      freqTab$addFootnote(gettextf(
        "All variables have more than %1$s distinct values", maximumDistinctValues
      ))
      freqTabs[["dummy"]] <- freqTab
    }
  }
}

.descriptivesFrequencyTableMeta <- function(variable = NULL, wantsSplit, splitName) {
  if (is.null(variable)) {
    freqTab <- createJaspTable(gettext("Frequencies"))
  } else {
    freqTab <- createJaspTable(gettextf("Frequencies for %s", variable))
    freqTab$dependOn(optionContainsValue = list(variables = variable))
  }

  if (wantsSplit) freqTab$addColumnInfo(name = "factor", title = splitName, type = "string", combine = TRUE)

  freqTab$addColumnInfo(name="Level",               title=variable,                       type="string")
  freqTab$addColumnInfo(name="Frequency",           title=gettext("Frequency"),           type="integer")
  freqTab$addColumnInfo(name="Percent",             title=gettext("Percent"),             type="number", format="dp:1")
  freqTab$addColumnInfo(name="Valid Percent",       title=gettext("Valid Percent"),       type="number", format="dp:1")
  freqTab$addColumnInfo(name="Cumulative Percent",  title=gettext("Cumulative Percent"),  type="number", format="dp:1")
  return(freqTab)
}

.descriptivesMatrixPlot <- function(dataset, options, name) {
  variables <- unlist(options$variables)

  l <- length(variables)
  depends <- c("correlationPlots", "variables", "splitBy", "distributionAndCorrelationPlotHistogramBinWidthType", "distributionAndCorrelationPlotDensity", "distributionAndCorrelationPlotRugMarks", "distributionAndCorrelationPlotHistogramManualNumberOfBins")

  if (l == 0) #Nothing to plot
    return(NULL)

  variable.statuses <- vector("list", length(variables))

  for (i in seq_along(variables)) {
    errorMessage <- .descriptivesCheckPlotErrors(dataset, variables[i], obsAmount = "< 3")
    if (!is.null(errorMessage))
      variable.statuses[[i]]$error <- errorMessage
    else
      variable.statuses[[i]]$error <- ""
  }

  plotMat <- matrix(list(), l, l)
  axisBreaks <- vector("list", l)

  # minor adjustments to plot margin to avoid cutting off the x-axis labels
  adjMargin <- ggplot2::theme(plot.margin = ggplot2::unit(c(.25, .40, .25, .25), "cm"))

  oldFontSize <- jaspGraphs::getGraphOption("fontsize")
  jaspGraphs::setGraphOption("fontsize", .85 * oldFontSize)

  # first do the diagonal and store breaks
  for (row in seq_along(variables)) {
    if (variable.statuses[[row]]$error != "") {
      plotMat[[row, row]] <- .displayErrorDescriptives(errorMessage = variable.statuses[[row]]$error)
    } else {
      plotMat[[row, row]] <- .plotMarginal(
        column         = dataset[[variables[[row]]]],
        variableName   = NULL,
        displayDensity = options[["distributionAndCorrelationPlotDensity"]],
        rugs           = options[["distributionAndCorrelationPlotRugMarks"]],
        binWidthType   = options[["distributionAndCorrelationPlotHistogramBinWidthType"]],
        numberOfBins   = options[["distributionAndCorrelationPlotHistogramManualNumberOfBins"]],
        lwd            = .7
      )
      axisBreaks[[row]] <- jaspGraphs::getAxisBreaks(plotMat[[row, row]])
    }
  }

  # now do off-diagonal and use the same breaks
  for (row in seq_len(l - 1)) {
    for (col in seq(row + 1, l)) {
      if (variable.statuses[[row]]$error != "") {
        plotMat[[row, col]] <- .displayErrorDescriptives(errorMessage = variable.statuses[[row]]$error)
      } else if (variable.statuses[[col]]$error != "") {
        plotMat[[row, col]] <- .displayErrorDescriptives(errorMessage = variable.statuses[[col]]$error)
      } else {
        plotMat[[row, col]] <- .plotScatterDescriptives(
          xVar    = dataset[[variables[[col]]]],
          yVar    = dataset[[variables[[row]]]],
          xBreaks = axisBreaks[[col]]$x,
          yBreaks = axisBreaks[[row]]$x
        ) + adjMargin
      }
    }
  }

  jaspGraphs::setGraphOption("fontsize", oldFontSize)

  # slightly adjust the positions of the labels left and above the plots.
  labelPos <- matrix(.5, 4, 2)
  labelPos[1, 1] <- .55
  labelPos[4, 2] <- .65
  p <- jaspGraphs::ggMatrixPlot(
    plotList = plotMat, leftLabels = variables, topLabels = variables,
    scaleXYlabels = NULL, labelPos = labelPos
  )

  return(createJaspPlot(plot = p, width = 250 * l + 20, aspectRatio = 1, title = name, dependencies = depends))
}

# temporaryly copied from correlation.R in koenderks
#### histogram with density estimator ####
.plotMarginalCorDescriptives <- function(variable, xName = NULL, yName = gettext("Density")) {
  variable <- na.omit(variable)
  isNumeric <- !(is.factor(variable) || (is.integer(variable) && length(unique(variable)) <= 10))

  if (isNumeric) {
    p <- ggplot2::ggplot(data = data.frame(x = variable))
    h <- hist(variable, plot = FALSE)
    hdiff <- h$breaks[2L] - h$breaks[1L]
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(variable, h$breaks), min.n = 3)
    dens <- h$density
    yBreaks <- c(0, 1.2 * max(h$density))

    p <- p + ggplot2::geom_histogram(
      mapping  = ggplot2::aes(x = x, y = ..density..),
      binwidth = hdiff,
      fill     = "grey",
      col      = "black",
      size     = .3,
      center   = hdiff / 2,
      stat     = "bin"
    ) +
      ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
  } else {
    p <- ggplot2::ggplot(data = data.frame(x = factor(variable)))
    hdiff <- 1L
    xBreaks <- unique(variable)
    yBreaks <- c(0, max(table(variable)))

    p <- p + ggplot2::geom_bar(
      mapping  = ggplot2::aes(x = x),
      fill     = "grey",
      col      = "black",
      size     = .3,
      stat     = "count"
    ) +
      ggplot2::scale_x_discrete(name = xName, breaks = xBreaks)
  }

  yLim <- range(yBreaks)

  if (isNumeric) {
    density <- density(variable)
    p <- p + ggplot2::geom_line(
      data = data.frame(x = density$x, y = density$y),
      mapping = ggplot2::aes(x = x, y = y), lwd = .7, col = "black"
    )
  }

  thm <- ggplot2::theme(
    axis.ticks.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = -5, b = 0, l = 0))
  )
  p <- p +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = c("", ""), limits = yLim) +
    ggplot2::theme()
  return(jaspGraphs::themeJasp(p) + thm)
}

.poly.predDescriptives <- function(fit, plot = NULL, line = FALSE, xMin, xMax, lwd) {
  # create function formula
  f <- vector("character", 0)

  for (i in seq_along(coef(fit))) {
    if (i == 1) {
      temp <- paste(coef(fit)[[i]])
      f <- paste0(f, temp)
    }

    if (i > 1) {
      temp <- paste0("(", coef(fit)[[i]], ")*", "x^", i - 1)
      f <- paste(f, temp, sep = "+")
    }
  }

  x <- seq(xMin, xMax, length.out = 100)
  predY <- eval(parse(text = f))

  if (line == FALSE) {
    return(predY)
  }

  if (line) {
    plot <- plot + ggplot2::geom_line(data = data.frame(x, predY), mapping = ggplot2::aes(x = x, y = predY), size = lwd)
    return(plot)
  }
}


.plotScatterDescriptives <- function(xVar, yVar, xBreaks = NULL, yBreaks = NULL, xName = NULL, yName = NULL) {
  isNumericX <- !(is.factor(xVar) || (is.integer(xVar) && length(unique(xVar)) <= 10))
  isNumericY <- !(is.factor(yVar) || (is.integer(yVar) && length(unique(yVar)) <= 10))
  bothNumeric <- isNumericX && isNumericY
  d <- data.frame(x = xVar, y = yVar)
  d <- na.omit(d)

  if (!isNumericX)
    d$x <- as.factor(d$x)

  if (!isNumericY)
    d$y <- as.factor(d$y)

  if (is.null(xBreaks))
    xBreaks <- jaspGraphs::getPrettyAxisBreaks(d$x)

  fit <- NULL

  if (bothNumeric) {
    fit <- lm(y ~ poly(x, 1, raw = TRUE), d)
    lineObj <- .poly.predDescriptives(fit, line = FALSE, xMin = xBreaks[1], xMax = xBreaks[length(xBreaks)], lwd = lwd)
    rangeLineObj <- c(lineObj[1], lineObj[length(lineObj)])
    yLimits <- range(c(pretty(yVar)), rangeLineObj)

    if (!all(is.na(yLimits))) { # this is NA in case both x and y only contain a single unique value
      if (is.null(yBreaks) || yLimits[1L] <= yBreaks[1L] || yLimits[2L] >= yBreaks[length(yBreaks)]) {
        yBreaks <- jaspGraphs::getPrettyAxisBreaks(yLimits)
      }
    }
  } else if (is.null(yBreaks)) {
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(d$y)
  }

  p <- ggplot2::ggplot(data = d, ggplot2::aes(x = x, y = y)) +
    jaspGraphs::geom_point()

  if (bothNumeric) {
    xr <- range(xBreaks)
    dfLine <- data.frame(x = xr, y = rangeLineObj)
    p <- p + ggplot2::geom_line(data = dfLine, ggplot2::aes(x = x, y = y), size = .7, inherit.aes = FALSE)
  }

  if (isNumericX) {
    p <- p + ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks))
  } else {
    p <- p + ggplot2::scale_x_discrete(name = xName)
  }
  if (isNumericY) {
    p <- p + ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))
  } else {
    p <- p + ggplot2::scale_y_discrete(name = yName)
  }

  return(jaspGraphs::themeJasp(p))
}


### empty Plot with error message ###
.displayErrorDescriptives <- function(errorMessage = NULL) {
  df <- data.frame(
    x = 0, y = 1,
    # base R version of stringr::str_wrap that automatically places \n after about 40 characters (but does not split words)
    label = paste(strwrap(errorMessage, width = 40, prefix = "\n", initial = ""), collapse = "")
  )
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y, label = label)) +
    ggplot2::geom_text(size = .4 * jaspGraphs::getGraphOption("fontsize")) +
    jaspGraphs::getEmptyTheme()
  return(p)
}


.descriptivesFrequencyPlots <- function(dataset, options, variable) {
  if (options$splitBy != "") {
    # return a collection
    split <- names(dataset)

    plotResult <- createJaspContainer(title = variable)
    plotResult$dependOn(options = "splitBy", optionContainsValue = list(variables = variable))

    for (l in split) {
      plotResult[[l]] <- .descriptivesFrequencyPlots_SubFunc(dataset = dataset[[l]], variable = variable, width = options$plotWidth, height = options$plotHeight, displayDensity = options$distributionAndCorrelationPlotDensity, rugs = options$distributionAndCorrelationPlotRugMarks, title = l, binWidthType = options$distributionAndCorrelationPlotHistogramBinWidthType, numberOfBins = options$distributionAndCorrelationPlotHistogramManualNumberOfBins)
      plotResult[[l]]$dependOn(optionsFromObject = plotResult)
    }

    return(plotResult)
  } else {
    column <- dataset[[variable]]
    aPlot <- .descriptivesFrequencyPlots_SubFunc(dataset = dataset, variable = variable, width = options$plotWidth, height = options$plotHeight, displayDensity = options$distributionAndCorrelationPlotDensity, rugs = options$distributionAndCorrelationPlotRugMarks, title = variable, binWidthType = options$distributionAndCorrelationPlotHistogramBinWidthType, numberOfBins = options$distributionAndCorrelationPlotHistogramManualNumberOfBins)
    aPlot$dependOn(options = "splitBy", optionContainsValue = list(variables = variable))

    return(aPlot)
  }
}

.descriptivesFrequencyPlots_SubFunc <- function(dataset, variable, width, height, displayDensity, rugs, title, binWidthType, numberOfBins) {
  freqPlot <- createJaspPlot(title = title, width = width, height = height)

  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 3")
  column <- dataset[[variable]]
  column <- column[!is.na(column)]
  isDiscrete <- is.factor(column) || is.character(column)
  if (!is.null(errorMessage)) {
    freqPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
  } else if (length(column) > 0 && isDiscrete) {
    freqPlot$plotObject <- .barplotJASP(column, variable)
  } else if (length(column) > 0 && !isDiscrete) {
    freqPlot$plotObject <- .plotMarginal(column, variableName = variable, displayDensity = displayDensity, rugs = rugs, binWidthType = binWidthType, numberOfBins = numberOfBins)
  }

  return(freqPlot)
}

.descriptivesSplitPlot <- function(dataset, options, variable) {
  depends <- c("boxPlotColourPalette", "boxPlotViolin", "boxPlotBoxPlot", "boxPlotJitter", "boxPlotOutlierLabel")

  thePlot <- createJaspPlot(title = variable, width = options$plotWidth, height = options$plotHeight, dependencies = depends)

  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 1")
  if (!is.null(errorMessage)) {
    thePlot$setError(gettextf("Plotting not possible: %s", errorMessage))
  } else if (!(options$boxPlotViolin || options$boxPlotBoxPlot || options$boxPlotJitter)) {
    thePlot$setError(gettext("Plotting is not possible: No plot type selected!"))
  } else {
    # we need to know which index in y is related to which index in the actual data, so we should not forget the NAs somehow, lets make a list of indices.
    yWithNA         <- dataset[[variable]]
    y               <- na.omit(dataset[[variable]])
    yIndexToActual  <- y
    yWithNAIndex    <- 1
    yNoNAIndex      <- 1

    while (yWithNAIndex <= length(yWithNA)) {
      if (!is.na(yWithNA[[yWithNAIndex]])) {
        yIndexToActual[[yNoNAIndex]] <- row.names(dataset)[[yWithNAIndex]]
        yNoNAIndex <- yNoNAIndex + 1
      }

      yWithNAIndex <- yWithNAIndex + 1
    }

    if (is.null(dataset[[options$splitBy]])) {
      group     <- factor(rep("", length(y)))
      xlab      <- "Total"
      boxWidth  <- 0.2
      vioWidth  <- 0.3
    } else {
      group     <- as.factor(dataset[[options$splitBy]])[!is.na(dataset[[variable]])]
      xlab      <- options$splitBy
      boxWidth  <- 0.4
      vioWidth  <- 0.6
    }

    plotDat <- data.frame(group = group, y = y)
    row.names(plotDat) <- yIndexToActual

    # Identify outliers to label. Note that ggplot uses the unchangeable quantiles(type=7),
    # if we ever change the quantile type then the boxplot needs to be overwritten with stat_summary(geom='boxplot')
    plotDat$outlier <- FALSE

    for (level in levels(plotDat$group)) {
      v <- plotDat[plotDat$group == level, ]$y
      quantiles <- quantile(v, probs = c(0.25, 0.75))
      obsIQR <- quantiles[2] - quantiles[1]
      plotDat[plotDat$group == level, ]$outlier <- v < (quantiles[1] - 1.5 * obsIQR) | v > (quantiles[2] + 1.5 * obsIQR)
    }

    plotDat$label <- ifelse(plotDat$outlier, row.names(plotDat), "")

    if (options[["boxPlotColourPalette"]]) {
      thePlot$dependOn("colorPalette") # only add color as dependency if the user wants it
      palette <- options[["colorPalette"]]
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = group, y, fill = group)) +
        jaspGraphs::scale_JASPfill_discrete(palette) +
        jaspGraphs::scale_JASPcolor_discrete(palette)
    } else {
      p <- ggplot2::ggplot(plotDat, ggplot2::aes(x = group, y, fill = group)) +
        ggplot2::scale_fill_manual(values = rep("grey", nlevels(group))) +
        ggplot2::scale_colour_manual(values = rep("grey", nlevels(group)))
    }

    if (options[["boxPlotViolin"]]) {
      p <- p + ggplot2::geom_violin(trim = FALSE, size = 0.75, scale = "width", width = vioWidth)
    }

    if (options[["boxPlotBoxPlot"]]) {
      # if we add jittered data points, don't show outlier dots
      outlierShape <- if (options[["boxPlotJitter"]]) NA else 19
      p <- p +
        ggplot2::stat_boxplot(geom = "errorbar", size = 0.75, width = boxWidth / 2) +
        ggplot2::geom_boxplot(size = 0.75, outlier.size = 2, width = boxWidth, outlier.shape = outlierShape)
    }

    if (options[["boxPlotJitter"]]) {
      p <- p + ggplot2::geom_jitter(ggplot2::aes(fill = group, alpha = 0.5), size = 3, shape = 21, stroke = 1, position = ggplot2::position_jitter(width = 0.05, height = 0))
      if (options[["boxPlotColourPalette"]])
        p <- p + jaspGraphs::scale_JASPfill_discrete(palette)
      else
        p <- p + ggplot2::scale_fill_manual(values = rep("grey", nlevels(group)))
    }

    if (options[["boxPlotOutlierLabel"]]) {
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = label), hjust = -1, min.segment.length = 99)
    }

    ### Theming & Cleaning
    yBreaks <- if (options[["boxPlotViolin"]]) {
      jaspGraphs::getPrettyAxisBreaks(range(unlist(tapply(y, group, function(x) range(x, density(x)$x)), use.names = FALSE)))
    } else {
      jaspGraphs::getPrettyAxisBreaks(p[["data"]][["y"]])
    }
    yLimits <- range(yBreaks)

    p <- p +
      ggplot2::xlab(xlab) +
      ggplot2::scale_y_continuous(name = variable, breaks = yBreaks, limits = yLimits) +
      jaspGraphs::geom_rangeframe(sides = "l") +
      jaspGraphs::themeJaspRaw()

    thePlot$plotObject <- p
  }
  return(thePlot)
}

.descriptivesIntervalPlot <- function(dataset, options, variable) {
  thePlot <- createJaspPlot(title = variable)

  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 1")
  if (!is.null(errorMessage)) {
    thePlot$setError(gettextf("Plotting not possible: %s", errorMessage))
  } else {
    y <- na.omit(dataset[[variable]])
  }

  if (is.null(dataset[[options$splitBy]])) {
    group <- factor(rep("", length(y)))
    xlab <- gettext("Total")
  } else {
    group <- as.factor(dataset[[options$splitBy]])[!is.na(dataset[[variable]])]
    xlab <- options$splitBy
  }

  plotDat <- data.frame(group = group, y = y)

  cdata <- aggregate(y ~ group, data = plotDat, function(x) {
    mu <- mean(x)
    err <- qnorm(0.975) * (sd(x) / sqrt(length(x)))
    c(
      mean = mu,
      lower = mu - err,
      upper = mu + err
    )
  })
  df <- cbind.data.frame(group = factor(cdata[["group"]]), cdata[["y"]])

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(df[["lower"]], df[["upper"]]))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = mean, ymin = lower, ymax = upper)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(width = .1) +
    ggplot2::xlab(xlab) +
    ggplot2::scale_y_continuous(name = variable, breaks = yBreaks, limits = range(yBreaks)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  thePlot$plotObject <- p

  return(thePlot)
}

.plotMarginal <- function(column, variableName,
                          rugs = FALSE, displayDensity = FALSE, binWidthType = c("doane", "fd", "scott", "sturges", "manual"),
                          numberOfBins = NA,
                          lwd = 1) {
  binWidthType <- match.arg(binWidthType)
  column <- as.numeric(column)
  variable <- na.omit(column)

  if (length(variable) == 0)
    return(NULL)

  if (binWidthType == "doane") { # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6 * (length(variable) - 2)) / ((length(variable) + 1) * (length(variable) + 3)))
    g1 <- mean(abs(variable)^3)
    k <- 1 + log2(length(variable)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k
  } else if (binWidthType == "fd" && nclass.FD(variable) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
    binWidthType <- 10000
  } else if (binWidthType == "manual") {
    binWidthType <- numberOfBins
  }


  h <- hist(variable, plot = FALSE, breaks = binWidthType)

  if (!displayDensity) {
    yhigh <- max(h$counts)
  } else {
    dens <- density(variable)
    yhigh <- max(max(h$density), max(dens$y))
  }

  xticks <- base::pretty(c(variable, h$breaks), min.n = 3)

  if (!displayDensity) {
    p <-
      jaspGraphs::drawAxis(
        xName = variableName, yName = gettext("Counts"), xBreaks = xticks,
        yBreaks = base::pretty(c(0, h$counts)), force = TRUE, xLabels = xticks
      )
  } else {
    p <-
      jaspGraphs::drawAxis(
        xName = variableName, yName = gettext("Density"), xBreaks = xticks,
        yBreaks = c(0, 1.05 * yhigh), force = TRUE, yLabels = NULL,
        xLabels = xticks
      )
  }


  if (displayDensity) {
    p <- p +
      ggplot2::geom_histogram(
        data    = data.frame(variable),
        mapping = ggplot2::aes(x = variable, y = ..density..),
        breaks  = h[["breaks"]],
        fill    = "grey",
        col     = "black",
        size    = .7
      ) +
      ggplot2::geom_line(
        data    = data.frame(x = dens$x, y = dens$y),
        mapping = ggplot2::aes(x = x, y = y),
        lwd     = lwd,
        col     = "black"
      )
  } else {
    p <- p +
      ggplot2::geom_histogram(
        data     = data.frame(variable),
        mapping  = ggplot2::aes(x = variable, y = ..count..),
        breaks   = h[["breaks"]],
        fill     = "grey",
        col      = "black",
        size     = .7
      )
  }

  if (rugs)
    p <- p + ggplot2::geom_rug(data = data.frame(variable), mapping = ggplot2::aes(x = variable), sides = "b")

  # JASP theme
  p <- jaspGraphs::themeJasp(p,
    axisTickWidth = .7,
    bty = list(type = "n", ldwX = .7, lwdY = 1)
  )
  # TODO: Fix jaspgraphs axis width X vs Y. See @vandenman.

  if (displayDensity)
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(p)
}

.descriptivesDotPlots <- function(dataset, options, variable) {
  if (options$splitBy != "") {
    # return a collection
    levelsSplitFactor <- names(dataset)

    plotContainer <- createJaspContainer(title = variable)
    plotContainer$dependOn(optionContainsValue = list(variables = variable))

    for (level in levelsSplitFactor)
      plotContainer[[level]] <- .descriptivesDotPlots_SubFunc(dataset = dataset[[level]], variable = variable, title = level)

    return(plotContainer)
  } else {
    dotplot <- .descriptivesDotPlots_SubFunc(dataset = dataset, variable = variable, title = variable)
    dotplot$dependOn(optionContainsValue = list(variables = variable))

    return(dotplot)
  }
}

.descriptivesDotPlots_SubFunc <- function(dataset, variable, title) {
  dotPlot <- createJaspPlot(title = title)
  x <- na.omit(dataset[[variable]])
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    dotPlot$setError(gettext("No non-missing values!"))
    return(dotPlot)
  }

  dotsize <- 1

  if (is.factor(x)) {
    tb <- as.data.frame(table(x))
    scaleX <- ggplot2::scale_x_discrete(limits = factor(tb[, 1L]))

    # so the geom_dotplot is very weird and the dot sizes are independent of the y-axis.
    # hence to avoid exceeding the y-axis, we need to make the dots smaller...
    # see also the many issues on the ggplot repo about the dotplot...
    # for example https://github.com/tidyverse/ggplot2/issues/2203
    # this post provides alternatives we should consider for the current implementation
    # https://stackoverflow.com/questions/53697235/ggplot-dotplot-what-is-the-proper-use-of-geom-dotplot
    maxFreq <- max(tb[["Freq"]])
    if (maxFreq >= 17L)
      dotsize <- 17 / maxFreq

  } else {

    xBreaks <- jaspGraphs::getPrettyAxisBreaks(x)
    scaleX <- ggplot2::scale_x_continuous(breaks = xBreaks, limits = range(xBreaks))

    if (length(unique(x)) == 1)
      dotsize <- .03
  }

  p <- ggplot2::ggplot(data = data.frame(x = x), ggplot2::aes(x = x)) +
    ggplot2::geom_dotplot(binaxis = "x", stackdir = "up", fill = "grey", dotsize = dotsize) +
    ggplot2::xlab(variable) +
    ggplot2::ylab(NULL) +
    scaleX +
    jaspGraphs::geom_rangeframe(sides = "b") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  dotPlot$plotObject <- p

  return(dotPlot)
}

.barplotJASP <- function(column, variable) {
  tb <- as.data.frame(table(column))
  p <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
    ggplot2::xlab(variable) +
    ggplot2::ylab(gettext("Counts")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(plot.margin = ggplot2::margin(5))

  return(p)
}

.descriptivesKurtosis <- function(x) {
  # Kurtosis function as in SPSS:
  # http://www.ats.ucla.edu/stat/mult_pkg/faq/general/kurtosis.htm
  # http://en.wikipedia.org/wiki/Kurtosis#Estimators_of_population_kurtosis

  n <- length(x)
  s4 <- sum((x - mean(x))^4)
  s2 <- sum((x - mean(x))^2)
  v <- s2 / (n - 1)
  a <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  b <- s4 / (v^2)
  c <- (-3 * (n - 1)^2) / ((n - 2) * (n - 3))

  kurtosis <- a * b + c

  return(kurtosis)
}

.descriptivesIqr <- function(x) {
  # Interquartile range based on the stats package
  return(stats::IQR(x))
}

.descriptivesSkewness <- function(x) {
  # Skewness function as in SPSS (for samlpes spaces):
  # http://suite101.com/article/skew-and-how-skewness-is-calculated-in-statistical-software-a231005

  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  z <- (x - m) / s # z scores
  a <- n / ((n - 1) * (n - 2))

  skewness <- sum(z^3) * a

  return(skewness)
}

.descriptivesSES <- function(x) {
  # Standard Error of Skewness
  # Formula found http://web.ipac.caltech.edu/staff/fmasci/home/statistics_refs/SkewStatSignif.pdf

  n <- length(x)
  SES <- sqrt((6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))))

  return(SES)
}

.descriptivesSEK <- function(x) {
  # Standard Error of Kurtosis
  # Formula found http://web.ipac.caltech.edu/staff/fmasci/home/statistics_refs/SkewStatSignif.pdf

  n <- length(x)
  SEK <- 2 * .descriptivesSES(x) * sqrt((n^2 - 1) / ((n - 3) * (n + 5)))

  return(SEK)
}

.descriptivesMeanCI <- function(data, options, jaspResults, variableName) {
  ciWidth <- options[["meanCiLevel"]]
  if (options[["meanCiMethod"]] == "normalModel") {
    xBar <- mean(data)
    se <- sd(data) / sqrt(length(data))
    z <- qnorm((1 - ciWidth) / 2, lower.tail = FALSE)
    lowerBound <- xBar - z * se
    upperBound <- xBar + z * se
  } else if (options[["meanCiMethod"]] == "bootstrap") {
    stateContainerName <- paste0("bootstrapSamples", variableName)
    means <- .bootstrapStats(data, options, jaspResults, stateContainerName)$means
    percentiles <- (1 + c(-ciWidth, ciWidth)) / 2
    CIs <- quantile(means, probs = percentiles)
    lowerBound <- CIs[1]
    upperBound <- CIs[2]
  } else if (options[["meanCiMethod"]] == "oneSampleTTest") {
    ttestResult <- stats::t.test(x = data, conf.level = ciWidth)
    CIs <- ttestResult[["conf.int"]]
    lowerBound <- CIs[1]
    upperBound <- CIs[2]
  }
  return(list(
    "upper" = upperBound,
    "lower" = lowerBound
  ))
}

.descriptivesSdCI <- function(data, options, jaspResults, variableName) {
  ciWidth <- options[["sdCiLevel"]]
  stateContainerName <- paste0("bootstrapSamples", variableName)
  sds <- .bootstrapStats(data, options, jaspResults, stateContainerName)$sds
  percentiles <- (1 + c(-ciWidth, ciWidth)) / 2
  CIs <- quantile(sds, probs = percentiles)
  lowerBound <- CIs[1]
  upperBound <- CIs[2]
  return(list(
    "upper" = upperBound,
    "lower" = lowerBound
  ))
}

.descriptivesVarianceCI <- function(data, options, jaspResults, variableName) {
  ciWidth <- options[["varianceCiLevel"]]
  stateContainerName <- paste0("bootstrapSamples", variableName)
  variances <- .bootstrapStats(data, options, jaspResults, stateContainerName)$variances
  percentiles <- (1 + c(-ciWidth, ciWidth)) / 2
  CIs <- quantile(variances, probs = percentiles)
  lowerBound <- CIs[1]
  upperBound <- CIs[2]
  return(list(
    "upper" = upperBound,
    "lower" = lowerBound
  ))
}

.bootstrapStats <- function(data, options, jaspResults, stateContainerName) {
  if (!is.null(jaspResults[[stateContainerName]]$object))
    return(jaspResults[[stateContainerName]]$object)

  bootstrapSamples <- createJaspState()
  k <- options[["ciBootstrapSamples"]]
  means <- numeric(k)
  sds <- numeric(k)
  variances <- numeric(k)
  n <- length(data)
  for (i in seq_len(k)) {
    bootData <- sample(data, size = n, replace = TRUE)
    means[i] <- mean(bootData)
    sds[i] <- sd(bootData)
    variances[i] <- var(bootData)
  }
  bootstrapSamples$object <- list(means = means, sds = sds, variances = variances)
  jaspResults[[stateContainerName]] <- bootstrapSamples
  jaspResults[[stateContainerName]]$dependOn(options = c("ciBootstrapSamples"))
  return(list(means = means, sds = sds, variances = variances))
}

.desriptivesComputeModeContinuous <- function(x) {

  n <- 2^15
  bw <- stats::bw.nrd0(x)
  lowsup <- -Inf
  uppsup <- Inf
  # based on multimode::nmodes
  fn <- stats::density(x, bw = bw, n = n)
  z  <- c(1:(n - 1))
  re <- z[diff(fn$y) > 0]
  z2 <- c(1:length(re))
  se <- z2[diff(re) > 1]
  posic <- re[se]
  if (re[length(re)] < (n - 1)) {
    posic <- c(posic, re[length(re)])
  }
  posic <- posic[fn$x[posic] > lowsup]
  posic <- posic[fn$x[posic] < uppsup]
  num  <- length(posic)
  # end of multimode::nmodes
  xValues <- fn[["x"]][posic]
  yValues <- fn[["y"]][posic]
  return(list(
    numModes = num,
    xValues  = xValues,
    yValues  = yValues,
    bw       = bw
  ))
}


.descriptivesCovarianceTables <- function(container, dataset, variables, options) {

  # only make tables for which the checkbox is checked:
  for (thisAss in  c("Covariance", "Correlation")[c(options[["covariance"]], options[["correlation"]])]) {

    container[[thisAss]] <- createJaspContainer(title = thisAss)

    if (!is.data.frame(dataset)) { # dataset is split
      splitLevels <- names(dataset)
      for (split in splitLevels) {
        tableName <- paste0(thisAss, "_", split)
        if (is.null(container[[thisAss]][[tableName]])) {
          container[[thisAss]][[tableName]] <- .descriptivesCovarianceCreateSingleTable(dataset[[split]][variables], thisAss, options[["associationMatrixUse"]], split)
        }
      }
    } else {
      container[[thisAss]] <- .descriptivesCovarianceCreateSingleTable(dataset[variables], thisAss, options[["associationMatrixUse"]], thisAss)
    }
  }
}

.descriptivesCovarianceCreateSingleTable <- function(x, thisAss, useObs, title = "") {
  tb <- createJaspTable(title = title)

  tb$addColumnInfo(name = "colnames(x)", title = "", type = "string")
  for (var in colnames(x)) {
    tb$addColumnInfo(name = var, type = "number")
  }

  result <- try({
    if (thisAss == "Covariance") {
      cov(x, use = useObs)
    } else if (thisAss == "Correlation") {
      cor(x, use = useObs)
    }
  })

  if (isTryError(result)) {
    if (grepl(pattern = "missing observations", result[[1]])) {
      tb$setError(gettext("Association cannot be computed due to missing values. Try a different 'Use' setting."))
    } else {
      tb$setError(result[[1]])
    }
  }

  if (anyNA(result)) {
    tb$addFootnote(message = gettext("Some associations cannot be computed due to missing values. Try a different 'Use' setting."))
  }

  tb$setData(cbind(colnames(x), as.data.frame(result)))

  return(tb)
}


.descriptivesQQPlot <- function(dataset, options, qqvar, levelName = NULL) {
  # to put a subtitle if there is a split
  title <- qqvar
  if(!is.null(levelName))
    title <- levelName

  descriptivesQQPlot <- createJaspPlot(width = 400, aspectRatio = 1, title = title)

  if (!is.null(qqvar)) {
    errorMessage <- .descriptivesCheckPlotErrors(dataset, qqvar, obsAmount = "< 1")
    if (!is.null(errorMessage)) {
      descriptivesQQPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
    } else {
      varCol <- dataset[[qqvar]]
      varCol <- varCol[!is.na(varCol)]

      standResid <- as.data.frame(stats::qqnorm(varCol, plot.it = FALSE))
      standResid <- na.omit(standResid)

      # adapted from qqline
      x <- stats::qnorm(c(0.25, 0.75))
      y <- stats::quantile(varCol, probs = c(0.25, 0.75))
      slope <- diff(y) / diff(x)
      int <- y[1L] - slope * x[1L]

      xVar <- standResid$x
      yVar <- standResid$y

      # Format x ticks
      xBreaks <- jaspGraphs::getPrettyAxisBreaks(xVar)
      xLimits <- range(xBreaks)

      dfPoint <- data.frame(x = xVar, y = yVar)
      dfLine <- data.frame(x = xLimits, y = int + xLimits * slope)
      mapping <- ggplot2::aes(x = x, y = y)

      # Format y ticks -- ensure that the abline is shown entirely
      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(yVar, dfLine$y))
      yLimits <- range(yBreaks)

      # format axes labels
      xLabs <- jaspGraphs::axesLabeller(xBreaks)
      yLabs <- jaspGraphs::axesLabeller(yBreaks)

      p <- ggplot2::ggplot(data = dfPoint, mapping) +
        ggplot2::geom_line(data = dfLine, mapping, col = "darkred", size = 1) +
        jaspGraphs::geom_point() +
        ggplot2::scale_x_continuous(name = gettext("Theoretical Quantiles"), breaks = xBreaks, labels = xLabs, limits = xLimits) +
        ggplot2::scale_y_continuous(name = gettext("Sample Quantiles"), breaks = yBreaks, labels = yLabs, limits = yLimits) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw(axis.title.cex = jaspGraphs::getGraphOption("axis.title.cex"))

      descriptivesQQPlot$plotObject <- p
    }
  }

  if (is.null(levelName))
    descriptivesQQPlot$dependOn(optionContainsValue=list(variables=qqvar))
  return(descriptivesQQPlot)
}

.descriptivesPieChart <- function(dataset, options, variable) {
  if (options$splitBy != "") {
    # return a collection
    split <- names(dataset)

    plotResult <- createJaspContainer(title = variable)
    plotResult$dependOn(optionContainsValue = list(variables = variable))

    for (l in split) {
      plotResult[[l]] <- .descriptivesPieChart_SubFunc(
        dataset = dataset[[l]], variable = variable, width = options$plotWidth, height = options$plotHeight, title = l,
        palette = options[["colorPalette"]]
      )
      plotResult[[l]]$dependOn(optionsFromObject = plotResult)
    }

    return(plotResult)
  } else {
    aPlot <- .descriptivesPieChart_SubFunc(
      dataset = dataset, variable = variable, width = options$plotWidth, height = options$plotHeight, title = variable,
      palette = options[["colorPalette"]]
    )
    aPlot$dependOn(options = "splitBy", optionContainsValue = list(variables = variable))

    return(aPlot)
  }
}

.descriptivesPieChart_SubFunc <- function(dataset, variable, width, height, title, palette) {
  pieChart <- createJaspPlot(title = title, width = width, height = height)

  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 3")
  column <- dataset[[variable]]
  column <- column[!is.na(column)]
  if (!is.null(errorMessage)) {
    pieChart$setError(gettextf("Plotting not possible: %s", errorMessage))
  } else if (length(column) > 0) {
    tb <- as.data.frame(table(column))
    pieChart$plotObject <- jaspGraphs::plotPieChart(tb[, 2], tb[, 1],
      legendName = variable,
      palette = palette
    )
  }

  return(pieChart)
}

.descriptivesScatterPlots <- function(jaspContainer, dataset, variables, split, options, name = NULL, dependOnVariables = TRUE) {
  jaspGraphs::setGraphOption("palette", options[["colorPalette"]])
  # omit NAs here so it also affects the split variable
  dataset <- na.omit(dataset)

  if (!is.null(split) && split != "") {
    group <- dataset[, split]
    legendTitle <- split
  } else {
    group <- NULL
    split <- NULL
    legendTitle <- NULL
  }

  # remove non-numeric variables
  numerics <- sapply(variables, .descriptivesIsNumericColumn, dataset = dataset)
  variables <- variables[numerics]
  nvar <- length(variables)
  # Set's a message with instruction for user using jaspHtml
  if (nvar < 2L) {
    #   msg <- if (length(numerics) > 1L) { # basically all user variables have the wrong type...
    #     "These plots can only be shown for scale variables."
    #   } else {
    #     "Please enter two variables."
    #   }
    #   jaspContainer[["scatterplotMsg"]] <- createJaspHtml(text = msg, dependencies = "variables")
    return()
  }

  for (i in 1:(nvar - 1L)) {
    for (j in (i + 1L):nvar) {
      v1 <- variables[i]
      v2 <- variables[j]

      if (!is.null(name)) {
        plotName <- name
      } else {
        plotName <- paste(v1, "-", v2)
      }

      if (is.null(jaspContainer[[plotName]])) {
        scatterPlot <- createJaspPlot(title = plotName)
        if (dependOnVariables) {
          scatterPlot$dependOn(optionContainsValue = list(variables = c(v1, v2)))
        }

        errorMessage <- .descriptivesCheckPlotErrors(dataset, c(v1, v2, split), obsAmount = "< 2")
        if (is.null(errorMessage)) {

          scatterData <- apply(dataset[, c(v1, v2)], 2, as.numeric) # ensure nominal ints are numeric

          p <- try(jaspGraphs::JASPScatterPlot(
            x                 = scatterData[, v1],
            y                 = scatterData[, v2],
            group             = group,
            xName             = v1,
            yName             = v2,
            showLegend        = options[["scatterPlotLegend"]],
            addSmooth         = options[["scatterPlotRegressionLine"]],
            addSmoothCI       = options[["scatterPlotRegressionLineCi"]],
            smoothCIValue     = options[["scatterPlotRegressionLineCiLevel"]],
            forceLinearSmooth = options[["scatterPlotRegressionLineType"]] == "linear",
            plotAbove         = options[["scatterPlotGraphTypeAbove"]],
            plotRight         = options[["scatterPlotGraphTypeRight"]],
            legendTitle       = legendTitle
          ))

          if (isTryError(p)) {
            errorMessage <- .extractErrorMessage(p)
          }
        }

        if (!is.null(errorMessage)) {
          scatterPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
        } else {
          scatterPlot$plotObject <- p
        }

        jaspContainer[[plotName]] <- scatterPlot
      }
    }
  }
}

.descriptivesCheckPlotErrors <- function(dataset, vars, obsAmount) {
  errors <- .hasErrors(dataset, all.target = vars, message = "short", type = c("infinity", "observations"), observations.amount = obsAmount)
  if (!isFALSE(errors)) {
    return(errors$message)
  }

  return(NULL)
}

.descriptivesIsNumericColumn <- function(dataset, colName) {
  column <- na.omit(dataset[[colName]])
  if (is.factor(column) && !anyNA(suppressWarnings(as.numeric(levels(column))))) {
    return(TRUE)
  } else if (is.numeric(column)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

.descriptivesStemAndLeafTables <- function(container, dataset, variables, options, width = 120, atom = 1e-08) {
  # parameters for graphics::stem
  scale <- options[["stemAndLeafScale"]]
  # width and atom are set to R's default values. We could allow the user to set them but the R documentation is unclear as to what it does.

  if (!is.data.frame(dataset)) { # dataset is split

    splitLevels <- names(dataset)

    for (var in variables) {
      if (is.null(container[[var]])) {
        subcontainer <- createJaspContainer(title = var)
        subcontainer$dependOn(optionContainsValue = list(variables = var))
        container[[var]] <- subcontainer

        for (split in splitLevels) {
          tableName <- paste0("stem_and_leaf_", var, "_", split)
          subcontainer[[tableName]] <- .descriptivesStemAndLeafCreateSingleTable(dataset[[split]][[var]], split, scale, width, atom)
        }
      }
    }
  } else {
    for (var in variables) {
      tableName <- paste0("stem_and_leaf_", var)
      if (is.null(container[[tableName]])) {
        container[[tableName]] <- .descriptivesStemAndLeafCreateSingleTable(dataset[[var]], var, scale, width, atom)
        container[[tableName]]$dependOn(optionContainsValue = list(variables = var))
      }
    }
  }
}

.descriptivesStemAndLeafCreateSingleTable <- function(x, title, scale = 1, width = 80, atom = 1e-08) {
  tb <- createJaspTable(title = title)
  tb$addColumnInfo(name = "left", title = gettext("Stem"), type = "integer")
  tb$addColumnInfo(name = "sep", title = "", type = "separator")
  tb$addColumnInfo(name = "right", title = gettext("Leaf"), type = "string")

  if (length(na.omit(x)) < 2L) {
    tb$setError(gettext("A stem and leaf table could not be made because there are fewer than 2 non-missing observations"))
    return(tb)
  }

  # NOTE: graphics::stem is fast because it works in C, but it prints directly to the R output and returns NULL...
  # so we resort to capturing the string and manipulating it.
  # as.numeric ensures factors are handled correctly
  temp <- capture.output(graphics::stem(as.numeric(x), scale, width, atom))
  other <- temp[4:max(4, (length(temp) - 1L))]

  # parse the footnote so that we can translate it.
  # see https://github.com/wch/r-source/blob/c6710da21a5d869cd02889352483919ec3487000/src/library/graphics/src/stem.c#L105-L110 for details
  originalFootnote <- temp[2L]
  if (grepl("at the |", originalFootnote, fixed = TRUE)) {
    footnote <- gettext("The decimal point is at the |")
  } else {
    digits <- as.numeric(regmatches(originalFootnote, gregexpr("[[:digit:]]+", originalFootnote))[[1L]])

    footnote <- if (grepl("right", originalFootnote)) {
      sprintf(ngettext(
        digits,
        "The decimal point is %s digit to the right of the |",
        "The decimal point is %s digits to the right of the |"
      ), digits)
    } else {
      sprintf(ngettext(
        digits,
        "The decimal point is %s digit to the left of the |",
        "The decimal point is %s digits to the left of the |"
      ), digits)
    }
  }

  text <- strsplit(other, " | ", fixed = TRUE)
  left <- vapply(text, `[`, 1L, FUN.VALUE = character(1L))
  right <- vapply(text, function(x) if (length(x) == 1L) "" else x[2L], FUN.VALUE = character(1L))

  tb[["left"]] <- left
  tb[["sep"]] <- rep("|", length(left))
  tb[["right"]] <- right

  tb$addFootnote(footnote)

  return(tb)
}

.descriptivesHeatmaps <- function(container, dataset, variables, options) {
  axesNames <- c(options[["heatmapHorizontalAxis"]], options[["heatmapVerticalAxis"]])

  # we are not ready to plot
  if (length(variables) == 0 || isFALSE(options[["heatmapPlot"]]))
    return()

  axes <- .readDataSetToEnd(columns.as.factor = axesNames)

  for (i in seq_along(variables)) {
    variableName <- variables[[i]]
    variable <- dataset[[variableName]]

    if (options[["splitBy"]] == "") {
      .descriptivesCreateSingleHeatmap(container, axes, axesNames, variable, variableName, i, options)
    } else {
      container[[variableName]] <- createJaspContainer(variableName)
      splitBy <- dataset[, options[["splitBy"]]]
      groups <- levels(splitBy)

      for (g in seq_along(groups)) {
        activeCases <- groups[g] == splitBy
        .descriptivesCreateSingleHeatmap(container[[variableName]], axes[activeCases, ], axesNames, variable[activeCases], groups[g], g, options)
      }
    }
  }
}

.descriptivesCreateSingleHeatmap <- function(container, axes, axesNames, variable, plotName, position, options) {
  if (is.factor(variable)) {
    data <- .descriptivesHeatmapAggregateData(variable, axes, options[["heatmapStatisticDiscrete"]])
  } else {
    data <- .descriptivesHeatmapAggregateData(variable, axes, options[["heatmapStatisticContinuous"]])
  }

  nLevels <- c(nlevels(data[["horizontal"]]), nlevels(data[["vertical"]]))
  plotSize <- c(200 + nLevels * 20) * c(options[["heatmapTileWidthHeightRatio"]], 1)
  if (any(plotSize > 700)) {
    plotSize <- (plotSize / max(plotSize)) * 700
  }
  if (options[["heatmapLegend"]]) {
    plotSize <- plotSize + c(50, 50)
  }

  if (any(table(data[["horizontal"]], data[["vertical"]]) > 1)) {
    jaspPlot <- createJaspPlot(title = plotName, error = gettext("There must be a unique value per combination of levels of horizontal and vertical axis!"), position = position)
  } else {
    jaspPlot <- createJaspPlot(title = plotName, width = plotSize[1], height = plotSize[2], position = position)

    plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = horizontal, y = vertical, fill = value)) +
      ggplot2::geom_tile(color = "black", size = 1) +
      ggplot2::xlab(axesNames[1]) +
      ggplot2::ylab(axesNames[2]) +
      ggplot2::coord_fixed(ratio = 1 / options[["heatmapTileWidthHeightRatio"]])

    if (options[["heatmapDisplayValue"]]) {
      plot <- plot + ggplot2::geom_text(ggplot2::aes(x = horizontal, y = vertical, label = label),
        size = 8 * options[["heatmapDisplayValueRelativeTextSize"]]
      )
    }

    palette <- options[["colorPalette"]]
    plot <- plot + if (is.factor(data[["value"]])) {
      jaspGraphs::scale_JASPfill_discrete(palette)
    } else {
      jaspGraphs::scale_JASPfill_continuous(palette)
    }

    plot <- jaspGraphs::themeJasp(plot, legend.position = if (options[["heatmapLegend"]]) "right" else "none")

    jaspPlot$plotObject <- plot
  }

  container[[plotName]] <- jaspPlot
}

.descriptivesHeatmapAggregateData <- function(variable, groups, fun = c("identity", "mean", "median", "mode", "length")) {
  fun <- match.arg(fun)
  mode <- function(x) {
    levels <- levels(x)
    out <- names(which.max(table(x)))
    factor(out, levels = levels)
  }
  data <- switch(fun,
    identity = cbind(groups, variable = variable),
    mean     = stats::aggregate(x = variable, by = groups, mean),
    median   = stats::aggregate(x = variable, by = groups, median),
    mode     = stats::aggregate(x = variable, by = groups, mode),
    length   = stats::aggregate(x = variable, by = groups, length)
  )

  colnames(data) <- c("horizontal", "vertical", "value")
  data$label <- if (is.numeric(data$value)) round(data$value, digits = 2) else data$value

  return(data)
}

.descriptivesLikertPlots <- function(dataset, name, options) {
  if(ncol(dataset) == 0) return()
  variables <- names(dataset)

  if (options[["likertPlotAssumeVariablesSameLevel"]]) {
    plotR <- .descriptivesLikertPlotsFill(dataset, variables, name, options)
    return(plotR)
  } else {
    plotResult <- createJaspContainer(title = name)
    for (var in variables) {
      data <- dataset[, names(dataset) %in% c(var), drop = FALSE]
      plotResult[[var]] <- .descriptivesLikertPlotsFill(data, var, var, options)
    }
    return(plotResult)
  }
}

.descriptivesLikertPlotsFill <- function(dataset, variables, name, options) {
  leng <- length(dataset)
  depends <- c("likertPlot", "splitBy", "variables", "likertPlotAssumeVariablesSameLevel", "likertPlotAdjustableFontSize")

  likPlot <- createJaspPlot(title = name, dependencies = depends, width = 1300, height = if (leng == 1) 250 else 200 * (leng * 0.8))
  errorMessage <- .descriptivesCheckPlotErrors(dataset, variables, obsAmount = "< 2")
  if (!is.null(errorMessage)) {
    likPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
    return(likPlot)
  }

  # Likert Part: Preparing & summarize data in the likert format (% of levels per variable)
  nLevels <- nlevels(dataset[, 1])
  center <- (nLevels - 1) / 2 + 1
  lowRange <- 1:floor(center - 0.5)
  highRange <- ceiling(center + 0.5):nLevels

  if (!all(sapply(dataset, function(x) nlevels(x)) == nLevels)) {
    likPlot$setError(gettext("All categorical variables must have the same number of levels!"))
    return(likPlot)
  }
  if (center < 1.5) {
    likPlot$setError(gettext("Items must have 2 or more levels!"))
    return(likPlot)
  }

  # Summarizing item contribution and get clean data
  results <- data.frame()
  results <- data.frame(Response = seq_len(nLevels))
  for (i in seq_len(ncol(dataset))) {
    t <- table(dataset[, i])
    t <- (t / sum(t) * 100)
    results <- cbind(results, as.data.frame(t)[, 2])
    names(results)[ncol(results)] <- names(dataset)[i]
  }
  results <- as.data.frame(t(results))
  names(results) <- levels(dataset[, 1])
  results <- results[2:nrow(results), ]

  # Summarizing likert data: high, low, neutral %
  resultsTwo <- data.frame(
    Item = row.names(results),
    low = rep(NA, nrow(results)),
    neutral = rep(NA, nrow(results)),
    high = rep(NA, nrow(results))
  )
  resultsTwo$low <- if (length(lowRange) == 1) {
    results[, lowRange]
  } else {
    rowSums(results[, lowRange])
  }
  resultsTwo$high <- if (length(highRange) == 1) {
    results[, highRange]
  } else {
    rowSums(results[, highRange])
  }
  if (lowRange[length(lowRange)] + 1 != highRange[1]) {
    resultsTwo$neutral <- results[, (highRange[1] - 1)]
  }
  row.names(resultsTwo) <- seq_len(nrow(resultsTwo))

  results <- cbind(row.names(results), results)
  names(results)[1] <- "Item"
  row.names(results) <- seq_len(nrow(results))

  # Correcting for missing values in "results"
  for (i in 2:ncol(results)) {
    narows <- which(is.na(results[, i]))
    if (length(narows) > 0) {
      results[narows, i] <- 0
    }
  }
  # Correcting for missing values in "resultsTwo"
  narows <- which(is.na(resultsTwo$low))
  if (length(narows) > 0) {
    resultsTwo[narows, ]$low <- 0
  }
  narows <- which(is.na(resultsTwo$neutral))
  if (length(narows) > 0) {
    resultsTwo[narows, ]$neutral <- 0
  }
  narows <- which(is.na(resultsTwo$high))
  if (length(narows) > 0) {
    resultsTwo[narows, ]$high <- 0
  }

  lik <- list(results = results, levels = levels(dataset[, 1]), sum = resultsTwo)

  # Likert Plot Part:
  textSize <- 6
  textColor <- "black"
  yMin <- -100
  yMax <- 100
  yBuffer <- 5

  palette <- c(
    "#D8B365", "#E1C58B", "#EBD9B2", "#F5ECD8",
    "#D5ECEA", "#ACD9D5", "#83C6C0", "#5AB4AC"
  )
  cols <- scales::gradient_n_pal(palette, values = NULL)(seq(0, 1, length.out = nLevels))
  if (center %% 1 == 0) {
    cols[center] <- "grey90"
  }

  resultsLong <- stats::reshape(
    data = lik$results,
    idvar = "Item",
    v.name = c("value"),
    varying = c(names(lik$results[, 2:length(lik$results)])),
    times = c(names(lik$results[, 2:length(lik$results)])),
    timevar = "variable",
    new.row.names = seq_len(length(lik$results[2:length(lik$results)]) * length(lik$results$Item)),
    direction = "long"
  )

  resultsLong$Item <- factor(resultsLong$Item, levels = rev(lik$results$Item))
  orderLeg <- lik$levels # important for the correct legend sequence
  resultsLong$variable <- factor(resultsLong$variable, levels = orderLeg)

  # Make high, low, neutral values distinguishable
  rows <- which(resultsLong$variable %in% names(lik$results)[2:(length(lowRange) + 1)])
  resultsLong[rows, "value"] <- -1 * resultsLong[rows, "value"]
  if (center %% 1 == 0) {
    rowsMid <- which(resultsLong$variable %in% names(lik$results)[center + 1])
    tmp <- resultsLong[rowsMid, ]
    tmp$value <- tmp$value / 2 * -1
    resultsLong[rowsMid, "value"] <- resultsLong[rowsMid, "value"] / 2
    resultsLong <- rbind(resultsLong, tmp)
  }
  resultsLow <- resultsLong[resultsLong$value < 0, ]
  resultsHigh <- resultsLong[resultsLong$value > 0, ]

  p <- ggplot2::ggplot(resultsLong, ggplot2::aes(y = value, x = Item, group = Item)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey90") +
    ggplot2::geom_bar(data = resultsLow[nrow(resultsLow):1, ], ggplot2::aes(fill = variable), stat = "identity") +
    ggplot2::geom_bar(data = resultsHigh, ggplot2::aes(fill = variable), stat = "identity")

  names(cols) <- levels(resultsLong$variable)
  p <- p + ggplot2::scale_fill_manual("Response", breaks = names(cols), values = cols, drop = FALSE)

  p <- p + ggplot2::geom_text(
    data = lik$sum, # plot percent low
    y = yMin,
    ggplot2::aes(x = Item, label = paste0(round(low), "%")),
    size = textSize,
    hjust = 0.7,
    color = textColor
  ) +
    ggplot2::geom_text(
      data = lik$sum, # plot percent high
      y = 100,
      ggplot2::aes(x = Item, label = paste0(round(high), "%")),
      size = textSize,
      hjust = 0.3,
      color = textColor
    )

  if (nLevels %% 2 == 1) { # plot percent neutral
    p <- p + ggplot2::geom_text(
      data = lik$sum,
      y = 0,
      ggplot2::aes(x = Item, label = paste0(round(neutral), "%")),
      size = textSize,
      hjust = 0.5,
      color = textColor
    )
  }
  p <- p + ggplot2::coord_flip() +
    ggplot2::ylab("Percentage") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(labels = function(x) {
      return(abs(x))
    }, limits = c(yMin - yBuffer, yMax + yBuffer)) +
    ggplot2::theme(legend.position = "bottom") + ggplot2::guides(fill = ggplot2::guide_legend(byrow = TRUE)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(size = 1, color = "grey90", fill = NA)) +
    ggplot2::theme(text = ggplot2::element_text(size = 22.5), axis.title.x = ggplot2::element_text(size = 18))

  p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(
    size = switch(options[["likertPlotAdjustableFontSize"]],
      "small"  = 20,
      "medium" = 22.5,
      "large"  = 25
    )
  ))

  likPlot$plotObject <- p

  return(likPlot)
}

.descriptivesParetoPlots <- function(dataset, variable, options) {
  if (options[["splitBy"]] != "") {
    split <- names(dataset)

    plotResult <- createJaspContainer(title = variable)
    plotResult$dependOn(options = "splitBy", optionContainsValue = list(variables = variable))

    for (i in split) {
      plotResult[[i]] <- .descriptivesParetoPlots_SubFunc(dataset[[i]], variable, i, options)
      plotResult[[i]]$dependOn(optionsFromObject = plotResult)
    }

    return(plotResult)
  } else {
    pPlot <- .descriptivesParetoPlots_SubFunc(dataset, variable, variable, options)
    pPlot$dependOn(options = "splitBy", optionContainsValue = list(variables = variable))

    return(pPlot)
  }
}

.descriptivesParetoPlots_SubFunc <- function(dataset, variable, title, options) {
  parPlot <- createJaspPlot(title = title, width = options[["plotWidth"]], height = options[["plotHeight"]])

  errorMessage <- .descriptivesCheckPlotErrors(dataset, variable, obsAmount = "< 2")
  column <- dataset[[variable]]
  column <- column[!is.na(column)]
  if (!is.null(errorMessage)) {
    parPlot$setError(gettextf("Plotting not possible: %s", errorMessage))
  } else if (length(column) > 0) {
    parPlot$plotObject <- .descriptivesParetoPlots_Fill(column, variable, options)
  }

  return(parPlot)
}

.descriptivesParetoPlots_Fill <- function(column, variable, options) {
  tb <- as.data.frame(table(column))
  tb <- tb[order(tb$Freq, decreasing = TRUE), ]
  tb$cums <- cumsum(tb$Freq)
  tb$cums <- 100 * tb$cums / tail(tb$cums, n = 1)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, tb$Freq))
  scaleRight <- tail(tb$cums, n = 1) / tail(yBreaks, n = 1) # scaling factor for 2nd y axis & cumulative line

  # Ordered distribution chart
  p <- ggplot2::ggplot(data = data.frame(x = tb[, 1], y = tb[, 2]), ggplot2::aes(x = reorder(x, -y), y = y)) +
    ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
    ggplot2::xlab(variable) +
    ggplot2::ylab(gettext("Counts"))

  # Adding Pareto Line
  if (options[["paretoPlotRule"]]) {
    perc <- options[["paretoPlotRuleCi"]]
    absVal <- perc * 100 # to get absolute value of percentage
    colOrdered <- as.numeric(tb$column[order(tb$column, decreasing = FALSE)])
    interSec <- approx(colOrdered, tb$cums, n = 1000) # Finding x axis intersection at X%
    interY <- which.min(abs(interSec$y - absVal))
    interX <- interSec$x[interY]

    p <- p + ggplot2::geom_segment(ggplot2::aes(x = interX, xend = (nrow(tb) + 0.5), y = (max(yBreaks) * perc), yend = (max(yBreaks) * perc)),
      linetype = "dashed", color = "orange", size = 1.3
    )
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = interX, xend = interX, y = 0, yend = (max(yBreaks) * perc)),
      linetype = "dashed", color = "orange", size = 1.3
    )
  }

  # Adding cumulative percentages
  p <- p + ggplot2::geom_path(ggplot2::aes(y = tb[, 3] / scaleRight, group = 1), colour = "black", size = 1) +
    ggplot2::geom_point(ggplot2::aes(y = tb[, 3] / scaleRight, group = 1), colour = "steelblue", size = 3) +
    ggplot2::scale_y_continuous(
      breaks = yBreaks, limits = range(yBreaks), oob = scales::rescale_none,
      sec.axis = ggplot2::sec_axis(~ . * scaleRight, name = "Percentage (%)", breaks = seq(0, 100, 20))
    ) +
    jaspGraphs::geom_rangeframe(sides = "rbl") +
    jaspGraphs::themeJaspRaw() +
    ggplot2::theme(plot.margin = ggplot2::margin(5))
}

.descriptivesDensityPlots <- function(container, dataset, variables, options) {
  # we are not ready to plot
  if (length(variables) == 0)
    return()

  if (options[["densityPlotSeparate"]] != "") {
    separator <- .readDataSetToEnd(columns.as.factor = options[["densityPlotSeparate"]])
    separator <- separator[, , drop = TRUE]
  }

  for (i in seq_along(variables)) {
    if (!is.double(dataset[[i]])) next

    variableName <- variables[[i]]
    variable <- .readDataSetToEnd(columns.as.numeric = variableName)
    variable <- variable[, , drop = TRUE]

    data <- if (options[["densityPlotSeparate"]] != "") {
      data.frame(variable, separator)
    } else {
      data.frame(variable)
    }

    if (options[["splitBy"]] != "") {
      container[[variableName]] <- createJaspContainer(variableName)
      splitBy <- .readDataSetToEnd(columns.as.factor = options[["splitBy"]])
      data$split <- splitBy[, , drop = TRUE]
      data <- na.omit(data)
      groups <- levels(data$split)

      for (g in seq_along(groups)) {
        active <- groups[g] == data$split
        .descriptivesDensityPlotsFill(container[[variableName]], data[active, ], groups[g], variableName, g, options)
      }
    } else {
      data <- na.omit(data)
      .descriptivesDensityPlotsFill(container, data, variableName, variableName, i, options)
    }
  }
}

.descriptivesDensityPlotsFill <- function(container, data, plotName, axeName, position, options) {
  jaspGraphs::graphOptions(palette = options[["colorPalette"]])

  densPlot <- createJaspPlot(title = plotName, width = 480, height = 320, position = position)
  if (options[["densityPlotSeparate"]] != "" && any(table(data$separator) == 1)) {
    densPlot$setError(gettext("Levels within variable require at least two or more data points!"))
  } else if (options[["densityPlotType"]] == "density") {

    densPlot$plotObject <- jaspGraphs::jaspHistogram(x = data[["variable"]],
                                                     histogram = FALSE,
                                                     density = TRUE,
                                                     densityColor = TRUE,
                                                     densityShade = TRUE,
                                                     densityShadeAlpha = 1 - (options[["densityPlotTransparency"]]/100),
                                                     xName = axeName,
                                                     groupingVariableName = options[["densityPlotSeparate"]],
                                                     groupingVariable = data[["separator"]],
                                                     histogramPosition = options[["customHistogramPosition"]])


  } else if (options[["densityPlotType"]] == "histogram") {

    densPlot$plotObject <- jaspGraphs::jaspHistogram(x = data[["variable"]],
                                                     xName = axeName,
                                                     groupingVariableName = options[["densityPlotSeparate"]],
                                                     groupingVariable = data[["separator"]],
                                                     binWidthType = "sturges",
                                                     histogramPosition = options[["customHistogramPosition"]])
    if (options[["customHistogramPosition"]] == "identity")
      densPlot$plotObject$layers[[1]]$aes_params$alpha <- 1 - (options[["densityPlotTransparency"]]/100)
  }


  container[[plotName]] <- densPlot
}
