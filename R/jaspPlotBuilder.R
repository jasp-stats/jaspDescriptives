
#
# Copyright (C) 2018 University of Amsterdam
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


# Main function ----
jaspPlotBuilderInternal <- function(jaspResults, dataset, options) {



  # Set title
  jaspResults$title <- "Plot builder"

  # Initialize options
  options <- .plotBuilderInitOptions(jaspResults, options)

  # Read dataset
  dataset <- .plotBuilderReadData(options)

  # (Optional) Additional error handling if needed
  # .plotBuilderErrorHandling(dataset, options)

  # Compute results
  plotResults <- .plotBuilderComputeResults(jaspResults, dataset, options)

  # Error handling (plotId presence, uniqueness)
  .plotBuilderErrorHandling(dataset, options)

  # Output individual plots
  .plotBuilderOutputIndividualPlots(jaspResults, options, plotResults)

  # Output available plot IDs
  .plotBuilderOutputAvailablePlotIDs(jaspResults, plotResults)

  # Output plot grid
  .plotBuilderOutputPlotGrid(jaspResults, options, plotResults)

  ## TRÁBLSÚTING_________________________________________________________
  return()
}


# Init functions ----
.plotBuilderInitOptions <- function(jaspResults, options) {
  # Initialize options if needed
  # Currently no specific initialization. Return options as-is.
  options
}


.plotBuilderReadData <- function(options) {

  # Collect all required columns
  allColumns <- unique(unlist(lapply(options$PlotBuilderTab, function(tab) {
    c(
      tab$variableXPlotBuilder,
      tab$variableYPlotBuilder,
      tab$variableColorPlotBuilder,
      tab$idVariablePlotBuilder,
      tab$variableRepeatedMeasures,
      tab$columnsvariableSplitPlotBuilder,
      tab$rowsvariableSplitPlotBuilder
    )
  })))

  # Read the dataset
  dataset <- .readDataSetToEnd(columns = allColumns)

  # Add row_id
  dataset <- dplyr::mutate(dataset, row_id = dplyr::row_number())

  # Initialize a list to store RM datasets per plot
  datasetRMList <- list()
  datasetNonRM  <- dataset

  # Process each RM plot separately
  for (tab in options$PlotBuilderTab) {

    if (identical(tab[["isRM"]], "RM")) {
      repeatedMeasuresCols <- tab[["variableRepeatedMeasures"]]
      pivotedXName         <- tab[["rmFactorText"]]
      pivotedYName         <- tab[["dimensionText"]]

      # Validate parameters
      if (!is.null(repeatedMeasuresCols) &&
          length(repeatedMeasuresCols) > 0 &&
          !is.null(pivotedXName) &&
          nzchar(pivotedXName) &&
          !is.null(pivotedYName) &&
          nzchar(pivotedYName)) {

        tempRM <- dataset |>
          dplyr::mutate(ID = dplyr::row_number()) |>
          tidyr::pivot_longer(
            cols      = dplyr::all_of(repeatedMeasuresCols),
            names_to  = pivotedXName,
            values_to = pivotedYName
          ) |>
          dplyr::mutate(!!rlang::sym(pivotedXName) := decodeColNames(!!rlang::sym(pivotedXName)))

        # Handle missing columns
        originalCols <- names(dataset)
        pivotedCols  <- names(tempRM)
        missingCols  <- setdiff(originalCols, pivotedCols)
        missingCols  <- setdiff(missingCols, "row_id")

        if (length(missingCols) > 0) {
          additionalData <- dplyr::select(dataset, row_id, dplyr::all_of(missingCols))

          tempRM <- dplyr::left_join(
            x  = tempRM,
            y  = additionalData,
            by = "row_id"
          )
        }

        # Store RM dataset separately using plotId as key
        plotId <- as.character(tab$plotId)
        datasetRMList[[plotId]] <- tempRM
      }
    }
  }

  return(list(datasetRMList = datasetRMList, datasetNonRM = datasetNonRM))
}

# Error handling function ----
.plotBuilderErrorHandling <- function(dataset, options) {

  .hasErrors(
    dataset = dataset,
    custom  = list(
      # Existing Check: Ensure all PlotBuilderTab have a plotId
      plotId_present = function() {
        # Extract plotIds from PlotBuilderTab
        plotIds <- sapply(options$PlotBuilderTab, function(tab) tab$plotId)
        # Identify missing plotIds
        missingPlotIds <- is.na(plotIds) | plotIds == ""
        if (any(missingPlotIds)) {
          return("PlotID is missing for one or more plots. Please enter a unique plotID for each plot to continue.")
        }
        return(NULL)  # No error
      },

      # Existing Check: Ensure all plotIds are unique
      plotId_unique = function() {
        # Extract plotIds from PlotBuilderTab
        plotIds <- sapply(options$PlotBuilderTab, function(tab) tab$plotId)
        # Check for duplicates
        duplicatedPlotIds <- any(duplicated(plotIds))
        if (duplicatedPlotIds) {
          return("Some plotID is duplicated. Please enter a unique plotID for each plot.")
        }
        return(NULL)  # No error
      }

    ),
    message              = "default",
    exitAnalysisIfErrors = TRUE
  )
}



# Results functions ----
.plotBuilderComputeResults <- function(jaspResults, dataset, options) {

  # If not computed yet, compute and store in state
  if (is.null(jaspResults[["statePlotResults"]])) {
    plotResults <- .plotBuilderPlots(dataset, options)
    jaspResults[["statePlotResults"]] <- createJaspState(plotResults)

    # Depend on PlotBuilderTab
    jaspResults[["statePlotResults"]]$dependOn("PlotBuilderTab")
  } else {
    plotResults <- jaspResults[["statePlotResults"]]$object
  }

  return(plotResults)
}


.plotBuilderPlots <- function(dataset, options) {

  datasetRMList <- dataset$datasetRMList
  datasetNonRM  <- dataset$datasetNonRM

  # Initialize the list to store plots
  updatedPlots <- list()

  # Creating plots -----
  for (tab in options[["PlotBuilderTab"]]) {

    plotId    <- as.character(tab[["plotId"]])
    localData <- if (identical(tab[["isRM"]], "RM")) {
      # Use the specific RM dataset for this plot
      datasetRMList[[plotId]]
    } else {
      datasetNonRM
    }

    # Remove error placeholder plot if present
    if (!is.null(updatedPlots[["scatterPlotError"]])) {
      updatedPlots[["scatterPlotError"]] <- NULL
    }

    # Alapértelmezett beállítások
    colorVar <- tab[["variableColorPlotBuilder"]]
    xVar     <- tab[["variableXPlotBuilder"]]
    yVar     <- tab[["variableYPlotBuilder"]]

    # Ha RM van
    if (identical(tab[["isRM"]], "RM")) {
      yVar <- tab[["dimensionText"]]

      if (tab[["useRMFactorAsFill"]]) {
        colorVar <- tab[["rmFactorText"]]
        xVar     <- tab[["variableXPlotBuilder"]]
      } else {
        colorVar <- tab[["variableColorPlotBuilder"]]
        xVar     <- tab[["rmFactorText"]]
      }
    }

    plotId     <- as.character(tab[["plotId"]])
    pointShape <- as.numeric(tab[["pointShapePlotBuilder"]])

    # Convert px to mm
    plotWidthPx  <- tab[["widthPlotBuilder"]]
    plotHeightPx <- tab[["heightPlotBuilder"]]
    plotWidthMm  <- plotWidthPx  * 25.4 / 96
    plotHeightMm <- plotHeightPx * 25.4 / 96

    # Build the base tidyplot call
    tidyplot_args <- list(data = localData)

    if (!is.null(xVar) && xVar %in% colnames(localData)) {
      tidyplot_args$x <- rlang::sym(xVar)
    }
    if (!is.null(yVar) && yVar %in% colnames(localData)) {
      tidyplot_args$y <- rlang::sym(yVar)
    }

    legend_position <- "none"

    if (!is.null(colorVar) && colorVar %in% colnames(localData)) {
      tidyplot_args$color <- rlang::sym(colorVar)
      legend_position <- tab[["legendPosistionPlotBuilder"]]
    }

    if (!is.null(tab[["colorByVariableY"]]) && tab[["colorByVariableY"]]) {
      tidyplot_args$color <- rlang::sym(yVar)
      legend_position <- "none"
    }

    if (!is.null(tab[["colorByVariableX"]]) && tab[["colorByVariableX"]]) {
      tidyplot_args$color <- rlang::sym(xVar)
      legend_position <- "none"
    }

    # Create the tidyplot object (tidyplots package)
    tidyplot_obj <- do.call(
      tidyplots::tidyplot,
      tidyplot_args
    )

    # Adjust color labels (tidyplots::rename_color_labels)----
    if (!is.null(tab[["colorLabelRenamer"]]) && length(tab[["colorLabelRenamer"]]) > 0) {
      label_map_color <- setNames(
        sapply(tab[["colorLabelRenamer"]], function(x) x$newColorLabel),
        sapply(tab[["colorLabelRenamer"]], function(x) x$originalColorLabel)
      )
      if (length(label_map_color) > 0) {
        tidyplot_obj <- tidyplots::rename_color_labels(tidyplot_obj, new_names = label_map_color)
      }
    }

    # Add data points (tidyplots::add_data_points_jitter)----
    if (tab[["addDataPoint"]]) {

      argList <- list(
        dodge_width   = tab[["pointDodgePlotBuilder"]],
        jitter_height = tab[["jitterhPlotBuilder"]],
        jitter_width  = tab[["jitterwPlotBuilder"]],
        size          = tab[["pointsizePlotBuilder"]],
        alpha         = tab[["alphaPlotBuilder"]],
        shape         = 21
      )

      if (tab[["blackOutlineDataPoint"]]) {
        argList$color <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_data_points_jitter,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add histogram (tidyplots::add_histogram)----
    if (tab[["addHistogram"]]) {

      argList <- list(
        bins  = tab[["binsPlotBuilder"]],
        alpha = tab[["alphaHistogramPlotBuilder"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_histogram,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add boxplot (tidyplots::add_boxplot)----
    if (tab[["addBoxplot"]]) {

      argList <- list(
        dodge_width    = tab[["dodgeBoxplotPlotBuilder"]],
        alpha          = tab[["alphaBoxplotPlotBuilder"]],
        box_width      = tab[["widthBoxplotPlotBuilder"]],
        linewidth      = tab[["widthLineBoxplotPlotBuilder"]],
        whiskers_width = tab[["widthWhiskersPlotBuilder"]],
        show_outliers  = tab[["outlierBoxplotPlotBuilder"]],
        outlier.size   = tab[["outlierSizeBoxplotPlotBuilder"]],
        coef           = tab[["outlierCoefBoxplotPlotBuilder"]]
      )

      if (tab[["blackOutlineBoxplot"]]) {
        argList$color <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_boxplot,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add violin (tidyplots::add_violin)----
    if (tab[["addViolin"]]) {

      # Beolvassuk a bemeneti szöveget
      input_text <- tab[["drawQuantilesViolinPlotBuilder"]]

      # Ha a szöveg nem tartalmazza a `c()` függvényt, akkor hozzáadjuk
      if (!grepl("^c\\(", input_text)) {
        input_text <- paste0("c(", input_text, ")")
      }

      # Értékeljük ki a kifejezést
      draw_quantiles <- eval(parse(text = input_text))

      argList <- list(
        draw_quantiles = draw_quantiles,
        alpha          = tab[["alphaViolinPlotBuilder"]],
        dodge_width    = tab[["dodgeViolinPlotBuilder"]],
        linewidth      = tab[["linewidthViolinPlotBuilder"]],
        trim           = tab[["trimViolinPlotBuilder"]],
        scale          = tab[["scaleViolinPlotBuilder"]]
      )

      if (tab[["blackOutlineViolin"]]) {
        argList$color <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_violin,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Count Bar (tidyplots::add_count_bar)----
    if (tab[["addCountBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeCountBar"]],
        width       = tab[["barwidthCountBar"]],
        alpha       = tab[["alphaCountBar"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_count_bar,
        c(list(tidyplot_obj), argList)
      )

      # the address of the axes is basically defined at the end of the script,
      # but for some reason it has to be valid for count geoms...

      titleValueY <- tab[["titleYPlotBuilder"]]
      if (!is.null(titleValueY) && titleValueY != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_y_axis_title(title = titleValueY)

      }

      titleValueX <- tab[["titleXPlotBuilder"]]
      if (!is.null(titleValueX) && titleValueX != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_x_axis_title(title = titleValueX)
      }

    }

    # Add Count Dash (tidyplots::add_count_dash)----
    if (tab[["addCountDash"]]) {

      argList <- list(
        dodge_width = tab[["dodgeCountDash"]],
        linewidth   = tab[["linewidthCountDash"]],
        width       = tab[["dashwidthCountDash"]],
        alpha       = tab[["alphaCountDash"]]
      )

      if (tab[["blackOutlineCountDash"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_count_dash,
        c(list(tidyplot_obj), argList)
      )

      # the address of the axes is basically defined at the end of the script,
      # but for some reason it has to be valid for count geoms...

      titleValueY <- tab[["titleYPlotBuilder"]]
      if (!is.null(titleValueY) && titleValueY != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_y_axis_title(title = titleValueY)

      }

      titleValueX <- tab[["titleXPlotBuilder"]]
      if (!is.null(titleValueX) && titleValueX != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_x_axis_title(title = titleValueX)
      }
    }

    # Add Count Line (tidyplots::add_count_line)----
    if (tab[["addCountLine"]]) {

      argList <- list(
        dodge_width = tab[["dodgeCountLine"]],
        linewidth   = tab[["linewidthCountLine"]],
        alpha       = tab[["alphaCountLine"]]
      )

      if (tab[["blackOutlineCountLine"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_count_line,
        c(list(tidyplot_obj), argList)
      )

      # the address of the axes is basically defined at the end of the script,
      # but for some reason it has to be valid for count geoms...

      titleValueY <- tab[["titleYPlotBuilder"]]
      if (!is.null(titleValueY) && titleValueY != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_y_axis_title(title = titleValueY)

      }

      titleValueX <- tab[["titleXPlotBuilder"]]
      if (!is.null(titleValueX) && titleValueX != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_x_axis_title(title = titleValueX)
      }
    }

    # Add Count Area (tidyplots::add_count_area)----
    if (tab[["addCountArea"]]) {

      argList <- list(
        dodge_width = tab[["dodgeCountArea"]],
        alpha       = tab[["alphaCountArea"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_count_area,
        c(list(tidyplot_obj), argList)
      )

      # the address of the axes is basically defined at the end of the script,
      # but for some reason it has to be valid for count geoms...

      titleValueY <- tab[["titleYPlotBuilder"]]
      if (!is.null(titleValueY) && titleValueY != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_y_axis_title(title = titleValueY)

      }

      titleValueX <- tab[["titleXPlotBuilder"]]
      if (!is.null(titleValueX) && titleValueX != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_x_axis_title(title = titleValueX)
      }
    }

    # Add Count Value (tidyplots::add_count_value)----
    if (tab[["addCountValue"]]) {

      argList <- list(
        fontsize = tab[["fontsizeCountValue"]],
        accuracy = eval(parse(text = tab[["accuracyCountValue"]])),
        alpha    = tab[["alphaCountValue"]],
        hjust    = tab[["hjustCountValue"]],
        vjust    = tab[["vjustCountValue"]]
      )

      if (tab[["blackOutlineCountValue"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_count_value,
        c(list(tidyplot_obj), argList)
      )

      # the address of the axes is basically defined at the end of the script,
      # but for some reason it has to be valid for count geoms...

      titleValueY <- tab[["titleYPlotBuilder"]]
      if (!is.null(titleValueY) && titleValueY != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_y_axis_title(title = titleValueY)

      }

      titleValueX <- tab[["titleXPlotBuilder"]]
      if (!is.null(titleValueX) && titleValueX != "") {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_x_axis_title(title = titleValueX)
      }

    }

    # Add Sum Bar (tidyplots::add_sum_bar)----
    if (tab[["addSumBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeSumBar"]],
        width       = tab[["widthSumBar"]],
        alpha       = tab[["alphaSumBar"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_sum_bar,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Sum Dash (tidyplots::add_sum_dash)----
    if (tab[["addSumDash"]]) {

      argList <- list(
        dodge_width = tab[["dodgeSumDash"]],
        width       = tab[["widthSumDash"]],
        linewidth   = tab[["linewidthSumDash"]],
        alpha       = tab[["alphaSumDash"]]
      )

      if (tab[["blackOutlineSumDash"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sum_dash,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Sum Value (tidyplots::add_sum_value)----
    if (tab[["addSumValue"]]) {

      argList <- list(
        fontsize = tab[["fontsizeSumValue"]],
        accuracy = eval(parse(text = tab[["accuracySumValue"]])),
        alpha    = tab[["alphaSumValue"]],
        hjust    = tab[["hjustSumValue"]],
        vjust    = tab[["vjustSumValue"]]
      )

      if (tab[["blackOutlineSumValue"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sum_value,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Sum Line (tidyplots::add_sum_line)----
    if (tab[["addSumLine"]]) {

      argList <- list(
        dodge_width = tab[["dodgeSumLine"]],
        linewidth   = tab[["linewidthSumLine"]],
        alpha       = tab[["alphaSumLine"]]
      )

      if (tab[["blackOutlineSumLine"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sum_line,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Sum Area (tidyplots::add_sum_area)----
    if (tab[["addSumArea"]]) {

      argList <- list(
        dodge_width = tab[["dodgeSumArea"]],
        linewidth   = tab[["linewidthSumArea"]],
        alpha       = tab[["alphaSumArea"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_sum_area,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Bar Stack Absolute (tidyplots::add_barstack_absolute)----
    if (tab[["addBarStackAbsolute"]]) {

      argList <- list(
        reverse = tab[["reverseBarStackAbsolute"]],
        alpha   = tab[["alphaBarStackAbsolute"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_barstack_absolute,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Bar Stack Relative (tidyplots::add_barstack_relative)----
    if (tab[["addBarStackRelative"]]) {

      argList <- list(
        reverse = tab[["reverseBarStackRelative"]],
        alpha   = tab[["alphaBarStackRelative"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_barstack_relative,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Area Stack Absolute (tidyplots::add_areastack_absolute)----
    if (tab[["addAreaStackAbsolute"]]) {

      argList <- list(
        reverse    = tab[["reverseAreaStackAbsolute"]],
        alpha      = tab[["alphaAreaStackAbsolute"]],
        linewidth  = tab[["linewidthAreaStackAbsolute"]],
        replace_na = tab[["replaceNaAreaStackAbsolute"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_areastack_absolute,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Area Stack Relative (tidyplots::add_areastack_relative)----
    if (tab[["addAreaStackRelative"]]) {

      argList <- list(
        reverse    = tab[["reverseAreaStackRelative"]],
        alpha      = tab[["alphaAreaStackRelative"]],
        linewidth  = tab[["linewidthAreaStackRelative"]],
        replace_na = tab[["replaceNaAreaStackRelative"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_areastack_relative,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Mean Bar (tidyplots::add_mean_bar)----
    if (tab[["addMeanBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMeanBar"]],
        alpha       = tab[["alphaMeanBar"]],
        width       = tab[["widthMeanBar"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_mean_bar,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Mean Dash (tidyplots::add_mean_dash)----
    if (tab[["addMeanDash"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMeanDash"]],
        alpha       = tab[["alphaMeanDash"]],
        width       = tab[["dashwidthMeanDash"]],
        linewidth   = tab[["linewidthMeanDash"]]
      )

      if (tab[["blackOutlineMeanDash"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_mean_dash,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Mean Line (tidyplots::add_mean_line)----
    if (tab[["addMeanLine"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMeanLine"]],
        alpha       = tab[["alphaMeanLine"]],
        linewidth   = tab[["linewidthMeanLine"]]
      )

      if (tab[["blackOutlineMeanLine"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_mean_line,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Mean Area (tidyplots::add_mean_area)----
    if (tab[["addMeanArea"]]) {

      argList <- list(
        group       = tab[["groupMeanArea"]],
        dodge_width = tab[["dodgeMeanArea"]],
        alpha       = tab[["alphaMeanArea"]],
        linewidth   = tab[["linewidthMeanArea"]]
      )

      tidyplot_obj <- do.call(
        tidyplots::add_mean_area,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Mean Value (tidyplots::add_mean_value)----
    if (tab[["addMeanValue"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMeanValue"]],
        alpha       = tab[["alphaMeanValue"]],
        fontsize    = tab[["fontsizeMeanValue"]],
        accuracy    = eval(parse(text = tab[["accuracyMeanValue"]])),
        hjust       = tab[["hjustMeanValue"]],
        vjust       = tab[["vjustMeanValue"]]
      )

      if (tab[["blackOutlineMeanValue"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_mean_value,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Median Bar (tidyplots::add_median_bar)----
    if (tab[["addMedianBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMedianBar"]],
        alpha       = tab[["alphaMedianBar"]],
        width       = tab[["widthMedianBar"]],
      )

      tidyplot_obj <- do.call(
        tidyplots::add_median_bar,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Median Dash (tidyplots::add_median_dash)----
    if (tab[["addMedianDash"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMedianDash"]],
        width       = tab[["dashwidthMedianDash"]],
        linewidth   = tab[["linewidthMedianDash"]],
        alpha       = tab[["alphaMedianDash"]]
      )

      if (tab[["blackOutlineMedianDash"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_median_dash,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Median Line (tidyplots::add_median_line)----
    if (tab[["addMedianLine"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMedianLine"]],
        linewidth   = tab[["linewidthMedianLine"]],
        alpha       = tab[["alphaMedianLine"]]
      )

      if (tab[["blackOutlineMedianLine"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_median_line,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Median Area (tidyplots::add_median_area)----
    if (tab[["addMedianArea"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMedianArea"]],
        linewidth   = tab[["linewidthMedianArea"]],
        alpha       = tab[["alphaMedianArea"]]
      )

      if (tab[["blackOutlineMedianArea"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_median_area,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Median Value (tidyplots::add_median_value)----
    if (tab[["addMedianValue"]]) {

      argList <- list(
        fontsize = tab[["fontsizeMedianValue"]],
        accuracy = eval(parse(text = tab[["accuracyMedianValue"]])),
        alpha    = tab[["alphaMedianValue"]],
        hjust    = tab[["hjustMedianValue"]],
        vjust    = tab[["vjustMedianValue"]]
      )

      if (tab[["blackOutlineMedianValue"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_median_value,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add SEM Error Bar (tidyplots::add_sem_errorbar)----
    if (tab[["addSEMErrorBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeSEMErrorBar"]],
        width       = tab[["widthSEMErrorBar"]],
        linewidth   = tab[["linewidthSEMErrorBar"]],
        alpha       = tab[["transparencySDErrorBar"]]
      )

      if (tab[["blackOutlineSEMErrorBar"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sem_errorbar,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Range Error Bar (tidyplots::add_range_errorbar)----
    if (tab[["addRangeErrorBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeRangeErrorBar"]],
        width       = tab[["widthRangeErrorBar"]],
        linewidth   = tab[["linewidthRangeErrorBar"]],
        alpha       = tab[["transparencyRangeErrorBar"]]
      )

      if (tab[["blackOutlineRangeErrorBar"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_range_errorbar,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add SD Error Bar (tidyplots::add_sd_errorbar)----
    if (tab[["addSDErrorBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeSDErrorBar"]],
        width       = tab[["widthSDErrorBar"]],
        linewidth   = tab[["linewidthSDErrorBar"]],
        alpha       = tab[["transparencySDErrorBar"]]
      )

      if (tab[["blackOutlineSDErrorBar"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sd_errorbar,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add 95% CI Error Bar (tidyplots::add_ci95_errorbar)----
    if (tab[["addCI95ErrorBar"]]) {

      argList <- list(
        dodge_width = tab[["dodgeCI95ErrorBar"]],
        width       = tab[["widthCI95ErrorBar"]],
        linewidth   = tab[["linewidthCI95ErrorBar"]],
        alpha       = tab[["transparencyCI95ErrorBar"]]
      )

      if (tab[["blackOutlineCI95ErrorBar"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_ci95_errorbar,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add SEM Ribbon (tidyplots::add_sem_ribbon)----
    if (tab[["addSemRibbon"]]) {

      argList <- list(
        dodge_width = 0.8,
        alpha       = tab[["alphaSemRibbon"]]
      )

      if (tab[["blackOutlineSemRibbon"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sem_ribbon,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Range Ribbon (tidyplots::add_range_ribbon)----
    if (tab[["addRangeRibbon"]]) {

      argList <- list(
        dodge_width = 0.8,
        alpha       = tab[["alphaRangeRibbon"]]
      )

      if (tab[["blackOutlineRangeRibbon"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_range_ribbon,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add SD Ribbon (tidyplots::add_sd_ribbon)----
    if (tab[["addSdRibbon"]]) {

      argList <- list(
        dodge_width = 0.8,
        alpha       = tab[["alphaSdRibbon"]]
      )

      if (tab[["blackOutlineSdRibbon"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sd_ribbon,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add 95% CI Ribbon (tidyplots::add_ci95_ribbon)----
    if (tab[["addCi95Ribbon"]]) {

      argList <- list(
        dodge_width = 0.8,
        alpha       = tab[["alphaCi95Ribbon"]]
      )

      if (tab[["blackOutlineCi95Ribbon"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_ci95_ribbon,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Count Dot (tidyplots::add_count_dot)----
    if (tab[["addCountDot"]]) {

      argList <- list(
        dodge_width = tab[["dodgeCountDot"]],
        size        = tab[["sizeCountDot"]],
        alpha       = tab[["alphaCountDot"]],
        shape       = 21
      )

      if (tab[["blackOutlineCountDot"]]) {
        argList$color <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_count_dot,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Sum Dot (tidyplots::add_sum_dot)----
    if (tab[["addSumDot"]]) {

      argList <- list(
        dodge_width = tab[["dodgeSumDot"]],
        size        = tab[["sizeSumDot"]],
        alpha       = tab[["alphaSumDot"]],
        shape       = 21
      )

      if (tab[["blackOutlineSumDot"]]) {
        argList$color <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_sum_dot,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Mean Dot (tidyplots::add_mean_dot)----
    if (tab[["addMeanDot"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMeanDot"]],
        alpha       = tab[["alphaMeanDot"]],
        size        = tab[["sizeMeanDot"]],
        shape       = 21
      )

      if (tab[["blackOutlineMeanDot"]]) {
        argList$color <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_mean_dot,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add Median Dot (tidyplots::add_median_dot)----
    if (tab[["addMedianDot"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMedianDot"]],
        size        = tab[["sizeMedianDot"]],
        alpha       = tab[["alphaMedianDot"]],
        shape       = 21
      )

      if (tab[["blackOutlineMedianDot"]]) {
        argList$color <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_median_dot,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add curve fit (tidyplots::add_curve_fit)----
    if (tab[["addCurveFitPlotBuilder"]]) {

      argList <- list(
        method     = tab[["curvaFitMethod"]],
        linewidth  = tab[["linewidthCurveFit"]],
        alpha      = tab[["transparencyCurveFit"]]
      )

      if (tab[["blackOutlineCurveFit"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      tidyplot_obj <- do.call(
        tidyplots::add_curve_fit,
        c(list(tidyplot_obj), argList)
      )
    }

    # Add reference line (tidyplots::add_reference_lines)----
    if (tab[["addReferenceLinePlotBuilder"]]) {
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add_reference_lines(
          x         = eval(parse(text = paste0("c(", tab[["xReferenceLine"]], ")"))),
          y         = eval(parse(text = paste0("c(", tab[["yReferenceLine"]], ")"))),
          linetype  = "solid",
          linewidth = tab[["linewidhtReferenceLines"]],
          color     = eval(parse(text = paste0("\"", tab[["colorReferenceLine"]], "\"")))
        )
    }

    # Add title (tidyplots::add_title) ----
    if (tab[["addTitlePlotBuilder"]]) {
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add_title(title = tab[["titlePlotBuilder"]])
    }

    # Add caption (tidyplots::add_caption) ----
    if (tab[["addCaptionPlotBuilder"]]) {
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add_caption(caption = tab[["captionPlotBuilder"]])
    }

    # Possibly re-apply if user directly typed into titlePlotBuilder / captionPlotBuilder
    titleValue <- tab[["titlePlotBuilder"]]
    if (!is.null(titleValue) && titleValue != "") {
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add_title(title = titleValue)
    }

    captionValue <- tab[["captionPlotBuilder"]]
    if (!is.null(captionValue) && captionValue != "") {
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add_caption(caption = captionValue)
    }

    # Color palettes (tidyplots::adjust_colors)----
    if (!is.null(tab[["colorsAll"]])) {
      colorOption    <- tab[["colorsAll"]]
      color_palettes <- list(
        colors_JASP                      = jaspGraphs::JASPcolors(),
        colors_discrete_friendly         = tidyplots::colors_discrete_friendly,
        colors_discrete_seaside          = tidyplots::colors_discrete_seaside,
        colors_discrete_apple            = tidyplots::colors_discrete_apple,
        colors_discrete_friendly_long    = tidyplots::colors_discrete_friendly_long,
        colors_discrete_okabeito         = tidyplots::colors_discrete_okabeito,
        colors_discrete_ibm              = tidyplots::colors_discrete_ibm,
        colors_discrete_metro            = tidyplots::colors_discrete_metro,
        colors_discrete_candy            = tidyplots::colors_discrete_candy,
        colors_continuous_viridis        = tidyplots::colors_continuous_viridis,
        colors_continuous_magma          = tidyplots::colors_continuous_magma,
        colors_continuous_inferno        = tidyplots::colors_continuous_inferno,
        colors_continuous_plasma         = tidyplots::colors_continuous_plasma,
        colors_continuous_cividis        = tidyplots::colors_continuous_cividis,
        colors_continuous_rocket         = tidyplots::colors_continuous_rocket,
        colors_continuous_mako           = tidyplots::colors_continuous_mako,
        colors_continuous_turbo          = tidyplots::colors_continuous_turbo,
        colors_continuous_bluepinkyellow = tidyplots::colors_continuous_bluepinkyellow,
        colors_diverging_blue2red        = tidyplots::colors_diverging_blue2red,
        colors_diverging_blue2brown      = tidyplots::colors_diverging_blue2brown,
        colors_diverging_BuRd            = tidyplots::colors_diverging_BuRd,
        colors_diverging_BuYlRd          = tidyplots::colors_diverging_BuYlRd,
        colors_diverging_spectral        = tidyplots::colors_diverging_spectral,
        colors_diverging_icefire         = tidyplots::colors_diverging_icefire
      )

      if (colorOption %in% names(color_palettes)) {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::adjust_colors(color_palettes[[colorOption]])
      }
    }

    # Add custom colors (tidyplots::adjust_colors)----
    if (!is.null(tab[["customColors"]]) && nchar(trimws(tab[["customColors"]])) > 0) {
      custom_colors <- strsplit(tab[["customColors"]], ",")[[1]]
      custom_colors <- trimws(custom_colors)

      tidyplot_obj <- tidyplot_obj |>
        tidyplots::adjust_colors(new_colors = c(custom_colors))
    }

    # The setting of Themes / legend position----
    if (legend_position == "none") {
      # Remove the legend entirely
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add(ggplot2::guides(color = "none", fill = "none", shape = "none", linetype = "none"))
    } else {
      # Respect user setting
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add(ggplot2::theme(
          legend.position = legend_position
        ))
    }

    # Read style choice
    plotStyle    <- tab[["plotStyle"]]
    baseFontSize <- tab[["baseFontSize"]]



    # # Remove legend title if requested (tidyplots::remove_legend_title)
    # if (tab[["removeLegendTitle"]]) {
    #   tidyplot_obj <- tidyplot_obj |>
    #     tidyplots::remove_legend_title()
    # }


    # Adjust X axis (tidyplots::adjust_x_axis)----
    adjust_args_xaxis <- list()

    titleValueX <- tab[["titleXPlotBuilder"]]
    if (!is.null(titleValueX) && titleValueX != "") {
      adjust_args_xaxis$title <- titleValueX
    }

    fromValueX <- tab[["breakFromX"]]
    toValueX   <- tab[["breakToX"]]
    byValueX   <- tab[["breakByX"]]

    if (!is.null(fromValueX) && fromValueX != "" &&
        !is.null(toValueX) && toValueX != "" &&
        !is.null(byValueX) && byValueX != "") {

      fromNumeric <- suppressWarnings(as.numeric(fromValueX))
      toNumeric   <- suppressWarnings(as.numeric(toValueX))
      byNumeric   <- suppressWarnings(as.numeric(byValueX))

      if (!is.na(fromNumeric) && !is.na(toNumeric) && !is.na(byNumeric)) {
        adjust_args_xaxis$breaks <- seq(fromNumeric, toNumeric, byNumeric)
      }
    }

    limitFromX <- tab[["limitFromX"]]
    limitToX   <- tab[["limitToX"]]

    if (!is.null(limitFromX) && limitFromX != "" &&
        !is.null(limitToX)   && limitToX   != "") {

      limitFromNumericX <- suppressWarnings(as.numeric(limitFromX))
      limitToNumericX   <- suppressWarnings(as.numeric(limitToX))

      if (!is.na(limitFromNumericX) && !is.na(limitToNumericX)) {
        adjust_args_xaxis$limits <- c(limitFromNumericX, limitToNumericX)
      }
    }

    rotateLabels <- tab[["rotateXLabel"]]
    adjust_args_xaxis$rotate_labels <- isTRUE(rotateLabels)

    cutShortScale <- tab[["cutShortScale"]]
    adjust_args_xaxis$cut_short_scale <- isTRUE(cutShortScale)

    if (length(adjust_args_xaxis) > 0) {
      tidyplot_obj <- do.call(
        tidyplots::adjust_x_axis,
        c(list(tidyplot_obj), adjust_args_xaxis)
      )
    }

    # Adjust X axis labels (tidyplots::rename_x_axis_labels)----
    if (!is.null(tab[["xAxisLabelRenamer"]]) && length(tab[["xAxisLabelRenamer"]]) > 0) {
      label_map_x <- setNames(
        sapply(tab[["xAxisLabelRenamer"]], function(x) x$newXLabel),
        sapply(tab[["xAxisLabelRenamer"]], function(x) x$originalXLabel)
      )
      if (length(label_map_x) > 0) {
        tidyplot_obj <- tidyplots::rename_x_axis_labels(tidyplot_obj, new_names = label_map_x)
      }
    }

    # Sort X axis labels (tidyplots::sort_x_axis_labels)----
    enableSort <- tab[["enableSort"]]
    if (!is.null(enableSort) && enableSort) {
      sortOrder <- tab[["sortXLabelsOrder"]]
      if (!is.null(sortOrder) && sortOrder %in% c("Increasing", "Decreasing")) {
        reverse_order <- (sortOrder == "Decreasing")
        aggFun        <- tab[["aggregationFun"]]
        if (!is.null(aggFun) && aggFun %in% c("mean", "median")) {
          tidyplot_obj <- tidyplots::sort_x_axis_labels(
            tidyplot_obj,
            .reverse = reverse_order,
            .fun     = aggFun
          )
        }
      }
    }

    # Adjust Y axis (tidyplots::adjust_y_axis)----
    adjust_args_yaxis <- list()

    titleValueY <- tab[["titleYPlotBuilder"]]
    if (!is.null(titleValueY) && titleValueY != "") {
      adjust_args_yaxis$title <- titleValueY
    }

    fromValueY <- tab[["breakFromY"]]
    toValueY   <- tab[["breakToY"]]
    byValueY   <- tab[["breakByY"]]

    if (!is.null(fromValueY) && fromValueY != "" &&
        !is.null(toValueY)   && toValueY   != "" &&
        !is.null(byValueY)   && byValueY   != "") {

      fromNumericY <- suppressWarnings(as.numeric(fromValueY))
      toNumericY   <- suppressWarnings(as.numeric(toValueY))
      byNumericY   <- suppressWarnings(as.numeric(byValueY))

      if (!is.na(fromNumericY) && !is.na(toNumericY) && !is.na(byNumericY)) {
        adjust_args_yaxis$breaks <- seq(fromNumericY, toNumericY, byNumericY)
      }
    }

    limitFromY <- tab[["limitFromY"]]
    limitToY   <- tab[["limitToY"]]

    if (!is.null(limitFromY) && limitFromY != "" &&
        !is.null(limitToY)   && limitToY   != "") {

      limitFromNumericY <- suppressWarnings(as.numeric(limitFromY))
      limitToNumericY   <- suppressWarnings(as.numeric(limitToY))

      if (!is.na(limitFromNumericY) && !is.na(limitToNumericY)) {
        adjust_args_yaxis$limits <- c(limitFromNumericY, limitToNumericY)
      }
    }

    rotateLabelsY <- tab[["rotateYLabel"]]
    adjust_args_yaxis$rotate_labels <- isTRUE(rotateLabelsY)

    cutShortScaleY <- tab[["cutShortScaleY"]]
    adjust_args_yaxis$cut_short_scale <- isTRUE(cutShortScaleY)

    if (length(adjust_args_yaxis) > 0) {
      tidyplot_obj <- do.call(
        tidyplots::adjust_y_axis,
        c(list(tidyplot_obj), adjust_args_yaxis)
      )
    }

    enableSortY <- tab[["enableSortY"]]
    if (isTRUE(enableSortY)) {
      sortOrderY <- tab[["sortYLabelsOrder"]]
      if (!is.null(sortOrderY) && sortOrderY %in% c("Increasing", "Decreasing")) {
        reverse_orderY <- (sortOrderY == "Decreasing")
        aggFunY        <- tab[["aggregationFunY"]]
        if (!is.null(aggFunY) && aggFunY %in% c("mean", "median")) {
          tidyplot_obj <- tidyplots::sort_y_axis_labels(
            tidyplot_obj,
            .reverse = reverse_orderY,
            .fun     = aggFunY
          )
        }
      }
    }

    # Adjust Y axis labels (tidyplots::rename_y_axis_labels)----
    if (!is.null(tab[["yAxisLabelRenamer"]]) && length(tab[["yAxisLabelRenamer"]]) > 0) {
      label_map_y <- setNames(
        sapply(tab[["yAxisLabelRenamer"]], function(x) x$newYLabel),
        sapply(tab[["yAxisLabelRenamer"]], function(x) x$originalYLabel)
      )
      if (length(label_map_y) > 0) {
        tidyplot_obj <- tidyplots::rename_y_axis_labels(tidyplot_obj, new_names = label_map_y)
      }
    }

    # Extract the ggplot object from tidyplot-----
    tidyplot_obj <- tidyplot_obj[[1]]


    # Apply theme
    if (plotStyle == "JASP") {
      # Custom JASP theme (assuming themeJaspRaw is from a JASP-specific pkg)
      tidyplot_obj <- tidyplot_obj +
        jaspGraphs::themeJaspRaw(fontsize = baseFontSize, legend.position = legend_position) +
        ggplot2::geom_blank() +
        jaspGraphs::geom_rangeframe() +
        ggplot2::theme(
          strip.text = ggplot2::element_text(size = baseFontSize)
        )

    } else if (plotStyle == "ggplotgray") {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::theme_gray(base_size = baseFontSize) +
        ggplot2::theme(legend.position = legend_position)

    } else if (plotStyle == "tidyplot") {
      tidyplot_obj <- tidyplot_obj +
        theme_tidyplot(fontsize = baseFontSize) +
        ggplot2::theme(legend.position = legend_position)

    } else if (plotStyle == "ggpubr") {
      tidyplot_obj <- tidyplot_obj +
        ggpubr::theme_pubr(base_size = baseFontSize) +
        ggplot2::theme(legend.position = legend_position)

    } else if (plotStyle == "PlotBuilder") {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::theme_bw(base_size = baseFontSize) +
        ggplot2::theme(
          strip.background = ggplot2::element_rect(fill = "#f3f3f3"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position  = legend_position
        )
    }

    # Margins
    t <- tab[["topMargin"]]
    r <- tab[["rightMargin"]]
    b <- tab[["bottomMargin"]]
    l <- tab[["leftMargin"]]

    tidyplot_obj <- tidyplot_obj +
      ggplot2::theme(
        legend.title  = ggplot2::element_text(
          hjust  = 0,
          margin = ggplot2::margin(10, 10, 10, 10)
        ),
        legend.text   = ggplot2::element_text(
          margin = ggplot2::margin(10, 10, 10, 10)
        ),
        legend.margin = ggplot2::margin(10, 10, 10, 10),
        plot.margin   = ggplot2::margin(t = t, r = r, b = b, l = l)
      )

    # Remove legend title if requested (tidyplots::remove_legend_title)
    if (tab[["removeLegendTitle"]]) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::theme(legend.title = ggplot2:::element_blank())
    }



    # 1) Facet logic for row / column
    rowsVar <- tab[["rowsvariableSplitPlotBuilder"]]
    colsVar <- tab[["columnsvariableSplitPlotBuilder"]]

    hasRows <- (!is.null(rowsVar) && rowsVar != "")
    hasCols <- (!is.null(colsVar) && colsVar != "")

    if (hasRows) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(~ .,
                                       name   = paste0(rowsVar, " (binned)"),
                                       breaks = NULL,
                                       labels = NULL
          )
        )
    }

    if (hasCols) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::ggtitle(colsVar) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }

    if (hasRows && hasCols) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::facet_grid(stats::as.formula(paste(rowsVar, "~", colsVar)))
    } else if (hasCols) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::facet_grid(stats::as.formula(paste(". ~", colsVar)))
    } else if (hasRows) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::facet_grid(stats::as.formula(paste(rowsVar, "~ .")))
    }

    # Annotation (base R annotate from ggplot2 or tidyplots might differ, but here it's ggplot2::annotate)
    if (!is.null(tab[["annotationPlotBuilder"]]) && length(tab[["annotationPlotBuilder"]]) > 0) {
      for (i in seq_along(tab[["annotationPlotBuilder"]])) {
        rowData   <- tab[["annotationPlotBuilder"]][[i]]
        plotText  <- rowData$annotationText
        plotX     <- rowData$annotationX
        plotY     <- rowData$annotationY
        plotSize  <- if (!is.null(rowData$annotationSize)) rowData$annotationSize else 5

        tidyplot_obj <- tidyplot_obj +
          ggplot2::annotate(
            "text",
            x     = plotX,
            y     = plotY,
            label = gsub("\\$", "", plotText),
            parse = TRUE,
            size  = plotSize,
            color = "black"
          )
      }
    }

    # Connect points with lines if needed (for RM)----
    if (tab[["connectRMPlotBuilder"]]) {
      gg    <- tidyplot_obj
      built <- ggplot2::ggplot_build(gg)

      points_data <- built$data[[1]] |>
        dplyr::mutate(ID = localData$ID)

      lines_data <- points_data |>
        dplyr::arrange(ID, x) |>
        dplyr::group_by(ID) |>
        dplyr::mutate(order = dplyr::row_number()) |>
        dplyr::ungroup()

      gg2 <- gg +
        ggplot2::geom_line(
          data          = lines_data,
          ggplot2::aes(x = x, y = y, group = ID),
          inherit.aes   = FALSE,
          color         = lines_data$colour,
          alpha         = tab[["lineRMtransparency"]],
          size          = tab[["lineRMsize"]]
        )
      tidyplot_obj <- gg2
    }

    # P value from ggpubr::stat_pvalue_manual----
    if (!is.null(tab[["pairwiseComparisons"]]) && length(tab[["pairwiseComparisons"]]) > 0) {

      dfComparisons <- data.frame(
        group1     = as.character(sapply(tab[["pairwiseComparisons"]], function(x) x$group1)),
        group2     = as.character(sapply(tab[["pairwiseComparisons"]], function(x) x$group2)),
        p          = as.character(sapply(tab[["pairwiseComparisons"]], function(x) x$pAdj)),
        y.position = as.numeric(sapply(tab[["pairwiseComparisons"]], function(x) x$yPositionPValue)),
        stringsAsFactors = FALSE
      )

      colorVar <- tab[["variableColorPlotBuilder"]]
      if (!is.null(colorVar) && nchar(colorVar) > 0) {
        dfComparisons$color <- as.character(
          sapply(tab[["pairwiseComparisons"]], function(x) x$GroupPValue)
        )
      }

      rowsVar <- tab[["rowsvariableSplitPlotBuilder"]]
      if (!is.null(rowsVar)) {
        dfComparisons[[rowsVar]] <- as.character(
          sapply(tab[["pairwiseComparisons"]], function(x) x$RowPValue)
        )
      }

      colsVar <- tab[["columnsvariableSplitPlotBuilder"]]
      if (!is.null(colsVar)) {
        dfComparisons[[colsVar]] <- as.character(
          sapply(tab[["pairwiseComparisons"]], function(x) x$ColumnPValue)
        )
      }

      label_size <- unique(sapply(tab[["pairwiseComparisons"]], function(x) x$labelSizePValue))
      bracket.size <- unique(sapply(tab[["pairwiseComparisons"]], function(x) x$bracketSizePValue))
      tip_length <- unique(sapply(tab[["pairwiseComparisons"]], function(x) x$tipLengthPValue))
      labelcolor <- unique(sapply(tab[["pairwiseComparisons"]], function(x) x$labelcolor))


      tidyplot_obj <- tidyplot_obj +
        ggpubr::stat_pvalue_manual(
          data          = dfComparisons,
          label         = "p",
          xmin          = "group1",
          xmax          = "group2",
          y.position    = "y.position",
          size          = label_size,
          bracket.size  = bracket.size,
          tip.length    = tip_length,
          color = if (!is.null(colorVar) && nchar(colorVar) > 0) "color" else as.character(labelcolor),
          inherit.aes   = FALSE,
          step.increase = 0.15
        )
    }

    # Save the plot in our results list
    updatedPlots[[plotId]] <- tidyplot_obj
  }

  return(list(updatedPlots = updatedPlots))
}



.plotBuilderOutputIndividualPlots <- function(jaspResults, options, plotResults) {
  updatedPlots <- plotResults$updatedPlots

  # Create or retrieve individual tidy plots container
  if (!is(jaspResults[["tidyPlotsContainer"]], "JaspContainer")) {
    tidyPlotsContainer <- createJaspContainer(title = "Individual Tidy Plots")
    jaspResults[["tidyPlotsContainer"]] <- tidyPlotsContainer
  } else {
    tidyPlotsContainer <- jaspResults[["tidyPlotsContainer"]]
  }

  # Render individual tidy plots in their container
  for (plotId in names(updatedPlots)) {
    # Skip error plot if exists
    if (plotId == "scatterPlotError")
      next

    plotKey <- paste0("tidyPlot_", plotId)
    if (!is(tidyPlotsContainer[[plotKey]], "JaspPlot")) {
      # Retrieve plot dimensions from the corresponding tab
      tab <- NULL
      for (t in options$PlotBuilderTab) {
        if (as.character(t$plotId) == plotId) {
          tab <- t
          break
        }
      }

      # Default dimensions if tab not found
      if (is.null(tab)) {
        plotWidth  <- 400
        plotHeight <- 300
      } else {
        plotWidthPx  <- tab[["widthPlotBuilder"]]
        plotHeightPx <- tab[["heightPlotBuilder"]]
        plotWidth    <- plotWidthPx  * 25.4 / 96
        plotHeight   <- plotHeightPx * 25.4 / 96
      }

      tidyPlot <- createJaspPlot(
        title  = paste("Tidy Plot", plotId),
        width  = tab[["widthPlotBuilder"]],
        height = tab[["heightPlotBuilder"]]
      )
      tidyPlot$plotObject <- updatedPlots[[plotId]]
      tidyPlot$dependOn(names(options))
      tidyPlotsContainer[[plotKey]] <- tidyPlot
    } else {
      tidyPlotsContainer[[plotKey]]$plotObject <- updatedPlots[[plotId]]
    }
  }
}

.plotBuilderOutputAvailablePlotIDs <- function(jaspResults, plotResults) {
  updatedPlots <- plotResults$updatedPlots

  if (!is.null(jaspResults[["availablePlotIDs"]])) {
    availablePlotIDsContainer <- jaspResults[["availablePlotIDs"]]
  } else {
    availablePlotIDsContainer <- createJaspContainer(title = "Available Plot IDs")
    jaspResults[["availablePlotIDs"]] <- availablePlotIDsContainer
  }

  availablePlotIDsDisplay <- names(updatedPlots)
  availablePlotIDsText    <- paste0(
    "Available Plot IDs: ",
    paste(availablePlotIDsDisplay, collapse = ", ")
  )
  availablePlotIDsHtml <- createJaspHtml(availablePlotIDsText)
  availablePlotIDsContainer[["availablePlotIDsText"]] <- availablePlotIDsHtml
}


# plot builder ----
.plotBuilderOutputPlotGrid <- function(jaspResults, options, plotResults, dataset) {
  updatedPlots <- plotResults$updatedPlots

  if (!is(jaspResults[["plotGridContainer"]], "JaspContainer")) {
    plotGridContainer <- createJaspContainer(title = "Plot Grid")
    jaspResults[["plotGridContainer"]] <- plotGridContainer
  } else {
    plotGridContainer <- jaspResults[["plotGridContainer"]]
  }

  getCommonLegendGlobal <- FALSE
  if (!is.null(options[["getCommonLegend"]]) && options[["getCommonLegend"]]) {
    getCommonLegendGlobal <- TRUE
  }

  if (!is.null(options[["compilePlotGrid"]]) && options[["compilePlotGrid"]]) {
    columnGrid  <- NULL
    fullRowGrid <- NULL

    rowSpecifications <- options[["rowSpecifications"]]
    if (!is.null(rowSpecifications) && length(rowSpecifications) > 0) {

      ncol <- length(rowSpecifications)

      columnsWidth <- options[["columnWidthInput"]]
      if (!is.null(columnsWidth) && columnsWidth != "") {
        if (!grepl("^c\\s*\\(", columnsWidth)) {
          columnsWidth <- paste0("c(", columnsWidth, ")")
        }
        columnsWidth <- tryCatch(
          eval(parse(text = columnsWidth)),
          error = function(e) {
            rep(1, ncol)
          }
        )
        if (length(columnsWidth) != ncol) {
          columnsWidth <- rep(1, ncol)
        }
      } else {
        columnsWidth <- rep(1, ncol)
      }

      labelSize <- 5
      if (!is.null(options[["labelSize"]])) {
        labelSizeParsed <- as.numeric(options[["labelSize"]])
        if (is.na(labelSizeParsed)) {
          labelSizeParsed <- 5
        }
        labelSize <- labelSizeParsed
      }

      relativeHeight <- options[["relativeHeight"]]
      if (!is.null(relativeHeight) && relativeHeight != "") {
        if (!grepl("^c\\s*\\(", relativeHeight)) {
          relativeHeight <- paste0("c(", relativeHeight, ")")
        }
        relativeHeight <- tryCatch(
          eval(parse(text = relativeHeight)),
          error = function(e) {
            rep(1, ncol)
          }
        )
        if (length(relativeHeight) != ncol) {
          relativeHeight <- rep(1, ncol)
        }
      } else {
        relativeHeight <- rep(1, ncol)
      }

      columnPlots <- lapply(seq_len(ncol), function(colIdx) {
        columnSpec <- rowSpecifications[[colIdx]]
        if (is.null(columnSpec)) {
          return(ggplot2::ggplot() + ggplot2::theme_void())
        }

        plotIDsStr    <- columnSpec[["plotIDs"]]
        labelsStr     <- columnSpec[["labelsColumn"]]
        rowHeightsStr <- columnSpec[["rowHeightsColumn"]]

        getCommonLegendColumn <- FALSE
        if (!getCommonLegendGlobal && !is.null(columnSpec[["getCommonLegendColumn"]])) {
          if (columnSpec[["getCommonLegendColumn"]]) {
            getCommonLegendColumn <- TRUE
          }
        }

        plotIDs <- strsplit(plotIDsStr, ",")[[1]]
        plotIDs <- trimws(plotIDs)
        nRows   <- length(plotIDs)

        rowHeights <- rep(1, nRows)
        if (!is.null(rowHeightsStr) && rowHeightsStr != "") {
          if (!grepl("^c\\s*\\(", rowHeightsStr)) {
            rowHeightsStr <- paste0("c(", rowHeightsStr, ")")
          }
          rowHeightsParsed <- tryCatch(
            eval(parse(text = rowHeightsStr)),
            error = function(e) {
              rep(1, nRows)
            }
          )
          if (length(rowHeightsParsed) == nRows) {
            rowHeights <- rowHeightsParsed
          }
        }

        labels <- NULL
        if (!is.null(labelsStr) && labelsStr != "") {
          labels <- strsplit(labelsStr, ",")[[1]]
          labels <- trimws(labels)
          if (length(labels) != nRows) {
            labels <- NULL
          }
        }

        plotsInColumn <- vector("list", nRows)
        for (rowIdx in seq_len(nRows)) {
          plotID <- plotIDs[rowIdx]
          if (is.na(plotID) || plotID == "") {
            plotsInColumn[[rowIdx]] <- ggplot2::ggplot() + ggplot2::theme_void()
            next
          }

          matchedPlot <- updatedPlots[[plotID]]
          if (!is.null(matchedPlot)) {
            if (!is.null(labels) && labels[rowIdx] != "") {
              matchedPlot <- matchedPlot +
                ggplot2::labs(tag = labels[rowIdx]) +
                ggplot2::theme(
                  plot.tag.position = "topleft",
                  plot.tag          = ggplot2::element_text(size = labelSize, face = "bold")
                )
            }
            if (getCommonLegendGlobal) {
              matchedPlot <- matchedPlot + ggplot2::theme(legend.position = "none")
            }
            plotsInColumn[[rowIdx]] <- matchedPlot
          } else {
            plotsInColumn[[rowIdx]] <- ggplot2::ggplot() + ggplot2::theme_void()
          }
        }

        collectLegends <- if (getCommonLegendGlobal) FALSE else getCommonLegendColumn
        layoutGuides   <- if (collectLegends) "collect" else "auto"

        columnPatchwork <- patchwork::wrap_plots(plotsInColumn, ncol = 1, heights = rowHeights) +
          patchwork::plot_layout(guides = layoutGuides)

        if (collectLegends) {
          columnPatchwork <- columnPatchwork &
            ggplot2::theme(legend.position = "right")
        }

        return(columnPatchwork)
      })

      if (length(columnPlots) > 0) {
        layoutGuides <- if (getCommonLegendGlobal) "auto" else "auto"
        columnGrid <- patchwork::wrap_plots(columnPlots, ncol = ncol, widths = columnsWidth) +
          patchwork::plot_layout(guides = layoutGuides)
      }

      fullRowSpecifications <- options[["fullRowSpecifications"]]
      if (!is.null(fullRowSpecifications) && length(fullRowSpecifications) > 0) {
        fullRowPlots <- list()

        labelSize <- 5
        if (!is.null(options[["labelSize"]])) {
          labelSizeParsed <- as.numeric(options[["labelSize"]])
          if (is.na(labelSizeParsed)) {
            labelSizeParsed <- 5
          }
          labelSize <- labelSizeParsed
        }

        for (rowIdx in seq_along(fullRowSpecifications)) {
          rowSpec <- fullRowSpecifications[[rowIdx]]

          plotIDsStr     <- rowSpec[["plotIDsFullRow"]]
          labelsStr      <- rowSpec[["labelsFullRow"]]
          relWidthsStr   <- rowSpec[["relWidthsFullRow"]]

          getCommonLegendRow <- FALSE
          if (!getCommonLegendGlobal && !is.null(rowSpec[["getCommonLegendRows"]])) {
            if (rowSpec[["getCommonLegendRows"]]) {
              getCommonLegendRow <- TRUE
            }
          }

          plotIDs <- strsplit(plotIDsStr, ",")[[1]]
          plotIDs <- trimws(plotIDs)
          nPlots  <- length(plotIDs)

          relWidths <- rep(1, nPlots)
          if (!is.null(relWidthsStr) && relWidthsStr != "") {
            if (!grepl("^c\\s*\\(", relWidthsStr)) {
              relWidthsStr <- paste0("c(", relWidthsStr, ")")
            }
            relWidthsParsed <- tryCatch(
              eval(parse(text = relWidthsStr)),
              error = function(e) {
                rep(1, nPlots)
              }
            )
            if (length(relWidthsParsed) == nPlots) {
              relWidths <- relWidthsParsed
            }
          }

          labels <- NULL
          if (!is.null(labelsStr) && labelsStr != "") {
            labels <- strsplit(labelsStr, ",")[[1]]
            labels <- trimws(labels)
            if (length(labels) != nPlots) {
              labels <- NULL
            }
          }

          plotsInFullRow <- vector("list", nPlots)
          for (idx in seq_len(nPlots)) {
            plotID <- plotIDs[idx]
            if (is.na(plotID) || plotID == "") {
              plotsInFullRow[[idx]] <- ggplot2::ggplot() + ggplot2::theme_void()
              next
            }

            matchedPlot <- updatedPlots[[plotID]]
            if (!is.null(matchedPlot)) {
              if (!is.null(labels) && labels[idx] != "") {
                matchedPlot <- matchedPlot +
                  ggplot2::labs(tag = labels[idx]) +
                  ggplot2::theme(
                    plot.tag.position = "topleft",
                    plot.tag          = ggplot2::element_text(size = labelSize, face = "bold")
                  )
              }
              if (getCommonLegendGlobal) {
                matchedPlot <- matchedPlot + ggplot2::theme(legend.position = "none")
              }
              plotsInFullRow[[idx]] <- matchedPlot
            } else {
              plotsInFullRow[[idx]] <- ggplot2::ggplot() + ggplot2::theme_void()
            }
          }

          collectLegends   <- if (getCommonLegendGlobal) FALSE else getCommonLegendRow
          layoutGuidesFull <- if (collectLegends) "collect" else "auto"

          fullRowPatchwork <- patchwork::wrap_plots(plotsInFullRow, ncol = nPlots, widths = relWidths) +
            patchwork::plot_layout(guides = layoutGuidesFull)

          if (collectLegends) {
            fullRowPatchwork <- fullRowPatchwork &
              ggplot2::theme(legend.position = "right")
          }

          fullRowPlots <- append(fullRowPlots, list(fullRowPatchwork))
        }

        relheightWithinRowLayoutStr <- options[["relHeightWithinRowLayout"]]
        if (!is.null(relheightWithinRowLayoutStr) && relheightWithinRowLayoutStr != "") {
          if (!grepl("^c\\s*\\(", relheightWithinRowLayoutStr)) {
            relheightWithinRowLayoutStr <- paste0("c(", relheightWithinRowLayoutStr, ")")
          }
          relheightWithinRowLayout <- tryCatch(
            eval(parse(text = relheightWithinRowLayoutStr)),
            error = function(e) {
              rep(1, length(fullRowPlots))
            }
          )
          if (length(relheightWithinRowLayout) != length(fullRowPlots)) {
            relheightWithinRowLayout <- rep(1, length(fullRowPlots))
          }
        } else {
          relheightWithinRowLayout <- rep(1, length(fullRowPlots))
        }

        if (length(fullRowPlots) > 0) {
          fullRowGrid <- patchwork::wrap_plots(fullRowPlots, ncol = 1, heights = relheightWithinRowLayout) +
            patchwork::plot_layout(guides = "auto")
        }
      }

      relativeHeightStr <- options[["relativeHeight"]]
      if (!is.null(relativeHeightStr) && relativeHeightStr != "") {
        if (!grepl("^c\\s*\\(", relativeHeightStr)) {
          relativeHeightStr <- paste0("c(", relativeHeightStr, ")")
        }
        relativeHeight <- tryCatch(
          eval(parse(text = relativeHeightStr)),
          error = function(e) {
            c(1, 1)
          }
        )
        if (!is.null(columnGrid) && !is.null(fullRowGrid)) {
          if (length(relativeHeight) != 2) {
            relativeHeight <- c(1, 1)
          }
        } else {
          relativeHeight <- 1
        }
      } else {
        if (!is.null(columnGrid) && !is.null(fullRowGrid)) {
          relativeHeight <- c(1, 1)
        } else {
          relativeHeight <- 1
        }
      }

      if (!is.null(columnGrid) && !is.null(fullRowGrid)) {
        if (getCommonLegendGlobal) {
          finalGrid <- (columnGrid / fullRowGrid) +
            patchwork::plot_layout(heights = relativeHeight, guides = "collect") &
            ggplot2::theme(legend.position = "right")
        } else {
          finalGrid <- (columnGrid / fullRowGrid) +
            patchwork::plot_layout(heights = relativeHeight, guides = "auto")
        }
      } else if (!is.null(columnGrid)) {
        if (getCommonLegendGlobal) {
          finalGrid <- columnGrid + patchwork::plot_layout(guides = "collect") &
            ggplot2::theme(legend.position = "right")
        } else {
          finalGrid <- columnGrid + patchwork::plot_layout(guides = "auto")
        }
      } else if (!is.null(fullRowGrid)) {
        if (getCommonLegendGlobal) {
          finalGrid <- fullRowGrid + patchwork::plot_layout(guides = "collect") &
            ggplot2::theme(legend.position = "right")
        } else {
          finalGrid <- fullRowGrid + patchwork::plot_layout(guides = "auto")
        }
      } else {
        finalGrid <- ggplot2::ggplot() +
          ggplot2::ggtitle("No plots available") +
          ggplot2::theme_void()
      }

      if (!is(plotGridContainer[["plotGrid"]], "JaspPlot")) {
        gridPlot <- createJaspPlot(
          title  = "Combined Plot Grid",
          width  = options[["layoutWidth"]],
          height = options[["layoutHeight"]]
        )
        gridPlot$plotObject <- finalGrid
        gridPlot$dependOn(
          c(
            "compilePlotGrid", "rowSpecifications", "fullRowSpecifications",
            "getCommonLegend", "relHeightWithinRowLayout", "relativeHeight",
            "columnWidthInput", "labelSize",
            "layoutWidth", "layoutHeight"
          )
        )
        plotGridContainer[["plotGrid"]] <- gridPlot
      } else {
        plotGridContainer[["plotGrid"]]$plotObject <- finalGrid
      }
    }
  }
}

