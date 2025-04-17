
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

  # Initialize options
  options <- .plotBuilderInitOptions(jaspResults, options)

  # Read dataset
  dataset <- .plotBuilderReadData(options)

  # Compute results
  plotResults <- .plotBuilderComputeResults(jaspResults, dataset, options)

  # Output individual plots
  .plotBuilderOutputIndividualPlots(jaspResults, options, plotResults)

  # Output available plot IDs
  .plotBuilderOutputAvailablePlotIDs(jaspResults, plotResults)

  # Output plot grid
  .plotBuilderOutputPlotGrid(jaspResults, options, plotResults)

  return()
}


# Init functions ----
.plotBuilderInitOptions <- function(jaspResults, options) {
  # Initialize options if needed
  # Currently no specific initialization. Return options as-is.
  options
}

.plotBuilderReadData <- function(options) {

  datasetRMList    <- list()
  datasetNonRMList <- list()

  for (tab in options$PlotBuilderTab) {
    plotId <- as.character(tab$value)

    neededCols <- unique(c(
      encodeColNames(tab$variableXPlotBuilder),
      encodeColNames(tab$variableYPlotBuilder),
      encodeColNames(tab$variableColorPlotBuilder),
      encodeColNames(tab$variableRepeatedMeasures),
      encodeColNames(tab$columnsvariableSplitPlotBuilder),
      encodeColNames(tab$gridVariablePlotBuilder),
      encodeColNames(tab$rowsvariableSplitPlotBuilder)
    ))

    ds <- .readDataSetToEnd(columns = neededCols)
    ds <- dplyr::mutate(ds, row_id = dplyr::row_number())

    if (identical(tab[["isRM"]], "RM")) {
      repeatedMeasuresCols <- encodeColNames(tab[["variableRepeatedMeasures"]])
      pivotedXName <- "Repeated measures"
      pivotedYName <- "Values"

      if (is.null(repeatedMeasuresCols) || length(repeatedMeasuresCols) == 0) {
        datasetRMList[[plotId]] <- ds
      } else {
        tempRM <- ds |>
          dplyr::mutate(ID = dplyr::row_number()) |>
          tidyr::pivot_longer(
            cols      = dplyr::all_of(repeatedMeasuresCols),
            names_to  = pivotedXName,
            values_to = pivotedYName
          ) |>
          dplyr::mutate(
            !!rlang::sym(pivotedXName) := factor(decodeColNames(!!rlang::sym(pivotedXName)))
          )

        originalCols <- names(ds)
        pivotedCols  <- names(tempRM)
        missingCols  <- setdiff(originalCols, pivotedCols)
        missingCols  <- setdiff(missingCols, "row_id")
        if (length(missingCols) > 0) {
          additionalData <- dplyr::select(ds, row_id, dplyr::all_of(missingCols))
          tempRM <- dplyr::left_join(tempRM, additionalData, by = "row_id")
        }

        if (!is.null(tab$variableColorPlotBuilder)) {
          colorVarRaw <- tab$variableColorPlotBuilder
          colorVar <- encodeColNames(colorVarRaw)
          if (colorVar %in% names(tempRM)) {
            tempRM <- tempRM[!is.na(tempRM[[colorVar]]), ]
          }
        }
        if (!is.null(tab$variableXPlotBuilder)) {
          xVarRaw <- tab$variableXPlotBuilder
          xVar <- encodeColNames(xVarRaw)
          if (xVar %in% names(tempRM)) {
            tempRM <- tempRM[!is.na(tempRM[[xVar]]), ]
          }
        }
        if (!is.null(tab$columnsvariableSplitPlotBuilder)) {
          colSplitRaw <- tab$columnsvariableSplitPlotBuilder
          colSplit <- encodeColNames(colSplitRaw)
          if (colSplit %in% names(tempRM)) {
            tempRM <- tempRM[!is.na(tempRM[[colSplit]]), ]
          }
        }
        if (!is.null(tab$rowsvariableSplitPlotBuilder)) {
          rowSplitRaw <- tab$rowsvariableSplitPlotBuilder
          rowSplit <- encodeColNames(rowSplitRaw)
          if (rowSplit %in% names(tempRM)) {
            tempRM <- tempRM[!is.na(tempRM[[rowSplit]]), ]
          }
        }
        if (!is.null(tab$gridVariablePlotBuilder)) {
          gridVarRaw <- tab$gridVariablePlotBuilder
          gridVar <- encodeColNames(gridVarRaw)
          if (gridVar %in% names(tempRM)) {
            tempRM <- tempRM[!is.na(tempRM[[gridVar]]), ]
          }
        }

        datasetRMList[[plotId]] <- tempRM
      }
    } else {
      datasetNonRMList[[plotId]] <- na.omit(ds)
    }
  }

  return(list(datasetRMList = datasetRMList, datasetNonRMList = datasetNonRMList))
}


#Results functions ----
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
  datasetNonRMList <- dataset$datasetNonRMList

  # Initialize the list to store plots
  updatedPlots <- list()

  # Creating plots -----
  for (tab in options[["PlotBuilderTab"]]) {

    plotId <- as.character(tab$value)
    localData <- if (identical(tab[["isRM"]], "RM")) {
      datasetRMList[[plotId]]
    } else {
      datasetNonRMList[[plotId]]
    }


    # read variables from the QML interface
    xVar    <- encodeColNames(tab[["variableXPlotBuilder"]])
    yVar    <- encodeColNames(tab[["variableYPlotBuilder"]])
    rowsVar <- encodeColNames(tab[["rowsvariableSplitPlotBuilder"]])
    colsVar <- encodeColNames(tab[["columnsvariableSplitPlotBuilder"]])
    colorVar <- NULL
    gridVar <- encodeColNames(tab[["gridVariablePlotBuilder"]])

    if (identical(tab[["isRM"]], "RM")) {

      # Process the repeated measures factor option
      rmFactorOption <- tab[["rmFactorOptionsDropDown"]]
      if (rmFactorOption == "rmFactorAsX") {
        xVar <- "Repeated measures"
      } else if (rmFactorOption == "rmFactorAsY") {
        yVar <- "Repeated measures"
      } else if (rmFactorOption == "rmFactorAsGroup") {
        colorVar <- "Repeated measures"
      } else if (rmFactorOption == "rmFactorAsColumnSplit") {
        colsVar <- "Repeated measures"
      } else if (rmFactorOption == "rmFactorAsGrid") {
        gridVar <- "Repeated measures"
      } else if (rmFactorOption == "rmFactorAsRowSplit") {
        rowsVar <- "Repeated measures"
      } else if (rmFactorOption == "rmFactorNotVisible") {
        hiddenRM <- "Repeated measures"
      }



      # Process the repeated measures value option
      rmValueOption <- tab[["rmValueOptionsDropDown"]]
      if (rmValueOption == "rmValueAsX") {
        xVar <- "Values"
      } else if (rmValueOption == "rmValueAsY") {
        yVar <- "Values"
      }
    }

    # Process the colorBy option only if not already set by rmFactorAsGroup
    if (is.null(colorVar)) {
      colorBy <- tab[["colorByGroup"]]
      if (colorBy == "none") {
        colorVar <- ""
      } else if (colorBy == "grouping") {
        colorVar <- encodeColNames(tab[["variableColorPlotBuilder"]])
      } else if (colorBy == "x") {
        colorVar <- xVar
      } else if (colorBy == "y") {
        colorVar <- yVar
      } else if (colorBy == "rm") {
        colorVar <- "Repeated measures"
      } else if (colorBy == "splitColumn") {
        colorVar <- colsVar
      } else if (colorBy == "splitRow") {
        colorVar <- rowsVar
      }
    }

    # The next section is necessary because some functions do not work if the
    # variable is defined as ordinal within JASP.
    # If xVar exists in localData and is non-numeric, convert it to a factor.
    # If xVar is already a factor, preserve its current level order.
    if (!is.null(xVar) && xVar %in% colnames(localData)) {
      if (!is.numeric(localData[[xVar]])) {  # Only convert non-numeric variables
        if (is.factor(localData[[xVar]])) {
          # Save the existing order of levels.
          # This preserves any specific ordering already defined (e.g., from JASP settings).
          lvls <- levels(localData[[xVar]])
          # Convert to a regular factor with the preserved level order.
          localData[[xVar]] <- factor(as.character(localData[[xVar]]), levels = lvls)
        } else {
          # If xVar is not already a factor (e.g., a character vector),
          # convert it to a factor using the order of first appearance.
          # This creates a factor with levels in the order they appear in the data.
          localData[[xVar]] <- factor(localData[[xVar]], levels = unique(localData[[xVar]]))
        }
      }
    }

    # For yVar:
    # Repeat the same conversion for yVar, preserving level order if it already exists.
    if (!is.null(yVar) && yVar %in% colnames(localData)) {
      if (!is.numeric(localData[[yVar]])) {  # Only convert non-numeric variables
        if (is.factor(localData[[yVar]])) {
          # Preserve the current factor level order.
          lvls <- levels(localData[[yVar]])
          localData[[yVar]] <- factor(as.character(localData[[yVar]]), levels = lvls)
        } else {
          # Convert to a factor using the unique order from the data.
          localData[[yVar]] <- factor(localData[[yVar]], levels = unique(localData[[yVar]]))
        }
      }
    } # end of ordinal/nominal problem section

    # extract plot id
    plotId <- as.character(tab$value)

    # Build the base tidyplot call
    tidyplot_args <- list(data = localData)

    if (!is.null(xVar) && xVar %in% colnames(localData)) {
      tidyplot_args$x <- rlang::sym(xVar)
    }
    if (!is.null(yVar) && yVar %in% colnames(localData)) {
      tidyplot_args$y <- rlang::sym(yVar)
    }

    legend_position <- "none"

    if (!is.na(colorVar) && colorVar %in% colnames(localData)) {
      tidyplot_args$color <- rlang::sym(colorVar)
      legend_position <- tab[["legendPosistionPlotBuilder"]]
    }

    # Create the tidyplot object (tidyplots package)
    tidyplot_obj <- do.call(
      tidyplots::tidyplot,
      tidyplot_args
    )

    # Adjust color labels (tidyplots::rename_color_labels)----
    if (!is.null(tab[["colorLabelRenamer"]]) && length(tab[["colorLabelRenamer"]]) > 0) {

      # Ellenőrizd, hogy minden elem esetén meg vannak-e adva az új és régi címkék
      valid_labels <- sapply(tab[["colorLabelRenamer"]], function(x) {
        !is.null(x$newColorLabel) && nzchar(x$newColorLabel) &&
          !is.null(x$originalColorLabel) && nzchar(x$originalColorLabel)
      })

      if (all(valid_labels)) {
        label_map_color <- setNames(
          sapply(tab[["colorLabelRenamer"]], function(x) x$newColorLabel),
          sapply(tab[["colorLabelRenamer"]], function(x) x$originalColorLabel)
        )
        if (length(label_map_color) > 0) {
          tidyplot_obj <- tidyplots::rename_color_labels(tidyplot_obj, new_names = label_map_color)
        }
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
        shape         = ifelse(tab[["emptyCircles"]], 1, 21)
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

      tidyplot_obj <- tryCatch({
        if (!is.numeric(localData[[xVar]]) && !is.numeric(localData[[yVar]])) {
          stop("The histogram requires that the X-Axis or Y-Axis Variable is continuous", call. = FALSE)
        }
        argList <- list(
          bins  = tab[["binsPlotBuilder"]],
          alpha = tab[["alphaHistogramPlotBuilder"]]
        )
        do.call(tidyplots::add_histogram, c(list(tidyplot_obj), argList))
      },
      error = function(e) {
        stop(e$message, call. = FALSE)
      })
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

      input_text <- tab[["drawQuantilesViolinPlotBuilder"]]

      if (!grepl("^c\\(", input_text)) {
        input_text <- paste0("c(", input_text, ")")
      }

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
      if ((exists("colorBy") && colorBy %in% c("x", "y")) || tab[["blackOutlineCountLine"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }
      if (exists("colorBy") && colorBy %in% c("x", "y")) {
        argList$group <- 1
      }
      tidyplot_obj <- do.call(tidyplots::add_count_line, c(list(tidyplot_obj), argList))
      titleValueY <- tab[["titleYPlotBuilder"]]
      if (!is.null(titleValueY) && titleValueY != "") {
        tidyplot_obj <- tidyplot_obj |> tidyplots::adjust_y_axis_title(title = titleValueY)
      }
      titleValueX <- tab[["titleXPlotBuilder"]]
      if (!is.null(titleValueX) && titleValueX != "") {
        tidyplot_obj <- tidyplot_obj |> tidyplots::adjust_x_axis_title(title = titleValueX)
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
      if ((exists("colorBy") && colorBy %in% c("x", "y")) || tab[["blackOutlineSumLine"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }
      if (exists("colorBy") && colorBy %in% c("x", "y")) {
        argList$group <- 1
      }
      tidyplot_obj <- do.call(tidyplots::add_sum_line, c(list(tidyplot_obj), argList))
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

    # Add proportion bar and area ####
    # For Bar Stack, use the single "addBarStack" option.
    if (tab[["addBarStack"]]) {
      # Retrieve the proportion mode from the radio button group:
      # It should be "absolute" or "relative".
      mode <- tab[["propMode"]]

      # Create the common argument list for the bar stack.
      argList <- list(
        reverse = tab[["reverseBarStack"]],
        alpha   = tab[["alphaBarStack"]]
      )

      # Depending on the selected mode, call the appropriate function.
      if (mode == "absolute") {
        tidyplot_obj <- do.call(
          tidyplots::add_barstack_absolute,
          c(list(tidyplot_obj), argList)
        )

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

      } else if (mode == "relative") {
        tidyplot_obj <- do.call(
          tidyplots::add_barstack_relative,
          c(list(tidyplot_obj), argList)
        )

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
    }

    # For Area Stack, use the single "addAreaStack" option.
    if (tab[["addAreaStack"]]) {
      # Retrieve the proportion mode ("absolute" or "relative").
      mode <- tab[["propMode"]]

      # Create the common argument list for the area stack.
      argList <- list(
        reverse    = tab[["reverseAreaStack"]],
        alpha      = tab[["alphaAreaStack"]],
        linewidth  = tab[["linewidthAreaStack"]],
        replace_na = tab[["replaceNaAreaStack"]]
      )

      # Depending on the selected mode, call the appropriate function.
      if (mode == "absolute") {
        tidyplot_obj <- do.call(
          tidyplots::add_areastack_absolute,
          c(list(tidyplot_obj), argList)
        )
      } else if (mode == "relative") {
        tidyplot_obj <- do.call(
          tidyplots::add_areastack_relative,
          c(list(tidyplot_obj), argList)
        )
      }
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

  if ((exists("colorBy") && colorBy %in% c("x", "y")) || tab[["blackOutlineMeanLine"]]) {
    argList$color <- "black"
    argList$fill  <- "black"
  }

  if (exists("colorBy") && colorBy %in% c("x", "y")) {
    argList$group <- 1
  }

  tidyplot_obj <- do.call(
    tidyplots::add_mean_line,
    c(list(tidyplot_obj), argList)
  )
}


    # Add Mean Area (tidyplots::add_mean_area)----
    if (tab[["addMeanArea"]]) {

      argList <- list(
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
        width       = tab[["widthMedianBar"]]
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
      if ((exists("colorBy") && colorBy %in% c("x", "y")) || tab[["blackOutlineMedianLine"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }
      if (exists("colorBy") && colorBy %in% c("x", "y")) {
        argList$group <- 1
      }
      tidyplot_obj <- do.call(tidyplots::add_median_line, c(list(tidyplot_obj), argList))
    }


    # Add Median Area (tidyplots::add_median_area)----
    if (tab[["addMedianArea"]]) {

      argList <- list(
        dodge_width = tab[["dodgeMedianArea"]],
        linewidth   = tab[["linewidthMedianArea"]],
        alpha       = tab[["alphaMedianArea"]]
      )

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
        alpha      = tab[["transparencyCurveFit"]],
        se         = tab[["seCurveFit"]]
      )

      if (tab[["blackOutlineCurveFit"]]) {
        argList$color <- "black"
        argList$fill  <- "black"
      }

      # If the color is based on the X or Y variable,
      # force a single group (group = 1) so that the curve fit is applied
      # to the entire dataset instead of separately for each color.
      if (colorBy %in% c("x", "y")) {
        argList$group <- 1
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
          y         = eval(parse(text = paste0("c(", tab[["yReferenceLine"]], ")"))),
          x         = eval(parse(text = paste0("c(", tab[["xReferenceLine"]], ")"))),
          linetype  = "dashed",
          linewidth = tab[["linewidhtReferenceLines"]],
          color     = eval(parse(text = paste0("\"", tab[["colorReferenceLine"]], "\"")))
        )
    }

    # Add identity line ----
    if (!is.null(tab[["addIdentityLinePlotBuilder"]]) && tab[["addIdentityLinePlotBuilder"]]) {
      if (!is.null(xVar) && xVar %in% colnames(localData) && is.numeric(localData[[xVar]])) {
        x_min <- min(localData[[xVar]], na.rm = TRUE)
        x_max <- max(localData[[xVar]], na.rm = TRUE)
      } else {
        x_min <- 0
        x_max <- 1
      }

      if (!is.null(yVar) && yVar %in% colnames(localData) && is.numeric(localData[[yVar]])) {
        y_min <- min(localData[[yVar]], na.rm = TRUE)
        y_max <- max(localData[[yVar]], na.rm = TRUE)
      } else {
        y_min <- 0
        y_max <- 1
      }

      if (!is.null(tab[["reversedirectionIdentityLine"]]) && tab[["reversedirectionIdentityLine"]]) {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::add_annotation_line(
            x     = x_min,
            xend  = x_max,
            y     = y_max,
            yend  = y_min,
            color = eval(parse(text = paste0("\"", tab[["colorIdentityLine"]], "\"")))
          )
      } else {
        tidyplot_obj <- tidyplot_obj |>
          tidyplots::add_annotation_line(
            x     = x_min,
            xend  = x_max,
            y     = y_min,
            yend  = y_max,
            color = eval(parse(text = paste0("\"", tab[["colorIdentityLine"]], "\"")))
          )
      }
    }

    if (length(tab[["titlePlotBuilder"]]) > 0) {
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add_title(title = tab[["titlePlotBuilder"]])
    }

    if (!is.null(tab[["captionPlotBuilder"]]) && tab[["captionPlotBuilder"]] != "") {
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::add_caption(caption = tab[["captionPlotBuilder"]])
    }

   #Color palette settings ----
    if (!is.null(tab[["colorsAll"]])) {
      colorOption <- tab[["colorsAll"]]


      if (is.null(tab[["customColors"]]) || nchar(trimws(tab[["customColors"]])) == 0) {
        # define tidy plot palette
        tidy_colors <- list(
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

        # define jasp palette
        jasp_colors <- c("blue", "colorblind", "colorblind2",
                         "colorblind3", "sportsTeamsNBA", "ggplot2",
                         "grandBudapest", "jaspPalette", "gray")

        # if tidy plot palette
        if (colorOption %in% names(tidy_colors)) {
          tidyplot_obj <- tidyplot_obj |>
            tidyplots::adjust_colors(tidy_colors[[colorOption]])
        }

        # if jasp palette
        if (colorOption %in% jasp_colors) {

          activeColorVar <- NULL
          if (isTRUE(tab[["colorByVariableX"]]) && !is.null(xVar) && xVar != "") {
            activeColorVar <- xVar
          } else if (isTRUE(tab[["colorByVariableY"]]) && !is.null(yVar) && yVar != "") {
            activeColorVar <- yVar
          } else if (!is.null(colorVar) && colorVar != "") {
            activeColorVar <- colorVar
          }

          if (!is.null(activeColorVar) && activeColorVar %in% colnames(localData)) {
            if (is.factor(localData[[activeColorVar]])) {
              tidyplot_obj <- tidyplot_obj |>
                tidyplots::add(jaspGraphs::scale_JASPcolor_discrete(colorOption)) |>
                tidyplots::add(jaspGraphs::scale_JASPfill_discrete(colorOption))
            } else if (is.numeric(localData[[activeColorVar]])) {
              tidyplot_obj <- tidyplot_obj |>
                tidyplots::add(jaspGraphs::scale_JASPcolor_continuous(colorOption)) |>
                tidyplots::add(jaspGraphs::scale_JASPfill_continuous(colorOption))
            }
          } else {
            color <- jaspGraphs::JASPcolors(colorOption)
            tidyplot_obj <- tidyplot_obj |>
              tidyplots::adjust_colors(color)
          }
        }
      }
    }

    # Add custom colors (tidyplots::adjust_colors)----
    if (!is.null(tab[["customColors"]]) && nchar(trimws(tab[["customColors"]])) > 0) {
      custom_colors <- strsplit(tab[["customColors"]], ",")[[1]]
      custom_colors <- trimws(custom_colors)
      tidyplot_obj <- tidyplot_obj |>
        tidyplots::adjust_colors(new_colors = custom_colors)
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


    mode <- tab[["propMode"]]
    if (is.null(mode)) mode <- "absolute"

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

    # add padding
    adjust_args_xaxis$padding <- c(0.1, 0.1)

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

    # add padding
    adjust_args_yaxis$padding <- c(0.1, 0.1)

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

    # #Add annotation (tidyplot::add_annotation_text )----
    # if (!is.null(tab[["annotationPlotBuilder"]]) && length(tab[["annotationPlotBuilder"]]) > 0) {
    #   for (i in seq_along(tab[["annotationPlotBuilder"]])) {
    #     rowData   <- tab[["annotationPlotBuilder"]][[i]]
    #     plotText  <- rowData$annotationText
    #     plotX     <- rowData$annotationX
    #     plotY     <- rowData$annotationY
    #     fontSize  <- rowData$annotationSize
    #
    #     tidyplot_obj <- tidyplot_obj |>
    #       tidyplots::add_annotation_text(
    #       text     = plotText,
    #       x        = plotX,
    #       y        = plotY,
    #       fontsize = fontSize
    #     )
    #   }
    # }

    # Extract the ggplot object from tidyplot----
    tidyplot_obj <- tidyplot_obj[[1]]

    # Add annotation (using ggplot2::geom_text) ----
    if (!is.null(tab[["annotationPlotBuilder"]]) && length(tab[["annotationPlotBuilder"]]) > 0) {
      for (i in seq_along(tab[["annotationPlotBuilder"]])) {
        rowData  <- tab[["annotationPlotBuilder"]][[i]]
        plotText <- rowData$annotationText
        plotX    <- rowData$annotationX
        plotY    <- rowData$annotationY
        fontSize <- as.numeric(rowData$annotationSize)

        # Read annotation text color (default is black)
        colorText <- if (!is.null(rowData$colorAnnotationLine) && nzchar(rowData$colorAnnotationLine)) {
          rowData$colorAnnotationLine
        } else {
          "black"
        }

        # Convert text to a plotmath expression if it is wrapped in dollar signs,
        # e.g. "$italic(P)$" will become an expression italic(P)
        label_expr <- plotText
        if (grepl("^\\$.*\\$$", plotText)) {
          stripped <- sub("^\\$(.*)\\$$", "\\1", plotText)
          label_expr <- parse(text = stripped)
        }

        # Handle facet drop-down inputs for Column, Row, and Grid.
        # There is no prioritization; each facet field is stored as an independent column.
        facetData <- list()

        # Column facet: first check the standard drop-down; if empty, check the RM-specific one.
        if (!is.null(rowData$ColumnAnnotation) && nzchar(rowData$ColumnAnnotation)) {
          facetData[[colsVar]] <- rowData$ColumnAnnotation
        } else if (!is.null(rowData$RMColumnAnnotation) && nzchar(rowData$RMColumnAnnotation)) {
          facetData[[colsVar]] <- rowData$RMColumnAnnotation
        }

        # Row facet:
        if (!is.null(rowData$RowAnnotation) && nzchar(rowData$RowAnnotation)) {
          facetData[[rowsVar]] <- rowData$RowAnnotation
        } else if (!is.null(rowData$RMRowAnnotation) && nzchar(rowData$RMRowAnnotation)) {
          facetData[[rowsVar]] <- rowData$RMRowAnnotation
        }

        # Grid facet:
        if (!is.null(rowData$GridAnnotation) && nzchar(rowData$GridAnnotation)) {
          facetData[[gridVar]] <- rowData$GridAnnotation
        } else if (!is.null(rowData$RMGridAnnotation) && nzchar(rowData$RMGridAnnotation)) {
          facetData[[gridVar]] <- rowData$RMGridAnnotation
        }

        # If in RM mode, decode each facet value using decodeColNames()
        if (!is.null(tab[["isRM"]]) && tab[["isRM"]] == "RM" && length(facetData) > 0) {
          for (field in names(facetData)) {
            facetData[[field]] <- decodeColNames(facetData[[field]])
          }
        }

        # Build a one-row data frame to hold the annotation coordinates and all facet assignments.
        thisAnnotation <- data.frame(
          x = plotX,
          y = plotY,
          label = label_expr,
          stringsAsFactors = FALSE
        )
        # Add facet information (if available) to the annotation data frame
        if (length(facetData) > 0) {
          for (f in names(facetData)) {
            thisAnnotation[[f]] <- facetData[[f]]
          }
        }

        # Add the annotation to the ggplot object.
        tidyplot_obj <- tidyplot_obj +
          ggplot2::geom_text(
            data = thisAnnotation,
            mapping = ggplot2::aes(x = x, y = y, label = label),
            size = fontSize,
            color = colorText,
            inherit.aes = FALSE
          )
      }
    }


    # Add custom comparison line (using ggplot2::geom_text, geom_segment) ----
    rmOption <- tab[["rmFactorOptionsDropDown"]]

    if (!is.null(tab[["annotationLineList"]]) && length(tab[["annotationLineList"]]) > 0) {

      if (!is.null(xVar) && xVar %in% colnames(localData) && !is.factor(localData[[xVar]])) {
        stop("The X-Axis variable can only be a factor variable. The Y-Axis variable must be either continuous or ordinal.", call. = FALSE)
      }

      if (!is.null(yVar) && yVar %in% colnames(localData) && !(is.numeric(localData[[yVar]]) || is.ordered(localData[[yVar]]))) {
        stop("The X-Axis variable can only be a factor variable. The Y-Axis variable must be either continuous or ordinal.", call. = FALSE)
      }



      for (line in tab[["annotationLineList"]]) {
        if (!is.null(line[["xAnnotation"]]) && nzchar(line[["xAnnotation"]]) &&
            !is.null(line[["xendAnnotation"]]) && nzchar(line[["xendAnnotation"]]) &&
            !is.null(line[["yAnnotation"]]) && nzchar(line[["yAnnotation"]]) &&
            !is.null(line[["yendAnnotation"]]) && nzchar(line[["yendAnnotation"]])) {

          x_val    <- eval(parse(text = paste0("c(", line[["xAnnotation"]], ")")))
          xend_val <- eval(parse(text = paste0("c(", line[["xendAnnotation"]], ")")))
          y_val    <- eval(parse(text = paste0("c(", line[["yAnnotation"]], ")")))
          yend_val <- eval(parse(text = paste0("c(", line[["yendAnnotation"]], ")")))

          if (rmOption == "rmFactorAsColumnSplit") {
            colAnnotLine <- if (!is.null(line[["RMColumnCompLine"]])) decodeColNames(line[["RMColumnCompLine"]]) else ""
          } else {
            colAnnotLine <- if (!is.null(line[["ColumnAnnotationCompLine"]])) line[["ColumnAnnotationCompLine"]] else ""
          }

          if (rmOption == "rmFactorAsRowSplit") {
            rowAnnotLine <- if (!is.null(line[["RMRowCompLine"]])) decodeColNames(line[["RMRowCompLine"]]) else ""
          } else {
            rowAnnotLine <- if (!is.null(line[["RowAnnotationCompLine"]])) line[["RowAnnotationCompLine"]] else ""
          }

          if (rmOption == "rmFactorAsGrid") {
            gridAnnotLine <- if (!is.null(line[["RMGridCompLine"]])) decodeColNames(line[["RMGridCompLine"]]) else ""
          } else {
            gridAnnotLine <- if (!is.null(line[["GridAnnotationCompLine"]])) line[["GridAnnotationCompLine"]] else ""
          }

          thisSegment <- data.frame(
            x = x_val,
            xend = xend_val,
            y = y_val,
            yend = yend_val,
            stringsAsFactors = FALSE
          )
          if (!is.null(colsVar) && nzchar(colAnnotLine)) {
            thisSegment[[colsVar]] <- colAnnotLine
          }
          if (!is.null(rowsVar) && nzchar(rowAnnotLine)) {
            thisSegment[[rowsVar]] <- rowAnnotLine
          }
          if (!is.null(gridVar) && nzchar(gridAnnotLine)) {
            thisSegment[[gridVar]] <- gridAnnotLine
          }

          tidyplot_obj <- tidyplot_obj +
            ggplot2::geom_segment(
              data = thisSegment,
              mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
              color = eval(parse(text = paste0("\"", line[["colorAnnotationLine"]], "\""))),
              inherit.aes = FALSE
            )

          if (!is.null(line[["textAnnotationline"]]) && nzchar(line[["textAnnotationline"]])) {
            x_mid <- (x_val + xend_val) / 2
            y_mid <- (y_val + yend_val) / 2

            offset <- as.numeric(line[["textDistanceAnnotationLine"]])
            globalYMin   <- min(localData[[yVar]], na.rm = TRUE)
            globalYMax   <- max(localData[[yVar]], na.rm = TRUE)
            globalYRange <- globalYMax - globalYMin
            offset_scaled <- offset * (globalYRange / 10)

            thisText <- data.frame(
              x = x_mid,
              y = y_mid + offset_scaled,
              label = eval(parse(text = paste0("\"", line[["textAnnotationline"]], "\""))),
              stringsAsFactors = FALSE
            )
            if (!is.null(colsVar) && nzchar(colAnnotLine)) {
              thisText[[colsVar]] <- colAnnotLine
            }
            if (!is.null(rowsVar) && nzchar(rowAnnotLine)) {
              thisText[[rowsVar]] <- rowAnnotLine
            }
            if (!is.null(gridVar) && nzchar(gridAnnotLine)) {s
              thisText[[gridVar]] <- gridAnnotLine
            }

            tidyplot_obj <- tidyplot_obj +
              ggplot2::geom_text(
                data = thisText,
                mapping = ggplot2::aes(x = x, y = y, label = label),
                size = as.numeric(line[["textSizeAnnotationLine"]]),
                inherit.aes = FALSE
              )
          }

        }
      }
    }









    if (tab[["propMode"]] == "relative") {
      blankLayer <- ggplot2::geom_blank(
        data = data.frame(x = c(0, 1), y = c(0, 1)),
        mapping = ggplot2::aes(x = x, y = y),
        inherit.aes = FALSE
      )
    } else {
      blankLayer <- ggplot2::geom_blank()
    }


    # Apply theme
    if (plotStyle == "JASP") {
      tidyplot_obj <- tidyplot_obj +
        jaspGraphs::themeJaspRaw(fontsize = baseFontSize, legend.position = legend_position) +
        blankLayer +
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
        axis.text.x = ggplot2::element_text(size = baseFontSize * 0.85),
        axis.text.y = ggplot2::element_text(size = baseFontSize * 0.85),
        legend.title = ggplot2::element_text(
          size   = baseFontSize * 0.85,
          hjust  = 0,
          margin = ggplot2::margin(5, 5, 5, 5)
        ),
        legend.text = ggplot2::element_text(
          size   = baseFontSize * 0.8,
          margin = ggplot2::margin(5, 5, 5, 5)
        ),
        legend.margin = ggplot2::margin(10, 10, 10, 10),
        plot.margin   = ggplot2::margin(t = t, r = r, b = b, l = l)
      )

    # Remove legend title if requested (tidyplots::remove_legend_title)
    if (tab[["removeLegendTitle"]]) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::theme(legend.title = ggplot2:::element_blank())
    }

    # Facet logic for row / column ----

    hasRows <- (!is.null(rowsVar) && rowsVar != "")
    hasCols <- (!is.null(colsVar) && colsVar != "")
    as_table_value <- if (tab[["asTable"]] %in% c("bottom-right", 0)) TRUE else FALSE


    if (hasRows) {
      # For the displayed label, we do NOT add backticks
      yAxisTitleSplit <- tab[["yAxisTitleSplit"]]

      if (is.null(yAxisTitleSplit) || nchar(trimws(yAxisTitleSplit)) == 0) {
        axis_name <- rowsVar
      } else {
        axis_name <- yAxisTitleSplit
      }

      tidyplot_obj <- tidyplot_obj +
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(~ .,
                                       name   = axis_name,  # display "Repeated measures" without backticks
                                       breaks = NULL,
                                       labels = NULL
          )
        )
    }

    if (hasCols) {
      # For the displayed label, we do NOT add backticks
      xAxisTitleSplit <- tab[["xAxisTitleSplit"]]

      if (is.null(xAxisTitleSplit) || nchar(trimws(xAxisTitleSplit)) == 0) {
        subtitle_name <- colsVar
      } else {
        subtitle_name <- xAxisTitleSplit
      }

      tidyplot_obj <- tidyplot_obj +
        ggplot2::labs(subtitle = subtitle_name) +  # display "Repeated measures" without backticks
        ggplot2::theme(
          plot.subtitle = ggplot2::element_text(size = baseFontSize, hjust = 0.5),
          strip.text     = ggplot2::element_text(size = baseFontSize)
        )
    }

    # Use backticks ONLY inside the formula:
    if (hasRows && hasCols) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::facet_grid(
          stats::as.formula(paste0("`", rowsVar, "` ~ `", colsVar, "`")),
          scales = tab[["scales"]],
          axes = tab[["axes"]],
          axis.labels = tab[["axisLabels"]],
          as.table = as_table_value,
          space = tab[["space"]],
          margins = tab[["margins"]]
        )
    } else if (hasCols) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::facet_grid(
          stats::as.formula(paste0(". ~ `", colsVar, "`")),
          scales = tab[["scales"]],
          axes = tab[["axes"]],
          axis.labels = tab[["axisLabels"]],
          as.table = as_table_value,
          space = tab[["space"]],
          margins = tab[["margins"]]
        )
    } else if (hasRows) {
      tidyplot_obj <- tidyplot_obj +
        ggplot2::facet_grid(
          stats::as.formula(paste0("`", rowsVar, "` ~ .")),
          scales = tab[["scales"]],
          axes = tab[["axes"]],
          axis.labels = tab[["axisLabels"]],
          as.table = as_table_value,
          space = tab[["space"]],
          margins = tab[["margins"]]
        )
    }

    # Facet logic for facet_wrap using gridVariable ----
    hasGrid <- (!is.null(gridVar) && gridVar != "")

    if (hasGrid) {
      as_table_value <- if (tab[["asTableFacetWrap"]] %in% c("bottom-rightFacetWrap", 0)) TRUE else FALSE

      nrow_value <- if (!is.null(tab[["nrowFacetWrap"]]) && nchar(trimws(tab[["nrowFacetWrap"]])) > 0)
        as.numeric(tab[["nrowFacetWrap"]]) else NULL
      ncol_value <- if (!is.null(tab[["ncolFacetWrap"]]) && nchar(trimws(tab[["ncolFacetWrap"]])) > 0)
        as.numeric(tab[["ncolFacetWrap"]]) else NULL

      tidyplot_obj <- tidyplot_obj +
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~ `", gridVar, "`")),
          nrow = nrow_value,
          ncol = ncol_value,
          scales = tab[["scalesFacetWrap"]],
          as.table = as_table_value,
          strip.position = tab[["stripPosition"]]
        )
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

      if (!is.null(xVar) && xVar %in% colnames(localData) && !is.factor(localData[[xVar]])) {
        stop("The X-Axis variable can only be a factor variable. The Y-Axis variable must be either continuous or ordinal.", call. = FALSE)
      }

      if (!is.null(yVar) && yVar %in% colnames(localData) && !(is.numeric(localData[[yVar]]) || is.ordered(localData[[yVar]]))) {
        stop("The X-Axis variable can only be a factor variable. The Y-Axis variable must be either continuous or ordinal.", call. = FALSE)
      }



      # universal settings for the p value
      label_size <- tab[["labelSizePValue"]]
      labelcolor <- tab[["labelcolor"]]

      # separate settings for each bracket
      dfComparisons <- data.frame(
        group1     = as.character(sapply(tab[["pairwiseComparisons"]], function(x) x$group1)),
        group2     = as.character(sapply(tab[["pairwiseComparisons"]], function(x) x$group2)),
        p          = as.character(sapply(tab[["pairwiseComparisons"]], function(x) x$pAdj)),
        y.position =  tab[["yPositionPValue"]],
        stringsAsFactors = FALSE
      )

      rmOption <- tab[["rmFactorOptionsDropDown"]]

      # Color settings: check for both standard and RM-specific variable names
      if (!is.null(colorVar) && nchar(colorVar) > 0) {
        dfComparisons$color <- as.character(
          sapply(tab[["pairwiseComparisons"]], function(x) {
            if (rmOption == "rmFactorAsGroup")
              decodeColNames(x$RMGroupPValue)
            else
              x$GroupPValue  # Use standard value otherwise
          })
        )
      }

      # Rows grouping: check for both standard and RM-specific variable names
      if (!is.null(rowsVar)) {
        dfComparisons[[rowsVar]] <- as.character(
          sapply(tab[["pairwiseComparisons"]], function(x) {
            if (rmOption == "rmFactorAsRowSplit")
              decodeColNames(x$RMRowPValue)
            else
              x$RowPValue  # Use standard value otherwise
          })
        )
      }

      # Columns grouping: check for both standard and RM-specific variable names
      if (!is.null(colsVar)) {
        dfComparisons[[colsVar]] <- as.character(
          sapply(tab[["pairwiseComparisons"]], function(x) {
            if (rmOption == "rmFactorAsColumnSplit")
              decodeColNames(x$RMColumnPValue)
            else
              x$ColumnPValue  # Use standard value otherwise
          })
        )
      }

      # Grid: check for both standard and RM-specific variable names
      if (!is.null(gridVar)) {
        dfComparisons[[gridVar]] <- as.character(
          sapply(tab[["pairwiseComparisons"]], function(x) {
            if (rmOption == "rmFactorAsGrid")
              decodeColNames(x$RMGridPValue)
            else
              x$GridPValue  # Use standard value otherwise
          })
        )
      }



      bracket.size <- unique(sapply(tab[["pairwiseComparisons"]], function(x) x$bracketSizePValue))
      tip_length   <- unique(sapply(tab[["pairwiseComparisons"]], function(x) x$tipLengthPValue))
      stepDistance <- tab[["stepDistance"]]

      # Itt jön az új logika: pValueColor
      pValueColor <- labelcolor  # default
      if (!is.null(colorVar) && nchar(colorVar) > 0) {
        if (!colorBy %in% c("x", "y")) {
          pValueColor <- "color"  # <-- csak akkor "color", ha NEM x/y
        }
      }

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
          color         = pValueColor,
          inherit.aes   = FALSE,
          step.increase = stepDistance
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
    tidyPlotsContainer <- createJaspContainer(title = gettext("Individual Plots"))
    jaspResults[["tidyPlotsContainer"]] <- tidyPlotsContainer
  } else {
    tidyPlotsContainer <- jaspResults[["tidyPlotsContainer"]]
  }

  # Render individual tidy plots in their container
  for (plotId in names(updatedPlots)) {

    plotKey <- paste0("tidyPlot_", plotId)
    if (!is(tidyPlotsContainer[[plotKey]], "JaspPlot")) {
      # Retrieve plot dimensions from the corresponding tab
      tab <- NULL
      for (t in options$PlotBuilderTab) {
        if (as.character(t$value) == plotId) {
          tab <- t
          break
        }
      }

      tidyPlot <- createJaspPlot(
        title  = paste(plotId),
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
    availablePlotIDsContainer <- createJaspContainer(title = gettext("Available Plot IDs"))
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


# plot layout  ----
.validLayoutSpecified <- function(options) {
  validRowSpecs <- FALSE
  if (!is.null(options[["rowSpecifications"]]) && length(options[["rowSpecifications"]]) > 0) {
    validRowSpecs <- any(sapply(options[["rowSpecifications"]], function(spec) {
      !is.null(spec[["plotIDs"]]) && nchar(spec[["plotIDs"]]) > 0
    }))
  }

  validFullRowSpecs <- FALSE
  if (!is.null(options[["fullRowSpecifications"]]) && length(options[["fullRowSpecifications"]]) > 0) {
    validFullRowSpecs <- any(sapply(options[["fullRowSpecifications"]], function(spec) {
      !is.null(spec[["plotIDsFullRow"]]) && nchar(spec[["plotIDsFullRow"]]) > 0
    }))
  }
  return(validRowSpecs || validFullRowSpecs)
}


.plotBuilderOutputPlotGrid <- function(jaspResults, options, plotResults, dataset) {
  updatedPlots <- plotResults$updatedPlots

  if (!is(jaspResults[["plotGridContainer"]], "JaspContainer")) {
    plotGridContainer <- createJaspContainer(gettext(title = "Plot Grid"))
    jaspResults[["plotGridContainer"]] <- plotGridContainer
  } else {
    plotGridContainer <- jaspResults[["plotGridContainer"]]
  }

  getCommonLegendGlobal <- FALSE
  if (!is.null(options[["getCommonLegend"]]) && options[["getCommonLegend"]]) {
    getCommonLegendGlobal <- TRUE
  }

  # Based on the plotSpacing option, we set the gap size (in pt) and calculate the half value
  desiredGap <- if (!is.null(options[["plotSpacing"]]) && options[["plotSpacing"]] != "")
    as.numeric(options[["plotSpacing"]]) else 0
  halfGap <- desiredGap / 2

  if (.validLayoutSpecified(options)) {
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
          error = function(e) { rep(1, ncol) }
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
        if (is.na(labelSizeParsed)) { labelSizeParsed <- 5 }
        labelSize <- labelSizeParsed
      }

      relativeHeight <- options[["relativeHeight"]]
      if (!is.null(relativeHeight) && relativeHeight != "") {
        if (!grepl("^c\\s*\\(", relativeHeight)) {
          relativeHeight <- paste0("c(", relativeHeight, ")")
        }
        relativeHeight <- tryCatch(
          eval(parse(text = relativeHeight)),
          error = function(e) { rep(1, ncol) }
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

        # Daraboljuk a plot azonosítókat vessző mentén, megtartva az üres elemeket
        plotIDs <- unlist(strsplit(plotIDsStr, ","))
        if(length(plotIDs) == 0) {
          plotIDs <- c("")
        } else {
          plotIDs <- sapply(plotIDs, function(x) {
            if(nchar(trimws(x)) == 0) "" else trimws(x)
          })
        }
        nRows <- length(plotIDs)

        rowHeights <- rep(1, nRows)
        if (!is.null(rowHeightsStr) && rowHeightsStr != "") {
          if (!grepl("^c\\s*\\(", rowHeightsStr)) {
            rowHeightsStr <- paste0("c(", rowHeightsStr, ")")
          }
          rowHeightsParsed <- tryCatch(
            eval(parse(text = rowHeightsStr)),
            error = function(e) { rep(1, nRows) }
          )
          if (length(rowHeightsParsed) == nRows) { rowHeights <- rowHeightsParsed }
        }

        labels <- NULL
        if (!is.null(labelsStr) && labelsStr != "") {
          labels <- unlist(strsplit(labelsStr, ","))
          labels <- sapply(labels, function(x) {
            if(nchar(trimws(x)) == 0) "" else trimws(x)
          })
          if (length(labels) != nRows) { labels <- NULL }
        }

        plotsInColumn <- vector("list", nRows)
        for (rowIdx in seq_len(nRows)) {
          plotID <- plotIDs[rowIdx]
          if (is.na(plotID) || plotID == "") {
            p <- ggplot2::ggplot() + ggplot2::theme_void()
          } else {
            p <- updatedPlots[[plotID]]
            if (!is.null(p)) {
              if (!is.null(labels) && labels[rowIdx] != "") {
                p <- p +
                  ggplot2::labs(tag = labels[rowIdx]) +
                  ggplot2::theme(
                    plot.tag.position = c(options[["labelDistance1"]], options[["labelDistance2"]]),
                    plot.tag          = ggplot2::element_text(size = labelSize, face = "bold")
                  )
              }
              if (getCommonLegendGlobal) {
                p <- p + ggplot2::theme(legend.position = "none")
              }
            } else {
              p <- ggplot2::ggplot() + ggplot2::theme_void()
            }
          }

          topMargin    <- if (rowIdx == 1) 0 else halfGap
          bottomMargin <- if (rowIdx == nRows && is.null(fullRowGrid)) 0 else halfGap
          leftMargin   <- if (colIdx == 1) 0 else halfGap
          rightMargin  <- if (colIdx == ncol) 0 else halfGap

          p <- p + ggplot2::theme(plot.margin = grid::unit(c(topMargin, rightMargin, bottomMargin, leftMargin), "pt"))
          plotsInColumn[[rowIdx]] <- p
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
        layoutGuides <- "auto"
        columnGrid <- patchwork::wrap_plots(columnPlots, ncol = ncol, widths = columnsWidth) +
          patchwork::plot_layout(guides = layoutGuides)
      }
    }

    fullRowSpecifications <- options[["fullRowSpecifications"]]
    if (!is.null(fullRowSpecifications) && length(fullRowSpecifications) > 0) {
      fullRowPlots <- list()

      labelSize <- 5
      if (!is.null(options[["labelSize"]])) {
        labelSizeParsed <- as.numeric(options[["labelSize"]])
        if (is.na(labelSizeParsed)) { labelSizeParsed <- 5 }
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

        plotIDs <- unlist(strsplit(plotIDsStr, ","))
        if(length(plotIDs) == 0) {
          plotIDs <- c("")
        } else {
          plotIDs <- sapply(plotIDs, function(x) {
            if(nchar(trimws(x)) == 0) "" else trimws(x)
          })
        }
        nPlots <- length(plotIDs)

        relWidths <- rep(1, nPlots)
        if (!is.null(relWidthsStr) && relWidthsStr != "") {
          if (!grepl("^c\\s*\\(", relWidthsStr)) {
            relWidthsStr <- paste0("c(", relWidthsStr, ")")
          }
          relWidthsParsed <- tryCatch(
            eval(parse(text = relWidthsStr)),
            error = function(e) { rep(1, nPlots) }
          )
          if (length(relWidthsParsed) == nPlots) { relWidths <- relWidthsParsed }
        }

        labels <- NULL
        if (!is.null(labelsStr) && labelsStr != "") {
          labels <- unlist(strsplit(labelsStr, ","))
          labels <- sapply(labels, function(x) {
            if(nchar(trimws(x)) == 0) "" else trimws(x)
          })
          if (length(labels) != nPlots) { labels <- NULL }
        }

        plotsInFullRow <- vector("list", nPlots)
        for (idx in seq_len(nPlots)) {
          plotID <- plotIDs[idx]
          if (is.na(plotID) || plotID == "") {
            p <- ggplot2::ggplot() + ggplot2::theme_void()
          } else {
            p <- updatedPlots[[plotID]]
            if (!is.null(p)) {
              if (!is.null(labels) && labels[idx] != "") {
                p <- p +
                  ggplot2::labs(tag = labels[idx]) +
                  ggplot2::theme(
                    plot.tag.position = c(options[["labelDistance1"]], options[["labelDistance2"]]),
                    plot.tag          = ggplot2::element_text(size = labelSize, face = "bold")
                  )
              }
              if (getCommonLegendGlobal) {
                p <- p + ggplot2::theme(legend.position = "none")
              }
            } else {
              p <- ggplot2::ggplot() + ggplot2::theme_void()
            }
          }

          topMargin    <- if (rowIdx == 1 && is.null(columnGrid)) 0 else halfGap
          bottomMargin <- if (rowIdx == length(fullRowSpecifications)) 0 else halfGap
          leftMargin   <- if (idx == 1) 0 else halfGap
          rightMargin  <- if (idx == nPlots) 0 else halfGap

          p <- p + ggplot2::theme(plot.margin = grid::unit(c(topMargin, rightMargin, bottomMargin, leftMargin), "pt"))
          plotsInFullRow[[idx]] <- p
        }

        collectLegends <- if (getCommonLegendGlobal) FALSE else getCommonLegendRow
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
          error = function(e) { rep(1, length(fullRowPlots)) }
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
        error = function(e) { c(1, 1) }
      )
      if (!is.null(columnGrid) && !is.null(fullRowGrid)) {
        if (length(relativeHeight) != 2) { relativeHeight <- c(1, 1) }
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
          "rowSpecifications", "fullRowSpecifications",
          "getCommonLegend", "relHeightWithinRowLayout", "relativeHeight",
          "columnWidthInput", "labelSize",
          "layoutWidth", "layoutHeight",
          "plotSpacing"
        )
      )
      plotGridContainer[["plotGrid"]] <- gridPlot
    } else {
      plotGridContainer[["plotGrid"]]$plotObject <- finalGrid
    }
  }
}
