#
# Copyright (C) 2013-2024 University of Amsterdam
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



# Main function: raincloudPlots() ----
raincloudPlotsInternal <- function(jaspResults, dataset, options) {
  ready    <- (length(options$dependentVariables) > 0)
  dataset  <- .rainReadData(dataset, options)
  dataInfo <- .rainDataInfo(dataset, options)
  .rainCreatePlots( jaspResults, dataInfo, options, ready)
  .rainCreateTables(jaspResults, dataInfo, options, ready)
}  # End raincloudPlots()



# .rainReadData() ----
.rainReadData <- function(dataset, options) {

  if(!is.null(dataset)) {
    output <- dataset
  } else {

    # Step 1: Read in all variables that are given by JASP; if no input, then nothing is read in
    columnsVector <- c(options$dependentVariables)
    optionsVector <- c(options$primaryFactor, options$secondaryFactor, options$covariate, options$observationId)
    for (option in optionsVector) {
      if (option != "") columnsVector <- c(columnsVector, option)
    }
    datasetInProgress <- .readDataSetToEnd(columns = columnsVector)

    # Step 2: Create columns with consistent names; if no input then assign default
    datasetInProgress$primaryFactor   <- .rainDataColumn(datasetInProgress,  options$primaryFactor)
    datasetInProgress$secondaryFactor <- .rainDataColumn(datasetInProgress,  options$secondaryFactor)
    datasetInProgress$covariate       <- .rainDataColumn(datasetInProgress,  options$covariate)
    datasetInProgress$observationId   <- .rainDataColumn(datasetInProgress,  options$observationId)

    # Step 3: Make sure, that factors are factors
    datasetInProgress$primaryFactor   <- as.factor(datasetInProgress$primaryFactor)
    datasetInProgress$secondaryFactor <- as.factor(datasetInProgress$secondaryFactor)


    output <- datasetInProgress
  }

  return(output)
} # End .rainReadData()



# .rainDataColumn() ----
.rainDataColumn <- function(inputDataset, inputOption) {
  output <- if (inputOption != "") {
    inputDataset[[inputOption]]
  } else {
      as.factor(rep("none", nrow(inputDataset)))
  }
  return(output)
}  # End .rainDataColumn()



# .rainDataInfo() ----
.rainDataInfo <- function(dataset, options) {

  # Omit NAs row-wise
  exclusiveDataset   <- na.omit(dataset)
  numberOfExclusions <- nrow(dataset) - nrow(exclusiveDataset)
  sampleSize         <- nrow(exclusiveDataset)
  dataset            <- exclusiveDataset

  # Calculate meanInterval
  if (options$meanInterval || options$meanIntervalCustom) {
    intervalBounds <- list()
    for (dependentVariable in options$dependentVariables) {
      intervalBounds[[dependentVariable]] <- .rainMeanInterval(dataset, options, dependentVariable)
    }
  } else {
    intervalBounds <- NULL
  }

  output <- list(
    dataset            = dataset,
    sampleSize         = sampleSize,
    numberOfExclusions = numberOfExclusions,
    intervalBounds     = intervalBounds
  )
  return(output)
}  # End .rainDataInfo()



# .rainMeanInterval() ----
.rainMeanInterval <- function(dataset, options, inputVariable) {

  infoFactorCombinations <- .rainInfoFactorCombinations(dataset, inputPlot = NULL, extractColor = FALSE)
  uniqueCombis           <- infoFactorCombinations$uniqueCombis

  output <- list(lowerBound = c(), upperBound = c(), successfulComputation = FALSE, sd = c())

  if (options$meanIntervalCustom) {
    if (options$numberOfClouds == nrow(uniqueCombis)) {
      output$lowerBound <- options$customizationTable[[2]]$values
      output$upperBound <- options$customizationTable[[3]]$values
      output$successfulComputation <- TRUE
    } else {
      # successfulComputation remains FALSE
    }
    return(output)
  }

  # Loop through each cell of the design...
  for (cloudNumber in 1:nrow(uniqueCombis)) {
    primaryLevel   <- as.character(uniqueCombis$primaryFactor[cloudNumber])    # as.character() ensures that it also works
    secondaryLevel <- as.character(uniqueCombis$secondaryFactor[cloudNumber])  # for numeric factor levels
    currentCell    <- dataset[dataset$primaryFactor == primaryLevel & dataset$secondaryFactor == secondaryLevel, ][[inputVariable]]

    # ...and calculate lowerBound, upperBound accordingly
    if (options$meanIntervalOption == "sd") {
      xBar <- mean(currentCell)
      sd   <- sd(currentCell)
      output$sd[cloudNumber] <- sd
      output$lowerBound[cloudNumber] <- xBar - sd
      output$upperBound[cloudNumber] <- xBar + sd
      output$successfulComputation <- TRUE

    } else if (options$observationId == "" && options$meanIntervalOption == "ci" && options$meanCiAssumption) {
      # The following is adapted from .descriptivesMeanCI() in jaspDescriptives

      if (options$meanCiMethod == "normalModel") {
        xBar <- mean(currentCell)
        se <- sd(currentCell) / sqrt(length(currentCell))
        z <- qnorm((1 - options$meanCiWidth) / 2, lower.tail = FALSE)
        output$lowerBound[cloudNumber] <- xBar - z * se
        output$upperBound[cloudNumber] <- xBar + z * se
        output$successfulComputation <- TRUE

      } else if (options$meanCiMethod == "oneSampleTTest") {
        ttestResult <- stats::t.test(x = currentCell, conf.level = options$meanCiWidth)
        CIs <- ttestResult[["conf.int"]]
        output$lowerBound[cloudNumber] <- CIs[1]
        output$upperBound[cloudNumber] <- CIs[2]
        output$successfulComputation <- TRUE

      } else if (options$meanCiMethod == "bootstrap") {
        k <- options$meanCiBootstrapSamples
        means <- numeric(k)
        n <- length(currentCell)
        jaspBase::.setSeedJASP(options)
        for (i in seq_len(k)) {
          bootData <- sample(currentCell, size = n, replace = TRUE)
          means[i] <- mean(bootData)
        }
        percentiles <- (1 + c(-options$meanCiWidth, options$meanCiWidth)) / 2
        CIs <- quantile(means, probs = percentiles)
        output$lowerBound[cloudNumber] <- CIs[1]
        output$upperBound[cloudNumber] <- CIs[2]
        output$successfulComputation <- TRUE
      }

    }

  }

  return(output)
}  # End .rainMeanInterval()



# .rainCreatePlots() ----
# Creates a container with a plot for each options$dependentVariables - if none then placeholder
.rainCreatePlots <- function(jaspResults, dataInfo, options, ready) {

  # Create container in jaspResults
  if (is.null(jaspResults[["containerRaincloudPlots"]])) {

    jaspResults[["containerRaincloudPlots"]] <- createJaspContainer(title = gettext("Raincloud Plots"))
    jaspResults[["containerRaincloudPlots"]]$dependOn(
      c(
        "primaryFactor", "secondaryFactor", "covariate", "observationId",  # VariablesForm

        "colorPalette", "colorAnyway",  # General Settings
        "covariatePalette",
        "horizontal",

        "vioNudge",        "boxNudge",        "pointNudge",  # Cloud Elements
        "vioHeight",       "boxWidth",        "pointSpread",
        "vioSmoothing",    "boxPadding",      "pointSize",
        "vioOpacity",      "boxOpacity",      "pointOpacity",
        "vioOutline",      "boxOutline",      "jitter",
        "vioOutlineWidth", "boxOutlineWidth",
        "observationIdLineOpacity", "observationIdLineWidth",

        "customAxisLimits",     "lowerAxisLimit",  "upperAxisLimit",  # Axes, Legend, Caption, Plot size
        "showCaption",
        "widthPlot", "heightPlot",

        "customSides",  # Advanced
        "mean", "meanPosition", "meanSize", "meanLines", "meanLinesWidth", "meanLinesOpacity"
      )
    )
  }  # End create container

  # Access through container object
  container <- jaspResults[["containerRaincloudPlots"]]

  # Placeholder plot, if no dependentVariables
  if (!ready) {
    container[["placeholder"]] <- createJaspPlot(title = "", dependencies = c("dependentVariables"))
    return()
  }

  # Plot for each dependentVariable
  for (variable in options$dependentVariables) {

    # If plot for variable already exists, we can skip recalculating plot
    if (!is.null(container$variable)) next

    variablePlot <- createJaspPlot(title = variable, width = options$widthPlot, height = options$heightPlot)
    variablePlot$dependOn(optionContainsValue = list(dependentVariables = variable))  # Depends on respective variable

    variablePlot[["plotObject"]] <- .rainFillPlot(dataInfo, options, variable)

    container[[variable]] <- variablePlot
  }  # End of for loop

}  # End .rainCreatePlots()



# .rainFillPlot() ----
# Fills each inputPlot from .rainCreatePlots() with ggplot + palettes + geom_rain() + theme
.rainFillPlot <- function(dataInfo, options, inputVariable) {

  dataset <- dataInfo$dataset

  # Ggplot() with aes()
  aesX     <- dataset$primaryFactor
  aesFill  <- if(options$secondaryFactor != "") dataset$secondaryFactor else if (options$colorAnyway) aesX else NULL
  aesColor <- if(options$covariate       != "") dataset$covariate       else if (options$colorAnyway) aesX else aesFill
  aesArg   <- ggplot2::aes(y = .data[[inputVariable]], x = aesX, fill = aesFill, color = aesColor)
  plotInProgress <- ggplot2::ggplot(data = dataset, mapping = aesArg)

  # Palettes
  palettes       <- .rainSetPalettes(dataset, options)
  plotInProgress <- plotInProgress + palettes$fill + palettes$color

  # Preparation
  infoFactorCombinations <- .rainInfoFactorCombinations(dataset, plotInProgress)  # Also has color info

  getVioSides   <- .rainSetVioSides(options, dataset, infoFactorCombinations)  # Default "r" or custom orientation
  vioSides      <- getVioSides$sides
  errorVioSides <- getVioSides$error

  boxPosVec   <- .rainNudgeForEachCloud(options$boxNudge, vioSides)
  boxPosition <- ggpp::position_dodge2nudge(  # boxPosition for whiskers and .rainGeomRain
    x        = boxPosVec,
    width    = 0,
    padding  = options$boxPadding,
    preserve = "single"  # All boxes same width and different amounts of boxes are centered around middle
  )

  # .rainGeomRain() - workhorse function, uses ggrain::geom_rain()
  plotInProgress <- plotInProgress + .rainGeomRain(
    dataset, options, infoFactorCombinations, vioSides, boxPosition, plotInProgress
  )

  # Whiskers for boxplots
  boxDataIndex   <- if (options$observationId == "") 2 else 3
  boxData        <- ggplot2::ggplot_build(plotInProgress)$data[[boxDataIndex]]
  getWhiskers    <- .rainWhiskers(options, boxData, infoFactorCombinations, boxPosition)
  plotInProgress <- plotInProgress + getWhiskers$lowerWhiskers + getWhiskers$upperWhiskers

  # Means and Lines
  meanPosition <- if (options$meanPosition == "likeBox") {
    ggpp::position_dodge2nudge(x = boxPosVec, width = options$boxWidth, preserve = "single")
  } else {
    ggpp::position_dodge2nudge(x = 0, width = 0.00000000000001, preserve = "single")
    # With the default "identity" as position, colors of meanLines would be scrambled
    # and width = 0 messes up the position of the means (same for several position_ functions I tried)
    # Thus, set very small width that the human eye will not notice.
  }
  getMeansAndLines <- .rainMeansAndLines(options, boxPosVec, aesX, aesFill, infoFactorCombinations, meanPosition)
  plotInProgress <- plotInProgress + getMeansAndLines$meanLines + getMeansAndLines$means  # Lines first so means cover

  # Interval around mean
  intervalPosition <- if (options$meanPosition == "likeBox") meanPosition else "identity"
  if (options$meanInterval || options$meanIntervalCustom) {
    intervalBounds <- dataInfo$intervalBounds[[inputVariable]]
      if (intervalBounds$successfulComputation) {
        lowerBound <- intervalBounds$lowerBound
        upperBound <- intervalBounds$upperBound
        meanInterval <- ggplot2::stat_summary(
          ggplot2::aes(ymin = ..y.. - (..y.. - lowerBound), ymax = ..y.. + (upperBound - ..y..)),
          fun         = mean,
          geom        = "errorbar",
          width       = options$boxWidth,
          lwd         = options$boxOutlineWidth,
          position    = intervalPosition,
          color       = .rainOutlineColor(options, "colorPalette", infoFactorCombinations),
          show.legend = FALSE
        )
      } else {
        meanInterval <- NULL
      }
  } else {
    meanInterval <- NULL
  }
  plotInProgress <- plotInProgress + meanInterval


  # Horizontal plot?
  if (options$horizontal) plotInProgress <- plotInProgress + ggplot2::coord_flip()

  # Theme setup
  plotInProgress <- plotInProgress + jaspGraphs::geom_rangeframe() + jaspGraphs::themeJaspRaw(legend.position = "right")

  # Axes
  if (!options$customAxisLimits) {
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(dataset[[inputVariable]])
    yLimits <- range(c(yBreaks, dataset[[inputVariable]]))
    warningAxisLimits <- FALSE
  } else {
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(options$lowerAxisLimit, options$upperAxisLimit))
    yLimits <- range(yBreaks)
    warningAxisLimits <- if (
      min(yBreaks) > min(dataset[[inputVariable]]) || max(yBreaks) < max(dataset[[inputVariable]])
    ) {
      TRUE
    } else {
      FALSE
    }
  }
  yAxis   <- ggplot2::scale_y_continuous(breaks = yBreaks, limits = yLimits)

  inwardTicks <- ggplot2::theme(axis.ticks.length = ggplot2::unit(-0.25, "cm"))

  xTitle     <- if (options$primaryFactor == "") "Total" else options$primaryFactor
  axisTitles <- ggplot2::labs(x = xTitle, y = inputVariable)

  noFactorBlankAxis <- if (options$primaryFactor == "") {
    if (!options$horizontal) {
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
    } else {
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
    }
  } else {
    NULL
  }

  plotInProgress <- plotInProgress + yAxis + inwardTicks + axisTitles + noFactorBlankAxis

  # Legend
  guideFill <- if (options$primaryFactor == ""  && options$secondaryFactor == "") {
    "none"  # If there is just a single cloud and colorAnyway, we do not need a legend
  } else {
    ggplot2::guide_legend(
      order = 1, reverse = options$horizontal, override.aes = list(alpha = 0.5, color = NA)  # NA removes points in boxes
    )
  }
  guideColor <- if (options$covariate == "") {
    NULL
  } else {
    if (is.factor(dataset$covariate)) {
      ggplot2::guide_legend(override.aes = list(size = 6.5))
    } else {
      ggplot2::guide_colourbar()
    }
  }
  guide <- ggplot2::guides(fill = guideFill, color = guideColor)

  legendCloser <- ggplot2::theme(legend.box.spacing = ggplot2::unit(0, "pt"), legend.margin = ggplot2::margin(0, 0, 0, 0))

  plotInProgress <- plotInProgress + guide + legendCloser

  # Caption
  if (options$showCaption) {
    caption         <- .rainCaption(options, dataInfo, dataInfo$intervalBounds[[inputVariable]], warningAxisLimits, errorVioSides)
    addCaption      <- ggplot2::labs(caption = caption)
    captionPosition <- ggplot2::theme(plot.caption = ggtext::element_markdown(hjust = 0))  # Bottom left position
    plotInProgress  <- plotInProgress + addCaption + captionPosition
  }

  # Assign to inputPlot
  return(plotInProgress)
}  # End .rainFillPlot()



# .rainSetPalettes() ----
.rainSetPalettes <- function(dataset, options) {

  fillTitle <- if (options$secondaryFactor == "") options$primaryFactor else options$secondaryFactor
  paletteFill <- if (options$secondaryFactor != "" || options$colorAnyway) {
    jaspGraphs::scale_JASPfill_discrete(options$colorPalette, name = fillTitle)
  } else {
    NULL
  }

  paletteColor <- if (options$covariate != "") {
    if (is.factor(dataset$covariate)) {
      jaspGraphs::scale_JASPcolor_discrete(  options$covariatePalette, name = options$covariate)
    } else {
      jaspGraphs::scale_JASPcolor_continuous(options$covariatePalette, name = options$covariate)
    }
  } else {
    if (options$secondaryFactor != "" || options$colorAnyway) {
      jaspGraphs::scale_JASPcolor_discrete(  options$colorPalette,     name = options$secondaryFactor)
    } else {
      NULL
    }
  }

  return(list(fill = paletteFill, color = paletteColor))
}  # End .rainSetPalettes()



# .rainInfoFactorCombinations() ----
# Calculates info to determine colors & geom orientation
.rainInfoFactorCombinations <- function(inputDataset, inputPlot, extractColor = TRUE) {
  onlyFactors    <- inputDataset[c("primaryFactor", "secondaryFactor")]
  # Extract used Fill colors from plot
  # https://stackoverflow.com/questions/11774262/how-to-extract-the-fill-colours-from-a-ggplot-object

  if (extractColor) {  # So that we can re-use it in .rainFillTable()

    onlyFactors$color <- tryCatch(
      {
        ggplot2::ggplot_build(inputPlot)$data[[1]]["fill"]$fill  # Requires secondaryFactor or colorAnyway
      },
      error = function(e) {                                      # Thus error handling, but not used further
        return("black")
      },
      warning = function(w) {
        return("black")
      }
    )

  } else {
    onlyFactors$color <- "black"
  }

  # In the following possibleCombis <- expand.grid() secondaryFactor first because
  # then structure will match order in which ggplot accesses the clouds:
  # for each level of primaryFactor, we get the levels of secondaryFactor
  # (as opposed to: for each level of secondaryFactor, the levels of primaryFactor)
  possibleCombis <- expand.grid(
    secondaryFactor = levels(onlyFactors$secondaryFactor), primaryFactor = levels(onlyFactors$primaryFactor)
  )

  possibleCombis$rowId <- 1:nrow(possibleCombis)  # Id is necessary as merge will scramble order of possibleCombis
  observedCombis <- merge(possibleCombis, onlyFactors)
  observedCombis <- observedCombis[order(observedCombis$rowId), ] # Re-order rows according to id
  observedCombis <- observedCombis[ , c("primaryFactor", "secondaryFactor", "color", "rowId")]  # Re-order columns

  uniqueCombis   <- dplyr::count(observedCombis, pick(everything()))

  numberOfClouds <- nrow(uniqueCombis)

  return(
    list(
      numberOfClouds = numberOfClouds,
      colors         = uniqueCombis$color,
      uniqueCombis   = uniqueCombis
    )
  )
}  # End .rainInfoFactorCombinations()



# .rainSetVioSides() ----
# Either default all right orientation or as customSides from user in GUI
# Output is as long as there are rainclouds in the plot
# because each cloud needs own side specified in point.args.pos argument of ggrain:geom_rain()
# see also .rainNudgeForEachCloud()
.rainSetVioSides <- function(options, dataset, infoFactorCombinations) {

  defaultSides  <- rep("r", infoFactorCombinations$numberOfClouds)

  if (options$numberOfClouds == infoFactorCombinations$numberOfClouds && options$customSides) {
    outputSides <- tolower(options$customizationTable[[1]]$values)
    error <- FALSE
  } else {
    outputSides <- defaultSides
    error <- TRUE
  }

  return(list(sides = outputSides, error = error))
}  # End .rainSetVioSides()



# .rainNudgeForEachCloud() ----
# Depending on default/custom orientation
# For example, if there are two clouds and options$customSides is "LL"
# then vioNudge should be c(options$vioNudge * -1, options$vioNudge * -1)
# because options$vioNudge is inverted (* -1) as violins are flipped to left (instead of default right).
.rainNudgeForEachCloud <- function(inputNudge, vioSides) {
  nudgeVector <- rep(inputNudge, length(vioSides))
  for (i in 1:length(vioSides)) if (vioSides[i] == "l") nudgeVector[i] <- nudgeVector[i] * -1
  return(nudgeVector)
}  # End .rainNudgeForEachCloud()



# .rainGeomRain() ----
# Call of ggrain:geom_rain() with prior set up of all input arguments
.rainGeomRain <- function(dataset, options, infoFactorCombinations, vioSides, boxPosition, plotInProgress) {

  # Arguments for the cloud elements: Violin, Box, Point, observationId lines
  showVioGuide    <- if (options$secondaryFactor == "") TRUE else FALSE
  vioArgs         <- list(alpha = options$vioOpacity, adjust = options$vioSmoothing, lwd = options$vioOutlineWidth)
  vioOutlineColor <- .rainOutlineColor(options, options$vioOutline, infoFactorCombinations)
  perCloud512     <- rep(512, infoFactorCombinations$numberOfClouds)  # Each violin consists of 512 points by default
  vioArgs$color   <- rep(vioOutlineColor, perCloud512)

  boxArgs       <- list(outlier.shape = NA, alpha = options$boxOpacity, show.legend = FALSE, lwd = options$boxOutlineWidth)
  boxArgs$color <- .rainOutlineColor(options, options$boxOutline, infoFactorCombinations)

  showPointGuide <- if (options$covariate == "") FALSE else TRUE
  pointArgs      <- list(alpha = options$pointOpacity, show.legend = showPointGuide, size = options$pointSize)

  lineArgs <- list(alpha = options$observationIdLineOpacity, show.legend = FALSE, lwd = options$observationIdLineWidth)
  if (options$secondaryFactor   == "") lineArgs$color <- "black"

  # Violin positioning
  vioNudgeForEachCloud <- .rainNudgeForEachCloud(options$vioNudge, vioSides)  # Based on default/custom orientation
  vioPosVec <- c()
  for (i in vioNudgeForEachCloud) {
    vioPosVec <- c(vioPosVec, rep(i, 512))  # Each violin consists of 512 points by default
  }
  vioArgsPos <- list(
    width = options$vioHeight, position = ggplot2::position_nudge(x = vioPosVec), side = vioSides
  )

  # Box positioning
  boxArgsPos <- list(width = options$boxWidth, position = boxPosition)

  # Point positioning
  negativePointNudge <- if (!options$customSides) {
    options$pointNudge * -1  # This way all nudges in the GUI are positive by default
  } else {
    0                        # CustomSides fixes points to Axis ticks (see HelpButton in .qml)
  }
  jitter <- if (!options$jitter) 0 else NULL
  pointArgsPos   <- list(
    position = ggpp::position_jitternudge(
      nudge.from = "jittered",
      width      = options$pointSpread, # xJitter
      x          = negativePointNudge,  # Nudge
      height     = jitter,              # Jitter, particularly interesting for likert data
      seed       = 1                    # Reproducible jitter
    )
  )

  # Cov and id
  covArg <- if (options$covariate == "")                                 NULL else "covariate"  # Must be string
  idArg  <- if (options$observationId   == "" || options$primaryFactor == "")  NULL else "observationId"
            # primaryFactor condition necessary because if user input primaryFactor, then adds observationId input,
            # and then removes primaryFactor again, JASP/GUI/qml will not remove observationId input

  # Call geom_rain()
  output <- ggrain::geom_rain(

    violin.args = vioArgs, boxplot.args = boxArgs, point.args = pointArgs, line.args = lineArgs,

    rain.side        = NULL,  # Necessary for neat positioning
    violin.args.pos  = vioArgsPos,
    boxplot.args.pos = boxArgsPos,
    point.args.pos   = pointArgsPos,
    line.args.pos    = pointArgsPos,  # Dependent on points

    cov         = covArg,
    id.long.var = idArg,
    likert      = FALSE  # TRUE won´t work because of ggpp:position_jitternudge() in pointArgsPos
                         # instead use height argument in ggpp:position_jitternudge()
  )  # End geom_rain()

  return(output)
}  # End .rainGeomRain()



# .rainOutlineColor() ----
# Sets the Outline color of violins and boxes
.rainOutlineColor <- function(options, inputOutline, infoFactorCombinations) {

  output <- NULL

  if (inputOutline == "black") {
    output <- rep("black", infoFactorCombinations$numberOfClouds)
  } else if (inputOutline == "none") {
    output <- rep(NA, infoFactorCombinations$numberOfClouds)
  } else if (inputOutline == "colorPalette") {
    if (options$secondaryFactor != "" || options$colorAnyway) {
      output <- infoFactorCombinations$colors
    } else {
      output <- rep("black", infoFactorCombinations$numberOfClouds)
    }
  } else {
    print("error with Outline")
  }

  return(output)
}  # End .rainOutlineColor()



# .rainWhiskers() ----
# Creates the lower and upper whiskers for each boxplot
# The idea is to draw orthogonal lines as the end of the lines that originate from the box.
# Thus, we get the corresponding y-values from boxData and then draw a geom "errorbar" that
# starts and ends at that position, what remains is the orthogonal whisker.
# I am aware there is also the following approach:
# https://stackoverflow.com/questions/12993545/put-whisker-ends-on-boxplot
# HOWEVER, if boxOpacity != 1, then stat_boxplot(geom = "errorbar") will lead to a line that crosses through the boxbody
# and that looks ugly.
.rainWhiskers <- function(options, boxData, infoFactorCombinations, boxPosition) {

  lowerEnds <- boxData$ymin
  upperEnds <- boxData$ymax

  lowerWhiskers <- ggplot2::stat_summary(
    fun = median, geom = "errorbar", width = options$boxWidth, position = boxPosition,
    ggplot2::aes(ymin = ..y.. - (..y.. - lowerEnds), ymax = ..y.. - (..y.. - lowerEnds)),
    color = .rainOutlineColor(options, options$boxOutline, infoFactorCombinations),
    lwd = options$boxOutlineWidth, show.legend = FALSE
  )

  upperWhiskers <- ggplot2::stat_summary(
    fun = median, geom = "errorbar", width = options$boxWidth, position = boxPosition,
    ggplot2::aes(ymin = ..y.. + abs(..y.. - upperEnds), ymax = ..y.. + abs(..y.. - upperEnds)),
    color = .rainOutlineColor(options, options$boxOutline, infoFactorCombinations),
    lwd = options$boxOutlineWidth, show.legend = FALSE
  )

  return(list(lowerWhiskers = lowerWhiskers, upperWhiskers = upperWhiskers))
}  # End .rainWhiskers()



# .rainMeansAndLines() ----
.rainMeansAndLines <- function(options, boxPosVec, aesX, aesFill, infoFactorCombinations, meanPosition) {

  means <- if (options$mean) {
    ggplot2::stat_summary(
      fun = mean, geom = "point",
      mapping = ggplot2::aes(x = aesX, fill = aesFill),  # No color argument, covariate will interfere with it
      color = .rainOutlineColor(options, "colorPalette", infoFactorCombinations),  # Instead like Outlines
      shape = 18, size = options$meanSize, alpha = 1, show.legend = FALSE, position = meanPosition
    )
  } else {
    NULL
  }
  meanLinesGroupMapping <- if (options$secondaryFactor == "") 1 else aesFill
  meanLinesColor <- if (options$secondaryFactor == "") "black" else .rainOutlineColor(options, "colorPalette", infoFactorCombinations)
  meanLines <- if (options$mean && options$meanLines) {  # Needs options$mean as qml wont uncheck options$meanLines
    ggplot2::stat_summary(                                # if options$mean is unchecked again
      fun = mean, geom = "line", mapping = ggplot2::aes(group = meanLinesGroupMapping),
      color = meanLinesColor, alpha = options$meanLinesOpacity, position = meanPosition, lwd = options$meanLinesWidth
    )
  } else {
    NULL
  }

  return(list(means = means, meanLines = meanLines))
}  # End .rainMeansAndLines()



# .rainCaption() ----
# CSS formatting works through ggtext::element_markdown(); see .rainFillPlot()
.rainCaption <- function(options, dataInfo, intervalBounds, warningAxisLimits, errorVioSides) {

  exclusions <- if (dataInfo$numberOfExclusions > 0) {
    gettextf("Not shown are %s observations due to missing data.", dataInfo$numberOfExclusions)
  }

  if (options$meanIntervalCustom) {
    if (intervalBounds$successfulComputation) {
      meanInterval <- gettextf("Interval around mean is custom.")
    } else {
      meanInterval <- gettextf("<span style = 'color: darkorange'>Error with custom interval: Specified number of clouds does not match clouds in plot.</span>")
    }
  } else if (options$meanInterval) {
    if (options$meanIntervalOption == "sd") {
      meanInterval <- gettextf("Interval around mean represents ± 1 standard deviation.")
    } else if (options$observationId == "" && options$meanIntervalOption == "ci" && options$meanCiAssumption) {
      meanInterval <- paste0(
        "Interval around mean represents ",
        options$meanCiWidth * 100,
        "% confidence interval;<br>confidence intervals were computed independently for each group.")
    } else {
      meanInterval <- NULL
    }
  } else {
    meanInterval <- NULL
  }

  jitter <- if (options$jitter) gettextf("Points are slightly jittered from their true values.")

  warningAxisLimits <- if (warningAxisLimits) {
    gettextf("<span style = 'color: darkorange'> Warning! Some data is not shown<br>because it lies outside of the interval set by the custom axis limits.</span>")
  }

  errorVioSides <- if (errorVioSides && options$customSides) {
    gettextf("<span style = 'color: darkorange'> Error with custom orientation: Specified number of clouds does not match clouds in plot.<br>Reverted to default all 'R'. Point nudge set to 0.</span>")
  }

  output <- paste0(exclusions, "\n\n", meanInterval, "\n\n", jitter, "\n\n", warningAxisLimits, "\n\n", errorVioSides)
  return(output)
}  # End .rainCaption()



# .rainCreateTables ----
.rainCreateTables <- function(jaspResults, dataInfo, options, ready) {

  if (!options$table) return()

  # Create container in jaspResults
  if (is.null(jaspResults[["containerTables"]])) {

    jaspResults[["containerTables"]] <- createJaspContainer(title = gettext("Statistics"))
    jaspResults[["containerTables"]]$dependOn(
      c(
        "dependentVariables", "table", "tableBoxStatistics"
      )
    )
  }  # End create container

  # Access through container object
  container <- jaspResults[["containerTables"]]

  # Placeholder table, if no dependentVariables
  if (!ready) {
    placeholderTable <- createJaspTable(title = "", dependencies = c("dependentVariables"))
    placeholderTable <- .rainAddTableColumns(options, placeholderTable)
    container[["placeholderTable"]] <- placeholderTable
    return()
  }

  # Table for each dependentVariable
  for (variable in options$dependentVariables) {

    # If table for variable already exists, we can skip recreating table
    if (!is.null(container$variable)) next

    variableTable <- createJaspTable(title = variable)
    variableTable$dependOn(optionContainsValue = list(dependentVariables = variable))  # Depends on respective variable
    variableTable <- .rainAddTableColumns(options, variableTable)

    .rainFillTable(jaspResults, dataInfo, variable, options, variableTable)

    container[[variable]] <- variableTable

  }  # End of for loop

}  # End .rainCreateTables()



# .rainAddTableColumns() ----
.rainAddTableColumns <- function(options, inputTable) {

  tableInProgress <- inputTable

  # Add columns; names of boxplot statistics are like extracted from ggplot, see boxData and .rainFillTable
  tableInProgress$addColumnInfo(name = "primaryFactor", title = gettextf("Primary Factor"), type = "string")

  if (options$secondaryFactor != "") {
    tableInProgress$addColumnInfo(name = "secondaryFactor", title = gettextf("Secondary Factor"), type = "string")
  }

  tableInProgress$addColumnInfo(name = "n", title = gettextf("<i>N</i>"), type = "integer")

  if (options$tableBoxStatistics) {
    tableInProgress$addColumnInfo(name = "ymin",   title = gettextf("Lower Whisker"),   type = "number", format = "dp:2")
    tableInProgress$addColumnInfo(name = "lower",  title = gettextf("25th Percentile"), type = "number", format = "dp:2")
    tableInProgress$addColumnInfo(name = "middle", title = gettextf("Median"),          type = "number", format = "dp:2")
    tableInProgress$addColumnInfo(name = "upper",  title = gettextf("75th Percentile"), type = "number", format = "dp:2")
    tableInProgress$addColumnInfo(name = "ymax",   title = gettextf("Upper Whisker"),   type = "number", format = "dp:2")
  }

  meanInterval <- options$observationId == "" && options$meanInterval && options$meanIntervalOption == "ci" && options$meanCiAssumption

  if (options$mean && (meanInterval || options$meanIntervalCustom)) {
      tableInProgress$addColumnInfo(
        name = "lowerBound", title = gettextf("Lower Interval Limit"), type = "number", format = "dp:2"
      )
  }

  if (options$mean) {
    tableInProgress$addColumnInfo(name = "mean", title = gettextf("Mean"), type = "number", format = "dp:2")
  }

  if (options$mean && options$meanInterval && options$meanIntervalOption == "sd" && !options$meanIntervalCustom) {
      tableInProgress$addColumnInfo(name = "sd", title = gettextf("Standard Deviation"), type = "number", format = "dp:2")
  }

  if (options$mean && (meanInterval || options$meanIntervalCustom)) {
    tableInProgress$addColumnInfo(
      name = "upperBound", title = gettextf("Upper Interval Limit"), type = "number", format = "dp:2"
    )
  }

  tableInProgress$showSpecifiedColumnsOnly <- TRUE  # Only show columns that were added

  return(tableInProgress)
}  # End .rainAddTableColumns()



# .rainFillTable() ----
.rainFillTable <- function(jaspResults, dataInfo, inputVariable, options, inputTable) {

  # Extract statistics from corresponding plot
  plotContainer <- jaspResults[["containerRaincloudPlots"]]
  targetJaspPlot <- plotContainer[[inputVariable]]
  targetPlotObject <- targetJaspPlot[["plotObject"]]

  # Extract observed factor combinations and remove duplicate levels of primaryFactor for table
  combinations <- .rainInfoFactorCombinations(dataInfo$dataset, targetPlotObject, extractColor = FALSE)$uniqueCombis
  primaryLevels <- as.character(combinations$primaryFactor)
  previousLevel <- primaryLevels[1]
  tidyLevels <- c(previousLevel)
  if (length(primaryLevels) > 1) {
    for (i in 2:length(primaryLevels)) {
      if (primaryLevels[i] == previousLevel) {
        tidyLevels <- c(tidyLevels, "")
      } else {
        tidyLevels <- c(tidyLevels, primaryLevels[i])
        previousLevel <- primaryLevels[i]
      }
    }
  }
  combinations$primaryFactor <- tidyLevels

  boxDataIndex <- if (options$observationId == "") 2 else 3
  boxData <- ggplot2::ggplot_build(targetPlotObject)$data[[boxDataIndex]]

  tableStatistics <- cbind(combinations, boxData)

  if (options$mean) {
    meanDataIndex <- if (options$observationId == "") 6 else 7
    tableStatistics$mean <- ggplot2::ggplot_build(targetPlotObject)$data[[meanDataIndex]]$y_orig
  }
  if (options$meanInterval || options$meanIntervalCustom) {
    tableStatistics$lowerBound <- dataInfo$intervalBounds[[inputVariable]]$lowerBound
    tableStatistics$sd         <- dataInfo$intervalBounds[[inputVariable]]$sd
    tableStatistics$upperBound <- dataInfo$intervalBounds[[inputVariable]]$upperBound
  }

  # Add statistics to table
  inputTable$setData(tableStatistics)

  # Footnote
  sampleSize <- gettextf("<i>N</i><sub>Total</sub> = %s.", dataInfo$sampleSize)
  exclusions <- if (dataInfo$numberOfExclusions > 0) {
    gettextf(" This excludes %s observations due to missing data.", dataInfo$numberOfExclusions)
  } else {
    NULL
  }
  meanInterval <- if (options$meanIntervalCustom) {
    gettextf(" Interval around mean is custom.")
  } else if (options$observationId == "" && options$meanInterval && options$meanIntervalOption == "ci" && options$meanCiAssumption) {
    paste0(
      " Interval around mean represents ",
      options$meanCiWidth * 100,
      "% confidence interval; confidence intervals were computed independently for each group.")
  } else {
    NULL
  }

  footnote <- paste0(sampleSize, exclusions, meanInterval)
  inputTable$addFootnote(footnote)
}  # End .rainFillTable()
