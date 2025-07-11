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

#' Raincloud Plots
#'
#' This module has a <a href='https://www.youtube.com/watch?v=AAdXUAl_w6E'> youtube tutorial</a> <br><br>If you use this module, please cite it as:<br>Ott, V. L, van den Bergh, D., Boutin, B., van Doorn, J., Bartoš, F., Judd, N., Luke, K., Kievit, R. A., Groot, L., and Wagenmakers, E.-J. (2024). Informative Data Visualization with Raincloud Plots in JASP. OSF Preprint. <a href="https://doi.org/10.31219/osf.io/gv3ph">https://doi.org/10.31219/osf.io/gv3ph</a>
#'
#' @param colorAnyway, Applies the color palette to the levels of the primary factor. Otherwise, the plot stays black and white.<br>This option is superseeded by a secondary factor; then color coding is according to that.
#'    Defaults to \code{TRUE}.
#' @param colorPalette, How to color code the levels of the secondary factor.
#' @param covariate, Points are color coded according to this.
#' @param covariatePalette, How to color code the covariate. 'Viridis' works good for both discrete and continuous covariates.
#' @param customAxisLimits, Use this with care!<br>If your dataset has observations that lie outside of the specified, the plot will be truncated and it will not show these observations.<br>The limits you specify may only be applied approximately. For further fine-tuning of the axis, click the title of the plot where it says the name of dependent variable.<br>Then select 'Edit Image' in the drop down menu and then go to the headers 'x-axis' or 'y-axis'.
#'    Defaults to \code{FALSE}.
#' @param dependentVariables, Select all the variables that you want to plot.
#' @param horizontal, Plots the dependent variable axis horizontally, at the bottom of the plot.<br>This is where the raincloud plot gets its name from: It will look like the points are raining from the violin and box (cloud).
#'    Defaults to \code{FALSE}.
#' @param mean, Whether to also show a mean for each cloud.<h4>Position</h4>Use a 'Custom' position to have the mean placed inside of the box - even if you do not show the box.<br>In fact, the mean nudge and distance fields are synchronized with the box nudge and box fields, respectively.<br>Use the 'On axis ticks' position to have all means for each primary factor level on top of each other.<br>This can be useful if you want to show the change of secondary factor levels (e.g. groups) across the primary factor levels (e.g. time).<h4>Size</h4>Try different values to make your plot especially pretty.<h4>Mean Lines: Width</h4>See outline width of box and violin as well as ID lines width.
#'    Defaults to \code{FALSE}.
#' @param meanInterval, Whether to also show an interval around the mean.<br>This hides and disables the box.<br><br>You can choose between a confidence interval, ±1 standard error, or ±1  standard deviation.<br>For the computation of the confidence interval/se/sd, within and between factors are determined based on the specifciation of the ID variable<br><h4>
#'    Defaults to \code{FALSE}.
#' @param numberOfClouds, To apply custom cloud orientation or custom mean intervals, you first must specify the number of clouds that are currently shown in the plot.<br>If the number you entered does not match the number of clouds in the plot, the plot caption will show a warning and you can correct your entry.<br>The number you specify determines the number of rows in the table.<br><br>In the table, you can then specify the custom orientation ('L' vs. 'R') for each cloud as well as custom lower and upper interval limits for each mean.<br>Make sure to also check the corresponding checkboxes 'Apply custom orientation' or 'Apply custom mean interval limits'.<br>Otherwise, your input into the table will not be applied.<br><br>The order in which the rows of the table are mapped to the clouds in the plot is as follows:<br>Suppose, there are three times of measurement (primary factor): pre, post, and follow-up.<br>Further, there are two species (secondary factor): alpha and beta.<br>This means that there are 6 clouds, in the following order:<br>pre-alpha<br>pre-beta<br>post-alpha<br>post-beta<br>follow-up-alpha<br>follow-up-beta<br><br>In this example you could now specify the odd rows as 'L' and the even rows as 'R'.<br>That way, at each time point, the left cloud would be alpha and the right cloud would be beta.<br>If you have a 2x2 design (e.g. no follow-up in as primary factor), then a useful custom orientation could be LLRR.<br>That way, both the alpha and the beta cloud at time point pre would be left and at time point they would be right.<br>Combine this with ID input to connect the individual observations over time.<br>They will then run between the two clouds on the left to the two clouds on the right.<br><br>Any custom orientation or mean intervals will be applied to the plot of every dependent variable.<br>How can you specify different custom mean intervals for two dependent variables?<br>For this, you can duplicate the analysis (see symbol: white plus on green background),<br>only select one dependent variable in each version, and specify the custom intervals separately.<br><br>For custom cloud colors, just enter the hexcode you want. This features only works when there is no Secondary Factor.
#' @param observationId, Select a participant/observation ID in your dataset, to connect individual observations (points) accross the levels of the primary factor. Otherwise, you do not need this.
#' @param primaryFactor, Its levels are shown on the x-axis (y-axis for horizontal plot).
#' @param secondaryFactor, Its levels are color coded.
#' @param showBox, Whether or not the box should be shown. If un-checked, opacity is set to 0 and outline to 'none'.<br>An interval around the mean disables the box altogether.
#'    Defaults to \code{TRUE}.
#' @param showCaption, The caption provides important information about the plot such as warnings, if there are any.<br>We strongly recommend to leave this checkbox checked and to only un-check it if you are otherwise happy with the plot and want to export it.<h3>Plot Size: Width and Height</h3>This also includes the legend (if there is color coding) and the caption.<br>While you can also change the size of a JASP plot with your mouse, the precise specification of width and height allows you to exactly reproduce a plot.
#'    Defaults to \code{TRUE}.
#' @param showLegend, Shows the legend for the color-coding of the primary factor.<br>This option is superseeded by a secondary factor; then the legend is always added.
#'    Defaults to \code{FALSE}.
#' @param showPoint, Whether or not the points of a cloud should be shown. If un-checked, opacity is set to 0.<br>If you have many, many points, it can be helpful to hide them.<br><br><h3>Element Settings</h3><h4>Nudge</h4>The nudge for violin, box, and point determines how far the elements are nudged from the center (axis tick).<br>By default, the box is in the center (nudge = 0) and the violin is nudged to the right. The points are nudged to the left of the box.<br>With a custom orientation (see Advanced section), the points get centered (nudge fixed to 0). Violin and box are nudged depending on orientation: left or right.<h4>Height, Width, and Spread</h4>These settings determine the respective properties of the elements:<br><br>How high should the violin peak? (Does not change the proportions of the probability density.)<br>With a custom orientation to the left (see Advanced Section), it can be helpful to decrease the height if the violin overlaps with axis ticks.<br><br>How wide should the box be?<br><br>How wide should the points spread? I.e., how jittered should they be along the x-axis (y-axis if horizontal plot?.<br>Note, that this still correctly shows the values of the points on the dependent variable axis.<br>(If you would also like y-jitter see the 'Jitter' option for points.)<h4>Box Padding</h4>With a secondary factor, there will be a box for each factor level. With the padding, you can change the spacing between theses boxes.<br>Increasing the padding will - visually - decrease the box width. If you want to keep box width constant, also increase the box width.<h4>Point Size</h4>We recommend a greater size, the fewer points you have.<h4>Opacity</h4>Increase this to _decrease_ the transparency of the respective element.<h4>Outline and Outline Width</h4>Would you like the outline to match the 'Color palette', be 'black', or have 'none' at all?<br>The width determines how thick the outline is.<h4>Violin Smoothing</h4>This percentage determines the smoothness of the probability density.<br>The lower, the stronger it is influenced by the presence/absence of individual points.<h4>Point Jitter</h4>Use this with care!<br>It slightly jitters the points along the dependent variable axis. This means that the position of the points no longer matches the values of the observations.<br>If you want to have less overlap between the points while keeping an accurate visualization, increases the point spread.<br>Using point jitter can be useful in some cases. For example, if you are working with likert data.<h4>ID Lines Width</h4>Determines how thick the lines are.
#'    Defaults to \code{TRUE}.
#' @param showVio, Whether or not the violin should be shown. If un-checked, opacity is set to 0 and outline to 'none'.
#'    Defaults to \code{TRUE}.
#' @param table, Shows a table under the plot with statistics per cloud, like number of observations or median.<br>If you select 'Mean' or 'Interval around mean' (see Advanced Section), they will also be shown here.
#'    Defaults to \code{FALSE}.
#' @param tableBoxStatistics, Shows the box statistics in the table: lower whisker, 25th percentile, median, 75th percentile, upper whisker.<br>It can be helpful to un-check this if you are also working with 'Interval around mean' (see Advanced Section).
#'    Defaults to \code{TRUE}.
raincloudPlots <- function(
          data = NULL,
          version = "0.95",
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
          customizationTable = list(list(levels = list("Cat", "Training", "Dance"), name = "data 1", values = list("R", "R", "R")), list(levels = list("Cat", "Training", "Dance"), name = "data 2", values = list(0, 0, 0)), list(levels = list("Cat", "Training", "Dance"), name = "data 3", values = list(0, 0, 0)), list(levels = list("Cat", "Training", "Dance"), name = "data 4", values = list("#00A9E6", "#00A9E6", "#00A9E6"))),
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