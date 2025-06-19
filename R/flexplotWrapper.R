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

#' Flexplot
#'
#' Flexplot allows the user to create graphical displays of data, using barcharts and histograms for univariate data, and various different types of scatterplots for bivariate and multivariate data.
#'
#' @param alpha, The degree of transparency of the dots in the graphics
#' @param confidence, Should 95% confidence intervals be displayed?
#'    Defaults to \code{FALSE}.
#' @param dependent, The variable of interest. This is also called the outcome variable. If only an outcome variable is specified, Flexplot will produce a histogram for numeric data and a barchart for categorical data. If independent variable(s) and/or panelled variable(s) are specified, this variable will be displayed on the Y axis.
#' @param ghost, Ghost lines are a visual aid that can be used when doing panelled plots. Ghost lines simply repeat the fitted line from one panel across the other panels to make it easier to make comparisons across panels.
#'    Defaults to \code{TRUE}.
#' @param jitx, The maximal amount of "jittering" used on the X axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap.
#' @param jity, The maximal amount of "jittering" used on the Y axis to remove overlap between datapoints. The maximal amount of jittering is proportional to the density of the data. In other words, the maximum jittering will occur at the mode of the dataset, and little to no jittering will occur in locations where there is no overlap.
#' @param paneledVars, Variables specified in these boxes will be binned (if numeric) then displayed as different subplots. The first variable specified as a panelled variable will form the column plots, while the second will form the row plots.
#' @param theme, the type of GGplot theme to use when displaying the data. Can be one of the following:
#' @param type, The type of fitted line displayed when the x axis is a numeric variable. These can be one of the following:
#' \itemize{
#'   \item \code{"Loess"} (default) : A non-parametric loess line
#'   \item \code{"Regression"}: A straight (regression) line
#'   \item \code{"Quadratic"}: A line that includes both a linear effect and a quadratic (squared) term
#'   \item \code{"Cubic"}: A line that includes a linear, squared, and cubed term
#'   \item \code{"None"}: No line
#' }
#' @param variables, The variable(s) for which we wish to visually assess relationships with the DV. The first variable chosen shows up on the X axis, either as a scatterplot (for numeric predictors) or as a beeswarm plot (for categorical variables). The second variable chosen will show up as different colors/lines/symbols. If the second varaible chosen is numeric, it will be binned first.
flexplot <- function(
          data = NULL,
          version = "0.95",
          formula = NULL,
          alpha = 0.4,
          bw = FALSE,
          confidence = FALSE,
          dependent = list(types = list(), value = ""),
          ghost = TRUE,
          intervals = "Quartiles",
          jitx = 0.1,
          jity = 0,
          nameCols = "",
          nameLegend = "",
          nameRows = "",
          nameX = "",
          nameY = "",
          palette = "GGplot Default",
          paneledVars = list(types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          theme = "JASP",
          type = "Loess",
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspDescriptives::flexplot)
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
   optionsWithFormula <- c("dependent", "intervals", "palette", "paneledVars", "theme", "type", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDescriptives", "flexplot", "Flexplot.qml", options, version, FALSE))
}
