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



context("Raincloud Plots")



# 1: Primary factor without color; with table ----
test_that("plot with primary factor and no color matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$colorAnyway <- FALSE # the form colors the primary factor by default when there is no secondary factor
  options$boxNudge <- 0
  options$boxOpacity <- 0.5
  options$boxOutlineWidth <- 1.5
  options$boxPadding <- 0.1
  options$boxWidth <- 0.1
  options$colorPalette <- "colorblind"
  options$covariatePalette <- "viridis"
  options$dependentVariables <- "bill_length_mm"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$meanLines <- FALSE
  options$pointNudge <- 0.19
  options$pointOpacity <- 0.5
  options$pointSize <- 3
  options$pointSpread <- 0.1
  options$primaryFactor <- "island"
  options$primaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$table <- TRUE
  options$vioNudge <- 0.09
  options$vioOpacity <- 0.5
  options$vioOutlineWidth <- 1.5
  options$widthPlot <- 675
  results <- jaspTools::runAnalysis("raincloudPlots", testthat::test_path("palmerPenguins.csv"), options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_bill_length_mm"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "primary-colorless")
})

test_that("table results match for a plot with primary factor", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$boxNudge <- 0
  options$boxOpacity <- 0.5
  options$boxOutlineWidth <- 1.5
  options$boxPadding <- 0.1
  options$boxWidth <- 0.1
  options$colorPalette <- "colorblind"
  options$covariatePalette <- "viridis"
  options$dependentVariables <- "bill_length_mm"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$meanLines <- FALSE
  options$pointNudge <- 0.19
  options$pointOpacity <- 0.5
  options$pointSize <- 3
  options$pointSpread <- 0.1
  options$primaryFactor <- "island"
  options$primaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$table <- TRUE
  options$vioNudge <- 0.09
  options$vioOpacity <- 0.5
  options$vioOutlineWidth <- 1.5
  options$widthPlot <- 675
  results <- jaspTools::runAnalysis("raincloudPlots", "palmerPenguins.csv", options)
  table <- results[["results"]][["containerTables"]][["collection"]][["containerTables_bill_length_mm"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(42, 45.8, 167, "Biscoe", 48.7, 55.9, 34.5, 39.15, 44.65, 124,
                                      "Dream", 49.85, 58, 32.1, 36.65, 38.9, 51, "Torgersen", 41.1,
                                      46, 33.5))
})



# 2: Both factors; horizontal; custom axis limits ----
test_that("horizontal plot with both factors and custom axis limits matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0
  options$boxOpacity <- 0.5
  options$boxPadding <- 0.25
  options$boxWidth <- 0.25
  options$colorPalette <- "colorblind"
  options$covariatePalette <- "viridis"
  options$customAxisLimits <- TRUE
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "body_mass_g"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$horizontal <- TRUE
  options$meanLines <- FALSE
  options$pointNudge <- 0.275
  options$pointOpacity <- 0.5
  options$pointSpread <- 0.1
  options$primaryFactor <- "species"
  options$primaryFactor.types <- "nominal"
  options$secondaryFactor <- "sex"
  options$secondaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$upperAxisLimit <- 6500
  options$vioNudge <- 0.15
  options$vioOpacity <- 0.5
  options$widthPlot <- 675
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", "palmerPenguins.csv", options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_body_mass_g"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "horizontal-both-factors-custom-axis-limits")
})



# 3: Both factors; means; meanLines; confidence interval; table ----
test_that("plot with both factors, no box, means, meanLines, and, bootstrapped ci matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0
  options$boxOutline <- "none"
  options$boxPadding <- 0.1
  options$boxWidth <- 0.275
  options$colorPalette <- "ggplot2"
  options$covariatePalette <- "viridis"
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "bill_length_mm"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$mean <- TRUE
  options$meanCiAssumption <- TRUE
  options$meanCiBootstrapSamples <- 1001
  options$meanCiMethod <- "bootstrap"
  options$meanCiWidth <- 0.99
  options$meanInterval <- TRUE
  options$meanIntervalOption <- "ci"
  options$meanSize <- 4.5
  options$pointNudge <- 0.25
  options$pointOpacity <- 0.25
  options$primaryFactor <- "species"
  options$primaryFactor.types <- "nominal"
  options$secondaryFactor <- "island"
  options$secondaryFactor.types <- "nominal"
  options$seed <- 42
  options$setSeed <- TRUE
  options$showBox <- TRUE
  options$table <- TRUE
  options$tableBoxStatistics <- FALSE
  options$vioNudge <- 0.2
  options$vioOpacity <- 0.5
  options$widthPlot <- 675
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", "palmerPenguins.csv", options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_bill_length_mm"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "both-factors-boxless-means-meanlines-bootstrapped-ci")
})

test_that("table results match for a plot with both factors, no box, means, meanLines, and ci", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0
  options$boxOutline <- "none"
  options$boxPadding <- 0.1
  options$boxWidth <- 0.275
  options$colorPalette <- "ggplot2"
  options$covariatePalette <- "viridis"
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "bill_length_mm"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$mean <- TRUE
  options$meanCiWidth <- 0.99
  options$meanInterval <- TRUE
  options$meanIntervalOption <- "ci"
  options$meanSize <- 4.5
  options$pointNudge <- 0.25
  options$pointOpacity <- 0.25
  options$primaryFactor <- "species"
  options$primaryFactor.types <- "nominal"
  options$secondaryFactor <- "island"
  options$secondaryFactor.types <- "nominal"
  options$seed <- 42
  options$setSeed <- TRUE
  options$showBox <- TRUE
  options$table <- TRUE
  options$tableBoxStatistics <- FALSE
  options$vioNudge <- 0.2
  options$vioOpacity <- 0.5
  options$widthPlot <- 675
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", "palmerPenguins.csv", options)
  table <- results[["results"]][["containerTables"]][["collection"]][["containerTables_bill_length_mm"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(37.9669992428727, 38.975, 44, "Adelie", "Biscoe", 39.9830007571273,
                                      37.6227486231599, 38.5017857142857, 56, "", "Dream", 39.3808228054115,
                                      37.8165888244058, 38.9509803921569, 51, "", "Torgersen", 40.0853719599079,
                                      47.760227341363, 48.8338235294118, 68, "Chinstrap", "Dream",
                                      49.9074197174605, 46.7777368375394, 47.5048780487805, 123, "Gentoo",
                                      "Biscoe", 48.2320192600216))
})



# 4: Primary factor; continuous covariate ----
test_that("plot with primary factor and continuous covariate matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0
  options$boxOpacity <- 0.75
  options$boxOutline <- "black"
  options$boxPadding <- 0.1
  options$boxWidth <- 0.1
  options$colorAnyway <- TRUE
  options$colorPalette <- "grandBudapest"
  options$covariate <- "bill_depth_mm"
  options$covariate.types <- "scale"
  options$covariatePalette <- "viridis"
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "flipper_length_mm"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$meanLines <- FALSE
  options$pointNudge <- 0.175
  options$pointOpacity <- 0.66
  options$pointSize <- 1.5
  options$primaryFactor <- "species"
  options$primaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$vioNudge <- 0.09
  options$vioOpacity <- 0.75
  options$vioOutline <- "black"
  options$widthPlot <- 675
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", testthat::test_path("palmerPenguins.csv"), options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_flipper_length_mm"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "primary-covariate-continuous")
})



# 5: Secondary factor; discrete covariate; means ----
test_that("plot with secondary factor, discrete covariate, and means matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0
  options$boxOpacity <- 0.5
  options$boxPadding <- 0.3
  options$boxWidth <- 0.25
  options$colorPalette <- "colorblind"
  options$covariate <- "sex"
  options$covariate.types <- "nominal"
  options$covariatePalette <- "colorblind3"
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "body_mass_g"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$mean <- TRUE
  options$meanLines <- FALSE
  options$pointNudge <- 0.3
  options$pointOpacity <- 0.5
  options$pointSpread <- 0.15
  options$secondaryFactor <- "species"
  options$secondaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$vioHeight <- 0.6
  options$vioNudge <- 0.15
  options$vioOpacity <- 0.5
  options$widthPlot <- 675
  set.seed(1)
  df <- read.csv(testthat::test_path("palmerPenguins.csv"))
  df$sex <- as.factor(df$sex)
  results <- jaspTools::runAnalysis("raincloudPlots", df, options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_body_mass_g"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "secondary-covariate-discrete-mean")
})



# 6: ID over time ----
test_that("plot for ID over time matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0.18
  options$boxOpacity <- 0.5
  options$boxPadding <- 0.3
  options$colorPalette <- "sportsTeamsNBA"
  options$covariatePalette <- "viridis"
  options$customSides <- TRUE
  options$customizationTable <- list(
    list(levels = "", name = "", values = c("L","L", "L", "R", "R", "R")),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "sepalWidth"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 500
  options$jitter <- TRUE
  options$meanLines <- FALSE
  options$numberOfClouds <- 6
  options$observationId <- "id"
  options$observationId.types <- "scale"
  options$observationIdLineOpacity <- 0.1
  options$observationIdLineWidth <- 0.75
  options$pointNudge <- 0.15
  options$pointOpacity <- 0.5
  options$primaryFactor <- "time"
  options$primaryFactor.types <- "nominal"
  options$secondaryFactor <- "Species"
  options$secondaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$showCaption <- FALSE
  options$vioHeight <- 0.4
  options$vioNudge <- 0.3
  options$vioOpacity <- 0.5
  options$widthPlot <- 750
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", testthat::test_path("irisFertilizer.csv"), options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_sepalWidth"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "id-over-time")
})



# 7: Custom mean intervals ----
test_that("plot with custom mean interval matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0
  options$boxOutline <- "none"
  options$boxOutlineWidth <- 1.5
  options$boxPadding <- 0.1
  options$boxWidth <- 0.1
  options$colorPalette <- "colorblind"
  options$covariatePalette <- "viridis"
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = c(2.6, 2.9, 3, 2.53, 3.33, 2.44)),
    list(levels = "", name = "", values = c(2.85, 3.07, 3.45, 2.91, 3.86, 2.66)),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "sepalWidth"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$mean <- TRUE
  options$meanIntervalCustom <- TRUE
  options$meanLinesOpacity <- 0.25
  options$meanLinesWidth <- 2
  options$meanPosition <- "onAxisTicks"
  options$meanSize <- 7.5
  options$numberOfClouds <- 6
  options$pointOpacity <- 0.25
  options$primaryFactor <- "time"
  options$primaryFactor.types <- "nominal"
  options$secondaryFactor <- "Species"
  options$secondaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$vioNudge <- 0.09
  options$vioOutline <- "none"
  options$widthPlot <- 675
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", "irisFertilizer.csv", options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_sepalWidth"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "custom-mean-interval")
})



# 8: Flanking clouds and vioSmoothing ----
test_that("plot with flanking clouds and vioSmoothing matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxOpacity <- 0.25
  options$boxPadding <- 0.1
  options$boxWidth <- 0.1
  options$colorPalette <- "colorblind"
  options$covariatePalette <- "viridis"
  options$customSides <- TRUE
  options$customizationTable <- list(
    list(levels = "", name = "", values = c("L","L", "R", "R")),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#00A9E6"))
  )
  options$dependentVariables <- "sepalWidth"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$jitter <- TRUE
  options$meanLines <- FALSE
  options$numberOfClouds <- 4
  options$observationId <- "id"
  options$observationId.types <- "scale"
  options$observationIdLineOpacity <- 0.15
  options$pointNudge <- 0.15
  options$pointOpacity <- 0.75
  options$primaryFactor <- "time"
  options$primaryFactor.types <- "nominal"
  options$secondaryFactor <- "Species"
  options$secondaryFactor.types <- "nominal"
  options$showBox <- TRUE
  options$vioHeight <- 0.6
  options$vioNudge <- 0.225
  options$vioOpacity <- 0.33
  options$vioSmoothing <- 0.33
  options$widthPlot <- 675
  set.seed(1)
  df <- read.csv("irisFertilizer.csv")
  df <- subset(df, time != "t2")  # Remove for 2x2 flanking design
  results <- jaspTools::runAnalysis("raincloudPlots", df, options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_sepalWidth"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "flanking-clouds-viosmoothing")
})



# 9: Custom Color ----
test_that("plot with custom cloud color matches", {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$.meta <- list(covariate = list(shouldEncode = TRUE), customizationTable = list(
    shouldEncode = TRUE), dependentVariables = list(shouldEncode = TRUE),
    observationId = list(shouldEncode = TRUE), primaryFactor = list(
      shouldEncode = TRUE), secondaryFactor = list(shouldEncode = TRUE))
  options$boxNudge <- 0
  options$boxOutline <- "none"
  options$boxPadding <- 0.1
  options$boxWidth <- 0.275
  options$colorPalette <- "ggplot2"
  options$covariatePalette <- "viridis"
  options$colorAnyway <- TRUE
  options$customColors <- TRUE
  options$numberOfClouds <- 3
  # one row per cloud in every column, as the form's table has it (it rejects a table of another shape)
  clouds <- c("Row 0", "Row 1", "Row 2")
  options$customizationTable <- list(
    list(levels = clouds, name = "data 1", values = c("R", "R", "R")),
    list(levels = clouds, name = "data 2", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 3", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 4", values = c("#8B93FF", "#5755FE", "#FF71CD"))
  )
  options$dependentVariables <- "bill_length_mm"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$mean <- TRUE
  options$meanCiAssumption <- FALSE
  options$meanCiWidth <- 0.99
  options$meanInterval <- TRUE
  options$meanIntervalOption <- "ci"
  options$meanSize <- 4.5
  options$pointNudge <- 0.25
  options$pointOpacity <- 0.25
  options$primaryFactor <- "island"
  options$primaryFactor.types <- "nominal"
  options$seed <- 42
  options$setSeed <- TRUE
  options$showBox <- TRUE
  options$table <- TRUE
  options$tableBoxStatistics <- FALSE
  options$vioNudge <- 0.2
  options$vioOpacity <- 0.5
  options$widthPlot <- 675
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", "palmerPenguins.csv", options)
  plotName <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_bill_length_mm"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "custom-cloud-color")
})



# 10: Clouds with one observation ----
rainOneObservationOptions <- function() {
  options <- jaspTools::analysisOptions("raincloudPlots")
  options$colorAnyway <- TRUE # the form's default without a secondary factor
  options$colorPalette <- "colorblind"
  options$covariatePalette <- "viridis"
  options$dependentVariables <- "y"
  options$dependentVariables.types <- "scale"
  options$heightPlot <- 550
  options$vioNudge <- 0.3
  options$widthPlot <- 675
  options
}

rainOneObservationPlot <- function(results) {
  plotResult <- results[["results"]][["containerRaincloudPlots"]][["collection"]][["containerRaincloudPlots_y"]]
  testthat::expect_identical(plotResult[["status"]], "complete")
  results[["state"]][["figures"]][[plotResult[["data"]]]][["obj"]]
}

test_that("plot is drawn when every level of a discrete covariate has one observation", {
  # jasp-issues#4435: no factor, so one cloud, split by the covariate into seven groups of one observation
  options <- rainOneObservationOptions()
  options$covariate <- "k"
  options$covariate.types <- "nominal"
  dataset <- data.frame(y = c(12, 18, 21, 27, 16, 24, 14), k = factor(c("A", "B", "C", "D", "E", "F", "G")))
  results <- jaspTools::runAnalysis("raincloudPlots", dataset, options)
  testPlot <- rainOneObservationPlot(results)
  # the violin outline takes the fill colour, so the violin is drawn for the whole cloud, 0.3 right of x = 1
  violins <- ggplot2::layer_data(testPlot, 1)
  expect_equal(unique(violins$group), 1)
  expect_equal(unique(violins$x), 1.3)
  expect_equal(nrow(ggplot2::layer_data(testPlot, 3)), 7)
})

test_that("violins keep the nudge of their cloud when the middle cloud has one observation", {
  options <- rainOneObservationOptions()
  options$primaryFactor <- "g"
  options$primaryFactor.types <- "nominal"
  options$customSides <- TRUE
  options$numberOfClouds <- 3
  clouds <- c("Row 0", "Row 1", "Row 2")
  options$customizationTable <- list(
    list(levels = clouds, name = "data 1", values = c("L", "R", "L")),
    list(levels = clouds, name = "data 2", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 3", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 4", values = c("#00A9E6", "#00A9E6", "#00A9E6"))
  )
  dataset <- data.frame(y = c(4.1, 5.3, 6.2, 5.0, 4.8, 7.4, 6.6, 5.9, 3.7),
                        g = factor(c("a", "a", "a", "a", "b", "c", "c", "c", "c")))
  results <- jaspTools::runAnalysis("raincloudPlots", dataset, options)
  violins <- ggplot2::layer_data(rainOneObservationPlot(results), 1)
  # cloud b has no violin; clouds a and c are "L", so their violins sit 0.3 left of x = 1 and x = 3
  expect_equal(sort(unique(violins$group)), c(1, 3))
  expect_equal(unique(violins$x[violins$group == 1]), 0.7)
  expect_equal(unique(violins$x[violins$group == 3]), 2.7)
})

test_that("plot is drawn when a secondary factor leaves a cloud with one observation", {
  # jasp-issues#3123
  options <- rainOneObservationOptions()
  options$primaryFactor <- "g"
  options$primaryFactor.types <- "nominal"
  options$secondaryFactor <- "h"
  options$secondaryFactor.types <- "nominal"
  dataset <- data.frame(y = c(4.1, 5.3, 6.2, 5.0, 4.8, 7.4, 6.6, 5.9, 3.7, 5.5),
                        g = factor(rep(c("a", "b"), each = 5)),
                        h = factor(c("u", "u", "u", "u", "v", "u", "u", "v", "v", "v")))
  results <- jaspTools::runAnalysis("raincloudPlots", dataset, options)
  violins <- ggplot2::layer_data(rainOneObservationPlot(results), 1)
  # clouds a-u, a-v, b-u, b-v; a-v has one observation, so no violin; the others sit 0.3 right of their x
  expect_equal(sort(unique(violins$group)), c(1, 3, 4))
  expect_equal(unique(violins$x[violins$group == 1]), 1.3)
  expect_equal(unique(violins$x[violins$group %in% c(3, 4)]), 2.3)
})

test_that("plot is drawn when the first covariate level of a cloud has one observation", {
  options <- rainOneObservationOptions()
  options$colorAnyway <- FALSE # without the palette on the primary factor, the covariate splits the violins
  options$covariate <- "k"
  options$covariate.types <- "nominal"
  dataset <- data.frame(y = c(10, 20, 22, 25, 14), k = factor(c("A", "B", "B", "B", "B")))
  results <- jaspTools::runAnalysis("raincloudPlots", dataset, options)
  violins <- ggplot2::layer_data(rainOneObservationPlot(results), 1)
  # only level B (group 2) has a violin, 0.3 right of the single cloud at x = 1
  expect_equal(unique(violins$group), 2)
  expect_equal(unique(violins$x), 1.3)
})

test_that("violins keep their cloud's side with a covariate when the middle cloud has one observation", {
  options <- rainOneObservationOptions()
  options$primaryFactor <- "g"
  options$primaryFactor.types <- "nominal"
  options$covariate <- "k"
  options$covariate.types <- "nominal"
  options$customSides <- TRUE
  options$numberOfClouds <- 3
  clouds <- c("Row 0", "Row 1", "Row 2")
  options$customizationTable <- list(
    list(levels = clouds, name = "data 1", values = c("L", "R", "L")),
    list(levels = clouds, name = "data 2", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 3", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 4", values = c("#00A9E6", "#00A9E6", "#00A9E6"))
  )
  dataset <- data.frame(y = c(4.1, 5.3, 6.2, 5.0, 4.8, 7.4, 6.6, 5.9, 3.7),
                        g = factor(c("a", "a", "a", "a", "b", "c", "c", "c", "c")),
                        k = factor(c("p", "q", "p", "q", "p", "p", "q", "p", "q")))
  results <- jaspTools::runAnalysis("raincloudPlots", dataset, options)
  violins <- ggplot2::layer_data(rainOneObservationPlot(results), 1)
  # one violin per cloud (the outline takes the fill colour); b has none; a and c are "L": 0.3 left of x = 1 and x = 3
  expect_equal(sort(unique(violins$group)), c(1, 3))
  expect_equal(unique(violins$x[violins$group == 1]), 0.7)
  expect_equal(unique(violins$x[violins$group == 3]), 2.7)
})

test_that("violins keep their cloud's side with a covariate and a black violin outline", {
  options <- rainOneObservationOptions()
  options$colorAnyway <- FALSE
  options$vioOutline <- "black" # a fixed outline colour, so the covariate does not split the violins
  options$primaryFactor <- "g"
  options$primaryFactor.types <- "nominal"
  options$covariate <- "k"
  options$covariate.types <- "nominal"
  options$customSides <- TRUE
  options$numberOfClouds <- 3
  clouds <- c("Row 0", "Row 1", "Row 2")
  options$customizationTable <- list(
    list(levels = clouds, name = "data 1", values = c("R", "L", "R")),
    list(levels = clouds, name = "data 2", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 3", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 4", values = c("#00A9E6", "#00A9E6", "#00A9E6"))
  )
  dataset <- data.frame(y = c(4.1, 5.3, 6.2, 5.0, 4.8, 7.4, 6.6, 5.9, 3.7, 5.5, 6.1, 4.4),
                        g = factor(rep(c("a", "b", "c"), each = 4)),
                        k = factor(rep(c("p", "p", "q", "q"), 3)))
  results <- jaspTools::runAnalysis("raincloudPlots", dataset, options)
  violins <- ggplot2::layer_data(rainOneObservationPlot(results), 1)
  # one violin per cloud; a and c are "R": 0.3 right of x = 1 and x = 3; b is "L": 0.3 left of x = 2
  expect_equal(sort(unique(violins$group)), c(1, 2, 3))
  expect_equal(unique(violins$x[violins$group == 1]), 1.3)
  expect_equal(unique(violins$x[violins$group == 2]), 1.7)
  expect_equal(unique(violins$x[violins$group == 3]), 3.3)
})

test_that("violins keep their cloud's side when a discrete covariate splits them", {
  options <- rainOneObservationOptions()
  options$colorAnyway <- FALSE # without the palette on the primary factor, the covariate splits the violins
  options$primaryFactor <- "g"
  options$primaryFactor.types <- "nominal"
  options$covariate <- "k"
  options$covariate.types <- "nominal"
  options$customSides <- TRUE
  options$numberOfClouds <- 3
  clouds <- c("Row 0", "Row 1", "Row 2")
  options$customizationTable <- list(
    list(levels = clouds, name = "data 1", values = c("L", "R", "R")),
    list(levels = clouds, name = "data 2", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 3", values = c(0, 0, 0)),
    list(levels = clouds, name = "data 4", values = c("#00A9E6", "#00A9E6", "#00A9E6"))
  )
  dataset <- data.frame(y = c(4.1, 5.3, 6.2, 5.0, 4.8, 7.4, 6.6, 5.9, 3.7, 5.5, 6.1, 4.4),
                        g = factor(rep(c("a", "b", "c"), each = 4)),
                        k = factor(rep(c("p", "q"), 6)))
  results <- jaspTools::runAnalysis("raincloudPlots", dataset, options)
  testPlot <- rainOneObservationPlot(results)
  violins <- ggplot2::layer_data(testPlot, 1)
  # two violins per cloud (levels p and q); a is "L": 0.3 left of x = 1; b and c are "R": 0.3 right of x = 2 and x = 3
  expect_equal(sort(unique(violins$group)), 1:6)
  expect_equal(unique(violins$x[violins$group %in% 1:2]), 0.7)
  expect_equal(unique(violins$x[violins$group %in% 3:4]), 2.3)
  expect_equal(unique(violins$x[violins$group %in% 5:6]), 3.3)
  expect_equal(testPlot$layers[[1]]$geom_params$side, c("l", "l", "r", "r", "r", "r"))
})
