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
  options$boxNudge <- 0
  options$boxOpacity <- 0.5
  options$boxOutlineWidth <- 1.5
  options$boxPadding <- 0.1
  options$boxWidth <- 0.1
  options$colorPalette <- "colorblind"
  options$covariatePalette <- "viridis"
  options$dependentVariables <- "bill_length_mm"
  options$heightPlot <- 550
  options$meanLines <- FALSE
  options$pointNudge <- 0.19
  options$pointOpacity <- 0.5
  options$pointSize <- 3
  options$pointSpread <- 0.1
  options$primaryFactor <- "island"
  options$showBox <- TRUE
  options$table <- TRUE
  options$vioNudge <- 0.09
  options$vioOpacity <- 0.5
  options$vioOutlineWidth <- 1.5
  options$widthPlot <- 675
  results <- jaspTools::runAnalysis("raincloudPlots", "palmerPenguins.csv", options)
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
  options$heightPlot <- 550
  options$meanLines <- FALSE
  options$pointNudge <- 0.19
  options$pointOpacity <- 0.5
  options$pointSize <- 3
  options$pointSpread <- 0.1
  options$primaryFactor <- "island"
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
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "body_mass_g"
  options$heightPlot <- 550
  options$horizontal <- TRUE
  options$meanLines <- FALSE
  options$pointNudge <- 0.275
  options$pointOpacity <- 0.5
  options$pointSpread <- 0.1
  options$primaryFactor <- "species"
  options$secondaryFactor <- "sex"
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
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "bill_length_mm"
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
  options$secondaryFactor <- "island"
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

test_that("table results match for a plot with both factors, no box, means, meanLines, and bootstrapped ci", {
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
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "bill_length_mm"
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
  options$secondaryFactor <- "island"
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
                                 list(37.9772727272727, 38.975, 44, "Adelie", "Biscoe", 39.9204545454545,
                                      37.7125, 38.5017857142857, 56, "", "Dream", 39.3267857142857,
                                      37.9117647058824, 38.9509803921569, 51, "", "Torgersen", 40.021568627451,
                                      47.8, 48.8338235294118, 68, "Chinstrap", "Dream", 49.8808823529412,
                                      46.8463414634146, 47.5048780487805, 123, "Gentoo", "Biscoe",
                                      48.2170731707317))
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
  options$covariatePalette <- "viridis"
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "flipper_length_mm"
  options$heightPlot <- 550
  options$meanLines <- FALSE
  options$pointNudge <- 0.175
  options$pointOpacity <- 0.66
  options$pointSize <- 1.5
  options$primaryFactor <- "species"
  options$showBox <- TRUE
  options$vioNudge <- 0.09
  options$vioOpacity <- 0.75
  options$vioOutline <- "black"
  options$widthPlot <- 675
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", "palmerPenguins.csv", options)
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
  options$covariatePalette <- "colorblind3"
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "body_mass_g"
  options$heightPlot <- 550
  options$mean <- TRUE
  options$meanLines <- FALSE
  options$pointNudge <- 0.3
  options$pointOpacity <- 0.5
  options$pointSpread <- 0.15
  options$secondaryFactor <- "species"
  options$showBox <- TRUE
  options$vioHeight <- 0.6
  options$vioNudge <- 0.15
  options$vioOpacity <- 0.5
  options$widthPlot <- 675
  set.seed(1)
  df <- read.csv("palmerPenguins.csv")
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
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "sepalWidth"
  options$heightPlot <- 500
  options$jitter <- TRUE
  options$meanLines <- FALSE
  options$numberOfClouds <- 6
  options$observationId <- "id"
  options$observationIdLineOpacity <- 0.1
  options$observationIdLineWidth <- 0.75
  options$pointNudge <- 0.15
  options$pointOpacity <- 0.5
  options$primaryFactor <- "time"
  options$secondaryFactor <- "Species"
  options$showBox <- TRUE
  options$showCaption <- FALSE
  options$vioHeight <- 0.4
  options$vioNudge <- 0.3
  options$vioOpacity <- 0.5
  options$widthPlot <- 750
  set.seed(1)
  results <- jaspTools::runAnalysis("raincloudPlots", "irisFertilizer.csv", options)
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
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "sepalWidth"
  options$heightPlot <- 550
  options$mean <- TRUE
  options$meanIntervalCustom <- TRUE
  options$meanLinesOpacity <- 0.25
  options$meanLinesWidth <- 2
  options$meanPosition <- "onAxis"
  options$meanSize <- 7.5
  options$numberOfClouds <- 6
  options$pointOpacity <- 0.25
  options$primaryFactor <- "time"
  options$secondaryFactor <- "Species"
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
    list(levels = list(""), name = "", values = list("'F8766D"))
  )
  options$dependentVariables <- "sepalWidth"
  options$heightPlot <- 550
  options$jitter <- TRUE
  options$meanLines <- FALSE
  options$numberOfClouds <- 4
  options$observationId <- "id"
  options$observationIdLineOpacity <- 0.15
  options$pointNudge <- 0.15
  options$pointOpacity <- 0.75
  options$primaryFactor <- "time"
  options$secondaryFactor <- "Species"
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
  options$customizationTable <- list(
    list(levels = "", name = "", values = "R"),
    list(levels = "", name = "", values = 0),
    list(levels = "", name = "", values = 0),
    list(levels = list(""), name = "", values = list("#8B93FF", "#5755FE", "#FF71CD"))
  )
  options$dependentVariables <- "bill_length_mm"
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
  options$primaryFactor <- "island"
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
