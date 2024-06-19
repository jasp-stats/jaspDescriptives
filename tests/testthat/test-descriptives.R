context("Descriptives")

# does not test
# - error handling
test_that("Main table results match", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$variables.types <- "scale"
  options$splitBy <- "contBinom"
  options$splitBy.types <- "nominal"
  options$median <- TRUE
  options$mode <- TRUE
  options$sum <- TRUE
  options$variance <- TRUE
  options$range <- TRUE
  options$seMean <- TRUE
  options$kurtosis <- TRUE
  options$skewness <- TRUE
  options$shapiroWilkTest <- TRUE
  options$mode <- TRUE
  options$quantilesForEqualGroups <- TRUE
  options$quantilesForEqualGroupsNumber <- 5
  options$percentiles <- TRUE
  options$percentileValues <- c(2, 5, 8)
  options$quartiles <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(1.89652072756094, 0, 3.356094448, -0.120135614827586, -0.2223981035,
                                      -2.336742886, 0, -2.336742886, 0.00342000811150064, 5.692837334,
                                      0.933547444665698, 0.885861572513177, 1.10575982846952, 0.618135836828014,
                                      0.145193378675912, 0.313719932561217, -6.96786566, 58, "contNormal",
                                      1.22270479825695, -0.8465404722, -0.6015855064, 0.189093977,
                                      0.5121792992, -2.12776693668, -1.6743740472, -1.38430134284,
                                      -0.77748184225, -0.2223981035, 0.38502497975, 0.972132667292966,
                                      1, 2.179421126, -0.283499835571429, -0.405769511, -3.023963827,
                                      0, -3.023963827, 0.401705854633909, 5.203384953, 0.972586424088514,
                                      0.166587887409046, 0.994612407217046, 0.716632727345669, 0.15347202634745,
                                      0.365360605557062, -11.906993094, 42, "contNormal", 0.989253840590086,
                                      -0.9755913562, -0.5800195022, -0.2167812726, 0.521794901, -1.89726255948,
                                      -1.61858187715, -1.43841230624, -0.80595539125, -0.405769511,
                                      0.4460704255)
  )
})

# test confidence intervals only
test_that("Descriptive Statistics table results match", {
  options <- analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$variables.types <- "scale"
  options$valid <- FALSE
  options$missing <- FALSE
  options$variance <- TRUE
  options$minimum <- FALSE
  options$maximum <- FALSE
  options$meanCi <- TRUE
  options$sdCi <- TRUE
  options$varianceCi <- TRUE
  options$sdCi <- TRUE
  options$meanCiMethod <- "normalModel"
  set.seed(1)
  results <- runAnalysis("Descriptives", "test.csv", options)
  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.18874858754, -0.396193843016565, 0.018696667936565, 0.869717253788563,
                                      1.24667712392121, 1.05841360919316, "contNormal", 1.1202393681253,
                                      0.756408180905588, 1.55420388476218))
})

# test confidence intervals only
test_that("Descriptive Statistics table results match", {
  options <- analysisOptions("Descriptives")
  options$variables <- c("contNormal", "contGamma")
  options$variables.types <- c("scale", "scale")
  options$valid <- FALSE
  options$missing <- FALSE
  options$variance <- FALSE
  options$minimum <- FALSE
  options$maximum <- FALSE
  options$meanCi <- TRUE
  options$meanCiMethod <- "oneSampleTTest"
  set.seed(1)
  results <- runAnalysis("Descriptives", "test.csv", options)
  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.18874858754, -0.398760810055083, 0.0212636349750834, 1.05841360919316,
                                      "contNormal", 2.03296079621, 1.72889718286736, 2.33702440955264,
                                      1.53241112621044, "contGamma"))

  # compare against stats::t.test directly since these should be identical
  options <- analysisOptions("Descriptives")
  options$variables <- "extra"
  options$variables.types <- "scale"
  options$valid <- FALSE
  options$missing <- FALSE
  options$variance <- FALSE
  options$minimum <- FALSE
  options$maximum <- FALSE
  options$meanCi <- TRUE
  options$meanCiMethod <- "oneSampleTTest"
  data("sleep")
  set.seed(1)
  results <- runAnalysis("Descriptives", sleep, options)
  table <- results[["results"]][["stats"]][["data"]]
  baseR <- c(t.test(extra ~ 1, data = sleep)$conf.int)
  jasp  <- c(table[[1]][["MeanCILB"]], table[[1]][["MeanCIUB"]])
  testthat::expect_equal(jasp, baseR)

})


test_that("Association matrices match", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$associationMatrixUse <- "everything"
  options$variables <- c("contNormal",  "contGamma", "debMiss1")
  options$variables.types <- c("scale", "scale", "scale")
  options$covariance <- TRUE
  options$correlation <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  table <- results[["results"]][["associationMatrix"]][["collection"]][["associationMatrix_Correlation"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal", -0.0592003859505643, 1, "", "contGamma", 1, -0.0592003859505643,
                                      "", "debMiss1", "", "", 1))

  table <- results[["results"]][["associationMatrix"]][["collection"]][["associationMatrix_Covariance"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal", -0.0960185736017089, 1.1202393681253, "", "contGamma",
                                      2.34828385973354, -0.0960185736017089, "", "debMiss1", "", "", ""))
})

test_that("Frequencies table matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "facGender"
  options$variables.types <- "nominal"
  options$splitBy <- "contBinom"
  options$splitBy.types <- "nominal"
  options$frequencyTables <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  table <- results[["results"]][["tables"]][["collection"]][["tables_facGender"]][["data"]]
  jaspTools::expect_equal_tables(table,
  list("TRUE", 44.8275862068966, 26, "f", 44.8275862068966, 44.8275862068966,
			 0, "FALSE", 100, 32, "m", 55.1724137931034, 55.1724137931034,
			 0, "FALSE", "", 0, "Missing", 0, "", "", "FALSE", "", 58, "Total",
			 100, "", "", "TRUE", 57.1428571428571, 24, "f", 57.1428571428571,
			 57.1428571428571, 1, "FALSE", 100, 18, "m", 42.8571428571428,
			 42.8571428571428, 1, "FALSE", "", 0, "Missing", 0, "", "", "FALSE",
			 "", 42, "Total", 100, "", "")
  )
})

test_that("Frequencies table matches with missing values", {
  options <- jaspTools::analysisOptions("Descriptives")
  x <- c(rep(NA, 10), rep(1:2, times=10))
  split <- rep(1:2, each=15)
  data <- data.frame(x=as.factor(x), split=as.factor(split))
  options$variables <- "x"
  options$variables.types <- "nominal"
  options$splitBy <- "split"
  options$splitBy.types <- "nominal"
  options$frequencyTables <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", data, options)
  table <- results[["results"]][["tables"]][["collection"]][["tables_x"]][["data"]]
  jaspTools::expect_equal_tables(table,
  list("TRUE", 60, 3, 1, 20, 60, 1, "FALSE", 100, 2, 2, 13.3333333333333,
			 40, 1, "FALSE", "", 10, "Missing", 66.6666666666667, "", "",
			 "FALSE", "", 15, "Total", 100, "", "", "TRUE", 46.6666666666667,
			 7, 1, 46.6666666666667, 46.6666666666667, 2, "FALSE", 100, 8,
			 2, 53.3333333333333, 53.3333333333333, 2, "FALSE", "", 0, "Missing",
			 0, "", "", "FALSE", "", 15, "Total", 100, "", "")
  )
})

test_that("Distribution plot matches", {
  skip("This test need to be verified")
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$variables.types <- "scale"
  options$distributionPlots <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "distribution")
})

test_that("Correlation plot matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- c("contNormal", "contGamma")
  options$variables.types <- c("scale", "scale")
  options$correlationPlots <- TRUE
  options$distributionAndCorrelationPlotDensity <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "correlation")
})

test_that("Boxplot matches", {
  set.seed(0)
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "contGamma"
  options$variables.types <- "scale"
  options$splitBy <- "facFive"
  options$splitBy.types <- "nominal"
  options$boxPlotBoxPlot <- TRUE
  options$boxPlotColourPalette <- TRUE
  options$boxPlotJitter <- TRUE
  options$boxPlotOutlierLabel <- TRUE
  options$boxPlotViolin <- TRUE
  options$boxPlot <- TRUE
  options$colorPalette <- "ggplot2"
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "boxplot")
})

test_that("Q-QPlot plot matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$variables.types <- "scale"
  options$qqPlot <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "qqplot")
})

test_that("Scatter plot matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- c("contcor1", "contcor2")
  options$variables.types <- c("scale", "scale")
  # incorrectly parsed by jaspTools, which matches "enabled: distributionPlots.checked" a couple lines down and sets the option to true
  options$correlationPlots <- FALSE
  options$scatterPlot <- TRUE
  options$colorPalette <- "ggplot2"
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "scatterplot")
})

test_that("Dot plot matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$variables.types <- "scale"
  options$dotPlot <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "dotPlot")
})

test_that("Pie chart matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "facFive"
  options$variables.types <- "nominal"
  options$pieChart <- TRUE
  options$colorPalette <- "ggplot2"
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "pieChart")
})

test_that("Pareto plot matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "facFive"
  options$variables.types <- "nominal"
  options$paretoPlot <- TRUE
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "parPlot")
})

test_that("Likert plot matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "facFive"
  options$variables.types <- "nominal"
  options$likertPlot <- TRUE
  options$likertPlotAssumeVariablesSameLevel <- TRUE
  options$likertPlotAdjustableFontSize <- "normal"
  df <- jaspTools:::loadCorrectDataset("test.csv")
  df$facFive <- as.ordered(df$facFive)
  results <- jaspTools::runAnalysis("Descriptives", df, options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "likPlot")
})

test_that("Density plot matches", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$variables.types <- "scale"
  options$densityPlot <- TRUE
  options$densityPlotSeparate <- "facFive"
  # https://github.com/jasp-stats/jaspDescriptives/pull/216 added a reuseable QML element for colorPalette, but jaspTools doesn't understand that so we have to add the default value manually
  options$colorPalette <- "colorblind"
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "densPlot")
})

test_that("Analysis handles identical variables", {
  # catches this: https://github.com/jasp-stats/jasp-issues/issues/553
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- list("contNormal", "debSame")
  options$variables.types <- c("scale", "scale")
  options$splitBy <- "facFive"
  options$splitBy.types <- "nominal"
  options$shapiroWilkTest <- TRUE
  options$skewness <- TRUE
  options$kurtosis <- TRUE

  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)

  jaspTools::expect_equal_tables(results[['results']][['stats']][['data']],
                      list(-1.19915675837133, 1, 1.007309698, -0.33853731055, -1.625143884,
                           0, 0.0819021844894419, 0.915696014062066, 0.338805595442952,
                           0.850644958728125, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 0.992813181737034, 2, 1.889051803, -0.38388772215,
                           -1.953344972, 0, 0.467911256938122, 0.956031076407404, 0.535107137711909,
                           0.893846327627993, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 1.36037376866094, 3, 2.958797116, 0.1427499711,
                           -1.627592736, 0, 0.532926410776693, 0.959450290686737, 0.725619918998665,
                           1.0709839671614, 0.992383612541845, 0.512103336707757, 20, "contNormal",
                           2.42193307435088, 4, 2.179421126, -0.357863015, -3.023963827,
                           0, 0.358996514131301, 0.949461949369392, -0.0489162694087717,
                           1.04541944723916, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 1.62009376503733, 5, 3.356094448, -0.00620486110000001,
                           -2.336742886, 0, 0.0681263561514, 0.911517098559219, 0.828552911812968,
                           1.35277978138929, 0.992383612541845, 0.512103336707757, 20,
                           "contNormal", 0, 0, 0, 0, "NaN", 1, 12.3, 12.3, 12.3, 0, "NaN",
                           "NaN", "NaN", 0, 0.992383612541845, 0.512103336707757, 20, "debSame",
                           0, 0, 0, 0, "NaN", 2, 12.3, 12.3, 12.3, 0, "NaN", "NaN", "NaN",
                           0, 0.992383612541845, 0.512103336707757, 20, "debSame", 0, 0,
                           0, 0, "NaN", 3, 12.3, 12.3, 12.3, 0, "NaN", "NaN", "NaN", 0,
                           0.992383612541845, 0.512103336707757, 20, "debSame", 0, 0, 0,
                           0, "NaN", 4, 12.3, 12.3, 12.3, 0, "NaN", "NaN", "NaN", 0, 0.992383612541845,
                           0.512103336707757, 20, "debSame", 0, 0, 0, 0, "NaN", 5, 12.3,
                           12.3, 12.3, 0, "NaN", "NaN", "NaN", 0, 0.992383612541845, 0.512103336707757,
                           20, "debSame"))

  # also check footnotes
  jaspTools::expect_equal_tables(results[['results']][['stats']][['footnotes']],
                      list("Kurtosis", "P-value of Shapiro-Wilk", "Shapiro-Wilk", "Skewness",
                           174, "debSame1", 0, "All values are identical"))
})

test_that("Analysis explains supremum and infimum of empty sets", {
  options <- analysisOptions("Descriptives")
  options$variables <- "debMiss99"
  options$variables.types <- "scale"
  options$splitBy <- "contBinom"
  options$splitBy.types <- "nominal"

  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)

  jaspTools::expect_equal_tables(results[['results']][['stats']][['footnotes']],
                      list("Maximum", "Minimum", 22, "debMiss991", 0,
                           "Infimum (minimum) of an empty set is <unicode>, supremum (maximum) of an empty set is -<unicode>.")
                      )
})

test_that("Stem and leaf tables match", {

  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "contNormal"
  options$variables.types <- "scale"
  options$stemAndLeaf <- TRUE
  options$stemAndLeafScale <- 1.2
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  table <- results[["results"]][["stemAndLeaf"]][["collection"]][["stemAndLeaf_stem_and_leaf_contNormal"]][["data"]]
  expect_equal_tables(
    table,
    list(-3, 0, "|", -2, 320, "|", -1, 66644444431000, "|", 0, 9.99988888877778e+41,
         "|", 0, 1.12223333334445e+27, "|", 1, 1469, "|", 2, 27, "|",
         3, 4, "|"),
    label = "stem and life without split"
  )

  options$splitBy <- "contBinom"
  options$splitBy.types <- "nominal"
  results <- jaspTools::runAnalysis("Descriptives", "test.csv", options)
  table0 <- results[["results"]][["stemAndLeaf"]][["collection"]][["stemAndLeaf_contNormal"]][["collection"]][["stemAndLeaf_contNormal_stem_and_leaf_contNormal_0"]][["data"]]
  expect_equal_tables(
    table0,
    list(-2, 320, "|", -1, 6, "|", -1, 44430, "|", 0, 99988887777766672,
         "|", 0, 4441, "|", 0, 11222333333444, "|", 0, 555668, "|", 1,
         1, "|", 1, 6, "|", 2, "", "|", 2, 7, "|", 3, 4, "|"),
    label = "stem and life with split - 0"
  )

  table1 <- results[["results"]][["stemAndLeaf"]][["collection"]][["stemAndLeaf_contNormal"]][["collection"]][["stemAndLeaf_contNormal_stem_and_leaf_contNormal_1"]][["data"]]
  expect_equal_tables(
    table1,
    list(-3, 0, "|", -2, "", "|", -2, "", "|", -1, 66, "|", -1, 444100,
         "|", 0, 988777665, "|", 0, 444443221111, "|", 0, 4, "|", 0,
         5556689, "|", 1, 4, "|", 1, 9, "|", 2, 2, "|"),
    label = "stem and life with split - 1"
  )

})

options <- analysisOptions("Descriptives")
options$correlationPlots <- FALSE
options$distributionPlots <- TRUE
options$variables <- "facGender"
options$variables.types <- "nominal"
set.seed(1)
results <- runAnalysis("Descriptives", "debug.csv", options)

test_that("facGender plot matches", {
	plotName <- results[["results"]][["distributionPlots"]][["collection"]][["distributionPlots_facGender"]][["data"]]
	testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
	jaspTools::expect_equal_plots(testPlot, "facgender")
})


options <- analysisOptions("Descriptives")
options$variables <- "contExpon"
options$variables.types <- "scale"
options$splitBy   <- "facFive"
options$splitBy.types <- "nominal"
options$intervalPlot <- TRUE
set.seed(1)
results <- runAnalysis("Descriptives", "test.csv", options)

test_that("interval plot across groups matches", {
  plotName <- results[["results"]][["IntervalPlots"]][["collection"]][["IntervalPlots_contExpon"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "interval_plot_across_facFive_contExpon")
})


dat <- data.frame(factorLargeCounts = factor(rep(letters[1:8], seq(10, 80, 10))))
options <- analysisOptions("Descriptives")
options$variables <- "factorLargeCounts"
options$variables.types <- "nominal"
options$dotPlot <- TRUE
results <- runAnalysis("Descriptives", dat, options)

test_that("dot plot with large counts is legible", {
  plotName <- results[["results"]][["DotPlots"]][["collection"]][["DotPlots_factorLargeCounts"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "dot_plot_large_counts")
})
