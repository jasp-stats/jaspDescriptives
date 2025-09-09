context("Descriptives Time Series")

qmlFile <- if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
  system.file("qml", "common", "DescriptivesTimeSeriesForm.qml", package = "jaspDescriptives")
} else if (endsWith(getwd(), "testthat")) {
  "../../inst/qml/common/DescriptivesTimeSeriesForm.qml"
} else {
  "inst/qml/common/DescriptivesTimeSeriesForm.qml"
}

options <- jaspTools:::readQML(qmlFile)
options$dependent <- "visits"
options$time <- "date"
options$lagPlot <- TRUE
options$acf <- TRUE
options$pacf <- TRUE
set.seed(1)
results <- jaspTools::runAnalysis("DescriptivesTimeSeries", "JASP Webpage Visits.csv", options)


test_that("Autocorrelation Function plot matches", {
  plotName <- results[["results"]][["acfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "autocorrelation-function")
})

test_that("Descriptive Statistics table results match", {
  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.756356922794839, 3594, 4456, 1839.74007220217, 214, 0, 4242,
                                      734.307307636129, 214, 277, 539207.222047821, "visits"))
})

test_that("Lag Plot matches", {
  plotName <- results[["results"]][["lagPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "lag-plot")
})

test_that("Partial Autocorrelation Function plot matches", {
  plotName <- results[["results"]][["pacfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "partial-autocorrelation-function")
})

test_that("Time Series Plot matches", {
  skip("Not reproducible on CI")
  plotName <- results[["results"]][["timeSeriesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "time-series-plot-desc")
})
