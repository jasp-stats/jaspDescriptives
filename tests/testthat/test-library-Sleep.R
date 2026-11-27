context("Library: Sleep")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("Descriptives results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Sleep.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspDescriptives::Descriptives(
          data = NULL,
          version = "0.98.1",
          formula =  ~ extra,
          boxPlot = TRUE,
          boxPlotColourPalette = TRUE,
          boxPlotJitter = TRUE,
          boxPlotViolin = TRUE,
          meanCiMethod = "normalModel",
          paretoPlotRuleCi = 0.95,
          scatterPlotRegressionLineType = "smooth",
          splitBy = ~ group))

  plotName <- results[["results"]][["boxPlot"]][["collection"]][["boxPlot_extra"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_extra")

  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "sf:4;dp:3", "number", 3.7, 0.75, "sf:4;dp:3", "number", -1.6,
     0, 1.78900965775916, 10, "extra", 2, "sf:4;dp:3", "number",
     5.5, 2.33, "sf:4;dp:3", "number", -0.1, 0, 2.00224873579683,
     10, "extra"))

})

