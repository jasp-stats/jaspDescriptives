context("Library: Repeated measures")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("jaspPlotBuilder results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Repeated measures.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspDescriptives::jaspPlotBuilder(
          data = NULL,
          version = "0.98.1",
          PlotBuilderTab = list(list(addDataPoint = TRUE, connectRMPlotBuilder = TRUE, isRM = "RM", pairwiseComparisons = list(list(group1 = "Before", group2 = "After", pAdj = 0.325, tipLengthPValue = 0.02, value = "#")), repeatedMeasuresCells = list("Weight Before", "Weight After"), repeatedMeasuresFactors = list(list(levels = list("Before", "After"), name = "RM Factor 1")), titleXPlotBuilder = "Time point", titleYPlotBuilder = "Weight (lb)", value = "Plot 1", xVarRM = list(types = "unknown", value = "RM Factor 1"), yPositionPValue = 230))))

  plotName <- results[["results"]][["tidyPlotsContainer"]][["collection"]][["tidyPlotsContainer_tidyPlot_Plot 1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_plot-1")

})

