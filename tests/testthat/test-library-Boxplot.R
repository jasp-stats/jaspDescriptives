context("Library: Boxplot")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("jaspPlotBuilder results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Boxplot.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspDescriptives::jaspPlotBuilder(
          data = NULL,
          version = "0.98.1",
          PlotBuilderTab = list(list(YPaddingSecond = 0.25, addBoxplot = TRUE, addDataPoint = TRUE, colorByGroup = "x", legendPosistionPlotBuilder = "none", pairwiseComparisons = list(list(group1 = "setosa", group2 = "versicolor", pAdj = "<.001", value = "#"), list(group1 = "setosa", group2 = "virginica", pAdj = "<.001", value = "#2"), list(group1 = "versicolor", group2 = "virginica", pAdj = "<.001", value = "#3")), value = "Plot 1", variableXPlotBuilder = list(types = "nominal", value = "Species"), variableYPlotBuilder = list(types = "scale", value = "Sepal.Length"), yPositionPValue = 8))))

  plotName <- results[["results"]][["tidyPlotsContainer"]][["collection"]][["tidyPlotsContainer_tidyPlot_Plot 1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_plot-1")

})

