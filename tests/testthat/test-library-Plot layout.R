context("Library: Plot layout")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("jaspPlotBuilder results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Plot layout.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspDescriptives::jaspPlotBuilder(
          data = NULL,
          version = "0.98.1",
          PlotBuilderTab = list(list(addBoxplot = TRUE, addDataPoint = TRUE, emptyCircles = TRUE, value = "Plot 1", variableXPlotBuilder = list(types = "nominal", value = "species"), variableYPlotBuilder = list(types = "scale", value = "bill_length_mm"), whiteBorder = TRUE), list(addDataPoint = TRUE, addStatEllipse = TRUE, colorByGroup = "grouping", ellipseType = "norm", value = "Plot 2", variableColorPlotBuilder = list(types = "nominal", value = "species"), variableXPlotBuilder = list(types = "scale", value = "bill_length_mm"), variableYPlotBuilder = list(types = "scale", value = "bill_depth_mm"))),
          fullRowSpecifications = list(list(labelsFullRow = "A, B", plotIDsFullRow = "Plot 1, Plot 2", value = "#")),
          layoutHeight = 300,
          layoutWidth = 800))

  plotName <- results[["results"]][["plotGridContainer"]][["collection"]][["plotGridContainer_plotGrid"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_combined-plot-grid")

  plotName <- results[["results"]][["tidyPlotsContainer"]][["collection"]][["tidyPlotsContainer_tidyPlot_Plot 1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_plot-1")

  plotName <- results[["results"]][["tidyPlotsContainer"]][["collection"]][["tidyPlotsContainer_tidyPlot_Plot 2"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-3_plot-2")

})

