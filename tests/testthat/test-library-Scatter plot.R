context("Library: Scatter plot")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("jaspPlotBuilder results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Scatter plot.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspDescriptives::jaspPlotBuilder(
          data = NULL,
          version = "0.98.1",
          PlotBuilderTab = list(list(addCurveFitPlotBuilder = TRUE, addDataPoint = TRUE, annotationPlotBuilder = list(list(annotationText = "r  = -0.368", annotationX = 2, annotationY = 2.2, value = "#"), list(annotationText = "p < .001", annotationX = 2, annotationY = 1.8, value = "#2")), blackOutlineCurveFit = TRUE, value = "Plot 1", variableXPlotBuilder = list(types = "scale", value = "Neuroticism"), variableYPlotBuilder = list(types = "scale", value = "Conscientiousness")))))

  plotName <- results[["results"]][["tidyPlotsContainer"]][["collection"]][["tidyPlotsContainer_tidyPlot_Plot 1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_plot-1")

})

