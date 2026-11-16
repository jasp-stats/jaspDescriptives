context("Example: Scatter plot")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("jaspPlotBuilder results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Scatter plot.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Run analysis
  set.seed(1)
  results <- jaspTools::runAnalysis("jaspPlotBuilder", dataset, opts)

  plotName <- results[["results"]][["tidyPlotsContainer"]][["collection"]][["tidyPlotsContainer_tidyPlot_Plot 1"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_plot-1")

})

