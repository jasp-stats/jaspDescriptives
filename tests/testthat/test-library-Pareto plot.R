context("Library: Pareto plot")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("Descriptives (analysis 1) results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Pareto plot.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspDescriptives::Descriptives(
          data = NULL,
          version = "0.98.1",
          formula =  ~ Complaint,
          maximum = FALSE,
          meanArithmetic = FALSE,
          minimum = FALSE,
          paretoAddCountVariable = "Total",
          paretoPlot = TRUE,
          paretoPlotRule = TRUE,
          sd = FALSE))

  plotName <- results[["results"]][["paretoPlots"]][["collection"]][["paretoPlots_Complaint"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_complaint")

  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 9, "Complaint"))

})

test_that("Descriptives (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Pareto plot.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspDescriptives::Descriptives(
          data = NULL,
          version = "0.98.1",
          formula =  ~ Complaint,
          maximum = FALSE,
          meanArithmetic = FALSE,
          minimum = FALSE,
          paretoAddCountVariable = "Total",
          paretoPlot = TRUE,
          paretoPlotRule = TRUE,
          paretoShiftAccumulationLine = TRUE,
          sd = FALSE))

  plotName <- results[["results"]][["paretoPlots"]][["collection"]][["paretoPlots_Complaint"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_complaint")

  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 9, "Complaint"))

})

