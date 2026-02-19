context("Example: Pareto plot")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("Descriptives (analysis 1) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Pareto plot.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[1]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Descriptives", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 9, "jaspColumn2"))

})

test_that("Descriptives (analysis 2) results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Pareto plot.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)[[2]]
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("Descriptives", encoded$dataset, encoded$options, encodedDataset = TRUE)

  table <- results[["results"]][["stats"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0, 9, "jaspColumn2"))

})

