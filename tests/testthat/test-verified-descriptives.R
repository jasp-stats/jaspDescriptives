context("Descriptives -- Verification project")

# https://jasp-stats.github.io/jasp-verification-project/descriptives.html
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  options <- jaspTools::analysisOptions("Descriptives")
  options$variables <- "Data"
  options$mean <- TRUE
  options$median <- TRUE
  options$variance <- TRUE
  options$mode <- FALSE
  options$standardDeviation <- TRUE
  options$standardErrorMean <- TRUE
  options$minimum
  options$maximum
  options$mode <- TRUE
  options$quartiles <- TRUE
  
  # Main table
  results <- jaspTools::runAnalysis("Descriptives",
                                    data.frame("Data"=1:5),
                                    options)
  resultTable <- results$results$stats$data

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list(5, 3, 3, 1, 0, 1, 1.58113883008419, 0.707106781186548, 5, "Data",
               2.5, 2, 3, 4)
  )
})