# Real analysis integration. Time is cleared on manually ordered references so
# their expected response order does not depend on the preparation parser.
descriptivesOrderingRun <- function(input, time = TRUE, plots = TRUE, ...) {
  qml <- system.file("qml", "common", "DescriptivesTimeSeriesForm.qml", package = "jaspDescriptives")
  options <- jaspTools:::readQML(qml)
  options$dependent <- list(types = "scale", value = "y")
  options$time <- if (time) list(types = if (is.numeric(input$Time)) "scale" else "nominal", value = "Time") else
    list(types = list(), value = "")
  # The installed graph dependency lacks makeAlignedMatrixPlot. The independent
  # lag/ACF/PACF plots remain usable; this file does not certify the main plot.
  options$timeSeriesPlot <- FALSE
  options$lagPlot <- options$acf <- options$pacf <- plots
  options$lagPlotLag <- 1
  options$lagPlotRegressionLine <- FALSE
  options$acfMaxLag <- options$pacfMaxLag <- 3
  options$acfZeroLag <- FALSE
  options <- utils::modifyList(options, list(...))
  encoded <- jaspTools:::encodeOptionsAndDataset(jaspTools:::fixOptionsForVariableTypes(options), input)
  rownames(encoded$dataset) <- rownames(input)
  result <- jaspTools::runAnalysis("DescriptivesTimeSeries", dataset = encoded$dataset,
                                  options = encoded$options, encodedDataset = TRUE, view = FALSE)
  expect_identical(result$status, "complete")
  expect_false(isTRUE(result$results$error))
  result
}

descriptivesOrderingTable <- function(result) {
  rows <- result$results$descriptivesTable$data
  expect_length(rows, 1L)
  rows[[1L]]
}

descriptivesOrderingPlot <- function(result, name) {
  id <- result$results[[name]]$data
  expect_length(id, 1L)
  plot <- result$state$figures[[id]]$obj
  if (inherits(plot, "jaspGraphsPlot")) plot <- plot$subplots$mainPlot
  expect_s3_class(plot, "ggplot")
  plot
}

descriptivesOrderingPlotData <- function(result) {
  lag <- descriptivesOrderingPlot(result, "lagPlot")$data[, c("x", "y")]
  rownames(lag) <- NULL
  correlationData <- function(name) {
    plot <- descriptivesOrderingPlot(result, name)
    layers <- lapply(plot$layers, function(layer) layer$data)
    candidates <- Filter(function(data) is.data.frame(data) && all(c("acf", "lag") %in% names(data)), layers)
    expect_gt(length(candidates), 0L)
    data <- candidates[[1L]]
    rownames(data) <- NULL
    data
  }
  list(lag = lag, acf = correlationData("acfPlot"), pacf = correlationData("pacfPlot"))
}

expectDescriptivesOrderedLike <- function(actual, reference, y) {
  table <- descriptivesOrderingTable(actual)
  expect_equal(table, descriptivesOrderingTable(reference), tolerance = 1e-12)
  expect_equal(table$valid, sum(!is.na(y)))
  expect_equal(table$missing, sum(is.na(y)))
  expect_equal(table$start, y[which(!is.na(y))[1L]])
  expect_equal(table$end, tail(y[!is.na(y)], 1L))
  # Adjacent pairs are taken before omitting missing observations.
  pairs <- data.frame(x = head(y, -1L), y = tail(y, -1L))
  pairs <- pairs[complete.cases(pairs), , drop = FALSE]
  rownames(pairs) <- NULL
  expect_equal(table$ar, stats::cor(pairs$x, pairs$y), tolerance = 1e-12)

  plots <- descriptivesOrderingPlotData(actual)
  expect_equal(plots, descriptivesOrderingPlotData(reference), tolerance = 1e-12)
  expect_equal(plots$lag, pairs, tolerance = 1e-12)
  completeY <- y[!is.na(y)]
  expectedAcf <- stats::acf(completeY, lag.max = 3, plot = FALSE)
  expectedPacf <- stats::pacf(completeY, lag.max = 3, plot = FALSE)
  expect_equal(plots$acf$acf, as.numeric(expectedAcf$acf)[-1L], tolerance = 1e-12)
  expect_equal(plots$acf$lag, as.numeric(expectedAcf$lag)[-1L])
  expect_equal(plots$pacf$acf, as.numeric(expectedPacf$acf), tolerance = 1e-12)
  expect_equal(plots$pacf$lag, as.numeric(expectedPacf$lag))
}

descriptivesOrderingNote <- function(result, name) {
  paste(unlist(result$results[[name]], use.names = FALSE), collapse = " ")
}

expectUnavailableDescriptivesCell <- function(cell) {
  # The real results serializer represents NA table values as empty strings.
  # An absent field or non-finite numeric value must not pass this check.
  expect_length(cell, 1L)
  expect_identical(cell, "")
}

orderingY <- c(4, 2, 8, 1, 9, 6, 3, 7, 5, 11, 10, 12)
orderingPermutation <- c(5, 2, 9, 1, 8, 3, 12, 4, 11, 6, 10, 7)
orderingNumericTime <- c(-1.9, -1.7, -1.6, -1.2, -1, -.8, -.7, -.3, -.2, 0, .2, .6)

test_that("Descriptives uses numeric and irregular date Time to order lag-based output", {
  reference <- descriptivesOrderingRun(data.frame(y = orderingY), time = FALSE)
  dates <- format(as.Date("2020-01-01") + c(0, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233))
  for (time in list(orderingNumericTime, dates)) {
    input <- data.frame(y = orderingY, Time = time)[orderingPermutation, ]
    rownames(input) <- seq_len(nrow(input))
    actual <- descriptivesOrderingRun(input)
    expectDescriptivesOrderedLike(actual, reference, orderingY)
    expect_match(descriptivesOrderingNote(actual, "timeSortingNote"), "sorted by Time", fixed = TRUE)
    if (is.character(time))
      expect_match(descriptivesOrderingNote(actual, "timeInterpretationNote"), "YYYY-MM-DD", fixed = TRUE)
    else
      expect_null(actual$results$timeInterpretationNote)
  }
})

test_that("Descriptives retains missing-response positions when forming lag pairs", {
  y <- orderingY
  y[c(1, 5, 12)] <- NA_real_
  input <- data.frame(y = y, Time = factor(as.character(orderingNumericTime)))[orderingPermutation, ]
  rownames(input) <- seq_len(nrow(input))
  actual <- descriptivesOrderingRun(input)
  reference <- descriptivesOrderingRun(data.frame(y = y), time = FALSE)
  expectDescriptivesOrderedLike(actual, reference, y)
  expect_equal(descriptivesOrderingTable(actual)$missing, 3)
})

test_that("Descriptives treats tied observations as successive steps in stable input order", {
  input <- data.frame(y = orderingY, Time = c(2, 1, 2, 1, 3, 3, 2, 1, 4, 4, 3, 2))
  expectedY <- orderingY[c(2, 4, 8, 1, 3, 7, 12, 5, 6, 11, 9, 10)]
  actual <- descriptivesOrderingRun(input)
  reference <- descriptivesOrderingRun(data.frame(y = expectedY), time = FALSE)
  expectDescriptivesOrderedLike(actual, reference, expectedY)
  expect_match(descriptivesOrderingNote(actual, "timeTiesNote"), "successive steps", fixed = TRUE)
})

test_that("unusable Time produces the same Descriptives output as cleared Time", {
  y <- orderingY[orderingPermutation]
  reference <- descriptivesOrderingRun(data.frame(y = y), time = FALSE)
  times <- list(rep(c("France", "Germany", "Spain"), 4),
                rep(c("01-10.", "02-10.", "03-10."), 4),
                c(NA_real_, orderingNumericTime[-1L]))
  for (time in times) {
    actual <- descriptivesOrderingRun(data.frame(y = y, Time = time))
    expectDescriptivesOrderedLike(actual, reference, y)
    expect_match(descriptivesOrderingNote(actual, "timeWarning"), "the analysis used spreadsheet row order", fixed = TRUE)
  }
})

test_that("Descriptives filters numeric Time, UTC dates and original spreadsheet rows", {
  input <- data.frame(y = orderingY, Time = orderingNumericTime)[orderingPermutation, ]
  rownames(input) <- as.character(seq(2, 24, 2))
  actual <- descriptivesOrderingRun(input, filter = TRUE, filterBy = "time", timeStart = -1.2, timeEnd = .2)
  expectedY <- orderingY[4:11]
  reference <- descriptivesOrderingRun(data.frame(y = expectedY), time = FALSE)
  expectDescriptivesOrderedLike(actual, reference, expectedY)

  # Physical rows 4..12 are input positions 2..6, whose Time ranks are
  # 2, 9, 1, 8, 3. This also represents a dataset with a global row filter.
  actual <- descriptivesOrderingRun(input, filter = TRUE, filterBy = "row", rowStart = 4, rowEnd = 12)
  expectedY <- orderingY[c(1, 2, 3, 8, 9)]
  reference <- descriptivesOrderingRun(data.frame(y = expectedY), time = FALSE)
  expectDescriptivesOrderedLike(actual, reference, expectedY)
  expect_match(descriptivesOrderingNote(actual, "timeFilterWarning"), "skip observations", fixed = TRUE)

  dates <- c("2020-01-01T00:00Z", "2020-01-01T06:00Z", "2020-01-01T18:00Z",
             "2020-01-02T00:00Z", "2020-01-02T12:00Z", "2020-01-02T23:59:59.999Z",
             "2020-01-03T00:00Z", "2020-01-03T12:00Z", "2020-01-04T00:00Z",
             "2020-01-04T12:00Z", "2020-01-05T00:00Z", "2020-01-06T12:00Z")
  input <- data.frame(y = orderingY, Time = dates)[orderingPermutation, ]
  rownames(input) <- seq_len(nrow(input))
  actual <- descriptivesOrderingRun(input, filter = TRUE, filterBy = "date", dateStart = "2020-01-01", dateEnd = "2020-01-02")
  expectedY <- orderingY[1:6]
  reference <- descriptivesOrderingRun(data.frame(y = expectedY), time = FALSE)
  expectDescriptivesOrderedLike(actual, reference, expectedY)
})

test_that("unassigned dependent and zero input rows leave Descriptives unpopulated", {
  input <- data.frame(y = orderingY, Time = orderingNumericTime)
  result <- descriptivesOrderingRun(input, plots = FALSE, dependent = list(types = list(), value = ""))
  expect_length(result$results$descriptivesTable$data, 0L)
  result <- descriptivesOrderingRun(input[FALSE, ], plots = FALSE)
  expect_length(result$results$descriptivesTable$data, 0L)
})

test_that("a filter retaining one observation gives a valid table with unavailable dispersion and lag correlation", {
  input <- data.frame(y = c(4, 7, 2), Time = c(1, 2, 3))
  result <- descriptivesOrderingRun(input, plots = FALSE, filter = TRUE,
                                    filterBy = "time", timeStart = 2, timeEnd = 2)
  table <- descriptivesOrderingTable(result)
  expect_equal(table$valid, 1)
  expect_equal(table$missing, 0)
  for (name in c("mean", "start", "end", "min", "max")) expect_equal(table[[name]], 7)
  expect_equal(table$range, 0)
  for (name in c("sd", "var", "ar")) expectUnavailableDescriptivesCell(table[[name]])
})

test_that("all-missing responses and absent adjacent pairs do not terminate table-only output", {
  result <- descriptivesOrderingRun(data.frame(y = rep(NA_real_, 4), Time = 1:4), plots = FALSE)
  table <- descriptivesOrderingTable(result)
  expect_equal(table$valid, 0)
  expect_equal(table$missing, 4)
  for (name in c("mean", "sd", "var", "range", "min", "max", "start", "end", "ar"))
    expectUnavailableDescriptivesCell(table[[name]])

  result <- descriptivesOrderingRun(data.frame(y = c(2, NA, 7, NA, 11, NA), Time = 1:6), plots = FALSE)
  table <- descriptivesOrderingTable(result)
  expect_equal(table$valid, 3)
  expect_equal(table$missing, 3)
  expect_equal(table$mean, mean(c(2, 7, 11)))
  expect_equal(table$start, 2)
  expect_equal(table$end, 11)
  expectUnavailableDescriptivesCell(table$ar)
})


test_that("Descriptives reports accepted date conventions even when no sorting is needed", {
  input <- data.frame(y = orderingY, Time = format(as.Date("2020-01-01") + 0:11))
  actual <- descriptivesOrderingRun(input, plots = FALSE)
  reference <- descriptivesOrderingRun(data.frame(y = orderingY), time = FALSE, plots = FALSE)
  expect_equal(descriptivesOrderingTable(actual), descriptivesOrderingTable(reference), tolerance = 1e-12)
  expect_null(actual$results$timeSortingNote)
  expect_match(descriptivesOrderingNote(actual, "timeInterpretationNote"), "YYYY-MM-DD", fixed = TRUE)
  expect_match(descriptivesOrderingNote(actual, "timeInterpretationNote"), "10 January 2020", fixed = TRUE)
  expect_null(reference$results$timeInterpretationNote)
})
