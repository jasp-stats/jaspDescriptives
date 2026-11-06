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
  # runAnalysis encodes options and dataset through the bridge like the engine does
  result <- jaspTools::runAnalysis("DescriptivesTimeSeries", dataset = input,
                                  options = jaspTools:::fixOptionsForVariableTypes(options), view = FALSE)
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

expectDescriptivesOrderedLike <- function(actual, reference, y, correlationY = y) {
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
  expectedAcf <- stats::acf(correlationY, lag.max = 3, plot = FALSE)
  expectedPacf <- stats::pacf(correlationY, lag.max = 3, plot = FALSE)
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
  actual <- descriptivesOrderingRun(input, acfMissingValues = "longest", pacfMissingValues = "longest")
  reference <- descriptivesOrderingRun(data.frame(y = y), time = FALSE,
                                      acfMissingValues = "longest", pacfMissingValues = "longest")
  expectDescriptivesOrderedLike(actual, reference, y, correlationY = y[6:11])
  expect_equal(descriptivesOrderingTable(actual)$missing, 3)
})

test_that("Descriptives gives local correlation errors and independent missing-data choices", {
  y <- c(NA, orderingY, NA)
  input <- data.frame(y = y, Time = seq_along(y))
  strict <- descriptivesOrderingRun(input, acfMissingValues = "error", pacfMissingValues = "error")
  for (name in c("acfPlot", "pacfPlot"))
    expect_match(paste(unlist(strict$results[[name]]), collapse = " "),
                 "contains missing observations", fixed = TRUE)
  expect_equal(descriptivesOrderingTable(strict)$missing, 2)
  expect_null(strict$results$acfMissingNote)
  expect_null(strict$results$pacfMissingNote)

  mixed <- descriptivesOrderingRun(input, acfMissingValues = "trim", pacfMissingValues = "error")
  expect_match(paste(unlist(mixed$results$pacfPlot), collapse = " "),
               "contains missing observations", fixed = TRUE)
  expect_match(descriptivesOrderingNote(mixed, "acfMissingNote"),
               "12 of 14 observation steps: steps 2 to 13", fixed = TRUE)
  expect_null(mixed$results$pacfMissingNote)
  expect_equal(descriptivesOrderingTable(mixed), descriptivesOrderingTable(strict))
  trimmed <- descriptivesOrderingRun(input, acfMissingValues = "trim", pacfMissingValues = "trim")
  reference <- descriptivesOrderingRun(data.frame(y = orderingY), time = FALSE)
  actualPlots <- descriptivesOrderingPlotData(trimmed)
  referencePlots <- descriptivesOrderingPlotData(reference)
  expect_equal(actualPlots$acf, referencePlots$acf, tolerance = 1e-12)
  expect_equal(actualPlots$pacf, referencePlots$pacf, tolerance = 1e-12)

  # Differing choices remain independent when gaps are internal too.
  input$y[6] <- NA_real_
  internal <- descriptivesOrderingRun(input, acfMissingValues = "trim", pacfMissingValues = "longest")
  expect_match(paste(unlist(internal$results$acfPlot), collapse = " "), "remain inside", fixed = TRUE)
  expect_null(internal$results$acfMissingNote)
  expect_match(descriptivesOrderingNote(internal, "pacfMissingNote"),
               "7 of 14 observation steps: steps 7 to 13", fixed = TRUE)
  expected <- stats::pacf(input$y[7:13], lag.max = 3, plot = FALSE)
  plot <- descriptivesOrderingPlot(internal, "pacfPlot")
  layers <- lapply(plot$layers, function(layer) layer$data)
  values <- Filter(function(x) is.data.frame(x) && "acf" %in% names(x), layers)[[1L]]$acf
  expect_equal(as.numeric(values), as.numeric(expected$acf), tolerance = 1e-12)
})

test_that("non-omission policies do not join isolated observations", {
  input <- data.frame(y = c(1, NA, NA, -1, NA, 1, NA, -1, NA, NA), Time = seq_len(10))
  for (method in c("error", "trim", "longest")) {
    result <- descriptivesOrderingRun(input, lagPlot = FALSE,
                                      acfMissingValues = method, pacfMissingValues = method)
    expected <- switch(method, error = "contains missing observations",
                       trim = "remain inside", longest = "fewer than two")
    for (name in c("acfPlot", "pacfPlot"))
      expect_match(paste(unlist(result$results[[name]]), collapse = " "), expected, fixed = TRUE)
    expect_equal(descriptivesOrderingTable(result)$valid, 4)
    expect_null(result$results$acfMissingNote)
    expect_null(result$results$pacfMissingNote)
  }
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


test_that("default omission reports compression below correlations without changing other output", {
  y <- orderingY
  y[c(1L, 5L, 12L)] <- NA_real_
  input <- data.frame(y = y, Time = orderingNumericTime)[orderingPermutation, ]
  rownames(input) <- as.character(seq(2L, 24L, 2L))
  original <- input
  actual <- descriptivesOrderingRun(input)
  reference <- descriptivesOrderingRun(data.frame(y = y), time = FALSE,
    acfMissingValues = "omit", pacfMissingValues = "omit")
  expectDescriptivesOrderedLike(actual, reference, y, correlationY = y[!is.na(y)])
  expect_identical(input, original)
  outputNames <- vapply(actual$results$.meta, `[[`, "", "name")
  expect_true(all(c("acfPlot", "acfMissingNote", "pacfPlot", "pacfMissingNote") %in% outputNames))
  expect_equal(match("acfMissingNote", outputNames), match("acfPlot", outputNames) + 1L)
  expect_equal(match("pacfMissingNote", outputNames), match("pacfPlot", outputNames) + 1L)
  for (name in c("acfMissingNote", "pacfMissingNote")) {
    note <- descriptivesOrderingNote(actual, name)
    expect_match(note, "9 of 12 observations retained", fixed = TRUE)
    expect_match(note, "consecutive time steps", fixed = TRUE)
    expect_match(note, "internal gaps are ignored", fixed = TRUE)
  }
  complete <- descriptivesOrderingRun(data.frame(y = orderingY), time = FALSE)
  expect_null(complete$results$acfMissingNote)
  expect_null(complete$results$pacfMissingNote)
})

test_that("default omission and legacy trimming remain independent across ACF and PACF", {
  input <- data.frame(y = orderingY, Time = seq_along(orderingY))
  input$y[5L] <- NA_real_
  actual <- descriptivesOrderingRun(input, acfMissingValues = "trim")
  expect_match(paste(unlist(actual$results$acfPlot), collapse = " "),
               "remain inside", fixed = TRUE)
  expect_null(actual$results$acfMissingNote)
  expect_match(descriptivesOrderingNote(actual, "pacfMissingNote"),
               "11 of 12 observations retained", fixed = TRUE)
  plot <- descriptivesOrderingPlot(actual, "pacfPlot")
  layers <- lapply(plot$layers, function(layer) layer$data)
  values <- Filter(function(x) is.data.frame(x) && "acf" %in% names(x), layers)[[1L]]$acf
  expected <- stats::pacf(input$y[!is.na(input$y)], lag.max = 3L, plot = FALSE)
  expect_equal(as.numeric(values), as.numeric(expected$acf), tolerance = 1e-12)
})


test_that("wrapper and shared form agree on the omission default", {
  wrapper <- getFromNamespace("DescriptivesTimeSeries", "jaspDescriptives")
  qml <- system.file("qml", "common", "DescriptivesTimeSeriesForm.qml", package = "jaspDescriptives")
  options <- jaspTools:::readQML(qml)
  for (name in c("acfMissingValues", "pacfMissingValues")) {
    expect_identical(formals(wrapper)[[name]], "omit")
    expect_identical(options[[name]], "omit")
    # Only the two current choices are visible for new analyses; old development
    # choices stay representable and visible while explicitly selected.
    form <- readLines(qml, warn = FALSE)
    start <- grep(paste0("id: ", name, "Choice"), form, fixed = TRUE)
    expect_length(start, 1L)
    end <- start + which(trimws(form[seq.int(start + 1L, length(form))]) == "}")[1L]
    buttons <- grep("RadioButton {", form[seq.int(start, end)], value = TRUE, fixed = TRUE)
    expect_length(buttons, 4L)
    values <- sub('.*value: "([^"]+)".*', "\\1", buttons)
    expect_identical(values, c("omit", "longest", "trim", "error"))
    expect_false(any(grepl("visible:", buttons[1:2], fixed = TRUE)))
    expect_match(buttons[3L], paste0('visible: ', name, 'Choice.value === "trim"'), fixed = TRUE)
    expect_match(buttons[4L], paste0('visible: ', name, 'Choice.value === "error"'), fixed = TRUE)
  }
})
