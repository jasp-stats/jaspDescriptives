# Exercise the opt-in preparation contract without constructing native results.
prepareTime <- getFromNamespace(".tsPrepareData", "jaspDescriptives")
interpretTime <- getFromNamespace(".tsInterpretTime", "jaspDescriptives")

timePreparationOptions <- function(...) {
  utils::modifyList(list(
    dependent = "y", time = "Time", covariates = character(),
    filter = FALSE, filterBy = "row", rowStart = "", rowEnd = "",
    timeStart = "", timeEnd = "", dateStart = "", dateEnd = ""
  ), list(...))
}

timePreparationCollector <- function() {
  scope <- new.env(parent = environment(prepareTime))
  # Copy all preparation functions so private calls use the same collectors.
  # No loaded namespace is modified and no computation is replaced.
  for (name in c(".tsPrepareData", ".tsInterpretTime", ".tsFilterPreparedData")) {
    fn <- getFromNamespace(name, "jaspDescriptives")
    environment(fn) <- scope
    scope[[name]] <- fn
  }
  scope$createJaspHtml <- function(...) list(...)
  scope$.quitAnalysis <- function(message) stop(message, call. = FALSE)
  scope$.tsPrepareData
}

test_that("numeric Time orders complete observations and preserves spreadsheet identity", {
  for (time in list(c(2002, 2000, 2001), c(2001.8, 2001.5, 2001.6), c(2, -1, -.5))) {
    input <- data.frame(y = c(3, NA_real_, 2), Time = time,
                        x = c(30, 10, 20), z = c(-3, -1, -2),
                        row.names = c("3", "7", "12"))
    before <- input
    result <- prepareTime(NULL, input, timePreparationOptions(covariates = c("x", "z")), TRUE, TRUE)
    expect_named(result, c("raw", "data"))
    expect_identical(result$raw$y, c(NA_real_, 2, 3))
    expect_identical(result$raw$t, time[c(2, 3, 1)])
    expect_identical(result$raw$xreg1, c(10, 20, 30))
    expect_identical(result$raw$xreg2, c(-1, -2, -3))
    expect_identical(rownames(result$raw), c("7", "12", "3"))
    expect_identical(result$data, result$raw)
    expect_identical(input, before)
  }
})

test_that("numeric factor labels and native double precision determine order", {
  input <- data.frame(y = 1:3,
                      Time = factor(c("2002", "2000", "2001"), levels = c("2001", "2002", "2000")))
  result <- prepareTime(NULL, input, timePreparationOptions(), TRUE)
  expect_identical(result$data$y, c(2L, 3L, 1L))
  expect_identical(result$data$t, c(2000, 2001, 2002))
  input$Time <- c(" .5 ", "-1", "+2.5e2")
  result <- prepareTime(NULL, input, timePreparationOptions(), TRUE)
  expect_identical(result$data$y, c(2L, 1L, 3L))
  expect_identical(result$data$t, c(-1, .5, 250))

  input <- data.frame(y = 1:2, Time = c(1e16 + 2, 1e16))
  result <- prepareTime(NULL, input, timePreparationOptions(), TRUE)
  expect_identical(result$data$y, c(2L, 1L))
  expect_identical(result$data$t, c(1e16, 1e16 + 2))
  expect_identical(diff(result$data$t), 2)
  input$Time <- c(20200103, 20200101)
  expect_identical(prepareTime(NULL, input, timePreparationOptions(), TRUE)$data$t,
                   c(20200101, 20200103))
})

test_that("unassigned Time preserves input order, missing responses and row indices", {
  input <- data.frame(y = c(3, 1, NA_real_), Time = c("bad", NA, "worse"),
                      x = c(30, 10, 20), row.names = c("3", "7", "12"))
  options <- timePreparationOptions(time = "", covariates = "x")
  result <- prepareTime(NULL, input, options, TRUE)
  expect_identical(result$raw, data.frame(y = input$y, t = 1:3, row.names = rownames(input)))
  expect_identical(result$data, result$raw)
  options$filter <- TRUE
  options$rowStart <- 7
  options$rowEnd <- 12
  result <- prepareTime(NULL, input, options, TRUE)
  expect_identical(result$data$y, c(1, NA_real_))
  expect_identical(result$data$t, 2:3)
  expect_identical(rownames(result$data), c("7", "12"))
})

test_that("date ordering keeps irregular and month-end observations without adding rows", {
  cases <- list(
    list(input = c("2026-03-31", "2026-01-31", "2026-02-28"),
         expected = c("2026-01-31", "2026-02-28", "2026-03-31")),
    list(input = c("2020-1-5", "2020-1-1", "2020-1-2"),
         expected = c("2020-01-01", "2020-01-02", "2020-01-05")),
    list(input = c("2/1/2020", "12/31/2019", "1/15/2020"),
         expected = c("2019-12-31", "2020-01-15", "2020-02-01")),
    list(input = c("1/2/2020", "31/12/2019", "15/1/2020"),
         expected = c("2019-12-31", "2020-01-15", "2020-02-01"))
  )
  for (case in cases) {
    input <- data.frame(y = c(3, 1, 2), Time = case$input)
    result <- prepareTime(NULL, input, timePreparationOptions(), TRUE)
    expect_identical(result$data$y, c(1, 2, 3))
    expect_equal(result$data$t, as.POSIXct(case$expected, tz = "UTC"))
    expect_identical(nrow(result$data), 3L)
    expect_identical(result$raw, result$data)
  }
})

test_that("native dates and timestamps retain their instants and fractional precision", {
  input <- data.frame(y = 1:2, Time = as.Date(c("2020-01-03", "2020-01-01")))
  result <- prepareTime(NULL, input, timePreparationOptions(), TRUE)
  expect_identical(result$data$t, as.POSIXct(as.Date(c("2020-01-01", "2020-01-03")), tz = "UTC"))
  input$Time <- as.POSIXct(c("2020-01-03 13:14:15", "2020-01-01 12:13:14"), tz = "Europe/Amsterdam") + .25
  result <- prepareTime(NULL, input, timePreparationOptions(), TRUE)
  expect_identical(result$data$t, input$Time[c(2, 1)])
  expect_identical(result$data$y, c(2L, 1L))

  input <- data.frame(y = 1:3, Time = c("2020-01-02T01:00:00+01:00",
    "2020-01-01T21:00:00-0300", "2019-12-31T23:00:00.125Z"))
  result <- prepareTime(NULL, input, timePreparationOptions(), TRUE)
  expected <- as.POSIXct(c("2019-12-31 23:00:00", "2020-01-02 00:00:00", "2020-01-02 00:00:00"), tz = "UTC")
  expected[1] <- expected[1] + .125
  expect_identical(result$data$y, c(3L, 1L, 2L))
  expect_equal(as.numeric(result$data$t), as.numeric(expected), tolerance = 0)
})

test_that("unusable Time falls back for every row without losing responses or covariates", {
  cases <- list(
    c("France", "Germany", "Spain"), factor(c("France", "Germany", "Spain")),
    c(2002, NA_real_, 2001), c(2002, Inf, 2001), c("2", "1e309", "1"),
    c("2", "", "1"), c("2020-01-03", "wrong", "2020-01-01"),
    c("2020-01-03", "2020-02-30", "2020-01-01"),
    c("01-10.", "02-10.", "03-10."), c("01/02/2020", "02/03/2020", "03/04/2020"),
    c("15/1/2020", "1/15/2020", "2020-01-01"),
    c("2020-01-03T00:00Z", "2020-01-02T00:00", "2020-01-01T00:00Z"),
    c("2001.9", "2001.10", "2001.11")
  )
  for (time in cases) {
    input <- data.frame(y = c(3, NA_real_, 2), Time = time, x = c(30, 10, 20),
                        row.names = c("3", "7", "12"))
    result <- prepareTime(NULL, input, timePreparationOptions(covariates = "x"), TRUE, TRUE)
    expect_identical(result$raw$y, input$y)
    expect_identical(result$raw$xreg1, input$x)
    expect_identical(result$raw$t, 1:3)
    expect_identical(rownames(result$raw), rownames(input))
    expect_identical(result$data, result$raw)
  }
})

test_that("strict interpretation rejects incomplete dates and clocks without guessing", {
  invalid <- c("1900-02-29", "2020-02-30", "2020-13-01", "0000-01-01", "10000-01-01",
    "2020/01-01", "2020-01-01 suffix", "2020-01-01 12:", "2020-01-01T12:00:",
    "2020-01-01T12:00.5", "2020-01-01T24:00", "2020-01-01T12:60", "2020-01-01T12:00:60",
    "2020-01-01T00:00+24:00", "2020-01-01T00:00+01:60", "2020-01-01Z")
  for (value in invalid) {
    result <- interpretTime(c("2020-01-01", value))
    expect_identical(result$status, "invalid", info = value)
    expect_identical(result$affectedRows, 2L, info = value)
  }
  for (value in c("01/01/2020", "01/02/20", "01-10.")) {
    result <- interpretTime(c("2020-01-01", value))
    expect_identical(result$status, "ambiguous", info = value)
    expect_identical(result$affectedRows, 2L, info = value)
  }
  expect_identical(interpretTime(c("2000-02-29", "0001-01-01", "9999-12-31"))$status, "date")
  expect_identical(interpretTime(c("2001.1", "2001.10"))$reason, "yearMonth")
  expect_identical(interpretTime(c("2001.5", "2001.6", "2001.8"))$status, "numeric")
  expect_identical(interpretTime(c("2001.09", "2001.10", "2001.11"))$status, "numeric")
})

test_that("ties remain distinct observations in their original relative order", {
  prepare <- timePreparationCollector()
  results <- new.env(parent = emptyenv())
  input <- data.frame(y = c(3, 1, 2, 4), Time = c(2, 1, 2, 1),
                      row.names = c("3", "7", "12", "18"))
  result <- prepare(results, input, timePreparationOptions(), TRUE)
  expect_identical(result$data$y, c(1, 4, 3, 2))
  expect_identical(rownames(result$data), c("7", "18", "3", "12"))
  expect_identical(result$data$t, c(1, 1, 2, 2))
  expect_match(results$timeSortingNote$text, "sorted by Time", fixed = TRUE)
  expect_match(results$timeTiesNote$text, "successive steps", fixed = TRUE)
  expect_identical(results$timeTiesNote$dependencies, c("dependent", "time"))
  expect_null(results$timeWarning)
})

test_that("row filters use spreadsheet identities and raw retains future observations", {
  input <- data.frame(y = c(30, 10, 40, 20), Time = c(3, 1, 4, 2), x = c(300, 100, 400, 200),
                      row.names = c("3", "7", "12", "18"))
  options <- timePreparationOptions(covariates = "x", filter = TRUE, rowStart = 7, rowEnd = 18)
  result <- prepareTime(NULL, input, options, TRUE, TRUE)
  expect_identical(rownames(result$raw), c("7", "18", "3", "12"))
  expect_identical(result$raw$xreg1, c(100, 200, 300, 400))
  expect_identical(rownames(result$data), c("7", "18", "12"))
  expect_identical(result$data$y, c(10, 20, 40))
  options$filterBy <- "time"
  options$timeStart <- 1
  options$timeEnd <- 2
  filtered <- prepareTime(NULL, input, options, TRUE, TRUE)
  expect_identical(filtered$raw, result$raw)
  expect_identical(rownames(filtered$data), c("7", "18"))
  expect_identical(filtered$data$xreg1, c(100, 200))
  # These remain available after the final modeled row for future covariates.
  expect_identical(filtered$raw$xreg1[3:4], c(300, 400))
})

test_that("numeric filters are inclusive value ranges with decimal, negative and tied Time", {
  for (times in list(c(2001.8, 2001.5, 2001.6, 2001.6), c(-1.2, -1.5, -1.4, -1.4))) {
    input <- data.frame(y = c(4, 1, 2, 3), Time = times)
    options <- timePreparationOptions(filter = TRUE, filterBy = "time", timeStart = times[2], timeEnd = times[3])
    result <- prepareTime(NULL, input, options, TRUE)
    expect_identical(result$data$y, c(1, 2, 3))
    expect_identical(nrow(result$raw), 4L)
    options$timeStart <- times[3]
    expect_identical(prepareTime(NULL, input, options, TRUE)$data$y, c(2, 3))
    options$timeStart <- ""
    options$timeEnd <- ""
    result <- prepareTime(NULL, input, options, TRUE)
    expect_identical(result$data, result$raw)
  }
})

test_that("date filters include a complete UTC End day and retain exact timestamp boundaries", {
  midnight <- as.POSIXct("2020-01-01", tz = "UTC")
  input <- data.frame(y = 1:4, Time = midnight + c(172800, 86400, 172799.999, 129600), x = 11:14)
  options <- timePreparationOptions(covariates = "x", filter = TRUE, filterBy = "date",
                                    dateStart = "2020-01-02", dateEnd = "2020-01-02")
  result <- prepareTime(NULL, input, options, TRUE, TRUE)
  expect_identical(result$data$y, c(2L, 4L, 3L))
  expect_identical(result$data$xreg1, c(12L, 14L, 13L))
  expect_identical(result$raw$y, c(2L, 4L, 3L, 1L))
  options$dateEnd <- "2020-01-02T12:00Z"
  expect_identical(prepareTime(NULL, input, options, TRUE)$data$y, c(2L, 4L))
  options$dateStart <- options$dateEnd
  expect_identical(prepareTime(NULL, input, options, TRUE)$data$y, 4L)
  options$dateStart <- "2020-01-02T13:00+01:00"
  expect_identical(prepareTime(NULL, input, options, TRUE)$data$y, 4L)
  for (day in c("2020-01-31", "2020-02-29", "2020-12-31")) {
    input <- data.frame(y = 1:3, Time = as.POSIXct(day, tz = "UTC") + c(0, 86399.999, 86400))
    options$dateStart <- day
    options$dateEnd <- day
    expect_identical(prepareTime(NULL, input, options, TRUE)$data$y, 1:2)
  }
})

test_that("invalid bounds and mismatched Time filter types fail informatively", {
  prepare <- timePreparationCollector()
  input <- data.frame(y = 1:3, Time = c(-1, 0, 2))
  options <- timePreparationOptions(filter = TRUE, filterBy = "time")
  for (start in list(Inf, NA_real_, "bad", c(-1, 0))) {
    options$timeStart <- start
    expect_error(prepare(NULL, input, options, TRUE), "finite values")
  }
  options$timeStart <- ""
  for (end in list(NA_real_, c(0, 2))) {
    options$timeEnd <- end
    expect_error(prepare(NULL, input, options, TRUE), "finite values")
  }
  options$timeStart <- 2
  options$timeEnd <- -1
  expect_error(prepare(NULL, input, options, TRUE), "larger/later")
  options$timeStart <- 5
  options$timeEnd <- 8
  expect_error(prepare(NULL, input, options, TRUE), "No observations have Time values between")
  options <- timePreparationOptions(filter = TRUE, filterBy = "date")
  expect_error(prepare(NULL, input, options, TRUE), "Assign a date variable to Time")
  input$Time <- c("2020-01-01", "2020-01-02", "2020-01-03")
  result <- prepare(NULL, input, options, TRUE)
  expect_identical(result$data, result$raw)
  for (bound in c("dateStart", "dateEnd")) {
    vectorOptions <- options
    vectorOptions[[bound]] <- c("2020-01-01", "2020-01-02")
    expect_error(prepare(NULL, input, vectorOptions, TRUE), "year-month-day order|finite values")
  }
  options$dateStart <- "01/02/2020"
  expect_error(prepare(NULL, input, options, TRUE), "year-month-day order")
  options$dateStart <- ""
  options$dateEnd <- "2020-02-30"
  expect_error(prepare(NULL, input, options, TRUE), "year-month-day order")
  options$dateEnd <- NA_character_
  expect_error(prepare(NULL, input, options, TRUE), "year-month-day order")
  options <- timePreparationOptions(filter = TRUE, filterBy = "time")
  expect_error(prepare(NULL, input, options, TRUE), "Choose Date to filter it")
  options <- timePreparationOptions(filter = TRUE, rowStart = 20, rowEnd = 30)
  expect_error(prepare(NULL, input, options, TRUE), "contains no observations")
  options$rowStart <- NA_real_
  expect_error(prepare(NULL, input, options, TRUE), "finite values")
  options$rowStart <- ""
  options$rowEnd <- NA_real_
  expect_error(prepare(NULL, input, options, TRUE), "finite values")
})

test_that("fallback or unassigned Time cannot filter fabricated time values", {
  prepare <- timePreparationCollector()
  input <- data.frame(y = 1:3, Time = c("France", "Germany", "Spain"))
  for (time in c("Time", "")) {
    for (filter in c("time", "date")) {
      options <- timePreparationOptions(time = time, filter = TRUE, filterBy = filter)
      expect_error(prepare(NULL, input, options, TRUE), "requires a usable numeric or date variable")
    }
    options <- timePreparationOptions(time = time, filter = TRUE, rowStart = 2, rowEnd = 3)
    expect_identical(prepare(NULL, input, options, TRUE)$data$y, 2:3)
  }
})

test_that("row-filter warnings describe gaps in observation order and clear on later runs", {
  prepare <- timePreparationCollector()
  results <- new.env(parent = emptyenv())
  input <- data.frame(y = 1:6, Time = c(1, 4, 2, 5, 3, 6), x = 11:16)
  options <- timePreparationOptions(covariates = "x", filter = TRUE, rowStart = 1, rowEnd = 3)
  result <- prepare(results, input, options, TRUE, TRUE)
  expect_identical(result$data$y, c(1L, 3L, 2L))
  expect_identical(result$data$xreg1, c(11L, 13L, 12L))
  expect_identical(result$data$t, c(1, 2, 4))
  expect_match(results$timeFilterWarning$text, "skip observations", fixed = TRUE)
  expect_match(results$timeFilterWarning$text, "successive steps", fixed = TRUE)
  expect_identical(results$timeFilterWarning$dependencies,
                   c("dependent", "time", "filter", "filterBy", "rowStart", "rowEnd"))
  options$rowEnd <- 1
  expect_identical(prepare(results, input, options, TRUE)$data$y, 1L)
  expect_null(results$timeFilterWarning)
  options$rowEnd <- 3
  prepare(results, input, options, TRUE)
  options$filter <- FALSE
  result <- prepare(results, input, options, TRUE)
  expect_null(results$timeFilterWarning)
  expect_identical(result$data, result$raw)
  options$filter <- TRUE
  options$time <- ""
  expect_identical(prepare(results, input, options, TRUE)$data$y, 1:3)
  expect_null(results$timeFilterWarning)
})

test_that("fallback notes distinguish causes without listing a whole unsuitable column", {
  prepare <- timePreparationCollector()
  results <- new.env(parent = emptyenv())
  cases <- list(
    list(c("France", "Spain"), "cannot be interpreted as numbers or dates"),
    list(c("2020-1-1", NA_character_), "missing or non-finite"),
    list(c("15/1/2020", "1/15/2020"), "inconsistent day/month ordering"),
    list(c("1/2/2020", "2/3/2020"), "ambiguous"),
    list(c("2001.1", "2001.10"), "conflicting orders or ties"))
  for (case in cases) {
    input <- data.frame(y = 1:2, Time = case[[1]], row.names = c("7", "19"))
    prepare(results, input, timePreparationOptions(), TRUE)
    expect_match(results$timeWarning$text, case[[2]], fixed = TRUE)
    expect_match(results$timeWarning$text, "Time was ignored", fixed = TRUE)
    expect_match(results$timeWarning$text, "the analysis used spreadsheet row order", fixed = TRUE)
    if (anyNA(case[[1]]))
      expect_match(results$timeWarning$text, "Affected spreadsheet rows (first few): 19.", fixed = TRUE)
    else
      expect_false(grepl("Affected spreadsheet rows", results$timeWarning$text, fixed = TRUE))
    expect_identical(results$timeWarning$dependencies, c("dependent", "time"))
    expect_null(results$timeInterpretationNote)
  }
  for (time in list(rep("bad", 7), rep("01-10.", 7))) {
    input <- data.frame(y = 1:7, Time = time, row.names = as.character(seq(10, 70, 10)))
    prepare(results, input, timePreparationOptions(), TRUE)
    expect_false(grepl("Affected spreadsheet rows", results$timeWarning$text, fixed = TRUE))
  }
})

test_that("partially unusable Time retains bounded original row examples", {
  prepare <- timePreparationCollector()
  results <- new.env(parent = emptyenv())
  for (bad in c("wrong", "01-10.")) {
    input <- data.frame(y = 1:8, Time = c(rep(bad, 7), "2020-01-01"),
                        row.names = as.character(seq(10, 80, 10)))
    result <- prepare(results, input, timePreparationOptions(), TRUE)
    expect_match(results$timeWarning$text,
      "Affected spreadsheet rows (first few): 10, 20, 30, 40, 50.", fixed = TRUE)
    expect_false(grepl("50, 60", results$timeWarning$text, fixed = TRUE))
    expect_identical(result$data$y, input$y)
    expect_identical(rownames(result$data), rownames(input))
  }
})

test_that("changing Time replaces notes and inactive or empty analyses clear all notes", {
  prepare <- timePreparationCollector()
  results <- new.env(parent = emptyenv())
  input <- data.frame(y = 1:3, Time = c(2, 1, 1))
  options <- timePreparationOptions()
  prepare(results, input, options, TRUE)
  expect_false(is.null(results$timeSortingNote))
  expect_false(is.null(results$timeTiesNote))
  input$Time <- c("01-10.", "02-10.", "03-10.")
  prepare(results, input, options, TRUE)
  expect_match(results$timeWarning$text, "ambiguous", fixed = TRUE)
  expect_null(results$timeSortingNote)
  expect_null(results$timeTiesNote)
  input$Time <- 1:3
  prepare(results, input, options, TRUE)
  expect_null(results$timeWarning)
  expect_null(results$timeSortingNote)
  options$time <- ""
  expect_identical(prepare(results, input, options, TRUE)$data$y, 1:3)

  noteNames <- c("timeSortingNote", "timeWarning", "timeTiesNote", "timeFilterWarning",
                 "timeInterpretationNote")
  cases <- list(
    list(data = input, options = timePreparationOptions(), ready = FALSE),
    list(data = input, options = timePreparationOptions(dependent = ""), ready = TRUE),
    list(data = input[FALSE, ], options = timePreparationOptions(filter = TRUE), ready = TRUE))
  for (case in cases) {
    for (name in noteNames) results[[name]] <- list(text = "stale")
    result <- prepare(results, case$data, case$options, case$ready)
    expect_identical(result, list(raw = data.frame(), data = data.frame()))
    for (name in noteNames) expect_null(results[[name]])
  }
})


test_that("accepted text dates disclose their actual year, month and day conventions", {
  cases <- list(
    list(time = c("2020-01-10", "2020-01-13"), conventions = "ymd", labels = "YYYY-MM-DD"),
    list(time = c("2020/01/10", "2020.01.13"), conventions = "ymd", labels = "YYYY-MM-DD"),
    list(time = c("10/01/2020", "13/01/2020"), conventions = "dmy", labels = "DD-MM-YYYY"),
    list(time = c("01/10/2020", "01/13/2020"), conventions = "mdy", labels = "MM-DD-YYYY"),
    list(time = c("2020-01-10", "13/01/2020"), conventions = c("ymd", "dmy"),
         labels = c("YYYY-MM-DD", "DD-MM-YYYY")),
    list(time = c("2020-01-10", "01/13/2020"), conventions = c("ymd", "mdy"),
         labels = c("YYYY-MM-DD", "MM-DD-YYYY")),
    list(time = c("2020-01-10T12:00Z", "2020-01-13T13:00+01:00"),
         conventions = "ymd", labels = "YYYY-MM-DD"))
  prepare <- timePreparationCollector()
  results <- new.env(parent = emptyenv())
  for (case in cases) {
    interpreted <- interpretTime(case$time)
    expect_identical(interpreted$status, "date")
    expect_identical(interpreted$dateConventions, case$conventions)
    expect_identical(format(interpreted$t, "%Y-%m-%d", tz = "UTC"), c("2020-01-10", "2020-01-13"))
    input <- data.frame(y = 1:2, Time = case$time)
    actual <- prepare(results, input, timePreparationOptions(), TRUE)
    expect_identical(actual$data$y, input$y)
    expect_null(results$timeSortingNote)
    expect_null(results$timeWarning)
    expect_identical(results$timeInterpretationNote$dependencies, c("dependent", "time"))
    for (label in case$labels)
      expect_match(results$timeInterpretationNote$text, label, fixed = TRUE)
    expect_match(results$timeInterpretationNote$text, "10 January 2020", fixed = TRUE)
    for (label in setdiff(c("YYYY-MM-DD", "DD-MM-YYYY", "MM-DD-YYYY"), case$labels))
      expect_false(grepl(label, results$timeInterpretationNote$text, fixed = TRUE))
    if (length(case$conventions) > 1L)
      expect_match(results$timeInterpretationNote$text, "these date orders", fixed = TRUE)
  }

  input <- data.frame(y = c(2, 1), Time = c("2020-01-13", "2020-01-10"))
  actual <- prepare(results, input, timePreparationOptions(), TRUE)
  expect_identical(actual$data$y, c(1, 2))
  expect_match(results$timeSortingNote$text, "sorted by Time", fixed = TRUE)
  expect_match(results$timeInterpretationNote$text, "YYYY-MM-DD", fixed = TRUE)
})

test_that("typed dates and non-date inputs do not claim an inferred text convention", {
  prepare <- timePreparationCollector()
  results <- new.env(parent = emptyenv())
  times <- list(as.Date(c("2020-01-10", "2020-01-13")),
    as.POSIXct(c("2020-01-10 12:13:14", "2020-01-13 12:13:14"), tz = "Europe/Amsterdam"),
    c(1, 2), c("1", "2"), c("France", "Spain"), c("01-10.", "02-10."))
  for (time in times) {
    expect_null(interpretTime(time)$dateConventions)
    results$timeInterpretationNote <- list(text = "stale convention")
    input <- data.frame(y = 1:2, Time = time)
    prepare(results, input, timePreparationOptions(), TRUE)
    expect_null(results$timeInterpretationNote)
  }
  input <- data.frame(y = 1:2, Time = c("2020-01-10", "2020-01-13"))
  prepare(results, input, timePreparationOptions(), TRUE)
  expect_false(is.null(results$timeInterpretationNote))
  prepare(results, input, timePreparationOptions(time = ""), TRUE)
  expect_null(results$timeInterpretationNote)
})

test_that("an empty numeric Time filter reports requested and available values", {
  prepare <- timePreparationCollector()
  input <- data.frame(y = 1:3, Time = c(2000.1, 2000.4, 2000.9))
  options <- timePreparationOptions(filter = TRUE, filterBy = "time", timeStart = 1, timeEnd = 20)
  message <- tryCatch(prepare(NULL, input, options, TRUE), error = conditionMessage)
  expect_type(message, "character")
  expect_match(message, "No observations have Time values between", fixed = TRUE)
  for (value in c("1", "20", "2000.1", "2000.9")) expect_match(message, value, fixed = TRUE)
  expect_match(message, "Row number", fixed = TRUE)
  options$timeStart <- 2000.2
  options$timeEnd <- 2000.3
  # Bounds within the column range can still contain no measurements.
  expect_error(prepare(NULL, input, options, TRUE), "No observations have Time values between")
})


test_that("an empty date filter reports the available date range in UTC", {
  prepare <- timePreparationCollector()
  input <- data.frame(y = 1:2, Time = c("2020-01-10T23:30-0200", "2020-01-13T13:00+0100"))
  options <- timePreparationOptions(filter = TRUE, filterBy = "date",
                                    dateStart = "2020-01-12", dateEnd = "2020-01-12")
  message <- tryCatch(prepare(NULL, input, options, TRUE), error = conditionMessage)
  expect_type(message, "character")
  expect_match(message, "No observations have dates within the selected range", fixed = TRUE)
  expect_match(message, "2020-01-11 01:30:00", fixed = TRUE)
  expect_match(message, "2020-01-13 12:00:00", fixed = TRUE)
  expect_match(message, "(UTC)", fixed = TRUE)
  expect_match(message, "Row number", fixed = TRUE)
})
