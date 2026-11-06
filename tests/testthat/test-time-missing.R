timeMissingSeries <- getFromNamespace(".tsCompleteSeries", "jaspDescriptives")

test_that("complete series are identical under every missing-observation policy", {
  y <- c(4, 2, 8, 1, 9, 6)
  for (method in c("omit", "error", "trim", "longest")) {
    actual <- timeMissingSeries(y, method)
    expect_identical(actual$y, y)
    expect_identical(actual$indices, seq_along(y))
    expect_null(actual$error)
    expect_null(actual$note)
  }
  expect_identical(timeMissingSeries(y, NULL), timeMissingSeries(y, "omit"))
})

test_that("strict handling rejects all missing positions without compressing gaps", {
  for (y in list(c(NA, 1, 4), c(1, NA, 4), c(1, 4, NA), c(1, NaN, 4))) {
    original <- y
    actual <- timeMissingSeries(y, "error")
    expect_identical(actual$error,
      "The series used for this calculation contains missing observations. Choose Omit all missing observations or Use the longest complete segment under Missing observations.")
    expect_length(actual$y, 0)
    expect_null(actual$note)
    expect_identical(y, original)
  }
})

test_that("legacy end trimming retains every internal step and reports the calculation range", {
  y <- c(NA, NA, 7, 3, 9, 2, NA)
  actual <- timeMissingSeries(y, "trim")
  expect_identical(actual$y, c(7, 3, 9, 2))
  expect_identical(actual$indices, 3:6)
  expect_null(actual$error)
  expect_match(actual$note, "4 of 7 observation steps: steps 3 to 6", fixed = TRUE)
  for (y in list(c(NA, 7, NA, 9, NA), c(7, NA, 9))) {
    actual <- timeMissingSeries(y, "trim")
    expect_match(actual$error, "remain inside", fixed = TRUE)
    expect_length(actual$y, 0)
    expect_null(actual$note)
  }
})

test_that("the longest contiguous segment is explicit and earliest ties win", {
  y <- c(NA, 5, 2, 8, NA, 7, 4, 1, NA, 9)
  actual <- timeMissingSeries(y, "longest")
  expect_identical(actual$indices, 2:4)
  expect_identical(actual$y, c(5, 2, 8))
  expect_match(actual$note, "3 of 10 observation steps: steps 2 to 4", fixed = TRUE)
  y <- c(5, 2, NA, 8, 1, 7, 4, NA)
  actual <- timeMissingSeries(y, "longest")
  expect_identical(actual$indices, 4:7)
  expect_identical(actual$y, c(8, 1, 7, 4))
  expect_match(actual$note, "4 of 8 observation steps: steps 4 to 7", fixed = TRUE)
})

test_that("unusable segments and infinite values have controlled errors", {
  for (method in c("omit", "error", "trim", "longest")) {
    for (y in list(numeric(), c(NA_real_, NaN)))
      expect_match(timeMissingSeries(y, method)$error, "no observed values", fixed = TRUE)
    for (y in list(c(1, Inf, 4), c(-Inf, 1, 4)))
      expect_match(timeMissingSeries(y, method)$error, "infinite values", fixed = TRUE)
    expect_match(timeMissingSeries(4, method)$error, "fewer than two", fixed = TRUE)
    expect_match(timeMissingSeries(rep(4, 3), method)$error, "no variation", fixed = TRUE)
  }
  expect_match(timeMissingSeries(c(1, NA, -1, NA, 1), "longest")$error,
               "fewer than two", fixed = TRUE)
  # A constant longest segment must not silently be replaced by a shorter one.
  expect_match(timeMissingSeries(c(4, 4, 4, NA, 1, 2), "longest")$error,
               "no variation", fixed = TRUE)
  expect_error(timeMissingSeries(1:4, "unknown"), "Unknown missing-observation")
})

test_that("calculation selection leaves row identity and model input untouched", {
  dataset <- data.frame(y = c(NA, 7, 3, NA, 9, 2, 8, NA),
                        t = seq(2001.5, by = .1, length.out = 8),
                        covariate = seq_len(8), row.names = as.character(seq(2, 16, 2)))
  original <- dataset
  selected <- timeMissingSeries(dataset$y, "longest")
  expect_identical(selected$indices, 5:7)
  expect_identical(dataset, original)
  expect_identical(dataset$t, original$t)
  expect_identical(rownames(dataset), original |> rownames())
})


test_that("omission is the default and joins only the selected calculation input", {
  for (y in list(c(NA, 3, 1, 5, NA), c(3, NA, 1, NaN, 5),
                 c(NA, 3, NA, 1, 5, NA))) {
    original <- y
    expectedIndices <- which(!is.na(y))
    actual <- timeMissingSeries(y)
    expect_identical(actual, timeMissingSeries(y, "omit"))
    expect_identical(actual, timeMissingSeries(y, NULL))
    expect_identical(actual$indices, expectedIndices)
    expect_identical(actual$y, y[expectedIndices])
    expect_null(actual$error)
    expect_identical(actual$note, sprintf(
      "Missing observations were omitted for this calculation (%s of %s observations retained), so the remaining observations are treated as consecutive time steps and any internal gaps are ignored.",
      length(expectedIndices), length(y)))
    expect_false(grepl("steps 1 to", actual$note, fixed = TRUE))
    expect_identical(y, original)
  }
})

test_that("omission validates the remaining observations without relaxing other policies", {
  expect_match(timeMissingSeries(c(NA, 4, NA), "omit")$error,
               "fewer than two", fixed = TRUE)
  expect_match(timeMissingSeries(c(4, NA, 4), "omit")$error,
               "no variation", fixed = TRUE)
  expect_null(timeMissingSeries(c(NA, 4, NA), "omit")$note)
  expect_null(timeMissingSeries(c(4, NA, 4), "omit")$note)
  y <- c(1, NA, NA, -1, NA, 1, NA, -1, NA, NA)
  actual <- timeMissingSeries(y, "omit")
  expect_identical(actual$indices, c(1L, 4L, 6L, 8L))
  expect_identical(actual$y, c(1, -1, 1, -1))
  expect_match(timeMissingSeries(y, "error")$error, "contains missing", fixed = TRUE)
  expect_match(timeMissingSeries(y, "trim")$error, "remain inside", fixed = TRUE)
  expect_match(timeMissingSeries(y, "longest")$error, "fewer than two", fixed = TRUE)
})
