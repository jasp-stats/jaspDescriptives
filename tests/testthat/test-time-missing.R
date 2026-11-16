timeMissingSeries <- getFromNamespace(".tsCompleteSeries", "jaspDescriptives")

test_that("complete series are identical under every missing-observation policy", {
  y <- c(4, 2, 8, 1, 9, 6)
  for (method in c("listwise", "longest")) {
    actual <- timeMissingSeries(y, method)
    expect_identical(actual$y, y)
    expect_identical(actual$indices, seq_along(y))
    expect_null(actual$error)
    expect_null(actual$note)
  }
  expect_identical(timeMissingSeries(y, NULL), timeMissingSeries(y, "listwise"))
})

test_that("current policies select observations at every missing position without changing input", {
  cases <- list(
    list(y = c(NA, 1, 4, 2), longest = 2:4),
    list(y = c(1, NA, 4, 2), longest = 3:4),
    list(y = c(1, 4, 2, NA), longest = 1:3),
    list(y = c(1, NaN, 4, 2), longest = 3:4)
  )
  for (case in cases) {
    original <- case$y
    for (method in c("listwise", "longest")) {
      actual <- timeMissingSeries(case$y, method)
      indices <- if (method == "listwise") which(!is.na(case$y)) else case$longest
      expect_identical(actual$indices, indices)
      expect_identical(actual$y, case$y[indices])
      expect_null(actual$error)
      expect_type(actual$note, "character")
      expect_identical(case$y, original)
    }
  }
})

test_that("current policies agree on complete observations between missing ends", {
  y <- c(NA, NA, 7, 3, 9, 2, NA)
  for (method in c("listwise", "longest")) {
    actual <- timeMissingSeries(y, method)
    expect_identical(actual$y, c(7, 3, 9, 2))
    expect_identical(actual$indices, 3:6)
    expect_null(actual$error)
    if (method == "listwise")
      expect_match(actual$note, "4 of 7 observations retained", fixed = TRUE)
    else
      expect_match(actual$note, "4 of 7 observation steps: steps 3 to 6", fixed = TRUE)
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
  for (method in c("listwise", "longest")) {
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
})

test_that("removed and unknown missing-observation methods are rejected", {
  for (method in c("error", "trim", "unknown")) {
    for (y in list(1:4, c(1, NA, 4, 2)))
      expect_error(timeMissingSeries(y, method), "Unknown missing-observation handling option.", fixed = TRUE)
  }
})

test_that("calculation selection leaves row identity and model input untouched", {
  dataset <- data.frame(y = c(NA, 7, 3, NA, 9, 2, 8, NA),
                        t = seq(2001.5, by = .1, length.out = 8),
                        covariate = seq_len(8), row.names = as.character(seq(2, 16, 2)))
  original <- dataset
  for (method in c("listwise", "longest")) {
    selected <- timeMissingSeries(dataset$y, method)
    indices <- if (method == "listwise") c(2L, 3L, 5L, 6L, 7L) else 5:7
    expect_identical(selected$indices, indices)
    expect_identical(selected$y, dataset$y[indices])
    expect_identical(dataset, original)
    expect_identical(dataset$t, original$t)
    expect_identical(rownames(dataset), original |> rownames())
  }
})

test_that("omission is the default and joins only the selected calculation input", {
  for (y in list(c(NA, 3, 1, 5, NA), c(3, NA, 1, NaN, 5),
                 c(NA, 3, NA, 1, 5, NA))) {
    original <- y
    expectedIndices <- which(!is.na(y))
    actual <- timeMissingSeries(y)
    expect_identical(actual, timeMissingSeries(y, "listwise"))
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

test_that("omission validates retained observations and longest keeps isolated observations separate", {
  expect_match(timeMissingSeries(c(NA, 4, NA), "listwise")$error,
               "fewer than two", fixed = TRUE)
  expect_match(timeMissingSeries(c(4, NA, 4), "listwise")$error,
               "no variation", fixed = TRUE)
  expect_null(timeMissingSeries(c(NA, 4, NA), "listwise")$note)
  expect_null(timeMissingSeries(c(4, NA, 4), "listwise")$note)
  y <- c(1, NA, NA, -1, NA, 1, NA, -1, NA, NA)
  actual <- timeMissingSeries(y, "listwise")
  expect_identical(actual$indices, c(1L, 4L, 6L, 8L))
  expect_identical(actual$y, c(1, -1, 1, -1))
  expect_match(timeMissingSeries(y, "longest")$error, "fewer than two", fixed = TRUE)
})
