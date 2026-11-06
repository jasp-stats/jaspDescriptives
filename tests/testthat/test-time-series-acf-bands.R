# Capture the reference implementation's actual confidence lines rather than
# repeating Bartlett's formula in the expected values.
acfBandReference <- function(y, maxLag, ci) {
  ac <- stats::acf(y, plot = FALSE, lag.max = maxLag)
  reference <- getS3method("plot", "acf")
  lines <- list()
  scope <- new.env(parent = environment(reference))
  scope$lines <- function(x, y, ...) {
    lines[[length(lines) + 1L]] <<- data.frame(lag = as.numeric(x), bound = as.numeric(y))
  }
  environment(reference) <- scope

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  device <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(device)
    unlink(path)
  }, add = TRUE)
  reference(ac, ci = ci, ci.type = "ma")
  expect_length(lines, 2L)
  data.frame(lag = lines[[1L]]$lag, upper = lines[[1L]]$bound,
             lower = lines[[2L]]$bound)
}

# A plain R collector is sufficient for the plotting helper; no C++ result
# objects are constructed outside the supported analysis runner.
acfBandPlot <- function(y, type = "ACF", zeroLag = FALSE, maxLag = 3,
                        ci = TRUE, ciValue = .95, ciType = "movingAverage",
                        missingValues = "listwise") {
  collector <- new.env()
  collector$setError <- function(message) collector$error <- message
  note <- getFromNamespace(".tsFillACF", "jaspDescriptives")(
    collector, type, data.frame(y = y), list(), zeroLag = zeroLag,
    maxLag = maxLag, ci = ci, ciValue = ciValue, ciType = ciType,
    missingValues = missingValues
  )
  list(plot = collector$plotObject, error = collector$error, note = note)
}

acfBandLayer <- function(result, geom) {
  layers <- Filter(function(layer) inherits(layer$geom, geom), result$plot$layers)
  expect_length(layers, 1L)
  result <- layers[[1L]]$data
  rownames(result) <- NULL
  result
}

acfBandY <- c(2, 7, 3, 6, 4, 5, 8, 9, 11, 10, 15, 14, 17, 16, 13, 12)

test_that("moving-average bands agree with R's MA(k - 1) reference", {
  for (y in list(acfBandY, c(-8, 7, -6, 5, -4, 3, -2, 1, -3, 6, -9, 4))) {
    for (ci in c(.8, .95, .99)) {
      for (maxLag in c(1, 3, 7)) {
        result <- acfBandPlot(y, maxLag = maxLag, ciValue = ci)
        expect_null(result$error)
        expect_null(result$note)
        bands <- acfBandLayer(result, "GeomRibbon")
        expect_equal(bands[c("lag", "upper", "lower")],
                     acfBandReference(y, maxLag, ci), tolerance = 1e-12)
        correlations <- acfBandLayer(result, "GeomLinerange")
        reference <- stats::acf(y, plot = FALSE, lag.max = maxLag)
        expect_equal(correlations$acf, as.numeric(reference$acf)[-1L], tolerance = 1e-12)
      }
    }
  }
})

test_that("zero-lag display cannot alter positive-lag MA bands or correlations", {
  hidden <- acfBandPlot(acfBandY)
  shown <- acfBandPlot(acfBandY, zeroLag = TRUE)
  expect_equal(acfBandLayer(hidden, "GeomRibbon"), acfBandLayer(shown, "GeomRibbon"))
  expect_true(all(acfBandLayer(shown, "GeomRibbon")$lag > 0))
  shownCorrelations <- acfBandLayer(shown, "GeomLinerange")
  expect_equal(shownCorrelations$lag, 0:3)
  expect_equal(shownCorrelations$acf[1L], 1)
  expect_equal(shownCorrelations$acf[-1L], acfBandLayer(hidden, "GeomLinerange")$acf)
})

test_that("a single positive lag has the white-noise null band", {
  for (zeroLag in c(FALSE, TRUE)) {
    result <- acfBandPlot(acfBandY, zeroLag = zeroLag, maxLag = 1)
    bands <- acfBandLayer(result, "GeomRibbon")
    expect_equal(bands$lag, 1)
    expect_equal(bands$upper, stats::qnorm(.975) / sqrt(length(acfBandY)))
    expect_equal(bands$lower, -bands$upper)
  }
  empty <- getFromNamespace(".tsAcfBartlett", "jaspDescriptives")(numeric(), 16)
  expect_identical(empty, data.frame(r = numeric(), se = numeric()))
})

test_that("MA bands use the observation count selected by each current policy", {
  inputs <- list(listwise = c(NA, acfBandY[1:5], NA, acfBandY[6:16], NA),
                 longest = c(2, 8, NA, acfBandY, NA, 6, 3, 9))
  for (method in names(inputs)) {
    input <- inputs[[method]]
    original <- input
    for (ci in c(.8, .95)) {
      result <- acfBandPlot(input, ciValue = ci, missingValues = method)
      expect_null(result$error)
      expect_match(result$note, if (method == "listwise") "16 of 19 observations retained" else
                     "16 of 23 observation steps: steps 4 to 19", fixed = TRUE)
      bands <- acfBandLayer(result, "GeomRibbon")
      expect_equal(bands[c("lag", "upper", "lower")],
                   acfBandReference(acfBandY, 3, ci), tolerance = 1e-12)
      expect_equal(acfBandLayer(result, "GeomLinerange")$acf,
                   as.numeric(stats::acf(acfBandY, plot = FALSE, lag.max = 3)$acf)[-1L],
                   tolerance = 1e-12)
      expect_identical(input, original)
    }
  }
})

test_that("MA bands preserve validation of unusable observations under current policies", {
  cases <- list(
    list(y = c(NA_real_, NaN), error = "no observed values"),
    list(y = c(1, Inf, 4), error = "infinite values"),
    list(y = c(NA, 4, NA), error = "fewer than two"),
    list(y = c(NA, 4, 4, NA), error = "no variation")
  )
  for (method in c("listwise", "longest")) {
    for (case in cases) {
      result <- acfBandPlot(case$y, missingValues = method)
      expect_match(result$error, case$error, fixed = TRUE)
      expect_null(result$plot)
      expect_null(result$note)
    }
  }
  for (case in list(list(y = c(1, NA, 4, NA, 2), error = "fewer than two"),
                   list(y = c(4, 4, 4, NA, 1, 2), error = "no variation"))) {
    result <- acfBandPlot(case$y, missingValues = "longest")
    expect_match(result$error, case$error, fixed = TRUE)
    expect_null(result$plot)
    expect_null(result$note)
  }
})

test_that("white-noise and PACF bands keep their existing formulas", {
  input <- c(NA, acfBandY, NA, NA)
  for (ci in c(.8, .95, .99)) {
    for (type in c("ACF", "PACF")) {
      for (zeroLag in c(FALSE, TRUE)) {
        for (ciType in if (type == "ACF") "whiteNoise" else c("whiteNoise", "movingAverage")) {
          result <- acfBandPlot(input, type = type, zeroLag = zeroLag, ciValue = ci,
                                ciType = ciType, missingValues = "longest")
          expect_null(result$error)
          bands <- acfBandLayer(result, "GeomRibbon")
          bound <- stats::qnorm((1 + ci) / 2) / sqrt(length(acfBandY))
          expect_equal(bands$upper, rep(bound, nrow(bands)))
          expect_equal(bands$lower, -bands$upper)
          reference <- if (type == "ACF") stats::acf(acfBandY, plot = FALSE, lag.max = 3) else
            stats::pacf(acfBandY, plot = FALSE, lag.max = 3)
          expected <- as.numeric(reference$acf)
          if (type == "ACF" && !zeroLag) expected <- expected[-1L]
          expect_equal(acfBandLayer(result, "GeomLinerange")$acf, expected)
        }
      }
    }
  }
})

test_that("disabling confidence intervals leaves autocorrelations unchanged", {
  result <- acfBandPlot(acfBandY, ci = FALSE)
  expect_length(Filter(function(layer) inherits(layer$geom, "GeomRibbon"), result$plot$layers), 0L)
  expect_equal(acfBandLayer(result, "GeomLinerange")$acf,
               as.numeric(stats::acf(acfBandY, plot = FALSE, lag.max = 3)$acf)[-1L])
})


test_that("omitted observations use the retained sample size for ACF bands", {
  input <- c(NA, acfBandY[1:5], NA, acfBandY[6:16], NaN)
  for (type in c("ACF", "PACF")) {
    for (ciType in c("whiteNoise", "movingAverage")) {
      result <- acfBandPlot(input, type = type, ciType = ciType)
      reference <- acfBandPlot(acfBandY, type = type, ciType = ciType)
      expect_null(result$error)
      expect_equal(acfBandLayer(result, "GeomLinerange"),
                   acfBandLayer(reference, "GeomLinerange"))
      expect_equal(acfBandLayer(result, "GeomRibbon"),
                   acfBandLayer(reference, "GeomRibbon"))
      expect_match(result$note, "16 of 19 observations retained", fixed = TRUE)
      expect_match(result$note, "internal gaps are ignored", fixed = TRUE)
      expect_null(reference$note)
    }
  }
})
