context("Plot Builder interactions")

.plotBuilderTestPlot <- function(dataset, ...) {
  tab <- eval(formals(jaspPlotBuilder)$PlotBuilderTab)[[1]]
  for (name in names(tab)) {
    if (is.list(tab[[name]]) && "types" %in% names(tab[[name]]))
      tab[[name]] <- unlist(tab[[name]]$value)
  }
  tab <- utils::modifyList(tab, list(
    variableXPlotBuilder = "x", variableYPlotBuilder = "y",
    addDataPoint = TRUE, jitterwPlotBuilder = 0, jitterhPlotBuilder = 0,
    plotStyle = "ggplotgray"
  ))
  tab <- utils::modifyList(tab, list(...))
  data <- list(
    datasetNonRMList = setNames(list(dataset), tab$value),
    datasetRMList = setNames(list(dataset), tab$value)
  )
  .plotBuilderPlots(data, list(PlotBuilderTab = list(tab)))$updatedPlots[[tab$value]]
}

test_that("individual point appearance does not change summary groups", {
  dataset <- data.frame(
    x = factor(rep(c("a", "b"), each = 4)),
    y = rep(c(1, 3, 101, 103), 2),
    shape = factor(rep(c("one", "one", "two", "two"), 2))
  )
  plot <- .plotBuilderTestPlot(dataset, pointShapeVariable = "shape",
                               addMeanBar = TRUE, addMeanDot = TRUE)
  built <- ggplot2::ggplot_build(plot)
  expect_equal(built$data[[2]]$y, c(52, 52))
  expect_equal(built$data[[3]]$y, c(52, 52))
  expect_length(unique(built$data[[1]]$shape), 2)

  plot <- .plotBuilderTestPlot(dataset, sizeVariablePlotBuilder = "shape",
                               addMeanDot = TRUE)
  expect_equal(ggplot2::ggplot_build(plot)$data[[2]]$y, c(52, 52))
})

test_that("RM connections stay with their faceted and jittered points", {
  dataset <- data.frame(
    ID = rep(1:4, each = 2), time = factor(rep(c("a", "b"), 4)),
    Value = seq(10, 80, 10), facet = factor(rep(c("one", "two"), each = 4))
  )
  for (split in c("colSplitRM", "rowSplitRM", "gridVarRM")) {
    args <- list(dataset = dataset, isRM = "RM", xVarRM = "time",
                 connectRMPlotBuilder = TRUE, jitterwPlotBuilder = 0.3,
                 jitterhPlotBuilder = 0.3, ncolFacetWrap = 2, nrowFacetWrap = 1)
    args[[split]] <- "facet"
    built <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, args))
    points <- built$data[[1]]
    lines <- built$data[[2]]
    expect_equal(table(lines$PANEL), table(points$PANEL))
    expect_equal(as.numeric(lines$x[order(lines$y)]), as.numeric(points$x[order(points$y)]))
    expect_equal(sort(lines$y), sort(points$y))
  }
})

test_that("sorting RM categories keeps connections on the data points", {
  dataset <- data.frame(ID = rep(1:2, each = 2),
                        time = factor(rep(c("a", "b"), 2)), Value = c(100, 10, 110, 20))
  plot <- .plotBuilderTestPlot(dataset, isRM = "RM", xVarRM = "time",
                               connectRMPlotBuilder = TRUE, enableSort = TRUE)
  built <- ggplot2::ggplot_build(plot)
  points <- built$data[[1]]
  lines <- built$data[[2]]
  expect_equal(as.numeric(points$x), c(2, 1, 2, 1))
  expect_equal(as.numeric(lines$x[order(lines$y)]), as.numeric(points$x[order(points$y)]))
})

test_that("a complete 2 by 3 RM design has six cells", {
  dataset <- as.data.frame(setNames(rep(list(1:4), 6), paste0("v", 1:6)))
  readData <- .plotBuilderReadData
  environment(readData) <- list2env(list(
    .readDataSetToEnd = function(...) dataset,
    encodeColNames = identity,
    .quitAnalysis = function(message) stop(message)
  ), parent = environment(.plotBuilderReadData))
  tab <- list(value = "Plot 1", isRM = "RM", repeatedMeasuresCells = names(dataset),
              repeatedMeasuresFactors = list(list(name = "A", levels = c("a", "b")),
                                            list(name = "B", levels = c("c", "d", "e"))),
              betweenSubjectFactors = character(), covariates = character(),
              sizeVariablePlotBuilder = "", labelVariablePlotBuilder = "", pointShapeVariable = "",
              deleteNAListwise = TRUE)
  data <- readData(list(PlotBuilderTab = list(tab)))$datasetRMList[["Plot 1"]]
  expect_equal(nrow(data), 24)
  expect_equal(nrow(unique(data[c("A", "B")])), 6)
})

test_that("data labels build and stay in their assigned panels", {
  dataset <- data.frame(x = 1:4, y = 11:14, label = letters[1:4],
                        facet = factor(c("one", "one", "two", "two")))
  for (split in c("", "facet")) {
    plot <- .plotBuilderTestPlot(dataset, labelVariablePlotBuilder = "label",
                                 columnsvariableSplitPlotBuilder = split, jitterwPlotBuilder = 0.3)
    built <- ggplot2::ggplot_build(plot)
    expect_equal(built$data[[2]]$label, dataset$label)
    expect_equal(built$data[[2]]$PANEL, built$data[[1]]$PANEL)
    expect_equal(as.numeric(built$data[[2]]$x), as.numeric(built$data[[1]]$x))
  }
})

test_that("zero grid dimensions select an automatic facet layout", {
  dataset <- data.frame(x = 1:4, y = 11:14, facet = factor(letters[1:4]))
  for (dimensions in list(c(0, 0), c(0, 2), c(2, 0))) {
    plot <- .plotBuilderTestPlot(dataset, gridVariablePlotBuilder = "facet",
                                 nrowFacetWrap = dimensions[1], ncolFacetWrap = dimensions[2])
    expect_equal(nrow(ggplot2::ggplot_build(plot)$layout$layout), 4)
  }
})

test_that("flipping a plot preserves custom axis limits", {
  plot <- .plotBuilderTestPlot(data.frame(x = 1:4, y = 11:14), flipPlot = TRUE,
                               limitFromX = "2", limitToX = "3",
                               limitFromY = "11", limitToY = "13")
  expect_equal(plot$coordinates$limits$x, c(2, 3))
  expect_equal(plot$coordinates$limits$y, c(11, 13))
  expect_s3_class(plot$coordinates, "CoordFlip")
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[1]]), 4)
})

test_that("row facets preserve numeric and categorical Y scales", {
  dataset <- data.frame(x = 1:4, y = 11:14, facet = factor(c("one", "one", "two", "two")))
  plot <- .plotBuilderTestPlot(dataset, rowsvariableSplitPlotBuilder = "facet",
                               titleYPlotBuilder = "Measurement", breakFromY = "10",
                               breakToY = "16", breakByY = "2", YPaddingFirst = 0.2)
  scale <- plot$scales$get_scales("y")
  expect_equal(scale$name, "Measurement")
  expect_equal(scale$breaks, seq(10, 16, 2))
  expect_equal(scale$expand[1], 0.2)
  dataset$y <- factor(dataset$y)
  plot <- .plotBuilderTestPlot(dataset, rowsvariableSplitPlotBuilder = "facet")
  expect_true(plot$scales$get_scales("y")$is_discrete())
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[1]]), 4)
})

test_that("the identity line is independent of the observed ranges", {
  dataset <- data.frame(x = 1:4, y = 11:14)
  for (reverse in c(FALSE, TRUE)) {
    plot <- .plotBuilderTestPlot(dataset, addIdentityLinePlotBuilder = TRUE,
                                 reversedirectionIdentityLine = reverse)
    line <- ggplot2::ggplot_build(plot)$data[[2]]
    expect_equal(line$intercept, 0)
    expect_equal(line$slope, if (reverse) -1 else 1)
  }
})

test_that("histograms and densities accept either single axis variable", {
  dataset <- data.frame(x = rep(11:20, 2), y = rep(11:20, 2))
  for (axis in c("x", "y")) {
    plot <- .plotBuilderTestPlot(dataset, addDataPoint = FALSE, addHistogram = TRUE,
                                 addDensity = TRUE, binsPlotBuilder = 5,
                                 variableXPlotBuilder = if (axis == "x") "x" else "",
                                 variableYPlotBuilder = if (axis == "y") "y" else "")
    built <- ggplot2::ggplot_build(plot)
    expect_equal(sum(built$data[[1]]$count), nrow(dataset))
    expect_true(all(built$data[[1]]$flipped_aes == (axis == "y")))
    expect_true(all(is.finite(built$data[[2]][[axis]])))
    expect_equal(range(built$data[[2]][[axis]]), c(11, 20))
  }
})
