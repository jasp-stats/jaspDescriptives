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
  changes <- list(...)
  tab[names(changes)] <- changes
  data <- list(
    datasetNonRMList = setNames(list(dataset), tab$value),
    datasetRMList = setNames(list(dataset), tab$value)
  )
  .plotBuilderPlots(data, list(PlotBuilderTab = list(tab)))$updatedPlots[[tab$value]]
}

test_that("point outline color and empty fill are independent", {
  dataset <- data.frame(x = 1:6, y = 2:7, group = factor(rep(c("a", "b"), each = 3)))
  for (outline in c("automatic", "black", "white")) {
    for (empty in c(FALSE, TRUE)) {
      plot <- .plotBuilderTestPlot(dataset, outlineDataPoint = outline, emptyCircles = empty,
        pointsizePlotBuilder = 3, colorByGroup = "grouping", variableColorPlotBuilder = "group",
        colorsAll = "default")
      points <- ggplot2::ggplot_build(plot)$data[[1]]
      expect_true(all(points$size == 3))
      if (outline == "automatic") expect_length(unique(points$colour), 2)
      else expect_true(all(points$colour == outline))
      if (empty) expect_true(all(is.na(points$fill)))
      else expect_length(unique(points$fill), 2)
    }
  }
})

test_that("outline selections preserve fills and summaries for other elements", {
  dataset <- data.frame(x = factor(rep(c("a", "b"), each = 6)), y = rep(1:6, 2),
                        group = factor(rep(c("one", "two"), 6)))
  for (element in c("Boxplot", "Violin", "CountDot", "SumDot", "MeanDot", "MedianDot")) {
    layers <- lapply(c("automatic", "black", "white"), function(outline) {
      args <- list(dataset = dataset, addDataPoint = FALSE, colorByGroup = "grouping",
                   variableColorPlotBuilder = "group", colorsAll = "default")
      args[[paste0("add", element)]] <- TRUE
      args[[paste0("outline", element)]] <- outline
      if (element == "CountDot") args$variableYPlotBuilder <- ""
      plot <- do.call(.plotBuilderTestPlot, args)
      ggplot2::ggplot_build(plot)$data[[1]]
    })
    expect_true(all(layers[[2]]$colour == "black"))
    expect_true(all(layers[[3]]$colour == "white"))
    for (layer in layers[-1]) {
      expect_equal(layer$fill, layers[[1]]$fill)
      expect_equal(layer$y, layers[[1]]$y)
    }
  }
})

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

  plot <- .plotBuilderTestPlot(dataset, pointShapeVariable = "shape",
                               addMeanDot = TRUE, layerOrder = "mean_dot,point")
  expect_equal(ggplot2::ggplot_build(plot)$data[[1]]$y, c(52, 52))
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

test_that("RM connections and labels retain their positions in marginal panels", {
  dataset <- data.frame(ID = rep(1:4, each = 2), time = factor(rep(c("a", "b"), 4)),
                        Value = 1:8, facet = factor(rep(c("one", "two"), each = 4)), label = letters[1:8])
  plot <- .plotBuilderTestPlot(dataset, isRM = "RM", xVarRM = "time", colSplitRM = "facet",
    connectRMPlotBuilder = TRUE, margins = TRUE, jitterwPlotBuilder = 0.3, jitterhPlotBuilder = 0.3,
    labelVariablePlotBuilder = "label")
  built <- ggplot2::ggplot_build(plot)
  points <- built$data[[1]]
  lines <- do.call(rbind, built$data[2:4])
  labels <- do.call(rbind, built$data[5:7])
  expect_equal(nrow(points), 16)
  for (overlay in list(lines, labels)) {
    expect_equal(table(overlay$PANEL), table(points$PANEL))
    expect_equal(as.numeric(overlay$x[order(overlay$y)]), as.numeric(points$x[order(points$y)]))
    expect_equal(sort(overlay$y), sort(points$y))
  }
  expect_equal(sort(labels$label), sort(rep(dataset$label, 2)))
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

test_that("invalid layout weights fall back to equal dimensions", {
  output <- .plotBuilderOutputPlotGrid
  environment(output) <- list2env(list(
    createJaspContainer = function(...) new.env(parent = emptyenv()),
    createJaspPlot = function(...) list2env(list(dependOn = function(...) NULL)),
    is = function(...) FALSE
  ), parent = environment(.plotBuilderOutputPlotGrid))
  plot <- ggplot2::ggplot(data.frame(x = 1:4, y = 1:4), ggplot2::aes(x, y)) + ggplot2::geom_point()
  makeGrid <- function(weight, layout) {
    results <- new.env(parent = emptyenv())
    options <- list(columnWidthInput = weight, relativeHeight = weight,
                    relHeightWithinRowLayout = weight, layoutWidth = 600, layoutHeight = 600)
    if (layout %in% c("column", "both"))
      options$rowSpecifications <- list(list(plotIDs = "Plot 1", labels = "", rowHeightsColumn = weight))
    if (layout %in% c("row", "both"))
      options$fullRowSpecifications <- list(list(plotIDsFullRow = "Plot 1", labelsFullRow = "", relWidthsFullRow = weight))
    output(results, options, list(updatedPlots = list("Plot 1" = plot)), NULL)
    patchwork::patchworkGrob(results$plotGridContainer$plotGrid$plotObject)
  }
  for (layout in c("column", "row", "both")) {
    expected <- makeGrid("1", layout)
    for (weight in c("1e", "NA", "Inf", "\"one\",\"two\"")) {
      actual <- makeGrid(weight, layout)
      expect_equal(as.numeric(actual$widths), as.numeric(expected$widths))
      expect_equal(as.numeric(actual$heights), as.numeric(expected$heights))
    }
  }
})

test_that("percentages label only the relative axis", {
  dataset <- data.frame(x = rep(2019:2021, 2), y = c(1, 2, 3, 3, 2, 1),
                        group = factor(rep(c("a", "b"), each = 3)))
  plot <- .plotBuilderTestPlot(dataset, addDataPoint = FALSE, addAreaStack = TRUE,
    propMode = "relative", asPercentage = TRUE, colorByGroup = "grouping",
    variableColorPlotBuilder = "group", titleYPlotBuilder = "Share")
  built <- ggplot2::ggplot_build(plot)
  expect_false(any(grepl("%", built$layout$panel_params[[1]]$x$get_labels())))
  expect_true(any(grepl("%", built$layout$panel_params[[1]]$y$get_labels())))
  expect_equal(plot$scales$get_scales("y")$name, "Share")

  dataset$y <- factor(dataset$y)
  plot <- .plotBuilderTestPlot(dataset, addDataPoint = FALSE, addBarStack = TRUE,
    propMode = "relative", asPercentage = TRUE, colorByGroup = "grouping",
    variableColorPlotBuilder = "group")
  built <- ggplot2::ggplot_build(plot)
  expect_true(any(grepl("%", built$layout$panel_params[[1]]$x$get_labels())))
  expect_false(any(grepl("%", built$layout$panel_params[[1]]$y$get_labels())))
})

test_that("RM annotations target encoded within-subject and raw between-subject facets", {
  encoder <- function(x) ifelse(x %in% c("one", "two"), paste0(x, "_encoded"), x)
  plotFunction <- .plotBuilderPlots
  environment(plotFunction) <- list2env(list(encodeColNames = encoder),
                                         parent = environment(.plotBuilderPlots))
  plotHelper <- .plotBuilderTestPlot
  environment(plotHelper) <- list2env(list(.plotBuilderPlots = plotFunction),
                                       parent = environment(.plotBuilderTestPlot))
  for (within in c(FALSE, TRUE)) {
    dataset <- data.frame(ID = rep(1:4, each = 2), time = factor(rep(c("a", "b"), 4)),
                          Value = 1:8, facet = rep(c("one", "two"), each = 4))
    if (within) dataset$facet <- encoder(dataset$facet)
    dataset$facet <- factor(dataset$facet)
    factors <- list(list(name = "time", levels = c("a", "b")))
    if (within) factors[[2]] <- list(name = "facet", levels = c("one", "two"))
    plot <- plotHelper(dataset, isRM = "RM", xVarRM = "time", colSplitRM = "facet",
      repeatedMeasuresFactors = factors,
      annotationPlotBuilder = list(list(annotationText = "Text", annotationX = 1,
        annotationY = 4, annotationSize = 4, RMColumnAnnotation = "one")),
      annotationLineList = list(list(xAnnotation = "1", xendAnnotation = "2",
        yAnnotation = "4", yendAnnotation = "4", colorAnnotationLine = "black",
        RMColumnCompLine = "one")),
      pairwiseComparisons = list(list(group1 = "a", group2 = "b", pAdj = "*",
        RMColumnPValue = "one", RMRowPValue = "", RMGridPValue = "", RMGroupPValue = "",
        bracketSizePValue = 0.3, tipLengthPValue = 0.03)))
    built <- ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$layout$layout), 2)
    for (layer in built$data[-1]) expect_true(all(as.integer(layer$PANEL) == 1))
  }
})

test_that("each comparison retains its bracket width and symmetric tips", {
  comparison <- function(a, b, size, tip, facet) {
    list(group1 = a, group2 = b, pAdj = "*", GroupPValue = "", ColumnPValue = facet,
         RowPValue = "", GridPValue = "", bracketSizePValue = size, tipLengthPValue = tip)
  }
  dataset <- data.frame(x = factor(rep(c("a", "b", "c"), 2)), y = 1:6,
                        facet = factor(rep(c("one", "two"), each = 3)))
  plot <- .plotBuilderTestPlot(dataset, columnsvariableSplitPlotBuilder = "facet",
    pairwiseComparisons = list(comparison("a", "b", 0.3, 0.03, "two"),
                               comparison("a", "c", 0.6, 0.05, "one"),
                               comparison("b", "c", 0.3, 0.03, "two")))
  brackets <- ggplot2::ggplot_build(plot)$data[[2]]
  for (group in unique(brackets$group)) {
    bracket <- brackets[brackets$group == group, ]
    expect_equal(as.numeric(bracket$size), rep(c(0.3, 0.6, 0.3)[group], 3))
    expect_equal(bracket$yend[1] - bracket$y[1], bracket$y[3] - bracket$yend[3])
    expect_equal(unique(bracket$tip.length), c(0.03, 0.05, 0.03)[group])
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

test_that("black ribbon and fitted-curve outlines preserve fills and estimates", {
  dataset <- expand.grid(x = 1:5, group = factor(c("a", "b")), replicate = 1:4)
  dataset$y <- dataset$x + as.numeric(dataset$group) * 10 + dataset$replicate
  for (element in c("RangeRibbon", "SdRibbon", "SemRibbon", "Ci95Ribbon", "CurveFit")) {
    args <- list(dataset = dataset, addDataPoint = FALSE, colorByGroup = "grouping",
                 variableColorPlotBuilder = "group", colorsAll = "default", curvaFitMethod = "lm")
    args[[if (element == "CurveFit") "addCurveFitPlotBuilder" else paste0("add", element)]] <- TRUE
    args[[paste0("blackOutline", element)]] <- FALSE
    set.seed(1)
    original <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, args))$data[[1]]
    args[[paste0("blackOutline", element)]] <- TRUE
    set.seed(1)
    plot <- do.call(.plotBuilderTestPlot, args)
    outlined <- ggplot2::ggplot_build(plot)$data[[1]]
    expect_true(all(outlined$colour == "black"))
    expect_equal(outlined$fill, original$fill)
    expect_equal(outlined[c("ymin", "ymax", "group")], original[c("ymin", "ymax", "group")])
    expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
  }
})
