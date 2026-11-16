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
  if (!"facetLayout" %in% names(changes))
    tab$facetLayout <- if (nzchar(tab$gridVariablePlotBuilder) || nzchar(tab$gridVarRM)) "grid" else "rowsColumns"
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

test_that("point legends preserve mapped shapes and sizes", {
  dataset <- data.frame(x = 1:10, y = 11:20,
                        shape = factor(rep(1:5, 2)), size = rep(1:5, 2))
  legendPoints <- function(grob) {
    if (inherits(grob, "points"))
      return(list(data.frame(shape = grob$pch, size = grob$gp$fontsize)))
    unlist(lapply(c(grob$grobs, grob$children), legendPoints), recursive = FALSE)
  }

  for (mode in c("shape", "size", "shared", "separate", "cast", "color")) {
    plot <- .plotBuilderTestPlot(dataset,
      pointShapeVariable = if (mode == "size") "" else "shape",
      sizeVariablePlotBuilder = switch(mode, shape = "", shared = "shape", color = "shape", "size"),
      colorByGroup = "grouping", variableColorPlotBuilder = if (mode == "color") "shape" else "")
    if (mode == "cast") {
      # The same GUI variable cast as scale and nominal has one decoded legend title.
      sizeScale <- plot$scales$get_scales("size")
      sizeScale$name <- "shape"
    }
    built <- ggplot2::ggplot_build(plot)
    points <- built$data[[1]][1:5, ]
    grob <- ggplot2::ggplot_gtable(built)
    boxes <- grob$grobs[grepl("guide-box", grob$layout$name)]
    guides <- unlist(lapply(boxes, function(box) box$grobs[box$layout$name == "guides"]), recursive = FALSE)
    keys <- lapply(guides, function(guide) do.call(rbind, legendPoints(guide)))

    if (mode != "size")
      expect_true(any(vapply(keys, function(key) isTRUE(all.equal(key$shape, points$shape)), logical(1))), info = mode)
    if (mode != "shape") {
      expectedSizes <- vapply(seq_len(nrow(points)), function(i)
        ggplot2::draw_key_point(points[i, ], list(), 5)$gp$fontsize, numeric(1))
      expect_true(any(vapply(keys, function(key) isTRUE(all.equal(key$size, expectedSizes)), logical(1))), info = mode)
    }
    if (mode %in% c("shared", "cast", "color")) expect_length(keys, 1)
    if (mode == "separate") expect_length(keys, 2)
  }
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


test_that("summary line color honors the checkbox without changing the summaries", {
  dataset <- data.frame(x = factor(rep(letters[1:3], each = 4)), y = 1:12)
  for (element in c("Count", "Sum", "Mean", "Median")) {
    args <- list(dataset = dataset, addDataPoint = FALSE, colorByGroup = "x", colorsAll = "default")
    args[[paste0("add", element, "Line")]] <- TRUE
    if (element == "Count") args$variableYPlotBuilder <- ""
    args[[paste0("blackOutline", element, "Line")]] <- FALSE
    plot <- do.call(.plotBuilderTestPlot, args)
    colored <- ggplot2::ggplot_build(plot)$data[[1]]
    expect_length(unique(colored$colour), 3)
    expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
    args[[paste0("blackOutline", element, "Line")]] <- TRUE
    black <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, args))$data[[1]]
    expect_true(all(black$colour == "black"))
    expect_equal(black[c("x", "y", "group")], colored[c("x", "y", "group")])
  }
})


test_that("ellipse radii can exceed one while confidence levels remain finite", {
  dataset <- data.frame(x = c(0, 2, 3, 1, 4, 5), y = c(3, 1, 5, 4, 2, 6))
  for (type in c("euclid", "norm", "t")) {
    for (level in if (type == "euclid") c(0.5, 1, 5) else c(0.001, 0.95, 0.999)) {
      plot <- .plotBuilderTestPlot(dataset, addStatEllipse = TRUE, ellipseType = type, levelEllipse = level)
      ellipse <- ggplot2::ggplot_build(plot)$data[[2]]
      expect_true(all(is.finite(ellipse$x) & is.finite(ellipse$y)))
      expect_gt(nrow(ellipse), 3)
      if (type == "euclid")
        expect_equal(sqrt((ellipse$x - mean(dataset$x))^2 + (ellipse$y - mean(dataset$y))^2), rep(level, nrow(ellipse)))
    }
  }
  for (level in c(0, 1))
    expect_error(.plotBuilderTestPlot(dataset, addStatEllipse = TRUE, ellipseType = "norm", levelEllipse = level), "strictly between")
})


test_that("distribution layers reject categorical axes without restricting other layers", {
  dataset <- data.frame(x = factor(rep(letters[1:3], 3)), y = 1:9)
  for (layer in c("addHistogram", "addDensity")) {
    args <- list(dataset = dataset, addDataPoint = FALSE, variableYPlotBuilder = "")
    args[[layer]] <- TRUE
    expect_error(do.call(.plotBuilderTestPlot, args), "exactly one scale variable")
    args$variableYPlotBuilder <- "y"
    expect_gt(nrow(ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, args))$data[[1]]), 0)
  }
  plot <- .plotBuilderTestPlot(dataset, addBoxplot = TRUE)
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[1]]), nrow(dataset))
})


test_that("count density uses the histogram bin width in each panel and orientation", {
  dataset <- data.frame(x = rep(seq(0, 10, length.out = 40), 4),
                        group = factor(rep(c("a", "b"), c(40, 120))),
                        facet = factor(rep(c("one", "two"), each = 80)))
  dataset$x[dataset$facet == "two"] <- dataset$x[dataset$facet == "two"] * 10
  dataset$y <- dataset$x
  for (axis in c("x", "y")) {
    for (bins in c(10, 20)) {
      for (scales in c("fixed", "free")) {
        args <- list(dataset = dataset, addDataPoint = FALSE, addHistogram = TRUE, addDensity = TRUE,
          binsPlotBuilder = bins, colorByGroup = "grouping", variableColorPlotBuilder = "group",
          gridVariablePlotBuilder = "facet", ncolFacetWrap = 2, nrowFacetWrap = 1, scalesFacetWrap = scales,
          variableXPlotBuilder = if (axis == "x") "x" else "",
          variableYPlotBuilder = if (axis == "y") "y" else "")
        plot <- do.call(.plotBuilderTestPlot, args)
        built <- ggplot2::ggplot_build(plot)
        histogram <- built$data[[1]]
        density <- built$data[[2]]
        expect_gt(nrow(density), 0)
        for (panel in unique(density$PANEL)) {
          h <- histogram[histogram$PANEL == panel, ]
          d <- density[density$PANEL == panel, ]
          width <- h[[paste0(axis, "max")]][1] - h[[paste0(axis, "min")]][1]
          expect_equal(d$count, d$density * d$n * width)
        }
        expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
      }
    }
  }
  standalone <- .plotBuilderTestPlot(dataset, variableYPlotBuilder = "", addDataPoint = FALSE, addDensity = TRUE)
  d <- ggplot2::ggplot_build(standalone)$data[[1]]
  expect_equal(d$count, d$density * d$n)
})


test_that("p-value colors can be custom or follow groups without moving brackets", {
  dataset <- data.frame(x = factor(rep(c("a", "b"), 4)), y = 1:8, group = factor(rep(c("one", "two"), each = 4)))
  comparison <- list(group1 = "a", group2 = "b", pAdj = "*", GroupPValue = "one",
                     ColumnPValue = "", RowPValue = "", GridPValue = "", bracketSizePValue = 0.3, tipLengthPValue = 0.03)
  args <- list(dataset = dataset, colorByGroup = "grouping", variableColorPlotBuilder = "group",
               labelcolor = "magenta", pairwiseComparisons = list(comparison))
  custom <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, c(args, list(pValueColorMode = "custom"))))$data[[2]]
  grouped <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, c(args, list(pValueColorMode = "group"))))$data[[2]]
  legacy <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, c(args, list(pValueColorMode = NULL))))$data[[2]]
  expect_true(all(custom$colour == "magenta"))
  expect_false(any(grouped$colour == "magenta"))
  expect_equal(grouped$colour, legacy$colour)
  expect_equal(custom[c("x", "xend", "y", "yend")], grouped[c("x", "xend", "y", "yend")])
})


test_that("explicit facet layouts retain assignments and target only active annotation panels", {
  dataset <- expand.grid(x = factor(c("a", "b")), row = factor(c("r1", "r2")),
                         column = factor(c("c1", "c2")), facet = factor(c("g1", "g2", "g3")))
  dataset$y <- seq_len(nrow(dataset))
  for (rm in c(FALSE, TRUE)) {
    data <- dataset
    if (rm) { data$Value <- data$y; data$ID <- rep(seq_len(nrow(data) / 2), each = 2) }
    args <- list(dataset = data, isRM = if (rm) "RM" else "noRM", xVarRM = "x",
      rowsvariableSplitPlotBuilder = "row", columnsvariableSplitPlotBuilder = "column", gridVariablePlotBuilder = "facet",
      rowSplitRM = "row", colSplitRM = "column", gridVarRM = "facet", nrowFacetWrap = 0, ncolFacetWrap = 0,
      annotationPlotBuilder = list(list(annotationText = "Target", annotationX = 1, annotationY = -2, annotationSize = 4,
        ColumnAnnotation = "c1", RowAnnotation = "r1", GridAnnotation = "g1",
        RMColumnAnnotation = "c1", RMRowAnnotation = "r1", RMGridAnnotation = "g1")))
    for (layout in c("rowsColumns", "grid")) {
      built <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, c(args, list(facetLayout = layout))))
      expect_equal(nrow(built$layout$layout), if (layout == "grid") 3 else 4)
      expect_equal(nrow(built$data[[1]]), nrow(dataset))
      expect_equal(nrow(built$data[[2]]), 1)
      expect_equal(built$data[[2]]$y, -2)
    }
    legacy <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, c(args, list(facetLayout = NULL))))
    expect_equal(nrow(legacy$layout$layout), 3)
  }
})

test_that("unused facet assignments do not exclude observations", {
  dataset <- data.frame(x = 1:4, y = 5:8, column = c(NA, "a", "b", "b"), facet = c("a", "a", "b", "b"))
  readData <- .plotBuilderReadData
  environment(readData) <- list2env(list(
    .readDataSetToEnd = function(columns) dataset[, columns, drop = FALSE], encodeColNames = identity
  ), parent = environment(.plotBuilderReadData))
  tab <- list(value = "Plot 1", isRM = "noRM", facetLayout = "grid", colorByGroup = "none",
    variableXPlotBuilder = "x", variableYPlotBuilder = "y", variableColorPlotBuilder = "",
    columnsvariableSplitPlotBuilder = "column", rowsvariableSplitPlotBuilder = "", gridVariablePlotBuilder = "facet",
    sizeVariablePlotBuilder = "", labelVariablePlotBuilder = "", pointShapeVariable = "")
  expect_equal(nrow(readData(list(PlotBuilderTab = list(tab)))$datasetNonRMList[[1]]), 4)
  # An inactive facet can still supply colors, so it is read when explicitly used.
  tab$colorByGroup <- "splitColumn"
  expect_equal(nrow(readData(list(PlotBuilderTab = list(tab)))$datasetNonRMList[[1]]), 3)
  tab$colorByGroup <- "none"
  tab$facetLayout <- "rowsColumns"
  expect_equal(nrow(readData(list(PlotBuilderTab = list(tab)))$datasetNonRMList[[1]]), 3)
})


test_that("summary lines colored by Y retain their values and render", {
  dataset <- data.frame(x = factor(rep(letters[1:3], each = 4)), y = 1:12)
  for (element in c("Sum", "Mean", "Median")) {
    args <- list(dataset = dataset, addDataPoint = FALSE, colorByGroup = "y", colorsAll = "default")
    args[[paste0("add", element, "Line")]] <- TRUE
    args[[paste0("blackOutline", element, "Line")]] <- FALSE
    plot <- do.call(.plotBuilderTestPlot, args)
    colored <- ggplot2::ggplot_build(plot)$data[[1]]
    expect_length(unique(colored$colour), 3)
    expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
    args[[paste0("blackOutline", element, "Line")]] <- TRUE
    black <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, args))$data[[1]]
    expect_equal(black[c("x", "y", "group")], colored[c("x", "y", "group")])
  }
})

test_that("negative annotation coordinates work on continuous and categorical axes", {
  dataset <- data.frame(x = -4:-1, y = -8:-5)
  for (categorical in c(FALSE, TRUE)) {
    data <- dataset
    if (categorical) data$x <- factor(letters[1:4])
    plot <- .plotBuilderTestPlot(data,
      annotationPlotBuilder = list(list(annotationText = "Negative", annotationX = if (categorical) 1 else -3,
        annotationY = -6, annotationSize = 4)),
      annotationLineList = list(list(xAnnotation = if (categorical) "1" else "-4", xendAnnotation = if (categorical) "2" else "-1",
        yAnnotation = "-7", yendAnnotation = "-7", colorAnnotationLine = "black")))
    built <- ggplot2::ggplot_build(plot)
    expect_equal(built$data[[2]]$y, -6)
    expect_equal(built$data[[3]]$y, -7)
    expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
  }
})


test_that("Y-only count lines retain counts when colored or black", {
  dataset <- data.frame(x = 1:9, y = factor(rep(letters[1:3], c(2, 3, 4))))
  plots <- lapply(c(FALSE, TRUE), function(black) {
    plot <- .plotBuilderTestPlot(dataset, variableXPlotBuilder = "", addDataPoint = FALSE,
      addCountLine = TRUE, colorByGroup = "y", colorsAll = "default", blackOutlineCountLine = black)
    expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
    ggplot2::ggplot_build(plot)$data[[1]]
  })
  expect_equal(plots[[1]]$count, c(2, 3, 4))
  expect_length(unique(plots[[1]]$colour), 3)
  expect_true(all(plots[[2]]$colour == "black"))
  expect_equal(plots[[1]][c("x", "y", "count")], plots[[2]][c("x", "y", "count")])
})

test_that("scaled densities are independent of histogram bins and layer order", {
  dataset <- data.frame(x = seq(1, 10, length.out = 40), y = 1:40)
  densities <- lapply(c(10, 20), function(bins) {
    plot <- .plotBuilderTestPlot(dataset, variableYPlotBuilder = "", addDataPoint = FALSE,
      addHistogram = TRUE, addDensity = TRUE, densityOverlayMode = "scaled", binsPlotBuilder = bins,
      layerOrder = "density,histogram")
    ggplot2::ggplot_build(plot)$data[[1]]
  })
  expect_equal(densities[[1]]$y, densities[[2]]$y)
  expect_equal(max(densities[[1]]$y), 1)
})

test_that("joint centers differ from conditional summaries and retain explicit groups", {
  data <- data.frame(x = c(1, 1, 3, 5, 5, 9), y = c(2, 6, 7, 20, 22, 40),
                     group = factor(rep(c("a", "b"), each = 3)))
  for (statistic in c("Mean", "Median")) {
    args <- list(dataset = data, addDataPoint = FALSE, variableColorPlotBuilder = "group",
                 colorByGroup = "none")
    args[[paste0("add", statistic, "Dot")]] <- TRUE
    args[[paste0(tolower(statistic), "DotMode")]] <- "center"
    plot <- do.call(.plotBuilderTestPlot, args)
    centers <- ggplot2::ggplot_build(plot)$data[[1]]
    fun <- if (statistic == "Mean") mean else median
    expect_equal(centers$x, as.numeric(tapply(data$x, data$group, fun)))
    expect_equal(centers$y, as.numeric(tapply(data$y, data$group, fun)))
    expect_no_error(ggplot2::ggplotGrob(plot))
    args[[paste0(tolower(statistic), "DotMode")]] <- "position"
    conditional <- ggplot2::ggplot_build(do.call(.plotBuilderTestPlot, args))$data[[1]]
    expect_equal(nrow(conditional), 4)
  }
})

test_that("single-axis distributions and summaries preserve their numeric orientation", {
  data <- data.frame(x = c(1, 2, 4, 8, 10), y = c(10, 20, 40, 80, 100))
  for (axis in c("x", "y")) {
    for (geometry in c("DataPoint", "Boxplot", "Violin", "MeanDot", "MedianDot", "SumDot", "MeanBar", "MedianBar", "SumBar", "SEMErrorBar")) {
      args <- list(dataset = data, addDataPoint = FALSE)
      args[[if (axis == "x") "variableYPlotBuilder" else "variableXPlotBuilder"]] <- ""
      args[[paste0("add", geometry)]] <- TRUE
      plot <- do.call(.plotBuilderTestPlot, args)
      built <- ggplot2::ggplot_build(plot)$data[[1]]
      expect_gt(nrow(built), 0)
      if (geometry == "MeanDot") expect_equal(built[[axis]], mean(data[[axis]]))
      if (geometry == "MedianDot") expect_equal(built[[axis]], median(data[[axis]]))
      if (geometry == "SumDot") expect_equal(built[[axis]], sum(data[[axis]]))
      expect_no_error(ggplot2::ggplotGrob(plot))
    }
  }
})

test_that("categorical distribution positions retain panels and normalized depth", {
  data <- expand.grid(category = factor(c("a", "b")), group = factor(c("g1", "g2")),
                      facet = factor(c("f1", "f2")), replicate = 1:10)
  data$value <- data$replicate + as.numeric(data$category) * 20
  for (horizontal in c(FALSE, TRUE)) {
    plot <- .plotBuilderTestPlot(data, addDataPoint = FALSE, addHistogram = TRUE, addDensity = TRUE,
      variableXPlotBuilder = if (horizontal) "value" else "category",
      variableYPlotBuilder = if (horizontal) "category" else "value",
      variableColorPlotBuilder = "group", colorByGroup = "none", distributionHeight = 0.6,
      columnsvariableSplitPlotBuilder = "facet")
    built <- ggplot2::ggplot_build(plot)
    for (layer in built$data) {
      expect_length(unique(layer$PANEL), 2)
      expect_length(unique(layer$group), 4)
      depth <- if (horizontal) layer$ymax - layer$ymin else layer$xmax - layer$xmin
      expect_equal(max(depth), 0.6)
      expect_true(all(depth >= 0 & depth <= 0.6 + 1e-10))
      panelRange <- built$layout$panel_params[[1]][[if (horizontal) "y.range" else "x.range"]]
      expect_gt(max(panelRange), max(layer[[if (horizontal) "ymax" else "xmax"]]))
    }
    expect_no_error(ggplot2::ggplotGrob(plot))
  }
})

test_that("histogram grouping supports stacking and side-by-side bins", {
  data <- data.frame(x = rep(1:10, 2), group = factor(rep(c("a", "b"), each = 10)))
  totals <- numeric()
  for (position in c("identity", "stack", "dodge")) {
    plot <- .plotBuilderTestPlot(data, variableYPlotBuilder = "", addDataPoint = FALSE,
      addHistogram = TRUE, variableColorPlotBuilder = "group", colorByGroup = "none",
      histogramPosition = position, binsPlotBuilder = 5)
    built <- ggplot2::ggplot_build(plot)$data[[1]]
    expect_length(unique(built$group), 2)
    totals <- c(totals, sum(built$count))
    expect_no_error(ggplot2::ggplotGrob(plot))
  }
  expect_equal(totals, rep(nrow(data), 3))
})

test_that("RM means and uncertainty use one average per subject and position", {
  data <- data.frame(ID = c(1, 1, 1, 2, 3), time = factor("a"), Value = c(0, 3, 6, 9, 12))
  plot <- .plotBuilderTestPlot(data, isRM = "RM", xVarRM = "time", addDataPoint = FALSE,
    addMeanDot = TRUE, addSEMErrorBar = TRUE, addCI95ErrorBar = TRUE,
    layerOrder = "mean_dot,sem_errorbar,ci95_errorbar")
  built <- ggplot2::ggplot_build(plot)
  subjectMeans <- c(3, 9, 12)
  expect_equal(built$data[[1]]$y, mean(subjectMeans))
  expect_equal(built$data[[2]]$y, mean(subjectMeans))
  expect_equal(built$data[[2]]$ymax - built$data[[2]]$y, sd(subjectMeans) / sqrt(3))
  expect_equal(built$data[[3]]$y, mean(subjectMeans))
})

test_that("uncertainty omits insufficient groups but retains valid means", {
  data <- data.frame(x = factor(c("a", "a", "b")), y = c(1, 3, 7))
  plot <- .plotBuilderTestPlot(data, addDataPoint = FALSE,
    addMeanDot = TRUE, addSEMErrorBar = TRUE, layerOrder = "mean_dot,sem_errorbar")
  expect_warning(built <- ggplot2::ggplot_build(plot), "at least two")
  expect_equal(built$data[[1]]$y, c(2, 7))
  expect_equal(built$data[[2]]$y, 2)
})

test_that("stack measures preserve totals, facets, and absolute signed sums", {
  data <- expand.grid(x = 1:3, group = factor(c("a", "b")), facet = factor(c("f1", "f2")))
  data$y <- rep(c(1, 2, 4), 4)
  for (area in c(FALSE, TRUE)) {
    for (measure in c("count", "sum")) {
      args <- list(dataset = data, addDataPoint = FALSE, stackMeasure = measure,
        variableColorPlotBuilder = "group", colorByGroup = "none",
        columnsvariableSplitPlotBuilder = "facet", replaceNaAreaStack = TRUE)
      args[[if (area) "addAreaStack" else "addBarStack"]] <- TRUE
      plot <- do.call(.plotBuilderTestPlot, args)
      layer <- plot$layers[[1]]$data
      value <- if (measure == "count") ".plotBuilderCount" else "y"
      expect_equal(sum(layer[[value]]), if (measure == "count") nrow(data) else sum(data$y))
      expect_length(unique(layer$facet), 2)
      expect_no_error(ggplot2::ggplotGrob(plot))
    }
  }
  data$y[1] <- -1
  args <- list(dataset = data, addDataPoint = FALSE, stackMeasure = "sum", addBarStack = TRUE,
    variableColorPlotBuilder = "group", columnsvariableSplitPlotBuilder = "facet")
  expect_no_error(ggplot2::ggplotGrob(do.call(.plotBuilderTestPlot, args)))
  args$propMode <- "relative"
  expect_error(do.call(.plotBuilderTestPlot, args), "nonnegative")
  args$dataset$y <- 0
  expect_error(do.call(.plotBuilderTestPlot, args), "positive total")
})

test_that("count dashes and categorical annotations support horizontal positions", {
  data <- data.frame(x = c(1, 2, 3, 4), y = factor(c("a", "a", "b", "b")))
  plot <- .plotBuilderTestPlot(data, variableXPlotBuilder = "", addDataPoint = FALSE, addCountDash = TRUE)
  dash <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(dash$x, c(2, 2))
  expect_no_error(ggplot2::ggplotGrob(plot))
  plot <- .plotBuilderTestPlot(data,
    annotationLineList = list(list(xAnnotation = "1", xendAnnotation = "3", yAnnotation = "1", yendAnnotation = "2", colorAnnotationLine = "black")))
  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("shape palettes preserve observations beyond six levels", {
  data <- data.frame(x = 1:12, y = 12:1, shape = factor(1:12))
  plot <- .plotBuilderTestPlot(data, pointShapeVariable = "shape")
  points <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(nrow(points), 12)
  expect_false(anyNA(points$shape))
  expect_length(unique(points$shape), 12)
  data$shape <- factor(data$shape, levels = 1:15)
  data$Value <- data$y
  data$ID <- seq_len(nrow(data))
  rmPlot <- .plotBuilderTestPlot(data, isRM = "RM", xVarRM = "x", pointShapeVariable = "shape")
  expect_no_error(ggplot2::ggplotGrob(rmPlot))
})

test_that("RM marginal panels recompute subject averages from the original rows", {
  data <- data.frame(ID = c(1, 1, 2, 3, 1, 2, 2), time = factor("a"),
                     facet = factor(c("A", "A", "A", "A", "B", "B", "B")),
                     Value = c(0, 2, 4, 10, 10, 10, 30))
  plot <- .plotBuilderTestPlot(data, isRM = "RM", xVarRM = "time", colSplitRM = "facet",
    margins = TRUE, addDataPoint = FALSE, addMeanDot = TRUE, addSEMErrorBar = TRUE,
    layerOrder = "mean_dot,sem_errorbar")
  built <- ggplot2::ggplot_build(plot)
  expected <- list(c(1, 4, 10), c(10, 20), c(4, 44/3, 10))
  expect_equal(built$data[[1]]$y, vapply(expected, mean, numeric(1)))
  expect_equal(built$data[[2]]$ymax - built$data[[2]]$y,
    vapply(expected, function(x) sd(x) / sqrt(length(x)), numeric(1)))
  expect_no_error(ggplot2::ggplotGrob(plot))
})

test_that("RM trajectories do not cross other within-subject conditions", {
  data <- expand.grid(time = factor(c("a", "b")), condition = factor(c("one", "two")), ID = 1:2)
  data$Value <- seq_len(nrow(data))
  plot <- .plotBuilderTestPlot(data, isRM = "RM", xVarRM = "time", connectRMPlotBuilder = TRUE,
    repeatedMeasuresFactors = list(list(name = "time", levels = c("a", "b")),
                                  list(name = "condition", levels = c("one", "two"))))
  lines <- ggplot2::ggplot_build(plot)$data[[2]]
  expect_length(unique(lines$group), 4)
  expect_true(all(table(lines$group) == 2))
})

test_that("single-axis stack totals and relative percentages retain orientation", {
  data <- data.frame(x = c(1, 2, 3, 4), group = factor(c("a", "a", "b", "b")))
  for (axis in c("x", "y")) {
    for (measure in c("count", "sum", "auto")) {
      args <- list(dataset = data, addDataPoint = FALSE, addBarStack = TRUE,
        variableXPlotBuilder = if (axis == "x") "x" else "",
        variableYPlotBuilder = if (axis == "y") "x" else "",
        variableColorPlotBuilder = "group", colorByGroup = "grouping",
        stackMeasure = measure, propMode = "relative", asPercentage = TRUE)
      plot <- do.call(.plotBuilderTestPlot, args)
      built <- ggplot2::ggplot_build(plot)
      countAxis <- if (measure == "count") { if (axis == "x") "y" else "x" } else axis
      expect_equal(max(built$data[[1]][[paste0(countAxis, "max")]]), 1)
      expect_match(built$plot$scales$get_scales(countAxis)$labels(1), "100")
      expect_no_error(ggplot2::ggplotGrob(plot))
    }
  }
})

test_that("manual P-value brackets retain ordinal and count-axis use cases", {
  comparison <- list(group1 = "a", group2 = "b", pAdj = "*", GroupPValue = "",
    ColumnPValue = "", RowPValue = "", GridPValue = "", bracketSizePValue = 0.3, tipLengthPValue = 0.03)
  for (type in c("ordinal", "nominal", "count")) {
    data <- data.frame(x = factor(c("a", "a", "b", "b")),
                       y = factor(c("low", "high", "low", "high"), ordered = type == "ordinal"))
    plot <- .plotBuilderTestPlot(data, variableYPlotBuilder = if (type == "count") "" else "y",
      addDataPoint = type != "count", addCountBar = type == "count",
      yPositionPValue = 3, pairwiseComparisons = list(comparison))
    expect_no_error(ggplot2::ggplotGrob(plot))
    if (type == "ordinal") expect_true(is.ordered(plot$data$y))
  }
})

test_that("RM distributions and exact-value counts work without an X assignment", {
  data <- data.frame(ID = rep(1:4, each = 2), Value = rep(1:4, 2))
  for (geometry in c("Histogram", "Density", "Boxplot", "Violin", "CountBar", "CountDash", "CountDot", "CountLine", "CountArea", "CountValue")) {
    args <- list(dataset = data, isRM = "RM", xVarRM = "", addDataPoint = FALSE)
    args[[paste0("add", geometry)]] <- TRUE
    plot <- do.call(.plotBuilderTestPlot, args)
    expect_no_error(ggplot2::ggplotGrob(plot))
  }
})

test_that("multiple plot tabs retain independent statistic parameters", {
  data <- data.frame(value = rep(1:12, 2), category = factor(rep(c("a", "b"), each = 12)))
  first <- eval(formals(jaspPlotBuilder)$PlotBuilderTab)[[1]]
  for (name in names(first)) {
    if (is.list(first[[name]]) && "types" %in% names(first[[name]]))
      first[[name]] <- unlist(first[[name]]$value)
  }
  first <- utils::modifyList(first, list(value = "first", variableXPlotBuilder = "category",
    variableYPlotBuilder = "value", addHistogram = TRUE, addDensity = TRUE,
    binsPlotBuilder = 5, distributionHeight = 0.4, plotStyle = "ggplotgray"))
  second <- utils::modifyList(first, list(value = "second", variableXPlotBuilder = "value",
    variableYPlotBuilder = "category", binsPlotBuilder = 9, distributionHeight = 0.8))
  datasets <- list(datasetNonRMList = list(first = data, second = data), datasetRMList = list())
  plots <- .plotBuilderPlots(datasets, list(PlotBuilderTab = list(first, second)))$updatedPlots
  firstBuilt <- ggplot2::ggplot_build(plots$first)
  secondBuilt <- ggplot2::ggplot_build(plots$second)
  expect_equal(nrow(firstBuilt$data[[1]]), 10)
  expect_equal(nrow(secondBuilt$data[[1]]), 18)
  for (layer in firstBuilt$data) expect_equal(max(layer$xmax - layer$xmin), 0.4)
  for (layer in secondBuilt$data) expect_equal(max(layer$ymax - layer$ymin), 0.8)
})
