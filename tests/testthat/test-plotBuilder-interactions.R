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
