test_that("small descriptives plot builders materialize from recipes", {
  recipes <- list(
    boxplot = jaspGraphs::createJaspPlotRecipe(
      "jaspDescriptives:::.plotBoxDescriptives",
      list(
        y = c(1:9, 30, 2:11),
        yIndexToActual = as.character(seq_len(20)),
        group = factor(rep(c("a", "b"), each = 10)),
        xlab = "Group",
        boxWidth = 0.4,
        vioWidth = 0.6,
        variable = "Value",
        quantilesType = 7L,
        boxPlotColourPalette = FALSE,
        colorPalette = "colorblind",
        boxPlotViolin = TRUE,
        boxPlotBoxPlot = TRUE,
        boxPlotJitter = TRUE,
        boxPlotOutlierLabel = TRUE
      )
    ),
    interval = jaspGraphs::createJaspPlotRecipe(
      "jaspDescriptives:::.plotIntervalDescriptives",
      list(y = c(1, 2, 3, 4), group = factor(c("a", "a", "b", "b")), xName = "Group", yName = "Value")
    ),
    dot = jaspGraphs::createJaspPlotRecipe(
      "jaspDescriptives:::.plotDotDescriptives",
      list(x = c(1, 2, 2, 3), variableName = "Value", variableType = "scale")
    ),
    heatmap = jaspGraphs::createJaspPlotRecipe(
      "jaspDescriptives:::.plotHeatmapDescriptives",
      list(
        data = data.frame(
          horizontal = factor(c("a", "b")),
          vertical = factor(c("x", "y")),
          value = c(1, 2),
          label = c(1, 2)
        ),
        axesNames = c("Horizontal", "Vertical"),
        tileRatio = 1,
        displayValue = TRUE,
        valueTextSize = 1,
        palette = "colorblind",
        showLegend = TRUE
      )
    ),
    likert = jaspGraphs::createJaspPlotRecipe(
      "jaspDescriptives:::.plotLikertDescriptives",
      list(
        dataset = data.frame(
          item1 = factor(c("low", "middle", "high"), levels = c("low", "middle", "high")),
          item2 = factor(c("middle", "high", "high"), levels = c("low", "middle", "high"))
        ),
        fontSize = "medium"
      )
    ),
    categoricalDensity = jaspGraphs::createJaspPlotRecipe(
      "jaspDescriptives:::.plotCategoricalDensityDescriptives",
      list(
        data = data.frame(variable = factor(c("a", "a", "b"))),
        xName = "Category",
        groupingName = "",
        categoricalType = "count",
        palette = "colorblind"
      )
    )
  )

  plots <- lapply(recipes, jaspGraphs::materializeJaspPlotRecipe)
  expect_true(all(vapply(plots, ggplot2::is_ggplot, logical(1L))))
})

test_that("correlation matrix materializes from a recipe", {
  recipe <- jaspGraphs::createJaspPlotRecipe(
    "jaspDescriptives:::.plotCorrelationMatrixDescriptives",
    list(
      dataset = data.frame(x = c(1, 2, 3, 4), y = c(2, 1, 4, 3)),
      variables = c("x", "y"),
      errors = c("", ""),
      displayDensity = TRUE,
      rugs = FALSE,
      binWidthType = "sturges",
      numberOfBins = 10L
    )
  )

  plot <- jaspGraphs::materializeJaspPlotRecipe(recipe)
  expect_s3_class(plot, "jaspMatrixPlot")
  expect_length(plot$subplots, 7L)
})
