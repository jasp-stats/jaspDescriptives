# Geometry helpers shared by the Plot Builder layers.

.plotBuilderSummaryStat <- function(stat, averageSubjects, uncertainty) {
  force(averageSubjects)
  force(uncertainty)
  ggplot2::ggproto(NULL, stat,
    required_aes = unique(c(stat$required_aes, if (averageSubjects) ".subject")),
    compute_panel = function(data, scales, fun = NULL, na.rm = FALSE, flipped_aes = FALSE) {
      valueVars <- if (flipped_aes) "x" else "y"
      positionVars <- if (flipped_aes) "y" else "x"
      if (averageSubjects) {
        by <- c("group", ".subject", positionVars)
        otherVars <- setdiff(names(data), c(by, valueVars))
        # Aggregate after faceting, so marginal panels also have one value per subject.
        data <- data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
          dplyr::summarise(dplyr::across(dplyr::all_of(valueVars), ~ mean(.x, na.rm = TRUE)),
            dplyr::across(dplyr::all_of(otherVars), ~ .x[1]), .groups = "drop")
        data$.subject <- NULL
      }
      if (uncertainty)
        data <- .plotBuilderUncertaintyData(data, valueVars, c("group", positionVars))
      if (!nrow(data)) return(data.frame())
      stat$compute_panel(data, scales, fun = fun, na.rm = na.rm, flipped_aes = flipped_aes)
    }
  )
}

.plotBuilderUncertaintyData <- function(data, valueVar, groupVars) {
  groupVars <- intersect(unique(groupVars), names(data))
  data <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groupVars))) |>
    dplyr::mutate(.plotBuilderUsable = sum(is.finite(.data[[valueVar]]))) |>
    dplyr::ungroup()
  if (any(data$.plotBuilderUsable < 2))
    warning(gettext("SD, SEM and confidence intervals require at least two usable observations (subjects for repeated-measures SEM/CI) at each position within each group and panel. Insufficient groups are omitted from these layers."), call. = FALSE)
  data[data$.plotBuilderUsable >= 2, setdiff(names(data), ".plotBuilderUsable"), drop = FALSE]
}

.plotBuilderCenterStat <- function(fun, referenceLines = FALSE) {
  force(fun)
  force(referenceLines)
  ggplot2::ggproto(NULL, ggplot2::Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales) {
      x <- fun(data$x)
      y <- fun(data$y)
      result <- if (referenceLines)
        data.frame(x = c(-Inf, x), y = c(y, -Inf), xend = c(Inf, x), yend = c(y, Inf)) else
        data.frame(x = x, y = y)
      for (aesthetic in intersect(c("colour", "fill"), names(data)))
        if (is.numeric(data[[aesthetic]])) result[[aesthetic]] <- fun(data[[aesthetic]])
      result
    }
  )
}

.plotBuilderCenterLayer <- function(fun, style, size, linewidth, alpha, outline) {
  args <- list(stat = .plotBuilderCenterStat(fun, referenceLines = style == "lines"), alpha = alpha)
  if (outline != "automatic") args$colour <- outline
  if (style == "lines") {
    args$mapping <- ggplot2::aes(xend = ggplot2::after_stat(xend), yend = ggplot2::after_stat(yend))
    args$linewidth <- linewidth
    do.call(ggplot2::geom_segment, args)
  } else {
    args$shape <- if (style == "crosshair") 3 else 21
    args$size <- size
    args$stroke <- linewidth
    do.call(ggplot2::geom_point, args)
  }
}

.plotBuilderMeanEllipseStat <- function(type, multiplier, level) {
  force(type)
  force(multiplier)
  force(level)
  ggplot2::ggproto(NULL, ggplot2::Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales) {
      n <- nrow(data)
      if (n < if (type == "ci") 3 else 2) {
        warning(gettext("Mean ellipses require at least two complete observations per group and panel (three for confidence ellipses). Insufficient groups are omitted."), call. = FALSE)
        return(data.frame())
      }
      covariance <- stats::cov(data[c("x", "y")])
      if (type == "ci" && (any(diag(covariance) <= 0) ||
          abs(stats::cov2cor(covariance)[1, 2]) >= 1)) {
        warning(gettext("A confidence ellipse requires variation in both coordinates without perfect correlation. Degenerate groups are omitted."), call. = FALSE)
        return(data.frame())
      }
      # Hotelling's T-squared region for a bivariate mean; SD/SEM use a radius multiplier.
      radiusSquared <- switch(type, sd = multiplier^2, sem = multiplier^2 / n,
        ci = 2 * (n - 1) / (n * (n - 2)) * stats::qf(level, 2, n - 2))
      decomposition <- eigen(covariance, symmetric = TRUE)
      angle <- seq(0, 2 * pi, length.out = 121)
      ellipse <- cbind(cos(angle), sin(angle)) %*%
        diag(sqrt(pmax(decomposition$values, 0) * radiusSquared)) %*% t(decomposition$vectors)
      ellipse <- sweep(ellipse, 2, colMeans(data[c("x", "y")]), "+")
      result <- data.frame(x = ellipse[, 1], y = ellipse[, 2])
      for (aesthetic in intersect(c("colour", "fill"), names(data)))
        if (is.numeric(data[[aesthetic]])) result[[aesthetic]] <- mean(data[[aesthetic]])
      result
    }
  )
}

.plotBuilderStackMeasure <- function(measure, area, x, y) {
  if (!is.null(measure) && measure != "auto") return(measure)
  if (area) return(if (!is.null(x) && !is.null(y)) "sum" else "count")
  if (is.numeric(x) || is.numeric(y)) "sum" else "count"
}

.plotBuilderAreaStackStat <- function() {
  ggplot2::ggproto(NULL, ggplot2::StatAlign,
    compute_panel = function(data, scales, flipped_aes = FALSE) {
      # Facet margins combine panels: sum their contributions before aligning groups.
      positionVar <- if (flipped_aes) "y" else "x"
      valueVar <- if (flipped_aes) "x" else "y"
      by <- c("group", positionVar)
      otherVars <- setdiff(names(data), c(by, valueVar))
      data <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
        dplyr::summarise("{valueVar}" := sum(.data[[valueVar]]),
          dplyr::across(dplyr::all_of(otherVars), ~ .x[1]), .groups = "drop")
      ggplot2::StatAlign$compute_panel(data, scales, flipped_aes = flipped_aes)
    }
  )
}

.plotBuilderStack <- function(plot, tab, area, measure, xVar, yVar, groupVar, colorVar, facetVars) {
  data <- plot$data
  hasX <- nzchar(xVar)
  hasY <- nzchar(yVar)
  scaleX <- hasX && is.numeric(data[[xVar]])
  scaleY <- hasY && is.numeric(data[[yVar]])
  if (measure == "sum" && !scaleX && !scaleY)
    .quitAnalysis(gettext("Sum stacks require a scale variable."))
  if (hasX && hasY && !scaleX && !scaleY)
    .quitAnalysis(gettext("Stacks require a single positioning axis or a scale response with a positioning axis."))
  horizontal <- if (measure == "sum") scaleX && !scaleY else !hasX || (scaleX && hasY && !scaleY)
  positionVar <- if (horizontal) yVar else xVar
  valueVar <- if (measure == "sum") { if (horizontal) xVar else yVar } else ".plotBuilderCount"
  if (!nzchar(positionVar)) {
    positionVar <- ".plotBuilderPosition"
    data[[positionVar]] <- 1
  }
  hasColor <- length(colorVar) == 1 && nzchar(colorVar)
  groupVars <- unique(c(groupVar, if (hasColor && is.factor(data[[colorVar]])) colorVar))
  by <- unique(c(facetVars, positionVar, groupVars))
  if (measure == "count") data[[valueVar]] <- 1
  data <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise("{valueVar}" := sum(.data[[valueVar]], na.rm = TRUE),
      dplyr::across(dplyr::all_of(if (hasColor && !colorVar %in% c(by, valueVar)) colorVar else character()),
        ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  if (area && isTRUE(tab$replaceNaAreaStack)) {
    data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(facetVars))) |>
      tidyr::complete(!!rlang::sym(positionVar),
        tidyr::nesting(!!!rlang::syms(setdiff(groupVars, positionVar))),
        fill = stats::setNames(list(0), valueVar)) |>
      dplyr::ungroup()
  }
  if (tab$propMode == "relative") {
    totals <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(facetVars, positionVar))))) |>
      dplyr::summarise(total = sum(.data[[valueVar]]), .groups = "drop")
    if (any(!is.finite(data[[valueVar]]) | data[[valueVar]] < 0) || any(totals$total <= 0))
      .quitAnalysis(gettext("Relative stacks require nonnegative group contributions and a positive total at every position. Use Absolute for signed sums."))
  }
  mapping <- ggplot2::aes(group = interaction(!!!rlang::syms(groupVars), drop = TRUE))
  mapping[[if (horizontal) "y" else "x"]] <- rlang::sym(positionVar)
  mapping[[if (horizontal) "x" else "y"]] <- rlang::sym(valueVar)
  if (hasColor) mapping$fill <- rlang::sym(colorVar)
  reverse <- if (area) tab$reverseAreaStack else tab$reverseBarStack
  position <- if (tab$propMode == "relative") ggplot2::position_fill(reverse = reverse) else
    ggplot2::position_stack(reverse = reverse)
  args <- list(data = data, mapping = mapping, inherit.aes = FALSE, position = position,
               orientation = if (horizontal) "y" else "x",
               alpha = if (area) tab$alphaAreaStack else tab$alphaBarStack)
  if (area) {
    args$stat <- .plotBuilderAreaStackStat()
    args$linewidth <- tab$linewidthAreaStack
  } else args$width <- 0.8
  plot <- plot + do.call(if (area) ggplot2::geom_area else ggplot2::geom_col, args)
  if (measure == "count")
    plot <- plot + do.call(ggplot2::labs, stats::setNames(list("count"), if (horizontal) "x" else "y"))
  plot
}

.plotBuilderDistributionStat <- function(histogram, horizontal, bins, height, trim) {
  force(histogram)
  force(horizontal)
  force(bins)
  force(height)
  force(trim)
  ggplot2::ggproto(NULL, ggplot2::Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales) {
      # Work with the scale variable on x, then return to the selected axes.
      if (!horizontal) {
        data[c("x", "y")] <- data[c("y", "x")]
        scales[c("x", "y")] <- scales[c("y", "x")]
      }
      baseline <- unique(data$y)
      if (length(baseline) != 1) return(data.frame())
      if (histogram) {
        result <- ggplot2::StatBin$compute_group(data, scales, bins = bins)
        result$ymin <- baseline
        result$ymax <- baseline + height * result$count / max(result$count)
      } else {
        if (nrow(data) < 2) {
          warning(gettext("A density requires at least two observations in each group."), call. = FALSE)
          return(data.frame())
        }
        result <- ggplot2::StatDensity$compute_group(data, scales, trim = trim)
        result$ymin <- baseline
        result$ymax <- baseline + height * result$scaled
      }
      if (!horizontal) {
        if (histogram) {
          result[c("xmin", "xmax", "ymin", "ymax")] <- result[c("ymin", "ymax", "xmin", "xmax")]
          result$y <- result$x
          result$x <- baseline
        } else {
          result$y <- result$x
          result$xmin <- result$ymin
          result$xmax <- result$ymax
          result$x <- baseline
          result$ymin <- result$ymax <- NULL
        }
      } else result$y <- baseline
      result$flipped_aes <- !horizontal
      result
    }
  )
}

.plotBuilderBracketStat <- function(stat) {
  ggplot2::ggproto(NULL, stat,
    compute_group = function(data, scales, tip.length) {
      if (scales$y$is_discrete())
        scales$y <- list(range = list(range = scales$y$dimension()))
      stat$compute_group(data, scales, tip.length = tip.length)
    }
  )
}

.plotBuilderCountDensityStat <- function(bins) {
  force(bins)
  ggplot2::ggproto(NULL, ggplot2::StatDensity,
    compute_group = function(data, scales, flipped_aes = FALSE) {
      density <- ggplot2::StatDensity$compute_group(data, scales, flipped_aes = flipped_aes)
      histogram <- ggplot2::StatBin$compute_group(data, scales, bins = bins, flipped_aes = flipped_aes)
      density$count <- density$count * histogram$width[1]
      density
    })
}
