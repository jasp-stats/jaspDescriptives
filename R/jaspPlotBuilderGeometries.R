# Geometry helpers shared by the Plot Builder layers.

.plotBuilderSummaryStat <- function(stat, averageSubjects, jointCenter, uncertainty) {
  force(averageSubjects)
  force(jointCenter)
  force(uncertainty)
  ggplot2::ggproto(NULL, stat,
    required_aes = unique(c(stat$required_aes, if (averageSubjects) ".subject")),
    compute_panel = function(data, scales, fun = NULL, na.rm = FALSE, flipped_aes = FALSE) {
      valueVars <- if (jointCenter) c("x", "y") else if (flipped_aes) "x" else "y"
      positionVars <- if (jointCenter) character() else if (flipped_aes) "y" else "x"
      if (averageSubjects) {
        by <- c("group", ".subject", positionVars)
        otherVars <- setdiff(names(data), c(by, valueVars))
        # Aggregate after faceting, so marginal panels also have one value per subject.
        data <- data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
          dplyr::summarise(dplyr::across(dplyr::all_of(valueVars), ~ mean(.x, na.rm = TRUE)),
            dplyr::across(dplyr::all_of(otherVars), ~ .x[1]), .groups = "drop")
      }
      if (uncertainty)
        data <- .plotBuilderUncertaintyData(data, valueVars, c("group", positionVars))
      if (!nrow(data)) return(data.frame())
      if (jointCenter) stat$compute_panel(data, scales)
      else stat$compute_panel(data, scales, fun = fun, na.rm = na.rm, flipped_aes = flipped_aes)
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

.plotBuilderCenterStat <- function(fun) {
  ggplot2::ggproto(NULL, ggplot2::Stat,
    required_aes = c("x", "y"),
    compute_group = function(data, scales) {
      data.frame(x = fun(data$x), y = fun(data$y))
    }
  )
}

.plotBuilderStack <- function(plot, tab, area, xVar, yVar, groupVar, colorVar, facetVars) {
  data <- plot$data
  hasX <- nzchar(xVar)
  hasY <- nzchar(yVar)
  scaleX <- hasX && is.numeric(data[[xVar]])
  scaleY <- hasY && is.numeric(data[[yVar]])
  measure <- tab$stackMeasure
  if (is.null(measure) || measure == "auto")
    measure <- if (area) { if (hasX && hasY) "sum" else "count" } else
      if (scaleX || scaleY) "sum" else "count"
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
  if (area) args$linewidth <- tab$linewidthAreaStack else args$width <- 0.8
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
