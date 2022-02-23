likert_Plot <- function (items) {   # items = dataset (will be changed)
  # Likert Part: Preparing & summarize data in the likert format (% of levels per variable)
  nlevels <- nlevels(items[, 1])
  center <- (nlevels - 1)/2 + 1
  lowrange <- 1:floor(center - 0.5)
  highrange <- ceiling(center + 0.5):nlevels

  if (center < 1.5 || center > (nlevels - 0.5)) {
    stop(paste0("Items must have 2 or more levels!"))
  }
  if (!all(sapply(items, function(x) is.factor(x)))) {
    warning("items parameter contains non-factors. Will convert to factors")
    for (i in 1:ncol(items)) {
      items[, i] <- factor(items[, i], levels = 1:nlevels)
    }
  }
  if (!all(sapply(items, function(x) nlevels(x)) == nlevels)) {
    stop("All items (columns) must have the same number of levels")
  }
  results <- data.frame()
  results <- data.frame(Response = 1:nlevels)
  for(i in 1:ncol(items)) {
    t <- table(items[,i])
    t <- (t/sum(t) * 100)
    results <- cbind(results, as.data.frame(t)[,2])
    names(results)[ncol(results)] <- names(items)[i]
  }
  results <- as.data.frame(t(results))
  names(results) <- levels(items[,1])
  results <- results[2:nrow(results),]

  results2 <- data.frame(Item = row.names(results),    # Summary in high, low, neutral %
                         low = rep(NA, nrow(results)),
                         neutral = rep(NA, nrow(results)),
                         high = rep(NA, nrow(results)))
  if(length(lowrange) == 1) {
    results2$low <- results[,lowrange]
  } else {
    results2$low <- apply(results[,lowrange], 1, sum)
  }
  if(length(highrange) == 1) {
    results2$high <- results[,highrange]
  } else {
    results2$high <- apply(results[,highrange], 1, sum)
  }
  if(lowrange[length(lowrange)] + 1 != highrange[1]) {
    results2$neutral <- results[,(highrange[1] - 1)]
  }
  row.names(results2) <- 1:nrow(results2)
  results2 <- results2[order(results2$high, decreasing = TRUE),]

  results <- cbind(row.names(results), results)
  names(results)[1] <- "Item"
  row.names(results) <- 1:nrow(results)

  # Checking for missing values in "results"
  for(i in 2:ncol(results)) {
    narows <- which(is.na(results[,i]))
    if (length(narows) > 0) {
      results[narows,i] <- 0
      }
  }
  # Checking for missing values in "results2"
  narows <- which(is.na(results2$low))
  if(length(narows) > 0) {
    results2[narows,]$low <- 0
  }
  narows <- which(is.na(results2$neutral))
  if(length(narows) > 0) {
    results2[narows,]$neutral <- 0
  }
  narows <- which(is.na(results2$high))
  if(length(narows) > 0) {
    results2[narows,]$high <- 0
  }
  l <- list(results = results, items = items, levels = levels(items[, 1]), sum = results2)

  # Likert Plot Part:
  text.size <- 4
  text.color <- "black"
  ymin <- -100
  ymax <- 100
  ybuffer <- 5
  palette <- c("#D8B365", "#E1C58B", "#EBD9B2", "#F5ECD8",
               "#D5ECEA", "#ACD9D5", "#83C6C0", "#5AB4AC")
  cols <- scales::gradient_n_pal(palette, values = NULL)(seq(0, 1, length.out = nlevels))
  if (center%%1 == 0){
    cols[center] <- "grey90"
  }
  results3 <- stats::reshape(data = l$results, idvar = "Item",
                             v.name = c("value"),
                             varying = c(names(l$results[,2:length(l$results)])),
                             times = c(names(l$results[,2:length(l$results)])),
                             timevar = "variable",
                             new.row.names = 1:(length(l$results[2:length(l$results)])*length(l$results$Item)),
                             direction = "long")

  order <- l$sum[order(l$sum$high), "Item"] #important for low - high order of items in plot
  results3$Item <- factor(results3$Item, levels = order)
  order_2 <- l$levels  # important for the correct legend sequence
  results3$variable <- factor(results3$variable, levels = order_2)

  rows <- which(results3$variable %in% names(l$results)[2:(length(lowrange) + 1)])
  results3[rows, "value"] <- -1 * results3[rows, "value"]
  if (center%%1 == 0) {
    rows.mid <- which(results3$variable %in% names(l$results)[center + 1])
    tmp <- results3[rows.mid,]
    tmp$value <- tmp$value/2 * -1
    results3[rows.mid, "value"] <- results3[rows.mid, "value"]/2
    results3 <- rbind(results3, tmp)
  }
  results.low <- results3[results3$value < 0,]
  results.high <- results3[results3$value > 0,]
  p <- NULL
  p <- ggplot2::ggplot(results3, ggplot2::aes(y = value, x = Item, group = Item)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_bar(data = results.low[nrow(results.low):1,], ggplot2::aes(fill = variable), stat = "identity") +
    ggplot2::geom_bar(data = results.high, ggplot2::aes(fill = variable), stat = "identity")

  names(cols) <- levels(results3$variable)
  p <- p + ggplot2::scale_fill_manual("Response", breaks = names(cols), values = cols, drop = FALSE)

  p <- p + ggplot2::geom_text(data = l$sum,    # plot.percent.low
                              y = ymin,
                              ggplot2::aes(x = Item, label = paste0(round(low), "%")),
                              size = text.size,
                              hjust = 1,
                              color = text.color)

  p <- p + ggplot2::geom_text(data = l$sum,    # plot.percent.high
                              y = 100,
                              ggplot2::aes(x = Item, label = paste0(round(high), "%")),
                              size = text.size,
                              hjust = -0.2,
                              color = text.color)

  if (nlevels%%2 == 1) {                       # plot.percent.neutral
    p <- p + ggplot2::geom_text(data = l$sum,
                                y = 0,
                                ggplot2::aes(x = Item, label = paste0(round(neutral), "%")),
                                size = text.size,
                                hjust = 0.5,
                                color = text.color)
  }
  p <- p + ggplot2::coord_flip() +
    ggplot2::ylab("Percentage") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())

  p <- p + ggplot2::scale_y_continuous(labels = function(x) return(abs(x)), limits = c(ymin - ybuffer, ymax + ybuffer))

  p <- p + ggplot2::theme(legend.position = "bottom")

  p <- p + ggplot2::theme(panel.background = ggplot2::element_rect(size = 1, color = "grey70", fill = NA))

  p <- p + ggplot2::theme(text = ggplot2::element_text(size = jaspGraphs::getGraphOption("fontsize")))

  return(p)
}
