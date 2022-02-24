# First Example Dataset
set.seed(1)
n <- 1000
m <- 7
dataset.factors <- data.frame(ID = 1:n,
                              Gender = sample(c("male", "female"), n, replace = TRUE),
                              eng = sample(1:m, n, replace = TRUE),
                              psycho = sample(1:m, n, replace = TRUE),
                              math = sample(1:m, n, replace = TRUE),
                              bio = sample(1:m, n, replace = TRUE),
                              life = sample(1:m, n, replace = TRUE))

# Multiple Variables in Plot
dataset.factors <- dataset.factors[,-c(1,2)]
dataset.factors[1:5] <- lapply(dataset.factors[1:5], factor)

# One Variable in Plot
dataset.factors.one <- dataset.factors[, -c(2:5)]
dataset.factors.one <- data.frame(eng = dataset.factors.one)


####### ####### ####### #######


#  Second Example Dataset
library(likert)
data(pisaitems)
pisa.dataset.factors <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"]


####### ####### ####### #######


# Function
likert_Plot <- function (dataset) {
  # Likert Part: Preparing & summarize data in the likert format (% of levels per variable)
  nLevels <- nlevels(dataset[, 1])
  center <- (nLevels - 1)/2 + 1
  lowRange <- 1:floor(center - 0.5)
  highRange <- ceiling(center + 0.5):nLevels

  if (center < 1.5 || center > (nLevels - 0.5)) {
    stop(paste0("Items must have 2 or more levels!"))
  }
  if (!all(sapply(dataset, function(x) is.factor(x)))) {
    warning("items parameter contains non-factors. Will convert to factors")
    for (i in 1:ncol(dataset)) {
      dataset[, i] <- factor(dataset[, i], levels = 1:nLevels)
    }
  }
  if (!all(sapply(dataset, function(x) nlevels(x)) == nLevels)) {
    stop("All items (columns) must have the same number of levels")
  }
  results <- data.frame()
  results <- data.frame(Response = 1:nLevels)
  for(i in 1:ncol(dataset)) {
    t <- table(dataset[,i])
    t <- (t/sum(t) * 100)
    results <- cbind(results, as.data.frame(t)[,2])
    names(results)[ncol(results)] <- names(dataset)[i]
  }
  results <- as.data.frame(t(results))
  names(results) <- levels(dataset[,1])
  results <- results[2:nrow(results),]

  resultsTwo <- data.frame(Item = row.names(results),
                           low = rep(NA, nrow(results)),
                           neutral = rep(NA, nrow(results)),
                           high = rep(NA, nrow(results)))
  if(length(lowRange) == 1) {
    resultsTwo$low <- results[,lowRange]
  } else {
    resultsTwo$low <- apply(results[,lowRange], 1, sum)
  }
  if(length(highRange) == 1) {
    resultsTwo$high <- results[,highRange]
  } else {
    resultsTwo$high <- apply(results[,highRange], 1, sum)
  }
  if(lowRange[length(lowRange)] + 1 != highRange[1]) {
    resultsTwo$neutral <- results[,(highRange[1] - 1)]
  }
  row.names(resultsTwo) <- 1:nrow(resultsTwo)
  resultsTwo <- resultsTwo[order(resultsTwo$high, decreasing = TRUE),]

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
  # Checking for missing values in "resultsTwo"
  narows <- which(is.na(resultsTwo$low))
  if(length(narows) > 0) {
    resultsTwo[narows,]$low <- 0
  }
  narows <- which(is.na(resultsTwo$neutral))
  if(length(narows) > 0) {
    resultsTwo[narows,]$neutral <- 0
  }
  narows <- which(is.na(resultsTwo$high))
  if(length(narows) > 0) {
    resultsTwo[narows,]$high <- 0
  }
  l <- list(results = results, items = dataset, levels = levels(dataset[, 1]), sum = resultsTwo)

  # Likert Plot Part:
  textSize <- 4
  textColor <- "black"
  yMin <- -100
  yMax <- 100
  yBuffer <- 5
  palette <- c("#D8B365", "#E1C58B", "#EBD9B2", "#F5ECD8",
               "#D5ECEA", "#ACD9D5", "#83C6C0", "#5AB4AC")
  cols <- scales::gradient_n_pal(palette, values = NULL)(seq(0, 1, length.out = nLevels))
  if (center%%1 == 0){
    cols[center] <- "grey90"
  }
  resultsLong <- stats::reshape(data = l$results, idvar = "Item",
                                v.name = c("value"),
                                varying = c(names(l$results[,2:length(l$results)])),
                                times = c(names(l$results[,2:length(l$results)])),
                                timevar = "variable",
                                new.row.names = 1:(length(l$results[2:length(l$results)])*length(l$results$Item)),
                                direction = "long")

  order <- l$sum[order(l$sum$high), "Item"] #important for low - high order of items in plot
  resultsLong$Item <- factor(resultsLong$Item, levels = order)
  orderTwo <- l$levels  # important for the correct legend sequence
  resultsLong$variable <- factor(resultsLong$variable, levels = orderTwo)

  rows <- which(resultsLong$variable %in% names(l$results)[2:(length(lowRange) + 1)])
  resultsLong[rows, "value"] <- -1 * resultsLong[rows, "value"]
  if (center%%1 == 0) {
    rowsMid <- which(resultsLong$variable %in% names(l$results)[center + 1])
    tmp <- resultsLong[rowsMid,]
    tmp$value <- tmp$value/2 * -1
    resultsLong[rowsMid, "value"] <- resultsLong[rowsMid, "value"]/2
    resultsLong <- rbind(resultsLong, tmp)
  }
  resultsLow <- resultsLong[resultsLong$value < 0,]
  resultsHigh <- resultsLong[resultsLong$value > 0,]
  p <- NULL
  p <- ggplot2::ggplot(resultsLong, ggplot2::aes(y = value, x = Item, group = Item)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_bar(data = resultsLow[nrow(resultsLow):1,], ggplot2::aes(fill = variable), stat = "identity") +
    ggplot2::geom_bar(data = resultsHigh, ggplot2::aes(fill = variable), stat = "identity")

  names(cols) <- levels(resultsLong$variable)
  p <- p + ggplot2::scale_fill_manual("Response", breaks = names(cols), values = cols, drop = FALSE)

  p <- p + ggplot2::geom_text(data = l$sum,    # plot.percent.low
                              y = yMin,
                              ggplot2::aes(x = Item, label = paste0(round(low), "%")),
                              size = textSize,
                              hjust = 1,
                              color = textColor)

  p <- p + ggplot2::geom_text(data = l$sum,    # plot.percent.high
                              y = 100,
                              ggplot2::aes(x = Item, label = paste0(round(high), "%")),
                              size = textSize,
                              hjust = -0.2,
                              color = textColor)

  if (nLevels%%2 == 1) {                       # plot.percent.neutral
    p <- p + ggplot2::geom_text(data = l$sum,
                                y = 0,
                                ggplot2::aes(x = Item, label = paste0(round(neutral), "%")),
                                size = textSize,
                                hjust = 0.5,
                                color = textColor)
  }
  p <- p + ggplot2::coord_flip() +
    ggplot2::ylab("Percentage") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())

  p <- p + ggplot2::scale_y_continuous(labels = function(x) return(abs(x)), limits = c(yMin - yBuffer, yMax + yBuffer))

  p <- p + ggplot2::theme(legend.position = "bottom")

  p <- p + ggplot2::theme(panel.background = ggplot2::element_rect(size = 1, color = "grey70", fill = NA))

  p <- p + ggplot2::theme(text = ggplot2::element_text(size = jaspGraphs::getGraphOption("fontsize")))

  return(p)
}

# Test function
likert_Plot(dataset.factors)
likert_Plot(dataset.factors.one)
likert_Plot(pisa.dataset.factors)
