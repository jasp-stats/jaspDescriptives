# Example dataset (string levels)
library(likert)
data(pisaitems)
items28 <- pisaitems[, substr(names(pisaitems), 1, 5) == "ST24Q"]

# Example dataset (numeric levels)
set.seed(1)
n <- 1000
m <- 7
df <- data.frame(ID = 1:n,
                 Gender = sample(c("male", "female"), n, replace = TRUE),
                 eng = sample(1:m, n, replace = TRUE),
                 psycho = sample(1:m, n, replace = TRUE),
                 math = sample(1:m, n, replace = TRUE),
                 bio = sample(1:m, n, replace = TRUE),
                 life = sample(1:m, n, replace = TRUE))
# df <- data.frame(ID = i, Gender = g, eng = x) TEST for one variable


#setwd('/Users/lucat/OneDrive/Dokumente/Uni Amsterdam/Internship/Example Data/')
#write.csv(df,"LikertData.csv", row.names = FALSE)


# For jasp I will need at least two functions:
###  implemented if statement in the "Descriptives" function that creates layout (container)
### .descriptivesLikertPlots --> the actual plotting function ()

# What happens in JASP / What arrives at my function
### PHASE 1 ###
# Dataset is split
## SPLIT TRUE
splitName        <- "Gender"
df_split         <- df[,-1]
df_split$Gender  <- as.factor(df_split$Gender)
#split
variables        <- names(df_split[, -1])
# variables        <- names(df_split[2])  TEST for one variable
splitFactor      <- df_split[[.v(splitName)]] # requires jaspBase
splitLevels      <- levels(splitFactor)
df_split[2:length(df_split)]  <- lapply(df_split[2:length(df_split)], factor)
splitDat.factors <- split(df_split[.v(variables)], splitFactor) # delivered to function when split

# Dataset is not split
## SPLIT FALSE
dataset.factors <- df[,-c(1,2)]  # delivered to function when no split occurs
dataset.factors[1:length(dataset.factors)] <- lapply(dataset.factors[1:length(dataset.factors)], factor)
# dataset.factors <- data.frame(eng = df[,-c(1,2)])   TEST for one variable

### PHASE 2 ###
# The "Descriptive" function iterates through each GROUP an passes to .descriptivesLikertPlots

## PHASE 3 ###
# .descriptivesLikertPlots
# passes the dataset (dataset.factors/splitDat.factors$XXX) and variable to the plotting function


# My Function
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

  resultsTwo <- data.frame(Item = row.names(results),    # Summarizing likert data: high, low, neutral %
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

  # Correcting for missing values in "results"
  for(i in 2:ncol(results)) {
    narows <- which(is.na(results[,i]))
    if (length(narows) > 0) {
      results[narows,i] <- 0
      }
  }
  # Correcting for missing values in "resultsTwo"
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
  textSize <- 5
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
  orderTwo <- l$levels                      # important for the correct legend sequence
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
  p <- p + ggplot2::scale_fill_manual("Response", breaks = names(cols), values = scales::alpha(cols, 1), drop = FALSE)

  p <- p + ggplot2::geom_text(data = l$sum,    # plot.percent.low
                              y = yMin,
                              ggplot2::aes(x = Item, label = paste0(round(low), "%")),
                              size = textSize,
                              hjust = 0.7,
                              color = textColor)

  p <- p + ggplot2::geom_text(data = l$sum,    # plot.percent.high
                              y = 100,
                              ggplot2::aes(x = Item, label = paste0(round(high), "%")),
                              size = textSize,
                              hjust = 0.3,
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
  #return(createJaspPlot(plot=p, aspectRatio=1, title=name))
}


pdf("plot.pdf")
likert_Plot(dataset.factors)
dev.off()


# Testing my function
likert_Plot(dataset.factors)
likert_Plot(items28)

# Comparing to Likert Version
library(likert)
f <- likert(dataset.factors)
plot(f)

# Debugging my function
debug(likert_Plot)
likert_Plot(dataset.factors)
undebug(likert_Plot)


# Simulating Missing Values
library(mice)
miss <- ampute(dataset.factors, prop = 0.7, patterns = NULL, freq = NULL, mech = "MAR",
               weights = NULL, cont = TRUE, type = NULL, odds = NULL,
               bycases = TRUE, run = TRUE)
miss <- miss$amp
miss[1:length(miss)] <- lapply(miss[1:length(miss)], factor)
likert_Plot(miss)
# Amount missing/not missing
sum(is.na(miss))
sum(!is.na(miss))



# Palettes to choose from (probably substituted with a JASP palette)
# 2 Levels
palette <- c("#D8B365", "#5AB4AC")

# 4 Levels
palette <- c("#D8B365", "#EBD9B2",
          "#ACD9D5", "#5AB4AC")

# 6 Levels
palette <- c("#D8B365", "#E5CC98", "#F2E5CB",
          "#C8E6E3", "#91CDC7", "#5AB4AC")

# 8 Levels
palette <- c("#D8B365", "#E1C58B", "#EBD9B2", "#F5ECD8",
             "#D5ECEA", "#ACD9D5", "#83C6C0", "#5AB4AC")

# 10 Levels
palette <- c("#D8B365", "#DFC283", "#E7D1A2", "#EFE0C1", "#F7EFE0",
             "#DEF0EE", "#BDE1DD", "#9CD2CD", "#7BC3BC", "#5AB4AC")

# 12 Levels
palette <- c("#D8B365", "#DEBF7E", "#E5CC98", "#EBD9B2", "#F2E5CB", "#F8F2E5",
             "#E3F2F1", "#C8E6E3", "#ACD9D5", "#91CDC7", "#75C0B9", "#5AB4AC")
