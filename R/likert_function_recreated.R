# In the current state these work on their own! (only plot depends on summary to be available)

my_likert <- function (items, nlevels = length(levels(items[, 1]))) 
{
        center <- (nlevels - 1)/2 + 1
        if (!all(sapply(items, function(x) "factor" %in% class(x)))) {
                warning("items parameter contains non-factors. Will convert to factors")
                for (i in 1:ncol(items)) {
                        items[, i] <- factor(items[, i], levels = 1:nlevels)
                }
        }
        if (!all(sapply(items, function(x) length(levels(x))) == nlevels)) {
                stop("All items (columns) must have the same number of levels")
        }
        lowrange <- 1:ceiling(nlevels/2 - nlevels%%2)
        highrange <- ceiling(nlevels/2 + 1):nlevels
        results <- data.frame()
        
        results <- data.frame(Response = 1:nlevels)
        for (i in 1:ncol(items)) {
                t <- table(items[, i])
                t <- (t/sum(t) * 100)
                results <- cbind(results, as.data.frame(t)[, 2])
                names(results)[ncol(results)] <- names(items)[i]
        }
        results <- as.data.frame(t(results))
        names(results) <- levels(items[, 1])
        results <- results[2:nrow(results), ]
        results <- cbind(row.names(results), results)
        names(results)[1] <- "Item"
        row.names(results) <- 1:nrow(results)
        for (i in 2:ncol(results)) {
                narows <- which(is.na(results[, i]))
                if (length(narows) > 0) {
                        results[narows, i] <- 0
                }
        }
        l <- list(results = results, items = items, 
                  nlevels = nlevels, levels = levels(items[, 1]))
        return(l)
}

my_likert_summary <- function(object, center=(object$nlevels-1)/2 + 1) 
{
        
        results <- object$results
        items <- object$items
        nlevels <- object$nlevels
        lowrange <- 1 : floor(center - 0.5)
        highrange <- ceiling(center + 0.5) : nlevels
        
        results = data.frame(Response=1:nlevels)
        for(i in 1:ncol(items)) {
                t = table(items[,i])
                t = (t / sum(t) * 100)
                results = cbind(results, as.data.frame(t)[2])
                names(results)[ncol(results)] = names(items)[i]
        }
        results = as.data.frame(t(results))
        names(results) = levels(items[,1])
        results = results[2:nrow(results),]
        results2 = data.frame(Item = row.names(results),
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
        row.names(results2) = 1:nrow(results2)
        
        results2 <- results2[order(results2$high, decreasing=TRUE),]
        
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
        if(is.factor(object$results$Item)) {
                results2$Item <- factor(results2$Item, levels=levels(object$results$Item))
        }
        return(results2)
}

my_likert_combo <- function (items, nlevels = length(levels(items[, 1]))) # combines likert & summary function
{
        # Likert Part:
        center <- (nlevels - 1)/2 + 1
        lowrange <- 1 : floor(center - 0.5)
        highrange <- ceiling(center + 0.5) : nlevels
        if (!all(sapply(items, function(x) "factor" %in% class(x)))) {
                warning("items parameter contains non-factors. Will convert to factors")
                for (i in 1:ncol(items)) {
                        items[, i] <- factor(items[, i], levels = 1:nlevels)
                }
        }
        if (!all(sapply(items, function(x) length(levels(x))) == nlevels)) {
                stop("All items (columns) must have the same number of levels")
        }
        
        results <- data.frame()
        results <- data.frame(Response = 1:nlevels)
        for (i in 1:ncol(items)) {
                t <- table(items[, i])
                t <- (t/sum(t) * 100)
                results <- cbind(results, as.data.frame(t)[, 2])
                names(results)[ncol(results)] <- names(items)[i]
        }
        results <- as.data.frame(t(results))
        names(results) <- levels(items[, 1])
        results <- results[2:nrow(results), ]
        results <- cbind(row.names(results), results)
        names(results)[1] <- "Item"
        row.names(results) <- 1:nrow(results)
        for (i in 2:ncol(results)) {
                narows <- which(is.na(results[, i]))
                if (length(narows) > 0) {
                        results[narows, i] <- 0
                }
        }
        
        l <- list(results = results, items = items, 
                  nlevels = nlevels, levels = levels(items[, 1]))
        
        # Summary Part:
        results <- data.frame(Response = 1:nlevels)
        for(i in 1:ncol(items)) {
                t <- table(items[,i])
                t <- (t / sum(t) * 100)
                results <- cbind(results, as.data.frame(t)[2])
                names(results)[ncol(results)] <- names(items)[i]
        }
        results <- as.data.frame(t(results))
        names(results) <- levels(items[,1])
        results <- results[2:nrow(results),]
        results2 <- data.frame(Item = row.names(results),
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
        row.names(results2) = 1:nrow(results2)
        
        results2 <- results2[order(results2$high, decreasing=TRUE),]
        
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
        if(is.factor(l$results$Item)) {
                results2$Item <- factor(results2$Item, levels=levels(l$results$Item))
        }
        print(results2)
        return(l)
}


likert_Plot <- function (l) 
{
        # Likert Plot Part:
        center <- (l$nlevels - 1)/2 + 1
        if (center < 1.5 | center > (l$nlevels - 0.5)) {
                stop(paste0("Items must have 2 or more levels!"))
        }
        text.size <- 3
        text.color <- "black"
        high.color <- "#5AB4AC"
        low.color <- "#D8B365"
        neutral.color <- "grey90"
        neutral.color.ramp <- "white"
        legend <- "Response"
        legend.position <- "bottom"
        ymin <- -100
        ymax <- 100
        ybuffer <- 5
        lowrange <- 1:floor(center - 0.5)
        highrange <- ceiling(center + 0.5):l$nlevels
        
        ramp <- colorRamp(c(low.color, neutral.color.ramp))
        ramp <- rgb(ramp(seq(0, 1, length = length(lowrange) + 
                                     1)), maxColorValue = 255)
        bamp <- colorRamp(c(neutral.color.ramp, high.color))
        bamp <- rgb(bamp(seq(0, 1, length = length(highrange) + 
                                     1)), maxColorValue = 255)
        cols <- NULL
        if (center%%1 != 0) {
                cols <- c(ramp[1:(length(ramp) - 1)], bamp[2:length(bamp)])
        }
        else {
                cols <- c(ramp[1:(length(ramp) - 1)], neutral.color, bamp[2:length(bamp)])
        }
        
        lsum <- results2
        results <- stats::reshape(data = l$results, idvar = "Item",
                                  v.name = c("value"),
                                  varying = c(names(l$results[,2:length(l$results)])), 
                                  times = c(names(l$results[,2:length(l$results)])),
                                  timevar = "variable",
                                  new.row.names = 1:(length(l$results[2:length(l$results)])*length(l$results$Item)),
                                  direction = "long")
        order <- lsum[order(lsum$high), "Item"] # this order is important for low - high order of items in plot
        results$Item <- factor(results$Item, levels = order)
        order_2 <- l$levels  # this order is important for the correct legend sequence
        results$variable <- factor(results$variable, levels = order_2)
        
        p <- NULL
        
        rows <- which(results$variable %in% names(l$results)[2:(length(lowrange) + 1)])
        results[rows, "value"] <- -1 * results[rows, "value"]
        if (center%%1 == 0) {
                rows.mid <- which(results$variable %in% names(l$results)[center + 1])
                tmp <- results[rows.mid, ]
                tmp$value <- tmp$value/2 * -1
                results[rows.mid, "value"] <- results[rows.mid, "value"]/2
                results <- rbind(results, tmp)
        }
        results.low <- results[results$value < 0, ]
        results.high <- results[results$value > 0, ]
        p <- ggplot(results, aes(y = value, x = Item, group = Item)) + 
                geom_hline(yintercept = 0) + geom_bar(data = results.low[nrow(results.low):1, 
                ], aes(fill = variable), stat = "identity") + 
                geom_bar(data = results.high, aes(fill = variable), 
                         stat = "identity")
        names(cols) <- levels(results$variable)
        p <- p + scale_fill_manual(legend, breaks = names(cols), values = cols, drop = FALSE)
        
        p <- p + geom_text(data = lsum, y = ymin,  # plot.percent.low
                           aes(x = Item, label = paste0(round(low), "%")), 
                           size = text.size, 
                           hjust = 1, color = text.color)
        
        
        p <- p + geom_text(data = lsum, y = 100,   # plot.percent.high
                           aes(x = Item, label = paste0(round(high), "%")), 
                           size = text.size, 
                           hjust = -0.2, color = text.color)
        
        if (l$nlevels%%2 == 1) {    # plot.percent.neutral
                p <- p + geom_text(data = lsum, y = 0, 
                                   aes(x = Item, label = paste0(round(neutral), "%")), 
                                   size = text.size, 
                                   hjust = 0.5, color = text.color)
        }
        p <- p + coord_flip() + ylab("Percentage") + xlab("") + 
                theme(axis.ticks = element_blank())
        
        p <- p + scale_y_continuous(labels = function(x) return(abs(x)), limits = c(ymin - ybuffer, ymax + ybuffer))
        
        p <- p + theme(legend.position = legend.position)
        
        return(p)
}