# Installing jaspTools with devtools (this is required onyl once at the start)
devtools::install_github("jasp-stats/jaspTools")
library(jaspTools)
setupJaspTools() # This is required once after every reinstall of jaspTools.


# Loading jaspTools
library(jaspTools)
library(jaspBase)
library(jaspGraphs)
library(jaspResults)



setwd('/Users/lucat/OneDrive/Dokumente/Uni Amsterdam/Internship/Example Data/')
library(data.table)

dataset <- read.csv("LikertData.csv")[, -c(1,2)]
dataset <- fread("LikertData.csv")[, -c(1,2)]
class(dataset)

dataset[1:length(dataset)] <- as.factor(dataset[1:length(dataset)])

likert_Plot(dataset, "LOL")

dataset[1:length(dataset)] <- lapply(dataset[1:length(dataset)], factor)


typeof(dataset[,1])
class(dataset[,1])
dataset[,1] <- factor(dataset[,1])





x <- 1:5
is.double(x)
x_double <- as.double(x)        #
class(x)
class(x_double)
is.double(x_double)
