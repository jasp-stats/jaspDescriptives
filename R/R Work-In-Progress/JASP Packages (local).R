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
write.csv(df,"LikertData_Even.csv", row.names = FALSE)



x <- 1:5
is.double(x)
x_double <- as.double(x)
class(x)
class(x_double)
is.double(x_double)
