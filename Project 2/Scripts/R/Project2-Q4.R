library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 4 #####

jaccard.weights = fread("jaccard-index.csv",header = FALSE, data.table = TRUE)  # created using Python
colnames(jaccard.weights) = c("Node 1", "Node 2", "weights")
movies.network = graph.data.frame(jaccard.weights, directed = FALSE)

##### END OF QUESTION 4 #####

