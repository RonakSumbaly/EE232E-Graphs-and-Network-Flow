library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 9 #####

# construct a bipartite graph
edgelist = fread("movie-actor-2.csv", header = FALSE, sep = "\t")  # python script for csv
edgelist$V2 = NULL
colnames(edgelist) = c("Actors", "Movies")

bipartite.actor.movie = graph.data.frame(edgelist, directed = FALSE) # bipartite graph

map.ratings.to.movies = data.frame(movie.ratings.greater.5, row.names = movie.name.greater.5)
unique.actors.edge = unique(edgelist$Actors)
actors.node.ratings = c()

for (i in 1:length(unique.actors.edge)) {
  actors.node.ratings = c(actors.node.ratings, mean(map.ratings.to.movies[neighbors(bipartite.actor.movie, unique.actors.edge[i])$name,]))
}

# continued in Python Script for faster processing

##### END OF QUESTION 9 #####