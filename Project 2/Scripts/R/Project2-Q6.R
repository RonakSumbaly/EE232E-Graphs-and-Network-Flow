library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 6 #####

movie.name.greater.5 = fread("movies-names-greater-5.csv", header = FALSE)
node.not.there = c(89967, 230839)  # nodes not present in the network but have actors > 5
movie.name.greater.5 = movie.name.greater.5[-node.not.there,]

# get communities of the movies
batman.superman = movies.network.community$membership[which(movie.name.greater.5 == "Batman v Superman: Dawn of Justice (2016)")]
mission.impossible = movies.network.community$membership[which(movie.name.greater.5 == "Mission: Impossible - Rogue Nation (2015)")]
minions = movies.network.community$membership[which(movie.name.greater.5 == "Minions (2015)")]

cat("Community of 3 movies\n")
cat("Batman v Superman: Dawn of Justice (2016): ", batman.superman , "\n")
cat("Mission: Impossible - Rogue Nation (2015): ", mission.impossible , "\n")
cat("Minions (2015): ", minions , "\n")

batman.superman.neighbors = neighbors(movies.network, batman.superman)
mission.impossible.neighbors = neighbors(movies.network, mission.impossible)
minions.neighbors = neighbors(movies.network, minions)

# reset position 
movie.name.greater.5 = fread("movies-names-greater-5.csv", header = FALSE)

# get index of the movies
tag.batman = which(movie.name.greater.5 == "Batman v Superman: Dawn of Justice (2016)")
tag.mission = which(movie.name.greater.5 == "Mission: Impossible - Rogue Nation (2015)")
tag.minions = which(movie.name.greater.5 == "Minions (2015)")

# get the top 5 neighbors
for (tag in c(tag.batman, tag.mission, tag.minions)){

  edges.node = jaccard.weights[which(jaccard.weights$"Node 1" == tag | jaccard.weights$"Node 2" == tag),]
  edges.node =  edges.node[order(edges.node$"weights"),]
  
  top.neighbor = data.frame(edges.node[1:5,])
  top.neighbor$"weights" = NULL
  
  node.id = c()
  for (i in 1:5){
    node.id = c(node.id,top.neighbor[i,which((top.neighbor[i,])!=tag)])
  }
  
  print (movie.name.greater.5[node.id])
  print (movies.network.community$membership[node.id])
}

##### END OF QUESTION 6 #####
