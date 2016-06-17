library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 7 #####

# get movie ratings
movie.ratings = fread("movie_rating.txt", header = FALSE, sep = "\t")
movie.ratings$V2 = NULL

# pre-process movie-rating list similar to Q1
Encoding(movie.ratings$V1) = 'latin1'
movie.ratings$V1 = iconv(movie.ratings$V1, "latin1", "ASCII", sub="")

movie.ratings$V1 = gsub("^\\(", "", movie.ratings$V1)
movie.ratings$V1 = gsub("\\([^[:digit:]]+\\)", "", movie.ratings$V1)
movie.ratings$V1 = gsub("^\\s+|\\s+$", "", movie.ratings$V1)

# get ratings of movies with greater than 5 actors (basically nodes of the network)
movie.name.greater.5 = unlist(fread("movies-names-greater-5.csv", header = FALSE))

movie.ratings.greater.5 = fread("rating-list-greater-5.csv", header = FALSE)
node.not.there = c(89967, 230839)  # nodes not present in the network but have actors > 5
movie.ratings.greater.5 = movie.ratings.greater.5[-node.not.there,]
movie.name.greater.5 = movie.name.greater.5[-node.not.there]
movies.network$ratings = movie.ratings.greater.5

# get index of the movies
tag.batman = which(movie.name.greater.5 == "Batman v Superman: Dawn of Justice (2016)")
tag.mission = which(movie.name.greater.5 == "Mission: Impossible - Rogue Nation (2015)")
tag.minions = which(movie.name.greater.5 == "Minions (2015)")
tag = c(tag.batman, tag.mission, tag.minions)

k = 20
names.q = c("Batman v Superman: Dawn of Justice (2016)", "Mission: Impossible - Rogue Nation (2015)",  "Minions (2015)")
actual.q = c(7.1,7.5,6.4)

for (m in 1:3) {
  community = movies.network.community$membership[tag[m]]
  community.member.ratings = movies.network$ratings[which(movies.network.community$membership == community)]
  
  # non - zero ratings
  community.member.ratings = community.member.ratings[which(community.member.ratings != 0)]
  
  # k-nearest neigbhors ratings
  edges.node = jaccard.weights[which(jaccard.weights$"Node 1" == tag[m] | jaccard.weights$"Node 2" == tag[m]),]
  edges.node =  edges.node[order(edges.node$"weights"),]
  
  top.neighbor = data.frame(edges.node[1:k,])
  top.neighbor$"weights" = NULL
  
  node.id = c()
  for (i in 1:k){
    node.id = c(node.id,top.neighbor[i,which((top.neighbor[i,])!=tag[m])])
  }
  nearest.neighbor.rating = movie.ratings.greater.5[node.id]
  nearest.neighbor.rating = nearest.neighbor.rating[which(nearest.neighbor.rating != 0)]
  
  neigbhors = as.numeric(unlist(neighbors(movies.network, tag[m])))
  neighbors.ratings = movie.ratings.greater.5[neigbhors]
  neighbors.ratings = neighbors.ratings[which(neighbors.ratings != 0)]
  
  ratings.movie.q = c(as.numeric(unlist(community.member.ratings)), as.numeric(unlist(nearest.neighbor.rating)), as.numeric(unlist(neighbors.ratings)))
  cat(names.q[m], actual.q[m],"\n")
  cat("Community Rating ", mean(as.numeric(unlist(community.member.ratings))), "\n")
  cat("Nearest Neighbor Rating (k=20) ", mean(as.numeric(unlist(nearest.neighbor.rating))), "\n")
  cat("Neighbors Rating", mean(as.numeric(unlist(neighbors.ratings))), "\n")
  cat("All Combined ", mean(ratings.movie.q), "\n")
  cat("\n")
}

##### END OF QUESTION 7 #####
