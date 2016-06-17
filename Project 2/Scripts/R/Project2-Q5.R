library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 5 #####
# perform fast greedy community on network
movies.network.community = fastgreedy.community(movies.network, weights =  E(movies.network)$weights)

# genre list 
movie.genre.list = fread("movie_genre.txt", sep = "\t", header = FALSE)
movie.genre.list$V2 = NULL

# pre-process movie-genre list similar to Q1
Encoding(movie.genre.list$V1) = 'latin1'
movie.genre.list$V1 = iconv(movie.genre.list$V1, "latin1", "ASCII", sub="")

movie.genre.list$V1 = gsub("^\\(", "", movie.genre.list$V1)
movie.genre.list$V1 = gsub("\\([^[:digit:]]+\\)", "", movie.genre.list$V1)
movie.genre.list$V1 = gsub("^\\s+|\\s+$", "", movie.genre.list$V1)

# get movies and genres with greater than 5 actors/actress
movie.name.greater.5 = fread("movies-names-greater-5.csv", header = FALSE)
movie.genres.greater.5 = fread("genre-list-greater-5.csv", header= FALSE)

length(V(movies.network)) # number of vertices in network

# check for numbering of the graph 
node.1 = unique(jaccard.weights$`Node 1`)
node.2 = unique(jaccard.weights$`Node 2`)
sum.node = union(node.1,node.2)
node.all = 1:length(movie.genres.greater.5)

movie.genres.greater.5 = movie.genres.greater.5[-setdiff(node.all,sum.node),]
movie.name.greater.5 = movie.name.greater.5[-setdiff(node.all,sum.node),]
movie.genres.greater.5 = unlist(movie.genres.greater.5)

# add genres to the movies network
movies.network$genre = movie.genres.greater.5

# get number of communities
no.communities = sort(unique(movies.network.community$membership))

# get summary of communities
table(movies.network.community$membership)

# tag communities based on the count of genres
tag.communities = c()
for (i in no.communities) {
  community.genres = (movies.network$genre[which(movies.network.community$membership == i)])
  summ.community = table(community.genres)
  percent.20 = length(community.genres) * 0.2
  tag.communities = c(tag.communities, list(summ.community[which(summ.community > percent.20)]))
}
cat("Tagged Genres of communities")
print((tag.communities))

##### END OF QUESTION 5 #####

