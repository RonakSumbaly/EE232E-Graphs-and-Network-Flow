library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 2 #####
##### PART 1 - Map movies with actor/actress index #####

# get all movies in one place
all.movies = c(unlist(actors.movies), unlist(actress.movies))

cat("Number of Unique Movies = ", length(unique(all.movies)))

# create index of which movie belongs to which actor or actress by repeating the (index * movie.count)
actor.index = c()
actress.index = c()
extra = length(actors.names)
actor.index = foreach(i=1:length(actors.names)) %dopar% c(actor.actress.index, rep(i, actors.movies.count[i]))
actress.index = foreach(i=1:length(actress.names)) %dopar% c(actor.actress.index, rep(i, actress.movies.count[i]))
actress.index = unlist(actress.index) + extra

actor.actress.index = c(unlist(actor.index), unlist(actress.index))

##### END OF PART 1 #####

##### PART 2 - Create hash-table of movies vs. actor index #####
cat("Creating Hash-table")
# get hash table and only consider values in this table
hash.table = matrix(c(all.movies, actor.actress.index), ncol = 2)
hash.table = hash.table[-which(hash.table[,1] == ""),]

unique.movies = unlist(unique(hash.table[,1]))
movies.actors.count = as.data.frame(table(hash.table[,1]))  # summary of the hash-table
common.movies = movies.actors.count[which(movies.actors.count[,2] >= 2), ]
consider.movies = common.movies[,1]

# consider only hash.table
reduced.hash.table = data.frame(hash.table[hash.table[,1] %in% consider.movies, ])

# aggreate hash table
aggregate.hash.table = reduced.hash.table[, .(X2 = list(X2)), by = X1]
cat("Finished creating Hash-table")
##### END OF PART 2 #####

##### PART 3 - Create main variables  #####
cat("Creating all of the main variables")
# main variables
all.names = c(actors.names, actress.names)
all.movies = c(actors.movies, actress.movies)
all.movies.count = rbind(actors.movies.count, actress.movies.count)

# read movie combinations and weights # created from Python script
all.combinations = fread("combinations.csv", header = FALSE, data.table = TRUE)
all.common.movies.count = fread("weights.csv", header = FALSE, data.table = TRUE)

# calculate weights
weights.original = all.common.movies.count$V3 / as.numeric(all.movies.count[all.common.movies.count$V1])
weights.reverse = all.common.movies.count$V3 / as.numeric(all.movies.count[all.common.movies.count$V2])

# create data.frame of graph
column.1 = matrix(c(all.common.movies.count$V1, all.common.movies.count$V2), ncol = 1)
column.2 = matrix(c(all.common.movies.count$V2, all.common.movies.count$V1), ncol = 1)
column.3 = matrix(c(weights.original, weights.reverse), ncol = 1)

cat("Creating IMDB network")
main.edge.table = cbind(column.1, column.2, column.3)
colnames(main.edge.table) = c("Node 1", "Node 2", "weights")
imdb.network = graph.data.frame(main.edge.table, directed=TRUE)

##### END OF PART 3 #####
##### END OF QUESTION 2 #####
