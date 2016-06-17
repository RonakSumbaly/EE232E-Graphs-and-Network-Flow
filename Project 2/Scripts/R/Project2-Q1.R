library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 1 #####
##### PART 1 - Get names of actors and actress with more than 4 movies #####

setwd("~/Dropbox/Masters/Quarter 3/EE232E - Graph and Network Flows/Project 2/project_2_data")
actors.movies = read_delim("actor_movies.txt" , col_names = paste0("V",seq_len(15)), delim = "\t", quote = "\"")
actress.movies = read_delim("actress_movies.txt" , col_names = paste0("V",seq_len(15)), delim = "\t")

# get actor and actress names
actors.names = actors.movies$V1
actress.names = actress.movies$V1

# remove names from the table
actors.movies$V1 = NULL
actress.movies$V1 = NULL

# get actors and actress names who have more than 4 movies
actors.number.movies = matrix(apply(actors.movies, 1, function(x) (14 - sum(is.na(x)))), ncol = 1)
actress.number.movies = matrix(apply(actress.movies, 1, function(x) (14 - sum(is.na(x)))), ncol = 1)
actors.names =  actors.names[which(actors.number.movies >= 5)]
actress.names =  actress.names[which(actress.number.movies >= 5)]

##### END OF PART 1 #####

##### PART 2 - Get movies of actors and actress found from above #####
cat("Considering only movies which have actors > 5")
actors.content = readlines("actor_movies.txt")
actress.content = readlines("actress_movies.txt")

actors.content = actors.content[which(actors.number.movies >= 5)]
actress.content = actress.content[which(actress.number.movies >= 5)]

##### END OF PART 2 #####

##### PART 3 - Parse movie content to remove erroneous content #####
cat("Processing the content of movies")
# encoding of string
Encoding(actress.content) = 'latin1'
actress.content = iconv(actress.content, "latin1", "ASCII", sub="")

Encoding(actors.content) = 'latin1'
actors.content = iconv(actors.content, "latin1", "ASCII", sub="")

# split the movie content
actors.movies = foreach(i=1:length(actors.content)) %dopar% str_split(actors.content[i], sep = "\t\t")[, -1]
actress.movies = foreach(i=1:length(actress.content)) %dopar% str_split(actress.content[i], sep = "\t\t")[, -1]

# remove (????) 
# truncate spaces at beginning and end
actors.movies = foreach(i=1:length(actors.content)) %dopar% gsub("^\\(", "", actors.movies[i][[1]])
actors.movies = foreach(i=1:length(actors.content)) %dopar% gsub("\\([^[:digit:]]+\\)", "", actors.movies[i][[1]])
actors.movies = foreach(i=1:length(actors.content)) %dopar% gsub("^\\s+|\\s+$", "", actors.movies[i][[1]])

actress.movies = foreach(i=1:length(actress.content)) %dopar% gsub("^\\(", "", actress.movies[i][[1]])
actress.movies = foreach(i=1:length(actress.content)) %dopar% gsub("\\([^[:digit:]]+\\)", "", actress.movies[i][[1]])
actress.movies = foreach(i=1:length(actress.content)) %dopar% gsub("^\\s+|\\s+$", "", actress.movies[i][[1]])

actors.movies.count = matrix(foreach(i=1:length(actors.content)) %dopar% length(actors.movies[[i]]), ncol = 1)
actress.movies.count = matrix(foreach(i=1:length(actress.content)) %dopar% length(actress.movies[[i]]), ncol = 1)

rm(actors.content)
rm(actress.content)

##### END OF PART 3 #####
##### END OF QUESTION 1 #####

