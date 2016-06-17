library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(stringr)
library(MASS)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 8 #####

# get page rank features
imdb.page.rank = data.frame(imdb.page.rank$vector)  # from question 3
page.rank.actor = cbind(rownames(imdb.page.rank), imdb.page.rank[,1])

page.rank.movies = fread("page_rank_movies_greater_5.csv", header = FALSE)  # python script for csv
page.rank.movies = page.rank.movies[-node.not.there,]

movies.network$page.rank = page.rank.movies

# get directors of movies
directors.movies = readlines("director_movies.txt")

Encoding(directors.movies) = 'latin1'
directors.movies = iconv(directors.movies, "latin1", "ASCII", sub="")

directors.movies = gsub("^\\(", "", directors.movies)
directors.movies = gsub("\\([^[:digit:]]+\\)", "", directors.movies)
directors.movies = gsub("^\\s+|\\s+$", "", directors.movies)
directors.movies = gsub("\\s?\\{[^}]+\\}\\}", "", directors.movies)

# write.table(directors.movies, "director_movies_processed.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

directors.movies.greater.5 = fread("directors_greater_5.csv", sep = "\t", header = FALSE)
directors.movies.greater.5 = directors.movies.greater.5[-node.not.there,]

top100.directors = fread("directors_top_100.csv", header = FALSE, sep = "\t")

feature.directors = fread("feature_director.csv", header = FALSE, sep = "\t")
feature.directors = feature.directors[-node.not.there,]

movies.network$directors = feature.directors

feature.matrix = cbind(movies.network$page.rank, movies.network$directors)
feature.class = movies.network$ratings

batman.features = feature.matrix[tag.batman,]
mission.features = feature.matrix[tag.mission,]
minion.features = feature.matrix[tag.minions,]
colnames(mission.features) = c("PageRank1","PageRank2","PageRank3","PageRank4","PageRank5","Director")
colnames(batman.features) = c("PageRank1","PageRank2","PageRank3","PageRank4","PageRank5","Director")
colnames(minion.features) = c("PageRank1","PageRank2","PageRank3","PageRank4","PageRank5","Director")

model.data = cbind(feature.matrix, feature.class)
colnames(model.data) = c("PageRank1","PageRank2","PageRank3","PageRank4","PageRank5","Director","Ratings")

model.reg = lm(Ratings ~ ., data = model.data)
# summary(model.reg)

cat("Predicted Batman v Superman: Dawn of Justice (2016) Rating:", predict(model.reg, batman.features))
cat("Predicted Mission: Impossible - Rogue Nation (2015) Rating:",  predict(model.reg, mission.features))
cat("Predicted Minions (2015) Rating:", predict(model.reg, minion.features))

##### END OF QUESTION 8 #####