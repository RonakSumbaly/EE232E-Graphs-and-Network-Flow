library(doMC)
library(Kmisc)
library(readr)
library(igraph)
library(data.table)
registerDoMC(8)  # parallel processing

##### QUESTION 3 #####

cat("Running PageRank Algorithm")

imdb.page.rank = page.rank(imdb.network, directed = TRUE, damping = 0.85)
sorted.page.rank = sort(imdb.page.rank$vector, decreasing = TRUE, index.return = TRUE)

cat(sorted.page.rank$x[1:10])  # top 10 pagerank score
max.index = c(117935,66949,140481,137182,70570,60912,65575,180962,117128,32458)

cat("Top 10 Page Rank Actor/Actress Names")
all.names[max.index]
unlist(all.movies.count[max.index])

# Actor/actress with the maximum number of movies
cat("Top 10 Actor/Actress according to us")
max.movies.count = sort(as.numeric(all.movies.count), decreasing = TRUE, index.return = TRUE)
all.names[max.movies.count$ix[1:10]]
unlist(all.movies.count[max.movies.count$ix[1:10]])

# get page rank of 10 actor according to us
df.page.rank = data.frame(imdb.page.rank$vector)

cat("Page Rank Values")
imdb.page.rank$vector[as.numeric(rownames(df.page.rank)) %in% max.movies.count$ix[1:10]]

##### END OF QUESTION 3 #####
