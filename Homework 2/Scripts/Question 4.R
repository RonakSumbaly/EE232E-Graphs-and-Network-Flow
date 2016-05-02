library(igraph)
library(netrw)

#### QUESTION 4 - PART A ####

random_network = random.graph.game(n = 1000, p = 0.01, directed = TRUE)
page_rank = netrw(random_network, walker.num = 1000, damping = 0.85, T = 1000, output.walk.path = TRUE) # get vertex sequence of random walk
plot(page_rank$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",main="Simulate PageRank with Random Walk ",pch=1)

#### QUESTION 4 - PART B ####

random_network = random.graph.game(n = 1000, p = 0.01, directed = TRUE)
page_rank = netrw(random_network, walker.num = 1000, damping = 0.85, T = 1000, output.walk.path = TRUE) # get vertex sequence of random walk
teleportation_prob = page_rank$ave.visit.prob
teleport_page_rank = netrw(random_network, walker.num = 1000, damping = 0.85, T = 1000, teleport.prob = teleportation_prob, output.walk.path = TRUE)

relationship = cor(teleportation_prob, teleport_page_rank$ave.visit.prob)
cat("Correlation between page-rank and page-rank (with teleportation): ", relationship)

plot(teleport_page_rank$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",main="Personalized PageRank with Random Walk ",pch=1)
