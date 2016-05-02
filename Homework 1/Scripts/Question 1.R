# load igraph library
library("igraph") 

#### QUESTION 1 - PART A ####

# vector to probabilities of drawing edge
probabilities <- c(0.01, 0.05, 0.1) 
nodes <- 1000
# draw random undirected graphs with above probabilities
graph_1 <- random.graph.game(n = nodes, p = probabilities[1],directed = FALSE)
graph_2 <- random.graph.game(n = nodes, p = probabilities[2],directed = FALSE)
graph_3 <- random.graph.game(n = nodes, p = probabilities[3],directed = FALSE)

# get degrees of all graphs in vector format
degree_1 <- degree(graph = graph_1)
degree_2 <- degree(graph = graph_2)
degree_3 <- degree(graph = graph_3)

# plot the degree distribution for each graph
degree_dist_1 <- hist(x = degree_1, breaks = seq(from = min(degree_1), to = max(degree_1), by=1), main = "Degree Distribution for Random Undirected Graph (with p=0.01)", xlab = "Number of Degrees")
degree_dist_2 <- hist(x = degree_2, breaks = seq(from = min(degree_2), to = max(degree_2), by=1), main = "Degree Distribution for Random Undirected Graph (with p=0.05)", xlab = "Number of Degrees")
degree_dist_3 <- hist(x = degree_3, breaks = seq(from = min(degree_3), to = max(degree_3), by=1), main = "Degree Distribution for Random Undirected Graph (with p=0.1)", xlab = "Number of Degrees")

#### QUESTION 1 - PART B ####

cat("Random Undirected Graph (with p=0.01) is ", (is.connected(graph_1)), " CONNECTED with average diameter :", (diameter(graph_1)))
cat("Random Undirected Graph (with p=0.05) is ", (is.connected(graph_1)), " CONNECTED with average diameter :", (diameter(graph_2)))
cat("Random Undirected Graph (with p=0.1) is ", (is.connected(graph_1)), " CONNECTED with average diameter :",  (diameter(graph_3)))

# obtain average of connectivity and diameter 
connected_1 <- connected_2 <- connected_3 <- 0
diameter_1 <- diameter_2 <- diameter_3 <- 0

# iteration 100 times to get better observation
for (i in 2:100) {
  connected_1 <- connected_1 + is.connected(graph_1)
  connected_2 <- connected_2 + is.connected(graph_2)
  connected_3 <- connected_3 + is.connected(graph_3)
  
  diameter_1 <- diameter_1 + diameter(graph_1)
  diameter_2 <- diameter_2 + diameter(graph_2)  
  diameter_3 <- diameter_3 + diameter(graph_3)
  
  graph_1 <- random.graph.game(n = nodes, p = probabilities[1],directed = FALSE)
  graph_2 <- random.graph.game(n = nodes, p = probabilities[2],directed = FALSE)
  graph_3 <- random.graph.game(n = nodes, p = probabilities[3],directed = FALSE)
  
}

cat("Random Undirected Graph (with p=0.01) is ", (connected_1), "% CONNECTED with average diameter :", (diameter_1 / 100))
cat("Random Undirected Graph (with p=0.05) is ", (connected_2), "% CONNECTED with average diameter :", (diameter_2 / 100))
cat("Random Undirected Graph (with p=0.1) is ", (connected_3), "% CONNECTED with average diameter :", (diameter_3 / 100))

#### QUESTION 1 - PART C ####

# obtain average of prob_c for better analysis
probability_C <- 0

# iterate 100 times to get better observations
for (i in 1:100) {
  probability_c <- 0.000
  graph_c <- random.graph.game(n = nodes, p = probability_c,directed = FALSE)
  while(!(is.connected(graph_c))) {
    probability_c <- probability_c + 0.001
    graph_c <- random.graph.game(n = nodes, p = probability_c,directed = FALSE)
  }
  probability_C <- probability_C + probability_c
}

cat("Average probability_c value generated for random network is : ", (probability_C / 100)) 


