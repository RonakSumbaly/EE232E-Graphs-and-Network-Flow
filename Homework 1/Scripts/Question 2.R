# load igraph library
library("igraph") 

#### Supporting Functions ####

calculateModularity <- function(fat_tail_graph, nodes){
  
  clusters_graph <- clusters(fat_tail_graph)  # get all clusters from graph
  index_GCC <- which.max(clusters_graph$csize)  # get the cluster with the maximum size i.e. GCC
  non_GCC_nodes <- (1:vcount(fat_tail_graph))[clusters_graph$membership != index_GCC]  # get nodes which are not present in GCC cluster
  GCC <- delete.vertices(fat_tail_graph, non_GCC_nodes)  # remove nodes from the graph to get only GCC
  community_GCC <- fastgreedy.community(GCC)  # get community of GCC
  modularity_GCC <- modularity(community_GCC)  # get modularity of GCC

  if (nodes == 1000) {
    plot(community_GCC, fat_tail_graph)    
  }
  return(modularity_GCC)
}


nodes <- 1000
fat_tail_graph <- barabasi.game(n = nodes, directed = FALSE)
degree_graph <- degree(fat_tail_graph)
degree_dist <- hist(x = degree_graph, breaks = seq(from = min(degree_graph), to = max(degree_graph), by=1), main = "Degree Distribution for Fat Tail Distribution", xlab = "Number of Degrees")
cat("Diameter of Fat Tail Distribution Graph : ", diameter(fat_tail_graph))

#### QUESTION 2 - PART B ####
# Finding GCC, Community Structure and Modularity, and Connectivity 

cat("Fat Tail Distribution Graph is connected :", is.connected(fat_tail_graph))
cat("Modularity of the Graph : ", calculateModularity(fat_tail_graph, 1000))

#### QUESTION 2 - PART C ####

nodes <- 10000
modularity_GCC_10000 <- 0 

for (i in 1:100) {
  fat_tail_graph <- barabasi.game(n = nodes, directed = FALSE)
  modularity_GCC_10000 <- modularity_GCC_10000 + calculateModularity(fat_tail_graph, 10000)
}

cat("Modularity of the Graph with 10000 nodes : ", modularity_GCC_10000/100)

#### QUESTION 2 - PART D ####

nodes <- 1000
fat_tail_graph <- barabasi.game(n = nodes, directed = FALSE)
degree_dist_neighbor <- numeric(0)

for (i in 1:1000) {
  i_node <- sample(nodes, 1)   # get random number between 1 to number of nodes
  neighors_i <- neighbors(fat_tail_graph, i_node)  # get neighbors of node_i 
  if (length(neighors_i) == 1) { j_node <- neighors_i }  # if only one neighbor then assign as node_j
  else { j_node <- sample(neighors_i, 1) }  # else randomly choose one neighbor for node_j
  degree_dist_neighbor <- c(degree_dist_neighbor, degree(fat_tail_graph, j_node))  # append degree of node_j for the graph
}

hist(degree_dist_neighbor, main='Degree Distribution of jth node',  breaks = seq(from = min(degree_dist_neighbor), to = max(degree_dist_neighbor), by=1), xlab='Degree', ylab='Frequency')




