# load igraph library
library("igraph") 

#### QUESTION 3 - PART A ####

nodes <- 1000
evolving_graph <- aging.prefatt.game(n = nodes, pa.exp=1, aging.exp=-1, directed = FALSE)
degree_graph <- degree(evolving_graph)
degree_dist <- hist(x = degree_graph, breaks = seq(from = min(degree_graph), to = max(degree_graph), by=1), main = "Degree Distribution for Evolving Graph", xlab = "Number of Degrees")

#### QUESTION 3 - PART B ####

clusters_graph <- clusters(evolving_graph)  # get all clusters from graph
index_GCC <- which.max(clusters_graph$csize)  # get the cluster with the maximum size i.e. GCC
non_GCC_nodes <- (1:vcount(evolving_graph))[clusters_graph$membership != index_GCC]  # get nodes which are not present in GCC cluster
GCC <- delete.vertices(evolving_graph, non_GCC_nodes)  # remove nodes from the graph to get only GCC
community_GCC <- fastgreedy.community(GCC)  # get community of GCC
modularity_GCC <-  modularity(community_GCC)  # get modularity of GCC

plot(community_GCC, evolving_graph)    
cat("Modularity of the Graph : ", modularity_GCC)
