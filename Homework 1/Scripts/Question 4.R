# load igraph library
library("igraph") 

#### QUESTION 4 - PART A ####
nodes <- 1000
forest_fire_graph <- forest.fire.game(nodes = nodes, fw.prob=0.37, bw.factor=0.32/0.37)
in_degree <- degree(forest_fire_graph,mode="in")
out_degree <- degree(forest_fire_graph,mode="out")
hist(in_degree, breaks = seq(from = min(in_degree), to = max(in_degree), by=1),  main = "In-Degree Distirbution", xlab = "In-Degree", ylab = "Density")
hist(out_degree, breaks = seq(from = min(out_degree), to = max(out_degree), by=1),  main = "Out-Degree Distirbution", xlab = "Out-Degree", ylab = "Density")

#### QUESTION 4 - PART B & C ####

cat("Diameter of Forest Fire Graph : ", diameter(forest_fire_graph))

clusters_graph <- clusters(forest_fire_graph)  # get all clusters from graph
index_GCC <- which.max(clusters_graph$csize)  # get the cluster with the maximum size i.e. GCC
non_GCC_nodes <- (1:vcount(forest_fire_graph))[clusters_graph$membership != index_GCC]  # get nodes which are not present in GCC cluster
GCC <- delete.vertices(forest_fire_graph, non_GCC_nodes)  # remove nodes from the graph to get only GCC
community_GCC <- spinglass.community(GCC)  # get community of GCC 
modularity_GCC <- modularity(community_GCC)  # get modularity of GCC

plot(community_GCC, forest_fire_graph)    
cat("Modularity of the Graph : ", modularity_GCC)
