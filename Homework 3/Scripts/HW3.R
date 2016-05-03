library(igraph)
library(netrw)

##### QUESTION 1 #####
# get the input directed graph
graph_data = read.table("sorted_directed_net.txt", sep = "\t", header = FALSE) # read sorted directed network
colnames(graph_data) = c("Node 1", "Node 2", "weights")
directed_network = graph.data.frame(graph_data, directed=TRUE)

# check for connectivity of graph
connectivity = is.connected(directed_network)
connectivity

# get giant connected component
if(!connectivity) {
  clusters_graph = clusters(directed_network)  # get all clusters from graph
  index_GCC = which.max(clusters_graph$csize)  # get the cluster with the maximum size i.e. GCC
  non_GCC_nodes = (1:vcount(directed_network))[clusters_graph$membership != index_GCC]  # get nodes which are not present in GCC cluster
  giant_connected_component = delete.vertices(directed_network, non_GCC_nodes)  # remove nodes from the graph to get only GCC
}

##### QUESTION 2 #####
# plot in-degree and out-degree distribution

in_degree = degree(giant_connected_component, mode="in")
out_degree = degree(giant_connected_component, mode="out")

hist(in_degree, breaks = seq(from = min(in_degree), to = max(in_degree), by=1),  main = "In-Degree Distirbution", xlab = "In-Degree", ylab = "Density", border="blue", col="green")
hist(out_degree, breaks = seq(from = min(out_degree), to = max(out_degree), by=1),  main = "Out-Degree Distirbution", xlab = "Out-Degree", ylab = "Density",border="red", col="green")

##### QUESTION 3 #####
# convert directed to undirected graph
# option 1 : keep the number of edges unchanged, and just remove the directions

undirected_option_1 = as.undirected(giant_connected_component, mode = "each")
community_option_1 = label.propagation.community(undirected_option_1, weights = E(undirected_option_1)$weights)
modularity(community_option_1)
sizes(community_option_1)

# option 2 : merge the two directed edges between i and j
sqrt_weights = function(weight) sqrt(prod(weight))

undirected_option_2 = as.undirected(giant_connected_component, mode = "collapse", edge.attr.comb = sqrt_weights)

community_option_2a = label.propagation.community(undirected_option_2, weights = E(undirected_option_2)$weights)
modularity(community_option_2a)
sizes(community_option_2a)

community_option_2b = fastgreedy.community(undirected_option_2, weights = E(undirected_option_2)$weights)
modularity(community_option_2b)
sizes(community_option_2b)

##### QUESTION 4 #####
# Sub Giant Connected Component
index_sub_GCC = which.max(sizes(community_option_2b))
non_sub_GCC_nodes = (1:vcount(undirected_option_2))[community_option_2b$membership != index_sub_GCC]
sub_giant_connected_component = delete.vertices(undirected_option_2, non_sub_GCC_nodes)

community_sub_GCC = fastgreedy.community(sub_giant_connected_component, weights=E(sub_giant_connected_component)$weights)
modularity(community_sub_GCC)
sizes(community_sub_GCC)

##### QUESTION 5 #####
# Sub-communities with size > 100
community_option_2a_100 = which(sizes(community_option_2a)>100)
community_option_2b_100 = which(sizes(community_option_2b)>100)

modularity_100_2a = c()
sizes_100_2a = c()
for (i in 1:length(community_option_2a_100)) {
  index_nodes = (1:vcount(undirected_option_2))[community_option_2a$membership != community_option_2a_100[i]]
  GCC_nodes = delete.vertices(undirected_option_2, index_nodes)
  sub_GCC = fastgreedy.community(GCC_nodes)
  modularity_100_2a = c(modularity_100_2a, c(modularity(sub_GCC)))
  sizes_100_2a = c(sizes_100_2a, sizes(sub_GCC))
}
modularity_100_2a
sizes_100_2a

modularity_100_2b = c()
sizes_100_2b = c()
for (i in 1:length(community_option_2b_100)) {
  index_nodes = (1:vcount(undirected_option_2))[community_option_2b$membership != community_option_2b_100[i]]
  GCC_nodes = delete.vertices(undirected_option_2, index_nodes)
  sub_GCC = fastgreedy.community(GCC_nodes)
  modularity_100_2b = c(modularity_100_2b, modularity(sub_GCC))
  sizes_100_2b = c(sizes_100_2b, sizes(sub_GCC))
}
modularity_100_2b
sizes_100_2b


##### QUESTION 6 #####
# RANDOM WAlK USING PERSONIZED PAGE RANK #

multi_community = numeric(0)
# visit probability of each node
for (i in 1:vcount(directed_network)) {
  teleportation_prob = rep(0, vcount(directed_network))
  teleportation_prob[i] = 1 # local page-rank 
    
  page_rank = netrw(directed_network , walker.num = 1, start.node = i,
                    damping = 0.85, teleport.prob = teleportation_prob, output.visit.prob = TRUE) # random walk on node_i
  
  visit_prob = page_rank$ave.visit.prob
  top_visit_prob = sort(visit_prob, decreasing = TRUE, index.return = TRUE) # sort the visit_prob and keep index
  M_i = rep(0, length(community_option_2a)) 
  
  for (j in 1:30) { # loop through only top 30 visit_prob
    m_j = rep(0, length(community_option_2a))
    m_j[community_option_2a$membership[which(V(giant_connected_component) == V(directed_network)[top_visit_prob$ix[j]])]] = 1
    M_i = M_i + top_visit_prob$x[j] * m_j
  }
  
  if (length(which(M_i > 0.1)) >= 2) { # change threshold and see if there are more than 2 communities with this value
    multi_community = rbind(multi_community, c(i, M_i)) # store results and community score
  }
}
multi_community
