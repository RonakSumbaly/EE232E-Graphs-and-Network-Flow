library(igraph)
library(MASS)

# get the input undirected graph
network = read.graph("facebook_combined.txt", directed=FALSE)
network_degree = degree(network)

# plot community function for QUESTION 3 & 4

plot_community = function(personal_network, core){
  # properties of the personal network plot
  node_color = rep("lightblue",vcount(personal_network))
  node_size = rep(2, vcount(personal_network))
  
  if(core){
    node_color[personal_network$names == 2048] = "red"
    node_size[personal_network$names == 2048] = 4
  }
  
  # plot the personal network of node 2048
  plot(personal_network , vertex.size = node_size, vertex.color=node_color , vertex.label=NA , asp=9/16)
  
  # fast greedy community
  fast_greedy_comm = fastgreedy.community(personal_network)
  print(modularity(fast_greedy_comm))
  print(sizes(fast_greedy_comm))
  plot(fast_greedy_comm, personal_network, vertex.size=node_size , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
  plot(personal_network , vertex.size=node_size , vertex.label=NA , vertex.color=fast_greedy_comm$membership, asp=9/16, layout=layout.fruchterman.reingold)
  
  # edge betweenness community
  edge_betweenness_comm = edge.betweenness.community(personal_network)
  print(modularity(edge_betweenness_comm))
  print(sizes(edge_betweenness_comm))
  plot(edge_betweenness_comm, personal_network, vertex.size=node_size , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
  plot(personal_network , vertex.size=node_size , vertex.label=NA , vertex.color=edge_betweenness_comm$membership, asp=9/16, layout=layout.fruchterman.reingold)
  
  # infomap community
  infomap_comm = infomap.community(personal_network)
  print(sizes(infomap_comm))
  plot(infomap_comm, personal_network, vertex.size=node_size , asp = 9/16,vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
  plot(personal_network , vertex.size=node_size , vertex.label=NA , vertex.color=infomap_comm$membership, asp=9/16, layout=layout.fruchterman.reingold)
}

##### QUESTION 3 #####

core_nodes = numeric()
for (i in V(network)) {
  if (length(neighbors(network, i)) > 200)
    core_nodes = c(core_nodes, i)
} 

avg_degree = network_degree[core_nodes]
cat("Number of Core Nodes : ", length(core_nodes))
cat("Average Degree of Core Nodes : ", mean(avg_degree))

# core node 2048 chosen for construction of personal network
nn2048_core = neighborhood(network, order = 1, nodes = 2048)
personal_nn2048 = induced.subgraph(network, unlist(nn2048_core))
personal_nn2048$names = sort(unlist(nn2048_core))
cat("Size of Neighbors of Node 2048 :" , vcount(personal_nn2048))
cat("Number of Edges in Personal Network n2048 :", ecount(personal_nn2048)) # number of edges in personal network of Node 2048

plot_community(personal_nn2048, TRUE)

##### END OF QUESTION 3 #####

##### QUESTION 4 #####

nn2048_noncore = unlist(nn2048_core)[(unlist(nn2048_core) != 2048)]
personal_nn2048_noncore = induced.subgraph(network, vids = nn2048_noncore)

cat("Size of Neighbors of Node 2048 without core:" , vcount(personal_nn2048_noncore))
cat("Number of Edges in Personal Network n2048 without core :", ecount(personal_nn2048_noncore)) 

plot_community(personal_nn2048_noncore, FALSE)

##### END OF QUESTION 4 #####
