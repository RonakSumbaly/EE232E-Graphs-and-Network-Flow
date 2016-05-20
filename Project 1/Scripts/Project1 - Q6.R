library(igraph)
library(MASS)

setwd("~/Dropbox/Masters/Quarter 3/EE232E - Graph and Network Flows/Project 1/")
# get the input undirected graph
network = read.graph("facebook_combined.txt", directed=FALSE)

# get core nodes with neighbors > 200
core_nodes = numeric()
for (i in V(network)) {
  if (length(neighbors(network, i)) > 200)
    core_nodes = c(core_nodes, i)
} 
cat("Number of Core Nodes", length(core_nodes))

# construct a personal network
make_personal_network = function(network, core){
  nn_core = neighborhood(network, order = 1, nodes = core)
  personal_nn = induced.subgraph(network, unlist(nn_core))
  personal_nn$names = sort(unlist(nn_core))
  return(personal_nn)
}

# features considered for analysis
clustering_coefficient = list(rep(0,length(core_nodes))) # clustering coefficient
density_graph = list(rep(0,length(core_nodes)))  # density of graph
community_sizes = list(rep(0,length(core_nodes)))  # community sizes

# loop through each core-node to find feature values
for(index in 1:length(core_nodes)) # consider each core node
{
  cat("Currently Running: ", index, "\n")
  core_node = core_nodes[index]
  personal_nn_core_node = make_personal_network(network, core_node) # make personal network
  
  fast_greedy_comm = fastgreedy.community(personal_nn_core_node)
  size = as.vector(sizes(fast_greedy_comm))
  
  # initializing features
  clustering_coefficient[[index]] = rep(NA, length(size))
  density_graph[[index]] = rep(NA, length(size))
  community_sizes[[index]] = size # add community sizes
  
  # for each community of the core_node
  for(i in 1:length(fast_greedy_comm))
  {
    # seggregating community and non-community nodes within each community
    community_i_nodes = which(fast_greedy_comm$membership==i)
    noncommunity_i_nodes = which(fast_greedy_comm$membership!=i)
    vids = as.character(unlist(community_i_nodes))
    
    # construct community network from scratch
    community = induced.subgraph(personal_nn_core_node, vids)
    community$names = sort(unlist(community_i_nodes))
    
    # consider only those communities whose size > 10
    if(length(community_i_nodes) >= 10) 
    {
      Kv = degree(personal_nn_core_node)[which(personal_nn_core_node$names == core_node)] # calculating clustering coefficient
      count = 0
      
      # calculating number of direct links between neighbors of index in community
      pairs_community_i_nodes = combn(community_i_nodes, 2, FUN = NULL, simplify = TRUE)
      for(pair in 1: (length(pairs_community_i_nodes)/2))
      {
        
        node1 = pairs_community_i_nodes[,pair][1]
        node2 = pairs_community_i_nodes[,pair][2]
        if (community[which(community$names == node1),which(community$names == node2),edges=TRUE]!=0)
          count = count+1;
      }
      
      temp_cc = (2*count)/(Kv*(Kv - 1)) # calculating clustering coefficient
      temp_dv = graph.density(community, loops=TRUE) # calculating density
    }
    else
    {  
      temp_cc = NA
      temp_dv = NA
    }
    clustering_coefficient[[index]][i] = temp_cc;
    density_graph[[index]][i] = temp_dv;
  }
}
for (i in 1:length(core_nodes)) {
  cat("Index: ", i, "\n")
  cat("Clustering Coefficient", "\n")
  print(clustering_coefficient[i])
  cat("Density of Graph")
  print(density_graph[i])
  cat("Community Sizes")
  print(community_sizes[i])
}
