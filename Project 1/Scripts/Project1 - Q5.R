library(igraph)
library(MASS)

# get the input undirected graph
network = read.graph("facebook_combined.txt", directed=FALSE)
network$names = V(network)

# get core nodes with neighbors > 200
core_nodes = numeric()
for (i in V(network)) {
  if (length(neighbors(network, i)) > 200)
    core_nodes = c(core_nodes, i)
} 

##### QUESTION 5 #####

# construct a personal network
make_personal_network = function(network, core){
  nn_core = neighborhood(network, order = 1, nodes = core)
  personal_nn = induced.subgraph(network, unlist(nn_core))
  personal_nn$names = sort(unlist(nn_core))
  return(personal_nn)
}

mutual_friends = function(network, i, j){
  neighbor_of_i = neighbors(network, i)
  neighbor_of_j = neighbors(network, j)
  mutual_friends_ij = intersect(neighbor_of_i, neighbor_of_j)
  return(mutual_friends_ij)
}

# calculate embeddness of the network
calculate_embeddness = function(network, i, j){
  return(length(mutual_friends(network, i, j)))
}

# calculate dispersion of the network
calculate_dispersion = function(network, i, j)
{
  mutual_friends_ij = mutual_friends(network, i, j)
  dispersion_sub_graph = delete.vertices(network, c(which(network$names == i), which(network$names == j)))
  dispersion_sub_graph$names = sort(intersect(which(network$names != i), which(network$names != j)))
  
  disp_temp = numeric(0)
  
  if (length(mutual_friends_ij) < 2) {
    disp_temp = c(0)
  }
  else {
    pairs_mutual_friends = combn(mutual_friends_ij, 2, FUN = NULL, simplify = TRUE)

    for (i in 1:(length(pairs_mutual_friends)/2)) {
      pair_F1 = pairs_mutual_friends[,i][1]
      pair_F2 = pairs_mutual_friends[,i][2]
      disp_temp = c(disp_temp, shortest.paths(dispersion_sub_graph, which(dispersion_sub_graph$names == pair_F1), which(dispersion_sub_graph$names == pair_F2)))
    }
  }
  
  return(sum(disp_temp))  
}


embeddness = numeric()
dispersion = numeric()



# loop through each core node and find dispersion and embeddness
for (i in core_nodes) {
  personal_network = make_personal_network(network, i)
  print(i)
  for (j in personal_network$names) {
    if (i == j)
      next
  embeddness = c(embeddness, calculate_embeddness(network, i, j))
  # dispersion = c(dispersion, calculate_dispersion(network, i, j))
  }
}
hist(embeddness, breaks = 50, main = "Embeddedness Distribution",xlab = "Embeddedness", col="blue") # plot embeddness distribution
# hist(dispersion, breaks = 50, main = "Dispersion Distribution",xlab = "Dispersion", col="blue") # plot dispersion distribution

for(i in core_nodes){
  cat(i, " ", length(neighbors(network, i)), "\n")
}


# plot 3 personal networks showing community structure
embeddness = numeric()
dispersion = numeric()

core_nodes_personal = c(1, 349, 2612)
for (j in core_nodes_personal) {
  personal_network = make_personal_network(network, j)
  
  for (k in personal_network$names) {
    print(k)
    if (j == k)
      next
    embeddness = c(embeddness, calculate_embeddness(network, j, k))
    dispersion = c(dispersion, calculate_dispersion(network, j, k))
  }
  
  # get node with maximum feature values
  dispersion[mapply(is.infinite, dispersion)] = 0
  ed = dispersion / embeddness
  max_embeddness = personal_network$names[which(embeddness == max(embeddness))]
  max_dispersion = personal_network$names[which(dispersion == max(dispersion))]
  ed[mapply(is.nan, ed)] = 0
  max_ed = personal_network$names[which(ed == max(ed))]
  
  # construct community structure
  personal_community = fastgreedy.community(personal_network)
  sizes(personal_community)
 
   # set properties of the plotted network
  node_color = personal_community$membership+1
  node_size = rep(3,length(node_color))
  edge_color = rep("grey", length(E(personal_network)))
  edge_weight = rep(0.5, length(E(personal_network)))
  
  e_node = which(personal_network$names == max_embeddness)
  i_node = which(personal_network$names == j)
  d_node = which(personal_network$names == max_dispersion)
  ed_node = which(personal_network$names == max_ed)
  
  edge_color[which(get.edgelist(personal_network, name = FALSE)[,1] == d_node | 
                    get.edgelist(personal_network, name = FALSE)[,2] == d_node)] = "red";
  edge_weight[which(get.edgelist(personal_network, name = FALSE)[,1] == d_node |  
                     get.edgelist(personal_network, name = FALSE)[,2] == d_node)] = 3;
  
  node_size[d_node] = 5
  node_color[d_node] = 7
  node_size[i_node] = 4
  node_color[i_node] = 0
  
  plot.igraph(personal_network, vertex.size = node_size , vertex.label = NA , edge.width = edge_weight, edge.color =  edge_color, vertex.color = node_color, asp=9/16, layout = layout.fruchterman.reingold)
}


##### END OF QUESTION 5 #####

