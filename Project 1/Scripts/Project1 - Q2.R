library(igraph)
library(MASS)

# get the input undirected graph
network = read.graph("facebook_combined.txt", directed=FALSE)

##### QUESTION 2 #####

nn1 = neighborhood(network, order = 1, nodes = 1)
personal_nn1 = induced_subgraph(network, vids = unlist(nn1), impl = "auto")
personal_nn1$names = sort(unlist(nn1))
cat("Size of Neighbors of Node 1 :" , vcount(personal_nn1)) # number of vertices in personal network of Node 1
cat("Number of Edges in Personal Network :", ecount(personal_nn1)) # number of edges in personal network of Node 1


# plot the personal network of node 1
node_color = rep("lightblue",vcount(personal_nn1))
node_size = rep(2, vcount(personal_nn1))
node_color[personal_nn1$names == 1] = "red"
node_size[personal_nn1$names == 1] = 4

plot(personal_nn1 , vertex.size = node_size, vertex.color=node_color , vertex.label=NA , asp=9/16, layout = layout.fruchterman.reingold)

##### END OF QUESTION 2 #####