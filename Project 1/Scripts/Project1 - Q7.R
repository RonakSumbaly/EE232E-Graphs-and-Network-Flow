library(igraph)
library(corrplot)

setwd("~/Dropbox/Masters/Quarter 3/EE232E - Graph and Network Flows/Project 1/")
# read the google+ ego networks
file_names = list.files("gplus/")
file_ids = sub("^([^.]*).*", "\\1", file_names)
ego_node_ids = unique(file_ids) 

cat("Total Number of Ego Nodes = ", length(ego_node_ids))

ids_circles = numeric()
for (id in ego_node_ids) {
  # get the number of circles
  circles_file = paste("gplus/" , id , ".circles" , sep="")
  circles_connect = file(circles_file , open="r")
  circles_content = readLines(circles_connect)
  close(circles_connect)
  
  # check if greater than 2
  if(length(circles_content) > 2)
    ids_circles = c(ids_circles, id)
}

cat("Total IDs with > 2 circles = ", length(ids_circles))
for (id in  c(ids_circles[2], ids_circles[5], ids_circles[12])) {
  
  edges_file = paste("gplus/" , id  , ".edges" , sep="") # edge list
  circles_file = paste("gplus/" , id , ".circles" , sep="") # circles list
  
  circles_connect = file(circles_file , open="r")
  circles_content = readLines(circles_connect)
  
  circles = list()
  for (i in 1:length(circles_content)) {
    circle_nodes = strsplit(circles_content[i],"\t")
    circles = c(circles, list(circle_nodes[[1]][-1]))
  }
  
  close(circles_connect)
  
  # create network using edge list
  g_network = read.graph(edges_file , format = "ncol" , directed=TRUE)  
  g_network = add.vertices(g_network, nv = 1, name = id)
  ego_node_index = which(V(g_network)$name==id) 
  
  add_edge_list = c()
  for (vertex in 1:(vcount(g_network) - 1)) {
    add_edge_list = c(add_edge_list, c(ego_node_index, vertex))
  }
  
  g_network = add_edges(g_network, add_edge_list)
  
  walktrap_comm = walktrap.community(g_network)
  infomap_comm = infomap.community(g_network)
  
  percentage = vector()
  percentage_circle = vector()
  # check percentage of match for walktrap community 
  for(m in 1:max(walktrap_comm$membership)){
    
    community_nodes = V(g_network)$name[which(walktrap_comm$membership == m)]
    
    for (n in 1:length(circles)) {
      common_nodes = intersect(community_nodes, circles[[n]])
      percent_circle = length(common_nodes)/length(circles[[n]])
      percentage_circle = c(percentage_circle, percent_circle)
    }
  }
  
  for(m in 1:max(infomap_comm$membership)){
    
    community_nodes = V(g_network)$name[which(infomap_comm$membership == m)]
    
    for (n in 1:length(circles)) {
      common_nodes = intersect(community_nodes, circles[[n]])
      percent = length(common_nodes)/length(circles[[n]])
      percentage = c(percentage_circle, percent_circle)
    }
  }
  
  par(oma=c(0,0,0,0))
  
  cat("Number of Circles: ", length(circles))
  
  c_percentage = matrix(percentage_circle, nrow = max(walktrap_comm$membership), ncol = length(circles))
  colnames(c_percentage) = paste("Circle ",    1:length(circles),     sep="")
  rownames(c_percentage) = paste("Community ",    1:max(walktrap_comm$membership),     sep="")
  corrplot(c_percentage, method="color", cl.lim=c(0,1), tl.cex = 2, tl.col = 'black')
  
  i_percentage = matrix(percentage, nrow = max(infomap_comm$membership), ncol = length(circles))
  colnames(i_percentage) = paste("Circle ",    1:length(circles),     sep="")
  rownames(i_percentage) = paste("Community ",    1:max(infomap_comm$membership),     sep="")
  # corrplot(i_percentage, method="color", cl.lim=c(0,1), tl.cex = 2, tl.col = 'black')
  
}

# plot communities
node_size = rep(2, vcount(g_network))
node_size[ego_node_index] = 4
plot(walktrap_comm, g_network, vertex.size = node_size , asp = 9/16, vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
plot(infomap_comm, g_network, vertex.size = node_size , asp = 9/16, vertex.label=NA , edge.color = "grey", layout=layout.fruchterman.reingold)
 
# plot community structure
ggplot(data.frame(sizes(infomap_comm)), aes(x = Community.sizes, y = Freq)) + geom_bar(fill="darkseagreen",stat="identity") + xlab("Community Number") +  ylab("Frequency") + labs(title = "Community Structure Frequency") + geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)
 
