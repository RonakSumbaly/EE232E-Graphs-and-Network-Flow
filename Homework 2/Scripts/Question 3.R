library(igraph)
library(netrw)

#### QUESTION 3 - PART A ####

random_network = random.graph.game(n = 1000, p = 0.01, directed = FALSE)
vertex_sequence = netrw(random_network, walker.num = 1000, damping = 1, T = 1000, output.walk.path = TRUE) # get vertex sequence of random walk
network_degree = degree(random_network)
visit_probability = vertex_sequence$ave.visit.prob

relationship = cor(network_degree, visit_probability)
cat("Correlation between degree and visit probability: ", relationship)

plot_data = rbind(network_degree, visit_probability)
plot_data = plot_data[,order(plot_data[1,])]  #order by degree
plot(plot_data[1,], plot_data[2,], xlab = "Degree", ylab = "Visit Probability", main = "Relation Between Degree and Visit Probability for Undirected Network")

#### QUESTION 3 - PART B ####

random_network = random.graph.game(n = 1000, p = 0.01, directed = TRUE)
vertex_sequence = netrw(random_network, walker.num = 1000, damping = 1, T = 1000, output.walk.path = TRUE) # get vertex sequence of random walk

network_degree = degree(random_network)
network_in_degree = degree(random_network, mode = "in")
network_out_degree = degree(random_network, mode = "out")

visit_probability = vertex_sequence$ave.visit.prob

relationship = cor(network_degree, visit_probability)
cat("Correlation between total-degree and visit probability: ", relationship)

in_relationship = cor(network_in_degree, visit_probability)
cat("Correlation between in-degree and visit probability: ", in_relationship)

out_relationship = cor(network_out_degree, visit_probability)
cat("Correlation between out-degree and visit probability: ", out_relationship)

plot_data = rbind(network_in_degree, visit_probability)
plot_data = plot_data[,order(plot_data[1,])]  #order by degree
plot(plot_data[1,], plot_data[2,], xlab = "In-Degree", ylab = "Visit Probability", main = "Relation Between In Degree and Visit Probability for Directed Network")

plot_data = rbind(network_out_degree, visit_probability)
plot_data = plot_data[,order(plot_data[1,])]  #order by degree
plot(plot_data[1,], plot_data[2,], xlab = "Out-Degree", ylab = "Visit Probability", main = "Relation Between Out Degree and Visit Probability for Directed Network")

plot_data = rbind(network_out_degree, visit_probability)
plot_data = plot_data[,order(plot_data[1,])]  #order by degree
plot(plot_data[1,], plot_data[2,], xlab = "Total Degree", ylab = "Visit Probability", main = "Relation Between Total Degree and Visit Probability for Directed Network")

#### QUESTION 3 - PART C ####

random_network = random.graph.game(n = 1000, p = 0.01, directed = FALSE)
vertex_sequence = netrw(random_network, walker.num = 1000, damping = 0.85, T = 1000, output.walk.path = TRUE) # get vertex sequence of random walk
network_degree = degree(random_network)
visit_probability = vertex_sequence$ave.visit.prob

relationship = cor(network_degree, visit_probability)
cat("Correlation between degree and visit probability: ", relationship)

plot_data = rbind(network_degree, visit_probability)
plot_data = plot_data[,order(plot_data[1,])]  #order by degree
plot(plot_data[1,], plot_data[2,], xlab = "Degree", ylab = "Visit Probability", main = "Relation Between Degree and Visit Probability for Undirected Network")
