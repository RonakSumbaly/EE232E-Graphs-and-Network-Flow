library(igraph)
library(MASS)
library(ggplot2)

##### QUESTION 1 #####

# get the input undirected graph
network = read.graph("facebook_combined.txt", directed=FALSE)

# check for connectivity
is.connected(network)

# get diameter of network
diameter(network)

# plot degree distribution
network_degree = degree(network)
h = hist(network_degree, breaks = seq(from = min(network_degree), to = max(network_degree), by=1),  main = "Histogram of Degree of Facebook Graph", xlab = "Degree", ylab = "Frequency", border="blue", col="green")
df = data.frame(x=h$mids, y=h$density)
plot(df, main="Degree Distribution of Facebook Graph", xlab="Nodes", ylab="Degree Distribution",type="l")

# average degree of network
cat("Average Degree of Network : ", mean(network_degree))

# fitting multiple curves and checking for least MSE
models = list(
  nls(y ~ (1/x*a) + b*x, data = df, start = list(a = 0, b = 0), trace=T), 
  nls(y ~ (a + b*log(x)), data=df, start = list(a = 0, b = 0),trace=T),
  nls(y ~ (exp(a + b * x)), data=df, start = list(a=0,b=0),trace=T),
  nls(y ~ (1/x*a)+b, data=df, start = list(a=1,b=1),trace=T),
  nls(y ~ (exp(1)^(a + b * x)), data=df, start = list(a=0,b=0),trace=T))

# fit power law distribution

ggplot(df, aes(x, y)) + geom_point(size = 2) +
  geom_line(aes(x,fitted(models[[1]])),size = 1,colour = "blue") + 
  geom_line(aes(x,fitted(models[[2]])),size = 1, colour = "yellow") +
  geom_line(aes(x,fitted(models[[3]])),size = 1,  colour = "red") +
  geom_line(aes(x,fitted(models[[4]])),size = 1,  colour = "purple")+
  geom_line(aes(x,fitted(models[[5]])),size = 1,  colour = "orange")+
  ggtitle("Fitted curves for degree distribution")+ xlab("Nodes") +ylab("Degree Distribution")

summary(models[[5]])

##### END OF QUESTION ! #####