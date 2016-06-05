#problem1
edges <- read.csv("edges.csv")
str(edges)
users <- read.csv("users.csv")
str(users)
nrow(users)
nrow(edges) * 2 / nrow(users)
table(users$locale, users$school)
table(users$gender, users$school)

#problem2
library(igraph) 
#g <- graph.data.frame(edges, FALSE, users)
#g = graph.data.frame(edges, TRUE, users)
plot(g, vertex.size=5, vertex.label=NA)
g <- graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
degree(g)
sum(degree(g) >= 10)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
table(V(g)$size)
max(V(g)$size)
min(V(g)$size)
table(degree(g)) 
summary(degree(g))

#problem3
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
V(g)$color <- "black"
V(g)$color[V(g)$school == "A"] <- "red"
V(g)$color[V(g)$school == "AB"] <- "green"
plot(g, vertex.label=NA)
V(g)$color <- "black"
V(g)$color[V(g)$locale == "A"] <- "red"
V(g)$color[V(g)$locale == "B"] <- "green"
plot(g, vertex.label=NA)
library(rgl)
rglplot(g, vertex.label=NA)
plot(g, edge.width=2, vertex.label=NA)
plot(g)
plot.igraph(g)
tkplot(g)
g1 <- barabasi.game(100)
plot(g1, layout=layout_with_fr, vertex.size=4, vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
g2 <- sample_gnp(100, 1/100)
comps <- components(g2)$membership
colbar <- rainbow(max(comps)+1)
V(g2)$color <- colbar[comps+1]
plot(g2, layout=layout_with_fr, vertex.size=5, vertex.label=NA)
g3 <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g3 <- add_edges(g3, c(1,6, 1,11, 6,11))
com <- cluster_spinglass(g3, spins=5)
V(g3)$color <- com$membership+1
g3 <- set_graph_attr(g3, "layout", layout_with_kk(g3))
plot(g3, vertex.label.dist=1.5)
igraph_options(plot.layout=layout_as_tree)
plot(make_tree(20, 2))
plot(make_tree(50, 3), vertex.size=3, vertex.label=NA)
tkplot(make_tree(50, 2, mode="undirected"), vertex.size=10,
vertex.color="green")











