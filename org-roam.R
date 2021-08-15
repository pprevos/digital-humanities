## Visualise Org-Roam databse

## Connect to database and extract nodes and links
library(DBI)
roam <- dbConnect(RSQLite::SQLite(), "~/Documents/org-roam/data/org-roam.db")
nodes <- dbGetQuery(roam, "SELECT id, title FROM nodes")
links <- dbGetQuery(roam, "SELECT source, dest FROM links WHERE type = '\"id\"'")
dbDisconnect(roam)

## Clean node names and create network table
library(tidyverse)
nodes <- nodes %>% 
  mutate(title = str_remove_all(title, "\""))

network <- left_join(links, nodes, by = c("source" = "id")) %>% 
  left_join(nodes, by = c("dest" ="id")) %>% 
  select(from = title.x, to = title.y)

## Create network diagram
library(igraph)
g <- graph.data.frame(network)
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

## Centrality
centrality <- tibble(Node = names(degree(g)),
                     Degree = degree(g, mode = "total"),
                     Links = degree(g, mode = "out"),
                     Backlinks = degree(g, mode = "in")) 

centrality %>% 
  top_n(n = 10) %>% 
  arrange(desc(Degree))

## Community-detection
c <- cluster_spinglass(g)

communities <- tibble(Node = c$names,
                      Community = c$membership) %>% 
  left_join(centrality) %>% 
  group_by(Community) %>% 
  summarise(Name = Node[which.max(Backlinks)],
            Nodes = n()) %>% 
  arrange(desc(Nodes))

## Visualise graph
par(mar = c(0, 0, 0, 0))
plot(g,
     layout = layout.fruchterman.reingold,
     mark.border = NA,
     vertex.color = c$membership,
     vertex.label = NA,
     vertex.size = sqrt(degree(g, mode = "in") + 1),
     vertex.frame.color = NA,
     edge.arrow.size = 0,
     edge.color = "lightgrey")

## Subnetwork
node <- which(names(V(g)) == "Topology")
g1 <- make_ego_graph(g, order= 2, nodes = node)
c1 <- cluster_walktrap(g1[[1]])
    
plot(g1[[1]], layout = layout.kamada.kawai,
     mark.border = NA,
     vertex.size = sqrt(degree(g1[[1]])) + 2,
     vertex.color = c1$membership,
     vertex.frame.color = NA,
     vertex.label.family = "Sanserif",
     vertex.label.color = "black",
     edge.arrow.size = .3)

## Interactive visualisation
library(networkD3)
library(htmlwidgets)

## Convert to object suitable for networkD3
d3 <- igraph_to_networkD3(g, group = communities$Name[membership(c)])
d3$nodes$backlinks <- degree(g, mode = "in")

## Create force directed network plot
p <- forceNetwork(Links = d3$links, Nodes = d3$nodes, 
             Source = "source", Target = "target", 
             NodeID = "name", Group = "group", 
             Nodesize = "backlinks",
             zoom = TRUE, legend = TRUE)
saveWidget(p, file=paste0( getwd(), "/org-roam.html"))
