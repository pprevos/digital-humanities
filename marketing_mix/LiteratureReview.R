library(tidyverse)
library(RQDA)

## Open project
openProject("IWA_Abstracts.rqda", updateGUI = TRUE)

## Visualise codes
getCodingTable() %>%
    group_by(codename) %>%
    count() %>%
    arrange(n) %>%
    ungroup() %>%
    mutate(codename = factor(codename, levels = codename)) %>%
    ggplot(aes(codename, n)) +
        geom_col() +
        coord_flip() +
        xlab("Code name") + ylab("Occurence")
ggsave("code_frequencies.png", dpi = 150)

## Define network
library(igraph)
library(reshape2)
dtm <- getCodingTable()[,c(5, 4)] %>%
    mutate(freq = 1) %>%
    acast(filename ~ codename, sum)
adj <- crossprod(dtm)
g <- graph.adjacency(adj, weighted = T, mode = "undirected")
g <- simplify(g)

## Network Graphs
V(g)$name <- gsub(" ", "\n", V(g)$name)
par(mar = rep(0, 4))
plot(g,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = 1,
     vertex.size = degree(g),
     vertex.label.color = "black",
     vertex.frame.color = "white",
     vertex.color = "Dodgerblue",
     edge.width = E(g)$weight * 1,
     edge.color = "darkgray"
)

## Community Detection
set.seed(123)
comms <- spinglass.community(g, spins = 100)
par(mar = rep(0, 4))
plot(comms, g,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = .7,
     vertex.size = degree(g),
     vertex.label.color = "black",
     vertex.frame.color = NA,
     edge.color = "black",
     vertex.label.family = "sanserif",
     mark.border = NA
)
