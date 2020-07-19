## 
## Peter Prevos

## Libraies
library(tidyverse)
library(RQDA)
library(igraph)
library(reshape2)

## Search results
searches <- read_csv("IWA_Searches.csv")
nrow(searches)
sum(duplicated(searches$Result))

searches %>% 
    group_by(Screening) %>%
    count() %>%
    arrange(n)

searches %>%
    filter(Screening == "Selected") %>%
    group_by(Keyword, Year) %>%
    count() %>%
    ggplot(aes(Year, n, fill = Keyword)) + 
        geom_col() + 
        scale_fill_grey() + 
        theme_bw()

## Open RQD project
openProject("IWA_Abstracts.rqda", updateGUI=TRUE)

## Define network
dtm <- getCodingTable()[,c(5, 4)] %>%
    mutate(freq = 1) %>%
    acast(filename ~ codename, sum)
adj <- crossprod(dtm)
g <- graph.adjacency(adj, weighted = T, mode = "undirected")
g <- simplify(g)

#Network Graphs
V(g)$name <- gsub(" ", "\n", V(g)$name)
par(mar = rep(0, 4))
plot(g,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = 1.8,
     vertex.size = degree(g),
     vertex.label.color = "black",
     vertex.frame.color = "white",
     vertex.color = "Dodgerblue",
     edge.width = E(g)$weight * 1,
     edge.color = "darkgray"
)

comms <- spinglass.community(g, spins = 100)
par(mar = rep(0, 4))
plot(comms, g,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = 1,
     vertex.size = degree(g),
     vertex.label.color = "black",
     vertex.frame.color = NA,
     edge.color = "black",
     vertex.label.family = "sanserif",
     mark.border = NA
)

dev.off()
