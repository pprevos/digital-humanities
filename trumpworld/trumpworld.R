## Analysing Trumpworld
library(tidyverse)
library(igraph)
trumpworld.org <- read_csv("trumpworld/TrumpWorld Data org-org.csv") %>%
    filter(Connection == "Ownership") %>%
    select(OrgA = "Organization A", OrgB = "Organization B")

org.ownership <- trumpworld.org %>%
    as.matrix %>%
    graph.edgelist()

par(mar=rep(0,4))
plot(org.ownership,
layout = layout.fruchterman.reingold,
    vertex.label = NA,
    vertex.size = 2,
    edge.arrow.size = .1
)

which.max(degree(org.ownership))

org.ownership.d <- decompose(org.ownership)

largest <- which.max(sapply(org.ownership.d, diameter))

plot(org.ownership.d[[largest]],
     layout=layout.fruchterman.reingold,
     vertex.label.cex = .5,
     vertex.size = 5,
    edge.arrow.size = .1
)
