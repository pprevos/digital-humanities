## Analysing Trumpworld
## http://lucidmanager.org/trumpworld-analysis/
## Source: https://www.buzzfeed.com/johntemplon/help-us-map-trumpworld

# Read data
library(tidyverse)
library(igraph)
trumpworld.org <- read_csv("TrumpWorld Data org-org.csv") %>%
    filter(Connection == "Ownership") %>%
    select(OrgA = "Organization A", OrgB = "Organization B")

## Create graph of ownerships
org.ownership <- trumpworld.org %>%
    as.matrix %>%
    graph.edgelist()

# Plot Graph
par(mar=rep(0,4))
plot(org.ownership,
     layout = layout.fruchterman.reingold,
     vertex.label = NA,
     vertex.size = 2,
     edge.arrow.size = .1
)

## Find most connected firm
which.max(degree(org.ownership))
# Create subnetworks
org.ownership.d <- decompose(org.ownership)
## Find largest subnetwork
largest <- which.max(sapply(org.ownership.d, diameter))
## Visualise largest subnetwork
plot(org.ownership.d[[largest]],
     layout=layout.fruchterman.reingold,
     vertex.label.cex=.5,
     vertex.size=5,
     edge.arrow.size=.1
)
