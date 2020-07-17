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
