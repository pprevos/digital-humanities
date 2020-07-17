## Create Network from GEDCOM
library(tidyverse)
library(igraph)
source("read.gedcom.R")

## Read data
sample <- read_gedcom_ind("http://heiner-eichmann.de/gedcom/allged.ged")

## Convert to network
tree <- filter(sample, !is.na(Father_id), !is.na(Mother_id)) %>%
    select(id, Father_id, Mother_id) %>%
    gather("Relationshiop", "Parent", -id) %>%
    select(id, Parent) %>%
    as.matrix() %>%
    graph_from_edgelist()

## Attach properties to network
sample$Gender[is.na(sample$Gender)] <- "Unknown"
gender_shapes <- tibble(Gender = c("M", "F", "Unknown"),
                        Gender_Shape = c("square", "circle", "none"))
sample <- left_join(sample, gender_shapes)
V(tree)$Gender = sample$Gender_Shape
V(tree)$Name = sample$Full_Name

## Visualise
plot(tree,
     layout = layout.lgl,
     vertex.size = 10,
     vertex.shape = V(tree)$Gender,
     vertex.label = V(tree)$Name,
     vertex.label.cex = 1,
     edge.arrow.size = 1,
     )


