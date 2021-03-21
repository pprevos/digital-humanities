## Load data (CSV exported from wp_posts table in MySQL database)
library(tidyverse)
library(tidytext)
library(igraph)
library(RMySQL)
library(RColorBrewer)

## Download data using RmySQL
source("dbconnect.R")
## lucidmanager <- dbConnect(MySQL(),
##                          user = "username",
##                          password = "Password1",
##                          dbname = "databe name",
##                          host = "URL")

posts.query <- "SELECT p.post_name, p.post_content, t.name
                FROM wp_posts p
                JOIN wp_term_relationships tr ON (tr.object_id = p.ID)
                JOIN wp_term_taxonomy tt ON (tt.term_taxonomy_id = tr.term_taxonomy_id )
                JOIN wp_terms t ON (t.term_id = tt.term_id)
                WHERE
                 p.post_type='post'
                 AND
                 p.post_status='publish'
                 AND
                 tt.taxonomy = 'category'"

pages.query <- "SELECT post_name, post_content
                FROM wp_posts
                WHERE post_type='page' AND post_status='publish'"

posts.data <- dbSendQuery(lucidmanager, posts.query)
posts <- fetch(posts.data, n=-1)

pages.data <- dbSendQuery(lucidmanager, pages.query)
pages <- fetch(pages.data, n=-1)

dbDisconnect(lucidmanager)

pages$name = "Page"
content <- rbind(posts, pages)
as_tibble(content)

## Extract internal links
links <- content %>%
    unnest_tokens(word, post_content,
                  token = stringr::str_split, pattern = " ") %>%
    filter(grepl(".+lucidmanager.org/", word)) %>%
    mutate(link = gsub(".+lucidmanager.org/", "", word),
           link = gsub("/.+", "", link)) %>%
    filter(link != "tag" & link !=  "wp-content" & link != "author") %>%
    select(category = name, post_name, link)

tibble(links)

solitary <- c(posts$post_name, pages$post_name)[!(c(posts$post_name, pages$post_name) %in%
                                                  unique(c(links$post_name, links$link)))]

## Create network
network <- select(links, post_name, link) %>%
  graph_from_data_frame(directed = TRUE) + 
  vertices(solitary)

colour <- tibble(post_name = V(network)$name) %>%
  left_join(content) %>%
  mutate(name = factor(name),
         colour = brewer.pal(length(unique(content$name)), "Set2")[name]) %>%
  select(-post_content)

V(network)$color <- as.vector(colour$colour)

par(mar = rep(0, 4))
plot(network,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = .7,
     vertex.size = degree(network),
     vertex.label.color = "black",
     vertex.frame.color = NA,
     edge.arrow.size = .2,
     edge.color = "darkgray"
     )

## Diagnose network
which.max(degree(network, mode = "in"))

E(network)[which_loop(network)]

E(network)[which_multiple(network)]
