## Visualise internal link structure

## Load data (CSV exported from wp_posts table in MySQL database)

library(tidyverse)
library(tidytext)
library(igraph)
library(RMySQL)

lucidmanager <- dbConnect(MySQL(),
                          user = "peterpr5_rmysql",
                          password = "cy7J}O-8IsUQ",
                          dbname = "peterpr5_WPZ1W",
                          host = "lucidmanager.org")

dbListTables(lucidmanager)

posts <- dbSendQuery(lucidmanager, "SELECT ID, post_type, post_name, post_title, post_content FROM _Z1W_4_posts")
on.exit(dbDisconnect(lucidmanager))

data = fetch(posts, n=-1)
data
as.data.frame(posts)

dbUnloadDriver(lucidmanager)

posts <- read_csv("wp_posts.csv") %>%
    filter((post_type == "page" | post_type == "post") & post_status == "publish") %>%
    select(ID, post_date, post_type, post_name, post_title, post_content)

internal_links <- unnest_tokens(posts, word, post_content,
                             token = stringr::str_split, pattern = " ") %>%
    filter(grepl(".+lucidmanager.org/", word)) %>%
    mutate(link = gsub(".+lucidmanager.org/", "", word),
           link = gsub("/.+", "", link)) %>%
    filter(link != "tag" & link !=  "wp-content" & link != "author") %>%
    select(post_type, post_title, post_name, link)
internal_links

link_network <- internal_links %>%
    select(post_name, link) %>%
    graph_from_data_frame()

E(link_network)$weight

pdf("links.pdf")
plot(link_network,
     layout = layout.fruchterman.reingold,
     vertex.label.cex = .3,
     vertex.size = degree(link_network),
     vertex.label.color = "black",
     vertex.frame.color = "white",
     vertex.color = "Dodgerblue",
     edge.arrow.size = .3,
     edge.color = "darkgray"
     )
dev.off()
