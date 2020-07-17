## Decoding lyrics
library(tidyverse)
library(tidytext)
library(reshape2)
library(rvest)

get_lyrics <- function(artist, song) {
    artist <- gsub("[^A-Za-z0-9]+", "", tolower(artist))
    song <- gsub("[^A-Za-z0-9]+", "", tolower(song))
    artist <- gsub("^the", "", artist)
    url = paste("http://azlyrics.com/lyrics/", 
                artist, "/", song, ".html", sep = "")
    print(url)

    azlyrics <- read_html(url)
    lyrics <- html_nodes(azlyrics, "div")
    lyrics <- html_text(lyrics[23])
    gsub("\r|\n", " ", lyrics)
}

plot_snowflake <- function(artist, song){

    lyrics <- get_lyrics(artist, song)
    lyrics <- data_frame(line = lyrics) %>%
        filter(line != "")

    words <- lyrics  %>%
        unnest_tokens(word, line) 
    words_matrix <- lapply(1:nrow(words),
                           function(w){
                               as.character(words[w, 1]) == words
                           }
                           ) %>%
        do.call(cbind, .)
    rownames(words_matrix) <- 1:nrow(words)
    colnames(words_matrix) <- 1:nrow(words)

    melt(words_matrix, varnames = c("x",  "y")) %>%
        ggplot(aes(x, -y, fill = value)) +
        geom_raster() +
        scale_fill_manual(values = c("white", "dodgerblue4"), guide = FALSE) +
        theme_void() +     
        ggtitle(artist, subtitle = song)
}


plot_snowflake("Abba", "Waterloo")
ggsave("Abba-Waterloo.png")

artist = "Thomas Edison"
song <- "Mary Had a Little Lamb"
lyrics <- "Mary had a little lamb, whose fleece was white as snow. And everywhere that Mary went, the lamb was sure to go."

library(gridExtra)
png("DaftPunk-Queen.png", width = 1024, height = 768)
l1 <- plot_snowflake("Daft Punk", "Around the world")
l2 <- plot_snowflake("Queen", "Bohemian Rhapsody")
grid.arrange(l1, l2, ncol = 2)
dev.off()
getwd()

artist <- "Frank Zappa"
song <- "Titties Beer"
