library(tidyverse)
library(rvest)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
lm_palette <- c("#f7881f", "#55ace1",  "#5f6c36")

## Scrape definitions from website
definitions <- read_html("https://heidicohen.com/marketing-definition/") %>%
    html_nodes("ol li") %>%
    html_text() %>%
    as_tibble() %>%
    mutate(No = 1:nrow(.)) %>%
    select(No, Definition = value)

## bag of words
def_words <- definitions[1:72, ] %>%
    unnest_tokens(word, Definition) %>%
    mutate(word = gsub("s$", "", word))

word_freq <- def_words %>%
    anti_join(stop_words) %>%
    count(word) %>%
    filter(!(word %in%
             c("marketing", "vice", "president", "chief", "executive", "’")))

pdf("marketingcloud.pdf")
word_freq %>%
    with(wordcloud(word, n, max.words = 50, rot.per = .5,
                   colors = rev(lm_palette)))
dev.off()

word_freq %>%
    top_n(10) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) + geom_col(fill = lm_palette[2]) +
    coord_flip() +
    theme(text = element_text(size = 15))
ggsave("marketing-words.png", width = 6, height = 6)

## Topic modeling
library(topicmodels)

marketing_dtm <- word_freq %>%
    mutate(doc = 1) %>%
    cast_dtm(doc, word, n)

marketing_lda <- LDA(marketing_dtm, k = 4) %>%
    tidy(matrix = "beta") %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

marketing_lda %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
       facet_wrap(~topic, scales = "free") +
       coord_flip() +
       theme(text = element_text(size = 20))
