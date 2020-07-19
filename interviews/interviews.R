


project <- data.frame(Type = "Project", RQDAQuery("SELECT memo FROM project"), name = "project")
file <- data.frame(Type = "File", RQDAQuery("SELECT memo, name FROM source WHERE status==1 AND memo NOT IN ('NA','')"))
code_category <- data.frame(Type = "Code Category", RQDAQuery("SELECT memo, name FROM codecat WHERE status==1 AND memo NOT IN ('NA','')"))
code <- data.frame(Type = "Code",
                   RQDAQuery("SELECT memo, name FROM freecode WHERE status==1 AND memo NOT IN ('NA','')"))             
memos <- rbind(project, file, code_category, code)

memos
SELECT table1.column1, table2.column2 FROM table1, table2 WHERE table1.column1 = table2.column1;




project file code category code


## INIT
library(tidyverse)
library(RQDA)
library(tm)
library(wordcloud)
library(igraph)

openProject("stakeholders.rqda")
interviews <- data.frame(name = RQDAQuery("SELECT name FROM source"),
                         text = RQDAQuery("SELECT file FROM source"))
interviews <- Corpus(VectorSource(interviews$file))

interviews <-  tm_map(interviews, stripWhitespace)
interviews <-  tm_map(interviews, content_transformer(tolower))
interviews <-  tm_map(interviews, removeWords, stopwords("english"))
interviews <-  tm_map(interviews, removePunctuation)
interviews <-  tm_map(interviews, removeNumbers)
interviews <-  tm_map(interviews, removeWords, c("interviewer", "interviewee"))

# Word cloud
pdf("wordcloud.pdf")
set.seed(1969)
wordcloud(interviews, min.freq = 10, max.words = 50, rot.per=0.35, 
          colors = brewer.pal(8, "Blues")[-1:-5])
dev.off()

# Topic Models
dtm <- DocumentTermMatrix(interviews)
dtm <- removeSparseTerms(dtm, 0.99)
ldaOut <-LDA(dtm, k = 4)
terms(ldaOut,6)

#Load and transform data
codings <- getCodingTable()[,4:5]
categories <- RQDAQuery("SELECT filecat.name AS category, source.name AS filename 
                         FROM treefile, filecat, source 
                         WHERE treefile.catid=filecat.catid AND treefile.fid=source.id AND treefile.status=1")
codings <- merge(codings, categories, all.y = TRUE)
head(codings)

# Open coding
reorder_size <- function(x) {
    factor(x, levels = names(sort(table(x))))
}
ggplot(codings, aes(reorder_size(codename), fill=category)) + geom_bar(stat="count") + 
    facet_grid(~filename) + coord_flip() + 
    theme(legend.position="bottom", legend.title=element_blank()) + 
    ylab("Code frequency in interviews") + xlab("Code")
ggsave("rqda_codes.png", width=9, height=11, dpi = 300)

# Axial coding
edges <- RQDAQuery("SELECT codecat.name, freecode.name FROM codecat, freecode, treecode WHERE codecat.catid=treecode.catid AND freecode.id=treecode.cid")

g <- graph_from_edgelist(as.matrix(edges), directed = FALSE) %>%
  simplify()
V(g)$name <- gsub(" ", "\n", V(g)$name)

c <- spinglass.community(g)
pdf("network.pdf")
par(mar=rep(0,4))
set.seed(666)
plot(c, g, 
     vertex.size = 10,
     vertex.color = NA,
     vertex.frame.color = NA,
     layout = layout.drl)
dev.off()



