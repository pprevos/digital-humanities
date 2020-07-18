## Export WP to Hugo

## Read exported WP content
library(tibble)
library(readr)
library(dplyr)
library(stringr)

posts <- read_csv("Posts-Export-2020-July-17-2245.csv")

## Convert to Org Mode or Markdown
export <- ".org" # ".org" or ".md"

for (i in 1:nrow(posts)) {
    filename <- paste0(posts$Slug[i], ".html")
    writeLines(posts$Content[i], filename)
    pandoc <- paste0("pandoc -o content/post/", posts$Slug[i], export, " ", filename)
    system(pandoc)
}

## Clean folder
file.remove(list.files(pattern = "*.html"))

## Create Org Mode files
baseurl <- "https://lucidmanager.org"

## Create front matter
if(export == ".org") {
    fm <- tibble(title = paste("#+title:", posts$Title),
                 date = paste("#+date:", as.POSIXct(posts$Date, origin = "1970-01-01")),
                 lastmod = paste("#+lastmod:", Sys.Date()),
                 categories = paste("#+categories[]:", str_replace_all(posts$Categories, " ", "-")),
                 tags = paste("#+tags[]:", str_replace_all(posts$Tags, " ", "-")),
                 draft = "#+draft: true") %>%
        mutate(categories = str_replace_all(categories, "\\|", " "),
               tags = str_replace_all(tags, "\\|", " "))
} else {
    fm <- tibble(opening = "+++",
                 title = paste0('title = "', posts$Title, '"'),
                 date = paste0('date = "', as.POSIXct(posts$Date, origin = "1970-01-01"), '"'),
                 lastmod = paste0('lastmod = "', Sys.Date(), '"'),
                 categories = paste0('categories = ["', posts$Categories, '"]'),
                 tags = paste0('tags = ["', posts$Tags, '"]'),
                 draft = 'draft = "true"',
                 close = "+++") %>%
        mutate(categories = str_replace_all(categories, "\\|", '", "'),
               tags = str_replace_all(tags, "\\|", '", "'))
}

## Load Hugo files an append front matter
for (f in 1:nrow(posts)) {
    filename <- paste0("content/post/", posts$Slug[f], export)
    post <- c(paste(fm[f, ]), "", readLines(filename))
    ## Repoint images
    post <- str_replace_all(post, paste0(baseurl, "/wp-content"), "/images")
    ## R Code highlighting
    post <- str_replace_all(post, "``` \\{.*", "{{< highlight R >}}")
    post <- str_replace_all(post, "```", "{{< /highlight >}}")
    ## Remove remaining Wordpress artefacts
    post <- str_remove_all(post, ':::|\\{.wp.*|.*\\"\\}')
    ## Write to disk
    writeLines(post , filename)
}
