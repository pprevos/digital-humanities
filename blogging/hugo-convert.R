## Export WP to Hugo

## Read exported WP content
library(tibble)
library(readr)
posts <- read_csv("Posts-Export-2020-July-16-0959.csv")

## Convert to Org Mode or Markdown
export <- ".org" # ".org" or ".md"

for (i in 1:nrow(posts)) {
    filename <- paste0(posts$Slug[i], ".html")
    writeLines(posts$Content[i], filename)
    pandoc <- paste0("pandoc -o content/", posts$Slug[i], export, " ", filename)
    system(pandoc)
}

## Clean folder
file.remove(list.files(pattern = "*.html"))

## Create Org Mode files
baseurl <- "https://magicperspectives.net"

## Create front matter
library(stringr)

org_fm <- tibble(title = paste("#+title:", posts$Title),
                 date = paste("#+date:", as.POSIXct(posts$Date, origin = "1970-01-01")),
                 lastmod = paste("#+lastmod:", Sys.Date()),
                 alias = paste("#+aliases[]:",
                               str_remove_all(posts$Permalink, baseurl)),
                 draft = "#+draft: true")

rmd_fm <- tibble(opening = "---",
                 title = paste0("title: \"", posts$Title, "\""),
                 date = paste0("date: ", as.POSIXct(posts$Date, origin = "1970-01-01")),
                 lastmod = paste0("lastmod: ", Sys.Date()),
                 alias = paste0("aliases: [\"",
                                str_remove_all(posts$Permalink, baseurl),
                                "\"]"),
                 draft = "draft: true",
                 close = "---")

## Load Hugo files
files <- list.files("content", pattern = export, full.names = TRUE)
content <- lapply(files, readLines)

if(export == ".org") {
    fm <- org_fm
} else fm <- rmd_fm

## Append front matter and 
for (f in 1:length(files)) {
    new_file <- unlist(c(paste(fm[f, ]), "", content[f]))
    writeLines(new_file, files[f])
}
