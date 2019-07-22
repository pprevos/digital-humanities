## Basic family history statistics
library(tidyverse)
library(lubridate)
source("read.gedcom.R")
presidents <- read.gedcom("https://webtreeprint.com/tp_downloader.php?path=famous_gedcoms/pres.ged")

filter(presidents, grepl("Jefferson", Full_Name))

mutate(presidents, Year = year(Birth_Date)) %>%
    ggplot(aes(Year)) +
    geom_histogram(binwidth = 10, fill = "#6A6A9D", col = "white") +
    labs(title = "Birth years in the presidents file")
ggsave("../../Genealogy/years.png")

alive <- function(population, census_date){
    max_date <- census_date + 100 * 365.25
    filter(people, (is.na(Birth_Date) & (Death_Date <= max_date &
                                         Death_Date >= census_date)) |
                   (Birth_Date <= census_date & Death_Date >= census_date)) %>%
        arrange(Birth_Date) %>%
        mutate(Age = as.numeric(census_date - Birth_Date) / 365.25) %>%
        return()
}

alive(presidents, as.Date("1840-03-07")) %>%
    filter(is.na(Birth_Date)) %>%
    arrange(Death_Date)
    
    
