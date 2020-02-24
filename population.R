## Analyse population
library(dplyr)
library(ggplot2)
library(plotrix)

source("read.gedcom.R")
pop <- read_gedcom_ind("heugem.ged")
pop <- mutate(pop, Age = as.numeric(Death_Date - Birth_Date) / 365.25)
pop


plot.age.pyramid <- function(population) {
    population %>%
        select(Gender, Age) %>%
        mutate(Cohort = cut(Age, breaks = seq(0, 100, 10),
                            labels = paste(seq(0, 90, 10), seq(10, 100, 10), sep = "\U2013"),
                            include.lowest = TRUE)) %>%
        filter(!is.na(Gender) & !is.na(Cohort)) %>%
        group_by(Gender, Cohort) %>%
        count() %>%
        mutate(n = ifelse(Gender == "F", -n, n)) %>%
        ggplot(aes(Cohort, n, fill = Gender)) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(breaks = seq(-20, 40, 10), 
                           labels = paste0(as.character(c(seq(20, 0, -10), seq(10, 40, 10))))) + 
        scale_fill_brewer(palette = "Paired") + 
        theme_bw() + 
        labs(x = "Age group", y = "Number")
}

plot.age.pyramid(pop)



