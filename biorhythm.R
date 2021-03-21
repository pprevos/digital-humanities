# Biorhythm

library(ggplot2)
library(dplyr)
library(tidyr)

biorhythm <- function(dob, target = Sys.Date()) {
    dob <- as.Date(dob)
    target <- as.Date(target)
    t <- round(as.numeric(difftime(target, dob)))
    days <- (t - 14) : (t + 14)
    period <- data.frame(Date = seq.Date(from = target - 14, by = 1, length.out = 29),
                         Physical = sin (2 * pi * days / 23) * 100, 
                         Emotional = sin (2 * pi * days / 28) * 100, 
                         Intellectual = sin (2 * pi * days / 33) * 100) %>%
              pivot_longer(-Date, names_to = "Biorhythm", values_to = "Percentage")
    ggplot(period, aes(x = Date, y = Percentage, col = Biorhythm)) + geom_line() +  
        ggtitle(paste("DoB:", format(dob, "%d %B %Y"))) + 
        geom_vline(xintercept = as.numeric(target))
}

biorhythm("1969-09-12")
ggsave("biorhythm.png")

dob <- as.Date("1969-09-12")
target <- dob + 23 *28 * 33
biorhythm(dob, target)

bio.max <- 0
day.max <- NA
for (days in 0:(365*80)) {
    target <- dob + days
    biorhythm <- (sin (2 * pi * days / 23) * 100) + (sin (2 * pi * days / 28) * 100) + (sin (2 * pi * days / 33) * 100)
    if (biorhythm > bio.max) {
        bio.max <- biorhythm
        day.max <- target
    }
}
bio.max
day.max-dob

