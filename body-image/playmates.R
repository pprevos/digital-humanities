library(tidyverse)
library(readxl)
library(GGally)
library(psych)

## Read and correct data
playmates <- read_xls("1702_Infoporn_Playmate_Data.xls", range = "A1:H663") %>%
    mutate(Date = as.Date(paste(1, Month, Year), format = "%d %B %Y"),
           Bust = `Bust (in)`) %>%
    select(Date, Bust, Waist, Hips, Height, Weight)

playmates[, -1] <- apply(playmates[, -1], 2, as.numeric)
## Fix Feb 1991
playmates$Hips[which(playmates$Hips == 26)] <- 36
## Fix Jan 2005
playmates$Hips[which(playmates$Hips == 24)] <- 34
playmates <- playmates[which(apply(playmates[, -1], 1, any)),]

playmates %>%
    mutate(BMI = 703 * Weight / Height^2,
           wh = Waist / Hips,
           Year = as.numeric(format(playmates$Date, "%Y"))) %>%
    group_by(Year) %>%
    summarise(BMI = mean(BMI, na.rm = TRUE),
              wh = mean(wh, na.rm = TRUE)) %>%
    gather("Statistic", "Value", -Year) %>%
    mutate(Statistic = str_replace(Statistic, "wh", "Waist-Hip ratio")) %>%
    ggplot(aes(Year, Value, col = Statistic)) +
    geom_point() +
    geom_smooth() + 
    facet_grid(Statistic~., scales = "free_y") +
    scale_color_manual(values = c("#d85726", "#75541b"), guide = FALSE) +
    theme_light() +
    ggtitle("BMI and Bust to Hip Ratio for Playboy models 1953-2009",
            subtitle = "horizonofreason.com")
ggsave("playmates.png")

apply(playmates[, -1], 2, summary)
range(playmates$Date)

pairs.panels(playmates[playmates$Date > as.Date("1980-01-01"), -1])
