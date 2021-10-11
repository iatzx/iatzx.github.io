library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(ggpubr)
library(maps)
library(patchwork)
`%notin%` <- Negate(`%in%`)

nurses_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv') %>%
         clean_names()

nurses <- nurses_raw %>% 
   filter(year==2020, state %notin% c("Guam","Puerto Rico","Alaska")) %>%
   select(state, hourly_wage_avg) %>%
   mutate(state=tolower(state))

# z-score and group (low/medium/high)
nurses$z <- (nurses$hourly_wage_avg -mean(nurses$hourly_wage_avg))/sd(nurses$hourly_wage_avg)
nurses <- nurses %>% mutate(grp=cut(z, breaks=c(-Inf, -0.5, 0.5, Inf), labels=c("low","middle","high")))

states_map <- map_data("state")
djoin <- left_join(states_map, nurses, by = c("region" = "state"))

# map
m <- ggplot(data = djoin, aes(long, lat, group = group)) + 
   geom_polygon(aes(fill = grp), color = "black", size=0.1) +
   scale_fill_brewer(palette = "Greens") +
   coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
   theme_void() + 
   theme(legend.position = "none")
m

# barplot
p <- ggbarplot(nurses, x = "state", y = "z",
          fill = "grp",
          color = "white",
          palette = c("#E72200", "yellow", "#00AF00"),  
          sort.val = "desc",          
          sort.by.groups = FALSE,
          x.text.angle = 90,          
          ylab = "Hourly wage z-score",
          xlab = FALSE,
          legend="none"
)
p <- ggpar(p, 
      title = "How much do nurses get paid ?", 
      subtitle = "Deviation of each state compared to the national mean (2020)",
      font.main = c(14, "bold", "black"),
      font.legend = c(8, "plain", "black"),
      font.y = c(8, "plain", "black"),
      font.subtitle = c(8, "plain", "black"),
      font.tickslab = c(6, "plain", "black"))

p <- p +
   inset_element(m, left = 0.4, bottom = 0.3, right=1.1, top=1)
p

ggsave("wk41-nurses.png", plot = p, width = 45, height = 25, units = "cm")
