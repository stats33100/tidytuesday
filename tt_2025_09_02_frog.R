# Tidytuesday, week 35, 2025-09-02
# stats_33100

library(tidyverse)
library(showtext)

font <- "Poppins"
font_add_google(font)
showtext.opts(dpi = 300)
showtext_auto()

frogID_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv')
#frog_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv')

title <- "Numbers of frog species living in Australian provinces"
subtitle <- "There are over 250 known species of frogs in Australia. The Australian Museum's citizen science project FrogID is helping to 
understand Australia's frog fauna. This chart shows how many different species have been identified in each province."
caption <- "Tidytuesday 2025-09-02 | data from FrogID | stats_33100"

frogID_data %>% 
  select(scientificName, stateProvince) %>% 
  group_by(stateProvince) %>% 
  count(scientificName) %>% 
  count(stateProvince) %>% 
  ungroup() %>%
  arrange(n) %>%
  mutate(stateProvince = factor(stateProvince, levels = stateProvince)) %>% 
  ggplot(aes(x = n, y = stateProvince), xaxs = 0) +
           geom_bar(stat = "identity", color = "white", fill = "forestgreen") +
  labs(title = title, subtitle = subtitle, caption = caption) +
  theme_void(base_size = 6, base_family = font) +
  theme(axis.text.x = element_text(family = font, margin = margin(3, 0, 0, 0)),
        axis.text.y = element_text(family = font, margin = margin(0, -10, 0, 0)),
        strip.text = element_blank(),
        panel.grid.major.x = element_line(color = "grey80", linetype = "dashed", linewidth = 0.2),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, margin = margin(0,0,5,0)),
        plot.subtitle = element_text(margin = margin(0,0,5,0)), 
        plot.caption = element_text(family = font, size = 5, margin = margin(10,0,3,0)),
        plot.margin = margin(15,10,0,10))

ggsave("tt_25_35_frog.png", unit = "px", height = 1200, width = 1680, bg = "white")


