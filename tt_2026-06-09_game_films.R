# by ststa33100
# tidyverse 2026-06-09
# Films Based on Video Games
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-06-09/readme.md

library(tidyverse)
library(showtext)
library(ggtext)

game_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')

font <- "Open Sans"
showtext_opts(dpi = 300)
font_add_google(font)
showtext_auto()


game_films |> 
  drop_na(rotten_tomatoes, metacritic) |> 
  select(title, release_date, rotten_tomatoes, metacritic) |> 
  mutate(color = if_else(rotten_tomatoes > metacritic, "rotten", "meta")) |> 
  pivot_longer(c(rotten_tomatoes, metacritic), names_to = "site") |> 
  ggplot(aes(x = release_date, y = value)) +
  geom_line(
    aes(group = release_date, color = color),
    show.legend = FALSE) +
  geom_point(aes(color = site)) +
  scale_color_manual(
      values = c("rotten" = "tomato", "meta" = "darkblue", 
                 "rotten_tomatoes" = "tomato", "metacritic" = "darkblue"),
      labels = c("","Metacritic", "", "Rotten Tomatoes")) +
  coord_cartesian(
    expand = FALSE,
    ylim = c(0,90),
    xlim = as.Date(c("1991-12-01", "2026-12-01"))) +
  labs(
    title = "Novelty Attracts Rotten Tomatoes Users",
    subtitle = "Compared to <span style='color:darkblue'>**Metacritic**</span> users, <span style='color:#FA320A'>**Rotten Tomatoes**</span> users have rated gaming-themed films <br>released after 2020 more positively and older films more negatively.",
    caption = "#TidyTuesday: 2026-06-09 | Data: wikipedia | Graphic: stats33100") +
  theme_classic() +
  theme(
    text = element_text(family = font, size = 6),
    plot.title = element_text(size = 12),
    plot.subtitle = element_markdown(margin = margin(0,0,10,0)),
    axis.line = element_line(linewidth = 0.2),
    axis.ticks = element_line(linewidth = 0.2),
    axis.ticks.length = unit(2, "pt"),
    axis.title = element_blank(),
    axis.text = element_text(size = 6),
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggsave("./2026/tt_2026-06-09_gamefilms.png", 
       unit = "px", height = 1200, width = 1200)



# Kunihiko Yuyama ---------------------------------------------------------

# Extra table about extraordinary career of Kunihiko Yuyama

library(gt)

game_films |> 
  filter(director == "Kunihiko Yuyama") |> 
  select(director, release_date, title) |> 
  gt() |>
  tab_header(
    title = "Extraordinary Career of Kunihiko Yuyama",
    subtitle = "Every July for 20 years new Pokemon film was released by Kunihiko Yuyama")
