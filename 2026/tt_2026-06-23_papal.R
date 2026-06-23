# by stats33100
# tidyverse 2026-06-23
# Papal Encyclicals
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-06-23/readme.md

library(tidyverse)
library(tidytext)
library(showtext)

# DATA --------------------------------------------------------------------

encyclicals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/encyclicals.csv')
#papal_encyclicals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/papal_encyclicals.csv')
#scripture_references <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/scripture_references.csv')

# Split text into tokens
tidy_encyclicals <- encyclicals |>  
  unnest_tokens(word, text) |> 
  anti_join(stop_words) |> 
  drop_na(word) |> 
  select(encyclical, word)

# Count words and check TF-IDF
encyclicals_tf_idf <- tidy_encyclicals |> 
  count(encyclical, word, sort = TRUE) |> 
  bind_tf_idf(word, encyclical, n)
  

# VISUALIZATION -----------------------------------------------------------

font <- "Inter"
font_title <- "Cormorant Garamond"
showtext_opts(dpi = 300)
font_add_google(font)
font_add_google(font_title)
showtext_auto()

subtitle <- "The vocabulary of Pope Leo XIV's Magnifica Humanitas (2026) differs markedly from that of Pope Leo XIII's Rerum Novarum (1891), reflecting 135 years of social and technological change. TF–IDF is used to highlights the eight most distinctive words."
col_txt <- "black"

encyclicals_tf_idf |> 
  group_by(encyclical) |> 
  slice_max(tf_idf, n = 8) |> #print(n = Inf)
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = encyclical)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~encyclical, ncol = 2, scales = "free") +
  coord_cartesian(expand = FALSE) + 
  scale_fill_manual(values = c("#cc0000", "#357cd2")) +
  labs(
    x = "tf-idf", y = NULL,
    title = str_to_upper("From Classes to Economics"),
    subtitle = str_wrap(subtitle),
    caption = "#TidyTuesday: 2026-06-23 | Data: Vatican.va | Graphic: stats33100") +
  theme_minimal() +
  theme(
    text = element_text(size = 8, family = font, color = col_txt),
    axis.ticks.x = element_line(color = col_txt, linewidth = 0.2),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(color = col_txt),
    axis.title = element_blank(),
    plot.title = element_text(
      family = font_title,
      size = 16, 
      hjust = 0,
      face = "bold"),
    plot.caption = element_text(
      hjust = 0.5,
      margin = margin(t=15)),
    strip.text = element_text(
      size = 8, 
      color = col_txt),
    plot.margin = margin(15,20,5,5)
  )
  
ggsave("./2026/tt_2026-06-23_papal.png", 
       unit = "px", height = 1200, width = 1600)
