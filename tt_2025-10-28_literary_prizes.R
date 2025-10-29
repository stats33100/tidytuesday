# Tidytuesday, 2025-10-28, week 43
# stats_33100

# Data: Selected British Literary Prizes (1990-2022) from the Post45 Data Collective.
  # "This dataset contains primary categories of information on individual authors. 
  # We (Post45 Data Collective) aim to provide  information to assess the cultural, 
  # social and political factors determining literary prestige."


# LIBRARIES -------------------------------------------------------------

library(tidyverse)
library(showtext)
library(patchwork)


# DATA --------------------------------------------------------------------

prizes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv')

plot_by_decade <- prizes %>% 
  filter(prize_institution == "Costa" | prize_institution == "Whitbread") %>% 
  select(prize_year, prize_alias, gender) %>% 
  mutate(gender = if_else(gender == "transman", "man", gender)) %>% 
  mutate(decade = prize_year - prize_year %% 10) %>% 
  group_by(decade) %>% 
  count(gender) %>%
  group_by(decade) %>% 
  reframe(gender, n, percent = n / sum(n) * 100)
  

plot_by_decade_and_prize_alias <- prizes %>% 
  filter(prize_institution == "Costa" | prize_institution == "Whitbread") %>% 
  select(prize_year, prize_alias, gender) %>%
  mutate(gender = if_else(gender == "transman", "man", gender)) %>% 
  mutate(decade = prize_year - prize_year %% 10) %>% 
  group_by(decade, prize_alias) %>% 
  count(gender) %>% 
  ungroup() %>% 
  pivot_wider(names_from = gender, values_from = n, values_fill = 0) %>% 
  pivot_longer(!c(decade, prize_alias), names_to = "gender", values_to = "n") %>% 
  group_by(decade, prize_alias) %>% 
  reframe(gender, prize_alias, n, percent = n / sum(n) * 100)


# PLOT PREPERATION -------------------------------------------------------------------

font <- "Open Sans" # "Special Gothic" would be close to Costa font but it doesn't work for some reason.
font_b <- "Open Sans"
showtext_opts(dpi = 300)
font_add_google(font, regular.wt = 400, bold.wt = 700)
font_add_google(font_b, regular.wt = 400, bold.wt = 700)
showtext_auto()

col_bg <- "#F5F4EF"   # background color
col_t <- "#6d1f37"    # text color

col_h1 <- "#6d1f37"   # "Costa" color
col_h2 <- "rosybrown2"
col_h3 <- "#B51144"   # "Costa banner" color

# set theme

set_theme(
  theme_void() +
    theme(
      text = element_text(
        family = font, 
        color = col_t, 
        size = 7),
      axis.text.y = element_text(color = col_t),
      plot.background = element_rect(fill = col_bg)
      )
  )

label_years <- c("1990", "2000", "2010", "2020")


# PLOTS -------------------------------------------------------------------

# Plot 1

label_percent1 <- plot_by_decade %>% 
  mutate(percent = if_else(percent > 20, percent, NA),
         percent = round(percent, 1),
         percent = as.character(percent),
         percent = if_else(is.na(percent), NA, paste0(percent, '%'))) %>% 
  pull(percent)

p1 <- plot_by_decade %>%
  mutate(percent = round(percent, 1)) %>% 
  mutate(decade = factor(decade)) %>% 
  ggplot(aes(x = percent, y = decade, fill = gender, label = label_percent1)) +
  geom_col(width = 0.7, color = col_bg) +
  geom_text(
    position = position_stack(vjust = 0.5), 
    size = 6, 
    size.unit = "pt", 
    color = col_bg) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_discrete(label = label_years) +
  scale_fill_manual(
    breaks = c("woman", "non-binary", "man"),
    values = c(col_h3, col_h2, col_h1)) +
  labs(x = NULL, y = NULL,
       title = "Gender Distribution of Costa Literaty Prizes",) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(
      color = col_t, 
      margin = margin(3,3,3,3)),
    plot.margin = margin(5,5,0,5),
    plot.title = element_text(
      face = "bold", 
      size = 12, 
      hjust = 0.5,
      margin = margin(5,0,5,0)),
    plot.title.position = "plot") +
  annotate(geom = "text", x = 0, y = 8,
           label = "The Costa Book Awards (Whitbread Book Awards from 1971 to 2005) were a set of \nannual literary awards from 1971 to 2021 recognising English-language books by \nwriters based in UK and Ireland. Here is the gender distribution of Costa Prize \nwinners by decade from 1991 to 2022.",
           color = col_t, hjust = 0, vjust = 1, size = 2.5, family = font) +
  annotate(geom = "text", x = 32, y = 4.7,
           label = "All Costa prize winners 1991-2022",
           color = col_t, hjust = 0, size = 2.2, family = font, fontface = "bold")


# Plot 2

label_percent2 <- plot_by_decade_and_prize_alias %>% 
  mutate(percent = if_else(percent > 20, percent, NA),
         percent = round(percent, 1), 
         percent = as.character(percent), 
         percent = if_else(is.na(percent), NA, paste0(percent, '%'))) %>% 
  pull(percent)

p2 <- plot_by_decade_and_prize_alias %>% 
  mutate(category = paste(decade, prize_alias)) %>% 
  mutate(prize_alias = str_remove(prize_alias, "Costa ")) %>% 
  ggplot(
    aes(x = percent, y = category, fill = gender, label = label_percent2)) +
  geom_col(
    width = 0.8, 
    color = col_bg) +
  geom_text(
    position = position_stack(vjust = 0.5), 
    size = 6, 
    size.unit = "pt", 
    color = col_bg) +
  coord_cartesian(
    expand = FALSE, clip = "off") +
  scale_y_discrete(
    label = label_years) +
  scale_fill_manual(
    breaks = c("woman", "non-binary", "man"),
    values = c(col_h3, col_h2, col_h1)) +
  facet_wrap(
    ~prize_alias, nrow = 2, scale = "free") + 
  labs(x = NULL, y = NULL,
       caption = "#TidyTuesday: 2025 week 43 | Data: Post45 Data Collective | Graphic: stats33100") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(0.32, "cm"),
    strip.text = element_text(
      face = "bold", 
      size = 5.5, 
      margin = margin(5,5,2,5), 
      color = col_h1),
    panel.spacing = unit(5, "points"),
    axis.text.y = element_text(
      color = col_t, 
      margin = margin(3,3,3,3)),
    plot.title = element_text(
      face = "bold", 
      size = 16, 
      margin = margin(5,0,5,0),
      hjust = 0.5),
    plot.caption.position = "plot",
    plot.caption = element_text(
      hjust = 0.5, 
      margin = margin(10,0,0,0)),
    plot.title.position = "plot",
    plot.margin = margin(3,10,5,10)
  )

# Combined plot

p1 / p2 + plot_layout(heights = c(0.4, 0.6))

ggsave("tt_2025-10-28_literary_prizes.png", 
       unit = "px", height = 1400, width = 1400)
