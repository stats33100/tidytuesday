# Tidytuesday, 2025-12-02, week 48
# stats_33100

# The Böögg is a snowman effigy made of cotton wool and stuffed with 
# fireworks, created every year for Zurich's "Sechselaeuten" spring festival. 
# The saying goes that the quicker the Böögg's head explodes, the finer 
# the summer will be.

# https://www.meteoswiss.admin.ch/weather/weather-and-climate-from-a-to-z/boeoegg-prediction.html

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(glue)
library(ggtext)
library(correlation)

#  Data -------------------------------------------------------------------

sechselaeuten <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-02/sechselaeuten.csv')

result_duration <- cor_test(
  sechselaeuten, "duration", "tre200m0", method = "auto")
result_year <- cor_test(
  sechselaeuten, "year", "tre200m0", method = "auto")

# Plotting ----------------------------------------------------------------


p1 <- sechselaeuten |> 
  filter(year > 1964) |> 
  ggplot(
    aes(x = duration, y = tre200m0)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "lm", linewidth = 0.3) +
  coord_cartesian(
    #xlim = c(6,60)
    ) +
  scale_x_continuous(
    breaks = seq(10,60,10),
    labels = c("10min", "20min", "30min", "40min", "50min", "60min")) +
  labs(
    x = NULL, y = "Summer temperatures on Swiss plateau / °C",
    title = glue("**Duration** does not correlate with temperature<br>
                 (n = 67, *r* = {round(result_duration$r, 2)}, 
                 *p* = {round(result_duration$p,3)}).")) +
  theme_classic() +
  theme(
    plot.title = element_markdown(size = 8),
    axis.line = element_line(linewidth = 0.2),
    axis.ticks = element_line(linewidth = 0.2),
    axis.ticks.length = unit(2, "pt"),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6),
    panel.grid.major.y = element_line(color = "gray70", linetype = 2, linewidth = 0.2)
  )

p2 <- sechselaeuten |> 
  filter(year > 1964) |> 
  ggplot(
    aes(x = year, y = tre200m0)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "lm", linewidth = 0.3) +
  labs(
    x = NULL, y = NULL,
    title = glue("**Year** correlates with temperature<br>
                 (n = 67, *r* = {round(result_year$r, 2)}, 
                 *p* < .001).")) +
  theme_classic() +
  theme(
    plot.title = element_markdown(size = 8),
    axis.line.x = element_line(linewidth = 0.2),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(linewidth = 0.2),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(2, "pt"),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_line(
      color = "gray70", 
      linetype = 2, 
      linewidth = 0.2)
  )

p12 <- p1 + p2

title_theme <- theme(
  plot.title = element_text(
    face = "bold",
    size = 14,,
    margin = margin (t = 7, b = 5)),
  plot.subtitle = element_text(
    size = 8),
  plot.caption = element_text(
    size = 6)
)

p12 + 
  plot_annotation(
    title = "The Böögg does not foretell the global warming",
    subtitle = 
      "The Böögg statue is created and destroyed every year for the Sechseläuten spring festival in Zurich. \nIt is said that the faster Böögg's head explodes, the more beautiful the summer will be. Of course, \nthe duration of the burn is not actually predicting warming summers.",
    caption = "#TidyTuesday: 2025-12-02 | Data: OpenData for Zurich's Sechseläuten | Graphic: stats33100",
    theme = title_theme)


ggsave("tt_2025-12-02_böögg.png", 
       unit = "px", height = 1200, width = 1600)



# plot: facet -------------------------------------------------------------

subtitle1 <- "**Duration** does not correlate with temperature<br>
                 (n = 67, *r* = {round(result_duration$r, 2)}, 
                 *p* = {round(result_duration$p,3)})."
subtitle2 <- "**Year** correlates with temperature<br>
                 (n = 67, *r* = {round(result_year$r, 2)}, 
                 *p* < .001)."
  

sechselaeuten |> 
  filter(year > 1964) |> 
  select(duration, tre200m0, year) |> 
  pivot_longer(-tre200m0, names_to = "variable", values_to = "value") |> 
  ggplot(
    aes(x = value, y = tre200m0)) +
  geom_point(size = 0.3) +
  geom_smooth(method = "lm", linewidth = 0.3) +
  facet_wrap(~variable, 
             scales = "free_x", 
             #labeller = labeller(variable = c(subtitle1, subtitle2))
             ) +
  coord_cartesian(clip = "off", ylim = c(15, 22)) +
  #   xlim = c(6,60)
  # ) +
  # scale_x_continuous(
  #   breaks = seq(10,60,10),
  #   labels = c("10min", "20min", "30min", "40min", "50min", "60min")) +
  labs(
    y = "Summer temperatures on Swiss plateau / °C",
    title = "The Böögg does not foretell the global warming",
    subtitle = "The Böögg statue is created and destroyed every year for the Sechseläuten spring festival in Zurich. \nIt is said that the faster Böögg's head explodes, the more beautiful the summer will be. Of course, \nthe duration of the burn is not actually predicting warming summers.",
    caption = "#TidyTuesday: 2025-12-02 | Data: OpenData for Zurich's Sechseläuten | Graphic: stats33100") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14),
    plot.subtitle = element_text(size = 8, 
                                 #margin = margin(b = 40)
                                 ),
    axis.line = element_line(linewidth = 0.2),
    axis.ticks = element_line(linewidth = 0.2),
    axis.ticks.length = unit(2, "pt"),
    axis.title = element_text(size = 6),
    axis.text = element_text(size = 6),
    panel.grid.major.y = element_line(
      color = "gray70", linetype = 2, linewidth = 0.2),
    strip.background = element_blank(),
    strip.text = element_blank()
  )

ggsave("tt_2025-12-02_böögg2.png", 
       unit = "px", height = 1200, width = 1600)
