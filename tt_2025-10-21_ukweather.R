# Tidytuesday, 2025-10-21, week 42
# stats_33100

# Data is from the UK Met Office which is the United Kingdom’s national weather and climate service.

# LIBRARIES -------------------------------------------------------------

library(tidyverse)
library(showtext)
library(glue)

# DATA --------------------------------------------------------------------

historic_station_met <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-21/historic_station_met.csv')

year_month_rainfall <- historic_station_met %>% 
  select(station, year, month, rain) %>% 
  na.omit() %>% 
  group_by(year, month) %>% 
  summarise(avg = mean(rain)) %>% 
  ungroup()

month_rainfall <- year_month_rainfall %>% 
  group_by(month) %>% 
  summarise(avg2 = mean(avg)) %>% 
  ungroup()

# year_month_rainfall %>% 
#   group_by(month) %>% 
#   slice_max(avg)

# PLOT  -------------------------------------------------------------------

font <- "Noto Serif"
font_b <- "Open Sans"
showtext_opts(dpi = 300)
font_add_google(font, regular.wt = 400, bold.wt = 700)
font_add_google(font_b, regular.wt = 400, bold.wt = 700)
showtext_auto()

bg_col <- "white"   # background color
t_col <- "black"    # text color

wettest <- round(max(year_month_rainfall$avg),1)

months_txt <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.",
                "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")

year_month_rainfall %>% 
  ggplot() +
  geom_point(
    aes(x = month, y = avg, group = year, colour = year)) +
  geom_line(
    data = month_rainfall, 
    aes(x = month, y = avg2), 
    linetype = 2) +
  coord_cartesian(
    expand = FALSE,
    clip = "off", 
    ylim = c(0,210),
    xlim = c(0.5,12)) +
  scale_y_continuous(breaks = seq(0, 210, 50)) +
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = months_txt) +
  labs(
    title = "Monthly Rainfall in UK 1854-2024",
    subtitle = glue("The wettest months in UK are on average January, October, November and December. \nThe wettest single month in UK's history has been October 1903, with rainfall of {wettest} mm \n(according to the data provided)."),
    caption = "#TidyTuesday: 2025 week 42  | Data: UK Met Office | Graphic: stats33100",
    x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = font, color = t_col),
    axis.line = element_line(color = t_col, linewidth = 0.3),
    axis.text = element_text(color = t_col),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    legend.key.size = unit(0.30, "cm"),
    legend.key.width = unit(0.9, "cm"),
    legend.margin = margin(0,0,0,0),
    legend.position = "top",
    legend.text = element_blank(),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(color = bg_col),
    plot.title.position = "plot",
    plot.title = element_text(
      family = font, 
      size = 16, 
      face = 'bold'),
    plot.subtitle = element_text(
      size = 8, 
      margin = margin(0,0,7,0)),
    plot.caption = element_text(
      size = 5, 
      hjust = 0.5,
      margin = margin(10,0,0,0),
      family = font_b),
    plot.margin = margin(15, 20, 5, 15)) +
  annotate(
    geom = "text", 
    x = c(3.35, 8.6),
    y = 230, 
    label = c("1854", "2024"),
    family = font,
    hjust = 0,
    size = 2.5) +
  annotate(
      geom = "text", 
      x = 10, y = wettest, 
      label = glue("1903"),
      family = font,
      hjust = 1.3,
      size = 2.5) +
  annotate(
    geom = "text", 
    x = 0, y = 220, 
    label = "mm",
    family = font,
    hjust = 0.5,
    size = 3)

ggsave("tt_2025-10-21_uk_weather.png", 
       unit = "px", height = 1200, width = 1600)


# History of the wettest month in UK -----------------------------------------------------------------

# https://en.wikipedia.org/wiki/England_and_Wales_Precipitation

# https://www.trevorharley.com/1903.html
# October 1903 has been the wettest month of all in the England and Wales rainfall series since records began in 1766: 
# an average of 218 mm of rain fell (268% of the long-term average). 450 mm fell in the Lake District. 
# By the end of the month it is estimated that 25,000 acres of farmland were under water, particularly the Ouses, Severn, Trent, and Thames.
# Tornadoes hit Wareham and Banbury on the 25th.
# Bridges were weakened and collapsed: at Risca (Monmouthshir) four arches of a 40 foot high viaduct crossing the Ebbw River collapsed. 
# In the north the harvest was severely affected. 

# https://community.netweather.tv/topic/100492-1903-the-worst-year-on-record/
# It's described as a very dull month practically everywhere with a high frequency of thunderstorms and most places saw rain fall 28-31 days of the month. 

# https://rmets.onlinelibrary.wiley.com/doi/10.1002/gdj3.157
# Hawkins, E. et al. (2022). Millions of historical monthly rainfall observations taken in the UK and Ireland rescued by citizen scientists. https://doi.org/10.1002/gdj3.157
# "October 1903 remains the wettest UK month on record (~219 mm for the UK average)."
