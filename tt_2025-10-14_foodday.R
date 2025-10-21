# Tidytuesday, 2025-10-14, week 41
# stats_33100

# World Food Day (2025-10-16) is celebrating the foundation of 
# The Food and Agriculture Organization of the United Nations (FAO). 
# Data is looking at the FAO's Suite of Food Security Indicators.


# PREPERATIONS -------------------------------------------------------------

library(tidyverse)
library(showtext)

font <- "Open Sans"
font_add_google(font, regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

bg_col <- "white"   # background color
t_col <- "black"    # text color
hl_col <- "#ff4000" # highlight color

theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = font, color = t_col),
      axis.line = element_line(color = t_col, linewidth = 0.3),
      axis.text = element_text(color = t_col),
      panel.grid = element_blank(),
      plot.background = element_rect(color = bg_col),
      plot.title.position = "plot",
      plot.title = element_text(
        family = font, 
        size = 18, 
        face = 'bold'),
      plot.subtitle = element_text(
        size = 8, 
        margin = margin(0,0,7,0)),
      plot.caption = element_text(
        size = 5, 
        hjust = 0.5,
        margin = margin(10,0,0,0)),
      plot.margin = margin(15, 20, 5, 15))
)

# DATA --------------------------------------------------------------------

food_security <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-14/food_security.csv') %>% 
  rename_all(tolower)
  
plot_data <- food_security %>% 
  select(year = year_end, area, item, value) %>% 
  filter(
    item == "Prevalence of moderate or severe food insecurity in the female adult population (percent) (annual value)" |
      item == "Prevalence of moderate or severe food insecurity in the male adult population (percent) (annual value)") %>% 
  filter(
      area == "Africa" |
      area == "Asia" |
      area == "Europe" |
      area == "Northern America" |
      area == "South America" |
      area == "Oceania"
  )

# PLOT  -------------------------------------------------------------------

plot_data %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, group = item, color = item)) +
  facet_grid(~area,
             labeller = as_labeller(c("Africa"  = "Africa",
                                      "Asia" = "Asia",
                                      "Europe" = "Europe",
                                      "Northern America" = "N. America", 
                                      "South America" = "S. America",
                                      "Oceania" = "Oceania"))) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(breaks = seq(10,60,10)) +
  scale_color_manual(labels = c("Prevalence of moderate or severe food insecurity in the female adult population (%)",
                                "Prevalence of moderate or severe food insecurity in the male adult population (%)"),
                     values = c("#ee9453", "#116bac")) +
  #legend(legend = c("male", "female")) +
  labs(
    title = "World Food Insecurity 2014-2024",
    subtitle = "The level of moderate to severe food insecurity varies greatly by region. Although women are \nmore likely to be food insecure overall, the importance of gender is particularly evident in \nSouth America.",
    caption = "#TidyTuesday: 2025 week 41\nData: The Food and Agriculture Organization of the United Nations (FAO)\nGraphic: stats33100",
    y = NULL,
    x = NULL) +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.text = element_text(size = 6),
    legend.title = element_blank(),
    legend.margin = margin(0,0,0,0),
    legend.key.size = unit(0.32, "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 5),
    panel.grid.major.y = element_line(
      color = "gray80", 
      linewidth = 0.15, 
      linetype = 2)
  )

ggsave("tt_2025_10_14_foodday.png", 
       unit = "px", height = 1200, width = 1600)
