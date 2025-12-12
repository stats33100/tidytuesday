# Tidytuesday, 2025-12-09, week 49
# stats_33100

# This week data, qatarcars R-package, provides a more internationally focused, 
# modern-cars-based demonstration dataset by Paul Musgrave and student.
# Modern version of mtcars (which contains data on a bunch of cars from 1974) 
# that does not suffer from "U.S. defaultism".
# 
# https://musgrave.substack.com/p/introducing-the-qatar-cars-dataset

# 1 EUR = 4.15 QAR

library(tidyverse)
library(treemap)
library(ggfittext)
library(scales)
library(ggtext)
library(showtext)

qatarcars <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-09/qatarcars.csv')


# treemap -----------------------------------------------------------------

font <- "Electrolize"
showtext_opts(dpi = 300)
font_add_google(font)
showtext_auto()


q_data <- qatarcars |> 
  filter(seating == 5) |> 
  mutate(id_tree = row_number(),
         price = round((price / 4.15), 0),
         model = str_glue("{make}\n{model}"))

subtitle_text <- c("Cars grouped as 
<span style='color:#6D8325FF'>**electric**</span>, 
<span style='color:#DD8D29'>**hybrid**</span> or 
<span style='color:#8A1538'>**petrol**</span>. 
Box size depends on the price.")
                                       
data_tree <- q_data|> 
  treemap(
    index="model",
    vSize="price",
    type="categorical",
    vColor = "enginetype",
    algorithm = "pivotSize",
    sortID = "id_tree",
    mirror.y = TRUE,
    mirror.x = TRUE,
    border.lwds = 0.7,
    aspRatio = 5/3)


data_ggplot <- data_tree[["tm"]] %>% 
  as_tibble() %>% 
  arrange(desc(vSize)) %>% 
  mutate(rank = row_number(),
         xmax = x0 + w,
         ymax = y0 + h,
         label_car = model)

data_ggplot |> 
  ggplot() +
  geom_rect(
    aes(xmin = x0, ymin = y0, xmax = xmax, ymax= ymax, fill = vColor),
            linewidth = 0.1, colour = "white") +
  geom_fit_text(
    aes(xmin = x0, xmax = xmax, ymin = y0, ymax = ymax, 
        label = label_car, family = font),
    colour = "white", min.size = 3.5) +
  labs(title = "Five seaters of qatarcars dataset",
       subtitle = subtitle_text,
       caption = "#TidyTuesday: 2025-12-09 | Data: qatarcars | Graphic: stats33100") +
  scale_fill_manual(
    values = c("#6D8325FF", "#DD8D29", "#8A1538")) +
  theme_void() +
  theme(
    text = element_text(colour ="white", family = font),
    legend.position = "none",
    plot.background = element_rect(
      fill = "black"),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      color = "white",
      size = 14),
    plot.subtitle = element_markdown(
      size = 8,
      hjust = 0.5),
    plot.caption = element_text(
      hjust = 0.5,
      color = "white",
      size = 8),
    plot.margin = margin(t = 10, b = 5)
  )

ggsave("tt_2025-12-09_qatarcars.png", 
       unit = "px", height = 1200, width = 1680)
