# Tidytuesday, 2025-11-04, week 44
# stats_33100

#This data is collected in Flint, Michigan in 2015. The samples explore lead levels in water. The data comes from a paper by Loux and Gibson (2018).
# https://academic.oup.com/jrssig/article/14/2/16/7029247?login=false
library(tidyverse)
library(ggtext)
library(showtext)
library(glue)


# DATA --------------------------------------------------------------------


flint_mdeq <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv') %>% 
  mutate(lead_all = lead) %>% 
  mutate(lead = lead2) %>% 
  select(sample, lead, lead_all) %>% 
  mutate(sampler = "mdeq") 

flint_vt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv') %>% 
  mutate(sampler = "vt")

# PLOT --------------------------------------------------------------------

font <- "Open Sans"
showtext_opts(dpi = 300)
font_add_google(font)
showtext_auto()

col_vt1 <- "#6a2c3e" # Virginia Tech
col_vt2 <- "#CF4520" # Virginia Tech 2
col_mdeq <- "#277c78" # Michigan Department of Environmental Quality (MDEQ)

mdeq_p90 <- quantile(flint_mdeq$lead, 0.9, na.rm = TRUE) %>% as.numeric()
vt_p90 <- quantile(flint_vt$lead, 0.9, na.rm = TRUE) %>% as.numeric()
threshold <- 15
mdeq_all_p90 <- quantile(flint_mdeq$lead_all, 0.9, na.rm = TRUE) %>% as.numeric()

text1 <- glue("<span style='color:#6a2c3e'>**Virginia Tech**</span> coordinated sample collection (n = {nrow(flint_vt)}) included samples as **high as {max(flint_vt$lead)} ppb**. This alternative data had P90 of {vt_p90} ppb – {round(100 * vt_p90 / threshold, 1)}% of the threshold.")
text2 <- glue(" Of the {nrow(flint_mdeq)} samples taken by <span style='color:#277c78'>**MDEQ**</span>, the two results were questionably removed. In the used data, **the highest values remain at {max(flint_mdeq$lead, na.rm = TRUE)} ppb**, leaving P90 below the threshold.")
subtitle <- glue("The city of Flint, Michigan, had been plagued for years with symptoms of lead-contaminated water, but the Michigan Department of Environmental Quality (MDEQ) claimed that lead levels were below the lead threshold (ppb < {threshold}). Flint resident LeeAnne Walters and Virginia Tech environmental engineer Marc Edwards began new sampling in 2015 to provide an alternative view of the situation.")

ggplot() +
  geom_density(data = flint_mdeq,
               aes(x = -after_stat(density), y = lead), 
               fill = col_mdeq, color = NA) +
  geom_density(data = flint_vt, 
               aes(x = after_stat(density), y = lead), # (..density..)
               fill = col_vt1, color = NA) +
  geom_vline(xintercept = 0, linewidth = 0.2) +
  geom_hline(yintercept = mdeq_p90, color = col_mdeq) +
  geom_hline(yintercept = vt_p90, color = col_vt1) + 
  geom_hline(yintercept = 15, color = "black", linetype = 3) +
  labs(y = "Lead levels in parts per billion (ppb)",
       x = "Sample density",
       title = "Perseverance saved the city of Flint",
       subtitle = str_wrap(subtitle),
       caption = "#TidyTuesday: 2025 week 44 | Data: Loux and Gibson (2018) | Graphic: stats33100") +
  coord_cartesian(expand = FALSE, clip = "off", 
                  ylim = c(0, 160),
                  xlim = c(-0.13, 0.14)) +
  scale_x_continuous(breaks = seq(-0.2, 0.2, 0.1),
                     #labels = c("-20%", "-10%", "0%", "10%", "20%")
  ) +
  scale_y_continuous(breaks = seq(0, 200, 40)) +
  theme_minimal() +
  theme(
    text = element_text(size = 8, family = font),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.2),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", size = 6),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 14, 
      margin = margin(10,0,5,0),
      hjust = 0.5,
      face = "bold"),
    plot.subtitle = element_text(margin = margin(b=10, l=10)),
    plot.caption.position = "plot",
    plot.caption = element_text(
      color = "black",
      margin = margin(10,0,0,0),
      hjust = 0.5)
  ) +
  annotate(geom = "text", x = 0.14, y = c(vt_p90, threshold), 
           label = c("VT P90 = 26.6 ppb", "Threshold = 15 ppb"),
           color = c(col_vt1, "black"),
           vjust = -0.5,
           hjust = 1,
           size = 2) +
  annotate(geom = "text", x = 0.14, y = mdeq_p90, 
           label = glue("MDEQ P90 = {mdeq_p90} ppb"), 
           color = col_mdeq,
           vjust = 1.5,
           hjust = 1,
           size = 2) +
  annotate("curve", x = -0.06, xend = -0.015, 
           y = 87, yend = 42,
           arrow = arrow(length = unit(0.07, "inches"), type = "closed"),
           curvature = 0.2,
           colour = "black",
           linewidth = 0.3) +
  annotate("curve", x = 0.05, xend = 0.005, 
           y = 142, yend = 158,
           arrow = arrow(length = unit(0.07, "inches"), type = "closed"),
           curvature = 0.2,
           colour = "black",
           linewidth = 0.3) +
  geom_textbox(
    aes(x = -0.125, y = 140, label = text2), 
    hjust = 0, vjust = 1, 
    color = NA, # box.size = 0,
    fill = NA,  # "transparent"
    text.colour = "black",
    family = font,
    size = 2.8,
    maxwidth = unit(1.9, "in")) +
  geom_textbox(
    aes(x = 0.01, y = 140, label = text1), 
    hjust = 0, vjust = 1, 
    color = NA, # box.size = 0,
    fill = NA,  # "transparent"
    text.colour = "black",
    family = font,
    size = 2.8,
    maxwidth = unit(1.9, "in"))


ggsave("tt_2025-11-04_lead_flint.png", 
       unit = "px", height = 1400, width = 1400)



# 
# 
# ggplot() +
#   geom_histogram(data = flint_mdeq_e,
#                  aes(y = -(..density..), x = lead), 
#                  fill = "grey20", binwidth = 1) +
#   geom_histogram(data = flint_vt_e, 
#                  aes(y = (..density..), x = lead), 
#                  fill = "green4", binwidth = 1) +
#   geom_hline(yintercept = 0, linewidth = 0.2) +
#   labs(x = "Lead levels in parts per billion (ppb)",
#        y = "density%", 
#        title = "Lead in water") +
#   coord_cartesian(expand = FALSE, clip = "off") +
#   scale_y_continuous(breaks = seq(-1, 0.2, 0.1),
#                      #labels = c("-20%", "-10%", "0%", "10%", "20%")
#   ) +
#   scale_x_continuous(breaks = seq(0, 200, 40)) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 8),
#     plot.background = element_rect(fill = "white"),
#     panel.grid = element_blank(),
#     axis.ticks = element_line(color = "black", linewidth = 0.2),
#     axis.text = element_text(color = "black")
#   )
# 
