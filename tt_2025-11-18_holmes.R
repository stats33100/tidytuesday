# Tidytuesday, 2025-11-17, week 46
# stats_33100

# This data explores the complete line-by-line text of the Sherlock Holmes 
# stories and novels, made available through the {sherlock} R package 
# by Emil Hvitfeldt. 
# https://sherlock-holm.es/ascii/


# LIBRARIES -----------------------------------------------------------------

library(tidyverse)
library(showtext)
library(tidytext)


# DATA --------------------------------------------------------------------

# Get data
holmes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-18/holmes.csv')

# Split text into tokens
tidy_holmes <- holmes |>  
  unnest_tokens(word, text) |> 
  anti_join(stop_words) |> 
  drop_na(word)


# Sentiment Analyse Between Different Lexicons ------------------------------

# We create an index that breaks up each book by 300 words. 
# This is the approximate number of words per page.

words_per_page <- 280

# Calculate sentiments by AFINN
afinn <- tidy_holmes |> 
  filter(
    book == "The Hound of the Baskervilles" | 
      book == "The Valley Of Fear" | 
      book == "The Sign of the Four" | 
      book == "A Study In Scarlet") |>  
  group_by(book) |> 
  mutate(word_count = 1:n(),
         index = word_count %/% words_per_page + 1) |>  
  ungroup() |> 
  inner_join(get_sentiments("afinn")) |> 
  group_by(book, index) |> 
  summarise(sentiment = sum(value)) |> 
  ungroup() |> 
  mutate(method = "AFINN")

# Calculate sentiments by Bing
bing <- tidy_holmes |>  
  filter(
    book == "The Hound of the Baskervilles" | 
      book == "The Valley Of Fear" | 
      book == "The Sign of the Four" | 
      book == "A Study In Scarlet") |>  
  group_by(book) |> 
  mutate(word_count = 1:n(),
         index = word_count %/% words_per_page + 1) |>  
  ungroup() |> 
  inner_join(get_sentiments("bing")) |> 
  mutate(method = "Bing") |> 
  count(book, method, index, sentiment) |> 
  spread(sentiment, n, fill = 0) |> 
  mutate(sentiment = positive - negative) |> 
  select(book, method, index, sentiment)

# Calculate sentiments by NRC
nrc <- tidy_holmes |> 
  filter(
    book == "The Hound of the Baskervilles" | 
      book == "The Valley Of Fear" | 
      book == "The Sign of the Four" | 
      book == "A Study In Scarlet") |>  
  group_by(book) |>  
  mutate(word_count = 1:n(),
         index = word_count %/% words_per_page + 1) |> 
  ungroup() |> 
  inner_join(get_sentiments("nrc")) |> 
  mutate(method = "NRC") |> 
  count(book, method, index = index , sentiment) |> 
  spread(sentiment, n, fill = 0) |> 
  mutate(sentiment = positive - negative) |> 
  select(book, index, method, sentiment)
  

# VISUALISATION -----------------------------------------------------------

font <- "Lora"
font_b <- "Open Sans"
showtext_opts(dpi = 300)
font_add_google(font)
font_add_google(font_b)
showtext_auto()

col_txt = "black"
col_bg = "white"
col_palette = c("#3D5941FF", "#D3BA68FF", "#5D8CA8FF", "#CA562CFF")


# Compine sentiments and visualisate them with facets
  bind_rows(nrc, bing, afinn) |>
    mutate(book = recode(
      book, 
      "A Stady In Scarlet" = "A Stady In Scarlet", 
      "The Hound of the Baskervilles" = "The Hound of \nthe Baskervilles", 
      "The Sign of Four" = "The Sign of Four", 
      "The Valley Of Fear" = "The Valley of Fear")) |> 
    ggplot(
      aes(index, sentiment, fill = book)) +
    geom_bar(
      stat = "identity", show.legend = FALSE) +
    geom_segment(
      x = 0, xend = 66 , y = 0, yend = 0, 
      linewidth = 0.2, color = "black") +
    annotate(
      geom = "text", x = -2, y = 0, 
      label = "0", 
      size = 1.5, 
      hjust = 1) +
    scale_y_continuous(position = "right") +
    scale_fill_manual(values = col_palette) +
    coord_cartesian(
      clip = "off", 
      expand = FALSE, 
      xlim = c(-5, NA)) +
    facet_grid(book ~ method, scales = "free", switch = "y") +
    labs(y = "Sentiment (pos-neg)",
         x = "Page (280 words)",
         title = "Happy NRC - The Chosen Lexicon Affects Sentiments",
         subtitle = str_wrap("Sherlock Holmes novels were analyzed in one-page (280-word) sections using three different lexicons. There is clear difference between Bing and NRC on how positive or negative the used vocabilirality is."),
         caption = "#TidyTuesday: 2025 week 46 | Data: sherlock R package | Graphic: stats33100") +
  theme_void() +
    theme(
      text = element_text(size = 8, family = font, color = col_txt),
      plot.background = element_rect(fill = col_bg),
      axis.ticks = element_line(color = col_txt, linewidth = 0.2),
      axis.title = element_text(color = col_txt, size = 8),
      axis.title.y = element_text(angle = 90, margin = margin(t = 0,0,0,5)),
      plot.title.position = "plot",
      plot.title = element_text(
        size = 12, 
        margin = margin(10,0,5,0),
        hjust = 0.5,
        face = "bold",
        color = col_txt),
      plot.subtitle = element_text(
        color = col_txt,
        margin = margin(b = 10, l = 10)),
      plot.caption.position = "plot",
      plot.caption = element_text(
        family = font_b,
        color = col_txt,
        margin = margin(15,0,0,0),
        hjust = 0.5),
      strip.text = element_text(size = 8),
      plot.margin = margin(5,5,5,5)
    )
  
  
  ggsave("tt_2025-11-18_holmes.png", 
         unit = "px", height = 1400, width = 1600)
  