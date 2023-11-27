library(tidyverse)
library(lubridate)
library(reshape)
library(showtext)

# The Font in the Plot

font_add_google(name = "Bebas Neue",
                family = "bn")
showtext_auto()

# Data Import

tuesdata <- tidytuesdayR::tt_load(2021, week = 17)
netflix <- tuesdata$netflix

# Data Wrangling

netflix_data <- netflix %>%
  mutate(nr_char = nchar(title),
         day = parse_number(date_added),
         month_name = word(date_added),
         month_nr = as.integer(factor(month_name, levels = month.name))) %>%
  separate(date_added, into = c("date","year"), sep = ",") %>%
  select(-date) %>%
  mutate(date_added =  as.Date(paste(year, month_nr, day, sep = "-"),
                               "%Y-%m-%d"),
         year = as.numeric(year)) %>%
  filter(year > 2015) %>%
  arrange(date_added)

# Several Statistics of the Continuous Variables

netfix_summary <- netflix_data %>%
  summary(avg_char = mean(nchar))

# The longest and shortest titles

net <-netflix_data %>%
  filter(nr_char == 104)

# The Subtitle of the Text

subtitle_text <- str_wrap("\n\nThe titles of Netflix shows and movies include 17
characters on the average. The longest title belongs to a movie added to Netflix
in 2017 and the shortest ones which include a single character added in 2016,
2017, 2019 and 2020.\n\n", 120)

# The Plot

p <- ggplot(netflix_data, aes(x = date_added, y = 1, fill = nr_char,
                         color = nr_char)) +
  geom_tile() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradient(low = "#b20710",
                       high = "#000000") +
  scale_colour_gradient(guide="none",
                        low = "#b20710",
                      high = "#000000") +
  coord_cartesian() +
  labs(x = "Year",
       y = "",
       title = "NETFLIX TITLES",
       subtitle = subtitle_text,
       fill = "Number of\nCharacters\nin the Titles",
       caption = "\nData Source: Kaggle | TidyTuesday 2021 - Week 17 | Prepared by: C. YAZICI") +
  theme(panel.background = element_rect(fill = "ivory", color = NA),
        plot.background = element_rect(fill = "ivory", color = NA),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text = element_text(family = "bn", size = 18),
        axis.title = element_text(family = "bn", size = 19),
        legend.title = element_text(family = "bn", size = 19),
        legend.text = element_text(family = "bn", size = 18),
        legend.key.height = unit(2, 'cm'),
        legend.background = element_rect(fill = "ivory"),
        plot.title = element_text(family = "bn", size = 50),
        plot.subtitle = element_text(family = "bn", size = 25),
        plot.caption = element_text(family = "bn", size = 20, hjust = 1),
        plot.margin = margin(0.3, 0.5, 0.5, 0.5, "cm"),
        aspect.ratio = 5/9)

# Save the Plot

ggsave("Netflix_Tiles.png", p, width = 20, height = 15, dpi = 72)


