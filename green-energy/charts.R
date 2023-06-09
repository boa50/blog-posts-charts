library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(stringr)
library(scales)

### THEME
library(ggtext)
library(showtext)
font_add_google("Roboto", "roboto")
showtext_auto()

app_colours <- list(
  title = "#474747",
  axis = "#757575",
  legend_title = "#474747",
  legend_text = "#757575",
  subtitle = "#757575",
  caption = "#8f8f8f",
  main = "#1976d2",
  no_emphasis = "#8f8f8f",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c",
  renewables = "#008B45",
  nonrenewables = "#8B4500",
  point_value = "#474747"
)

theme_minimalistic <- function() {
  theme_classic() +
    theme(
      text = element_text(family = "roboto"),
      plot.title = element_text(hjust = 0, colour = app_colours$title),
      plot.title.position = "plot",
      axis.line = element_line(colour = app_colours$axis),
      axis.ticks = element_line(colour = app_colours$axis),
      axis.text = element_text(colour = app_colours$axis),
      axis.title = element_text(colour = app_colours$axis),
      legend.title = element_text(colour = app_colours$legend_title),
      legend.text = element_text(colour = app_colours$legend_text),
      plot.subtitle = element_textbox_simple(colour = app_colours$subtitle,
                                             margin = margin(b = 15)),
      plot.caption = element_text(colour = app_colours$caption),
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}

theme_set(theme_minimalistic())

annotate_point <- function(x, y, colour = "black", text_position = "left") {
  stopifnot(text_position %in% c("left", "right"))
  
  list(
    annotate(
      "point",
      x = {{ x }},
      y = {{ y }},
      size = 5,
      colour = {{ colour }}
    ),
    annotate(
      "text",
      label = label_number(scale_cut = cut_short_scale())({{ y }}),
      x = {{ x }},
      y = {{ y }},
      colour = app_colours$point_value,
      hjust = ifelse(text_position == "left", 1.5, -0.5)
    )
  )
}

# Data from https://ourworldindata.org/global-energy-200-years
df <- read.csv("green-energy/global-energy-substitution.csv", 
               check.names = FALSE) %>% 
  clean_names() %>% 
  filter(year >= 2000) %>% 
  set_names(~ str_replace(., "_t_wh_substituted_energy", "")) %>% 
  select(-c(entity, code)) %>% 
  pivot_longer(!year, names_to = "energy_type", values_to = "consumption") %>% 
  mutate(energy_type = ifelse(
    energy_type %in% c("other_renewables", "wind", "solar", "hydropower"),
    "renewable",
    "nonrenewable"
  )) %>% 
  group_by(year, energy_type) %>% 
  summarise(consumption = sum(consumption))

point_annotations <- filter(df, year %in% c(2000, 2021))

get_consumption_increase <- function(energy_type) {
  stopifnot(energy_type %in% c("renewable", "nonrenewable"))
  
  tmp_df <- point_annotations %>% 
    ungroup() %>% 
    filter(energy_type == {{energy_type}}) %>% 
    pull(consumption) 
  
  return(percent_format()((tmp_df[2] / tmp_df[1]) - 1))
}

annotate_energy_point <- function(x) {
  point <- point_annotations[x,]
  
  point_colour <- ifelse(
    getElement(point, "energy_type") == "renewable",
    app_colours$renewables,
    app_colours$nonrenewables
  )
    
  annotate_point(
    getElement(point, "year"), 
    getElement(point, "consumption"),
    colour = point_colour,
    text_position = ifelse(getElement(point, "year") == 2000, "left", "right")
  )
}

add_legend <- function(label, y, colour) {
  annotate(
    "text",
    label = {{label}},
    x = 2011,
    y = {{y}},
    colour = {{colour}},
    hjust = 1,
    fontface = "bold"
  )
}

ggplot(df, aes(x = year, y = consumption)) +
  geom_line(aes(colour = energy_type), size = 2) +
  map(1:4, annotate_energy_point) +
  labs(title = "Energy consumption since 2000",
       subtitle = paste0(
         "Renewable energy consumption increased by ",
         "<strong style = color:", app_colours$renewables, ";>",
         get_consumption_increase("renewable"), 
         "</strong>. ",
         "Non-renewables, in contrast, grew by ",
         "<strong style = color:", app_colours$nonrenewables, ";>",
         get_consumption_increase("nonrenewable"), 
         "</strong>."),
       x = "Year", 
       y = "Energy Consumption (TWh)",
       caption = "Source: https://ourworldindata.org/global-energy-200-years") +
  add_legend("Non-renewable", y = 128000, colour = app_colours$nonrenewables) +
  add_legend("Renewable", y = 6000, colour = app_colours$renewables) +
  scale_x_continuous(breaks = c(2000, 2021),
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(limits = c(0, max(df$consumption))) +
  scale_colour_manual(values = c(app_colours$nonrenewables, 
                                 app_colours$renewables)) +
  theme(legend.position = "none",
        plot.margin = unit(rep(1, times = 4), "lines"),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
