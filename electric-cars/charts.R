library(ggplot2)
library(dplyr)

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

### CAR SALES CHART
# https://www.iea.org/data-and-statistics/charts/passenger-car-sales-2010-2022
# IEA, Passenger car sales, 2010-2022, IEA, Paris https://www.iea.org/data-and-statistics/charts/passenger-car-sales-2010-2022, IEA. Licence: CC BY 4.0
df <- tibble(
  year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 
           2021, 2022),
  sales = c(66.7, 71.2, 73.8, 77.5, 80.0, 81.8, 86.4, 86.3, 85.6, 81.3, 71.8,
            74.9, 74.8)
)

df %>% 
  ggplot(aes(x = year, y = sales)) +
  geom_line() +
  scale_y_continuous(limits = c(60, 90))
