library(ggplot2)
library(dplyr)
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
  no_emphasis = "#CDC9C9",
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

### 01 - CAR SALES CHART
# https://www.iea.org/data-and-statistics/charts/passenger-car-sales-2010-2022
df_car_sales <- tibble(
  year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 
           2021, 2022),
  sales = c(66.7, 71.2, 73.8, 77.5, 80.0, 81.8, 86.4, 86.3, 85.6, 81.3, 71.8,
            74.9, 74.8)
)

df_car_sales %>% 
  ggplot(aes(x = year, y = sales)) +
  geom_line(colour = app_colours$main, linewidth = 1) +
  labs(
    title = "Cars sales since 2010",
    subtitle = paste0("Even with a decrease during COVID's worst days, car",
                      " sales are stepping up again, reaching",
                      " <strong style='color: ", app_colours$main,";'>",
                      "74.8 million</strong> sales on 2022."),
    caption = "Based on https://www.iea.org/data-and-statistics/charts/passenger-car-sales-2010-2022",
    x = "",
    y = "Sales (in million)"
  ) +
  scale_y_continuous(limits = c(60, 90)) +
  scale_x_continuous(labels = number_format(accuracy = 1, big.mark = ""),
                     breaks = df$year)

### 02 - GREENHOUSE EMISSION
library(treemapify)

df_greenhouse <- tibble(
  macro_group = c("Energy", "Energy", "Land Use", "Industry", "Waste"),
  group = c("Road Transport", "Other Energies", "Land Use", "Industry", "Waste"),
  percentage = c(11.9, 61.3, 18.4, 5.2, 3.2)
)

df_greenhouse %>% 
  ggplot(aes(area = percentage, fill = group, 
             label = paste(group, 
                           percent_format(scale = 1)(percentage), 
                           sep = "\n"),
             subgroup = macro_group)) +
  geom_treemap() +
  labs(
    title = "Greenhouse gas emissions by sector",
    subtitle = paste0("Road transport represents almost <strong style='color:", 
                      app_colours$nonrenewables, ";'>12%</strong> of greenhouse",
                      " gas emissions. It's more than the produced by Industry",
                      " and Waste together."),
    caption = "Based on https://ourworldindata.org/emissions-by-sector"
  ) +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_text(colour = "white", size = 12, fontface = c("bold", rep("plain", 4))) +
  scale_fill_manual(values = c(app_colours$no_emphasis, 
                               app_colours$no_emphasis, 
                               app_colours$no_emphasis, 
                               app_colours$nonrenewables, 
                               app_colours$no_emphasis)) +
  theme(legend.position = "none")
