library(ggplot2)
library(dplyr)
library(tidyr)
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
      plot.margin = margin(rep(15, 4)),
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
  annotate("rect", 
           xmin = 2019, xmax = 2021, 
           ymin = 60, ymax = 90,
           fill = "#FF4500",
           alpha = 0.3) +
  annotate("text", x = 2020, y = 65, label = "COVID", colour = "#CD3700") +
  scale_y_continuous(limits = c(60, 90),
                     expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(labels = number_format(accuracy = 1, big.mark = ""),
                     breaks = df_car_sales$year)

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
                      app_colours$nonrenewables, ";'> 12%</strong> of greenhouse",
                      " gas emissions. It's more than the produced by Industry",
                      " and Waste together."),
    caption = "Based on https://ourworldindata.org/emissions-by-sector"
  ) +
  geom_treemap_subgroup_border(colour = "white", size = 2) +
  geom_treemap_text(colour = "white", size = 12, fontface = c("bold", 
                                                              rep("plain", 4))) +
  scale_fill_manual(values = c(app_colours$no_emphasis, 
                               app_colours$no_emphasis, 
                               app_colours$no_emphasis, 
                               app_colours$nonrenewables, 
                               app_colours$no_emphasis)) +
  theme(legend.position = "none")

### 03 - CARS IN THE WORLD
### Try to create some sort of infographic
df_cars_capita <- tibble(
  country = c("United States", "New Zealand", "Canada", "Cyprus", "Luxembourg",
              "Australia", "Poland", "Italy", "Iceland", "Estonia"),
  cars_per_capita = c(0.89, 0.88, 0.79, 0.79, 0.78,
                      0.78, 0.77, 0.76, 0.72, 0.71)
) %>% 
  mutate(
    country = factor(country, levels = country)
  )

df_cars_capita %>% 
  ggplot(aes(x = country, y = cars_per_capita)) +
  geom_col(fill = app_colours$main) +
  labs(
    title = "Top 10 countries with the higher number of cars per person",
    subtitle = paste0("Some countries tend to have more particular cars than",
                      " sharing them with others. Would it be good if",
                      " everyone had their exclusive car?"),
    caption = "Based on https://www.whichcar.com.au/news/how-many-cars-are-there-in-the-world",
    x = "Country",
    y = "Cars per person"
  ) +
  scale_y_continuous(expand = expansion(mult = 0),
                     limits = c(0, 1),
                     breaks = seq(from = 0, to = 1, by = 0.2))

### 04 - ELECTRIC CAR SALES COMPARED TO NORMAL MODELS
df_electric_car_sales <- df_car_sales %>% 
  mutate(
    electric = c(0, 0, 0.1, 0.2, 0.3, 0.5, 0.8, 1.2, 2.0, 
                 2.0, 3.1, 6.6, 10.8),
    other = sales - electric
  ) %>% 
  select(year, electric, other) %>% 
  pivot_longer(cols = c(electric, other), 
               names_to = "power_type", values_to = "quantity")

df_electric_car_sales %>% 
  ggplot(aes(x = year, y = quantity, 
             fill = factor(power_type, levels = c("other", "electric")))) +
  geom_area() +
  labs(
    title = "Cars sales by power type since 2010",
    subtitle = paste0("Electric car sales had a dramatic increase since 2019",
                      " reaching <strong style='color", app_colours$renewables,
                      ";'>10.8 million</strong> sales in 2022."),
    caption = "Based on https://www.iea.org/data-and-statistics/charts/passenger-car-sales-2010-2022",
    x = "",
    y = "Sales (in million)"
  ) +
  scale_y_continuous(limits = c(0, 90),
                     expand = expansion(mult = c(0, 0))) +
  scale_x_continuous(labels = number_format(accuracy = 1, big.mark = ""),
                     breaks = df_car_sales$year,
                     expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(values = c(app_colours$no_emphasis,
                               app_colours$renewables),
                    labels = paste0("<span style='color:",
                                    c(app_colours$renewables, 
                                      app_colours$no_emphasis),
                                    "'>",
                                    c("Electric", "Conventional"),
                                    "</span>",
                                    "<span> </span>",
                                    "<span style='color:",
                                    c("#474747",
                                      "transparent"),
                                    "'>",
                                    "|",
                                    "</span>")) +
  theme(legend.position = "top",
        legend.text = element_markdown(face = "bold", size = 12),
        legend.justification = c(-.087, 0),
        legend.margin = margin(),
        legend.key.size = unit(1, units = "pt"),
        legend.spacing.x = unit(3, units = "pt")) +
  guides(fill = guide_legend(title = NULL,
                             label.position = "left",
                             override.aes = list(alpha = 0)))
