# install.packages("waffle")
# devtools::install_github("EdwinTh/dutchmasters")
library(waffle)
library(tidyverse)
library(dutchmasters)
workload_data <- read_csv("Workload_2025.csv")
workload_data <- workload_data %>% 
  mutate(fortnight = `Week of 20/01` + `Week of 27/01`)

# simple bar chart is a good start, but boring
plot_with_bars <- workload_data %>% 
  filter(!is.na(Category)) %>% 
  group_by(Category) %>% 
  summarise(total_hours = sum(fortnight, na.rm = TRUE)) %>% 
  ggplot(aes(x = Category, y = total_hours)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels=scales::percent_format(scale = 1))

# waffle plot is better
waffle_plot <- workload_data %>% 
  filter(!is.na(Category)) %>% 
  count(Category, wt = fortnight) %>% 
  ggplot(
    aes(fill = fct_rev(Category), values = n)
  ) +
  geom_waffle(
    n_rows = 5,
    size = 0.33, 
    colour = "white",
    flip = TRUE
  ) +
  # nice colour palette from the dutchmasters package
  # scale_fill_brewer(palette = "Accent") also worked ok
  scale_fill_dutchmasters(palette = "pearl_earring")+
  coord_equal() +
  theme_void() +
  theme_enhance_waffle() +
  guides(fill=guide_legend(title="Category"))

waffle_plot

# Could work on a pictogram next time
# The below doesn't work yet
pict_plot <- workload_data %>% 
  filter(!is.na(Category)) %>% 
  count(Category, wt = fortnight) %>% 
  geom_pictogram(
    n_rows = 7, 
    aes(colour = Category), 
    flip = TRUE, 
    make_proportional = TRUE
  )
  
