library(waffle)
library(tidyverse)
library(dutchmasters)

create_waffle_plot <- function(file_name, column_names, width = 5) {
  column_names <- enquo(column_names)
  workload_data_plot <- read_csv(file_name) %>% 
    filter(!is.na(Category)) %>% 
    rowwise() %>% 
    mutate(total_time = sum(across(all_of(!!column_names)), na.rm = TRUE)) %>% 
    count(Category, wt = total_time) %>% 
    ggplot(
      aes(fill = fct_rev(Category), values = n)
    ) +
    geom_waffle(
      n_rows = width,
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
  
  workload_data_plot
}

create_waffle_plot(file_name = "Workload_2025.csv", 
                   column_names = c("Week of 20/01", "Week of 27/01"))


create_waffle_plot(file_name = "Workload_2025_4weeks.csv", 
                   column_names = c("Week of 20/01", "Week of 27/01",
                                    "Week of 03/02", "Week of 10/02"),
                   width = 10)




