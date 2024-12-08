library(readxl)
FAOSTAT_data_en_12_8_2024 <- read_excel("~/Downloads/FAOSTAT_data_en_12-8-2024.xls")
data<-FAOSTAT_data_en_12_8_2024
library(ggplot2)
library(gganimate)
library(dplyr)
# Data preprocessing steps
area_data <- data %>%
  filter(Element == "Area harvested") %>%
  select(Area, Year, Value) %>%
  rename(area_harvested = Value) %>%
  mutate(Year = as.integer(Year))

yield_data <- data %>%
  filter(Element == "Yield") %>%
  select(Area, Year, Value) %>%
  rename(yield = Value) %>%
  mutate(Year = as.integer(Year))
# Combine the data
plot_data <- inner_join(area_data, yield_data, by = c("Area", "Year"))

# Create the animated scatter plot with modified legend sizes
scatter_animation <- ggplot(plot_data, 
                            aes(x = area_harvested, 
                                y = yield, 
                                color = Area,
                                size = area_harvested)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "gray98", alpha = 0.1) +
  geom_point(alpha = 0.8, stroke = 0.5) +
  scale_size_continuous(
    range = c(2, 8),        # Adjusted legend symbol sizes
    breaks = seq(0, 14, by = 2),
    limits = c(0, 14),
    guide = guide_legend(override.aes = list(alpha = 1))
  ) +
  scale_x_continuous(
    breaks = seq(0, 14, by = 2),
    limits = c(0, 14)
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, by = 50),
    limits = c(0, 200)
  ) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Cotton Seed (Unginned) Production Analysis",
       subtitle = "Year: {as.integer(frame_time)}",
       x = "Area Harvested (Mha)",
       y = "Yield (M/ha)",
       color = "Country",
       size = "Area Harvested") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 12, margin = margin(b = 7)),    # Already bold
    plot.subtitle = element_text(size = 12, margin = margin(b = 20)), # Added bold
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.8, "cm"),
    legend.spacing = unit(0.2, "cm"),
    legend.margin = margin(5, 5, 5, 5),
    legend.box.spacing = unit(0.2, "cm"),
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_line(color = "gray95", size = 0.1),
    axis.title = element_text(size = 12),             # Made all axis titles bold
    axis.text = element_text(size = 12, color = "gray30"),
    axis.title.x = element_text(margin = margin(t = 7)), # Added explicit bold
    axis.title.y = element_text(margin = margin(r = 7))  # Added explicit bold
  ) +
  transition_time(Year) +
  ease_aes('linear') +
  shadow_wake(wake_length = 0.15, alpha = 0.25) +
  enter_fade() +
  exit_fade()

# Save the animation
anim_save("cotton_scatter_animation.gif",
          scatter_animation,
          fps = 20,
          duration = 15,
          width = 2500,
          height = 1900,
          renderer = gifski_renderer(file = NULL),
          res = 300)
