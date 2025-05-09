
library(ggplot2)
library(dplyr)
library(scico)
library(purrr)
library(ggnewscale)

# Function to create a single trippy comma
make_comma <- function(n = 1000, angle_shift = 0, scale = 1, 
                       x_offset = 0, y_offset = 0, col_palette = "lajolla") {
  t <- seq(0, 25 * pi, length.out = n)
  r <- seq(0, 1.5, length.out = n)
  mod <- sin(seq(0, 30 * pi, length.out = n)) * 0.9
  
  r_mod <- tan(r) + tan(mod)
  r_mod[!is.finite(r_mod) | abs(r_mod) > 5] <- NA
  
  tibble(
    x = scale * r_mod * sin(t + angle_shift) + x_offset,
    y = scale * r_mod * cos(t + angle_shift) + y_offset,
    color_val = mod,
    group = paste0("comma_", sample(1e6, 1)),
    palette = col_palette
  )
}

# Create list of commas
set.seed(108)
comma_list <- map(1:20, ~ make_comma(
  n = 1200,
  angle_shift = runif(1, 0, 2 * pi),
  scale = runif(1, 0.4, 1.5),
  x_offset = runif(1, -10, 10),
  y_offset = runif(1, -7, 7),
  col_palette = sample(c("lajolla", "tokyo", "bamako", "vik", "roma"), 1)
))

# Start base plot
p <- ggplot()

# Loop through each comma and layer it with its own color scale
for (i in seq_along(comma_list)) {
  comma <- comma_list[[i]]
  pal <- unique(comma$palette)
  
  p <- p +
    geom_path(
      data = comma,
      aes(x, y, color = color_val, group = group),
      size = 0.4,
      alpha = 0.8,
      lineend = "round"
    ) +
    scale_color_scico(palette = pal) +
    ggnewscale::new_scale_color()
}

# Final styling
p +
  theme_void() +
  theme(legend.position = "none") +
  coord_equal(xlim = c(-15, 15), ylim = c(-10, 10))
