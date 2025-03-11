devtools::install_github("mtoto/ufc.stats",force = TRUE)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)


fighter_data <- ufc.stats::ufc_stats
View(fighter_data)

# Function to create a circle
create_circle <- function(center = c(0, 0), radius = 1, npoints = 100) {
  theta <- seq(0, 2 * pi, length.out = npoints)
  x <- center[1] + radius * cos(theta)
  y <- center[2] + radius * sin(theta)
  cbind(x, y)
}

# Function to create the human silhouette plot with the provided data
create_human_silhouette <- function(data) {
  # Calculate success rates
  head_rate <- data$head_landed / data$head_attempted
  body_arm_rate <- data$body_landed / data$body_attempted
  left_leg_rate <- data$leg_landed / data$leg_attempted
  right_leg_rate <- data$leg_landed / data$leg_attempted

  # Define coordinates for each body part
  head_coords <- create_circle(center = c(1.5, 9.5), radius = 0.75)
  body_coords <- matrix(c(0.5, 5, 0.5, 8.75, 2.5, 8.75, 2.5, 5, 0.5, 5), ncol = 2, byrow = TRUE)
  left_arm_coords <- matrix(c(-0.5, 5, 0.5, 5, 0.5, 8.75, -0.5, 8.75, -0.5, 5), ncol = 2, byrow = TRUE)
  right_arm_coords <- matrix(c(2.5, 5, 3.5, 5, 3.5, 8.75, 2.5, 8.75, 2.5, 5), ncol = 2, byrow = TRUE)
  left_leg_coords <- matrix(c(0.5, 1, 0.5, 5, 1.5, 5, 1.5, 1, 0.5, 1), ncol = 2, byrow = TRUE)
  right_leg_coords <- matrix(c(1.5, 1, 1.5, 5, 2.5, 5, 2.5, 1, 1.5, 1), ncol = 2, byrow = TRUE)

  # Create sf objects for each body part
  head <- st_polygon(list(head_coords)) %>% st_sfc(crs = 4326) %>% st_sf(part = "head", rate = head_rate)
  body <- st_polygon(list(body_coords)) %>% st_sfc(crs = 4326) %>% st_sf(part = "body", rate = body_arm_rate)
  left_arm <- st_polygon(list(left_arm_coords)) %>% st_sfc(crs = 4326) %>% st_sf(part = "left_arm", rate = body_arm_rate)
  right_arm <- st_polygon(list(right_arm_coords)) %>% st_sfc(crs = 4326) %>% st_sf(part = "right_arm", rate = body_arm_rate)
  left_leg <- st_polygon(list(left_leg_coords)) %>% st_sfc(crs = 4326) %>% st_sf(part = "left_leg", rate = left_leg_rate)
  right_leg <- st_polygon(list(right_leg_coords)) %>% st_sfc(crs = 4326) %>% st_sf(part = "right_leg", rate = right_leg_rate)

  # Combine all parts into one sf object
  silhouette <- bind_rows(head, body, left_arm, right_arm, left_leg, right_leg)

  label_coords <- data.frame(
    x = c(3.25,4.5,3.5),
    y = c(9.5, 7, 3),
    label = c(paste("Head:", data$head_landed, "/", data$head_attempt),
              paste("Body:", data$body_landed, "/", data$body_attempt),
              paste("Legs:", data$leg_landed, "/", data$leg_attempt))
  )

  plot_title <- paste(data$fighter,"in",data$round,"at",data$event)
  # Plot the silhouette
  ggplot() +
    geom_sf(data = silhouette, aes(fill = rate), color = "black") +
    scale_fill_gradient(low = "blue", high = "red") +
    geom_text(data = label_coords, aes(x = x, y = y, label = label), color = "black", size = 4) +
    coord_sf(xlim = c(-1, 6), ylim = c(0, 10)) +  # Adjust the limits to make the plot wider
    theme_void() +
    theme(legend.position = "right") +
    labs(fill = "Success Rate",title = plot_title ) +
    theme(plot.title = element_text(hjust = 0.5))
}

create_human_silhouette(fighter_data[1,])

