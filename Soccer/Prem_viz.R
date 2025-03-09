library(dplyr)
library(ggplot2)
library(ggimage)

prem_table <- read.csv("Cleaned_prem_table.csv")

prem_table <- prem_table %>% select(position,team_name,week,image_file)

prem_table$image_file <- paste("logos/", prem_table$image_file, sep = "")

ggplot(prem_table, aes(x = week, y = position, group = team_name)) +
  geom_line(aes(color = team_name), size = 1) +  # Team ranking trend lines
  geom_image(aes(image = image_file), size = 0.05) +  # Team logos at each point
  scale_y_reverse() +  # Rank 1 should be at the top
  theme_minimal() +
  labs(title = "Team Rankings Over Weeks",
       x = "Week",
       y = "Rank") +
  theme(legend.position = "none")  #
