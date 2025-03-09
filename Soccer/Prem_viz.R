library(dplyr)
library(ggplot2)
library(ggbump)
library(ggimage)

prem_table <- read.csv("Cleaned_prem_table.csv")

prem_table <- prem_table %>% select(position,team_name,week,image_file)

prem_table$image_file <- paste("logos/", prem_table$image_file, sep = "")

prem_table <- prem_table %>% mutate(
  fill_color = case_when(
    team_name == "Manchester City" ~ "#6CABDD",
    team_name == "Manchester United" ~ "#DA291C",
    team_name == "Sheffield United" ~ "black",
    .default = "grey50"
  ),
  alpha = ifelse(team_name %in% c("Manchester City", "Manchester United","Sheffield United"), 1, 0.5))

x <- ggplot(prem_table, aes(x = week, y = position, colour = fill_color ,group = team_name)) +
  geom_point(size = 1.7, aes(alpha = alpha)) +
  geom_bump(aes(alpha = alpha), linewidth = 1) +
  scale_color_identity() +
  scale_y_reverse() +  # Rank 1 should be at the top
  theme_minimal() +
  labs(title = "Premier League 2020-2021 Table Week By Week",
       x = "Matchweek",
       y = "Position") +
  theme(legend.position = "none")

x

ggsave("test.png", plot = x, bg = "white")
