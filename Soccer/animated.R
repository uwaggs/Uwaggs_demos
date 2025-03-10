library(dplyr)
library(ggplot2)
library(ggbump)
library(ggimage)
library(gganimate)

prem_table <- read.csv("Cleaned_prem_table.csv")

prem_table <- prem_table %>% select(position,team_name,week,image_file)

prem_table$image_file <- paste("logos/", prem_table$image_file, sep = "")

View(prem_table)

team_colours <- c(
  "Arsenal FC" = "#EF0107",
  "Leicester City" = "#003090",
  "Chelsea FC" = "#034694",
  "Manchester City" = "#6CABDD",
  "Newcastle United" = "#241F20",
  "Wolverhampton Wanderers" = "#FDB913",
  "Liverpool FC" = "#c8102E",
  "Crystal Palace" = "#1B458F",
  "Everton FC" = "#003399",
  "Manchester United" = "#DA291C",
  "Leeds United" = "#FFCD00",
  "Burnley FC" = "#6C1D45" ,
  "Southampton FC" = "#d71920",
  "Tottenham Hotspur" = "#132257",
  "Brighton & Hove Albion" = "#0057B8",
  "Aston Villa" = "#95bfe5",
  "Sheffield United" = "#EE2737",
  "West Ham United" = "#7A263A",
  "Fulham FC" = "#000000",
  "West Bromwich Albion" = "#122F67"
)

team_colours_df <- data.frame(
  team_name = names(team_colours),
  colour = unname(team_colours)
)

prem_table <- prem_table %>%
  left_join(team_colours_df, by = "team_name")

ggplot(prem_table, aes(x = week, y = -position, group = team_name)) +
  geom_line(aes(color = colour), size = 1, alpha = 0.7) +  # Track movement
  geom_image(aes(image = image_file), size = 0.05) +  # Show logos for all teams
  scale_y_continuous(labels = abs) +  # Keep rankings positive
  labs(title = "League Position Over The Entire Season",
       x = "Week", y = "League Position") +
  scale_color_identity() +
  theme_minimal() +
  transition_reveal(week) +
  shadow_mark(alpha = 0.2, size = 0.04) +
  ease_aes('linear') +
  theme(legend.position = "none")

anim_save("league_rankings.gif", animation = last_animation())



