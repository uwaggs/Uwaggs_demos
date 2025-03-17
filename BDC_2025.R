library(tidyverse)
library(gganimate)
library(sportyR)
library(ggplot2)

## reading files from bdc github releases
team_a_tracking <- read.csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-10-25.Team.H.@.Team.G.-.Tracking.csv")
team_c_tracking <- read.csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-15.Team.D.@.Team.C.-.Tracking.csv")
team_e_tracking <- read.csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-16.Team.F.@.Team.E.-.Tracking.csv")

## adding dataframes to a list
team_tracking <- list(
  a = team_a_tracking,
  c = team_c_tracking,
  e = team_e_tracking
)

## function to convert time stamp
to_seconds <- function(time) {
  parts <- as.numeric(strsplit(time, ":")[[1]])
  return(parts[1] * 60 + parts[2])
}

## function to create gif takes in a team (one of a,c,e), a timestamp from the game,
## length which is how long the gif is starting from timestamp, and period which is the period
## of the game. it returns the animated plot
create_play <- function(team,timestamp,length,period){
  curr_team <- team_tracking[[team]]

  curr_team <- curr_team %>% filter(Period == period)

  start_time <- to_seconds(timestamp)

  end_time <- start_time - length

  curr_team$seconds <- sapply(curr_team$Game.Clock,to_seconds)

  curr_team <- curr_team %>% filter(end_time <= seconds & (start_time >= seconds ))

  ##reindex the plays that we're looking for
  curr_team$id <- as.numeric(factor(curr_team$Image.Id))

  p <- sportyR::geom_hockey("nhl") + geom_point(data = curr_team %>% filter(Player.or.Puck == "Player"),
                                                aes(x = Rink.Location.X..Feet., y = Rink.Location.Y..Feet.,fill = Team),shape = 21, size = 6,
                                                show.legend = F) +
                                                geom_text(data = curr_team %>% filter(Player.or.Puck == "Player"),
                                                aes(x = Rink.Location.X..Feet., y = Rink.Location.Y..Feet., label = Player.Id),
                                                color = "black", size = 2,show.legend = F) +
                                                geom_point(data = curr_team %>%  filter(Player.or.Puck == "Puck"),
                                                aes(x = Rink.Location.X..Feet., y = Rink.Location.Y..Feet.,
                                                size = Rink.Location.Z..Feet.),fill = "black",shape = 21,size = 3,
                                                show.legend = F) + transition_time(id)


  max_frame = max(curr_team$id)
  min_frame = min(curr_team$id)

  p2 <- animate(p, fps = 30, duration = (max_frame - min_frame)/30 + 1,renderer = gifski_renderer())

  return (p2)
}
## example that is creating a 12 second clip from team e in the first period
x <- create_play("e","14:00",12,1)

x

## how to save these plots
save_animation(x,"test.gif")



## acknowledgements

## took inspiration/ideas from the following repos.
## https://github.com/bigdatacup/Big-Data-Cup-2021/blob/main/OTTHAC_Tutorial/Code/tracking_tutorial.R
## https://github.com/danielhocevar/SMT-Baseball-2023/blob/main/Analysis%20Code.Rmd

View(team_a_tracking)
