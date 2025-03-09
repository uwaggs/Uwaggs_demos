library(dplyr)

prem_table <- read.csv("Cleaned_prem_table.csv")

prem_table <- prem_table %>% select(position,team_name,week,image_file)


