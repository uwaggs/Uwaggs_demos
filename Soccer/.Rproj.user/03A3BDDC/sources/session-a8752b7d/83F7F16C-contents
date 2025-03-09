prem_table <- read.csv("2020-2021_premier_league_table.csv")
colnames(prem_table)[2] <- "position"
colnames(prem_table)[4] <- "team_name"


# Load required libraries
library(dplyr)
library(stringdist)  # For fuzzy matching

# Define the folder path containing images
image_folder <- "logos"

# Get list of image files
image_files <- list.files(image_folder, full.names = FALSE)  # File names only

# Extract base names without extensions
image_names <- gsub("\\..*$", "", image_files)  # Remove extensions

# Function to find the closest matching filename for a given team
find_closest_match <- function(team_name, image_names) {
  distances <- stringdist::stringdist(team_name, image_names, method = "jw")  # Jaro-Winkler distance
  best_match_index <- which.min(distances)
  return(image_files[best_match_index])  # Return full file name
}

# Apply fuzzy matching for each team
prem_table <- prem_table %>%
  rowwise() %>%
  mutate(image_file = find_closest_match(team_name, image_names))

View(prem_table)


write.csv(prem_table,"Cleaned_prem_table.csv")

