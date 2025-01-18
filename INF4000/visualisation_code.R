# Aggregate Goals Visualisation

# To run this code, download and extract the file "cleaned_match_info.csv" and change the filepath accordingly.




library(ggplot2)  
library(dplyr)    
library(lubridate) 


data <- read.csv("C:/Users/ethan/Downloads/cleaned_data/cleaned_match_info.csv")

# Filter matches where England is either the home or away team
england_matches <- subset(data, hometeamname == "England" | awayteamname == "England")


england_matches <- england_matches %>%
  mutate(
    england_goals = ifelse(hometeamname == "England", scorehome, scoreaway),  # Goals scored by England
    opponent_goals = ifelse(hometeamname == "England", scoreaway, scorehome), # Goals scored by the opponent
    aggregate_score = england_goals - opponent_goals,  # Goal difference (aggregate score)
    point_color = case_when(
      aggregate_score > 0 ~ "green",  # Green for wins
      aggregate_score == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Convert 'dateandtimecet' to a Date object for easier date handling
england_matches <- england_matches %>%
  mutate(match_date = as.Date(dateandtimecet))

# Aggregate England's goal difference (aggregate score) by match date
england_aggregate_scores <- england_matches %>%
  group_by(match_date) %>%  # Group data by match date
  summarise(
    aggregate_score = sum(aggregate_score),  # Sum of aggregate scores per match date
    final_score = paste(england_goals, ":", opponent_goals),  # Combine scores into "England's Goals:Opponent's Goals"
    point_color = case_when(
      aggregate_score > 0 ~ "green",  # Green for wins
      aggregate_score == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Add a matchday number for England's matches (sequential numbering of matches)
england_aggregate_scores <- england_aggregate_scores %>%
  mutate(matchday = seq_along(match_date))

# Filter matches where Italy is either the home or away team
italy_matches <- subset(data, hometeamname == "Italy" | awayteamname == "Italy")

# Add columns for:
# - Italy's goals
# - Opponent's goals
# - Aggregate score (Italy's goals - opponent's goals)
# - Point color (green for win, grey for draw, red for loss)
italy_matches <- italy_matches %>%
  mutate(
    italy_goals = ifelse(hometeamname == "Italy", scorehome, scoreaway),  # Goals scored by Italy
    opponent_goals_italy = ifelse(hometeamname == "Italy", scoreaway, scorehome), # Goals scored by the opponent
    aggregate_score_italy = italy_goals - opponent_goals_italy,  # Goal difference (aggregate score)
    point_color_italy = case_when(
      aggregate_score_italy > 0 ~ "green",  # Green for wins
      aggregate_score_italy == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Convert 'dateandtimecet' to a Date object for Italy's matches
italy_matches <- italy_matches %>%
  mutate(match_date_italy = as.Date(dateandtimecet))

# Aggregate Italy's goal difference (aggregate score) by match date
italy_aggregate_scores <- italy_matches %>%
  group_by(match_date_italy) %>%  # Group data by match date
  summarise(
    aggregate_score_italy = sum(aggregate_score_italy),  # Sum of aggregate scores per match date
    final_score_italy = paste(italy_goals, ":", opponent_goals_italy),  # Combine scores into "Italy's Goals:Opponent's Goals"
    point_color_italy = case_when(
      aggregate_score_italy > 0 ~ "green",  # Green for wins
      aggregate_score_italy == 0 ~ "grey",  # Grey for draws
      TRUE ~ "red"  # Red for losses
    )
  )

# Add a matchday number for Italy's matches (sequential numbering of matches)
italy_aggregate_scores <- italy_aggregate_scores %>%
  mutate(matchday_italy = seq_along(match_date_italy))

# Rename Italy's columns to match England's column names for consistency
italy_aggregate_scores <- italy_aggregate_scores %>%
  rename(
    match_date = match_date_italy,
    aggregate_score = aggregate_score_italy,
    final_score = final_score_italy,
    point_color = point_color_italy,
    matchday = matchday_italy
  )

# Add a new column to differentiate between England and Italy
england_aggregate_scores <- england_aggregate_scores %>%
  mutate(team = "England")

italy_aggregate_scores <- italy_aggregate_scores %>%
  mutate(team = "Italy")

# Combine both teams' data into a single dataset
combined_data <- rbind(england_aggregate_scores, italy_aggregate_scores)

# Add the new dashed line values (matchday and corresponding values)
dashed_line_values <- data.frame(
  matchday = 1:7,  # Matchday numbers
  dashed_score = c(1.5, 1.25, 1.7, 1.9, 0.75, 0.5, 0)  # The provided dashed line values
)

# Merge the dashed line values with the combined data
combined_data_with_dashed_line <- merge(combined_data, dashed_line_values, by = "matchday", all.x = TRUE)

# Create a time-series plot with matchday numbers on the x-axis
ggplot(combined_data_with_dashed_line, aes(x = matchday, y = aggregate_score, color = team)) + 
  geom_line(size = 1) +  # Plot lines for England and Italy
  geom_point(size = 3) +  # Add points for each matchday
  geom_text(aes(label = final_score), vjust = -0.5, hjust = 0.5, color = "black") +  # Label points with the final score
  scale_color_manual(values = c("England" = "blue", "Italy" = "red")) +  # Blue for England, red for Italy
  geom_line(aes(y = dashed_score, color = "Average Aggregate Score"), linetype = "dashed", size = 1) +  # Add a dashed line
  scale_color_manual(values = c("England" = "blue", "Italy" = "red", "Average Aggregate Score" = "purple")) + # Purple for dashed line
  ggtitle("England and Italy's Aggregate Score per Matchday with Dashed Line") +  # Plot title
  xlab("Matchday") +  # Label for x-axis
  ylab("Aggregate Score ") +  # Label for y-axis
  theme_minimal() +  # Use a clean minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "right"  # Place the legend on the right
  )


# Merge the dashed line values with the combined data
combined_data_with_dashed_line <- merge(combined_data, dashed_line_values, by = "matchday", all.x = TRUE)

# Create an enhanced time-series plot
ggplot(combined_data_with_dashed_line, aes(x = matchday, y = aggregate_score, group = team, color = team)) + 
  geom_line(size = 1.5, alpha = 0.8) +  # Smooth and prominent team lines
  geom_point(aes(shape = team), size = 4, alpha = 0.8) +  # Points for each matchday
  geom_text(aes(label = final_score), vjust = -1, hjust = 0.5, color = "black", size = 3) +  # Annotate with final scores
  geom_line(aes(y = dashed_score, color = "Average Aggregate Score"), linetype = "dashed", size = 1.2) +  # Dashed reference line
  scale_color_manual(
    values = c("England" = "#1f78b4", "Italy" = "#e31a1c", "Average Aggregate Score" = "#6a3d9a")
  ) +  # Colors for lines
  scale_shape_manual(values = c("England" = 16, "Italy" = 17)) +  # Different shapes for teams
  ggtitle("England and Italy's Aggregate Scores Across Matchdays") +  # Informative title
  labs(
    x = "Matchday", 
    y = "Aggregate Score", 
    caption = "Dashed line represents Average Aggregate Score trend"
  ) +  # Axis labels and caption
  theme_minimal(base_size = 14) +  # Clean and modern theme with increased base text size
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),  # Centered and bold title
    axis.title.x = element_text(face = "italic", size = 14),  # Italic x-axis label
    axis.title.y = element_text(face = "italic", size = 14),  # Italic y-axis label
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Rotated x-axis labels
    axis.text.y = element_text(size = 12),  # Increased y-axis text size
    legend.title = element_blank(),  # Remove legend title
    legend.position = "top",  # Place legend at the top
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )



# Second Visualisation: Clean Sheets 

# To run this code, download and extract the file "Pre-Match Information.csv" and update the file path accordingly.


library(dplyr)
library(ggplot2)
library(viridis) # For colorblind-friendly palettes

# Load dataset
dataset <- read.csv("C:/Users/ethan/Downloads/cervus-uefa-euro-2020/Pre-match information.csv")

# Filter goalkeepers and select relevant columns (Jersey Name, Clean Sheet, Role, Nationality)
goalkeepers <- dataset %>%
  filter(Role == "Goalkeeper") %>%
  select(JerseyName, CleanSheet, Role, Country)

# Clean data by removing duplicates and keeping the maximum Clean Sheet value for each goalkeeper
goalkeepers_clean <- goalkeepers %>%
  group_by(JerseyName) %>%
  summarise(
    CleanSheet = max(CleanSheet), 
    Role = first(Role), 
    Nationality = first(Country), 
    .groups = "drop"
  )

# Identify the top 5 goalkeepers with the most clean sheets
top_5_goalkeepers <- goalkeepers_clean %>%
  arrange(desc(CleanSheet)) %>%
  slice_head(n = 5) %>%
  mutate(
    label = paste(JerseyName, " (", Nationality, ")", sep = "") # Add nationality to the label
  )

# Create a bar plot of the top 5 goalkeepers with the most clean sheets
ggplot(top_5_goalkeepers, aes(x = reorder(label, CleanSheet), y = CleanSheet, fill = CleanSheet)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(
    title = "Top 5 Goalkeepers with the Most Clean Sheets",
    subtitle = "Highlighting top-performing goalkeepers during UEFA Euro 2020",
    x = "Goalkeeper (Nationality)",
    y = "Clean Sheets",
    fill = "Clean Sheets"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) +
  scale_fill_viridis_c(option = "C", direction = -1) + # Colorblind-friendly palette
  coord_flip() +
  geom_text(aes(label = CleanSheet), hjust = -0.2, color = "black", size = 5) # Add data labels






# Visualisation Three: Injury Time 

# To run this code, download and extract the file "cleaned_match_info.csv" and update the file path accordingly.


library(ggplot2)
library(dplyr)
library(scales)

data <- read.csv("C:/Users/ethan/Downloads/cleaned_data/cleaned_match_info.csv")


# Filter the data for matches involving England
england_data <- data %>%
  filter(hometeamname == "England" | awayteamname == "England")  # Filter for matches where England is the home or away team

# Filter the data for matches involving Spain
spain_data <- data %>%
  filter(hometeamname == "Spain" | awayteamname == "Spain")  # Filter for matches where Spain is the home or away team

# Calculate the average injury time for each match week for England
england_avg_injurytime <- england_data %>%
  group_by(matchday) %>%  # Group data by match week (match day)
  summarize(
    avg_injurytime = mean(injurytime, na.rm = TRUE),  # Calculate the mean injury time, ignoring missing values
    team = "England"  # Add a column indicating the team
  )

# Calculate the average injury time for each match week for Spain
spain_avg_injurytime <- spain_data %>%
  group_by(matchday) %>%  # Group data by match week (match day)
  summarize(
    avg_injurytime = mean(injurytime, na.rm = TRUE),  # Calculate the mean injury time, ignoring missing values
    team = "Spain"  # Add a column indicating the team
  )

# Calculate the average injury time for each match week across all teams
all_teams_avg_injurytime <- data %>%
  group_by(matchday) %>%  # Group data by match week (match day)
  summarize(
    avg_injurytime = mean(injurytime, na.rm = TRUE),  # Calculate the mean injury time, ignoring missing values
    team = "All Teams"  # Add a column indicating that this is for all teams
  )

# Combine the data for England, Spain, and All Teams into one data set
combined_avg_injurytime <- bind_rows(england_avg_injurytime, spain_avg_injurytime, all_teams_avg_injurytime)





# Enhance the data preparation step
combined_avg_injurytime <- combined_avg_injurytime %>%
  mutate(
    id = row_number(),  # Add unique ID for ordering
    matchday_label = paste0("Week ", matchday),  # Create clear match week labels
    avg_injurytime_label = round(avg_injurytime, 1)  # Round average injury time for display in the legend
  )

# Calculate average injury time for each team
avg_injury_time_by_team <- combined_avg_injurytime %>%
  group_by(team) %>%
  summarise(avg_injurytime = mean(avg_injurytime, na.rm = TRUE))

# Create a vector for custom labels with the average injury time for each team
custom_legend_labels <- avg_injury_time_by_team %>%
  mutate(label = paste0(team, ": ", round(avg_injurytime, 1), " min")) %>%
  pull(label)

# Ensure team order corresponds correctly to colors
team_colors <- c("England" = "#1f77b4", "Spain" = "#ff7f0e", "All Teams" = "#2ca02c")

# Improved circular barplot with adjustments
ggplot(combined_avg_injurytime, aes(
  x = as.factor(id),
  y = avg_injurytime,
  fill = team  # Use team for the fill color
)) +
  geom_bar(stat = "identity", show.legend = TRUE, width = 1) +  # Adjust bar width for better spacing
  coord_polar(start = 0) +  # Convert to circular layout
  scale_fill_manual(
    values = team_colors,  # Professional colors for teams
    name = "Team",  # First legend for teams
    breaks = names(team_colors),  # Ensure the breaks match the team names
    labels = custom_legend_labels  # Custom legend labels with average injury time
  ) +
  labs(
    title = "Circular Barplot: Average Injury Time by Match Week",
    subtitle = "Comparing England, Spain, and All Teams",
    x = NULL,
    y = "Average Injury Time (minutes)"
  ) +
  geom_text(
    aes(label = ifelse(avg_injurytime > 0, round(avg_injurytime, 1), "")),  # Add value labels inside bars
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +  # Add clear y-axis breaks
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Improve title style
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),  # Add subtitle style
    axis.text.x = element_blank(),  # Remove unnecessary match week labels on the outside
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.text = element_blank(),  # Hide unnecessary axis text
    panel.grid = element_blank(),  # Remove grid for a clean look
    legend.position = "top",  # Position the legend at the top
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  ) 





# Visualisation Four: Tackles won percentage 

# To run this code, download and extract the file "england_with_cleaned_stats.csv" and update the file path accordingly.



library(ggplot2)


data <- read.csv("C:/Users/ethan/Downloads/england_with_cleaned_stats.csv")

# Calculate total tackles and percentage of tackles won
data$TotalTackles <- data$TacklesWon + data$TacklesLost
data$PctTacklesWon <- (data$TacklesWon / data$TotalTackles) * 100

# Remove rows where PctTacklesWon is NA or 0
data <- data[!is.na(data$PctTacklesWon) & data$PctTacklesWon > 0, ]

# Create a column for "TacklesWon / TotalTackles" format
data$TacklesFraction <- paste(data$TacklesWon, "/", data$TotalTackles, sep = "")

# Plot the data
ggplot(data, aes(x = reorder(OfficialSurname, -PctTacklesWon), y = PctTacklesWon, fill = PctTacklesWon)) +
  geom_bar(stat = "identity", width = 0.7) +  # Dynamically fill bars based on percentage
  geom_text(aes(label = TacklesFraction), vjust = -0.5, color = "black", size = 4.5, fontface = "bold") +  # Bold fraction labels
  scale_fill_gradient(low = "#ADD8E6", high = "#00008B", name = "Tackle %") +  # Gradient from light to dark blue
  theme_minimal(base_size = 14) +  # Use a clean, professional theme with larger base font size
  labs(
    title = "Player Tackling Performance: Percentage and Success Ratio",
    subtitle = "Comparison of percentage of tackles won and total tackle attempts for each player",
    x = "Player",
    y = "Percentage of Tackles Won (%)",
    caption = "Data Source: England Team Player Stats"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#2E4053"),  # Bold, centered title
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#2E4053"),  # Subtitle styling
    plot.caption = element_text(size = 10, face = "italic", hjust = 0, color = "gray50"),  # Caption for data source
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "#2E4053"),  # Professional styling for x-axis labels
    axis.text.y = element_text(size = 12, color = "#2E4053"),  # Professional styling for y-axis labels
    axis.title.x = element_text(face = "bold", size = 14, color = "#2E4053"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold", size = 14, color = "#2E4053"),  # Bold y-axis title
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),  # Subtle dashed gridlines for clarity
    panel.grid.minor = element_blank(),  # Remove minor gridlines for a clean look
    legend.position = "right"  # Place the legend on the right
  ) +
  scale_y_continuous(
    limits = c(0, max(data$PctTacklesWon) + 10),  # Add padding to the y-axis
    expand = c(0, 0),  # Remove extra space below bars
    breaks = seq(0, max(data$PctTacklesWon) + 10, by = 10)  # Consistent y-axis breaks
  )


