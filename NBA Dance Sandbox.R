install.packages("hoopR")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("pbapply")
library(pbapply)
library(dplyr)
library(hoopR)
library(tidyverse)

#load box scores
box2024<-load_nba_player_box(seasons=most_recent_nba_season())
#remove All Star Game
box2024<-box2024 %>% 
  filter(team_name!="All-Stars")
#add UD scoring
box2024<-box2024 %>% 
  mutate(UD_pts=(points)+(assists*1.5)+(rebounds*1.2)+(steals*3)+(blocks*3)-turnovers)

#create averages of game logs by player by team
avg_box2024<-box2024 %>% 
  filter(minutes>0) %>% 
  group_by(team_name,athlete_display_name) %>% 
  summarize(UDfptspg=mean(UD_pts),
            games_played=n(),
            minutespg=mean(minutes),
            pointspg=mean(points),
            assistspg=mean(assists),
            reboundspg=mean(rebounds),
            stockspg=mean(steals+blocks),
            turnoverspg=mean(turnovers))

# Calculate the total number of games played by each team
team_games_count <- box2024 %>%
  distinct(game_id, team_name) %>%
  count(team_name)

# Rank players within each game by UD_pts and count the ranks
rank_counts <- box2024 %>%
  group_by(game_id, team_name) %>%
  mutate(rank_ud_pts = rank(-UD_pts, ties.method = "min")) %>%
  filter(rank_ud_pts <= 5) %>%
  ungroup() %>%
  count(athlete_display_name, team_name, rank_ud_pts)

# Merge the total team games count with the player rank counts
# Ensure the column `n` exists in `team_games_count` for this to work
rank_counts_with_total <- rank_counts %>%
  left_join(team_games_count, by= c("team_name")) %>%
  rename(total_team_games = n.y,
         games_at_thresh=n.x)

# Calculate the percentages for each rank based on the total team games
rank_percentages <- rank_counts_with_total %>%
  mutate(percentage = games_at_thresh / total_team_games * 100)

# Pivot the data to have separate columns for each rank's percentage
final_df <- rank_percentages %>%
  pivot_wider(id_cols = c(athlete_display_name, team_name),
              names_from = rank_ud_pts, 
              values_from = percentage,
              names_prefix = "rank_",
              values_fill = list(percentage = 0)) %>%
  group_by(athlete_display_name, team_name) %>%
  summarise(across(starts_with("rank_"), sum))

final_df<-final_df %>% 
  select(athlete_display_name,team_name,rank_1,rank_2,rank_3,rank_4,rank_5)



write.csv(avg_box2024,"player_box_average_by_team.csv")
write.csv(box2024,"all_player_box_scores.csv")
write.csv(final_df,"top5_breakout.csv")
            


              