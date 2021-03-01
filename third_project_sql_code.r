###################################################################################################################
###################################################################################################################

# Installing the necessary libraries

install.packages("tidyverse")
install.packages("DBI")
install.packages("RSQLite")

# Loading libraries

library(tidyverse)
library(DBI)

# Question 1


# Connecting to database

soccer=dbConnect(RSQLite::SQLite(), "C:/Users/Alex/R/database.sqlite")


# Question 2


# Reading Tables

dbReadTable (soccer,"Country")
dbReadTable (soccer,"League")
dbReadTable (soccer,"Match")
dbReadTable (soccer,"Player")
dbReadTable (soccer,"Player_Attributes")
dbReadTable (soccer,"Team")
dbReadTable (soccer,"Team_Attributes")
dbReadTable (soccer,"sqlite_sequence")

# Checking their structure

str(dbReadTable (soccer,"Country"))
str(dbReadTable (soccer,"League"))
str(dbReadTable (soccer,"Match"))
str(dbReadTable (soccer,"Player"))
str(dbReadTable (soccer,"Player_Attributes"))
str(dbReadTable (soccer,"Team"))
str(dbReadTable (soccer,"Team_Attributes"))
str(dbReadTable (soccer,"sqlite_sequence"))


# Question 3


# Creating pointers

country=tbl(soccer,("Country"))
league=tbl(soccer,("League"))
match=tbl(soccer,("Match"))
player=tbl(soccer,("Player"))
player_attributes=tbl(soccer,("Player_Attributes"))
team=tbl(soccer,("Team"))
team_attributes=tbl(soccer,("Team_Attributes"))

# Checking their dimensions

dim(country)
dim(league)
dim(match)
dim(player)
dim(player_attributes)
dim(team)
dim(team_attributes)


# Question 4


question_2a=player %>%
  summarize(player_name,height,weight) %>%
  filter(height>160,height<180) %>%
  arrange(-weight)
question_2b=match %>%
  filter(match_api_id==659039) %>%
  inner_join(team, by=c("home_team_api_id"="team_api_id")) %>%
  summarize(date,home_team_name=team_long_name,home_team_goal)
question_2c=team %>%
  inner_join(match, by=c("team_api_id"="home_team_api_id")) %>%
  inner_join(country, by=c("country_id"="id")) %>%
  summarize(team_long_name,name) %>%
  distinct()


# Question 5


# 5a)

left_right=player_attributes %>%
  summarize(preferred_foot) %>%
  group_by(preferred_foot)
tally(left_right)

# 5b)

ggplot(left_right, aes(x=preferred_foot))+geom_bar()


# Question 6


# 6a)

player %>%
  summarize(height,weight) %>%
  ggplot(aes(x=height, y=weight)) +
  geom_point()

# 6b)

player %>%
  summarize(height,weight) %>%
  ggplot(aes(x=height, y=weight)) +
  geom_jitter(alpha=0.2)


# Question 7


match_points <- match %>%
  mutate(home_team_points = if_else((home_team_goal > away_team_goal), 3, if_else((home_team_goal == away_team_goal), 1, 0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal), 0, if_else((home_team_goal == away_team_goal), 1, 3)))

# 7a)

home_points= match_points %>%
  group_by(league_id) %>% 
  inner_join(team, by=c("home_team_api_id"="team_api_id")) %>%
  summarize(team_api_id=home_team_api_id, mean_home_team_points=mean(home_team_points))

# 7b)

away_points= match_points %>%
  group_by(league_id) %>%
  inner_join(team, by=c("away_team_api_id"="team_api_id")) %>%
  summarize(team_api_id=away_team_api_id,mean_away_team_points=mean(away_team_points))

# 7c)

home_away_points=left_join(home_points,away_points, by = c("league_id", "league_id"))

# 7d)

ggplot(
  home_away_points,
  (aes(x=mean_home_team_points, y=mean_away_team_points))) + 
  geom_point() +
  geom_smooth(method=lm)

dbDisconnect(soccer)
  
  
###################################################################################################################
###################################################################################################################