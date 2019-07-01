# Finding the Best Shot in the NHL

# Load Packages
library("tidyverse")
library("ggthemes")

# Import Data

nhl_data <- read.csv("2017-18 Data.csv", stringsAsFactors = FALSE, fileEncoding ="UTF-8-BOM")

# Explore Data 

# Goals
nhl_goals <- select(nhl_data,"G.Bkhd", "G.Dflct", "G.Slap", "G.Snap", "G.Tip", "G.Wrap", "G.Wrst")

total_goals <- sum(nhl_goals)

backhand_goals <- sum(nhl_goals$G.Bkhd)
deflection_goals <- sum(nhl_goals$G.Dflct)
slap_goals <- sum(nhl_goals$G.Slap)
snap_goals <- sum(nhl_goals$G.Snap)
tip_goals <- sum(nhl_goals$G.Tip)
wrap_goals <- sum(nhl_goals$G.Wrap)
wrist_goals <- sum(nhl_goals$G.Wrst)

sum_goals <- data.frame(Number_of_Goals = c(backhand_goals, deflection_goals, slap_goals, snap_goals, tip_goals, wrap_goals, wrist_goals),
          Shot_Type = c("Backhand Goals", "Deflection Goals", "Slap Shot Goals", "Snap Shot Goals", "Tip in Goals", "Warp Around Goals", "Wrist Shot Goals"))

sum_goals$freq <- as.numeric(sum_goals[,1]) / total_goals

ggplot(sum_goals, aes(x = reorder(Shot_Type,-freq), y = freq)) +
      geom_bar(stat = "identity", fill = "skyblue4") +
      labs(title = "Frequency of Goals Based off Shot Type", subtitle = "2017-18 NHL Season", caption = "7,449 Goals Scored") +
      xlab("Type of Shot") +
      ylab("Frequency of Goals") +
      theme_wsj() + 
      scale_y_continuous(labels=scales::percent)

# Shots On Goals

nhl_shots <- select(nhl_data, "S.Bkhd", "S.Dflct", "S.Slap", "S.Snap", "S.Tip", "S.Wrap", "S.Wrst")

backhand_shots <- sum(nhl_shots$S.Bkhd)
deflection_shots <- sum(nhl_shots$S.Dflct)
slap_shots <- sum(nhl_shots$S.Slap)
snap_shots <- sum(nhl_shots$S.Snap)
tip_shots <- sum(nhl_shots$S.Tip)
wrap_shots <- sum(nhl_shots$S.Wrap)
wrist_shots <- sum(nhl_shots$S.Wrst)


sum_shots <- data.frame(Number_of_Shots = c(backhand_shots, deflection_shots, slap_shots, snap_shots, tip_shots, wrap_shots, wrist_shots),
                        Shot_Type = c("Backhand Shots", "Deflection Shots", "Slap Shot Shots", "Snap Shot Shots", "Tip in Shots", "Warp Around Shots", "Wrist Shot Shots"))

total_shots <- sum(nhl_shots)

# League Shooting %

total_goals / total_shots

# 9.17 %
# Shot Type Chart

sum_shots$freq <- as.numeric(sum_shots[,1]) / total_shots

ggplot(sum_shots, aes(x = reorder(Shot_Type,-freq), y = freq)) +
  geom_bar(stat = "identity", fill = "skyblue4") +
  labs(title = "Frequency of Shots Based off Shot Type", subtitle = "2017-18 NHL Season", caption = "81,228 Shots on Goal") +
  xlab("Type of Shot") +
  ylab("Frequency of Shots") +
  theme_wsj() +
  scale_y_continuous(labels=scales::percent)

# Most Effective Shot

sum_goals$ShootingPer <- as.numeric(sum_goals[,1]) / as.numeric(sum_shots[,1])

ggplot(sum_goals, aes(x = reorder(Shot_Type,-ShootingPer), y = ShootingPer)) +
  geom_bar(stat = "identity", fill = "skyblue4") +
  labs(title = "Most Effective Shot", subtitle = "2017-18 NHL Season") +
  xlab("Type of Shot") +
  ylab("Shooting Percentage") +
  theme_wsj() +
  scale_y_continuous(labels=scales::percent)


    
