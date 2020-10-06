#--- Report plots

#--- Source required packages & functions

source("src/r/00_header.R")
source("src/r/00_plot_themes.R")
source("src/r/get_course_detail.R")
source("src/r/get_player_detail.R")
source("src/r/get_player_scorecards.R")
source("src/r/get_match_scoring.R")
source("src/r/get_scoring_by_team.R")
source("src/r/get_course_summary.R")
source("src/r/get_point_scoring_by_hole.R")
source("src/r/get_point_scoring_by_group.R")

#--- Manual player teams table 

player_teams = read.csv("Data/raw/wigs_2020/player_teams.csv") %>%
  dplyr::select(player, team_id)

#--- Parse data into useful format

scorecards_file_path = "Data/raw/wigs_2020/R3/Scorecards_20200908.xlsx"
player_list_file_path = "Data/raw/wigs_2020/R3/Alphabetical_List_20200908.xlsx"
pairings_list_file_path = "Data/raw/wigs_2020/R3/Pairings_and_Starting_Times_20200908.xlsx"


course_detail = get_course_detail(
  file_path = scorecards_file_path
)

player_detail = get_player_detail(
  player_file_path = player_list_file_path,
  pairings_file_path = pairings_list_file_path,
  player_teams = player_teams,
  match_format = "singles"
)

player_scorecards = get_player_scorecards(
  file_path = scorecards_file_path,
  course_detail = course_detail,
  player_detail = player_detail
)

match_scoring = get_match_scoring(
  player_scorecards = player_scorecards
)


#--- Scoring 

scoring_by_team = get_scoring_by_team(
  match_scoring = match_scoring,
  match_format = "singles"
)

scoring_by_team$team_name = factor(scoring_by_team$team_name, levels = rev(scoring_by_team$team_name[order(scoring_by_team$team_score_nett_to_par)]))

ggplot(scoring_by_team, aes(team_name, team_score_nett_to_par, fill = team_id))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("royalblue", "white"))+
  labs(y = "Team Score (to par)", x = "")+
  coord_flip()+
  scale_y_reverse()+
  theme_wigs_night()+
  theme(legend.position = "none")

#--- Birdies 

scoring_by_team$team_name = factor(scoring_by_team$team_name, levels = scoring_by_team$team_name[order(scoring_by_team$bob_nett)])

ggplot(scoring_by_team, aes(team_name, bob_nett, fill = team_id))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("royalblue", "white"))+
  labs(y = "Birdies or Better", x = "")+
  coord_flip()+
  scale_y_continuous(breaks = seq(0, 10, 2))+
  theme_wigs_night()+
  theme(legend.position = "none")

#--- Course Summary 

course_summary = get_course_summary(
  match_scoring = match_scoring
)

ggplot(course_summary, aes(as.factor(hole_no), avg_nett_score, colour = avg_nett_score))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white")+
  geom_point(size = 10)+
  scale_colour_gradient2(low = "#37b36b", high = "firebrick3", mid = "gold", midpoint = mean(course_summary$avg_nett_score))+
  labs(x = "Hole No.", y = "Avg. Nett Score (to par)")+
  theme_wigs_night()+
  theme(legend.position = "none")


#--- Holes Won 

point_scoring_by_hole = get_point_scoring_by_hole(
  match_scoring = match_scoring
)

point_scoring_by_group = get_point_scoring_by_group(
  match_scoring = match_scoring
)

ggplot(point_scoring_by_hole, aes(x = as.factor(hole_no), y = team_points, fill = team_id))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("royalblue", "white"))+
  labs(x = "Hole No.", y = "Holes Won")+
  theme_wigs_night()+
  theme(legend.position = "none")

ggplot(point_scoring_by_hole, aes(x = as.factor(hole_no), y = team_points, colour = team_id, group = team_id))+
  geom_point(stat = "identity", size = 5)+
  geom_line(linetype = "dashed")+
  scale_colour_manual(values = c("royalblue", "white"))+
  labs(x = "Hole No.", y = "Holes Won")+
  theme_wigs_night()+
  theme(legend.position = "none")

ggplot(point_scoring_by_hole, aes(x = as.factor(hole_no), y = cumsum_team_points, colour = team_id, group = team_id))+
  geom_point(stat = "identity", size = 5)+
  geom_line(linetype = "dashed")+
  scale_colour_manual(values = c("royalblue", "white"))+
  labs(x = "Hole No.", y = "Holes Won")+
  theme_wigs_night()+
  theme(legend.position = "none")

ggplot(point_scoring_by_group, aes(x = as.factor(hole_no), y = match_score_plot, fill = lead_team))+
  facet_wrap(~group_id, nrow = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white")+
  geom_bar(stat = "identity", alpha = 0.9)+
  scale_fill_manual(values = c("royalblue", "white"))+
  scale_y_continuous(breaks = seq(-10, 10, 2), labels = abs(seq(-10, 10, 2)))+
  labs(x = "Hole No.", y = "Lead (Holes)")+
  theme_wigs_night()+
  theme(legend.position = "none")


