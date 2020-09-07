#--- Report plots

#--- Source required packages & data tables

source("src/r/00_header.R")
source("src/r/00_plot_themes.R")
source("src/r/course_detail.R")
source("src/r/player_detail.R")
source("src/r/player_scorecards.R")
source("src/r/match_scoring.R")

write.csv(course_detail, "src/r/tables/course_detail.csv")
write.csv(player_detail, "src/r/tables/player_detail.csv")
write.csv(player_scorecards, "src/r/tables/player_scorecards.csv")
write.csv(match_scoring, "src/r/tables/match_scoring.csv")

#--- Scoring 

scoring_by_team = match_scoring %>%
  drop_na() %>%
  group_by(group_id, team_id, player) %>%
  dplyr::mutate(
    player_last_name = strsplit(player, " ")[[1]][2]
  ) %>%
  group_by(group_id, team_id) %>%
  dplyr::mutate(
    team_name = paste(player_last_name[1], player_last_name[2], sep = "/")
  ) %>%
  group_by(team_name, team_id, hole_no, hole_par) %>%
  dplyr::summarise(
    team_hole_score_nett = min(team_hole_score_nett)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    team_hole_score_nett_to_par = team_hole_score_nett-hole_par
  ) %>%
  group_by(team_name, team_id) %>%
  dplyr::summarise(
    holes_played = n(),
    bob_nett = sum(team_hole_score_nett_to_par <= -1),
    bow_nett = sum(team_hole_score_nett_to_par >= 1),
    team_score_nett = sum(team_hole_score_nett),
    team_score_nett_to_par = sum(team_hole_score_nett_to_par)
  ) 

contribution_by_team = match_scoring %>%
  drop_na() %>%
  dplyr::mutate(
    hole_score_to_par = hole_score-hole_par,
    hole_score_nett_to_par = hole_score_nett-hole_par,
    contribution = case_when(
      hole_score_nett == team_hole_score_nett ~ 1,
      hole_score_nett != team_hole_score_nett ~ 0
    )
  ) %>%
  group_by(group_id, team_id, player) %>%
  dplyr::mutate(
    player_last_name = strsplit(player, " ")[[1]][2]
  ) %>%
  group_by(group_id, team_id) %>%
  dplyr::mutate(
    team_name = paste(player_last_name[1], player_last_name[2], sep = "/")
  ) %>%
  group_by(team_name, team_id, player) %>%
  dplyr::summarise(
    holes_played = n(),
    holes_contributed = sum(contribution),
    bob_nett = sum(hole_score_nett_to_par <= -1),
    bow_nett = sum(hole_score_nett_to_par >= 1),
    score = sum(hole_score),
    score_to_par = sum(hole_score_to_par),
    score_nett = sum(hole_score_nett),
    score_nett_to_par = sum(hole_score_nett_to_par)
  ) %>%
  group_by(team_name, team_id) %>%
  dplyr::mutate(
    total_holes = sum(holes_contributed)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    holes_contributed_perc = holes_contributed/total_holes
  ) %>%
  group_by(team_name, team_id) %>%
  arrange(holes_contributed_perc) %>%
  dplyr::mutate(
    ymin = c(0, min(holes_contributed_perc)),
    ymax = c(min(holes_contributed_perc), 1),
    min_max = c("min", "max")
  ) %>%
  ungroup()

scoring_by_team$team_name = factor(scoring_by_team$team_name, levels = rev(scoring_by_team$team_name[order(scoring_by_team$team_score_nett_to_par)]))

ggplot(scoring_by_team, aes(team_name, team_score_nett_to_par, fill = team_id))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("royalblue", "white"))+
  labs(y = "Team Score (to par)", x = "")+
  coord_flip()+
  scale_y_reverse()+
  theme_wigs_night()+
  theme(legend.position = "none")

contribution_by_team$player = factor(contribution_by_team$player, levels = rev(contribution_by_team$player[order(contribution_by_team$score_nett_to_par)]))

ggplot(contribution_by_team, aes(player, score_nett_to_par, fill = team_id))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white")+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("royalblue", "white"))+
  labs(y = "Score (to par)", x = "")+
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

contribution_by_team$player = factor(contribution_by_team$player, levels = contribution_by_team$player[order(contribution_by_team$bob_nett)])

ggplot(contribution_by_team, aes(player, bob_nett, fill = team_id))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("royalblue", "white"))+
  labs(y = "Birdies or Better", x = "")+
  coord_flip()+
  theme_wigs_night()+
  theme(legend.position = "none")


#--- Player Contribution 

contribution_by_team$team_name = factor(contribution_by_team$team_name, levels = unique(contribution_by_team$team_name[order(contribution_by_team$team_id)]))

ggplot(contribution_by_team, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=min_max)) +
  facet_wrap(~team_name, nrow = 2)+
  geom_rect() +
  scale_fill_manual(values = c("#37b36b", "firebrick3"))+
  scale_colour_manual(values = c("#37b36b", "firebrick3"))+
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_wigs_night()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(2)))


#--- Course Summary 

course_summary = match_scoring %>%
  drop_na() %>%
  dplyr::mutate(
    hole_score_to_par = hole_score-hole_par,
    hole_score_nett_to_par = hole_score_nett-hole_par
  ) %>%
  group_by(hole_no, hole_par) %>%
  dplyr::summarise(
    avg_nett_score = mean(hole_score_nett_to_par, na.rm = T),
    bob = mean(hole_score_nett_to_par <= -1),
    bow = mean(hole_score_nett_to_par >= 1)
  ) %>%
  ungroup()

ggplot(course_summary, aes(as.factor(hole_no), avg_nett_score, colour = avg_nett_score))+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white")+
  geom_point(size = 10)+
  scale_colour_gradient2(low = "#37b36b", high = "firebrick3", mid = "gold", midpoint = mean(course_summary$avg_nett_score))+
  labs(x = "Hole No.", y = "Avg. Nett Score (to par)")+
  theme_wigs_night()+
  theme(legend.position = "none")


#--- Holes Won 

point_scoring_by_hole = match_scoring %>%
  group_by(hole_no, group_id, team_id) %>%
  dplyr::summarise(
    hole_points = case_when(
      max(hole_won) == TRUE ~ 1,
      max(tie) == TRUE ~ 0,
      max(hole_won) == FALSE ~ 0,
    )
  ) %>%
  group_by(hole_no, team_id) %>%
  dplyr::summarise(
    team_points = sum(hole_points)
  ) %>%
  group_by(team_id) %>%
  arrange(hole_no) %>%
  dplyr::mutate(
    cumsum_team_points = cumsum(team_points)
  )

point_scoring_by_group = match_scoring %>%
  group_by(hole_no, group_id, team_id) %>%
  dplyr::summarise(
    hole_points = case_when(
      max(hole_won) == TRUE ~ 1,
      max(tie) == TRUE ~ 0,
      max(hole_won) == FALSE ~ 0,
    )
  ) %>%
  group_by(group_id, team_id) %>%
  arrange(hole_no) %>%
  dplyr::mutate(
    cumsum_holes = cumsum(hole_points)
  ) %>%
  group_by(group_id, hole_no) %>%
  dplyr::summarise(
    match_score = max(cumsum_holes)-min(cumsum_holes),
    lead_team = ifelse(
      match_score == 0,
      NA,
      team_id[cumsum_holes == max(cumsum_holes)]
    )
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    group_id = paste0("Match ", group_id),
    match_score_plot = case_when(
      lead_team == "Blue" ~ match_score*-1, 
      lead_team == "White" ~ match_score
    ),
    holes_remaining = 18-hole_no,
    match_won = case_when(
      match_score > holes_remaining ~ TRUE,
      match_score <= holes_remaining ~ FALSE,
    )
  ) %>%
  group_by(group_id) %>%
  arrange(hole_no) %>%
  dplyr::mutate(
    match_won = lag(match_won)
  ) %>%
  ungroup() %>%
  dplyr::filter(match_won == FALSE)

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
  facet_wrap(~group_id)+
  geom_hline(yintercept = 0, linetype = "dashed", colour = "white")+
  geom_bar(stat = "identity", alpha = 0.9)+
  scale_fill_manual(values = c("royalblue", "white"))+
  scale_y_continuous(breaks = seq(-10, 10, 2), labels = abs(seq(-10, 10, 2)))+
  labs(x = "Hole No.", y = "Holes Won")+
  theme_wigs_night()+
  theme(legend.position = "none")


