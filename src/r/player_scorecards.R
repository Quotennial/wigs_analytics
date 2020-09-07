
#--- Read in data

source("src/r/course_detail.R")
source("src/r/player_detail.R")

#--- Create player scorecards table 

players = team_scorecards$...1[grepl(",", team_scorecards$...1)]

player_scorecards = team_scorecards[grepl(",", team_scorecards$...1), ] %>%
  melt(id.vars = "...1", value.name = "hole_score") %>%
  dplyr::select(player = ...1, hole_score) %>%
  cbind(
    hole_no = rep(as.character(team_scorecards[1, ])[as.character(team_scorecards[1, ]) != "HOLES"], each = length(players))
  ) %>%
  dplyr::mutate(
    hole_no = as.numeric(hole_no)
  ) %>%
  dplyr::filter(hole_no %in% 1:18) %>%
  left_join(
    course_detail
  ) %>%
  group_by(player) %>%
  dplyr::mutate(
    player = parse_player_names(x = player)
  ) %>%
  ungroup() %>%
  left_join(
    player_detail
  )