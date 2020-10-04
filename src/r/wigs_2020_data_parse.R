
#--- Source required packages & functions

source("src/r/00_header.R")
source("src/r/00_plot_themes.R")
source("src/r/get_course_detail.R")
source("src/r/get_player_detail.R")
source("src/r/get_player_scorecards.R")
source("src/r/get_match_scoring.R")
source("src/r/get_scoring_by_team.R")
source("src/r/get_contribution_by_team.R")
source("src/r/get_course_summary.R")
source("src/r/get_point_scoring_by_hole.R")
source("src/r/get_point_scoring_by_group.R")

#--- Inputs 

json <- fromJSON(file = "Data/raw/wigs_2020/wigs_2020.json")

file_name = json$filename

course_detail = list()
player_detail = list()
player_scorecards = list()
match_scoring = list()
scoring_by_team = list()
contribution_by_team = list() 
course_summary = list()
point_scoring_by_hole = list()
point_scoring_by_group = list()

#--- Loop to read & parse data files

for(i in 1:length(json)){
  
  round_no_desc = json$rounds[[i]]$folder
  gb_code = json$rounds[[i]]$gamebookcode
  match_format = json$rounds[[i]]$match_format
  
  #--- Manual player teams table 
  
  player_teams = read.csv(paste0("Data/raw/", file_name, "/player_teams.csv")) %>%
    dplyr::select(player, team_id)
  
  #--- Parse data into useful format
  
  scorecards_file_path = paste0("Data/raw/", file_name, "/", round_no_desc, "/Scorecards_", gb_code, ".xlsx")
  player_list_file_path = paste0("Data/raw/", file_name, "/", round_no_desc, "/Alphabetical_List_", gb_code, ".xlsx")
  pairings_list_file_path = paste0("Data/raw/", file_name, "/", round_no_desc, "/Pairings_and_Starting_Times_", gb_code, ".xlsx")
  
  course_detail[[i]] = get_course_detail(
    file_path = scorecards_file_path
  ) %>%
    dplyr::mutate(
      round = round_no_desc
    )
  
  player_detail[[i]] = get_player_detail(
    player_file_path = player_list_file_path,
    pairings_file_path = pairings_list_file_path,
    player_teams = player_teams,
    match_format = match_format
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format 
    )
  
  player_scorecards[[i]] = get_player_scorecards(
    file_path = scorecards_file_path,
    course_detail = course_detail[[i]],
    player_detail = player_detail[[i]]
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format
    )
  
  match_scoring[[i]] = get_match_scoring(
    player_scorecards = player_scorecards[[i]],
    match_format = match_format
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format
    )
  
  scoring_by_team[[i]] = get_scoring_by_team(
    match_scoring = match_scoring[[i]],
    match_format = match_format
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format
    )
  
  contribution_by_team[[i]] = get_contribution_by_team(
    match_scoring = match_scoring[[i]],
    match_format = match_format
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format
    )
  
  course_summary[[i]] = get_course_summary(
    match_scoring = match_scoring[[i]]
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format
    )
  
  point_scoring_by_hole[[i]] = get_point_scoring_by_hole(
    match_scoring = match_scoring[[i]]
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format
    )
  
  point_scoring_by_group[[i]] = get_point_scoring_by_group(
    match_scoring = match_scoring[[i]]
  ) %>%
    dplyr::mutate(
      round = round_no_desc,
      match_format
    )
  
}

#--- Joining all rounds 

all_course_detail = rbindlist(course_detail, use.names=TRUE)
all_player_detail = rbindlist(player_detail, use.names=TRUE)
all_player_scorecards = rbindlist(player_scorecards, use.names=TRUE)
all_match_scoring = rbindlist(match_scoring, use.names=TRUE)
all_scoring_by_team = rbindlist(scoring_by_team, use.names=TRUE)
all_contribution_by_team = rbindlist(contribution_by_team, use.names=TRUE)
all_course_summary = rbindlist(course_summary, use.names=TRUE)
all_point_scoring_by_hole = rbindlist(point_scoring_by_hole, use.names=TRUE)
all_point_scoring_by_group = rbindlist(point_scoring_by_group, use.names=TRUE)

all_final_match_scores = all_match_scoring %>%
  group_by(team_name, group_id, team_id, round, match_format, hole_no) %>%
  dplyr::summarise(
    hole_points = case_when(
      max(hole_won) == TRUE ~ 1,
      max(hole_won) == FALSE & max(tie) == TRUE ~ 0,
      max(hole_won) == FALSE & max(tie) == FALSE ~ -1
    )
  ) %>%
  group_by(team_name, group_id, team_id, round, match_format) %>%
  arrange(hole_no) %>%
  dplyr::mutate(
    match_points = cumsum(hole_points)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    remaining_holes = 18-hole_no,
    match_end = ifelse(
      abs(match_points) > remaining_holes | (hole_no == 18 & match_points == 0),
      TRUE,
      FALSE
    )
  ) %>%
  group_by(team_name, group_id, team_id, round, match_format) %>%
  dplyr::mutate(
    first_match_end = max(remaining_holes[match_end == TRUE])
  ) %>%
  ungroup() %>%
  dplyr::filter(first_match_end == remaining_holes) %>%
  dplyr::mutate(
    match_result = case_when(
      match_points > 0 ~ "Won",
      match_points < 0 ~ "Loss",
      match_points == 0 ~ "Half"
    ),
    match_score = case_when(
      match_result == "Half" ~ "A/S",
      first_match_end == 0 ~ paste0(abs(match_points), " Up"),
      first_match_end > 0 ~ paste0(abs(match_points), " & ", first_match_end)
    )
  ) %>%
  dplyr::select(team_name, group_id, team_id, round, match_format, match_result, match_score)


all_contribution_by_player = all_contribution_by_team %>%
  left_join(
    all_final_match_scores
  ) %>%
  dplyr::mutate(
    match_points = case_when(
      match_result == "Won" ~ 1,
      match_result == "Half" ~ 0.5,
      match_result == "Loss" ~ 0
    ),
    match_contribution = holes_contributed_perc*match_points
  ) %>%
  group_by(round, group_id) %>%
  dplyr::mutate(
    match_format = ifelse(
      length(unique(player)) == 4, 
      "Pairs",
      "Singles"
    )
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    expected_contribution = ifelse(
      match_format == "Pairs",
      0.25,
      0.5
    )
  ) %>%
  group_by(player) %>%
  dplyr::summarise(
    matches_played = n(),
    expected_contribution = sum(expected_contribution),
    all_contribution = sum(match_contribution)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    matches_played = ifelse(
      player == "Benjamin Robinson",
      3, 
      matches_played
    ),
    expected_contribution = case_when(
      player == "Benjamin Robinson" ~ expected_contribution+0.25, 
      player == "Luke Carter" ~ expected_contribution-0.25, 
      !(player %in% c("Benjamin Robinson", "Luke Carter")) ~ expected_contribution
    ),
    all_contribution = case_when(
      player == "Benjamin Robinson" ~ all_contribution+0.5, 
      player == "Luke Carter" ~ all_contribution-0.5, 
      !(player %in% c("Benjamin Robinson", "Luke Carter")) ~ all_contribution
    ),
    relative_contribution = all_contribution-expected_contribution
  )
