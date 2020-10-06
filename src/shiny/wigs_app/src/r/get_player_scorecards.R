
get_player_scorecards = function(file_path, course_detail, player_detail){
  
  #--- Read in raw data
  
  team_scorecards = read_excel(file_path, col_names = FALSE)
  
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
    ) %>%
    distinct()
  
  return(player_scorecards)
  
}
