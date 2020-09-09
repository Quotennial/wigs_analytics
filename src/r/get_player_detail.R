
get_player_detail = function(player_file_path, pairings_file_path){
  
  #--- Read in raw data
  
  player_list = read_excel(player_file_path) %>%
    drop_na()
  
  pairings = read_excel(pairings_file_path, col_names = FALSE)
  
  #--- Create player detail table 
  
  player_teams = pairings[grepl("Tee", pairings$...1) == FALSE & grepl(" ", pairings$...1), ] %>%
    cbind(
      team_id = rep(rep(c("White", "Blue"), each = 2), length(unique(player_list$`Tee Time`)))
    ) %>%
    dplyr::rename(player = ...1, exact_hcp = ...2, playing_hcp = ...3)
  
  player_detail = player_list %>%
    dplyr::select(player = Player, tee_time = `Tee Time`) %>%
    arrange(tee_time) %>%
    dplyr::mutate(
      group_id = cumsum(!duplicated(tee_time))
    ) %>%
    group_by(player) %>%
    dplyr::mutate(
      player = parse_player_names(x = player)
    ) %>%
    ungroup() %>%
    left_join(
      player_teams, 
      by = 'player'
    )
  
  return(player_detail)
  
}
