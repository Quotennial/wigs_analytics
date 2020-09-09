
get_player_detail = function(player_file_path, pairings_file_path, player_teams, match_format){
  
  #--- Read in raw data
  
  player_list = read_excel(player_file_path) %>%
    drop_na()
  
  pairings = read_excel(pairings_file_path, col_names = FALSE)
  
  #--- Create player detail table 
  
  if(match_format == "pairs"){
  
  player_teams = pairings[grepl("Tee", pairings$...1) == FALSE & grepl(" ", pairings$...1), ] %>%
    dplyr::rename(player = ...1, exact_hcp = ...2, playing_hcp = ...3) %>%
    left_join(
      player_teams, 
      by = 'player'
    )
    
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
    
  }else{
    
    player_teams = pairings[grepl("Tee", pairings$...1) == FALSE & grepl(" ", pairings$...1), ] %>%
      dplyr::rename(player = ...1, exact_hcp = ...2, playing_hcp = ...3) %>%
      dplyr::mutate(
        group_id = rep(1:(length(player_teams$player)/2), each = 2)
      ) %>%
      left_join(
        player_teams, 
        by = 'player'
      )
    
    player_detail = player_list %>%
      dplyr::select(player = Player, tee_time = `Tee Time`) %>%
      group_by(player) %>%
      dplyr::mutate(
        player = parse_player_names(x = player)
      ) %>%
      ungroup() %>%
      left_join(
        player_teams, 
        by = 'player'
      )
    
  }
  
  
  return(player_detail)
  
}
