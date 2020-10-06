
get_scoring_by_team = function(match_scoring, match_format){
  
  if(match_format == "pairs"){
  
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
    group_by(group_id, team_name, team_id, hole_no, hole_par) %>%
    dplyr::summarise(
      team_hole_score_nett = min(team_hole_score_nett)
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      team_hole_score_nett_to_par = team_hole_score_nett-hole_par
    ) %>%
    group_by(group_id, team_name, team_id) %>%
    dplyr::summarise(
      holes_played = n(),
      bob_nett = sum(team_hole_score_nett_to_par <= -1),
      bow_nett = sum(team_hole_score_nett_to_par >= 1),
      team_score_nett = sum(team_hole_score_nett),
      team_score_nett_to_par = sum(team_hole_score_nett_to_par)
    ) 
  
  }else{
    
    scoring_by_team = match_scoring %>%
      drop_na() %>%
      group_by(group_id, team_id, player) %>%
      dplyr::mutate(
        player_last_name = strsplit(player, " ")[[1]][2]
      ) %>%
      group_by(group_id, team_id) %>%
      dplyr::mutate(
        team_name = player_last_name
      ) %>%
      group_by(group_id, team_name, team_id, hole_no, hole_par) %>%
      dplyr::summarise(
        team_hole_score_nett = min(team_hole_score_nett)
      ) %>%
      ungroup() %>%
      dplyr::mutate(
        team_hole_score_nett_to_par = team_hole_score_nett-hole_par
      ) %>%
      group_by(group_id, team_name, team_id) %>%
      dplyr::summarise(
        holes_played = n(),
        bob_nett = sum(team_hole_score_nett_to_par <= -1),
        bow_nett = sum(team_hole_score_nett_to_par >= 1),
        team_score_nett = sum(team_hole_score_nett),
        team_score_nett_to_par = sum(team_hole_score_nett_to_par)
      ) 
    
  }
  
  return(scoring_by_team)
  
}