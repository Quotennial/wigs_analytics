
get_match_scoring = function(player_scorecards, match_format){
  
  #--- Create match scoring table 
  
  if(match_format == "pairs"){
    
    match_scoring = player_scorecards %>%
      dplyr::select(player, group_id, team_id, hole_score, hole_no, hole_par, stroke_index, playing_hcp) %>%
      dplyr::mutate(
        hole_score = as.numeric(hole_score),
        playing_hcp = as.numeric(playing_hcp),
        stroke_index = as.numeric(stroke_index),
        hcp_hole_shots = case_when(
          (playing_hcp-stroke_index) > -1 ~ ((floor((playing_hcp-stroke_index)/18)*18)+18)/18,
          (playing_hcp-stroke_index) <= -1 ~ (ceiling((playing_hcp-stroke_index)/18)*18)/18
        ),
        hole_score_nett = hole_score-hcp_hole_shots
      ) %>%
      group_by(group_id, team_id, hole_no) %>%
      dplyr::mutate(
        team_hole_score_nett = min(hole_score_nett, na.rm = T)
      ) %>%
      group_by(group_id, hole_no) %>%
      dplyr::mutate(
        best_group_score = min(team_hole_score_nett, na.rm = T),
        worst_group_score = max(team_hole_score_nett, na.rm = T)
      ) %>%
      ungroup() %>%
      dplyr::mutate(
        tie = ifelse(
          best_group_score == worst_group_score,
          TRUE,
          FALSE
        ),
        hole_won = case_when(
          team_hole_score_nett == best_group_score & tie == FALSE ~ TRUE,
          team_hole_score_nett != best_group_score | tie == TRUE ~ FALSE,
        )
      ) %>%
      group_by(group_id, team_id, player) %>%
      dplyr::mutate(
        player_last_name = strsplit(player, " ")[[1]][2]
      ) %>%
      group_by(group_id, team_id) %>%
      dplyr::mutate(
        team_name = paste(player_last_name[1], player_last_name[2], sep = "/")
      )
    
  }else{
    
    match_scoring = player_scorecards %>%
      dplyr::select(player, group_id, team_id, hole_score, hole_no, hole_par, stroke_index, playing_hcp) %>%
      dplyr::mutate(
        hole_score = as.numeric(hole_score),
        playing_hcp = as.numeric(playing_hcp),
        stroke_index = as.numeric(stroke_index),
        hcp_hole_shots = case_when(
          (playing_hcp-stroke_index) > -1 ~ ((floor((playing_hcp-stroke_index)/18)*18)+18)/18,
          (playing_hcp-stroke_index) <= -1 ~ (ceiling((playing_hcp-stroke_index)/18)*18)/18
        ),
        hole_score_nett = hole_score-hcp_hole_shots
      ) %>%
      group_by(group_id, team_id, hole_no) %>%
      dplyr::mutate(
        team_hole_score_nett = min(hole_score_nett, na.rm = T)
      ) %>%
      group_by(group_id, hole_no) %>%
      dplyr::mutate(
        best_group_score = min(team_hole_score_nett, na.rm = T),
        worst_group_score = max(team_hole_score_nett, na.rm = T)
      ) %>%
      ungroup() %>%
      dplyr::mutate(
        tie = ifelse(
          best_group_score == worst_group_score,
          TRUE,
          FALSE
        ),
        hole_won = case_when(
          team_hole_score_nett == best_group_score & tie == FALSE ~ TRUE,
          team_hole_score_nett != best_group_score | tie == TRUE ~ FALSE,
        )
      ) %>%
      group_by(group_id, team_id, player) %>%
      dplyr::mutate(
        player_last_name = strsplit(player, " ")[[1]][2]
      ) %>%
      group_by(group_id, team_id) %>%
      dplyr::mutate(
        team_name = player_last_name
      )
    
  }
  
  return(match_scoring)
  
}
