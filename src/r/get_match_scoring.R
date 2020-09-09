
get_match_scoring = function(player_scorecards){
  
  #--- Create match scoring table 
  
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
      team_hole_score_nett = min(hole_score_nett)
    ) %>%
    group_by(group_id, hole_no) %>%
    dplyr::mutate(
      best_group_score = min(team_hole_score_nett),
      tie = case_when(
        length(team_hole_score_nett[team_hole_score_nett == best_group_score]) == 4 ~TRUE,
        length(team_hole_score_nett[team_hole_score_nett == best_group_score]) < 4 ~FALSE
      )
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      hole_won = case_when(
        team_hole_score_nett == best_group_score & tie == FALSE ~ TRUE,
        team_hole_score_nett != best_group_score | tie == TRUE ~ FALSE,
      )
    ) 
  
}
