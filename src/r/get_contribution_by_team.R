
get_contribution_by_team = function(match_scoring){
  
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
  
  return(contribution_by_team)
  
}