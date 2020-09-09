
get_point_scoring_by_hole = function(match_scoring){
  
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
  
  return(point_scoring_by_hole)
  
}

