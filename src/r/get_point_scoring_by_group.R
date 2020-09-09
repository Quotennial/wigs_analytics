
get_point_scoring_by_group = function(match_scoring){
  
  point_scoring_by_group = match_scoring %>%
    group_by(hole_no, group_id, team_id) %>%
    dplyr::summarise(
      hole_points = case_when(
        max(hole_won) == TRUE ~ 1,
        max(tie) == TRUE ~ 0,
        max(hole_won) == FALSE ~ 0,
      )
    ) %>%
    group_by(group_id, team_id) %>%
    arrange(hole_no) %>%
    dplyr::mutate(
      cumsum_holes = cumsum(hole_points)
    ) %>%
    group_by(group_id, hole_no) %>%
    dplyr::summarise(
      match_score = max(cumsum_holes)-min(cumsum_holes),
      lead_team = ifelse(
        match_score == 0,
        NA,
        team_id[cumsum_holes == max(cumsum_holes)]
      )
    ) %>%
    ungroup() %>%
    dplyr::mutate(
      group_id = paste0("Match ", group_id),
      match_score_plot = case_when(
        lead_team == "Blue" ~ match_score*-1, 
        lead_team == "White" ~ match_score
      ),
      holes_remaining = 18-hole_no,
      match_won = case_when(
        match_score > holes_remaining ~ TRUE,
        match_score <= holes_remaining ~ FALSE,
      )
    ) %>%
    group_by(group_id) %>%
    arrange(hole_no) %>%
    dplyr::mutate(
      match_won = lag(match_won)
    ) %>%
    ungroup() %>%
    dplyr::filter(match_won == FALSE)
  
  return(point_scoring_by_group)
  
}
