
get_course_summary = function(match_scoring){
  
  course_summary = match_scoring %>%
    drop_na() %>%
    dplyr::mutate(
      hole_score_to_par = hole_score-hole_par,
      hole_score_nett_to_par = hole_score_nett-hole_par
    ) %>%
    group_by(hole_no, hole_par) %>%
    dplyr::summarise(
      avg_nett_score = mean(hole_score_nett_to_par, na.rm = T),
      bob = mean(hole_score_nett_to_par <= -1),
      bow = mean(hole_score_nett_to_par >= 1)
    ) %>%
    ungroup()
  
  return(course_summary)
  
}