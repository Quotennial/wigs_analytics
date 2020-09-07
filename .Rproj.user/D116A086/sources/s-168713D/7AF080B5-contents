#--- Read in raw data

team_scorecards = read_excel("Data/raw/xlsx/Scorecards_20200902.xlsx", col_names = FALSE)

#--- Create course detail table

course_detail = data.frame(
  hole_no = as.numeric(team_scorecards[1, ]),
  hole_par = as.numeric(team_scorecards[4, ]),
  stroke_index = as.numeric(team_scorecards[2, ])
) %>%
  drop_na()