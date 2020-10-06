
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(kableExtra)
library(DT)

source("src/r/wigs_2020_data_parse.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    # App title ----
                    dashboardHeader(title = ""),
                    
                    # Sidebar layout with a input and output definitions ----
                    dashboardSidebar(
                        width = 250,
                        # Sidebar panel for inputs ----
                        sidebarMenu(
                            img(
                                src = "logo_night.png", 
                                width = 240,
                                style = "padding-left: 10px; margin-right: auto;"
                            ),
                            br(),
                            br(),
                            menuItem(text = "Team Performance",
                                     tabName = "team_performance"),
                            menuItem(text = "Player Performance",
                                     tabName = "player_performance"),
                            menuItem(text = "Player Comparison",
                                     tabName = "player_comparison"),
                            br(),
                            br()
                        )
                    ),
                    
                    # Main panel for displaying outputs ----
                    dashboardBody(
                        chooseSliderSkin(
                            skin = c("Flat"),
                            color = NULL
                        ),
                        setSliderColor(c("#4169e1"), c(1)),
                        tabItems(
                            tabItem(tabName = "team_performance",
                                    fluidRow(
                                        column(width = 12,
                                               box(title = "Results", 
                                                   column(width = 6,
                                                          selectizeInput("round_selection", "Round No.", choices = c("R1 - Pairs", "R2 - Pairs", "R3 - Singles", "All"), selected = c("R1 - Pairs"))
                                                   ),
                                                   column(width = 6,
                                                          selectizeInput("results_selection", "Results Selection", choices = c("Match Points", "Holes Won", "Nett Score"), selected = c("Match Points"))
                                                   ),
                                                   withSpinner(plotOutput("results_plot", width = "100%", height = "700px"), color = "#4169e1"),
                                                   width = NULL, height = 850),
                                               box(dataTableOutput("results_table"),
                                                   width = NULL, height = 450, style = "height:450px; overflow-y: scroll;overflow-x: scroll;")
                                        ))),
                            tabItem(tabName = "player_performance",
                                    fluidRow(
                                        column(width = 12,
                                               box(title = "Player Results", 
                                                   column(width = 6,
                                                          selectizeInput("player_round_selection", "Round No.", choices = c("R1 - Pairs", "R2 - Pairs", "R3 - Singles", "All"), selected = c("R1 - Pairs"))
                                                   ),
                                                   column(width = 6,
                                                          selectizeInput("player_results_selection", "Results Selection", choices = c("Strokes Gained", "Nett Score", "Birdies or Better", "Bogeys or Worse"), selected = c("Strokes Gained"))
                                                   ),
                                                   withSpinner(plotOutput("player_results_plot", width = "100%", height = "600px"),color = "#4169e1"),
                                                   width = NULL, height = 750),
                                               box(dataTableOutput("player_results_table"),
                                                   width = NULL, height = 550, style = "height:550px; overflow-y: scroll;overflow-x: scroll;")
                                        ))),
                            tabItem(tabName = "player_comparison",
                                    fluidRow(
                                        column(width = 12,
                                               box(title = "Player Comparison", 
                                                       column(width = 6,
                                                              selectizeInput("player1_selection", "Player 1", choices = unique(all_player_detail$player), selected = c("Corrie Reeves")),
                                                              uiOutput("player1_info")
                                                       ),
                                                       column(width = 6,
                                                              selectizeInput("player2_selection", "Player 2", choices = unique(all_player_detail$player), selected = c("Kieran Ratcliffe")),
                                                              uiOutput("player2_info")
                                                       ),
                                                   selectizeInput("player_comparison_variable", "Comparison Variable", choices = c("Strokes Gained", "Contribution", "Nett Score"), selected = c("Strokes Gained")),
                                                   withSpinner(plotOutput("player_comparison_plot", width = "100%", height = "600px"),color = "#4169e1"),
                                                   dataTableOutput("player_comparison_table"),
                                                   width = NULL, height = 1200)
                                        )))
                        )
                    ),
                    
                    tags$head(tags$style(HTML('
                                              /* logo */
                                              .skin-blue .main-header .logo {
                                              background-color: #343436;
                                              color: white;
                                              font-size: 21px;
                                              font-weight: 400;
                                              padding-left: 0px;
                                              }
                                              
                                              /* logo when hovered */
                                              .skin-blue .main-header .logo:hover {
                                              background-color: #343436;
                                              color: white;
                                              font-size: 21px;
                                              font-weight: 400;
                                              padding-left: 0px;
                                              }
                                              
                                              /* navbar (rest of the header) */
                                              .skin-blue .main-header .navbar {
                                              background-color: #343436;
                                              }        
                                              
                                              /* main sidebar */
                                              .skin-blue .main-sidebar {
                                              background-color: #343436;
                                              color: white!important;
                                              }
                                              
                                              /*Tab border color*/
                                              .nav-tabs-custom>.nav-tabs>li.active {
                                              border-top-color: #4169e1;
                                              color: #4169e1;
                                              }
                                              
                                              /* other links in the sidebarmenu */
                                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                              background-color: #343436 !important;
                                              color: white;
                                              }
                                              
                                              .main-sidebar .user-panel, .sidebar-menu, .sidebar-menu>li.header {
                                              color: white;
                                              }
                                              
                                              
                                              .sidebar-menu>li.active>.treeview-menu {
                                              background-color: #343436;
                                              color: white;
                                              }
                                              
                                              .skin-blue .sidebar-menu>li>.treeview-menu {
                                              background-color: #343436;
                                              color: white;
                                              }
                                              
                                              .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
                                              color: black;
                                              background: #343436;
                                              border-left-color: #4169e1;
                                              }
                                              
                                              /*background*/
                                              .content-wrapper,
                                              .right-side {
                                              background-color: #343436;
                                              color: white!important;
                                              }
                                              
                                              .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                                              color: white;
                                              }
                                              
                                              /*sidebar labels*/
                                              .label {
                                              background-color: #343436;
                                              color: white;
                                              }
                                              
                                              .btn {
                                              background-color: #343436;
                                              display: block;
                                              color: white;
                                              border-color: white;
                                              display: block;
                                              }
                                              
                                              .btn-default {
                                              background-color: #343436;
                                              display: block;
                                              color: white;
                                              border-color: black;
                                              display: block;
                                              }
                                              
                                              .box.box-solid[class*=bg]>.box-header {
                                              color: white;
                                              }
                                              
                                              .dataTables_wrapper {
                                              background-color: #343436;
                                              }
                                              
                                              .box-body {
                                              background-color: #343436;
                                              }
                                              
                                              .mybutton2 {
                                              margin-top: 320px;
                                              display: inline-block;
                                              }              
                                              
                                              .mybutton3{
                                              margin-left: 20px;
                                              margin-top: 320px;
                                              display: inline-block;
                                              }
                                              
                                              .box-header .box-title {
                                              color: white;
                                              background-color: #343436;
                                              }
                                              
                                              .skin-blue .main-sidebar .sidebar .sidebar-menu a {
                                              background-color: #4169e1;
                                              }
                                              
                                              .skin-blue .sidebar a {
                                              color: #343436;
                                              }
                                              
                                              .box {
                                              background: #343436;
                                              }                                                
                                              
                                              .box-header {
                                              background-color: #343436;
                                              }
                                              .alert-danger, .alert-error, .alert-info, .alert-success, .alert-warning, .bg-aqua, .bg-aqua-active, .bg-black, .bg-black-active, .bg-blue, .bg-blue-active, .bg-fuchsia, .bg-fuchsia-active, .bg-green, .bg-green-active, .bg-light-blue, .bg-light-blue-active, .bg-lime, .bg-lime-active, .bg-maroon, .bg-maroon-active, .bg-navy, .bg-navy-active, .bg-olive, .bg-olive-active, .bg-orange, .bg-orange-active, .bg-purple, .bg-purple-active, .bg-red, .bg-red-active, .bg-teal, .bg-teal-active, .bg-yellow, .bg-yellow-active, .callout.callout-danger, .callout.callout-info, .callout.callout-success, .callout.callout-warning, .label-danger, .label-info, .label-primary, .label-success, .label-warning, .modal-danger .modal-body, .modal-danger .modal-footer, .modal-danger .modal-header, .modal-info .modal-body, .modal-info .modal-footer, .modal-info .modal-header, .modal-primary .modal-body, .modal-primary .modal-footer, .modal-primary .modal-header, .modal-success .modal-body, .modal-success .modal-footer, .modal-success .modal-header, .modal-warning .modal-body, .modal-warning .modal-footer, .modal-warning .modal-header{
                                              color: white!important;
                                              } 
                                              
                                              table.dataTable tbody tr.selected td, table.dataTable td.selected {
                                              background-color: #D3D3D3 !important;
                                              color: black !important;
                                              }
                                              
                                              .selectize-input.full {
                                              background-color: #343436 !important;
                                              color: white !important;
                                              border-color: white;
                                              }
                                              
                                              .selectize-dropdown-content > .option{
                                              background-color: #343436 !important;
                                              color: white !important;
                                              }
                                              
                                              
                                              .selectize-dropdown-content {
                                              background-color: #343436 !important;
                                              color: white;
                                              border-color: white;
                                              }
                                              
                                              
                                              selectize-input.focus {
                                              background-color: #343436 !important;
                                              color: white;
                                              }
                                              
                                              .selectize-input, .selectize-control.single .selectize-input.input-active {
                                              background: #343436 !important;
                                              color: white;
                                              }
                                              
                                              .small-box.bg-yellow { 
                                              background-color: #FFFFFF !important; 
                                              color: #343436 !important; 
                                              }
                                              
                                              .small-box.bg-blue { 
                                              background-color: #4169e1 !important; 
                                              color: #FFFFFF !important; 
                                              }
                                              
                                              .small-box .icon-large {
                                              top: 5px;
                                              }
                                              
                                              ')),
                              
                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$player1_info <- renderValueBox({
        
        selected_player_detail = all_final_match_scores %>%
            left_join(
                all_player_detail %>%
                    dplyr::select(full_name = player, group_id, round, player_team_id = team_id, hcp = playing_hcp)
            ) %>%
            dplyr::filter(full_name == input$player1_selection, team_id == player_team_id) %>%
            dplyr::mutate(
                points = case_when(
                    match_result == "Won" ~ 1,
                    match_result == "Half" ~ 0.5,
                    match_result == "Loss" ~ 0
                )
            ) %>%
            group_by(full_name, player_team_id, hcp) %>%
            dplyr::summarise(
                matches_played = n(),
                points_won = sum(points)
            ) %>%
            ungroup()
        
        box_colour = ifelse(
            selected_player_detail$player_team_id == "Blue",
            "blue",
            "yellow"
        )
        
        status_colour = case_when(
            selected_player_detail$points_won <= 1 ~ "danger",
            selected_player_detail$points_won > 1 & selected_player_detail$points_won <= 2 ~ "warning",
            selected_player_detail$points_won > 2 ~ "success",
        )
        
        valueBox(
            subtitle = tagList(paste("Team:", selected_player_detail$player_team_id),
                               p(""),
                               paste("HCP:", selected_player_detail$hcp),
                               p(""),
                               paste("Points Won:", selected_player_detail$points_won),
                               shinyWidgets::progressBar(id = "test", value = selected_player_detail$points_won, total = selected_player_detail$matches_played, display_pct = FALSE, striped = TRUE, status = status_colour)
            ),
            value = tags$p(input$player1_selection, style = "font-size: 60%;"),
            icon = icon("user"),
            color = box_colour
        )
        
    })
    
    output$player2_info <- renderValueBox({
        
        selected_player_detail = all_final_match_scores %>%
            left_join(
                all_player_detail %>%
                    dplyr::select(full_name = player, group_id, round, player_team_id = team_id, hcp = playing_hcp)
            ) %>%
            dplyr::filter(full_name == input$player2_selection, team_id == player_team_id) %>%
            dplyr::mutate(
                points = case_when(
                    match_result == "Won" ~ 1,
                    match_result == "Half" ~ 0.5,
                    match_result == "Loss" ~ 0
                )
            ) %>%
            group_by(full_name, player_team_id, hcp) %>%
            dplyr::summarise(
                matches_played = n(),
                points_won = sum(points)
            ) %>%
            ungroup()
        
        box_colour = ifelse(
            selected_player_detail$player_team_id == "Blue",
            "blue",
            "yellow"
        )
        
        status_colour = case_when(
            selected_player_detail$points_won <= 1 ~ "danger",
            selected_player_detail$points_won > 1 & selected_player_detail$points_won <= 2 ~ "warning",
            selected_player_detail$points_won > 2 ~ "success",
        )
        
        valueBox(
            subtitle = tagList(paste("Team:", selected_player_detail$player_team_id),
                               p(""),
                               paste("HCP:", selected_player_detail$hcp),
                               p(""),
                               paste("Points Won:", selected_player_detail$points_won),
                               shinyWidgets::progressBar(id = "test", value = selected_player_detail$points_won, total = selected_player_detail$matches_played, display_pct = FALSE, striped = TRUE, status = status_colour)
            ),
            value = tags$p(input$player2_selection, style = "font-size: 60%;"),
            icon = icon("user"),
            color = box_colour
        )
        
    })

    output$results_plot <- renderPlot({
        
        if(input$results_selection == "Match Points"){
        
            if(grepl("R", input$round_selection) == TRUE){
                
                selected_round = substr(input$round_selection, 1, 2)
                
                all_final_match_scores = all_final_match_scores %>%
                    dplyr::filter(round == selected_round) %>%
                    arrange(group_id, team_id) %>%
                    dplyr::mutate(
                        Match = paste0("Match ", group_id)
                    ) %>%
                    dplyr::select(
                        Match,
                        `Player(s)` = team_name,
                        Team = team_id,
                        Result = match_result,
                        Score = match_score
                    )
                
                dt = datatable(all_final_match_scores)
                
                if(length(input$results_table_rows_selected) == 0){
                   
                    matches_selected = unique(all_final_match_scores$Match)
                    
                }else{
                    
                    matches_selected = dt$x$data$Match[dt$x$data$` ` %in% input$results_table_rows_selected]
                    
                }
                
                all_point_scoring_by_group = all_point_scoring_by_group %>% 
                    dplyr::filter(round == selected_round, group_id %in% matches_selected) 
                
                max_groups = length(unique(all_point_scoring_by_group$group_id))
                
                ggplot(all_point_scoring_by_group %>% dplyr::filter(round == selected_round), aes(x = hole_no, y = match_score_plot, fill = lead_team))+
                    facet_wrap(~group_id, nrow = 2)+
                    geom_hline(yintercept = 0, linetype = "dashed", colour = "white")+
                    geom_bar(stat = "identity", alpha = 0.9)+
                    scale_fill_manual(values = c("royalblue", "white"))+
                    scale_y_continuous(breaks = seq(-10, 10, 2), labels = abs(seq(-10, 10, 2)))+
                    labs(x = "Hole No.", y = "Lead (Holes)")+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }else{
                
                all_final_match_scores = all_final_match_scores %>%
                    arrange(round, group_id, team_id) %>%
                    dplyr::mutate(
                        Match = paste0(round, " Match ", group_id)
                    ) %>%
                    dplyr::select(
                        Match,
                        `Player(s)` = team_name,
                        Team = team_id,
                        Result = match_result,
                        Score = match_score
                    )
                
                dt = datatable(all_final_match_scores)
                
                if(length(input$results_table_rows_selected) == 0){
                    
                    matches_selected = unique(all_final_match_scores$Match)
                    
                }else{
                    
                    matches_selected = dt$x$data$Match[dt$x$data$` ` %in% input$results_table_rows_selected]
                    
                }
                
                all_point_scoring_by_group = all_point_scoring_by_group %>% 
                    dplyr::mutate(
                        group_id = paste(round, group_id)
                    ) %>%
                    dplyr::filter(group_id %in% matches_selected)
                
                ggplot(all_point_scoring_by_group, aes(x = hole_no, y = match_score_plot, fill = lead_team))+
                    facet_wrap(~group_id, nrow = 4)+
                    geom_hline(yintercept = 0, linetype = "dashed", colour = "white")+
                    geom_bar(stat = "identity", alpha = 0.9)+
                    scale_fill_manual(values = c("royalblue", "white"))+
                    scale_y_continuous(breaks = seq(-10, 10, 2), labels = abs(seq(-10, 10, 2)))+
                    labs(x = "Hole No.", y = "Lead (Holes)")+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }
            
        }else{
        
        if(input$results_selection == "Holes Won"){
            
            if(grepl("R", input$round_selection) == TRUE){
                
                selected_round = substr(input$round_selection, 1, 2)
                
                all_point_scoring_by_hole = all_point_scoring_by_hole %>% 
                    dplyr::filter(round == selected_round) %>%
                    group_by(team_id) %>%
                    dplyr::mutate(
                        max_holes = max(cumsum_team_points)
                    ) %>%
                    ungroup() %>%
                    arrange(team_id)
                
                ggplot(all_point_scoring_by_hole, aes(x = hole_no, y = cumsum_team_points, colour = team_id, group = team_id))+
                    geom_point(stat = "identity", size = 5)+
                    geom_line(linetype = "dashed")+
                    geom_text(aes(label = max_holes, x = 20, y = max_holes), size = 8)+
                    scale_x_continuous(breaks = seq(3, 18, 3))+
                    scale_colour_manual(values = c("royalblue", "white"))+
                    labs(x = "Hole No.", y = "Holes Won")+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }else{
                
                all_point_scoring_by_hole = all_point_scoring_by_hole %>% 
                    group_by(round, team_id) %>%
                    dplyr::mutate(
                        max_holes = max(cumsum_team_points)
                    ) %>%
                    ungroup() %>%
                    arrange(team_id)
                
                ggplot(all_point_scoring_by_hole, aes(x = hole_no, y = cumsum_team_points, colour = team_id, group = team_id))+
                    facet_wrap(~round, nrow = 1)+
                    geom_point(stat = "identity", size = 5)+
                    geom_line(linetype = "dashed")+
                    geom_text(aes(label = max_holes, x = 20, y = max_holes), size = 8)+
                    scale_x_continuous(breaks = seq(3, 18, 3))+
                    scale_colour_manual(values = c("royalblue", "white"))+
                    labs(x = "Hole No.", y = "Holes Won")+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }
            
        }else{
            
            if(grepl("R", input$round_selection) == TRUE){
                
                selected_round = substr(input$round_selection, 1, 2)
                
                team_scores = all_scoring_by_team %>%
                    dplyr::filter(round == selected_round) %>%
                    dplyr::mutate(
                        Match = paste0("Match ", group_id)
                    ) %>%
                    arrange(round, group_id, team_id) %>%
                    dplyr::select(
                        Round = round,
                        Match,
                        `Player(s)` = team_name,
                        Team = team_id, 
                        `Holes Played` = holes_played,
                        `Score` = team_score_nett,
                        `Score to Par` = team_score_nett_to_par,
                        `Birdies or Better` = bob_nett,
                        `Bogeys or Worse` = bow_nett
                    )
                
                dt = datatable(team_scores)
                
                if(length(input$results_table_rows_selected) == 0){
                    
                    matches_selected = unique(team_scores$Match)
                    
                }else{
                    
                    matches_selected = dt$x$data$Match[dt$x$data$` ` %in% input$results_table_rows_selected]
                    
                }
                
                all_hole_scoring_by_team = all_hole_scoring_by_team %>% 
                    dplyr::filter(round == selected_round, match_id %in% matches_selected) %>%
                    arrange(group_id)
                
                ggplot(all_hole_scoring_by_team, aes(hole_no, cumsum_nett_score_to_par, colour = team_id))+
                    facet_wrap(~match_id, nrow = 2)+
                    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                    geom_point(stat = "identity", size = 5)+
                    geom_line()+
                    scale_x_continuous(breaks = seq(3, 18, 3))+
                    scale_colour_manual(values = c("royalblue", "white"))+
                    scale_y_reverse()+
                    labs(x = "Hole No.", y = "Team Nett Score (to Par)")+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }else{
                
                team_scores = all_scoring_by_team %>%
                    dplyr::filter(round == selected_round) %>%
                    dplyr::mutate(
                        Match = paste0("Match ", group_id)
                    ) %>%
                    arrange(round, group_id, team_id) %>%
                    dplyr::select(
                        Round = round,
                        Match,
                        `Player(s)` = team_name,
                        Team = team_id, 
                        `Holes Played` = holes_played,
                        `Score` = team_score_nett,
                        `Score to Par` = team_score_nett_to_par,
                        `Birdies or Better` = bob_nett,
                        `Bogeys or Worse` = bow_nett
                    )
                
                dt = datatable(team_scores)
                
                if(length(input$results_table_rows_selected) == 0){
                    
                    matches_selected = unique(team_scores$Match)
                    
                }else{
                    
                    matches_selected = dt$x$data$Match[dt$x$data$` ` %in% input$results_table_rows_selected]
                    
                }
                
                all_hole_scoring_by_team = all_hole_scoring_by_team %>% 
                    dplyr::mutate(
                        match_id = paste0(round, " ", match_id)
                    ) %>%
                    arrange(round, group_id) %>%
                    dplyr::filter(match_id %in% matches_selected)
                
                ggplot(all_hole_scoring_by_team, aes(hole_no, cumsum_nett_score_to_par, colour = team_id))+
                    facet_wrap(~match_id, nrow = 4)+
                    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                    geom_point(stat = "identity", size = 5)+
                    geom_line()+
                    scale_x_continuous(breaks = seq(3, 18, 3))+
                    scale_colour_manual(values = c("royalblue", "white"))+
                    scale_y_reverse()+
                    labs(x = "Hole No.", y = "Team Nett Score (to Par)")+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }
            
        }
        }
    
    })
    
    output$results_table = renderDataTable({
        
        if(input$results_selection == "Match Points"){
        
        if(grepl("R", input$round_selection) == TRUE){
            
            selected_round = substr(input$round_selection, 1, 2)
            
            all_final_match_scores = all_final_match_scores %>%
                dplyr::filter(round == selected_round) %>%
                arrange(group_id, team_id) %>%
                dplyr::mutate(
                    Match = paste0("Match ", group_id)
                ) %>%
                dplyr::select(
                    Match,
                    `Player(s)` = team_name,
                    Team = team_id,
                    Result = match_result,
                    Score = match_score
                )
                
        }else{
            
            all_final_match_scores = all_final_match_scores %>%
                dplyr::mutate(
                    Match = paste0("Match ", group_id),
                    round_no = as.numeric(substr(round, 2, 2))
                ) %>%
                arrange(round_no, group_id, team_id) %>%
                dplyr::select(
                    Round = round,
                    Format = match_format,
                    Match,
                    `Player(s)` = team_name,
                    Team = team_id,
                    Result = match_result,
                    Score = match_score
                )
            
        }
        
        cols = length(all_final_match_scores)
        rows = length(all_final_match_scores$Match)
        
        datatable(
            all_final_match_scores, 
            options = list(
                scrollX = TRUE, 
                scrollY = TRUE, 
                paging = FALSE,
                # dom = 't',
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                    "}")
                )
        ) %>% 
            formatStyle(
            'Team',
            target = 'row',
            backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
            ) %>% 
            formatStyle(
                'Team',
                target = 'row',
                color = styleEqual(c('Blue', 'White'), c('white', 'black'))
            )
        
        }else{
            
            if(input$results_selection == "Holes Won"){
                
                if(grepl("R", input$round_selection) == TRUE){
                    
                    selected_round = substr(input$round_selection, 1, 2)
                    
                    holes_won_by_round = all_point_scoring_by_hole %>%
                        dplyr::filter(round == selected_round) %>%
                        group_by(round, team_id) %>%
                        dplyr::summarise(
                            team_points = sum(team_points, na.rm = T)
                        ) %>%
                        ungroup() %>%
                        dplyr::select(
                            Round = round,
                            Team = team_id, 
                            `Total Holes Won` = team_points
                        )
                    
                }else{
                    
                    holes_won_by_round = all_point_scoring_by_hole %>%
                        group_by(round, team_id) %>%
                        dplyr::summarise(
                            team_points = sum(team_points, na.rm = T)
                        ) %>%
                        ungroup() %>%
                        dplyr::select(
                            Round = round,
                            Team = team_id, 
                            `Total Holes Won` = team_points
                        )
                    
                }
                
                cols = length(holes_won_by_round)
                rows = length(holes_won_by_round$Round)
                
                datatable(
                    holes_won_by_round, 
                    options = list(
                        scrollX = TRUE, 
                        scrollY = TRUE, 
                        paging = FALSE,
                        # dom = 't',
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                            "}")
                    )
                ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                    ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                    )
                
            }else{
                
                if(grepl("R", input$round_selection) == TRUE){
                    
                    selected_round = substr(input$round_selection, 1, 2)
                    
                    team_scores = all_scoring_by_team %>%
                        dplyr::filter(round == selected_round) %>%
                        dplyr::mutate(
                            Match = paste0("Match ", group_id)
                        ) %>%
                        arrange(round, group_id, team_id) %>%
                        dplyr::select(
                            Round = round,
                            Match,
                            `Player(s)` = team_name,
                            Team = team_id, 
                            `Holes Played` = holes_played,
                            `Score` = team_score_nett,
                            `Score to Par` = team_score_nett_to_par,
                            `Birdies or Better` = bob_nett,
                            `Bogeys or Worse` = bow_nett
                        )
                    
                }else{
                    
                    team_scores = all_scoring_by_team %>%
                        dplyr::mutate(
                            Match = paste0("Match ", group_id)
                        ) %>%
                        arrange(round, group_id, team_id) %>%
                        dplyr::select(
                            Round = round,
                            Match,
                            `Player(s)` = team_name,
                            Team = team_id, 
                            `Holes Played` = holes_played,
                            `Score` = team_score_nett,
                            `Score to Par` = team_score_nett_to_par,
                            `Birdies or Better` = bob_nett,
                            `Bogeys or Worse` = bow_nett
                        )
                    
                }
                
                cols = length(team_scores)
                rows = length(team_scores$Round)
                
                datatable(
                    team_scores, 
                    options = list(
                        scrollX = TRUE, 
                        scrollY = TRUE, 
                        paging = FALSE,
                        # dom = 't',
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                            "}")
                    )
                ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                    ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                    )
                
            }
            
        }
        
        
    })
    
    
    output$player_results_plot <- renderPlot({
        
        if(input$player_results_selection == "Strokes Gained"){
            
            if(grepl("R", input$player_round_selection) == TRUE){
                
                selected_round = substr(input$player_round_selection, 1, 2)
                
                all_round_sg_by_player = all_round_sg_by_player %>%
                    dplyr::filter(round == selected_round) %>%
                    dplyr::select(
                        Round = round,
                        Player = player,
                        Team = team_id,
                        `Holes Played` = holes_played,
                        `Nett Score to Par` = nett_score_to_par,
                        `Strokes Gained` = sg
                    ) %>%
                    arrange(desc(`Strokes Gained`))
                
                dt = datatable(all_round_sg_by_player)
                
                if(length(input$player_results_table_rows_selected) == 0){
                    
                    players_selected = unique(all_round_sg_by_player$Player)
                    
                }else{
                    
                    players_selected = dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected]
                    
                }
                
                all_round_sg_by_player = all_round_sg_by_player %>%
                    dplyr::filter(Player %in% players_selected)

                all_round_sg_by_player$Player = factor(all_round_sg_by_player$Player, levels = unique(all_round_sg_by_player$Player[order(all_round_sg_by_player$`Strokes Gained`)]))
                
                plot_colour1 = case_when(
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                    length(unique(all_round_sg_by_player$Team)) > 1  ~ "royalblue",
                )
                
                plot_colour2 = case_when(
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                    length(unique(all_round_sg_by_player$Team)) > 1  ~ "white",
                )
                
                plot_colours = unique(c(plot_colour1, plot_colour2))
                
                ggplot(all_round_sg_by_player, aes(Player, `Strokes Gained`, fill =Team))+
                    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                    geom_bar(stat = "identity", size = 5)+
                    scale_fill_manual(values = plot_colours)+
                    labs(x = "", y = "Strokes Gained")+
                    coord_flip()+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }else{
                
                all_round_sg_by_player = all_round_sg_by_player %>%
                    group_by(player) %>%
                    dplyr::mutate(
                        sum_sg = sum(sg, na.rm = T)
                    ) %>%
                    dplyr::select(
                        Round = round,
                        Player = player,
                        Team = team_id,
                        `Holes Played` = holes_played,
                        `Nett Score to Par` = nett_score_to_par,
                        `Strokes Gained` = sg,
                        total_sg = sum_sg
                    ) %>%
                    arrange(desc(`Strokes Gained`))
                
                dt = datatable(all_round_sg_by_player)
                
                if(length(input$player_results_table_rows_selected) == 0){
                    
                    players_selected = unique(paste(all_round_sg_by_player$Round, all_round_sg_by_player$Player))
                    
                }else{
                    
                    players_selected = paste(dt$x$data$Round[dt$x$data$` ` %in% input$player_results_table_rows_selected], dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected])
                    
                }
                
                all_round_sg_by_player = all_round_sg_by_player %>%
                    dplyr::filter(paste(Round, Player) %in% players_selected)
                
                all_round_sg_by_player$Player = factor(all_round_sg_by_player$Player, levels = unique(all_round_sg_by_player$Player[order(all_round_sg_by_player$total_sg)]))
                
                plot_colour1 = case_when(
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                    length(unique(all_round_sg_by_player$Team)) > 1  ~ "royalblue",
                )
                
                plot_colour2 = case_when(
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                    length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                    length(unique(all_round_sg_by_player$Team)) > 1  ~ "white",
                )
                
                plot_colours = unique(c(plot_colour1, plot_colour2))
                
                ggplot(all_round_sg_by_player, aes(Player, `Strokes Gained`, fill = Team))+
                    facet_wrap(~Round, nrow = 1)+
                    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                    geom_bar(stat = "identity", size = 5)+
                    scale_fill_manual(values = plot_colours)+
                    labs(x = "", y = "Strokes Gained")+
                    coord_flip()+
                    theme_wigs_night()+
                    theme(
                        legend.position = "none"
                    )
                
            }
            
        }else{
            
            if(input$player_results_selection == "Nett Score"){
                
                if(grepl("R", input$player_round_selection) == TRUE){
                    
                    selected_round = substr(input$player_round_selection, 1, 2)
                    
                    all_round_sg_by_player = all_round_sg_by_player %>%
                        dplyr::filter(round == selected_round) %>%
                        dplyr::select(
                            Round = round,
                            Player = player,
                            Team = team_id,
                            `Holes Played` = holes_played,
                            `Nett Score to Par` = nett_score_to_par,
                            `Strokes Gained` = sg
                        ) %>%
                        arrange(`Nett Score to Par`)
                    
                    dt = datatable(all_round_sg_by_player)
                    
                    if(length(input$player_results_table_rows_selected) == 0){
                        
                        players_selected = unique(all_round_sg_by_player$Player)
                        
                    }else{
                        
                        players_selected = dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected]
                        
                    }
                    
                    all_round_sg_by_player = all_round_sg_by_player %>%
                        dplyr::filter(Player %in% players_selected)
                    
                    all_round_sg_by_player$Player = factor(all_round_sg_by_player$Player, levels = unique(all_round_sg_by_player$Player[order(desc(all_round_sg_by_player$`Nett Score to Par`))]))
                    
                    plot_colour1 = case_when(
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                        length(unique(all_round_sg_by_player$Team)) > 1  ~ "royalblue",
                    )
                    
                    plot_colour2 = case_when(
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                        length(unique(all_round_sg_by_player$Team)) > 1  ~ "white",
                    )
                    
                    plot_colours = unique(c(plot_colour1, plot_colour2))
                    
                    ggplot(all_round_sg_by_player, aes(Player, `Nett Score to Par`, fill =Team))+
                        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                        geom_bar(stat = "identity", size = 5)+
                        scale_fill_manual(values = plot_colours)+
                        labs(x = "", y = "Nett Score to Par")+
                        coord_flip()+
                        theme_wigs_night()+
                        theme(
                            legend.position = "none"
                        )
                    
                }else{
                    
                    all_round_sg_by_player = all_round_sg_by_player %>%
                        group_by(player) %>%
                        dplyr::mutate(
                            sum_score = sum(nett_score_to_par, na.rm = T)
                        ) %>%
                        dplyr::select(
                            Round = round,
                            Player = player,
                            Team = team_id,
                            `Holes Played` = holes_played,
                            `Nett Score to Par` = nett_score_to_par,
                            `Strokes Gained` = sg,
                            total_score = sum_score
                        ) %>%
                        arrange(`Nett Score to Par`)
                    
                    dt = datatable(all_round_sg_by_player)
                    
                    if(length(input$player_results_table_rows_selected) == 0){
                        
                        players_selected = unique(paste(all_round_sg_by_player$Round, all_round_sg_by_player$Player))
                        
                    }else{
                        
                        players_selected = paste(dt$x$data$Round[dt$x$data$` ` %in% input$player_results_table_rows_selected], dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected])
                        
                    }
                    
                    all_round_sg_by_player = all_round_sg_by_player %>%
                        dplyr::filter(paste(Round, Player) %in% players_selected)
                    
                    all_round_sg_by_player$Player = factor(all_round_sg_by_player$Player, levels = unique(all_round_sg_by_player$Player[order(desc(all_round_sg_by_player$total_score))]))
                    
                    plot_colour1 = case_when(
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                        length(unique(all_round_sg_by_player$Team)) > 1  ~ "royalblue",
                    )
                    
                    plot_colour2 = case_when(
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "Blue" ~ "royalblue",
                        length(unique(all_round_sg_by_player$Team)) == 1 & unique(all_round_sg_by_player$Team) == "White" ~ "white",
                        length(unique(all_round_sg_by_player$Team)) > 1  ~ "white",
                    )
                    
                    plot_colours = unique(c(plot_colour1, plot_colour2))
                    
                    ggplot(all_round_sg_by_player, aes(Player, `Nett Score to Par`, fill = Team))+
                        facet_wrap(~Round, nrow = 1)+
                        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                        geom_bar(stat = "identity", size = 5)+
                        scale_fill_manual(values = plot_colours)+
                        labs(x = "", y = "Nett Score to Par")+
                        coord_flip()+
                        theme_wigs_night()+
                        theme(
                            legend.position = "none"
                        )
                    
                }
                
            }else{
                
                if(input$player_results_selection == "Birdies or Better"){
                    
                    if(grepl("R", input$player_round_selection) == TRUE){
                        
                        selected_round = substr(input$player_round_selection, 1, 2)
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::filter(round == selected_round) %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bow = length(score_to_par[score_to_par >= 1])
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Birdie or Better` = bob,
                                `Bogey or Worse` = bow
                            ) %>%
                            arrange(desc(`Birdie or Better`))
                        
                        dt = datatable(all_match_scoring)
                        
                        if(length(input$player_results_table_rows_selected) == 0){
                            
                            players_selected = unique(all_match_scoring$Player)
                            
                        }else{
                            
                            players_selected = dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected]
                            
                        }
                        
                        all_match_scoring = all_match_scoring %>%
                            dplyr::filter(Player %in% players_selected)
                        
                        all_match_scoring$Player = factor(all_match_scoring$Player, levels = unique(all_match_scoring$Player[order(all_match_scoring$`Birdie or Better`)]))
                        
                        plot_colour1 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "royalblue",
                        )
                        
                        plot_colour2 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "white",
                        )
                        
                        plot_colours = unique(c(plot_colour1, plot_colour2))
                        
                        ggplot(all_match_scoring, aes(Player, `Birdie or Better`, fill =Team))+
                            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                            geom_bar(stat = "identity", size = 5)+
                            scale_fill_manual(values = plot_colours)+
                            labs(x = "", y = "Birdie or Better")+
                            coord_flip()+
                            theme_wigs_night()+
                            theme(
                                legend.position = "none"
                            )
                        
                    }else{
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(round, player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bow = length(score_to_par[score_to_par >= 1])
                            ) %>%
                            group_by(player) %>%
                            dplyr::mutate(
                                total_bob = sum(bob),
                                total_bow = sum(bow)
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Round = round,
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Birdie or Better` = bob,
                                `Bogey or Worse` = bow,
                                total_bob,
                                total_bow
                            ) %>%
                            arrange(desc(`Birdie or Better`))
                        
                        dt = datatable(all_match_scoring)
                        
                        if(length(input$player_results_table_rows_selected) == 0){
                            
                            players_selected = unique(paste(all_match_scoring$Round, all_match_scoring$Player))
                            
                        }else{
                            
                            players_selected = paste(dt$x$data$Round[dt$x$data$` ` %in% input$player_results_table_rows_selected], dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected])
                            
                        }
                        
                        all_match_scoring = all_match_scoring %>%
                            dplyr::filter(paste(Round, Player) %in% players_selected)
                        
                        all_match_scoring$Player = factor(all_match_scoring$Player, levels = unique(all_match_scoring$Player[order(all_match_scoring$total_bob)]))
                        
                        all_match_scoring = all_match_scoring %>%
                            dplyr::filter(Player %in% players_selected)
                        
                        all_match_scoring$Player = factor(all_match_scoring$Player, levels = unique(all_match_scoring$Player[order(all_match_scoring$`Birdie or Better`)]))
                        
                        plot_colour1 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "royalblue",
                        )
                        
                        plot_colour2 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "white",
                        )
                        
                        plot_colours = unique(c(plot_colour1, plot_colour2))
                        
                        ggplot(all_match_scoring, aes(Player, `Birdie or Better`, fill = Team))+
                            facet_wrap(~Round, nrow = 1)+
                            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                            geom_bar(stat = "identity", size = 5)+
                            scale_fill_manual(values = plot_colours)+
                            labs(x = "", y = "Birdie or Better")+
                            coord_flip()+
                            theme_wigs_night()+
                            theme(
                                legend.position = "none"
                            )
                        
                    }
                    
                }else{
                    
                    if(grepl("R", input$player_round_selection) == TRUE){
                        
                        selected_round = substr(input$player_round_selection, 1, 2)
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::filter(round == selected_round) %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bow = length(score_to_par[score_to_par >= 1])
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Birdie or Better` = bob,
                                `Bogey or Worse` = bow
                            ) %>%
                            arrange(desc(`Bogey or Worse`))
                        
                        dt = datatable(all_match_scoring)
                        
                        if(length(input$player_results_table_rows_selected) == 0){
                            
                            players_selected = unique(all_match_scoring$Player)
                            
                        }else{
                            
                            players_selected = dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected]
                            
                        }
                        
                        all_match_scoring = all_match_scoring %>%
                            dplyr::filter(Player %in% players_selected)
                        
                        all_match_scoring$Player = factor(all_match_scoring$Player, levels = unique(all_match_scoring$Player[order(all_match_scoring$`Bogey or Worse`)]))
                        
                        plot_colour1 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "royalblue",
                        )
                        
                        plot_colour2 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "white",
                        )
                        
                        plot_colours = unique(c(plot_colour1, plot_colour2))
                        
                        ggplot(all_match_scoring, aes(Player, `Bogey or Worse`, fill =Team))+
                            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                            geom_bar(stat = "identity", size = 5)+
                            scale_fill_manual(values = plot_colours)+
                            labs(x = "", y = "Bogey or Worse")+
                            coord_flip()+
                            theme_wigs_night()+
                            theme(
                                legend.position = "none"
                            )
                        
                    }else{
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(round, player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bow = length(score_to_par[score_to_par >= 1])
                            ) %>%
                            group_by(player) %>%
                            dplyr::mutate(
                                total_bob = sum(bob),
                                total_bow = sum(bow)
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Round = round,
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Birdie or Better` = bob,
                                `Bogey or Worse` = bow,
                                total_bob,
                                total_bow
                            ) %>%
                            arrange(desc(`Bogey or Worse`))
                        
                        dt = datatable(all_match_scoring)
                        
                        if(length(input$player_results_table_rows_selected) == 0){
                            
                            players_selected = unique(paste(all_match_scoring$Round, all_match_scoring$Player))
                            
                        }else{
                            
                            players_selected = paste(dt$x$data$Round[dt$x$data$` ` %in% input$player_results_table_rows_selected], dt$x$data$Player[dt$x$data$` ` %in% input$player_results_table_rows_selected])
                            
                        }
                        
                        all_match_scoring = all_match_scoring %>%
                            dplyr::filter(paste(Round, Player) %in% players_selected)
                        
                        all_match_scoring$Player = factor(all_match_scoring$Player, levels = unique(all_match_scoring$Player[order(all_match_scoring$total_bow)]))
                        
                        plot_colour1 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "royalblue",
                        )
                        
                        plot_colour2 = case_when(
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "Blue" ~ "royalblue",
                            length(unique(all_match_scoring$Team)) == 1 & unique(all_match_scoring$Team) == "White" ~ "white",
                            length(unique(all_match_scoring$Team)) > 1  ~ "white",
                        )
                        
                        plot_colours = unique(c(plot_colour1, plot_colour2))
                        
                        ggplot(all_match_scoring, aes(Player, `Bogey or Worse`, fill = Team))+
                            facet_wrap(~Round, nrow = 1)+
                            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                            geom_bar(stat = "identity", size = 5)+
                            scale_fill_manual(values = plot_colours)+
                            labs(x = "", y = "Bogey or Worse")+
                            coord_flip()+
                            theme_wigs_night()+
                            theme(
                                legend.position = "none"
                            )
                        
                    }
                    
                }
                
            }
        }
        
    })
    
    
    
    output$player_results_table <- renderDataTable({
        
        if(input$player_results_selection == "Strokes Gained"){
            
            if(grepl("R", input$player_round_selection) == TRUE){
                
                selected_round = substr(input$player_round_selection, 1, 2)
                
                all_round_sg_by_player = all_round_sg_by_player %>%
                    dplyr::filter(round == selected_round) %>%
                    dplyr::mutate(
                        sg = round(sg, 2)
                    ) %>%
                    dplyr::select(
                        Round = round,
                        Player = player,
                        Team = team_id,
                        `Holes Played` = holes_played,
                        `Strokes Gained` = sg
                    )
                
                cols = length(all_round_sg_by_player)
                rows = length(all_round_sg_by_player$Round)
                
                datatable(
                    all_round_sg_by_player %>%
                        arrange(desc(`Strokes Gained`)), 
                    options = list(
                        scrollX = TRUE, 
                        scrollY = TRUE, 
                        paging = FALSE,
                        # dom = 't',
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                            "}")
                    )
                ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                    ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                    )
                
            }else{
                
                all_round_sg_by_player = all_round_sg_by_player %>%
                    group_by(player) %>%
                    dplyr::mutate(
                        sum_sg = round(sum(sg, na.rm = T), 2),
                    ) %>%
                    dplyr::select(
                        Round = round,
                        Player = player,
                        Team = team_id,
                        `Holes Played` = holes_played,
                        `Strokes Gained` = sg,
                        `Total Strokes Gained` = sum_sg
                    )
                
                cols = length(all_round_sg_by_player)
                rows = length(all_round_sg_by_player$Round)
                
                datatable(
                    all_round_sg_by_player %>%
                        arrange(desc(`Strokes Gained`)), 
                    options = list(
                        scrollX = TRUE, 
                        scrollY = TRUE, 
                        paging = FALSE,
                        # dom = 't',
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                            "}")
                    )
                ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                    ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                    )
                
            }
            
        }else{
            
            if(input$player_results_selection == "Nett Score"){
                
                if(grepl("R", input$player_round_selection) == TRUE){
                    
                    selected_round = substr(input$player_round_selection, 1, 2)
                    
                    all_round_sg_by_player = all_round_sg_by_player %>%
                        dplyr::filter(round == selected_round) %>%
                        dplyr::select(
                            Round = round,
                            Player = player,
                            Team = team_id,
                            `Holes Played` = holes_played,
                            `Nett Score` = nett_score_to_par
                        )
                    
                    cols = length(all_round_sg_by_player)
                    rows = length(all_round_sg_by_player$Round)
                    
                    datatable(
                        all_round_sg_by_player %>%
                            arrange(`Nett Score`), 
                        options = list(
                            scrollX = TRUE, 
                            scrollY = TRUE, 
                            paging = FALSE,
                            # dom = 't',
                            initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                                "}")
                        )
                    ) %>% 
                        formatStyle(
                            'Team',
                            target = 'row',
                            backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                        ) %>% 
                        formatStyle(
                            'Team',
                            target = 'row',
                            color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                        )
                    
                }else{
                    
                    all_round_sg_by_player = all_round_sg_by_player %>%
                        group_by(player) %>%
                        dplyr::mutate(
                            sum_score = sum(nett_score_to_par, na.rm = T)
                        ) %>%
                        dplyr::select(
                            Round = round,
                            Player = player,
                            Team = team_id,
                            `Holes Played` = holes_played,
                            `Nett Score` = nett_score_to_par,
                            `Total Nett Score` = sum_score
                        )
                    
                    cols = length(all_round_sg_by_player)
                    rows = length(all_round_sg_by_player$Round)
                    
                    datatable(
                        all_round_sg_by_player %>%
                            arrange(`Nett Score`), 
                        options = list(
                            scrollX = TRUE, 
                            scrollY = TRUE, 
                            paging = FALSE,
                            # dom = 't',
                            initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                                "}")
                        )
                    ) %>% 
                        formatStyle(
                            'Team',
                            target = 'row',
                            backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                        ) %>% 
                        formatStyle(
                            'Team',
                            target = 'row',
                            color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                        )
                    
                }
                
            }else{
                
                if(input$player_results_selection == "Birdies or Better"){
                    
                    if(grepl("R", input$player_round_selection) == TRUE){
                        
                        selected_round = substr(input$player_round_selection, 1, 2)
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::filter(round == selected_round) %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(round, player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bob_perc = round((bob/holes_played)*100),
                                bow = length(score_to_par[score_to_par >= 1]),
                                bow_perc = round((bow/holes_played)*100)
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Round = round,
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Birdies or Better` = bob,
                                `%` = bob_perc
                            )
                        
                        cols = length(all_match_scoring)
                        rows = length(all_match_scoring$Player)
                        
                        datatable(
                            all_match_scoring %>%
                                arrange(desc(`Birdies or Better`)), 
                            options = list(
                                scrollX = TRUE, 
                                scrollY = TRUE, 
                                paging = FALSE,
                                # dom = 't',
                                initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                                    "}")
                            )
                        ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                            ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                            )
                        
                    }else{
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(round, player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bob_perc = round((bob/holes_played)*100),
                                bow = length(score_to_par[score_to_par >= 1]),
                                bow_perc = round((bow/holes_played)*100)
                            ) %>%
                            group_by(player) %>%
                            dplyr::mutate(
                                total_bob = sum(bob),
                                total_bob_perc = round(mean(bob_perc)),
                                total_bow = sum(bow),
                                total_bow_perc = round(mean(bow_perc))
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Round = round,
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Birdies or Better` = bob,
                                `%` = bob_perc,
                                `Total Birdies or Better` = total_bob,
                                `Total %` = total_bob_perc
                            )
                        
                        cols = length(all_match_scoring)
                        rows = length(all_match_scoring$Player)
                        
                        datatable(
                            all_match_scoring %>%
                                arrange(desc(`Birdies or Better`)), 
                            options = list(
                                scrollX = TRUE, 
                                scrollY = TRUE, 
                                paging = FALSE,
                                # dom = 't',
                                initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                                    "}")
                            )
                        ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                            ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                            )
                        
                    }
                    
                }else{
                    
                    if(grepl("R", input$player_round_selection) == TRUE){
                        
                        selected_round = substr(input$player_round_selection, 1, 2)
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::filter(round == selected_round) %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(round, player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bob_perc = round((bob/holes_played)*100),
                                bow = length(score_to_par[score_to_par >= 1]),
                                bow_perc = round((bow/holes_played)*100)
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Round = round,
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Bogeys or Worse` = bow,
                                `%` = bow_perc
                            )
                        
                        cols = length(all_match_scoring)
                        rows = length(all_match_scoring$Player)
                        
                        datatable(
                            all_match_scoring %>%
                                arrange(desc(`Bogeys or Worse`)), 
                            options = list(
                                scrollX = TRUE, 
                                scrollY = TRUE, 
                                paging = FALSE,
                                # dom = 't',
                                initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                                    "}")
                            )
                        ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                            ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                            )
                        
                    }else{
                        
                        all_match_scoring = all_match_scoring %>%
                            ungroup() %>%
                            dplyr::mutate(
                                score_to_par = hole_score_nett-hole_par
                            ) %>%
                            dplyr::filter(!is.na(score_to_par)) %>%
                            group_by(round, player, team_id) %>%
                            dplyr::summarise(
                                holes_played = n(),
                                bob = length(score_to_par[score_to_par <= -1]),
                                bob_perc = round((bob/holes_played)*100),
                                bow = length(score_to_par[score_to_par >= 1]),
                                bow_perc = round((bow/holes_played)*100)
                            ) %>%
                            group_by(player) %>%
                            dplyr::mutate(
                                total_bob = sum(bob),
                                total_bob_perc = round(mean(bob_perc)),
                                total_bow = sum(bow),
                                total_bow_perc = round(mean(bow_perc))
                            ) %>%
                            ungroup() %>%
                            dplyr::select(
                                Round = round,
                                Player = player,
                                Team = team_id,
                                `Holes Played` = holes_played,
                                `Bogeys or Worse` = bow,
                                `%` = bow_perc,
                                `Total Bogeys or Worse` = total_bow,
                                `Total %` = total_bow_perc
                            )
                        
                        cols = length(all_match_scoring)
                        rows = length(all_match_scoring$Player)
                        
                        datatable(
                            all_match_scoring %>%
                                arrange(desc(`Bogeys or Worse`)), 
                            options = list(
                                scrollX = TRUE, 
                                scrollY = TRUE, 
                                paging = FALSE,
                                # dom = 't',
                                initComplete = JS(
                                    "function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                                    "}")
                            )
                        ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                            ) %>% 
                            formatStyle(
                                'Team',
                                target = 'row',
                                color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                            )
                        
                    }
                    
                }
                
            }
        }
        
    })
    
    
    output$player_comparison_plot <- renderPlot({
        
        if(input$player_comparison_variable == "Contribution"){
            
            selected_players = c(input$player1_selection, input$player2_selection)
            
            ymin = min(all_contribution_by_player$relative_contribution)
            ymax = max(all_contribution_by_player$relative_contribution)
            
            all_contribution_by_player = all_contribution_by_player %>%
                dplyr::filter(player %in% selected_players) %>%
                arrange(team_id)
            
            plot_colour1 = case_when(
                length(unique(all_contribution_by_player$team_id)) == 1 & unique(all_contribution_by_player$team_id) == "Blue" ~ "royalblue",
                length(unique(all_contribution_by_player$team_id)) == 1 & unique(all_contribution_by_player$team_id) == "White" ~ "white",
                length(unique(all_contribution_by_player$team_id)) > 1 ~ "royalblue"
            )
            
            plot_colour2 = case_when(
                length(unique(all_contribution_by_player$team_id)) == 1 & unique(all_contribution_by_player$team_id) == "Blue" ~ "royalblue",
                length(unique(all_contribution_by_player$team_id)) == 1 & unique(all_contribution_by_player$team_id) == "White" ~ "white",
                length(unique(all_contribution_by_player$team_id)) > 1 ~ "white"
            )
            
            plot_colours = unique(c(plot_colour1, plot_colour2))
            
            ggplot(all_contribution_by_player, aes(player, relative_contribution))+
                geom_hline(yintercept = 0, colour = "white")+
                geom_bar(aes(fill = team_id), stat = "identity", width = 0.05, alpha = 0.8)+
                geom_point(aes(colour = team_id), size = 30)+
                geom_text(aes(label = ifelse(relative_contribution > 0, paste0("+", round(relative_contribution, 1)), round(relative_contribution, 1))), size = 10)+
                geom_hline(yintercept = ymin, linetype = "dashed", colour = "white")+
                geom_text(aes(y = ymin, x = 2.5, label = 'MIN'), colour = "white", size = 6, angle = 90, vjust = 1.5)+
                geom_hline(yintercept = ymax, linetype = "dashed", colour = "white")+
                geom_text(aes(y = ymax, x = 2.5, label = 'MAX'), colour = "white", size = 6, angle = 90, vjust = -0.5)+
                scale_fill_manual(values = plot_colours)+
                scale_colour_manual(values = plot_colours)+
                coord_flip()+
                labs(x = "", y = "Contribution Rating")+
                theme_wigs_night()+
                theme(
                    axis.text.x = element_blank(),
                    legend.position = "none"
                )
            
        }else{
            
            if(input$player_comparison_variable == "Strokes Gained"){
                
                selected_players = c(input$player1_selection, input$player2_selection)
                
                all_hole_sg_by_player = all_hole_sg_by_player %>%
                    dplyr::mutate(
                        round_no = as.numeric(gsub("R", "", round)),
                        round_start_hole_no = 18*(round_no-1),
                        overall_hole_no = round_start_hole_no+hole_no,
                        sg = ifelse(
                            is.na(sg),
                            0,
                            sg
                        )
                    ) %>%
                    distinct() %>%
                    arrange(overall_hole_no) %>%
                    group_by(player) %>%
                    dplyr::mutate(
                        cumsum_sg = cumsum(sg)
                    ) %>%
                    dplyr::filter(player %in% selected_players) %>%
                    arrange(team_id)
                
                plot_colour1 = case_when(
                    length(unique(all_hole_sg_by_player$team_id)) == 1 & unique(all_hole_sg_by_player$team_id) == "Blue" ~ "royalblue",
                    length(unique(all_hole_sg_by_player$team_id)) == 1 & unique(all_hole_sg_by_player$team_id) == "White" ~ "white",
                    length(unique(all_hole_sg_by_player$team_id)) > 1 ~ "royalblue"
                )
                
                plot_colour2 = case_when(
                    length(unique(all_hole_sg_by_player$team_id)) == 1 & unique(all_hole_sg_by_player$team_id) == "Blue" ~ "firebrick3",
                    length(unique(all_hole_sg_by_player$team_id)) == 1 & unique(all_hole_sg_by_player$team_id) == "White" ~ "firebrick3",
                    length(unique(all_hole_sg_by_player$team_id)) > 1 ~ "white"
                )
                
                plot_colours = unique(c(plot_colour1, plot_colour2))
                
                ggplot(all_hole_sg_by_player, aes(overall_hole_no, cumsum_sg))+
                    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                    geom_vline(xintercept = 18, colour = "grey80")+
                    geom_vline(xintercept = 36, colour = "grey80")+
                    geom_vline(xintercept = 54, colour = "grey80")+
                    geom_text(aes(y = max(cumsum_sg), x = 18, label = "Round 1"), size = 6, vjust = -0.5, hjust = 2.5, colour = "white")+
                    geom_text(aes(y = max(cumsum_sg), x = 36, label = "Round 2"), size = 6, vjust = -0.5, hjust = 2.5, colour = "white")+
                    geom_text(aes(y = max(cumsum_sg), x = 54, label = "Round 3"), size = 6, vjust = -0.5, hjust = 2.5, colour = "white")+
                    geom_point(aes(colour = player), size = 3)+
                    geom_line(aes(colour = player), linetype = "dashed")+
                    scale_colour_manual(values = plot_colours)+
                    labs(x = "Hole", y = "Strokes Gained", colour = "")+
                    theme_wigs_night()+
                    theme(
                        legend.position = "top"
                    )
                
            }else{
                
                if(input$player_comparison_variable == "Nett Score"){
                    
                    selected_players = c(input$player1_selection, input$player2_selection)
                    
                    all_match_scoring = all_match_scoring %>%
                        dplyr::select(player, team_id, round, hole_no, hole_score_nett, hole_par) %>%
                        dplyr::mutate(
                            round_no = as.numeric(gsub("R", "", round)),
                            round_start_hole_no = 18*(round_no-1),
                            overall_hole_no = round_start_hole_no+hole_no,
                            hole_score_nett = ifelse(
                                is.na(hole_score_nett),
                                hole_par,
                                hole_score_nett
                            ),
                            hole_score_nett_to_par = hole_score_nett-hole_par
                        ) %>%
                        distinct() %>%
                        arrange(overall_hole_no) %>%
                        group_by(player) %>%
                        dplyr::mutate(
                            cumsum_score_nett_to_par = cumsum(hole_score_nett_to_par)
                        ) %>%
                        dplyr::filter(player %in% selected_players) %>%
                        arrange(team_id)
                    
                    plot_colour1 = case_when(
                        length(unique(all_match_scoring$team_id)) == 1 & unique(all_match_scoring$team_id) == "Blue" ~ "royalblue",
                        length(unique(all_match_scoring$team_id)) == 1 & unique(all_match_scoring$team_id) == "White" ~ "white",
                        length(unique(all_match_scoring$team_id)) > 1 ~ "royalblue"
                    )
                    
                    plot_colour2 = case_when(
                        length(unique(all_match_scoring$team_id)) == 1 & unique(all_match_scoring$team_id) == "Blue" ~ "firebrick3",
                        length(unique(all_match_scoring$team_id)) == 1 & unique(all_match_scoring$team_id) == "White" ~ "firebrick3",
                        length(unique(all_match_scoring$team_id)) > 1 ~ "white"
                    )
                    
                    plot_colours = unique(c(plot_colour1, plot_colour2))
                    
                    ggplot(all_match_scoring, aes(overall_hole_no, cumsum_score_nett_to_par))+
                        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80")+
                        geom_vline(xintercept = 18, colour = "grey80")+
                        geom_vline(xintercept = 36, colour = "grey80")+
                        geom_vline(xintercept = 54, colour = "grey80")+
                        geom_text(aes(y = min(cumsum_score_nett_to_par), x = 18, label = "Round 1"), size = 6, vjust = -0.5, hjust = 2.5, colour = "white")+
                        geom_text(aes(y = min(cumsum_score_nett_to_par), x = 36, label = "Round 2"), size = 6, vjust = -0.5, hjust = 2.5, colour = "white")+
                        geom_text(aes(y = min(cumsum_score_nett_to_par), x = 54, label = "Round 3"), size = 6, vjust = -0.5, hjust = 2.5, colour = "white")+
                        geom_point(aes(colour = player), size = 3)+
                        geom_line(aes(colour = player), linetype = "dashed")+
                        scale_colour_manual(values = plot_colours)+
                        scale_y_reverse()+
                        labs(x = "Hole", y = "Nett Score to Par", colour = "")+
                        theme_wigs_night()+
                        theme(
                            legend.position = "top"
                        )
                    
                }else{
                    
                    
                    
                }
                
            }
            
        }
        
    })
    
    output$player_comparison_table <- renderDataTable({
        
        if(input$player_comparison_variable == "Contribution"){
            
            selected_players = c(input$player1_selection, input$player2_selection)

            all_contribution_by_player = all_contribution_by_player %>%
                dplyr::filter(player %in% selected_players) %>%
                dplyr::select(
                    Player = player,
                    Team = team_id,
                    `Matches Played` = matches_played,
                    `Points Won` = points_won,
                    `Expected Contribution` = expected_contribution,
                    `Actual Contribution` = all_contribution,
                    `Rel. Contribution` = relative_contribution
                )
            
            cols = length(all_contribution_by_player)
            rows = length(all_contribution_by_player$Player)
            
            datatable(
                all_contribution_by_player,
                options = list(
                    dom = 't',
                    initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                        "}")
                )
            ) %>% 
                formatStyle(
                    'Team',
                    target = 'row',
                    backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                ) %>% 
                formatStyle(
                    'Team',
                    target = 'row',
                    color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                )
            
        }else{
            
            if(input$player_comparison_variable == "Strokes Gained"){
                
                selected_players = c(input$player1_selection, input$player2_selection)
                
                all_hole_sg_by_player = all_hole_sg_by_player %>%
                    dplyr::filter(player %in% selected_players) %>%
                    group_by(player, team_id, round) %>%
                    dplyr::summarise(
                        sum_sg = round(sum(sg, na.rm = T), 2)
                    ) %>%
                    ungroup() %>%
                    dplyr::mutate(
                        round = paste(round, "SG")
                    ) %>%
                    dcast(player+team_id~round) %>%
                    dplyr::rename(
                        Player = player,
                        Team = team_id
                    )
                
                cols = length(all_hole_sg_by_player)
                rows = length(all_hole_sg_by_player$Player)
                
                datatable(
                    all_hole_sg_by_player,
                    options = list(
                        dom = 't',
                        initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                            "}")
                    )
                ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                    ) %>% 
                    formatStyle(
                        'Team',
                        target = 'row',
                        color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                    )
                
            }else{
                
                if(input$player_comparison_variable == "Nett Score"){
                    
                    selected_players = c(input$player1_selection, input$player2_selection)
                    
                    all_match_scoring = all_match_scoring %>%
                        dplyr::filter(player %in% selected_players) %>%
                        dplyr::select(player, team_id, round, hole_no, hole_score_nett, hole_par) %>%
                        dplyr::mutate(
                            hole_score_nett_to_par = hole_score_nett-hole_par
                        ) %>%
                        group_by(player, team_id, round) %>%
                        dplyr::summarise(
                            nett_score = sum(hole_score_nett_to_par, na.rm = T)
                        ) %>%
                        ungroup() %>%
                        dplyr::mutate(
                            round = paste(round, "Nett Score")
                        ) %>%
                        dcast(player+team_id~round) %>%
                        dplyr::rename(
                            Player = player,
                            Team = team_id
                        )
                    
                    cols = length(all_match_scoring)
                    rows = length(all_match_scoring$Player)
                    
                    datatable(
                        all_match_scoring,
                        options = list(
                            dom = 't',
                            initComplete = JS(
                                "function(settings, json) {",
                                "$(this.api().table().header()).css({'background-color': '#343436', 'color': 'white'});",
                                "}")
                        )
                    ) %>% 
                        formatStyle(
                            'Team',
                            target = 'row',
                            backgroundColor = styleEqual(c('Blue', 'White'), c('royalblue', 'white'))
                        ) %>% 
                        formatStyle(
                            'Team',
                            target = 'row',
                            color = styleEqual(c('Blue', 'White'), c('white', 'black'))
                        )
                    
                }else{
                    
                    
                    
                }
                
            }
            
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
