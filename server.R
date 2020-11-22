gg_court = make_court()

function(input, output) {

    output$plot_TOP <- renderPlotly({
        NBA_clean <- NBA_clean %>% 
            as.data.frame()
        
        NBA_TOP <- NBA_clean %>% 
            dplyr::filter(Season == input$season) %>% 
            dplyr::group_by(Player_Name) %>%
            dplyr::summarise(pctFG = mean(pctFG),
                      pctFG3 = mean(pctFG3),
                      pctFT = mean(pctFT),
                      pctFG2 = mean(pctFG2)) %>%
            dplyr::arrange(-pctFG3) %>% 
            head(10)
        
        NBA_TOP <- NBA_TOP %>% 
            as.data.frame()

        
        selected_column <- input$numvar
        
        plot_top <- NBA_TOP %>% 
            ggplot(aes(x = NBA_TOP[ , selected_column], 
                       y = reorder(Player_Name,NBA_TOP[, selected_column]),
                       text = glue("{Player_Name}
                         Percentage: {round(pctFG*100, 2)}%"))) +
            geom_col(fill = "dodgerblue4") +
            labs( x = NULL,
                  y = NULL,
                  title = glue("NBA Top Player by Field Goal Percentage {input$season}")) +
            scale_x_continuous(labels = percent_format(accuracy = 1)) +
            theme_custom
        
        ggplotly(plot_top, tooltip = "text")
    })
    
    output$plot_distribution <- renderPlotly({
        NBA_Team <- NBA_clean %>% 
            dplyr::filter(Season == input$season) %>% 
            dplyr::group_by(Team_Name) %>%
            dplyr::summarise(pctFG = mean(pctFG),
                      pctFG3 = mean(pctFG3),
                      pctFT = mean(pctFT),
                      pctFG2 = mean(pctFG2))
        
        plot_dist <- NBA_Team %>% 
            ggplot(aes(x = pctFG2, 
                       y = pctFG3, 
                       col = Team_Name,
                       text = glue("{str_to_upper(Team_Name)}
                         Two Point Field Goal Percentage: {round(pctFG2*100, 2)}%
                         Three Point Field Goal Percentage: {round(pctFG3*100, 2)}%"))) +
            geom_jitter() +
            labs(y = "Three Point Field Goal Percentage",
                 x = "Two Point Field Goal Percentage",
                 title = glue("Field Goal Comparison {input$season}")) +
            scale_x_continuous(labels = percent_format(accuracy = 1)) +
            scale_y_continuous(labels = percent_format(accuracy = 1)) +
            theme_custom +
            theme(legend.position = "none")
        
        ggplotly(plot_dist, tooltip = "text")
    })
    
    output$season_choice <- renderUI({
        seasons = nba_shots %>% 
            dplyr::filter(player_name == input$player_choice) %>%
            distinct(season) %>% 
            pull()
        
        selectizeInput("season_choice", label = h3("Select season"), choices = seasons,
                       selected = seasons[1], multiple = TRUE)
    })
    
    output$court_shots <- renderPlot({
        
        player_data = dplyr::filter(nba_shots, player_name == input$player_choice,
                             season %in% input$season_choice)
        
        
        gg_court + geom_point(data = player_data, alpha = 0.75, size = 2.5,
                              aes(loc_x, loc_y, color = shot_made_flag, shape = season)) +
            scale_color_manual("", values = c(made = "blue", missed = "orange"))
    })
    
    
    x <- list(
        title = "Player Name")
    
    y <- list(
        title = "Shot Distance")
    
    
    output$shot_distances <- renderPlotly({
        nba_shots %>%
            dplyr::filter(if(input$shots_made != 'all')  (shot_made_flag == input$shots_made) else TRUE) %>%
            plot_ly(y = ~shot_distance, color = ~player_name, type = "box") %>%
            layout(showlegend = FALSE,
                   yaxis = y,
                   xaxis = x)
    })
    
    output$mymap <- renderLeaflet({
        library(plyr)
        df2 <- plyr::ddply(df, .(lats, longs, Birthplace), summarize,
                           PlayerTeams=paste(PlayerTeams,collapse=" <br> "))
        
        leaflet(df2)%>%
            addCircleMarkers(lng = ~longs, lat = ~lats,
                             radius = ~sqrt(nchar(PlayerTeams)),
                             popup = ~as.character(paste0("<h3>", Birthplace, "</h3>",
                                                          "<h4>Players: </h4>", PlayerTeams)))%>%
            setView(lng = -99, lat = 40, zoom = 5)%>%
            addProviderTiles(providers$OpenStreetMap)
        
    })
    
    output$raw_data <- renderDataTable({
        datatable(data = NBA_clean,
                  options = list(scrollX = T))
    })
}