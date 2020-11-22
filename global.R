library(shiny)
library(shinydashboard)
library(plyr)
library(tidyverse)
library(plotly)
library(scales)
library(glue)
library(DT)
library(leaflet)
library(sp)
library(textclean)

filter <- dplyr::filter

lag <- dplyr::lag

NBA <- read_csv("NBA.csv")

NBA_clean <- NBA %>% 
  mutate(pctFG = replace_na(pctFG,
                            replace = 0),
         pctFT = replace_na(pctFT,
                            replace = 0),
         treb = replace_na(treb,
                           replace = 0)) %>% 
  select(-idTeam, 
         -isB2B,
         -slugLeague,
         -isB2BFirst,
         -isB2BSecond,
         -locationGame,
         -countDaysRestTeam,
         -countDaysNextGameTeam,
         -countDaysRestPlayer,
         -countDaysNextGamePlayer,
         -idPlayer,
         -hasVideo,
         -urlTeamSeasonLogo,
         -urlPlayerStats,
         -urlPlayerThumbnail,
         -urlPlayerHeadshot,
         -urlPlayerActionPhoto,
         -urlPlayerPhoto) %>% 
  dplyr::rename(Season = yearSeason,
         Slug_Season = slugSeason,
         Type_Season = typeSeason,
         Date = dateGame,
         ID_Game = idGame,
         Number_Game_Team_Season = numberGameTeamSeason,
         Team_Name = nameTeam,
         Matchup = slugMatchup,
         Slug_Team = slugTeam,
         Slug_Opponent = slugOpponent,
         Team_Winner = slugTeamWinner,
         Team_Loser = slugTeamLoser,
         Game_Outcome = outcomeGame,
         Player_Name = namePlayer,
         Player_Season = numberGamePlayerSeason,
         Win = isWin,
         Minutes = minutes,
         Plus_Minus = plusminus)


theme_custom <- theme(legend.key = element_rect(fill="black"),
                         legend.background = element_rect(color="white", fill="#263238"),
                         plot.subtitle = element_text(size=6, color="white"),
                         panel.background = element_rect(fill="#dddddd"),
                         panel.border = element_rect(fill=NA),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(color="darkblue", linetype=2),
                         panel.grid.minor.y = element_blank(),
                         plot.background = element_rect(fill="#000000"),
                         text = element_text(color="white"),
                         axis.text = element_text(color="white"))

# df <-  read.csv("NBA Birth Place.csv")
# df <- df %>% 
#   mutate(lats = replace_non_ascii(Latitude,replacement = "-"),
#          longs = replace_non_ascii(Longitutde,replacement = "-"))
# df <- df %>% 
#   mutate(lats = as.numeric(char2dms(lats,chd = "-", chm = "'", chs='"')),
#          longs = as.numeric(char2dms(longs,chd = "-", chm = "'", chs='"')))
# 
# saveRDS(df,"NBA_Birth.rds")

df <- readRDS("NBA_Birth.rds")

df$PlayerTeams <-  paste0(df$Player, " (", df$Team, ")")
# df
# chd <-  substr(df$Latitude, 3, 3)[1]
# chm <-  substr(df$Latitude, 6, 6)[1]
# chs <-  substr(df$Latitude, 9, 9)[1]
# 
# df$lats <-  as.numeric(char2dms(df$Latitude,chd=chd,chm=chm,chs=chs))
# 
# 
# df$longs = as.numeric(char2dms(df$Longitutde,chd=chd,chm=chm,chs=chs))

df2 <- plyr::ddply(df, .(lats, longs, Birthplace), summarize,
                   PlayerTeams=paste(PlayerTeams,collapse=" <br> "))



circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

theme_court = function(base_size = 16, bg_color = "white") {
  theme_bw(base_size) +
    theme(
      text = element_text(color = "#C0C0C0"),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      legend.background = element_rect(fill = bg_color, color = bg_color),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

load("nba_shots.RData")



make_court = function(){
  width = 50
  height = 94 / 2
  key_height = 19
  inner_key_width = 12
  outer_key_width = 16
  backboard_width = 6
  backboard_offset = 4
  neck_length = 0.5
  hoop_radius = 0.75
  hoop_center_y = backboard_offset + neck_length + hoop_radius
  three_point_radius = 23.75
  three_point_side_radius = 22
  three_point_side_height = 14
  
  
  court_points = data.frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2, 
          outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2,
          -backboard_width / 2, backboard_width / 2, 
          0, 0),
    y = c(height, 0, 0, height, height, 0, key_height, key_height, 0,
          backboard_offset, backboard_offset, 
          backboard_offset, backboard_offset + neck_length),
    desc = c(rep("perimeter", 5), rep("outer_key", 4), rep("backboard", 2),
             rep("neck", 2))
  )
  
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  foul_circle_top = dplyr::filter(foul_circle, y > key_height) %>% 
    dplyr::mutate(desc = "foul_circle_top")
  foul_circle_bottom = dplyr::filter(foul_circle, y < key_height) %>% 
    dplyr::mutate(desc = "foul_circle_bottom")
  
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% dplyr::mutate(desc = "hoop") 
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    dplyr::filter(y >= hoop_center_y) %>%
    dplyr::mutate(desc = "restricted")
  
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% dplyr::filter(y >= three_point_side_height)
  three_point_line = data.frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = rbind(court_points , foul_circle_top, foul_circle_bottom, hoop, restricted, three_point_line)
  court_points = dplyr::mutate(court_points , dash = (desc == "foul_circle_bottom"))
  
  
  
  court = ggplot() +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc, linetype = dash),
              color = "black") +
    scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
    coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
    theme_court(base_size = 22)
  
  court  
}




















