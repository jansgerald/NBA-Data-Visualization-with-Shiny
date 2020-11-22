players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()


dashboardPage(title = "NBA Data Visualization",
              skin = "blue",
              
              dashboardHeader(title = "NBA"),
              
              dashboardSidebar(
                  sidebarMenu(
                      menuItem(
                          text = "Top Player",
                          tabName = "TP",
                          icon = icon("male")),
                      menuItem(
                          text = "Top Team",
                          tabName = "TT",
                          icon = icon("users")),
                      menuItem(
                          text = "Top Player Shot Chart",
                          tabName = "TPSC",
                          icon = icon("bullseye")),
                      menuItem(
                          text = "Player Birthplace",
                          tabName = "PB",
                          icon = icon("map")),
                      menuItem(text = "Raw Data",
                               tabName = "data",
                               icon = icon("server")),
                      selectInput(
                          inputId = "season",
                          label = "Select Season",
                          choices = unique(NBA_clean$Season))
                  )),
              
              dashboardBody(
                  tabItems(
                      tabItem(tabName = "TP", align = "center",
                              div(h1("NBA Top Player", align = "center")),
                              div(h2("Top 10 NBA Player by Field Goal Percentage",align = "center")),
                              tags$br(),
                              fluidRow(
                                column(width = 12, 
                                       box(width = 12,
                                           background = "light-blue",
                                           radioButtons(inputId = "numvar",
                                                        label = "Choose Variable",
                                                        choices = NBA_TOP %>% 
                                                          select_if(is.numeric) %>% 
                                                          names())))),
                              fluidRow(
                                column(width = 12, 
                                       box(width = 12,
                                           plotlyOutput(outputId = "plot_TOP"))))),
                      tabItem(tabName = "TT",
                              div(h1("NBA Top Team", align = "center")),
                              div(h2("Comparison Among All NBA Team by Field Goal Percentage",align = "center")),
                              tags$br(),
                              fluidRow(
                                column(width = 12,
                                       box(width = 12,
                                           plotlyOutput(outputId = "plot_distribution"))))),
                      tabItem(tabName = "TPSC",
                              div(h1("Best Player Shoot Chart", align = "center")),
                              tags$br(),
                              fluidRow(
                                column(width = 3,
                                       fluidRow(box(width = 12,
                                                    height = 200,
                                                    background = "light-blue",
                                                    selectInput("player_choice", 
                                                       label = "Select player",
                                                       choices = players, selected = "Stephen Curry"))),
                                       fluidRow(box(width = 12,
                                                    height = 200,
                                                    background = "light-blue",
                                                    uiOutput("season_choice")))),
                                column(width = 9,
                                       fluidRow(box(width = 12,
                                                    plotOutput(outputId = "court_shots"))))),
                              tags$br(),
                              fluidRow(
                                column(width = 12,
                                       box(width = 12,
                                           background = "green",
                                           align = "center",
                                           radioButtons("shots_made", 
                                                        label = "Shot status", 
                                                        choices = list("all", "made", "missed"))))),
                              plotlyOutput(outputId = "shot_distances")),
                      tabItem(tabName = "PB",
                              div(h1("NBA Player Birthplace", align = "center")),
                              div(h2("Find Your Favourite Basketball Player Hometown",align = "center")),
                              tags$br(),
                              fluidRow(
                                column(width = 12,
                                       box(width = 12,
                                           background = "black",
                                           leafletOutput("mymap", height = "90vh"))))),
                      tabItem(tabName = "data",
                              div(h1("Raw Data From Package nbaStatR", align = "center")),
                              fluidRow(
                                column(width = 12,
                                       box(width = 12,
                                           dataTableOutput(outputId = "raw_data")))))
                  )
              )
)