#----------------------------------------------------------------------------------------------#
#                                                                                              #
#                         create a football betting app - the app                              #
#                                                                                              #
#----------------------------------------------------------------------------------------------#

##  libraries  ##
library(shiny)
library(shinythemes)
library(DT)

#------------------------------------------------------------------------#
#                               load data                                #
#------------------------------------------------------------------------#

source("./data_work.R")

#------------------------------------------------------------------------#
#                               Shiny app                                #
#------------------------------------------------------------------------#

#--------------------------------#
#            UI                  #
#--------------------------------#

ui <- fluidPage(
  titlePanel("Football Stats Collection"),
  
  theme = shinytheme("superhero"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      (""),
      br(),
      
      #------------------------------------------------------------------------------------#
      #            add select buttons for league, home team and away team                  #
      #------------------------------------------------------------------------------------#
      
      selectInput("league", "Choose League", choices = data_full$Div, selected = c(" England - Premier League")),
      
      uiOutput("hometeam"), # use uiOutput since teams should depend on selected leage
      
      checkboxGroupInput("pitch", label = "Stats by Playing Site",
                         choices = c("all", "home", "away"), selected = "all"),
    
      uiOutput("referee")), # use uiOutput since teams should depend on selected leage
    
    mainPanel(tabsetPanel(
      
      tabPanel("About this App",
               br(),
               includeMarkdown("./intro.rmd")
      ),
      
      tabPanel("Goals",
               br(),
               tags$head(
                 tags$style(type='text/css', 
                            ".nav-tabs {font-size: 16px} ")),
               tabsetPanel(
                 
                 #------------------------------------------------------------------------------------#
                 #                         build tabs within tab "Goals"                              #
                 #------------------------------------------------------------------------------------#
                 
                 tabPanel("Full Time Goals",
                          br(), br(),
                          htmlOutput("text_total_goals"), br(),br(),
                          (DT::dataTableOutput("table_goals_ft")), br(), br(),
                          htmlOutput("text_team_goals"), br(),br(),
                          (DT::dataTableOutput("table_teamgoals_ft")), br(), br()),
                 
                 tabPanel("Half Time Goals",
                          br(), br(),
                          htmlOutput("text_total_firsthalf_goals"), br(),br(),
                          (DT::dataTableOutput("table_goals_firsthalf")), br(), br(),
                          htmlOutput("text_firsthalf_teamgoals"), br(),br(),
                          (DT::dataTableOutput("table_teamgoals_firsthalf")), br(), br(),
                          htmlOutput("text_total_secondhalf_goals"), br(),br(),
                          (DT::dataTableOutput("table_goals_secondhalf")), br(),br(),
                          htmlOutput("text_secondhalf_teamgoals"), br(),br(),
                          (DT::dataTableOutput("table_teamgoals_secondhalf"))),
                 
                 tabPanel("BTTS",
                          br(), br(),
                          htmlOutput("text_btts"), br(),br(),
                          (DT::dataTableOutput("table_goals_btts")))
               )
      ),
      
      tabPanel("Cards",
               br(),
               tabsetPanel(
                 
                 #------------------------------------------------------------------------------------#
                 #                         build tabs within tab "Cards"                              #
                 #------------------------------------------------------------------------------------#
                 
                 tabPanel("Total Cards",
                          br(), br(),
                          (DT::dataTableOutput("table_match_cards"))),
                          
                 tabPanel("Team Cards",
                          br(), br(),
                          (DT::dataTableOutput("table_team_cards")))
               )
      ),
      
      tabPanel("Corners",
               br(),
               tabsetPanel(
                 
                 #------------------------------------------------------------------------------------#
                 #                         build tabs within tab "Corners"                            #
                 #------------------------------------------------------------------------------------#
                 
                 tabPanel("Total Corners",
                          br(), br(),
                          (DT::dataTableOutput("table_match_corners"))),
                          
                 tabPanel("Team Corners",
                          br(), br(),
                          (DT::dataTableOutput("table_team_corners")))
                 
               )
      ), 
      
      tabPanel("Referees",
               br(), br(),
               htmlOutput("text_ref1"), br(),br(),
               (DT::dataTableOutput("table_referees1")), br(), br(),
               htmlOutput("text_ref2"), br(),br(),
               (DT::dataTableOutput("table_referees2")))
                 
                
    ))
  )
)
  
#--------------------------------#
#            Server              #
#--------------------------------#

server <- function(input, output, session) {
  
  # Introduction
  output$intro <- renderUI({
    HTML(markdown::markdownToHTML(knit('intro.rmd', quiet = TRUE)))
  })

  #------------------------------------------------------------------------------------#
  #                 make team selection dependend on league selection                  #
  #------------------------------------------------------------------------------------#
  
  output$hometeam <- renderUI({
    selectizeInput("hometeam1", label = "Choose Team(s)", choices = data_full[data_full$Div == input$league, "HomeTeam"],
                   multiple = TRUE, options = list(placeholder = "Select Teams"))
  })
  
  #------------------------------------------------------------------------------------#
  #             make referee selection (not dependend on league selection)             #
  #------------------------------------------------------------------------------------#
  
  output$referee <- renderUI({
    selectizeInput("referee1", label = "Choose Referee(s)", choices = data_ref[, "Referee"],
                   multiple = TRUE, options = list(placeholder = "Select Referees"))
  })
  
  #------------------------------------------------------------------------------------#
  #            reactive expression to make table react on selections                   #
  #------------------------------------------------------------------------------------#
  
  data_goals_app <- reactive({
    if (is.null(input$hometeam1)){
      data_goals %>%
        filter(Div %in% input$league,
               place %in% input$pitch)}
    else data_goals %>% 
      filter(HomeTeam %in% input$hometeam1,
             place %in% input$pitch)
  })
  
  #------------------------------------------------------------------------------------#
  #            reactive expression to make table react on selections                   #
  #------------------------------------------------------------------------------------#
  
  data_cards_app <- reactive({
    if (is.null(input$hometeam1)){
      data_cards %>%
        filter(Div %in% input$league,
               place %in% input$pitch)}
    else data_cards %>% 
      filter(HomeTeam %in% input$hometeam1,
             place %in% input$pitch)
  })
  
  #------------------------------------------------------------------------------------#
  #            reactive expression to make table react on selections                   #
  #------------------------------------------------------------------------------------#
  
  data_corners_app <- reactive({
    if (is.null(input$hometeam1)){
      data_corners %>%
        filter(Div %in% input$league,
               place %in% input$pitch)}
    else data_corners %>% 
      filter(HomeTeam %in% input$hometeam1,
             place %in% input$pitch)
  })
  
  #------------------------------------------------------------------------------------#
  #         reactive expression to make referee table react on selections              #
  #------------------------------------------------------------------------------------#
  
  data_ref_app <- reactive({
    if (is.null(input$referee1)){
      data_ref} 
    else data_ref %>%
        filter(Referee %in% input$referee1)
  })
  
  #------------------------------------------------------------------------------------#
  #                        add table for total full time goals                         #
  #------------------------------------------------------------------------------------#
  
  output$text_total_goals <- renderText({
    print(paste0("<span style = color:aqua><b>Match Goals<b></span>"))
  })
  
  output$table_goals_ft <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "no_games_played", "goals_per_game",  "share_over0.5FT_total_goals", 
                         "share_over1.5FT_total_goals", "share_over2.5FT_total_goals", "share_over3.5FT_total_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average goals per game", "% of games >0.5 match goals", 
                     "% of games >1.5 match goals", "% of games >2.5 match goals", "% of games >3.5 match goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1), visible = TRUE, width = "60"),
                                         list(targets = c(2), visible = TRUE, width = "60"),
                                         list(targets = c(3), visible = TRUE, width = "80"),
                                         list(targets = c(4:7), visible = TRUE, width = "110"))),
                                         rownames = FALSE) %>%
      formatRound(columns = c("goals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5FT_total_goals", "share_over1.5FT_total_goals", "share_over2.5FT_total_goals", 
                         "share_over3.5FT_total_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                        add table for full time team goals                          #
  #------------------------------------------------------------------------------------#
  
  output$text_team_goals <- renderText({
    print(paste0("<span style = color:aqua><b>Team Goals<b></span>"))
  })
  
  output$table_teamgoals_ft <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "no_games_played", "teamgoals_per_game", "share_over0.5FT_team_goals", 
                         "share_over1.5FT_team_goals", "share_over2.5FT_team_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average goals per game", "% of games >0.5 team goals", 
                     "% of games >1.5 team goals", "% of games >2.5 team goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3), visible = TRUE, width = "80"),
                                         list(targets = c(4:6), visible = TRUE, width = "110"))),
        rownames = FALSE) %>%
      formatRound(columns = c("teamgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5FT_team_goals", "share_over1.5FT_team_goals", "share_over2.5FT_team_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                      add table for first half match goals                          #
  #------------------------------------------------------------------------------------#
  
  output$text_total_firsthalf_goals <- renderText({
    print(paste0("<span style = color:aqua><b>First Half Match Goals<b></span>"))
  })
  
  output$table_goals_firsthalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "no_games_played", "firsthalfgoals_per_game", "share_over0.5HT_total_goals", 
                         "share_over1.5HT_total_goals", "share_over2.5HT_total_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average first half goals per game", "% of games >0.5 first half goals", 
                     "% of games >1.5 first half goals", "% of games >2.5 first half  goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3:6), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
      formatRound(columns = c("firsthalfgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5HT_total_goals", "share_over1.5HT_total_goals", "share_over2.5HT_total_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                      add table for first half team goals                           #
  #------------------------------------------------------------------------------------#
  
  output$text_firsthalf_teamgoals <- renderText({
    print(paste0("<span style = color:aqua><b>First Half Team Goals</b></span>"))
  })
  
  output$table_teamgoals_firsthalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "no_games_played", "firsthalfteamgoals_per_game", "share_over0.5HT_team_goals", 
                         "share_over1.5HT_team_goals", "share_over2.5HT_team_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average first half team goals per game", 
                     "% of games >0.5 first half team goals", "% of games >1.5 first half team goals", 
                     "% of games >2.5 first half team goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3:6), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
      formatRound(columns = c("firsthalfteamgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5HT_team_goals", "share_over1.5HT_team_goals", 
                         "share_over2.5HT_team_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                       add table for second half match goals                        #
  #------------------------------------------------------------------------------------#
  
  output$text_total_secondhalf_goals <- renderText({
    print(paste0("<span style = color:aqua><b>Second Half Match Goals</b></span>"))
  })
  
  output$table_goals_secondhalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "no_games_played", "secondhalfgoals_per_game", "share_over0.5secondhalf_total_goals", 
                         "share_over1.5secondhalf_total_goals", "share_over2.5secondhalf_total_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average second half goals per game", "% of games >0.5 second half goals", 
                     "% of games >1.5 second half goals", "% of games >2.5 second half goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3:6), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
      formatRound(columns = c("secondhalfgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5secondhalf_total_goals", "share_over1.5secondhalf_total_goals",
                         "share_over2.5secondhalf_total_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                      add table for second half team goals                          #
  #------------------------------------------------------------------------------------#
  
  output$text_secondhalf_teamgoals <- renderText({
    print(paste0("<span style = color:aqua><b>Second Half Team Goals</b></span>"))
  })
  
  output$table_teamgoals_secondhalf <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "no_games_played", "secondhalfteamgoals_per_game", "share_over0.5secondhalf_team_goals", 
                         "share_over1.5secondhalf_team_goals", "share_over2.5secondhalf_team_goals")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average second half team goals per game", 
                     "% of games >0.5 second half team goals", "% of games >1.5 second half team goals", 
                     "% of games >2.5 second half team goals"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3:6), visible = TRUE, width = "120"))),
        rownames = FALSE) %>%
      formatRound(columns = c("secondhalfteamgoals_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5secondhalf_team_goals", "share_over1.5secondhalf_team_goals",
                         "share_over2.5secondhalf_team_goals"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                                 add table for btts                                 #
  #------------------------------------------------------------------------------------#
  
  output$text_btts <- renderText({
    print(paste0("<span style = color:aqua><b>Share of games where both teams scored</b></span>"))
  })
  
  output$table_goals_btts <- DT::renderDataTable({
    data_goals_app()[, c("HomeTeam", "place", "no_games_played", "share_btts")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "% of games both teams scored"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 40, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3), visible = TRUE, width = "110"))), 
        rownames = FALSE) %>%
      formatPercentage(c("share_btts"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                               add table for total cards                            #
  #------------------------------------------------------------------------------------#
  
  output$table_match_cards <- DT::renderDataTable({
    data_cards_app()[, c("HomeTeam", "place", "no_games_played", "total_cards_per_game", "share_over0.5_total_cards", 
                         "share_over1.5_total_cards", "share_over2.5_total_cards", "share_over3.5_total_cards", 
                         "share_over4.5_total_cards", "share_over5.5_total_cards")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average cards per game", "% of games >0.5 cards", 
                     "% of games >1.5 cards", "% of games >2.5 cards", 
                     "% of games >3.5 cards", "% of games >4.5 cards", "% of games >5.5 cards"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 500, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3:9), visible = TRUE, width = "130"))), 
        rownames = FALSE) %>%
      formatRound(columns = c("total_cards_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5_total_cards", "share_over1.5_total_cards", "share_over2.5_total_cards", 
                         "share_over3.5_total_cards", "share_over4.5_total_cards", "share_over5.5_total_cards"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                               add table for total cards                            #
  #------------------------------------------------------------------------------------#
  
 output$table_team_cards <- DT::renderDataTable({
    data_cards_app()[, c("HomeTeam", "place", "no_games_played", "team_cards_per_game", "share_over0.5_team_cards", 
                         "share_over1.5_team_cards", 
                         "share_over2.5_team_cards", "share_over3.5_team_cards")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Team", "Playing site", "Games played", "Average team cards per game", "% of games >0.5 team cards", 
                     "% of games >1.5 team cards", 
                     "% of games >2.5 team cards", "% of games >3.5 team cards"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 500, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:2), visible = TRUE, width = "60"),
                                         list(targets = c(3:7), visible = TRUE, width = "130"))), 
        rownames = FALSE) %>%
      formatRound(columns = c("team_cards_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5_team_cards", "share_over1.5_team_cards", "share_over2.5_team_cards", 
                         "share_over3.5_team_cards"), 0)
  })
 
 #------------------------------------------------------------------------------------#
 #                              add table for total corners                           #
 #------------------------------------------------------------------------------------#
 
 output$table_match_corners <- DT::renderDataTable({
   data_corners_app()[, c("HomeTeam", "place", "no_games_played", "total_corners_per_game", "share_over0.5_total_corners", 
                        "share_over1.5_total_corners", "share_over2.5_total_corners", "share_over3.5_total_corners", 
                        "share_over4.5_total_corners", "share_over5.5_total_corners", "share_over6.5_total_corners",
                        "share_over7.5_total_corners", "share_over8.5_total_corners", "share_over9.5_total_corners",
                        "share_over10.5_total_corners", "share_over11.5_total_corners", "share_over12.5_total_corners")] %>%
     datatable(
       style = "bootstrap",
       colnames = c("Team", "Playing site", "Games played", "Average corners per game", "% of games >0.5 corners", 
                    "% of games >1.5 corners", "% of games >2.5 corners", "% of games >3.5 corners", 
                    "% of games >4.5 corners", "% of games >5.5 corners", "% of games >6.5 corners", 
                    "% of games >7.5 corners", "% of games >8.5 corners", "% of games >9.5 corners",
                    "% of games >10.5 corners", "% of games >11.5 corners", "% of games >12.5 corners"),
       options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 500, 
                      columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                        list(targets = c(1:2), visible = TRUE, width = "60"),
                                        list(targets = c(3:16), visible = TRUE, width = "130"))), 
       rownames = FALSE) %>%
     formatRound(columns = c("total_corners_per_game"), 2) %>% 
     formatPercentage(c("share_over0.5_total_corners", "share_over1.5_total_corners", "share_over2.5_total_corners", 
                        "share_over3.5_total_corners", "share_over4.5_total_corners", "share_over5.5_total_corners", 
                        "share_over6.5_total_corners", "share_over7.5_total_corners", "share_over8.5_total_corners", 
                        "share_over9.5_total_corners", "share_over10.5_total_corners", "share_over11.5_total_corners", 
                        "share_over12.5_total_corners"), 0)
 })
 
 #------------------------------------------------------------------------------------#
 #                              add table for team corners                            #
 #------------------------------------------------------------------------------------#
 
 output$table_team_corners <- DT::renderDataTable({
   data_corners_app()[, c("HomeTeam", "place", "no_games_played", "team_corners_per_game", "share_over0.5_team_corners", 
                        "share_over1.5_team_corners", "share_over2.5_team_corners", "share_over3.5_team_corners",
                        "share_over4.5_team_corners", "share_over5.5_team_corners","share_over6.5_team_corners",
                        "share_over7.5_team_corners", "share_over8.5_team_corners")] %>%
     datatable(
       style = "bootstrap",
       colnames = c("Team", "Playing site", "Games played", "Average team corners per game", "% of games >0.5 team corners", 
                    "% of games >1.5 team corners", "% of games >2.5 team corners", "% of games >3.5 team corners",
                    "% of games >4.5 team corners", "% of games >5.5 team corners", "% of games >6.5 team corners",
                    "% of games >7.5 team corners", "% of games >8.5 team corners"),
       options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 500, 
                      columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                        list(targets = c(1:2), visible = TRUE, width = "60"),
                                        list(targets = c(3:12), visible = TRUE, width = "130"))), 
       rownames = FALSE) %>%
     formatRound(columns = c("team_corners_per_game"), 2) %>% 
     formatPercentage(c("team_corners_per_game", "share_over0.5_team_corners", 
                        "share_over1.5_team_corners", "share_over2.5_team_corners", "share_over3.5_team_corners",
                        "share_over4.5_team_corners", "share_over5.5_team_corners","share_over6.5_team_corners",
                        "share_over7.5_team_corners", "share_over8.5_team_corners"), 0)
 })
  
  #------------------------------------------------------------------------------------#
  #                               add table for referees 1                             #
  #------------------------------------------------------------------------------------#
  
  output$text_ref1 <- renderText({
    print(paste0("<span style = color:aqua><b>Cards per game and share of games with red cards</b></span>"))
  })
  
  output$table_referees1 <- DT::renderDataTable({
    data_ref_app()[, c("Referee", "no_games_refereed", "total_cards_per_game", "yellow_cards_per_game", 
                       "red_cards_per_game", "share_over0.5_red_cards")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Referee", "No of games", "Average total cards per game", "Average yellow cards per game", 
                     "Average red cards per game", "% of games with >0.5 red cards"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 50, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:4), visible = TRUE, width = "130"))), 
        rownames = FALSE) %>%
      formatRound(columns = c("total_cards_per_game", "yellow_cards_per_game", "red_cards_per_game"), 2) %>% 
      formatPercentage(c("share_over0.5_red_cards"), 0)
  })
  
  #------------------------------------------------------------------------------------#
  #                               add table for referees 2                             #
  #------------------------------------------------------------------------------------#
  
  output$text_ref2 <- renderText({
    print(paste0("<span style = color:aqua><b>Share of games with yellow cards</b></span>"))
  })
  
  output$table_referees2 <- DT::renderDataTable({
    data_ref_app()[, c("Referee", "share_over0.5_yellow_cards", "share_over1.5_yellow_cards", 
                       "share_over2.5_yellow_cards", "share_over3.5_yellow_cards", 
                       "share_over4.5_yellow_cards", "share_over5.5_yellow_cards")] %>%
      datatable(
        style = "bootstrap",
        colnames = c("Referee", "% of games with >0.5 yellow cards", "% of games with >1.5 yellow cards", 
                     "% of games with >2.5 yellow cards", "% of games with >3.5 yellow cards",
                     "% of games with >4.5 yellow cards", "% of games with >5.5 yellow cards"),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, pageLength = 50, 
                       columnDefs = list(list(targets = c(0), visible = TRUE, width = "100"),
                                         list(targets = c(1:6), visible = TRUE, width = "130"))), 
        rownames = FALSE) %>%
      formatPercentage(c("share_over0.5_yellow_cards", "share_over1.5_yellow_cards", 
                         "share_over2.5_yellow_cards", "share_over3.5_yellow_cards", 
                         "share_over4.5_yellow_cards", "share_over5.5_yellow_cards"), 0)
  })
  
}

shinyApp(ui = ui, server = server)