

ui <- page_navbar(
  title = "AWS Global Power Rankings",
  theme = bs_theme(bg = "#454040", fg = "#FBF7F7"),
  navset_card_pill(
    nav_panel("Rankings",
              layout_sidebar(
                textOutput("instructions"),
                DT::DTOutput("rankings"),
                
                fillable = TRUE,
                sidebar = 
                  sidebar(selectizeInput("mode", "Choose Ranking Method", choices = c("Global","Tournament","Teams")),
                          selectizeInput("team_name", "Teams", choices = sort(display_rank$"Team Name"), 
                                         multiple = TRUE, options = list(placeholder = "Please Select Teams")),
                          selectizeInput("tournament", "Tournament", choices = tournament_rankings_display$search,
                                         multiple = FALSE, options = list(placeholder = "Please Select a Tournament"))
                  )
              )
    ),
    nav_panel("About",
              textOutput("about")),
    nav_spacer(),
    nav_item(
      tags$a(icon("github"), "Source", href = "https://github.com/JayCampanell/global-power-rankings"),
      target = '_blank'
    ))
  
)
# Define UI for application that draws a histogram

# Define server logic required to draw a histogram
server <- function(input, output) {

# Disable Tournament and Teams Until The Mode is Activated
  
  observeEvent(input$mode, {
    if(input$mode == 'Global') {
      shinyjs::disable('team_name') 
    } else if(input$mode == 'Teams'){
      shinyjs::enable('team_name')
    }
  }, ignoreNULL = T)  
    
  
  userInstructions <- reactive({
    if(input$mode == "Teams" & length(input$team_name) == 0){
      "Please Select Teams"
    }
  })
  
  
# Create Data Frame That Filters Based on Inputted Teams  
  
  team_data <- reactive({
    req(input$team_name)
    
    display_rank %>%
      filter(`Team Name` %in% input$team_name) %>%
      arrange(desc(`Rank Score`)) %>%
      mutate(Rank = row_number())
  })
  
  
# Create Reactive Dataframe for Tournaments 
  
  tournament_data <- reactive({
    req(input$tournament)
    
    tournament_rankings_display %>%
      filter(search == input$tournament) %>%
      select(Rank, Team, Tournament, League, `Rank Score`) %>%
      mutate(Rank = row_number())
  
  })
  
# Render Datables   
  
    output$rankings <- renderDT(
      
      if(input$mode == "Global"){     # Check if User is Doing Global Rankings
        datatable(display_rank, filter = 'top')
      } else if(input$mode == "Teams"){      # Check if User is doing Teams Ranking
      datatable(team_data(), filter = 'top') 
      } else if(input$mode == "Tournament"){
        datatable(tournament_data(), filter = 'top')
      }
  
      )
    
     userInstructionsTeam <- reactive({
       
       if_else(length(input$team_name) == 0 & input$mode == "Teams",
              "Please Select Teams",
              ""
       )
       })

     userInstructionsTourn <- reactive({
       
       if_else(input$tournament == "" & input$mode == "Tournament",
               "Please Select Tournament",
               ""
       )
     })
    
    output$instructions <- renderText(
        if(input$mode == "Tournament"){
          userInstructionsTourn()
        } else {
          userInstructionsTeam()
        }
    )
    output$about <- renderText(
      "We used both the fixture and the games data.
      The global rankings use the most recent regular season for each league.   
      The global ranking are created from a multivariate statistical method 
      which use total gold, inhib, dragon, tower, and champion kills from the games data.  
      The source games data is filtered at the 3 & 97 percentile levels to exclude potential influencing outliers.  
      The data is averaged across all games in the season and fed into the multivariate method.  
      The multivariate method produces a ranking score for each team.  
      The scores are weighted based on the number of teams the league sends to Worlds."
    )
    
}

shinyApp(ui, server, onStart = "global.R")
