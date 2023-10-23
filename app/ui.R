source("global.R")


ui <- page_navbar(
  title = "AWS Global Power Rankings",
  theme = bs_theme(bg = "#454040", fg = "#FBF7F7"),
  fillable = TRUE,
  navset_card_pill(
   nav_panel("Rankings",
                layout_sidebar(
                fillable = TRUE,
                card(
                textOutput("instructions"),
                div(
                DT::DTOutput("rankings")),
                fill = TRUE
                ),
                sidebar = 
                  sidebar(selectizeInput("mode", "Choose Ranking Method", choices = c("Global","Tournament","Teams")),
                          selectizeInput("team_name", "Teams", choices = sort(display_rank$search), 
                                         multiple = TRUE, options = list(placeholder = "Please Select Teams")),
                          selectizeInput("tournament", "Tournament", choices = tournament_rankings_display$search,
                                         multiple = FALSE, options = list(placeholder = "Please Select a Tournament"))
      ),
              )
    ),
    nav_panel("About",
              textOutput("about")),
    nav_spacer(),
    nav_item(
      tags$a(icon("github"), "Source", href = "https://github.com/JayCampanell/global-power-rankings"),
      target = '_blank'
    )
    )
  
)

