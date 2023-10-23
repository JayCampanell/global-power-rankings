library(jsonlite)
library(httr)
library(dplyr)
library(tibble)
library(repurrrsive)
library(tidyr)
library(arrow)
library(tidyverse)

riot_data <- function (url) {
  
  url <- url
  
  response <- GET(url)
  gzipped_content <- content(response, as = "raw")
  decompressed_content <- gzcon(rawConnection(gzipped_content))
  
  # Parse JSON into a data frame
  df <- fromJSON(decompressed_content)
  return(df)
  
}


riot_json <- function(url) {
  
  urlJson <- url
  
  responseJson <- GET(urlJSON)
  
  gzipped_content <- content(response, as = "raw")
  decompressed_content <- gzcon(rawConnection(gzipped_content))
  
}



#---------------   Mapping file   -------------------------------#
mapping_url <- "https://power-rankings-dataset-gprhack.s3.us-west-2.amazonaws.com/esports-data/mapping_data.json.gz"
mapping_df <- riot_data(mapping_url) %>%
              rownames_to_column()
             
#----------------------------------------------------------------#
#   Remove NAs from mapping file?                                #
#----------------------------------------------------------------#
            
            
#--------------------------------------------------------------------------------#
#  Read fixture files as data frames                                             #
#--------------------------------------------------------------------------------#

#-------------------------   Read teams   --------------------------------------#
teams_url <- "https://power-rankings-dataset-gprhack.s3.us-west-2.amazonaws.com/esports-data/teams.json.gz"
teams_df <- riot_data(teams_url)

#----------------   Read leagues  -----------------------------#
league_url <- "https://power-rankings-dataset-gprhack.s3.us-west-2.amazonaws.com/esports-data/leagues.json.gz"
leagues_df <- riot_data(league_url)

#---------------  Read tournaments   --------------------------------#
tournament_url <- "https://power-rankings-dataset-gprhack.s3.us-west-2.amazonaws.com/esports-data/tournaments.json.gz"
tournament_df <- riot_data(tournament_url)

#--------------   Read players  -----------------------------#
players_url <- "https://power-rankings-dataset-gprhack.s3.us-west-2.amazonaws.com/esports-data/players.json.gz"
players_df <- riot_data(players_url)


# unnest worlds 2020 and msi 2023 dataframe

stages_df <- tournament_df %>%
  filter(name %in% c("worlds_2020", "MSI 2023")) %>%
  select(stages) %>%
  unlist() %>%
  enframe() 

# get unique team ids from worlds 2020 and msi 2023

team_tournament_ids <- stages_df %>%
  filter(name %in% c("stages.sections.matches.teams.id1", 
                      "stages.sections.matches.teams.id2")) %>%
  distinct(value) %>%
  rename(team_id = value)

# Get names of teams from ids

team_tournament_names <- team_tournament_ids %>%
  left_join(teams_df, by = "team_id") %>%
  distinct() 
  
# Get the region associated with every tournament 

regions_df <- leagues_df %>%
  unnest(tournaments, name_sep = "tournament") %>%
  select(c(id, name, region, id1)) %>%
  rename("tournamentid" = 'id1')


# Read in all parquet files from S3 Directory

aws_bucket <- arrow::s3_bucket('chickenjoy-power-rankings', access_key = "AKIAYL74JU2TYVDQ3XHV", 
                               secret_key = "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")
files <- aws_bucket$ls('parquet/athenaTest2/')
aws_keys <- map(files, aws_bucket$path)
games_data <- map_dfr(aws_keys, arrow::read_parquet, .progress = TRUE)

# Remove duplicates in teams data frame

teams_df <- teams_df %>%
  distinct()

# Join the game data with the team data

games_teams <- games_data %>%
  left_join(team_ids, by = c('platformgameid'='platformGameId')) %>%
  left_join(teams_df, by = c('100' = 'team_id'), relationship = "many-to-one") %>%
  rename("blueName" = "name", 'blueAcronym' = 'acronym', 'blueSlug' = 'slug') %>%
  left_join(teams_df, by = c('200' = 'team_id'), relationship = "many-to-one") %>%
  rename("redName" = "name", 'redAcronym' = 'acronym', 'redSlug' = 'slug')


# rename columns to side_variable pattern

red_col <- grep("^(?:red)", names(games_teams))
names(games_teams)[red_col] <- str_replace(names(games_teams)[red_col], pattern = "(.{3})(.*)", replacement = "\\1_\\2")

blue_col <- grep("^(?:blue)", names(games_teams))
names(games_teams)[blue_col] <- str_replace(names(games_teams)[blue_col], pattern = "(.{4})(.*)", replacement = "\\1_\\2")

# pivot data longer for all sides

games_long <- games_teams %>%
  select(-c(blue_teamid, red_teamid)) %>%
  rename(blue_teamid = "100",
         red_teamid = "200") %>%
  pivot_longer(!c(platformgameid, eventtime, stageid, gamename, gametime, rowname, esportsGameId),
               names_to = c("set", ".value"),
               names_pattern = "(.+)_(.+)"
  )


# join ranking games data with the data from games 

ranking_games <- readRDS("Ranked Games.rds")


ranking_data <- ranking_games %>%
  left_join(games_long, by = c("platformGameId" = "platformgameid", "team_id" = "teamid") )






# Read in Final Rankings 

final_rank <- read_csv("https://raw.githubusercontent.com/JayCampanell/global-power-rankings/main/balanced%20ranks.csv", col_types = 'c') 

final_rank_clean <- final_rank %>%
  mutate_all(format, scientific = F)

# Prepare final rankings to be displayed

team_names <- teams_df %>%
  select(-slug)

display_rank <- final_rank %>%
  left_join(team_names, by = "team_id") %>%
  filter(!is.na(name)) %>%
  arrange(desc(rank)) %>%
  mutate(rank2 = 1:nrow(display_rank)) %>%
  mutate(league_slug = toupper(league_slug)) %>%
  select(rank2, name, acronym, slug, rank, league_slug, team_id) %>%
  rename(
    "Team Name" = name,
    "Acronym" = acronym,
    "Slug" = slug,
    "Rank Score" = rank,
    "Team ID" = team_id,
    "Rank" = rank2,
    "Region" = league_slug
    )


# Read in All Games and Join with Games data 

allGames <- read_rds("https://github.com/JayCampanell/global-power-rankings/raw/main/All%20Games.rds")

allGamesData <- allGames %>%
  left_join(games_long ,by = c("platformGameId" = "platformgameid", "team_id" = "teamid"))

write_rds(allGamesData, "allgamesdata.rds")  

# 



tourn_games <- function(tourn_id) {
  
  stages_df <- tournament_df %>%
    filter(id %in% c(tourn_id)) %>%
    select(stages) %>%
    unlist() %>%
    enframe() 
  
  team_tournament_ids <- stages_df %>%
    filter(name %in% c("stages.sections.matches.teams.id1", 
                       "stages.sections.matches.teams.id2")) %>%
    distinct(value) %>%
    rename(team_id = value)
  
  
tourn_date <- tournament_df %>%
  select(id, leagueId, startDate)

tourn_name <- tournament_df %>%
  select(id, name, slug)

  # Get names of teams from ids and add the tournament info
  
  team_tournament_names <- team_tournament_ids %>%
    left_join(teams_df, by = "team_id") %>%
    distinct() %>%
    mutate(id = tourn_id) %>%
    left_join(tourn_date, by = "id")
  
startDate <- ymd(unique(team_tournament_names$startDate))
print(startDate) 

 tourn_game_data <- games_long %>%
    filter(teamid %in% team_tournament_names$team_id) %>%
    mutate(date = as_date(ymd_hms(eventtime))) %>%
   mutate(interval = interval(startDate - days(50), startDate)) %>%
   mutate(inRange = date %within% interval) %>%
    filter(inRange == TRUE) %>%
    group_by(teamid) %>%
    arrange(date, .by_group = TRUE) %>%
    slice(tail(row_number(), 15)) %>%
   mutate(tourn_id = tourn_id)  %>%
   left_join(tourn_name, by = c("tourn_id" = "id"))
   

   return(tourn_game_data)
  
}

# Apply Function to all tournament ids to get raw data to be ranked 
tourn_ranking <- lapply(tournament_df$id, tourn_games) %>%
  bind_rows()


# Read in Rankings for tournament 
tournament_rankings <- read_csv("https://raw.githubusercontent.com/JayCampanell/global-power-rankings/main/tournament_ranking_final.csv", col_types = cols(.default = "c"))

# Clean up to Display on shiny app

tournament_rankings_display <- tournament_rankings %>%
  mutate(search = paste0(tournament, " (", tourn_id, ")")) %>%
  rename(
    "Tournament" = 'tournament',
    "Team" = "team",
    "Rank Score" = "rank",
    "League" = "league"
  ) %>%
  mutate(Rank = row_number())


