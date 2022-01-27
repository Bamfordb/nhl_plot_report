library(nhlapi)
library(tidyverse)
library(lubridate)
library(ggforce)
library(ggthemes)
library(scatterpie)
library(wesanderson) # Color Palette
library(janitor)
library(jsonlite)


source('nhl_half_rink_plot_function.R') # Imports half rink plotting function.


# *******Requires nhl_half-rink-plot.r function to visualize ice surface. *******
# --------------Thanks mrbilltran for the original nhl_rink_plot-----------------

# Retrieve data-----------------------------------------------------------------
# Table of team names and ids.
all_teams_info <- nhl_teams() %>% as_tibble()

all_teams_ids <- all_teams_info %>% 
  select(name, id)

# Get information for last game from NHL API
tbl_team_id = 14  # 14 for TBL
nhl_teams(teamIds =tbl_team_id)
nhl_teams_schedule_next(teamIds = tbl_team_id)
nhl_teams_shedule_previous(teamIds = tbl_team_id)

# Get all 2021-22 seasons gameIds
all_games_schedule_info <- nhl_schedule_date_range(startDate= '2021-10-12', # First day of 2021-22 season
                                          endDate = '2022-04-29', # Last day of 2021-22 season
                                          teamIds = tbl_team_id)

# GET request for nhl_schdeule_date_range function: 
# GET https://statsapi.web.nhl.com/api/v1/schedule?teamId=14&startDate=2021-10-12&endDate=2022-04-29

# Pull data from API calls------------------------------------------------------
# GameIds
game_number <- 1:82
game_id_list <- data.frame(row.names = 'game.id')

for(i in game_number) {
  
  print(all_games_schedule_info[[1]]$dates$games[[i]]$gamePk)

  
  game_id_list[i] <- all_games_schedule_info[[1]]$dates$games[[i]]$gamePk
  }

game_id_list <-   
game_id_list %>% 
  t() # Transpose to one column
  
rownames(game_id_list) <- NULL # Reset indexs


# Obtain Home or Away status from all_games_schedule_info-----------------------

# Home Team
home_teams <- tibble(teams.home.team.name = '')
for(i in game_number) {
  
  print(all_games_schedule_info[[1]]$dates$games[[i]]$teams.home.team.name)
  
  home_teams[i] <- all_games_schedule_info[[1]]$dates$games[[i]]$teams.home.team.name
  }

home_teams <- home_teams %>%  
  t()

rownames(home_teams) <- NULL

home_teams <- as_tibble(home_teams) %>% 
  rename(teams.home.team.name = V1)


# Away Team
away_teams <- tibble(teams.home.away.name = '')

for(i in game_number) {
  
  #print(all_games_schedule_info[[1]]$dates$games[[i]]$teams.away.team.name)
  
  away_teams[i] <- all_games_schedule_info[[1]]$dates$games[[i]]$teams.away.team.name
  }

away_teams <- away_teams %>% 
  t()

rownames(away_teams) <- NULL

away_teams <- as_tibble(away_teams) %>% 
  rename(teams.away.team.name = V1)


# Run gameIds through NHL API for a single games events. ----------------------
# ***Retrieves entire seasons game data from LAST time ran*** 

all_games_play_data <- nhl_games_feed(gameIds = game_id_list) # Last Ran 01/26/22


# Get Request for all games in Tampa's 2021-22 season so far.

url_base <- c('https://statsapi.web.nhl.com/api/v1/game/')
url_end <- c('/feed/live')

# ***Need to remove leading and trailing ';' as collapse does to all elements. ***
tbl_game_play_request <- paste0(url_base, 
                                game_id_list, 
                                url_end,
                                sep = '') %>% 
                        as.character() %>% 
                         as_tibble()
                          

# ***All code after here applies to ONE game ***--------------------------------------

game_1_play_data <- all_games_play_data[[1]][["liveData"]][["plays"]][["allPlays"]]
#game_24_play_data <- all_games_play_data[[24]][["liveData"]][["plays"]][["allPlays"]]

# Data cleaning and manipulation. ------------------------------------------------
shot_order <- c('GOAL', 'MISSED_SHOT', 'BLOCKED_SHOT', "SHOT")

game_1_play_data$about.period <- as.factor(game_1_play_data_filtered$about.period)
game_1_play_data$about.dateTime <- ymd_hms(game_1_play_data$about.dateTime)
game_1_play_data$coordinates.x <- abs(game_1_play_data$coordinates.x)
game_1_play_data$result.eventTypeId <- factor(as.character(game_1_play_data$result.eventTypeId), levels = shot_order)
game_1_play_data$team.home <- pluck(all_games_play_data, 1, 'gameData', 'teams', 'home', 'name')
game_1_play_data$team.away <- pluck(all_games_play_data, 1, 'gameData', 'teams', 'away', 'name')



# Flip x and/or y coordinates depending on home or away status 
# Home shoots on right side of ice every odd period, Away shoots on right side of ice every even period,
game_1_play_data <- 
game_1_play_data %>% 
  #select(about.period, result.eventTypeId, coordinates.x, coordinates.y, result.description, team.home, team.away, team.name) %>% 
  #filter(result.eventTypeId == 'GOAL') %>% 
  mutate(coordinates.y = 
    case_when(team.name == team.home & about.period %% 2 == 0 ~ (-1 * coordinates.y), # Home
              team.name == team.away & about.period %% 2 == 1 ~ (-1 * coordinates.y), # Away
            TRUE ~ coordinates.y
            ),
    # print(coordinates.y)
        )

# Filter down data to more managable size with relevent information.------------          
game_1_play_data_filtered <- 
  game_1_play_data %>% 
  select(result.description,
         result.event,
         result.eventTypeId,
         result.secondaryType,
         result.strength.name,
         coordinates.x, 
         coordinates.y,
         team.id,
         team.name,
         about.period,
         about.periodType,
         about.periodTimeRemaining,
         about.goals.away,
         about.goals.home)


shots_and_goals <- 
  game_1_play_data_filtered %>% 
  select(result.description,
         result.eventTypeId,
         result.secondaryType,
         coordinates.x, 
         coordinates.y,
         about.period,
         team.id,
         team.name) %>% 
  filter(result.eventTypeId == 'SHOT' | result.eventTypeId == 'GOAL')

all_shots_and_goals <- 
  game_1_play_data_filtered %>% 
  filter(result.eventTypeId == 'SHOT' | 
           result.eventTypeId == 'GOAL' |
           result.eventTypeId == 'BLOCKED_SHOT' |
           result.eventTypeId == 'MISSED_SHOT')

write_csv(game_1_play_data, 'game_01_play_data_2022.csv')

# Begin Graphing----------------------------------------------------------------

# Set global graph elements.
game_date <- as_date(game_1_play_data$about.dateTime[1])
color_cols <- c('Red', 'Blue')
fill_cols <- c('Green', 'Yellow', 'Red', 'Grey')
shape_values <- c(21, 24, 22, 23)


# All shots and goals for both teams. -----------------------------------------
game_1_all_shots_and_goals_plot <- 
nhl_half_rink_plot() +
  geom_point(data = all_shots_and_goals,
             size = 4, 
             stroke = 4, 
             aes(x = coordinates.x, 
                 y = coordinates.y, 
                 color = team.name, 
                 shape = about.period,
                  
             )
            ) +
  geom_point(data = all_shots_and_goals,
             shape = 21, 
             size = 5,
             stroke = 2,
             aes(x = coordinates.x,
                 y= coordinates.y,
                 fill =  result.eventTypeId
             )
            ) +
  labs(title = paste(
              as.character(na.omit(unique(game_1_play_data$team.away))), 
              'vs', 
              as.character(na.omit(unique(game_1_play_data$team.home)))
                    ),
       subtitle = game_date
                    
       ) +
  
  scale_color_manual(values = color_cols) +
  scale_fill_manual(values = fill_cols) +
  scale_shape_manual(values = shape_values) +
  
  coord_flip() +
  theme_void()


game_1_all_shots_and_goals_plot

# Home team period 1 shots and goals. -----------------------------------------
home_period_1_shots_and_goals <- 
game_1_play_data %>% 
  filter(team.name == team.home,
         about.period == 1,
           result.eventTypeId == 'SHOT' | 
           result.eventTypeId == 'GOAL' |
           result.eventTypeId == 'BLOCKED_SHOT' |
           result.eventTypeId == 'MISSED_SHOT')

home_period_1_shots_and_goals$result.eventTypeId <- factor(as.character(home_period_1_shots_and_goals$result.eventTypeId), levels = shot_order)

home_period_1_shots_and_goals_plot <- 
nhl_half_rink_plot() +
  
  geom_point(data = home_period_1_shots_and_goals,
             shape = 21, 
             size = 5,
             stroke = 2,
        aes(x = coordinates.x,
            y= coordinates.y,
            fill = result.eventTypeId
            #color = row.names(home_period_1_shots_and_goals),
              ) 
            ) +
  
  geom_text(data = home_period_1_shots_and_goals,
            size = 3,
            fontface = 'bold',
            
    aes(x = coordinates.x,
        y = coordinates.y,
        label = row.names(home_period_1_shots_and_goals))) +
  
  labs(title = paste(
                    as.character(na.omit(unique(game_1_play_data$team.home))),
    
                    'Period 1'
                    ),
      subtitle = game_date
  
      ) +
  
  scale_color_manual(values = color_cols) +
  scale_fill_manual(values = fill_cols) +
  scale_shape_manual(values = shape_values) +
  
  
  coord_flip() +
  theme_void()

home_period_1_shots_and_goals_plot

# Faceoff win / loss graph ---------------------------------------------------

game_1_faceoffs <-              
game_1_play_data %>% 
  filter(result.event == 'Faceoff') %>% 
  group_by(coordinates.x, coordinates.y, team.name) %>% 
  count(team.name) %>% 
  ungroup()

game_1_faceoffs_wide <-
  game_1_faceoffs %>% 
  pivot_wider(names_from = team.name, values_from = n)


game_1_faceoffs_wins_plot <- 
nhl_half_rink_plot() +
  coord_flip() +
  geom_scatterpie(
    data = game_1_faceoffs_wide,
    cols = c('Pittsburgh Penguins', 'Tampa Bay Lightning'),
    
    aes(x = coordinates.x,
        y = coordinates.y,
        r = 6.0
    )
  ) +
  labs(
    title = paste(
    as.character(na.omit(unique(game_1_play_data$team.away))), 
    'vs', 
    as.character(na.omit(unique(game_1_play_data$team.home)))
                    ),
  
    subtitle = 'Faceoff Win/Loss by Team'
      ) + 
      scale_color_manual(values = color_cols) +
  theme_void()

game_1_faceoffs_wins_plot

# Takeaways / Giveaways total and locations ------------------------------------
game_1_aways <- game_1_play_data %>% 
  filter(result.event == 'Takeaway' |
          result.event == 'Giveaway')

aways_fill <- c('Pink', 'Purple')

takeaway_giveaway_plot <- 
nhl_half_rink_plot() +
  geom_point(data = game_1_aways,
             size = 4, 
             stroke = 4, 
             aes(x = coordinates.x, 
                 y = coordinates.y, 
                 color = team.name, 
                 shape = about.period,
                 
             )
  ) +
  geom_point(data = game_1_aways,
             shape = 21, 
             size = 5,
             stroke = 2,
             aes(x = coordinates.x,
                 y= coordinates.y,
                 fill =  result.event
             )
  ) +
  labs(title = paste(
    as.character(na.omit(unique(game_1_play_data$team.away))), 
    'vs', 
    as.character(na.omit(unique(game_1_play_data$team.home)))
  ),
  subtitle = 'Takeaways and Giveaways'
  
  ) +
  
  scale_color_manual(values = color_cols) +
  scale_fill_manual(values = aways_fill) +
  scale_shape_manual(values = shape_values) +
  
  coord_flip() +
  theme_void()

takeaway_giveaway_plot

# All hit locations ------------------------------------------------------------


game_1_hits <- 
  game_1_play_data %>% 
  filter(result.event == 'Hit')

all_hits_plot <- 
nhl_half_rink_plot() +
  geom_point(data = game_1_hits,
             size = 5, 
             #stroke = 4, 
            aes(x = coordinates.x, 
                 y = coordinates.y, 
                 fill = team.name, 
                 shape = about.period,
                 
             )
  ) +
  labs(title = paste(
    as.character(na.omit(unique(game_1_play_data$team.away))), 
    'vs', 
    as.character(na.omit(unique(game_1_play_data$team.home)))
  ),
  subtitle = 'Hits'
  
  ) +
  
  scale_color_manual(values = color_cols) +
  scale_fill_manual(values = color_cols) +
  scale_shape_manual(values = shape_values) +
  
  coord_flip() +
  theme_void()

all_hits_plot

# All penalty locations and types ----------------------------------------------

game_1_penalties <- 
  game_1_play_data %>% 
  filter(result.event == 'Penalty')


penalty_location_plot <- 
nhl_half_rink_plot() +
  geom_point(data = game_1_penalties,
             size = 3, 
             stroke = 3, 
             shape = 16,
             aes(x = coordinates.x, 
                 y = coordinates.y, 
                 color = team.name, 
                 shape = about.period,
                 
             )
  ) +
  labs(title = paste(
    as.character(na.omit(unique(game_1_play_data$team.away))), 
    'vs', 
    as.character(na.omit(unique(game_1_play_data$team.home)))
  ),
  subtitle = 'Penalties'
  
  ) +
  
  geom_text(data = game_1_penalties,
             vjust = 0,
             nudge_x = -2.5,
            
            aes(x = coordinates.x,
                y = coordinates.y,
                label = result.secondaryType)
            ) +
  
  scale_color_manual(values = color_cols) +
  scale_shape_manual(values = shape_values) +
  
  coord_flip() +
  theme_void()

penalty_location_plot


# Shot heatmap -----------------------------------------------------------------
tampa_shots <- 
all_shots_and_goals %>% filter(team.name == 'Tampa Bay Lightning')
# Hex bin map
nhl_half_rink_plot() +
  geom_hex(data = tampa_shots,
           bins = 15,
           aes(x = coordinates.x,
               y= coordinates.y),
           #show.legend =  FALSE,

  ) +
  theme_void() 
  
# Density plot
nhl_half_rink_plot() +
  geom_density_2d_filled(data = all_shots_and_goals,
           bins = 15,
           
           aes(x = coordinates.x,
               y= coordinates.y)
  ) 
 

# Some console print summary information tables. -------------------------------

home_team <- all_games_play_data[[1]][["gameData"]][["teams"]][["home"]][["name"]]
away_team <- all_games_play_data[[1]][["gameData"]][["teams"]][["away"]][["name"]]

shot_types <- c('Wrist Shot', 'Slap Shot', 'Backhand', 'Tip-In', 'Snap Shot', 'Wrap-around', 'Deflected')
# Number of shots and their type breakdown by team
game_1_play_data %>% 
  tabyl(team.name, result.secondaryType, show_na = FALSE) %>% 
  adorn_totals('col') %>% 
  knitr::kable()


# Each unique game stoppage of this game.
game_1_play_data %>% 
  select(result.event, result.description) %>% 
  filter(result.event == 'Stoppage') %>% 
  unique()


# Number of game stoppages and their totals.
game_1_play_data %>% 
  select(result.event, result.description, team.name) %>% 
  filter(result.event == 'Stoppage') %>% 
  count(result.description) %>% 
  adorn_totals('row') %>% 
  knitr::kable()

# Faceoffs by player
# 

# game_1_play_data %>% 
#   pluck('player.fullName')
#     
# 
# game_1_play_data %>% 
#   filter(result.event == 'Faceoff') %>% 

    
# Game Linescore Summary Table

game_1_linescore <- nhl_games_linescore(all_games_play_data[[1]]$gamePk)
    game_1_linescore %>% pluck(1, 'periods') %>% knitr::kable()
  

# Game final summary metrics   
# Home    
all_games_play_data[[1]]$liveData$boxscore$teams$home$teamStats %>% 
  bind_rows()

# Away
all_games_play_data[[1]]$liveData$boxscore$teams$away$teamStats %>% 
  bind_rows()



