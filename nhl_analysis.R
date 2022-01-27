library(tidyverse)
library (lubridate)

player_data <- read.csv('player_info.csv')
game_plays <- read.csv('game_plays.csv')
game_plays$dateTime <- ymd_hms(game_plays$dateTime)
game_plays$year <- as.numeric(substring(game_plays$game_id, 1, 4))

game_goals <- read.csv('game_goals.csv')
game_plays_players <- read.csv('game_plays_players.csv')
plays_players <- left_join(game_plays_players, game_plays, by = 'play_id')
full_game_data <- left_join(plays_players, player_data, by = 'player_id')

#Change Event type Goal to Assist based on playerType
full_game_data <- full_game_data %>% 
                      mutate(event = case_when(
                      playerType == 'Assist' ~ 'Assist',
                      TRUE  ~ as.character(event)))

# All Goals from 00-19
Goals <- full_game_data %>% 
            filter(playerType == 'Scorer') %>% 
            distinct(play_id, .keep_all = TRUE) %>% 
            select(play_id,firstName, lastName, primaryPosition, birthDate, year, x, y)

#Total Goals in a Season(00-19)
Goals %>% 
  ggplot(aes(year)) +
  geom_bar()

#Move coordinate values to non-negative. To View on one-side of the Rink.
absx <- abs(Goals$x)
Goals$x <- absx


nhl_half_rink_plot()+
points(Goals$x, Goals$y)+ 
  coord_flip()


#Add player age column based on last game of 2020
age_end_date <- ymd_hms('2020-09-29 00:00:00')
player_data$birthDate <- ymd_hms(player_data$birthDate)
int <- interval(player_data$birthDate, age_end_date)

age <- round(time_length(int, 'year'), 0)
player_data$age <- age


player_data %>% 
  ggplot() +
  geom_bar(aes(age))

player_data %>% 
  ggplot() +
  geom_density(aes(weight)) +
  facet_wrap(~primaryPosition)


