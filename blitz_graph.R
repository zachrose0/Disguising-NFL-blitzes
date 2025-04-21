week1 <- read.csv('tracking_week_1.csv')
player_play <- read.csv('player_play.csv')
plays <- read.csv('plays.csv')
players <- read.csv('players.csv')
games <- read.csv('games.csv')
tracking <- week1


defensive_positions <- c('CB', 'DE', 'DT', 'FS', 'ILB', 'LB', 'MLB', 'NT', 'OLB', 'SS')


library(tidyverse)
library(dplyr)

LOS <- tracking %>% 
  filter(frameType == 'SNAP',
         displayName == 'football') %>% 
  select(gameId, playId, x, y)



dropbacks <- plays %>% 
  left_join(games, by = 'gameId') %>% 
  mutate(gameClock_seconds = as.numeric(substr(gameClock, 1, 2)) * 60 + as.numeric(substr(gameClock, 4, 5))) %>%
  filter(week == week,
         playNullifiedByPenalty == 'N',
         absoluteYardlineNumber > 12,
         absoluteYardlineNumber < 108,
         isDropback == T,
         qbSpike == F,
         dropbackType %in% c('TRADITIONAL', 'SCRAMBLE'),
         !(quarter %in% c(2, 4) & gameClock_seconds < 30))


check_frames <- tracking %>% 
  filter(frameType == 'SNAP') %>% 
  select(gameId, playId, nflId, frameId) %>% 
  mutate(check_frame = frameId) %>% 
  select(-frameId) %>% 
  left_join(players, by = 'nflId') %>% 
  filter(position %in% defensive_positions) %>% 
  select(gameId, playId, nflId, check_frame, displayName)

player_locations <- check_frames %>% 
  left_join(tracking, by = c('gameId', 'playId', 'nflId')) %>% 
  semi_join(dropbacks, by = c('gameId', 'playId')) %>% 
  filter(check_frame == frameId) %>% 
  select(gameId, playId, nflId, displayName.x, x, y, playDirection) %>% 
  rename(displayName = displayName.x,
         player.x = x,
         player.y = y) %>% 
  left_join(LOS, by = c('gameId', 'playId')) %>% 
  rename(los.x = x, los.y = y)




 

getPlot <- function(x, y) {
  play <- player_locations %>% 
    filter(gameId == x & playId == y)
  
  if(play$playDirection[1] == 'right'){
    return(ggplot(play, aes(x = player.x, y = player.y))+
             geom_point()+
             geom_vline(xintercept = play$los.x)+
             geom_hline(yintercept = play$los.y +10)+
             geom_hline(yintercept = play$los.y -10)+
             geom_vline(xintercept = 
                          abs(min(play$player.x) - max(play$player.x))*.1 + min(play$player.x)))
  }
  if(play$playDirection[1] == 'left'){
    return(ggplot(play, aes(x = player.x, y = player.y))+
             geom_point()+
             geom_vline(xintercept = play$los.x)+
             geom_hline(yintercept = play$los.y +10)+
             geom_hline(yintercept = play$los.y -10)+
             geom_vline(xintercept = 
                          max(play$player.x) - abs(min(play$player.x) - max(play$player.x))*.1
             ))
  }
}

getPlot(2022091103, 5074)


