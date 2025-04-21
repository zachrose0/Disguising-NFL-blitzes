#1 import stuff
games <- read.csv('games.csv')
player_play <- read.csv('player_play.csv')
plays <- read.csv('plays.csv')
players <- read.csv('players.csv')
week1 <- read.csv('tracking_week_1.csv')
week2 <- read.csv('tracking_week_2.csv')
week3 <- read.csv('tracking_week_3.csv')
week4 <- read.csv('tracking_week_4.csv')
week5 <- read.csv('tracking_week_5.csv')
week6 <- read.csv('tracking_week_6.csv')
week7 <- read.csv('tracking_week_7.csv')
week8 <- read.csv('tracking_week_8.csv')
week9 <- read.csv('tracking_week_9.csv')

library(tidyverse)
library(dplyr)

defensive_positions <- c('CB', 'DE', 'DT', 'FS', 'ILB', 'LB', 'MLB', 'NT', 'OLB', 'SS')




get_WPA <- function(df, week) {
  tracking <- df
  
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
  
  success <- dropbacks %>%
    select(gameId, playId, down, yardsToGo, possessionTeam, defensiveTeam, yardsGained) %>% 
    mutate(successful_play_for_defense = case_when(
      down == 1 & yardsGained >= .4*yardsToGo ~ F,
      down == 2 & yardsGained >= .6*yardsToGo ~F,
      (down == 3 | down == 4) & yardsGained >= yardsToGo ~ F,
      .default = T
    ))
  
  
  rushers_per_play <- player_play %>% 
    semi_join(dropbacks, by = c('gameId', 'playId')) %>% 
    group_by(gameId, playId) %>% 
    summarise(rushers = sum(wasInitialPassRusher, na.rm = T))
  
  
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
  
  
  show_blitz_right <- player_locations %>% 
    filter(playDirection == 'right') %>%
    group_by(gameId, playId) %>% 
    mutate(back_line = abs(min(player.x) - max(player.x))*.1 + min(player.x)) %>% 
    ungroup() %>% 
    mutate(blitzing = case_when(
      player.x < back_line & player.y < los.y +10 & player.y > los.y -10 ~ T,
      .default = F)) %>% 
    filter(blitzing == T) %>% 
    group_by(gameId, playId) %>% 
    summarise(potentials = sum(blitzing))
  
  
  show_blitz_left <- player_locations %>% 
    filter(playDirection == 'left') %>%
    group_by(gameId, playId) %>% 
    mutate(back_line = max(player.x) - abs(min(player.x) - max(player.x))*.1) %>% 
    ungroup() %>% 
    mutate(blitzing = case_when(
      player.x > back_line & player.y < los.y +10 & player.y > los.y -10 ~ T,
      .default = F)) %>% 
    filter(blitzing == T) %>% 
    group_by(gameId, playId) %>% 
    summarise(potentials = sum(blitzing))
  
  
  show_blitz <- rbind(show_blitz_right, show_blitz_left) %>% 
    filter(potentials != 1)
  
  
  disguise <- show_blitz %>% 
    left_join(rushers_per_play, by = c('gameId', 'playId')) %>% 
    mutate(diff = rushers - potentials)
  
  success_disguise <- disguise %>% 
    left_join(success) %>% 
    group_by(diff) %>% 
    summarise(count = n(),
              success_percent = mean(successful_play_for_defense))
                
  
  return(success_disguise)
  
}

total_success <- rbind(
  get_WPA(week1, 1),
  get_WPA(week2, 2),
  get_WPA(week3, 3),
  get_WPA(week4, 4),
  get_WPA(week5, 5),
  get_WPA(week6, 6),
  get_WPA(week7, 7),
  get_WPA(week8, 8),
  get_WPA(week9, 9))

final <- total_success %>% 
  group_by(diff) %>% 
  summarise(count = sum(count),
            success_percent = mean(success_percent)) %>% 
  filter(count >= 50)

ggplot(final, aes(x=diff, y = success_percent))+
  geom_col()+
  scale_x_continuous(breaks = -4:2)+
  scale_y_continuous(limits = c(0,1))+
  theme_classic()


