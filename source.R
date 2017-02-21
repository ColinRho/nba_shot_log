
library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)




dat <- fread("data/shot_logs.csv")

head(dat)

#--- pre-processing ###############################################################################
dat %<>% 
  separate(MATCHUP, c('DATE', 'MATCHUP'), sep = ' - ', remove = TRUE) %>%
  mutate(DATE = as.Date(DATE, format = '%b %d, %Y')
         , TEAM = substr(MATCHUP, 1, 3)
         , VS = substr(MATCHUP, nchar(MATCHUP) - 2, nchar(MATCHUP))
         # game clock into numeric var (seconds)
         , GAME_CLOCK = as.numeric(lubridate::ms(GAME_CLOCK))) %>%
  select(-MATCHUP) %>% 
  arrange(DATE) %>% 
  as.data.table()

colnames(dat) <- toupper(colnames(dat))
setcolorder(dat, c(1, 2, 20:23, 3:19))
  
#- unifying player name format  
v <- tolower(dat$CLOSEST_DEFENDER)
v1 <- strsplit(v, split = ", ") %>% do.call('rbind', .)
v2 <- paste(v1[,2], v1[,1], sep = " ")
dat[, "CLOSEST_DEFENDER"] <- v2

rm(list=c("v", "v1", "v2"))




#--- game list



dat %>% select(GAME_ID, DATE, AWAY = TEAM, HOME = VS, LOCATION, W, FINAL_MARGIN) %>%
  filter(LOCATION == "A") %>% distinct() %>%
  mutate(HOME_WIN = (W == 'L')) %>%
  select(-c(LOCATION, W)) %>%
  arrange(GAME_ID) %>% as.data.table() -> game_list

setcolorder(game_list, neworder = c(1:4, 6, 5))

game_list


#--- team list

game_list %>% group_by(HOME) %>%
  summarise(n(), WINNINGS = sum(HOME_WIN == T))


dat %>% group_by(TEAM) %>% 
  summarise(WINNINGS = sum(W != 'L' & PLAYER_ID == min(PLAYER_ID) & SHOT_NUMBER == 1)
            , LOSES = sum(W == 'L' & PLAYER_ID == min(PLAYER_ID) & SHOT_NUMBER == 1)
            , FGR = round(sum(FGM == 1)/n(), 3)) %>%
  data.frame()

#--- player list

player_list <- dat %>% select(PLAYER_NAME, PLAYER_ID, TEAM) %>% distinct()

#- adjusting trade during season
player_list %>% group_by(PLAYER_NAME) %>% 
  filter(n() > 1) %>% distinct() %>%
  unlist(., use.names = FALSE) -> trades

#- extract trade date from the first playing game as a new team
dat %>% select(DATE, PLAYER_NAME, TEAM) %>%
  filter(., PLAYER_NAME %in% trades) %>% arrange(PLAYER_NAME) %>%
  group_by(PLAYER_NAME, TEAM) %>% summarise(d = min(DATE)) %>% ungroup() %>%
  group_by(PLAYER_NAME) %>% summarise(TRADE_DATE = max(d)) %>% as.data.table() -> trades

#- distinct players
player_list %<>% 
  group_by(PLAYER_NAME, PLAYER_ID) %>% 
  summarise(TEAM = paste(TEAM, collapse = '-')) %>%
  as.data.table()

#- merge player list and trade date 
player_list <- merge(x = player_list, y = trades, by = 'PLAYER_NAME', all.x = TRUE)

player_list




###################################################################################################

#--- EDA
cat_var <- names(dat)[which(sapply(dat, is.character))]
num_var <- names(dat)[which(sapply(dat, is.numeric))]

dat %>% select_(., .dots = cat_var)
dat %>% select_(., .dots = num_var)







