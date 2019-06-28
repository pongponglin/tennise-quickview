library(magrittr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(highcharter)

atp_m2015 <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2015.csv",
                      stringsAsFactors = FALSE)
atp_m2016 <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2016.csv",
                      stringsAsFactors = FALSE)
atp_m2017 <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2017.csv",
                      stringsAsFactors = FALSE)
atp_m2018 <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_2018.csv",
                      stringsAsFactors = FALSE)

ranking <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_10s.csv")
ranking2017 <- ranking %>% filter(ranking_date%in%unique(atp_m2017$tourney_date)) %>% 
  filter(player%in%unique(atp_m2017$winner_id,atp_m2017$loser_id))
player <- read.csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv")

sum(is.na(atp_m2017$winner_rank))
sum(is.na(atp_m2017$loser_rank))


atp_m2015 %>% filter(!is.na(winner_rank) & !is.na(loser_rank)) %>% mutate(year="2015") %>% 
  rbind(filter(atp_m2016,!is.na(winner_rank) & !is.na(loser_rank)) %>% mutate(year="2016")) %>% 
  rbind(filter(atp_m2017,!is.na(winner_rank) & !is.na(loser_rank)) %>% mutate(year="2017")) %>% 
  rbind(filter(atp_m2018,!is.na(winner_rank) & !is.na(loser_rank)) %>% mutate(year="2018")) %>% 
  mutate(rank_diff = abs(winner_rank-loser_rank),
         high_win = ifelse(winner_rank<loser_rank,1,0)) -> atp
table(atp$year)
table(atp$surface)

atp_player <- filter(player,player_id%in%unique(atp$winner_id,atp$loser_id))
table(atp_player$hand)

######
atp %>% group_by(rank_diff) %>% 
  summarise(total_win=sum(high_win),prob=mean(high_win),n=n()) %>% 
  # filter(rank_diff<=100) %>% 
  ggplot(aes(x=rank_diff, y=prob)) +
  geom_point() +
  ylim(0.3, 1) +
  ggtitle("All") +
  geom_smooth(method=lm) 

atp %>% 
  mutate(group = cut(rank_diff,breaks=quantile(c(0,rank_diff),probs = seq(0,1,0.05)), labels=c(1:20))) %>% 
  group_by(group) %>% 
  summarise(total_win=sum(high_win),prob=mean(high_win),n=n()) %>% 
  ggplot(aes(x=group, y=prob)) +
  geom_point() +
  ylim(0.4, 0.9) +
  ggtitle("All") + 
  geom_smooth(method=lm) 

atp %>% group_by(year) %>%
  mutate(y_group = cut(rank_diff,breaks=quantile(c(0,rank_diff),probs = seq(0,1,0.1)), labels=c(1:10))) %>% 
  group_by(year,y_group) %>% 
  summarise(total_win=sum(high_win),prob=mean(high_win),n=n()) %>% 
  hchart("line", hcaes(x = y_group, y = prob, group = year))

##### Surface #########
atp %>% filter(surface%in%c("Hard","Grass","Clay")) %>% 
  group_by(surface) %>% 
  mutate(m_group = cut(rank_diff,breaks=quantile(c(0,rank_diff),probs = seq(0,1,0.1)), labels=c(1:10))) %>% 
  group_by(surface,m_group) %>% 
  summarise(total_win=sum(high_win),prob=mean(high_win),n=n()) %>% 
  ggplot(aes(x=m_group, y=prob, group=surface, colour=surface)) + 
  geom_line() + geom_point() 

atp %>% filter(surface%in%c("Hard","Grass","Clay")) %>% 
  group_by(surface) %>% 
  mutate(m_group = cut(rank_diff,breaks=quantile(c(0,rank_diff),probs = seq(0,1,0.1)), labels=c(1:10))) %>% 
  group_by(surface,m_group) %>% 
  summarise(total_win=sum(high_win),prob=mean(high_win),n=n()) %>% 
  hchart("line", hcaes(x = m_group, y = prob, group = surface),color=c("#f36162","#5C954A","#495397"))

simple.fit = lm(prob~rank_diff, data=hard100)
summary(simple.fit)

###### age ######
atp_m2015 %>% filter(!is.na(winner_age) & !is.na(loser_age)) %>% mutate(year="2015") %>% 
  rbind(filter(atp_m2016,!is.na(winner_age) & !is.na(loser_age)) %>% mutate(year="2016")) %>% 
  rbind(filter(atp_m2017,!is.na(winner_age) & !is.na(loser_age)) %>% mutate(year="2017")) %>% 
  rbind(filter(atp_m2018,!is.na(winner_age) & !is.na(loser_age)) %>% mutate(year="2018")) %>% 
  mutate(age_diff = abs(winner_age-loser_age),
         old_win = ifelse(winner_age>loser_age,1,0)) -> atp_age

atp_age %>% 
  mutate(group = cut(age_diff,breaks=quantile(c(-1,age_diff),probs = seq(0,1,0.05)), labels=c(1:20))) %>% 
  group_by(group) %>% 
  summarise(total_win=sum(old_win),prob=mean(old_win),n=n()) %>% 
  ggplot(aes(x=group, y=prob)) +
  geom_point() +
  ylim(0.4, 0.6) +
  ggtitle("All") + 
  geom_smooth(method=lm) 

###### win_rate
atp %>% group_by(year,winner_id,winner_name) %>% 
  summarise(win_n=n()) %>% 
  full_join(atp %>% group_by(year,loser_id,loser_name) %>% summarise(lose_n=n()),
             by=c("winner_id"="loser_id","winner_name"="loser_name","year"="year")) -> win_rate
win_rate$win_n[is.na(win_rate$win_n) ] <- 0
win_rate$lose_n[is.na(win_rate$lose_n) ] <- 0
win_rate %<>% mutate(total_n=win_n+lose_n,
                    win_rate=round(win_n/total_n*100,2),
                    lose_rate=round(lose_n/total_n*100,2))
win_rate %>% filter(total_n>10 ) %>% 
  group_by(year) %>% 
  mutate(y=n(),rank=1+y-rank(win_rate)) %>% 
  filter(rank%in%c(1:10)) -> year4_win
year4_win %>% 
  select(rank,year, winner_name) %>% View()
  spread(year, winner_name) %>% View()
