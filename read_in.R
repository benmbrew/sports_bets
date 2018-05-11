##########################################
# Examine sports betting 
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(readr)
##########
# read in data and subset for gabe
#########
# read in 2017 data

if('nba_dat.RData' %in% dir('../data')){
  load('data/nba_dat.RData')
} else {
  dat <- read_csv('data/bets_2017.csv')
  
  # remove unneeded columns 
  dat$person <- dat$settled <-  NULL
  
  # rename columns to match 
  colnames(dat)[4:6] <- c('ben_picks', 'gabe_picks', 'line')
  
  # fix date for dat_full
  dat$date <- as.Date(dat$date, "%m/%d/%Y")
  
  # remove where line is na
  dat <- dat %>% filter(!is.na(line))
  
  # remove NA 
  dat <- dat[which(!is.na(dat$money_outcome)),]

  dat <- dat %>% filter(category == 'nba')
  
  # get cumsum
  dat$cum_sum <- cumsum(dat$money_outcome)
  
  
  #split ove under variable into 2 columns
  dat$ben_over_under_team <- unlist(lapply(strsplit(as.character(dat$over_under), '_'), function(x){
    x[1]
  }))
  
  dat$gabe_over_under_team <- unlist(lapply(strsplit(as.character(dat$over_under), '_'), function(x){
    x[2]
  }))
  
  # get over_under fac variable 
  dat$over_under_fac <- ifelse(is.na(dat$over_under), 'The spread', 'The over under')
  
  # create variable that is indicator of with i bet for underdog or favorie 
  dat$under_dog <- ifelse(dat$line < 0, 'favorite', 
                        ifelse(dat$line == 0, 'even', 'under dog'))
  
  # fill NA in under dog where line is >50 (over under game)
  dat$under_dog[dat$line > 50] <- 'NA'
  
  # remove unneeded columns
  dat$over_under <- dat$category <-  NULL

  
  dat$gabe_over_under_team <- ifelse(grepl('7ers', dat$gabe_over_under_team ),
                                     '76ers',
                                     ifelse(grepl('pis', dat$gabe_over_under_team), 
                                            'pistons',
                                            ifelse(grepl('timber', dat$gabe_over_under_team),
                                                   'timberwolves', dat$gabe_over_under_team)))
  
  
  dat$ben_over_under_team <- ifelse(grepl('trai', dat$ben_over_under_team ),
                                    'trailblazers',
                                    ifelse(grepl('warrio', dat$ben_over_under_team), 
                                           'warriors',
                                           ifelse(grepl('nugg', dat$ben_over_under_team),
                                                  'nuggets',
                                                  ifelse(grepl('timber', dat$ben_over_under_team),
                                                         'timberwolves', dat$ben_over_under_team))))
  
  dat$ben_picks <- gsub('mavericks', 'mavs', dat$ben_picks)
  dat$ben_picks <- gsub('houston', 'rockets', dat$ben_picks)
  dat$gabe_picks <- gsub('cleveland', 'cavs', dat$gabe_picks)
  dat$gabe_picks <- gsub('golden_state', 'warriors', dat$gabe_picks)
  dat$gabe_picks <- gsub('warrors', 'warriors', dat$gabe_picks)

  
  
  save.image('data/nba_dat.RData')
  # ggplot(dat, aes(date, cum_sum)) + geom_point(stat= 'identity') +geom_smooth()
  
  
}

# get choices 
nba_teams <- c(unique(dat$ben_picks), unique(dat$gabe_picks))
nba_teams <- nba_teams[!duplicated(nba_teams)]
nba_teams <- nba_teams[!nba_teams %in% c('over', 'under')]
nba_teams <- c('All', Hmisc::capitalize(sort(nba_teams)))

# give teams from data same treamtent 
dat$ben_picks <- Hmisc::capitalize(dat$ben_picks)
dat$gabe_picks <- Hmisc::capitalize(dat$gabe_picks)
dat$ben_over_under_team <- Hmisc::capitalize(dat$ben_over_under_team)
dat$gabe_over_under_team <- Hmisc::capitalize(dat$gabe_over_under_team)
dat$under_dog <- Hmisc::capitalize(dat$under_dog)


