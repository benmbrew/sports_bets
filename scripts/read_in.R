##########################################
# Examine sports betting 
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
##########
# read in data and subset for gabe
#########
# read in 2017 data
dat <- read.csv('data/bets_2017.csv')

# remove unneeded columns 
dat$person <- dat$settled <-  NULL

# rename columns to match 
colnames(dat)[4:6] <- c('ben_picks', 'gabe_picks', 'line')

# fix date for dat_full
dat$date <- as.Date(dat$date, "%m/%d/%Y")

# remove where line is na
dat <- dat %>% filter(!is.na(line))

# get over_under fac variable 
dat$over_under_fac <- ifelse(is.na(dat$over_under), 'No', 'Yes')


