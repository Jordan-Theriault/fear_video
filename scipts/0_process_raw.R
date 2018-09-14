## Load Libraries ################################
library(tidyverse)
library(stringr)
library(readr)
library(gtools)
library(gsubfn)
library(lme4)

## Input ############################################
# Subject names should be consistent for input and output.
data.dir <- '~/projects/fear_video_behav/data/raw/csv/' # Subjects in this directory.
output.dir <- '~/projects/fear_video_behav/data/processed' # BIDS format. 

df_raw <- read_csv(paste0(data.dir, '/fear_video_mturk_ratings.csv'))
names <- colnames(df_raw)[2:length(colnames(df_raw))] # get column names.
df_ratings <- gather_(df_raw, 'vars', 'vals', names) # expand out all columns, so we can label them.

df_ratings$vars <- gsub('^pl|^pleas', 'pleasant', df_ratings$vars)
df_ratings$vars <- gsub('^pleasantant', 'pleasant', df_ratings$vars) # This is very hacky, but it'll have to do.
df_ratings$vars <- gsub('^f', 'fear', df_ratings$vars)
df_ratings$vars <- gsub('^fearear', 'fear', df_ratings$vars)
df_ratings$vars <- gsub('^ar', 'arousal', df_ratings$vars)
df_ratings$vars <- gsub('^arousalousal', 'arousal', df_ratings$vars)
df_ratings$vars <- gsub('^pr|^pres', 'present', df_ratings$vars)
df_ratings$vars <- gsub('^presentent', 'present', df_ratings$vars) # Present = "how present a threat 
# levels(as.factor(df_ratings$vars)) # check variable names

df_ratings <- separate(df_ratings, vars, c('rating', 'condition', 'item'))
# df_ratings$arousal <- 0
df_ratings$fear <- 0
df_ratings$pleasant <- 0
df_ratings$present <- 0
df_ratings$fear[which(df_ratings$rating == 'fear')] <- 1
df_ratings$pleasant[which(df_ratings$rating == 'pleasant')] <- 1
df_ratings$present[which(df_ratings$rating == 'present')] <- 1

# df_ratings$heights <- 0
df_ratings$social <- 0
df_ratings$spider <- 0
df_ratings$social[which(df_ratings$condition == 'Social')] <- 1
df_ratings$spider[which(df_ratings$condition == 'Spider')] <- 1

# levels(as.factor(df_ratings$rating)) # check levels.
# levels(as.factor(df_ratings$condition)) # check levels.
# levels(as.factor(df_ratings$item)) # check levels.
names(df_ratings)[names(df_ratings) == 'SubjectNumber'] <- 'SubjID'
df_ratings$item <- paste0(df_ratings$item, '_', df_ratings$condition)

# Full model - does not coverge.
# mdla <- lmer(vals ~ fear + pleasant + present +
#                        social + fear:social + pleasant:social + present:social +
#                        spider + fear:spider + pleasant:spider + present:spider +
#                (fear + pleasant + present +
#                   social + fear:social + pleasant:social + present:social +
#                   spider + fear:spider + pleasant:spider + present:spider |SubjID) + 
#                (fear+pleasant+present|item), data=df_ratings,
#              control=lmerControl(optCtrl = list(maxfun=1e6)))



mdlb <- lmer(vals ~ fear + pleasant + present +
               social + fear:social + pleasant:social + present:social +
               spider + fear:spider + pleasant:spider + present:spider +
               (1+fear + pleasant + present | SubjID) + # corr within heights
               (0+social + fear:social + pleasant:social + present:social | SubjID) + # corr within social
               (0+spider + fear:spider + pleasant:spider + present:spider | SubjID) + # corr within spider
               # (1+social + spider | SubjID) + # corr within anxiety
               # (0+fear + fear:social + fear:spider | SubjID) + # corr within fear
               # (0+pleasant + pleasant:social + pleasant:spider | SubjID) + # corr within pleasant.
               # (0+present + present:social + present:spider | SubjID) + # corr within present
               (fear+pleasant+present|item), data=df_ratings,
             control=lmerControl(optCtrl = list(maxfun=1e4)))

# Set up contrasts.

