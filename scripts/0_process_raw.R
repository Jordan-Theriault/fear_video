## Load Libraries ################################
library(tidyverse)
library(stringr)
library(readr)
library(gtools)
library(gsubfn)
library(lme4)

## Input ############################################
# Subject names should be consistent for input and output.
data.dir <- '~/projects/fear_video_behav/fear_video/data/raw/csv/' # Subjects in this directory.
output.dir <- '~/projects/fear_video_behav/fear_video/data/processed' # BIDS format. 

df_raw <- read_csv(paste0(data.dir, '/fear_video_survey_raw.csv'))
q_text <- df_raw[1,]

## Exclusions ############################################
df_raw <- df_raw[df_raw$Status == 0,] # Drop trial runs.
df_raw <- df_raw[df_raw$Finished == 1,] # Drop incomplete

## Group Data ############################################
height_txt <- 'Below is a list containing situations involving height. Some people become anxious (tense or uncomfortable) and avoid these situations because of their fear. Please indicate how you would feel in each situation nowadays by checking one of the numbers below each item. - '
spider_txt <- 'Below is a list of situations involving spiders. For each item, please record a number to indicate how much you agree with the statement. Ratings can include any number between 0 (Totally Disagree) and 7 (Totally Agree). - '
social_txt <- 'Below is a list of social situations. Please indicate the level of fear you feel in response to the following situations. - '
height_qs <- sapply(df_raw[,306:325], as.numeric); colnames(height_qs) <- gsub(height_txt, '', q_text[306:325], fixed=TRUE)
spider_qs <- sapply(df_raw[,326:343], as.numeric); colnames(spider_qs) <- gsub(spider_txt, '', q_text[326:343], fixed=TRUE)
social_qs <- sapply(df_raw[,344:367], as.numeric); colnames(social_qs) <- gsub(social_txt, '', q_text[344:367], fixed=TRUE)

# Adjust/counterbalance
height_qs <- height_qs-1
# spider_qs <- TODO
# social_qs <- TODO

## Video Data ############################################
# get names ready.
video_text <- c('Click_First', # full text.
                'Click_Last',
                'Submit',
                'Click_Count',
                "How fearful do you feel in response to the video?",
                "How much arousal do you feel in response to the video?",
                "How pleasant do you feel in response to the video?",
                "How present/distant do you feel in response to the video?")

vid_txt <- c('Click_F', # abbreviated text.
                'Click_L',
                'Sub',
                'C_Count',
                "How_fearful",
                "How_aroused",
                "How_pleasant",
                "How_present-distant")

video_cats <- rep(c(rep('Heights', 12), rep('Social', 12), rep('Spider', 12)), each=length(vid_txt))
video_intense <- rep(rep(c(rep('High', 6), rep('Low', 6)), 3), each=length(vid_txt))
video_num <- rep(c(1:36), each=length(vid_txt))

# create dataframe.
video_qs <- sapply(df_raw[,18:305], as.numeric)
colnames(video_qs) <- c(rep(vid_txt, length(video_qs[1,])/length(vid_txt)))






# OLD


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

