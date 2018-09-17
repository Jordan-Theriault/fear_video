## Load Libraries ################################
library(tidyverse)
library(stringr)
library(readr)
library(gtools)
library(gsubfn)
library(lme4)

# Subject names should be consistent for input and output.
base.dir <- '~/projects/fear_video_behav/fear_video/'
data.dir <- file.path(base.dir, '/data/processed') # BIDS format. 
output.dir <- file.path(base.dir, '/data/processed') # BIDS format. 

## Load Data.  ################################
load(file.path(data.dir, 'fear_vid_behav.rda'))


## Create dummy variables.  ################################
# video_df$arousal <- 0
video_df$fear <- 0
video_df$pleasant <- 0
video_df$present <- 0
video_df$fear[which(video_df$video_ratings == 'how_fearful')] <- 1
video_df$pleasant[which(video_df$video_ratings == 'how_pleasant')] <- 1
video_df$present[which(video_df$video_ratings == 'how_present_distant')] <- 1

# df_ratings$heights <- 0
video_df$social <- 0
video_df$spider <- 0
video_df$social[which(video_df$video_cats == 'Social')] <- 1
video_df$spider[which(video_df$video_cats == 'Spider')] <- 1

# video_df$lo_intense <- 0
video_df$hi_intense <- 0
video_df$hi_intense[which(video_df$video_intense == 'High')] <- 1


## Model  ################################

mdla <- lmer(Rating ~ fear + pleasant + present +
               social + fear:social + pleasant:social + present:social +
               spider + fear:spider + pleasant:spider + present:spider +
               (1+fear + pleasant + present | Subject) + # corr within heights
               (0+social + fear:social + pleasant:social + present:social | Subject) + # corr within social
               (0+spider + fear:spider + pleasant:spider + present:spider | Subject) + # corr within spider
               # (1+social + spider | SubjID) + # corr within anxiety
               # (0+fear + fear:social + fear:spider | SubjID) + # corr within fear
               # (0+pleasant + pleasant:social + pleasant:spider | SubjID) + # corr within pleasant.
               # (0+present + present:social + present:spider | SubjID) + # corr within present
               (fear+pleasant+present|video_num), data=video_df,
             control=lmerControl(optCtrl = list(maxfun=1e4)))