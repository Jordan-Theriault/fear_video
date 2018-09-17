## Load Libraries ################################
library(tidyverse)
library(stringr)
library(readr)
library(gtools)
library(gsubfn)
library(lme4)

## Input ############################################
base.dir <- '~/projects/fear_video_behav/fear_video/'
data.dir <- file.path(base.dir, 'data/raw/csv/') # Subjects in this directory.
output.dir <- file.path(base.dir, '/data/processed') # BIDS format. 

df_raw <- read_csv(paste0(data.dir, '/fear_video_survey_raw.csv'))
q_text <- df_raw[1,]

## Exclusions ############################################
df_raw <- df_raw[df_raw$Status == 0,] # Drop trial runs.
df_raw <- df_raw[df_raw$Finished == 1,] # Drop incomplete

## Get IDS ############################################
subj_IDs <- df_raw$ResponseId

## Group Data ############################################
height_txt <- 'Below is a list containing situations involving height. Some people become anxious (tense or uncomfortable) and avoid these situations because of their fear. Please indicate how you would feel in each situation nowadays by checking one of the numbers below each item. - '
spider_txt <- 'Below is a list of situations involving spiders. For each item, please record a number to indicate how much you agree with the statement. Ratings can include any number between 0 (Totally Disagree) and 7 (Totally Agree). - '
social_txt <- 'Below is a list of social situations. Please indicate the level of fear you feel in response to the following situations. - '
height_qs <- sapply(df_raw[,306:325], as.numeric); colnames(height_qs) <- gsub(height_txt, '', q_text[306:325], fixed=TRUE)
spider_qs <- sapply(df_raw[,326:343], as.numeric); colnames(spider_qs) <- gsub(spider_txt, '', q_text[326:343], fixed=TRUE)
social_qs <- sapply(df_raw[,344:367], as.numeric); colnames(social_qs) <- gsub(social_txt, '', q_text[344:367], fixed=TRUE)

rownames(height_qs) <- subj_IDs
rownames(spider_qs) <- subj_IDs
rownames(social_qs) <- subj_IDs

# Adjust/counterbalance
height_qs <- height_qs-1
spider_qs <- spider_qs-1
social_qs <- social_qs-1

# Subject-level ratings
phobia_df <- data.frame(cbind(rowSums(height_qs),
                              rowSums(spider_qs),
                              rowSums(social_qs)))
colnames(phobia_df) <- c('heights', 'spiders', 'social')

## Video Data ############################################
# get data.
video_qs <- sapply(df_raw[,18:305], as.numeric)
# get names ready.
video_text <- c('Click_First', # full text.
                'Click_Last',
                'Submit',
                'Click_Count',
                "How fearful do you feel in response to the video?",
                "How much arousal do you feel in response to the video?",
                "How pleasant do you feel in response to the video?",
                "How present/distant do you feel in response to the video?")

vid_txt <- c('RT.click_F', # abbreviated text.
                'RT.click_L',
                'RT.sub',
                'RT.c_count',
                "how_fearful",
                "how_aroused",
                "how_pleasant",
                "how_present_distant")

video_cats <- rep(c(rep('Heights', 12), rep('Social', 12), rep('Spider', 12)), each=length(vid_txt))
video_intense <- rep(rep(c(rep('High', 6), rep('Low', 6)), 3), each=length(vid_txt))
video_num <- rep(c(1:36), each=length(vid_txt))
video_ratings <- rep(vid_txt, length(video_qs[1,])/length(vid_txt))

video_info <- data.frame(cbind(video_cats, video_intense, video_num, video_ratings))

# create dataframe.
rownames(video_qs) <- subj_IDs
video_qs <- data.frame(video_qs)
video_qs <- cbind(t(video_qs), video_info)
# Drop RT.
video_qs <- video_qs[grep("^RT", video_qs$video_ratings, inv=TRUE),] # get everything but RT.
video_df <- gather_(video_qs, 'Subject', 'Rating', subj_IDs)

## Save ############################################
save('video_df', 'phobia_df', file = file.path(output.dir, 'fear_vid_behav.rda'))
 
