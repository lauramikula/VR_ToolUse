library(tidyverse)
library(magrittr)
library(ggpubr) #for plots
library(rstatix)
library(emmeans)



#import csv files into R ----


getSurvey <- function (version_expe) {
  
  #set the path from version_expe
  FileName <- sprintf('./data/data_Qualtrics_%s.csv', version_expe)
  
  survey <- read_csv(FileName, 
                     col_types = cols())
  
  #import headers labels
  labs <- read_csv('./data/data_Qualtrics_labels_V1.csv', 
                   col_types = cols(.default = 'c'))
  
  #set labels for the survey data frame
  survey %<>% 
    set_label(label = as.character(labs[1,])) #need to convert 1st row of labs into character
  
  return(survey)
  
}


getDataFile <- function (version_expe) {
  
  #set the path from version_expe
  FileName <- sprintf('./data/data_VRToolUse_%s.csv', version_expe)
  
  data <- read_csv(FileName, 
                   col_types = cols())
  
  #process raw data
  data <- data %>% 
    #make expe_phase as levels
    mutate(expe_phase = factor(expe_phase, levels = c('practice',
                                                      'baseline',
                                                      'exposure',
                                                      'washout',
                                                      'reexposure'))) %>% 
    #calculate the difference between launch angle and target angle (< 0 is left; > 0 is right)
    mutate(launch_angle_err = target_angle - launch_angle) %>% 
    #replace errors > 0.5 with NA
    mutate(error_size = ifelse(error_size > 0.5, NA, error_size),
           launch_angle = ifelse(is.na(error_size), NA, launch_angle),
           launch_angle_err = ifelse(is.na(error_size), NA, launch_angle_err)) %>% 
    #make all launch error angles the same sign
    mutate(launch_angle_err = ifelse(launch_angle_err < 0, 
                                     launch_angle_err*-1, launch_angle_err))
  
  return(data)
  
}

