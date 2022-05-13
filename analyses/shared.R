library(tidyverse)
library(magrittr)
library(sjlabelled) #for labelled variables
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
  FileLblName <- sprintf('./data/data_Qualtrics_labels_%s.csv', version_expe)
  labs <- read_csv(FileLblName, 
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
    #remove rows in which trial_type is 'instruction'
    filter(trial_type != 'instruction') %>% 
    #make expe_phase as levels
    mutate(expe_phase = factor(expe_phase, levels = c('practice',
                                                      'baseline',
                                                      'exposure',
                                                      'washout',
                                                      'reexposure'))) %>% 
    #calculate launch_angle_err:
    #difference between launch angle and target angle (< 0 is left; > 0 is right)
    mutate(launch_angle_err = target_angle - launch_angle) %>% 
    #replace errors > 0.5 with NA
    mutate(error_size = ifelse(error_size > 0.5, NA, error_size),
           launch_angle = ifelse(is.na(error_size), NA, launch_angle),
           launch_angle_err = ifelse(is.na(error_size), NA, launch_angle_err)) %>% 
    #calculate launch angle error direction:
    #(> 0 is the same direction as the perturbation and < 0 is opposite to the perturbation)
    mutate(launch_angle_err_dir = sign_pert_tool * launch_angle_err)
  
  #create trial number per tool used
  data <- data %>% 
    group_by(ppid, tool_used) %>% 
    mutate(trialN_tool = row_number(), .after = tool_used)
  
  return(data)
  
}

