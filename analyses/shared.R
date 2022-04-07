#library(sjlabelled)
library(ggpubr) #for plots
#library(cowplot)
library(rstatix)
library(emmeans)
library(afex)
library(lme4)
library(svglite)
library(tidyverse)
library(magrittr)
#library(MASS)
# library(ggforce)
#library(Skillings.Mack)



#import csv files into R ----


getSurvey <- function (version_expe) {
  
  #set the path from version_expe
  FileName <- sprintf('./data/data_Qualtrics_%s.csv', version_expe)
  
  survey <- read_csv(FileName, 
                     col_types = cols())
  
  #import headers labels
  labs <- read_csv('./data/data_Qualtrics_labels_V1.csv', 
                   col_types = cols(.default = 'c'))
  
  # #convert columns into numeric and recode groups
  # toCvt <- c('Duration (in seconds)',
  #            'Q2.2',
  #            'Q2.4',
  #            'Q2.5',
  #            'Q3.2_1',
  #            'Q3.2_2',
  #            'Q3.8',
  #            'Q4.3',
  #            'Q4.5',
  #            'Q4.6',
  #            'Q5.1',
  #            'Q6.4_1',
  #            'Q8.4_1',
  #            'Q8.4_2')
  
  # #suppress warnings because they only appear when running the getSurvey function
  # suppressWarnings(
  #   survey <- survey %>%
  #     mutate(across(all_of(toCvt), as.numeric),
  #            group = recode(group, `1` = 'train_horiz', `2` = 'train_tilt')) #Group: 1 = train_horiz and 2 = train_tilt
  # )
  
  # #recode the group column
  # survey %<>% 
  #   mutate(group = recode(group,
  #                         `1` = 'train_horiz', `2` = 'train_tilt')) %>% 
  #   mutate(group = factor(group, levels = c('train_horiz', 'train_tilt')))
  
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
  
  return(data)
  
}

