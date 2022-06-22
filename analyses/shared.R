library(tidyverse)
library(magrittr)
library(sjlabelled) #for labelled variables
library(ggpubr) #for plots
library(rstatix)
library(emmeans)
library(afex)



#create new figures folder if needed
dir.create(file.path('./', 'docs/figures'), showWarnings = FALSE)



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
  
  #read .csv file
  data <- read_csv(FileName, 
                   col_types = cols())
  
  #define maximum error size (0.5 in Shanaa's experiment)
  max_err <- 0.6
  
  #number of observations before removing errors > max_err
  N_obs_before <- nrow(data)
  
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
    #replace errors > max_err with NA
    mutate(error_size = ifelse(error_size > max_err, NA, error_size),
           launch_angle = ifelse(is.na(error_size), NA, launch_angle),
           launch_angle_err = ifelse(is.na(error_size), NA, launch_angle_err)) %>%
    #calculate launch angle error direction:
    #(> 0 is the same direction as the perturbation and < 0 is opposite to the perturbation)
    mutate(launch_angle_err_dir = sign_pert_tool * launch_angle_err)
  
  #calculate score_per_trial (subtract score between two successive rows after grouping by ID)
  data <- data %>% 
    group_by(ppid) %>% 
    mutate(score_per_trial = score - lag(score, default = 0), .after = score) %>% 
    ungroup()
  
  #create trial number per tool used
  data <- data %>% 
    group_by(ppid, tool_used) %>% 
    mutate(trialN_tool = row_number(), .after = tool_used)
  
  #add block number per tool used
  data <- addBlockNtool(data)
  
  #number of observations after removing errors > max_err
  N_obs_after <- data %>% 
    filter(is.na(error_size)) %>% 
    nrow()
  
  #calculate % of trials removed (errors > max_err)
  obs_rm <- N_obs_after * 100 / N_obs_before
  
  #print message in console
  message(
    sprintf('Maximum error size (max_err) set to %s\nPercentage of observations removed (error_size > max_err): %.2f%%', 
            max_err, obs_rm)
  )
  
  return(data)
  
}



#calculate additional parameters ----


addBlockNtool <- function(df) {
  
  #define trial number cutoffs to create block number per tool used
  cutoffs_blockNum <- df %>% 
    filter(ppid == 1) %>% 
    filter(tool_used == .$tool_used[1]) %>%
    group_by(block_num) %>%
    filter(row_number() == 1) %>%
    ungroup() %>% 
    pull(trial_num)
  
  #define number of blocks based on the cutoffs
  blockNum <- seq(1:(length(cutoffs_blockNum)-1))
  
  #create block number per tool used
  df <- df %>% 
    group_by(ppid, tool_used) %>% 
    mutate(blockN_tool = cut(trial_num, breaks = cutoffs_blockNum, labels = blockNum, 
                             include.lowest = TRUE, right = FALSE), 
           .before = trialN_tool) %>% 
    mutate(blockN_tool = as.factor(blockN_tool))
  
  return(df)
  
}


addFirtLastTrials_perTool <- function (df) {
  
  #get the 1st and last trial number per tool for each experimental phase
  trials_to_keep <- df %>% 
    filter(expe_phase != 'practice') %>% #remove practice trials
    group_by(expe_phase) %>% 
    filter(row_number() == 1 | row_number() == n()) %>% 
    arrange(trialN_tool) %>% 
    pull(trialN_tool)
  
  #extract the first trials of each experimental phase
  #(every other element of trials_to_keep, starting from the 1st element)
  firstTr <- trials_to_keep[c(TRUE, FALSE)]
  #extract the last trials of each experimental phase
  #(every other element of trials_to_keep, starting from the 2nd element)
  lastTr <- trials_to_keep[c(FALSE, TRUE)]
  
  df2 <- df %>% 
    filter(trialN_tool %in% trials_to_keep) %>% 
    #create trialN
    mutate(trialN = case_when(trialN_tool %in% firstTr ~ 'First trial',
                              trialN_tool %in% lastTr ~ 'Last trial',
                              TRUE ~ 'NA')) %>%
    #rearrange otherwise geom_line and geom_point for individual 
    #data points don't have the same jitter and aren't aligned
    arrange(expe_phase, trialN, ppid, tool_used)
  
  return(df2)
  
}


addFirtLastBlocks_perTool <- function (df) {
  
  #get the 1st and last block number per tool for each experimental phase
  blocks_to_keep <- df %>%
    filter(expe_phase != 'practice') %>% #remove practice trials
    group_by(expe_phase) %>%
    filter(row_number() == 1 | row_number() == n()) %>%
    arrange(blockN_tool) %>%
    pull(blockN_tool)

  #extract the first blocks of each experimental phase
  #(every other element of blocks_to_keep, starting from the 1st element)
  firstBl <- blocks_to_keep[c(TRUE, FALSE)]
  #extract the last blocks of each experimental phase
  #(every other element of blocks_to_keep, starting from the 2nd element)
  lastBl <- blocks_to_keep[c(FALSE, TRUE)]

  df2 <- df %>%
    filter(blockN_tool %in% blocks_to_keep) %>%
    #create blockN
    mutate(blockN = case_when(blockN_tool %in% firstBl ~ 'First block',
                              blockN_tool %in% lastBl ~ 'Last block',
                              TRUE ~ 'NA')) %>%
    #calculate launch angle error direction across the whole block (i.e. 8 trials)
    group_by(experiment, ppid, expe_phase, blockN, tool_used) %>%
    summarise(mn_launch_angle_err_dir = mean(launch_angle_err_dir, na.rm = TRUE),
              n = n(),
              .groups = 'drop') %>%
    #rearrange otherwise geom_line and geom_point for individual
    #data points don't have the same jitter and aren't aligned
    arrange(expe_phase, blockN, ppid, tool_used)

  return(df2)
  
}



#get non-learners ----

getListNonLearners <- function (df) {
  
  nonlearners <- df %>% 
    #keep exposure trials only
    filter(expe_phase == 'exposure') %>% 
    #keep the last 5 trials for each tool (df is already grouped by ppid and tool_used)
    do(tail(., 5)) %>% 
    ungroup(tool_used) %>% 
    #calculate median error on 10 trials (5 last trials of each tool during exposure)
    summarise(median_err = median(launch_angle_err_dir, na.rm = TRUE),
              .groups = 'drop') %>% 
    #filter those who have median errors > 15°
    filter(median_err > 15 | median_err < -15) %>% 
    pull(ppid)
  
  #get a table with ppid and first_pert_cond
  df_nonlearners <- df %>% 
    group_by(ppid) %>% 
    filter(row_number() == 1) %>% 
    select(ppid, first_pert_cond) %>% 
    filter(ppid %in% nonlearners) %>% 
    ungroup() %>% 
    mutate(pert = case_when(
      grepl('i30|s-30', first_pert_cond) ~ 'paddle CCW',
      grepl('i-30|s30', first_pert_cond) ~ 'paddle CW',
      TRUE ~ 'NA'
    ))
  
  df_nonlearners2 <- df_nonlearners %>% 
    group_by(pert) %>% 
    summarise(n = n(),
              .groups = 'drop')
  
  #print the table
  message('Participants identified as non-learners')
  print(df_nonlearners)
  print(df_nonlearners2)
  
  return(nonlearners)
  
}

