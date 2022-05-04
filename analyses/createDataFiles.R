#Create data files for each experiment version of the task

library(tidyverse)
library(magrittr)
library(sjlabelled) #for labelled variables


#combine single data files into one, for each experiment version ----

##extract a single participant's data into a data frame ----
extract_oneFile <- function (path_data) {
  
  #find the trial_results file
  result_file <- list.files(path = path_data,
                            pattern = '*trial_results.csv',
                            full.names = TRUE)
  
  #set the experiment version based on the path of the trial_results file
  xpVersion <- strsplit(result_file, '/')[[1]][3] #3rd substring of the current path
  
  #extract data
  df <- read.csv(result_file, stringsAsFactors = FALSE)
  
  #keep only relevant data
  if ('launch_angle' %in% colnames(df)) {
    df <- df %>%
      select(experiment, ppid, trial_num, block_num,
             trial_num_in_block, type, hand, score, 
             starts_with('per_block'), starts_with('target'), 
             starts_with('tool'), error_size, launch_angle)
  } else { 
    #for first pilot data files without the 'launch_angle' column
    df <- df %>%
      select(experiment, ppid, trial_num, block_num,
             trial_num_in_block, type, hand, score, 
             starts_with('per_block'), starts_with('target'), 
             starts_with('tool'), error_size) %>% 
      #calculate launch_angle
      mutate(launch_angle = compute_launchAng(tool_x, tool_z))
  }
  
  #rearrange and add columns
  df <- df %>% 
    #rename 'experiment' column as 'first_pert_cond' (first perturbation condition)
    rename(first_pert_cond = experiment) %>% 
    #create the actual 'experiment' column
    mutate(experiment = xpVersion, .before = first_pert_cond) %>% 
    #rename some columns
    rename(trial_type = type,
           target_angle = per_block_targetListToUse,
           tool_used = per_block_list_tool_type)
  
  #add experiment phase
  df <- addExpePhase(df)
  
  return(df)
  
}


#compute launch angle
compute_launchAng <- function (x, y) {
  
  #x will be tool_x
  #y will be tool_z
  
  #calculate the angle in deg
  launchAng <- atan(y / x) * 180/pi
  
  #add 180° if the angle is < 0 so that all launch angles are positive
  launchAng <- ifelse(launchAng < 0, launchAng + 180, launchAng)
  
  return(launchAng)
  
}


#add experiment phase to data file
addExpePhase <- function (df) {
  
  #define where each experiment phase starts
  #1 = practice
  #2 = baseline
  #3 = exposure
  #4 = washout
  #5 = reexposure
  
  #define cutoffs based on trial number
  expePhase <- list(
    c(0, 40, 120, 240, Inf),      # pilot1 (participants 1 to 5)
    c(0 ,40, 121, 242, 323, Inf), # pilot2 (participants 6 to 10)
    c(0, 48, 129, 291, 340, Inf)  # v1
  )
  
  #define names of the experiment phase
  lbl_expePhase <- list(
    c('practice', 'baseline', 'exposure', 'washout'),
    c('practice', 'baseline', 'exposure', 'washout', 'reexposure'),
    c('practice', 'baseline', 'exposure', 'washout', 'reexposure')
  )
  
  df <- df %>% 
    mutate(expe_phase = case_when(
      #pilot 1
      grepl('pilot1', experiment) ~
        as.character(cut(trial_num, breaks = expePhase[[1]], labels = lbl_expePhase[[1]])),
      #use as.character before cut because otherwise it returns a factor 
      #and it doesn't work with the next bit of code (pilot 2)
      
      #pilot 2
      grepl('pilot2', experiment) ~
        as.character(cut(trial_num, breaks = expePhase[[2]], labels = lbl_expePhase[[2]])),
      
      #other than pilot
      TRUE ~
        as.character(cut(trial_num, breaks = expePhase[[3]], labels = lbl_expePhase[[3]]))
    )
    ) %>%
    #if trial_type is 'instruction', change expe_phase to 'break'
    mutate(expe_phase = ifelse(trial_type == 'instruction', 'break', expe_phase)) %>%
    #convert expe_phase to factor
    mutate(expe_phase = as.factor(expe_phase)) %>%
    #move column 'expe_phase' after 'trial_type'
    relocate(expe_phase, .after = trial_type) 
  
  return(df)
  
}


##combine single data files ----

combine_singleFiles <- function (path_expeV) {
  
  #create empty list for all data frames
  all_dfs <- list()
  
  for (n in list.files(path = path_expeV, full.names = TRUE)) {
    #add folder with session number to path
    n <- paste(n, 'S001', sep = '/')
    
    #extract data into a list
    all_dfs[[n]] <- extract_oneFile(n)
  }
  
  #merge list into a single data frame
  allData <- do.call(rbind, all_dfs)
  
  #reset the row names
  rownames(allData) <- NULL
  
  #arrange rows by participant ID
  allData <- allData %>% 
    arrange(ppid)
  
  return(allData)
  
}


##create a data file for each version of the experiment ----

create_dataFiles <- function (version_xp) {
  
  #create empty list to store paths to experiment versions
  path_expeV <- list()
  
  for (i in version_xp) {
    #build path
    path_expeV[[i]] <- paste('./data', i, sep = '/')
    
    #combine single data files
    df_expeV <- combine_singleFiles(path_expeV[[i]])
    
    #set the names of data files
    name_df = sprintf('./data/data_VRToolUse_%s.csv', i)
    
    #save data files as csv
    write_csv(df_expeV, name_df)

    rm(df_expeV)
  }
  
}

