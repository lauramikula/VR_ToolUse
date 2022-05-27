####
# 
# Analysis script for VR Tool Use experiment
# 
####


source('analyses/shared.R')
source('analyses/makePlots.R')



#load data files ----

#select only data files from pilots
expeV <- c('V1')

#load data file
data <- getDataFile(version_expe = expeV)

#load survey file
survey <- getSurvey(version_expe = expeV)



#get some demographics ----

#get the number of participants in each first_pert_cond
N_gps <- data %>% 
  group_by(ppid) %>% 
  filter(row_number() == 1) %>% 
  ungroup(ppid) %>% 
  count(first_pert_cond)
N_gps



#make individual figures ----

##launch angle errors on each trials ----
plotLauchErrIndiv(data, expeV)


##identify non-learners ----

#look at errors in the last 5 trials of each tool, at the end of exposure
plotLast5Exposure_perTool(data, expeV)

#look at errors in the last 5 trials for both tools, at the end of exposure
plotLast5Exposure(data, expeV)

#extract IDs of non-learners
nonLearners <- getListNonLearners(data)

##create new data frame with learners only ----
data_learn <- data %>% 
  filter(!ppid %in% nonLearners) #keep ppid that are not inside nonLearners



#make figures for averaged data ----

##adaptation across all trials ----
plotAdapt_all(data)

##adaption separated by the tool being used ----
plotAdapt_tool(data)

##adaptation separated between tool used and direction of the perturbation ----
plotAdapt_rotation(data)



#look at individual angular errors on specific trials ----

##create new data frames ----

#first and last trials
data_FirstLastTr <- addFirtLastTrials_perTool(data)

#first and last block (average across 8 trials)
data_FirstLastBl <- addFirtLastBlocks_perTool(data)


##launch angle error on first and last trials of each experimental phase ----
plotAngErr_FirstLast_Trial(data_FirstLastTr, WxL = c(10,7), pp = 'all', extension = 'png')


##launch angle error on first and last blocks of each experimental phase ----
plotAngErr_FirstLast_Block(data_FirstLastBl, WxL = c(10,7), pp = 'all', extension = 'png')


##improvement between first and last trials of each experimental phase ----
plotImprove_First_Last(data_FirstLast)



#same as before but for learners only ----

#create data frames ----
data_FirstLastTr_learn <- addFirtLastTrials_perTool(data_learn)
data_FirstLastBl_learn <- addFirtLastBlocks_perTool(data_learn)


##do plots ----
plotAngErr_FirstLast_Trial(data_FirstLastTr_learn, WxL = c(10,7), 
                           pp = 'learners', extension = 'png')
plotAngErr_FirstLast_Block(data_FirstLastBl_learn, WxL = c(10,7), 
                           pp = 'learners', extension = 'png')

