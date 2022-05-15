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

plotLauchErrIndiv(data, expeV)



#start to make figures ----

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
plotAngErr_FirstLast_Trial(data_FirstLastTr, WxL = c(10,7))


##launch angle error on first and last blocks of each experimental phase ----
plotAngErr_FirstLast_Block(data_FirstLastBl, WxL = c(10,7))


##improvement between first and last trials of each experimental phase ----
plotImprove_First_Last(data_FirstLast)

