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

plotAdapt_all(data)

plotAdapt_tool(data)

plotAdapt_rotation(data)



#look at individual angular errors on specific trials ----

plotAngErr(data)

