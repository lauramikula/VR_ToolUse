####
# 
# Analysis script for pilot data
# 
####


source('analyses/shared.R')
source('analyses/makePlots.R')



#load data files ----

#select only data files from pilots
expeV <- c('pilot1', 'pilot2')

#load data file
data <- getDataFile(version_expe = expeV)

#get first 4 trials of exposure phase (to compare launch angle error to perturbation)
test <- data %>% 
  group_by(ppid) %>% 
  filter(expe_phase == 'exposure') %>% 
  filter(row_number() == 1:4)



#make individual figures ----

plotLauchErrIndiv(data)



#start to make figures ----

plotAdapt_all(data)

plotAdapt_tool(data)

