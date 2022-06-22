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



#make individual figures ----

plotLauchErrIndiv(data, 'pilot')



#start to make figures ----

plotAdapt_all(data)

plotAdapt_tool(data)

