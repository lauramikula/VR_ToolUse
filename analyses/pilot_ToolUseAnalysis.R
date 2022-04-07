####
# 
# Analysis script for pilot data
# 
####


source('analyses/shared.R')



#load data files ----

#select only data files from pilots
expeV <- 'pilot'

#load data file
data <- getDataFile(expeV)

