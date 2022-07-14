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

#filter only V1 participants
no_pp <- c('wn10', 'In10')
survey2 <- survey %>% 
  filter(as.Date(RecordedDate) < as.Date(c('2022-06-15')),
         !Q4 %in% no_pp) %>% 
  distinct(Q4, .keep_all = TRUE)
  



#get some demographics ----

#get the number of participants in each first_pert_cond
N_gps <- data %>% 
  group_by(experiment) %>% 
  group_by(ppid) %>% 
  filter(row_number() == 1) %>% 
  ungroup(ppid) %>% 
  count(experiment, first_pert_cond)
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
plotAdapt_tool_noPractice(data, save = FALSE)
plotAdapt_tool_noPractice(data_learn, pp = 'learners', save = FALSE)

##adaptation separated between tool used and direction of the perturbation ----
plotAdapt_rotation(data)

##adaptation first and last trials on every switch ----
plotAdapt_EverySwitch(data, WxL = c(10,7), 
                      pp = 'all', extension = 'png')
plotAdapt_EverySwitch(data_learn, WxL = c(10,7), 
                      pp = 'learners', extension = 'png')



#look at individual angular errors on specific trials ----

##create new data frames ----

#first and last trials
data_FirstLastTr <- addFirtLastTrials_perTool(data)

#first and last block (average across 8 trials)
data_FirstLastBl <- addFirtLastBlocks_perTool(data)

#average across first 4 and last 4 trials
data_FirstLast4Tr <- addFirtLast4Trials_perTool(data)
#remove baseline first trials
to_rmv <- data_FirstLast4Tr %>% 
  filter(expe_phase == 'baseline' & trialN == 'First 4 trials') #get trials to remove first (in to_rmv)
data_FirstLast4Tr <- anti_join(data_FirstLast4Tr, to_rmv) #anti-join to remove trials in to_rmv
rm(to_rmv)
#add a column for interaction between tool and experiment version (to get ordered plots)
data_FirstLast4Tr <- data_FirstLast4Tr %>% 
  mutate(tool_expe = factor(interaction(tool_used, experiment),
                            levels = c('paddle.V2', 'paddle.V1', 'slingshot.V1', 'slingshot.V2'))) 
data_FirstLast4Tr <- data_FirstLast4Tr %>% 
  arrange(tool_expe)
  


##launch angle error on first and last trials of each experimental phase ----
plotAngErr_FirstLast_Trial(data_FirstLastTr, WxL = c(10,7), pp = 'all', extension = 'png')

#visualize data
ggplot(data_FirstLastTr, 
       aes(x = launch_angle_err_dir, group = tool_used, color = tool_used)) + 
  geom_density() + 
  facet_wrap(expe_phase ~ trialN)


#stats
dataStats <- data_FirstLastTr %>% 
  convert_as_factor(ppid, tool_used, expe_phase, trialN, trialN_tool)
#normality assumption
dataStats %>% 
  group_by(tool_used, trialN_tool) %>% 
  shapiro_test(launch_angle_err_dir)
ggqqplot(dataStats, 'launch_angle_err_dir') + facet_grid(tool_used ~ trialN)
#ANOVA
res.aov <- aov_ez(data = dataStats, dv = 'launch_angle_err_dir', id = 'ppid',
                  within = c('tool_used', 'expe_phase', 'trialN'),
                  anova_table = list(es = 'pes'), #get partial eta-squared
                  include_aov = TRUE) #get uncorrected degrees of freedom
get_anova_table(res.aov)
#qqplot
ggqqplot(as.numeric(residuals(res.aov$lm)))
#interaction
postHoc <- emmeans(res.aov, ~ trialN*expe_phase, adjust = 'bonferroni')
test <- pairs(postHoc) %>% 
  as.data.frame() %>% 
  mutate(p_val = format.pval(p.value, digits = 3)) %>% 
  add_significance()
#get effect sizes
eff_size(postHoc, sigma = sigma(res.aov$aov$`ppid:expe_phase:trialN`), 
         edf = df.residual(res.aov$aov$`ppid:expe_phase:trialN`))

#posthoc
contrast(postHoc, interaction = c('pairwise', 'eff'))
contrast(postHoc, interaction = c('eff', 'pairwise'))

#GLMM
#first transform variables into factors!!!!!!!! or use dataStats as before
mdl <- glm(launch_angle_err_dir ~ tool_used + expe_phase * trialN + (1 | ppid),
            family = gaussian,
            data = dataStats)
summary(mdl)
plot(allEffects(mdl))
emms <- emmeans(mdl, ~ trialN*expe_phase, adjust = 'bonferroni')
emms2 <- pairs(emms) %>% 
  as.data.frame() %>% 
  mutate(p_val = format.pval(p.value, digits = 3)) %>% 
  add_significance()
#posthoc
contrast(emms, interaction = c('pairwise', 'eff'))


##launch angle error on first and last blocks of each experimental phase ----
plotAngErr_FirstLast_Block(data_FirstLastBl, WxL = c(10,7), pp = 'all', extension = 'png')


##launch angle error on first and last trials of each switch ----
plotAngErr_EverySwitch(data, WxL = c(10,7), pp = 'all', extension = 'png')


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
plotAngErr_EverySwitch(data_learn, WxL = c(10,7), 
                       pp = 'learners', extension = 'png')



#redo some plots for presentations ----

#trial-by-trial adaptation
plotAdapt_tool_noPractice(data, save = TRUE, extension = 'svg')

#single trial errors
#all
plotAngErr_FirstLast_Trial_pres(data_FirstLastTr, extension = 'png')
#learners
plotAngErr_FirstLast_Trial_pres(data_FirstLastTr_learn, pp = 'learners', extension = 'png')

#single block errors
#all
plotAngErr_FirstLast_Block_pres(data_FirstLastBl, extension = 'svg')
#learners
plotAngErr_FirstLast_Block_pres(data_FirstLastBl_learn, pp = 'learners', extension = 'png')

#every 4 trials errors
#all
plotAngErr_FirstLast_4Trial_pres(data_FirstLast4Tr, extension = 'png')
#V1 and V2 on the same plot
plotAngErr_FirstLast_4Trial_V1V2_pres(data_FirstLast4Tr, extension = 'png')

