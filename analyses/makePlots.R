source('analyses/shared.R')


#plots individual data ----

plotLauchErrIndiv <- function (df, expeV) {
  
  #to save figures as a pdf doc
  fname = sprintf('./docs/LaunchAngleError_indiv_%s.pdf', expeV)
  pdf(fname, width = 12, height = 9)
  par(mfrow = c(2,2)) #2*2 grid
  
  #list all participants
  pp <- unique(df$ppid)
  
  for (i in 1:length(pp)) {
    
    #empty list to store plots
    plt_ls <- list()
    
    #get data for a single participant
    dataN <- df %>% 
      filter(ppid %in% pp[i])
    
    #get the name of the experiment
    expeV <- unique(dataN$experiment)
    
    #set title plot
    plot_ttl <- sprintf('%s - Participant %s', expeV, pp[i])
    
    plot(dataN$trial_num, dataN$launch_angle_err,
         type = 'l', col = 'gray50',
         xlab = 'Trials', ylab = 'Left <--- Angular error re: target (°) ---> Right',
         main = plot_ttl,
         ylim = c(-60, 45))
    points(dataN$trial_num, dataN$launch_angle_err,
           col = dataN$expe_phase)
    abline(h = c(-30, 0, 30))
    legend(x = 'bottomleft', col = unique(dataN$expe_phase), pch = 1,
           title = 'Experimental phase', legend = as.character(unique(dataN$expe_phase)),
           bg = 'white', horiz = TRUE, bty = 'n')
    
  }
  
  dev.off()
  
}



#plots averaged data ----

plotAdapt_all <- function (df) {
  
  plot <- list()
  
  for (i in 1:length(unique(df$experiment))) {
    
    expe_v <- unique(df$experiment)[i]
    
    #get the number of participants for the experiment
    Npp <- df %>%
      filter(experiment == expe_v)
    Npp <- length(unique(Npp$ppid))
    
    #calculate mean launch error angle across all participants
    df_i <- df %>% 
      filter(experiment == expe_v) %>% 
      group_by(trial_num, expe_phase) %>% 
      # summarise(mn_err = mean(launch_angle_err, na.rm = TRUE),
      #           sd_err = sd(launch_angle_err, na.rm = TRUE),
      #           .groups = 'drop')
      summarise(mn_err = mean(launch_angle_err_dir, na.rm = TRUE), #error direction instead of just error
                sd_err = sd(launch_angle_err_dir, na.rm = TRUE),
                .groups = 'drop')
    
    plot[[i]] <- ggplot(data = df_i, 
                        aes(x = trial_num, y = mn_err,
                            color = expe_phase, fill = expe_phase)) + 
      geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') +
      geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') +
      geom_hline(yintercept = 30, linetype = 'dashed', color = 'grey40') +
      geom_ribbon(aes(ymin = mn_err - sd_err, max = mn_err + sd_err),
                  linetype = 0, alpha = 0.4) +
      geom_line() +
      geom_point() + 
      
      theme_classic_article() +
      scale_color_discrete(name = 'Experimental phase') +
      scale_fill_discrete(name = 'Experimental phase') +
      labs(title = sprintf('%s (N = %s) - All trials', expe_v, Npp), 
           x = 'Trials', y = 'Opposite side of the perturbation <-- Angular error (°) --> Same side of the perturbation')
    
    print(plot[[i]])
    
  }
  
}



plotAdapt_tool <- function (df) {
  
  plot <- list()
  
  for (i in 1:length(unique(df$experiment))) {
    
    expe_v <- unique(df$experiment)[i]
    
    #get the number of participants for the experiment
    Npp <- df %>%
      filter(experiment == expe_v)
    Npp <- length(unique(Npp$ppid))
    
    #calculate mean launch error angle across all participants
    df_i <- df %>% 
      filter(experiment == expe_v) %>% 
      group_by(trialN_tool, expe_phase, tool_used) %>% 
      summarise(mn_err = mean(launch_angle_err_dir, na.rm = TRUE),
                sd_err = sd(launch_angle_err_dir, na.rm = TRUE),
                .groups = 'drop')
    
    plot[[i]] <- ggplot(data = df_i, 
                        aes(x = trialN_tool, y = mn_err, 
                            color = interaction(tool_used, expe_phase), 
                            fill = interaction(tool_used, expe_phase))) + 
      geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') +
      geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') +
      geom_hline(yintercept = 30, linetype = 'dashed', color = 'grey40') +
      geom_ribbon(aes(ymin = mn_err - sd_err, max = mn_err + sd_err),
                  linetype = 0, alpha = 0.3) +
      geom_line(size = 0.8) + 
      geom_point() + 
      
      theme_classic_article() +
      scale_color_discrete(name = 'Experimental phase') +
      scale_fill_discrete(name = 'Experimental phase') +
      scale_y_continuous(breaks = seq(0, 30, 15), expand = c(0, 0)) + 
      labs(title = sprintf('%s (N = %s) - Per tool', expe_v, Npp), 
           x = 'Trials per tool', y = 'Opposite side of the perturbation <-- Angular error (°) --> Same side of the perturbation') + 
      #make drawings unconfined to the plot panel
      coord_cartesian(clip = 'off')
    
    print(plot[[i]])
    
  }
  
}



plotAdapt_rotation <- function (df) {
  
  plot <- list()
  
  for (i in 1:length(unique(df$experiment))) {
    
    expe_v <- unique(df$experiment)[i]
    
    #get the number of participants for the experiment
    Npp <- df %>%
      filter(experiment == expe_v)
    Npp <- length(unique(Npp$ppid))
    
    #calculate mean launch error angle across all participants
    df_i <- df %>% 
      filter(experiment == expe_v) %>% 
      group_by(trialN_tool, expe_phase, tool_used, sign_pert_tool) %>% 
      summarise(mn_err = mean(launch_angle_err, na.rm = TRUE),
                sd_err = sd(launch_angle_err, na.rm = TRUE),
                .groups = 'drop')
    
    plot[[i]] <- ggplot(data = df_i, 
                        aes(x = trialN_tool, y = mn_err, 
                            color = interaction(tool_used, sign_pert_tool, expe_phase), 
                            fill = interaction(tool_used, sign_pert_tool, expe_phase))) + 
      geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') +
      geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') +
      geom_hline(yintercept = c(-30, 30), linetype = 'dashed', color = 'grey40') +
      geom_ribbon(aes(ymin = mn_err - sd_err, max = mn_err + sd_err),
                  linetype = 0, alpha = 0.3) +
      geom_line(size = 0.8) + 
      geom_point() + 
      
      theme_classic_article() +
      scale_color_discrete(name = 'Experimental phase') +
      scale_fill_discrete(name = 'Experimental phase') +
      scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0, 0)) + 
      labs(title = sprintf('%s (N = %s) - Per rotation', expe_v, Npp), 
           x = 'Trials per rotation', y = 'CCW <-- Angular error (°) --> CW') + 
      #make drawings unconfined to the plot panel
      coord_cartesian(clip = 'off')
    
    print(plot[[i]])
    
  }
  
}



#plots individual and averaged data ----

plotAngErr <- function (df) {
  
  #custom colors
  myCols <- c('#0065A9', '#F68B69')
  
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
  
  df <- df %>% 
    filter(trialN_tool %in% trials_to_keep) %>% 
    #create trialN
    mutate(trialN = case_when(trialN_tool %in% firstTr ~ 'First',
                              trialN_tool %in% lastTr ~ 'Last',
                              TRUE ~ 'NA')) %>%
    #rearrange otherwise geom_line and geom_point for individual 
    #data points don't have the same jitter and aren't aligned
    arrange(expe_phase, trialN, ppid, tool_used) 
  
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = trialN_tool, y = launch_angle_err_dir, 
                    color = tool_used, fill = tool_used)) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(-30, 30), linetype = 'dashed', color = 'grey40') + 
    # facet_grid(. ~ trialN_tool) + 
    facet_grid(. ~ expe_phase + trialN) + 
    #lines between individual data points
    geom_line(aes(x = tool_used, group = ppid),
              color = 'grey50', alpha = 0.4,
              position = position_jitter(width = 0.1, seed = 1)) + 
    #boxplots for each tool used
    geom_boxplot(aes(x = tool_used),
                 outlier.shape = NA, color = 'black', alpha = 0.4) + 
    #individual data points
    geom_point(aes(x = tool_used), size = 2,
               position = position_jitter(width = 0.1, seed = 1)) + 
    
    theme_classic_article() +
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols) +
    scale_fill_manual(name = 'Tool', values = myCols) +
    scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0.02, 0)) + 
    labs(x = 'Trials per tool', y = 'Opposite to perturbation <-- Angular error (°) --> Same as perturbation') 
  
  print(plt)
  
}



#custom theme for ggplot ----

theme_classic_article <- function(){
  
  theme_classic() %+replace%
    
    theme(
      
      #text elements
      text = element_text(size = 12),
      
      #legend elements
      legend.position = 'top',
      
      # #margins around plot (top, right, bottom, left)
      # plot.margin = unit(c(10,15,0,15), 'pt'),
      
      #axes elements
      # axis.title.x.bottom = element_text(margin = margin(10,0,0,0, 'pt')), #add top margin to x label
      # axis.title.y.left = element_text(margin = margin(0,10,0,0, 'pt')), #add right margin to y label
      axis.text   = element_text(color = 'grey40'),
      axis.line   = element_line(color = 'grey40'),
      axis.ticks  = element_line(color = 'grey40'),
      axis.ticks.length = unit(5, 'pt')
      
    )
  
}

