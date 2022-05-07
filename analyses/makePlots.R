source('analyses/shared.R')


#plots individual data ----

plotLauchErrIndiv <- function (df) {
  
  #to save figures as a pdf doc
  pdf('./docs/LaunchAngleError_indiv.pdf', width = 12, height = 9)
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
         xlab = 'Trials', ylab = 'Left <--- Angular error (°) ---> Right',
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

plotAdapt_all <- function(df) {
  
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
      
      theme_classic_article() +
      scale_color_discrete(name = 'Experimental phase') +
      scale_fill_discrete(name = 'Experimental phase') +
      labs(title = sprintf('%s (N = %s)', expe_v, Npp), 
           x = 'Trials', y = 'Angular error (°)')
    
    print(plot[[i]])
    
  }
  
}



plotAdapt_tool <- function(df) {
  
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
                  linetype = 0, alpha = 0.4) +
      geom_line(size = 0.8) + 
      
      theme_classic_article() +
      scale_color_discrete(name = 'Experimental phase') +
      scale_fill_discrete(name = 'Experimental phase') +
      scale_y_continuous(breaks = seq(0, 30, 15), expand = c(0, 0)) + 
      labs(title = sprintf('%s (N = %s)', expe_v, Npp), 
           x = 'Trials', y = 'Angular error (°)') + 
      #make drawings unconfined to the plot panel
      coord_cartesian(clip = 'off')
    
    print(plot[[i]])
    
  }
  
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

