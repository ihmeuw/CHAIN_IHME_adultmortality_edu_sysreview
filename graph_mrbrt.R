################## PLOTTING MODEL RUNS ##########################################################################
## Sourced from 02_meta_regression.R
library(ggbeeswarm, lib.loc =  '/FILEPATH/')
pacman::p_load(ggforce)
calc_summary_means <- function(c.data,var,by_var){
  data <- copy(c.data)
  data[get(var)>-1&get(var)<1,obs_mean := mean(get(var)), by = c(by_var)]
  data[get(var)>-1&get(var)<1,obs_sd := sd(get(var)), by = c(by_var)]
  data[get(var)>-1&get(var)<1,obs_n := .N, by = c(by_var)]
  
  data[,low_slope := obs_mean - qnorm(0.975)*obs_sd/sqrt(obs_n), by = c(by_var)]
  data[,up_slope := obs_mean + qnorm(0.975)*obs_sd/sqrt(obs_n), by = c(by_var)]
  return(data)
}

### Interactive ########################
### Graph Diagnostic and Publication Curves ################################################################################################################
  message('Graph')
  ## visualize knot locations
  if(am.model_version %like% 'Spline'){
    length <- length(mod1$cov_models)
    knot_locations <- round(mod1$cov_models[[length]]$spline_knots,2)
    knot_string <- paste(knot_locations,collapse = ", ")
  } else{
    knot_string <- '6.5,12.5'
    knot_locations <- c(6.5,12.5)
  }
  
  pdf(paste0(mod_viz_dir,"Diagnostics_Risk_Curves_",c.run.type,"_",Sys.Date(),".pdf"),width=16,height=9)
  # fully controlled with dots
  gg0 <- ggplot() +
    aesth+
    geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
    geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
    geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
    geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0, color = 'darkgreen')+
    geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = 'darkgreen', color = 'darkgreen')+
    geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',data=df_pred2[cohort_exp_def_lower==6],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3))))+
    geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white', data=df_pred2[cohort_exp_def_lower==12],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3))))+
    geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white', data=df_pred2[cohort_exp_def_lower==18],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3))))+
    scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
    scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
    scale_size_continuous(range = c(0.5,50),guide = 'none')+
    theme(legend.position='top')+
    labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", title = paste0(c.run.type),caption = paste0(am.model_version,' - ', c.run.type), subtitle =paste0("Relative Risk"))+
    scale_fill_discrete("")+
    scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
  
  print(gg0)
  
  # individual controls off/on diagnostic
  for (c.var in c(conf.list)){
    plot.dat <- df_pred1[type==c.var]
    plot.dat[get(c.var)==0,presence_n:="Not Controlled"]
    plot.dat[get(c.var)==1,presence_n:="Controlled"]
    
    gg1 <- ggplot() +
      aesth+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
      geom_line(data = plot.dat, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(presence_n)), linewidth=1.0)+
      geom_ribbon(data = plot.dat, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                 color=factor(presence_n),fill=factor(presence_n)), alpha=.4)+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==6],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==12],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==18],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position='top')+
      labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), subtitle =paste0("Relative Risk With and Without Controlling for: ",trimws(str_to_title(gsub("confounders|_"," ",c.var)))))+
      scale_fill_discrete("")+
      scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
    
    print(gg1)
    
  }
  
  gg2b2 <- ggplot() +
    aesth+
    geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
    geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr_ln,size = (1/se_ln), color = factor(outlier)), shape = 1, alpha = 0.2)+ #,color = factor(outlier)
    geom_line(data = df_pred2, mapping = aes(y=(rr_pred),x=cohort_exp_def_lower), size=1.0, color = 'darkgreen')+
    geom_ribbon(data = df_pred2, mapping = aes(y=(rr_pred),ymin=(rr_pred_lwr),ymax=(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = 'darkgreen', color = 'darkgreen')+
    geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',
                     data=df_pred2[cohort_exp_def_lower==6],aes(y=(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round((rr_pred),3))))+
    geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',
                     data=df_pred2[cohort_exp_def_lower==12],aes(y=(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round((rr_pred),3))))+
    geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',
                     data=df_pred2[cohort_exp_def_lower==18],aes(y=(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round((rr_pred),3))))+
    scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
    scale_y_continuous(breaks = seq(-3,1.4,0.2), labels = seq(-3,1.4,0.2), limits = c(-3,1.4)) +
    scale_size_continuous(range = c(0.5,25),guide = 'none')+
    theme(legend.position='top')+
    labs(y="Log RR",x="Years of Schooling", title = paste0(c.run.type, ' (log space)'), subtitle = paste0("Knots: ",knot_string), caption = paste0(am.model_version,' - ', c.run.type))+
    scale_fill_discrete("")+
    scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
  
  print(gg2b2)
  
  # age groups
  if (predict_for_groups == TRUE){
    gg2 <- ggplot() + 
      aesth+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
      geom_line(data = agg_df_pred4, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(group)), linewidth=1.0)+
      geom_ribbon(data = agg_df_pred4, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                     color=factor(group),fill=factor(group)), alpha=.4)+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',data=agg_df_pred4[cohort_exp_def_lower==6],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)), fill = group))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',data=agg_df_pred4[cohort_exp_def_lower==12],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)), fill = group))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',data=agg_df_pred4[cohort_exp_def_lower==18],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)), fill = group))+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
      scale_y_continuous(breaks = seq(0,1.1,0.2), labels = seq(0,1.1,0.2), limits = c(0,1.1)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position='top')+
      labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", 
           title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), 
           subtitle = paste0("Relative risk predicting for aggregated age-groups"))+
      scale_fill_discrete("")+
      scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
    
    print(gg2)
    
    for (c.age in seq(20,95,5)){
      plot.dat <- df_pred4[age5==c.age]
      plot.dat[,presence_n:=paste0('Predicting for age: ',c.age)]
      
      gg1 <- ggplot() +
        aesth+
        geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
        geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
        geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
        geom_line(data = plot.dat, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(presence_n)), size=1.0)+
        geom_ribbon(data = plot.dat, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                   color=factor(presence_n),fill=factor(presence_n)), alpha=.4)+
        geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==6],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
        geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==12],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
        geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==18],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
        scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
        scale_y_continuous(breaks = seq(0,1.1,0.2), labels = seq(0,1.1,0.2), limits = c(0,1.1)) +
        scale_size_continuous(range = c(0.5,50),guide = 'none')+
        theme(legend.position='top')+
        labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), subtitle = paste0("Relative risk predicting for age: ",c.age))+
        scale_fill_discrete("")+
        scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
      
      print(gg1)
      
    }
  
  }
  
  if (predict_for_COHORT == TRUE){
    
    gg2 <- ggplot() + 
      aesth+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
      geom_line(data = df_pred5, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(cohort)), linewidth=1.0)+
      geom_ribbon(data = df_pred5, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                     color=factor(cohort),fill=factor(cohort)), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.1)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position='top')+
      labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", 
           title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), 
           subtitle = paste0("Relative risk predicting for cohorts"))+
      scale_fill_discrete("")+
      scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
    
    print(gg2)
    
    for (c.cohort in seq(1870,1980,10)){
      plot.dat <- df_pred5[cohort==c.cohort]
      plot.dat[,presence_n:=paste0('Predicting for Cohort: ',c.cohort)]
      
      gg1 <- ggplot() +
        aesth+
        geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
        geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
        geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
        geom_line(data = plot.dat, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(presence_n)), size=1.0)+
        geom_ribbon(data = plot.dat, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                   color=factor(presence_n),fill=factor(presence_n)), alpha=.4)+
        scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
        scale_y_continuous(breaks = seq(0,1.1,0.2), labels = seq(0,1.1,0.2), limits = c(0,1.1)) +
        scale_size_continuous(range = c(0.5,50),guide = 'none')+
        theme(legend.position='top')+
        labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), subtitle = paste0("Relative risk predicting for Cohort: ",c.cohort))+
        scale_fill_discrete("")+
        scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
      
      print(gg1)
      
    }
  }
  
  if (predict_for_PERIOD == TRUE){
    
    gg2 <- ggplot() + 
      aesth+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
      geom_line(data = df_pred6, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(period)), linewidth=1.0)+
      geom_ribbon(data = df_pred6, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                 color=factor(period),fill=factor(period)), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.1)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position='top')+
      labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", 
           title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), 
           subtitle = paste0("Relative risk predicting for period"))+
      scale_fill_discrete("")+
      scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
    
    print(gg2)
    
    for (c.period in seq(1870,1980,10)){
      plot.dat <- df_pred6[period==c.period]
      plot.dat[,presence_n:=paste0('Predicting for Time: ',c.period)]
      
      gg1 <- ggplot() +
        aesth+
        geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
        geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
        geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
        geom_line(data = plot.dat, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(presence_n)), size=1.0)+
        geom_ribbon(data = plot.dat, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                   color=factor(presence_n),fill=factor(presence_n)), alpha=.4)+
        scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
        scale_y_continuous(breaks = seq(0,1.1,0.2), labels = seq(0,1.1,0.2), limits = c(0,1.1)) +
        scale_size_continuous(range = c(0.5,50),guide = 'none')+
        theme(legend.position='top')+
        labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), subtitle = paste0("Relative risk predicting for Time: ",c.period))+
        scale_fill_discrete("")+
        scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
      
      print(gg1)
      
    }
    
    
  }
  
  #SDI groups
  if (predict_for_SDI_groups == TRUE){
    gg3 <- ggplot() +
      aesth+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
      geom_ribbon(data = agg_df_pred3, mapping = aes(ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                     fill=group), alpha=.3)+
      geom_line(data = agg_df_pred3, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,linetype=group), linewidth=1.0)+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=agg_df_pred3[cohort_exp_def_lower==6],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)), fill=group))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=agg_df_pred3[cohort_exp_def_lower==12],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)), fill=group))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=agg_df_pred3[cohort_exp_def_lower==18],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)), fill=group))+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
      scale_y_continuous(breaks = seq(0,1.1,0.2), labels = seq(0,1.1,0.2), limits = c(0,1.1)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position='top')+
      labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", 
           title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), 
           subtitle = paste0("Relative risk predicting for each SDI group"))+
      scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
    
    print(gg3)
    
    # sdi groups
    for (c.SDI in names(dt)[names(dt)%like%"dummy_SDI" & !(names(dt)%like%"_int")]){
      plot.dat <- df_pred3[sdi_group==gsub('dummy_','',c.SDI)]
      plot.dat[get(c.SDI)==0,presence_n:="Reference SDI 5"]
      plot.dat[get(c.SDI)==1,presence_n:=gsub('dummy_','Predicting for ',c.SDI)]
      
      gg1 <- ggplot() +
        aesth+
        geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4, color = 'blue')+
        geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
        geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
        geom_line(data = plot.dat, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower,color=factor(presence_n)), size=1.0)+
        geom_ribbon(data = plot.dat, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower,
                                                   color=factor(presence_n),fill=factor(presence_n)), alpha=.4)+
        geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==6],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
        geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==12],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
        geom_label_repel(show.legend = F,min.segment.length = 200,direction='y', data=plot.dat[cohort_exp_def_lower==18],aes(y=exp(rr_pred),x=cohort_exp_def_lower+0.5,label=paste0(round(exp(rr_pred),3)),fill=factor(presence_n)))+
        scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
        scale_y_continuous(breaks = seq(0,1.1,0.2), labels = seq(0,1.1,0.2), limits = c(0,1.1)) +
        scale_size_continuous(range = c(0.5,50),guide = 'none')+
        theme(legend.position='top')+
        labs(y="Relative Risk of All-Cause Mortality",x="Years of Schooling", title = paste0(c.run.type), caption = paste0(am.model_version,' - ', c.run.type), subtitle =paste0("Relative risk predicting for: ",trimws(str_to_title(gsub("dummy_|_"," ",c.SDI)))))+
        scale_fill_discrete("")+
        scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'))
      
      print(gg1)
      
    }
    
  }
  
  #get presence/absence var collapsed
  for (c.var in c(conf.list)){
    df_pred1[type==c.var&get(c.var)==1,presence:="present"]
    df_pred1[type==c.var&get(c.var)==0,presence:="absent"]
  }
  
  #calculate RRs with 95% uncertainty intervals
  draws1 <- data.table(draws1)
  names(draws1) <- paste0("draw",1:1000)
  df_pred1_draws <- cbind(df_pred1[,c("type","cohort_exp_def_lower","presence")],data.table(draws1))
  df_pred1_draws <- melt.data.table(df_pred1_draws,id.vars=c("type","cohort_exp_def_lower","presence"),variable.name = "draw",value.name = "est")
  df_pred1_draws[,RR:=exp(est)]
  df_pred1_draws <- dcast.data.table(df_pred1_draws,type+cohort_exp_def_lower+draw~presence,value.var = "RR")
  df_pred1_draws[,RR_ratio:=absent/present]
  
  df_pred1_ci = df_pred1_draws[,.(RR_ratio_mean=mean(RR_ratio),
                                  RR_ratio_lwr=quantile(RR_ratio,.025),
                                  RR_ratio_upr=quantile(RR_ratio,.975)),
                               by=.(type,cohort_exp_def_lower)]
  
  df_pred1_ci[,type:=trimws(str_to_title(gsub("confounders|_"," ",type)))]
  
  #plot the relative risks of each predictor
  gg4 <- ggplot(df_pred1_ci[cohort_exp_def_lower==18], aes(x=type,y=RR_ratio_mean,ymin=RR_ratio_lwr,ymax=RR_ratio_upr))+
    geom_pointrange(shape=21,stroke=1.2)+
    geom_hline(aes(yintercept=1),linetype="longdash",alpha=.7)+
    aesth_pub + coord_flip()+scale_y_log10(breaks=seq(.7,1.5,.05))+
    labs(x="Model Covariate",y="Relative Risk Ratio (Log Scale)")
  
  print(gg4)
  
  dev.off()
  
  # publication plots
  if(subset_mod == FALSE){ #specify color of global model for pub (blue)
    # Global mod with data points
    gglobal_01a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0, color = '#357EBDFF')+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = '#357EBDFF')+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.1, 0.9),legend.background = element_rect(fill = "white"))+ guides(color = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))+
      scale_fill_discrete("")+
      scale_color_manual(values = c('1' = 'red', '0' = 'black'), labels = c('1' = 'Outlier', '0' = 'Inlier'))
    
    gglobal_02a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, shape = factor(outlier)), alpha = 0.5)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0, color = '#357EBDFF')+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = '#357EBDFF')+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.1, 0.9),legend.background = element_rect(fill = "white"))+ guides(shape = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))+
      scale_shape_manual(values = c('1' = 4, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))
    
    gglobal_03a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, shape = factor(outlier),color = factor(outlier)), alpha = 0.5)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0, color = '#357EBDFF')+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = '#357EBDFF')+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))+
      scale_shape_manual(name = 'Outliers',values = c('1' = 4, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))+
      scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'), labels = c('1' = 'Outlier', '0' = 'Inlier'))+
      theme(legend.position = c(0.1, 0.9),legend.background = element_rect(fill = "white"), legend.title = element_blank())
    
    gglobal_04a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, shape = factor(outlier), color = super_region_name), alpha = 0.5)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))+
      theme(legend.position = c(0.2, 0.2),legend.background = element_rect(fill = "white"),legend.text = element_text(size = 12),legend.key.height = unit(0.2, "cm"))+ 
      guides(colour = guide_legend(ncol=1,title = NULL), shape = guide_legend(title = NULL))+
      scale_shape_manual(name = 'Outliers',values = c('1' = 4, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))+
      scale_colour_manual(name = 'Super Region',values=c('Central Europe, Eastern Europe, and Central Asia' = '#E41A1C','North Africa and Middle East'='#377EB8','High-income'='#4DAF4A','Sub-Saharan Africa'='#984EA3','South Asia'='#FF7F00','Latin America and Caribbean'='#f781bf','Southeast Asia, East Asia, and Oceania'='#A65628'))
    
    gglobal_05a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, color = super_region_name), alpha = 0.5, shape = 1)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.2, 0.2), legend.background = element_rect(fill = "white"), legend.text = element_text(size = 12),legend.key.height = unit(0.2, "cm"))+ 
      guides(colour = guide_legend(ncol=1,title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))+
      scale_colour_manual(name = 'Super Region',values=c('Central Europe, Eastern Europe, and Central Asia' = '#E41A1C','North Africa and Middle East'='#377EB8','High-income'='#4DAF4A','Sub-Saharan Africa'='#984EA3','South Asia'='#FF7F00','Latin America and Caribbean'='#f781bf','Southeast Asia, East Asia, and Oceania'='#A65628'))
    
    myPalette <- colorRampPalette((brewer.pal(11, "Spectral")))
    gglobal_06a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, color = sdi), alpha = 0.5, shape = 1)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.2, 0.15),legend.text = element_text(size = 12),legend.title = element_text(size = 12))+ 
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))+
      guides(color = guide_colorbar(barwidth = 8, direction = 'horizontal'))+
      scale_colour_gradientn(name = 'Sociodemographic Index',colours = myPalette(100), limits=c(min(dt$sdi), max(dt$sdi)), breaks = c(seq(round(min(dt$sdi),2),round(max(dt$sdi),2),0.2)))
    
    gglobal_nodata_pub <- ggplot() +
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_line(data = df_pred2, mapping = aes(y=RR,x=cohort_exp_def_lower,color=Model), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(ymin=RR_lwr,ymax=RR_upr,x=cohort_exp_def_lower, color = Model, fill = Model), alpha=.4,show.legend = FALSE )+
      scale_x_continuous(breaks=seq(0,19,2), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0.4,1,0.2), labels = seq(0.4,1,0.2), limits = c(0.4,1.01)) +
      aesth_pub+theme(legend.position='none')+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))+
      scale_color_manual(values = c('Global'='#357EBDFF'))+
      scale_fill_manual(values = c('Global'='#357EBDFF'))
    
    pdf(paste0(mod_viz_dir,"Risk_curves_",c.run.type,"_",Sys.Date(),".pdf"),width=12,height=6)
    print(gglobal_01a_pub)
    print(gglobal_02a_pub)
    print(gglobal_03a_pub)
    print(gglobal_04a_pub)
    print(gglobal_05a_pub)
    print(gglobal_06a_pub)
    print(gglobal_nodata_pub)
    
    print(gglobal_01a_pub +
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    print(gglobal_02a_pub + # final figure main curve
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    print(gglobal_03a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    print(gglobal_04a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2)))))
    print(gglobal_05a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2)))))
    print(gglobal_06a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2)))))
    print(gglobal_nodata_pub+ 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    
    dev.off()
    
    dt[,age_name := age_group]
    dt[age_name=='15 to 49', age_name := '18 to 49']
    dt[,age_name := paste0('Age ',age_name)]
    
    # combine multiple PREDICTION SCENARIOS from one model
    graph_dat <- copy(agg_df_pred4)
    graph_dat <- graph_dat[pred_type=='Age Groups']
    graph_dat[,group := paste0('Age ',group)]
    graph_dat[,group := factor(group, levels = c('Age 18 to 49','Age 50 to 59','Age 60 to 69', 'Age > 70'))]
    
    ggage1 <- ggplot() +
      aesth_pub+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_line(data = graph_dat, mapping = aes(y=RR,x=cohort_exp_def_lower,color=group), size=1.0)+
      geom_ribbon(data = graph_dat, mapping = aes(ymin=RR_lwr,ymax=RR_upr,x=cohort_exp_def_lower, color = group, fill = group), alpha=.4,show.legend = FALSE )+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0.4,1,0.2), labels = seq(0.4,1,0.2), limits = c(0.4,1.01)) +
      theme(legend.position = c(0.15, 0.15),legend.text = element_text(size = 14))+ 
      guides(color = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0('Predicting with different Age reference categories - ', am.model_version))
    
    ggage2 <- ggage1 +
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 5,
                       data=graph_dat[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5,color = group, label=paste0(round(RR,2))))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 5,
                       data=graph_dat[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5,color = group, label=paste0(round(RR,2))))+
      geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 5,
                       data=graph_dat[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5,color = group, label=paste0(round(RR,2))))
  
  
    
    pdf(paste0(mod_viz_dir,"Age_preds_",am.model_version,".pdf"),width=10,height=6)
    print(ggage1)
    print(ggage2)
    dev.off()
    
    # break out age groups
    ggage_indiv1 <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt[age_name=='Age 18 to 49'], mapping = aes(x = cohort_exp_def_mid, y = rr), alpha = 0.5, shape = 1)+ 
      geom_line(data = graph_dat[group == 'Age 18 to 49'], mapping = aes(y=RR,x=cohort_exp_def_lower),color = '#F8766D', size=1.0)+
      geom_ribbon(data = graph_dat[group == 'Age 18 to 49'], mapping = aes(ymin=RR_lwr,ymax=RR_upr,x=cohort_exp_def_lower),color = '#F8766D',fill = '#F8766D', alpha=.4,show.legend = FALSE )+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.15, 0.15),legend.background = element_rect(fill = "white"))+ guides(color = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))
    
    ggage_indiv2 <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt[age_name=='Age 50 to 59'], mapping = aes(x = cohort_exp_def_mid, y = rr), alpha = 0.5, shape = 1)+ 
      geom_line(data = graph_dat[group == 'Age 50 to 59'], mapping = aes(y=RR,x=cohort_exp_def_lower),color = '#7CAE00', size=1.0)+
      geom_ribbon(data = graph_dat[group == 'Age 50 to 59'], mapping = aes(ymin=RR_lwr,ymax=RR_upr,x=cohort_exp_def_lower),color = '#7CAE00',fill = '#7CAE00', alpha=.4,show.legend = FALSE )+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.15, 0.15),legend.background = element_rect(fill = "white"))+ guides(color = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))
    
    ggage_indiv3 <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt[age_name=='Age 60 to 69'], mapping = aes(x = cohort_exp_def_mid, y = rr), alpha = 0.5, shape = 1)+ 
      geom_line(data = graph_dat[group == 'Age 60 to 69'], mapping = aes(y=RR,x=cohort_exp_def_lower),color = '#00BFC4', size=1.0)+
      geom_ribbon(data = graph_dat[group == 'Age 60 to 69'], mapping = aes(ymin=RR_lwr,ymax=RR_upr,x=cohort_exp_def_lower),color = '#00BFC4',fill = '#00BFC4', alpha=.4,show.legend = FALSE )+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.15, 0.15),legend.background = element_rect(fill = "white"))+ guides(color = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))
    
    ggage_indiv4 <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt[age_name=='Age > 70'], mapping = aes(x = cohort_exp_def_mid, y = rr), alpha = 0.5, shape = 1)+ 
      geom_line(data = graph_dat[group == 'Age > 70'], mapping = aes(y=RR,x=cohort_exp_def_lower),color = '#C77CFF', size=1.0)+
      geom_ribbon(data = graph_dat[group == 'Age > 70'], mapping = aes(ymin=RR_lwr,ymax=RR_upr,x=cohort_exp_def_lower),color = '#C77CFF',fill = '#C77CFF', alpha=.4,show.legend = FALSE )+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.15, 0.15),legend.background = element_rect(fill = "white"))+ guides(color = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ', c.run.type))
    
    dev.off()
    pdf(paste0(mod_viz_dir,"Age_preds_indiv_",am.model_version,".pdf"),width=8,height=16)
    plot_grid(ggage_indiv4,ggage_indiv3,ggage_indiv2,ggage_indiv1,ncol=1)
    dev.off()
    
  }
  
  if(subset_mod == TRUE){ 
  
    # Global mod with data points
    gglobal_01a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(outlier)), shape = 1, alpha = 0.2)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0, color = '#357EBDFF')+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = '#357EBDFF')+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.1, 0.9),legend.background = element_rect(fill = "white"))+ guides(color = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ',c.run.type))+
      scale_fill_discrete("")+
      scale_color_manual(values = c('1' = 'red', '0' = 'black'), labels = c('1' = 'Outlier', '0' = 'Inlier'))
    
    gglobal_02a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, shape = factor(outlier)), alpha = 0.5)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0, color = '#357EBDFF')+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = '#357EBDFF')+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.1, 0.9),legend.background = element_rect(fill = "white"))+ guides(shape = guide_legend(title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ',c.run.type))+
      scale_shape_manual(values = c('1' = 4, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))
    
    gglobal_03a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, shape = factor(outlier),color = factor(outlier)), alpha = 0.5)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0, color = '#357EBDFF')+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4, fill = '#357EBDFF')+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ',c.run.type))+
      scale_shape_manual(name = 'Outliers',values = c('1' = 4, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))+
      scale_color_manual(name = 'Outliers',values = c('1' = 'red', '0' = 'black'), labels = c('1' = 'Outlier', '0' = 'Inlier'))+
      theme(legend.position = c(0.1, 0.9),legend.background = element_rect(fill = "white"), legend.title = element_blank())
    
    gglobal_04a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, shape = factor(outlier), color = super_region_name), alpha = 0.5)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ',c.run.type))+
      theme(legend.position = c(0.2, 0.2),legend.background = element_rect(fill = "white"),legend.text = element_text(size = 12),legend.key.height = unit(0.2, "cm"))+ 
      guides(colour = guide_legend(ncol=1,title = NULL), shape = guide_legend(title = NULL))+
      scale_shape_manual(name = 'Outliers',values = c('1' = 4, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))+
      scale_colour_manual(name = 'Super Region',values=c('Central Europe, Eastern Europe, and Central Asia' = '#E41A1C','North Africa and Middle East'='#377EB8','High-income'='#4DAF4A','Sub-Saharan Africa'='#984EA3','South Asia'='#FF7F00','Latin America and Caribbean'='#f781bf','Southeast Asia, East Asia, and Oceania'='#A65628'))
    
    gglobal_05a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, color = super_region_name), alpha = 0.5, shape = 1)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.2, 0.2), legend.background = element_rect(fill = "white"), legend.text = element_text(size = 12),legend.key.height = unit(0.2, "cm"))+ 
      guides(colour = guide_legend(ncol=1,title = NULL))+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ',c.run.type))+
      scale_colour_manual(name = 'Super Region',values=c('Central Europe, Eastern Europe, and Central Asia' = '#E41A1C','North Africa and Middle East'='#377EB8','High-income'='#4DAF4A','Sub-Saharan Africa'='#984EA3','South Asia'='#FF7F00','Latin America and Caribbean'='#f781bf','Southeast Asia, East Asia, and Oceania'='#A65628'))
    
    myPalette <- colorRampPalette((brewer.pal(11, "Spectral")))
    gglobal_06a_pub <- ggplot() +
      aesth_pub+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_jitter(data = dt, mapping = aes(x = cohort_exp_def_mid, y = rr, color = sdi), alpha = 0.5, shape = 1)+ 
      geom_line(data = df_pred2, mapping = aes(y=exp(rr_pred),x=cohort_exp_def_lower), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(y=exp(rr_pred),ymin=exp(rr_pred_lwr),ymax=exp(rr_pred_upr),x=cohort_exp_def_lower), alpha=.4)+
      scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
      scale_size_continuous(range = c(0.5,50),guide = 'none')+
      theme(legend.position = c(0.2, 0.15),legend.text = element_text(size = 12),legend.title = element_text(size = 12))+ 
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ',c.run.type))+
      guides(color = guide_colorbar(barwidth = 8, direction = 'horizontal'))+
      scale_colour_gradientn(name = 'Sociodemographic Index',colours = myPalette(100), limits=c(min(dt$sdi), max(dt$sdi)), breaks = c(seq(round(min(dt$sdi),2),round(max(dt$sdi),2),0.2)))
    
    gglobal_nodata_pub <- ggplot() +
      geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
      geom_vline(xintercept = knot_locations, linetype='longdash',alpha=.4)+
      geom_line(data = df_pred2, mapping = aes(y=RR,x=cohort_exp_def_lower,color=Model), linewidth=1.0)+
      geom_ribbon(data = df_pred2, mapping = aes(ymin=RR_lwr,ymax=RR_upr,x=cohort_exp_def_lower, color = Model, fill = Model), alpha=.4,show.legend = FALSE )+
      scale_x_continuous(breaks=seq(0,19,2), limits = c(0,18.99),expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0.4,1,0.2), labels = seq(0.4,1,0.2), limits = c(0.4,1.01)) +
      aesth_pub+theme(legend.position='none')+
      labs(y="RR",x="Education completed (years)", caption = paste0(am.model_version,' - ',c.run.type))+
      scale_color_manual(values = c('Male'='#00BFC4','Female'='#F8766D'))+
      scale_fill_manual(values = c('Male'='#00BFC4','Female'='#F8766D'))
    
    pdf(paste0(mod_viz_dir,"Risk_curves_",c.run.type,"_",Sys.Date(),".pdf"),width=12,height=6)
    print(gglobal_01a_pub)
    print(gglobal_02a_pub)
    print(gglobal_03a_pub)
    print(gglobal_04a_pub)
    print(gglobal_05a_pub)
    print(gglobal_06a_pub)
    print(gglobal_nodata_pub)
    
    print(gglobal_01a_pub +
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    print(gglobal_02a_pub + # final figure main curve
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    print(gglobal_03a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    print(gglobal_04a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2)))))
    print(gglobal_05a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2)))))
    print(gglobal_06a_pub + 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))))+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2)))))
    print(gglobal_nodata_pub+ 
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
            geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6, data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'))
    
    dev.off()
  }

## Graph Funnels #################################################################################################
dat_pred <- mr$MRData()
dat_pred$load_df(
  data = dt,
  col_covs=c(c.covariate_list,
             'age_int',
             "cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
dt[,rr_pred :=  mod1$predict(dat_pred,predict_for_study = FALSE, sort_by_data_id = TRUE)]
dt[,residual :=  rr_ln - rr_pred] #can i use beta from the model, given the way I utilize covariates?

pdf(paste0(mod_viz_dir,"Pub_funnel_",c.run.type,"_",Sys.Date(),".pdf"),width=10,height=6)
metafor::funnel(x = dt$residual, sei = dt$se_ln,ylim = c(0,2),  #https://towardsdatascience.com/constructing-contour-enhanced-funnel-plots-for-meta-analysis-6434cc8e51d0
                refline=0, level=c(90, 95, 99),shade=c("white", "gray", "darkgray"),pch=21, col=ifelse(dt$outlier > 0, "red", "black"),
                legend = FALSE,
                ylab = 'Standard Error of Observation',
                xlab = 'Residual (normalized RR)',
                hlines = 'black')
legend("topright", 
       bty = "0",
       c("p < 0.10", "p < 0.05", "p < 0.01", 'Outlier','Inlier'),
       fill=c("white", "gray", "darkgray",'red','black'))

dev.off()

### Graph beeswarm (main and appendix) #################################################################################

if(subset_mod == FALSE){
  graph_dat <- copy(dt)
  graph_dat[age_group=='15 to 49', age_group := '18 to 49']
  graph_dat[,datadummy := 'data']

  cleaned_temp1 <- calc_summary_means(graph_dat,'obs_slope','super_region_name')
  cleaned_temp_global <- calc_summary_means(graph_dat,'obs_slope','datadummy')
  cleaned_temp_global[,super_region_name := 'All']
  cleaned_temp_global[,exp_cat1 := 'All']
  cleaned_temp1 <- rbind(cleaned_temp1,cleaned_temp_global, fill=T)  
  
  pdf(paste0(mod_viz_dir,"Beeswarm_region_",c.run.type,"_",Sys.Date(),".pdf"),width=16,height=9)
  # facet by superregion
  ggbee_supreg <- ggplot(cleaned_temp1[super_region_name != 'Multiple Regions' &!(is.na(region_name))&region_name!='']) + 
    geom_beeswarm(aes(x = datadummy, y = obs_slope, color = as.factor(outlier)), alpha = 0.5, cex = 2) +
    geom_errorbar(aes(x = datadummy, y = obs_mean, ymin=low_slope, ymax=up_slope),  width = 1, color = 'black', size = 1)+
    geom_hline(yintercept = 0, linetype = "dotted") + 
    ylab("Normalized Log(RR)")+ 
    scale_color_manual(name = '', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
    facet_grid_paginate(~str_wrap(super_region_name,15))+
    ylim(-.25, .25) +
    aesth_pub + theme(axis.text.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.title.x = element_blank(),
                      legend.position = 'top')
  print(ggbee_supreg)
  
  # facet by superregion and region
  cleaned_temp1 <- calc_summary_means(graph_dat,'obs_slope','region_name')
  for(c.supregion in unique(cleaned_temp1[super_region_name != 'Multiple Regions']$super_region_name)){
    gg1 <- ggplot(cleaned_temp1[super_region_name == c.supregion&!(is.na(region_name))&region_name!='']) + 
      geom_beeswarm(aes(x = super_region_name, y = obs_slope, color = as.factor(outlier)), alpha = 0.5, cex = 2) +
      geom_errorbar(aes(y=obs_mean,ymin=low_slope,ymax=up_slope,x=super_region_name), 
                    width = 0.5, color = 'black', size = 1)+
      geom_hline(yintercept = 0, linetype = "dotted") + 
      ylab("Normalized Log(RR)")+ 
      #ggtitle("Regularized Log Relative Risks, Superimposed With\nAverage Effect Sizes")+
      scale_color_manual(name = 'Outliers', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
      facet_grid_paginate(super_region_name~str_wrap(region_name,15))+
      #facet_grid(~str_wrap(region_name,10)) + 
      ylim(-.25, .25) +scale_alpha(guide = 'none')+
      aesth_pub + theme(axis.text.x = element_blank(), 
                        axis.ticks.x = element_blank(), 
                        axis.title.x = element_blank(),
                        legend.position = 'top')
    print(gg1)
  }
  dev.off()
  
  pdf(paste0(mod_viz_dir,"Beeswarm_region_LPH_",c.run.type,"_",Sys.Date(),".pdf"),width=18,height=9)
  gg1a <- ggplot(data=cleaned_temp1[!(is.na(region_name))&region_name!='']) +
    geom_beeswarm(aes(y=obs_slope,x=str_wrap(region_name),color = as.factor(outlier)),alpha=.4, cex = 0.5) +#
    geom_hline(yintercept = 0, linetype = "dotted") + 
    facet_grid(~str_wrap(super_region_name,15), scales = 'free_x',space="free_x")+
    labs(y="Ln(RR) per additional year of education",x="Super Region (Region)",title="Geography")+
    scale_y_continuous(breaks=seq(-1,1,.25)) +
    scale_color_manual(name = 'Outliers', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
    ylim(-.25, .25) +scale_alpha(guide = 'none')+
    aesth_pub+ theme(legend.position = 'top',axis.text.x=element_text(angle = 45, hjust = 1,size = 12),
                 strip.text = element_text(size=12,face='bold'))
print(gg1a)
   dev.off()
 
  
  # facet by exposure
  graph_dat[,exp_cat := (floor(cohort_exp_def_mid/2)*2)]
  graph_dat[,exp_cat1 := paste0(exp_cat,'-',exp_cat+1.99)]
  graph_dat[exp_cat1 == '18-19.99', exp_cat1 := '18-18.99']
  table(graph_dat$exp_cat1)
  
  cleaned_temp2 <- calc_summary_means(graph_dat,'obs_slope','exp_cat1')
  cleaned_temp2 <- rbind(cleaned_temp2,cleaned_temp_global, fill = T)

  graph_dat[,exp_cat2 := cut(cohort_exp_def_mid, c(0,6,9,12,18.99), labels = c('Primary School','Lower Secondary','Upper Secondary','Tertiary'))]
  cleaned_temp3 <- calc_summary_means(graph_dat,'obs_slope','exp_cat2')
  
  graph_dat[,exp_cat3 := cut(cohort_exp_def_mid, c(0,6,9,12,16,18.99), labels = c('Primary School','Lower Secondary','Upper Secondary','Graduate','Post-graduate'))]
  cleaned_temp4 <- calc_summary_means(graph_dat,'obs_slope','exp_cat3')
  
  
  pdf(paste0(mod_viz_dir,"Beeswarm_exposure_group_",c.run.type,"_",Sys.Date(),".pdf"),width=16,height=9)
  ggbee_exp <- ggplot(cleaned_temp2[exp_cat!=22 & exp_cat!=0]) + 
    geom_beeswarm(aes(x = datadummy, y = obs_slope, color = as.factor(outlier)), alpha = 0.5, cex = 2) +
    geom_errorbar(aes(x = datadummy, y = obs_mean, ymin=low_slope, ymax=up_slope),  width = 1, color = 'black', size = 1)+
    geom_hline(yintercept = 0, linetype = "dotted") + 
    ylab("Normalized Log(RR)")+ 
    scale_color_manual(name = '', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
    facet_grid(~factor(exp_cat1, levels = c("All","2-3.99","4-5.99","6-7.99","8-9.99","10-11.99","12-13.99","14-15.99","16-17.99","18-18.99")))+
    ylim(-.25, .25) +
    aesth_pub + theme(axis.text.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.title.x = element_blank(),
                      legend.position = 'top')
  print(ggbee_exp)
  
  ggbee_exp2 <- ggplot(cleaned_temp3[!is.na(exp_cat2)]) + 
    geom_beeswarm(aes(x = datadummy, y = obs_slope, color = as.factor(outlier)), alpha = 0.5, cex = 2) +
    geom_errorbar(aes(x = datadummy, y = obs_mean, ymin=low_slope, ymax=up_slope),  width = 1, color = 'black', size = 1)+
    geom_hline(yintercept = 0, linetype = "dotted") + 
    ylab("Normalized Log(RR)")+ 
    scale_color_manual(name = '', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
    facet_grid(~factor(exp_cat2, levels = c('Primary School','Lower Secondary','Upper Secondary','Tertiary')))+
    ylim(-.25, .25) +
    aesth_pub + theme(axis.text.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.title.x = element_blank(),
                      legend.position = 'top')
  print(ggbee_exp2)

  ggbee_exp3 <- ggplot(cleaned_temp4[!is.na(exp_cat3)]) + 
    geom_beeswarm(aes(x = datadummy, y = obs_slope, color = as.factor(outlier)), alpha = 0.5, cex = 2) +
    geom_errorbar(aes(x = datadummy, y = obs_mean, ymin=low_slope, ymax=up_slope),  width = 1, color = 'black', size = 1)+
    geom_hline(yintercept = 0, linetype = "dotted") + 
    ylab("Normalized Log(RR)")+ 
    scale_color_manual(name = '', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
    facet_grid(~factor(exp_cat3, levels = c('Primary School','Lower Secondary','Upper Secondary','Graduate','Post-graduate')))+
    ylim(-.25, .25) +
    aesth_pub + theme(axis.text.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.title.x = element_blank(),
                      legend.position = 'top')
  print(ggbee_exp3)
  
  dev.off()
  # now facet by age group
  cleaned_temp_age1 <- calc_summary_means(graph_dat,'obs_slope','age_group')
  cleaned_temp_age1[,age_group := factor(age_group, levels = c('18 to 49','50 to 59','60 to 69', '> 70'))]
  
  #by AGE GROUP
  pdf(paste0(mod_viz_dir,"Beeswarm_agegroup_",c.run.type,"_",Sys.Date(),".pdf"),width=16,height=9)
  ggbee1 <- ggplot(cleaned_temp_age1[!(is.na(region_name))&region_name!='']) + 
    geom_beeswarm(aes(x = datadummy, y = obs_slope, color = as.factor(outlier), alpha = 0.5), cex = 2) +
    geom_errorbar(aes(y=obs_mean,ymin=low_slope,ymax=up_slope,x=datadummy), width = 0.5, color = 'black', size = 1)+
    geom_hline(yintercept = 0, linetype = "dotted") + 
    ylab("Normalized Log(RR)")+ 
    scale_color_manual(name = 'Outliers', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
    facet_grid_paginate(~age_group)+
    ylim(-.25, .25) + scale_alpha(guide = 'none')+
    aesth_pub + theme(axis.text.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.title.x = element_blank(),
                      legend.position = 'top')
  
  print(ggbee1)
  
  # 10 year age group
  cleaned_temp_age2 <- calc_summary_means(graph_dat,'obs_slope','age10_mid_name')
  cleaned_temp_age2[,age10_mid_name := factor(age10_mid_name,levels = c('18-29', '30-39','40-49','50-59','60-69','70-79','80-89','90-99'))]
  ggbee2 <- ggplot(cleaned_temp_age2[!(is.na(region_name))&region_name!='']) + 
    geom_beeswarm(aes(x = datadummy, y = obs_slope, color = as.factor(outlier)),alpha = 0.6, cex = 2) +
    geom_errorbar(aes(y=obs_mean,ymin=low_slope,ymax=up_slope,x=datadummy), 
                  width = 0.5, color = 'black', size = 1)+
    geom_hline(yintercept = 0, linetype = "dotted") + 
    ylab("Normalized Log(RR)")+ 
    scale_color_manual(name = 'Outliers', values = c('1'='#F8766D', '0' = '#00BFC4'),labels = c('1'='Outlier', '0' = 'Inlier'))+
    facet_grid_paginate(~paste0('Age ',age10_mid_name))+
    ylim(-.25, .25) +scale_alpha(guide = 'none')+
    aesth_pub + theme(axis.text.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.title.x = element_blank(),
                      legend.position = 'top')
  
  print(ggbee2)
  
  dev.off()
  
}

## Graph Dose Response (Raw log space w confounder means) ########################################################################
if(subset_mod == FALSE) {
  
  dt[age_group=='15 to 49', age_group := '18 to 49']
  dt[confounders_age==0&confounders_sex==0&confounders_marital_status==0,control := 'Unadjusted']
  dt[confounders_age==1&confounders_sex==1&confounders_marital_status==1,control := 'Age, Sex, and Marital Status']
  dt[!is.na(control),control_effect := mean(obs_slope), by = 'control']
  dt[!is.na(control),control_effect_age := mean(obs_slope), by = c('control','age_group')]
  
  gg_dr <- ggplot(dt)+
    geom_jitter(aes(x=cohort_exp_def_mid, y = obs_slope, size = 1/se_ln, shape = factor(outlier)), alpha = 0.4)+
    geom_hline(aes(yintercept = 0), linetype ='longdash')+
    geom_hline(aes(color = 'red', yintercept = mean(dt[confounders_age==0&confounders_sex==0&confounders_marital_status==0]$obs_slope)), linewidth = 1)+ #no controls
    geom_hline(aes(color = 'blue', yintercept = mean(dt[confounders_age==1&confounders_sex==1&confounders_marital_status==1]$obs_slope)), linewidth = 1)+ #age and sex and marital status
    ylab ('Ln(RR) per additional year of education') + 
    xlab('Education completed (years)')+
    scale_x_continuous(breaks=seq(0,19,1), limits = c(0,18.99)) +
    scale_y_continuous(breaks=seq(-0.5,0.3, 0.1), limits = c(-0.5,0.3)) +
    scale_size_continuous(name = expression(frac('1','SE of Ln(RR)')), range = c(0.1,10))+
    scale_shape_manual(name = '',values = c('1' = 13, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))+
    aesth_pub +
    scale_color_identity(name = "Control Level",
                         breaks = c("red",'blue'),
                         labels = c("Unadjusted", "Age, Sex, and Marital Status"),
                         guide = "legend") + guides(color = guide_legend(ncol = 1),size = guide_legend(ncol=1),shape = guide_legend(ncol=1))
  
  
  myplots <- vector('list', length(unique(dt$age_group)))
  for (i in unique(dt$age_group)) {
    message(i)
    myplots[[i]] <- local({
      i <- i
      p1 <- ggplot(dt[age_group==i])+
        geom_jitter(aes(x=cohort_exp_def_mid, y = obs_slope, size = 1/se_ln, shape = factor(outlier)), alpha = 0.4)+
        geom_hline(aes(yintercept = 0), linetype ='longdash')+
        geom_hline(aes(color = 'red', yintercept = unique(dt[age_group==i&control=='Unadjusted']$control_effect_age)), linewidth = 1)+ #no controls
        geom_hline(aes(color = 'blue', yintercept = unique(dt[age_group==i&control=='Age, Sex, and Marital Status']$control_effect_age)), linewidth = 1)+ #age and sex and marital status
        ylab ('Ln(RR) per additional year of education') + 
        xlab('Education completed (years)')+
        scale_x_continuous(breaks=seq(0,19,2), limits = c(0,18.99)) +
        scale_y_continuous(breaks=seq(-0.5,0.3, 0.1), limits = c(-0.5,0.3)) +
        scale_size_continuous(name = expression(frac('1','SE of Ln(RR)')), range = c(0.1,10))+
        scale_shape_manual(name = '',values = c('1' = 13, '0' = 1),labels = c('1' = 'Outlier', '0' = 'Inlier'))+
        aesth_pub + ggtitle(paste0(i,' years'))+
        scale_color_identity(name = "Control Level",
                             breaks = c("red",'blue'),
                             labels = c("Unadjusted", "Age, Sex, and Marital Status"),
                             guide = "legend") + guides(color = guide_legend(ncol = 1),size = guide_legend(nrow=2),shape = guide_legend(ncol=1))     
      print(p1)
    })
  }
  leg <- get_legend( myplots$`> 70`)
  
  #dr plot
  pdf(paste0(mod_viz_dir,"Pub_dr_covs_",c.run.type,"_",Sys.Date(),".pdf"),width=12,height=12)
  print(gg_dr)
  plot_grid(plot_grid(myplots$`18 to 49`+theme(legend.position = 'none'),
                      myplots$`50 to 59`+theme(legend.position = 'none'),
                      myplots$`60 to 69`+theme(legend.position = 'none'),
                      myplots$`> 70`+theme(legend.position = 'none'),ncol=2),plot_grid(leg),ncol = 1, rel_heights=c(1, 0.2), rel_widths = c(1,1))
  
  dev.off()
  
}

### Plot effects of controls #########################################################

if(subset_mod == FALSE){
  calc_yearly_effect <- function(c.group, c.run.type){
    effects <- temp[group == c.group & cohort_exp_def_lower %in% c(0,6,12,18), .(RR,RR_lwr,RR_upr,cohort_exp_def_lower,Model,group)]
    effects <- dcast.data.table(effects, ... ~ cohort_exp_def_lower, value.var = c('RR','RR_lwr','RR_upr'), fun.aggregate = mean)
    
    effects[,year_eff := round(((RR_0-RR_18)/18)*100,2)]
    effects[,year_eff_up := round(((RR_upr_0-RR_upr_18)/18)*100,2)]
    effects[,year_eff_low := round(((RR_lwr_0-RR_lwr_18)/18)*100,2)]
    effects[,year_eff_ci := paste0(year_eff,'(',year_eff_up,'-',year_eff_low,')')]
    
    effects[,prim_eff := round((RR_0-RR_6)*100,2)]
    effects[,prim_eff_up := round((RR_upr_0-RR_upr_6)*100,2)]
    effects[,prim_eff_low := round((RR_lwr_0-RR_lwr_6)*100,2)]
    effects[,prim_eff_ci := paste0(prim_eff,'(',prim_eff_up,'-',prim_eff_low,')')]
    
    effects[,sec_eff := round((RR_0-RR_12)*100,2)]
    effects[,sec_eff_up := round((RR_upr_0-RR_upr_12)*100,2)]
    effects[,sec_eff_low := round((RR_lwr_0-RR_lwr_12)*100,2)]
    effects[,sec_eff_ci := paste0(sec_eff,'(',sec_eff_up,'-',sec_eff_low,')')]
    
    effects[,ter_eff := round((RR_0-RR_18)*100,2)]
    effects[,ter_eff_up := round((RR_upr_0-RR_upr_18)*100,2)]
    effects[,ter_eff_low := round((RR_lwr_0-RR_lwr_18)*100,2)]
    effects[,ter_eff_ci := paste0(ter_eff,'(',ter_eff_up,'-',ter_eff_low,')')]
    
    effects <- effects[,.(group,Model,year_eff,year_eff_ci,prim_eff,prim_eff_ci,sec_eff,sec_eff_ci,ter_eff,ter_eff_ci)]
    return(effects)
  }
  
  temp <- copy(df_pred1)
  temp[,mod_type := (confounders_age+confounders_sex+confounders_marital_status)]
  temp <- temp[mod_type==1 | (mod_type==0&type=='confounders_age')]
  temp[,group := type]
  temp[(mod_type==0&type=='confounders_age'), group := 'Uncontrolled']
  
  effects3 <- rbindlist(lapply(unique(temp$group), calc_yearly_effect))
  without_controls <- effects3[group=='Uncontrolled']$year_eff
  effects3[,diff := without_controls-year_eff]
  effects3[,group := str_to_title(gsub('confounders_','',group))]
  effects3[,group := (gsub('_',' ',group))]
  gg_confeffects <- ggplot(effects3[group != 'Uncontrolled'],aes(x = group, y = diff))+
    geom_col(aes(fill = '#F8766D'))+
    aesth_pub + 
    geom_hline(aes(yintercept = 0, color = '#00BFC4'), linewidth = 2)+
    geom_text(aes(label = paste0('-',round(without_controls - diff,3)), # label original values (optional)
                  vjust = ifelse(diff > 0, 0, 1)), size = 5) +
    scale_y_continuous(breaks = seq(-0.3,0.6, 0.1),
                       limits = c(-0.3,0.6),
                       labels = function(x) round(x - without_controls,2)) + # add cut.off to label values
    labs(caption='Difference in yearly reduction of mortality risk over exposure range (1-18 years), relative to no contols')+
    xlab('Study-Level Control')+
    ylab('Yearly Reduction in All-Cause Adult Mortality Risk (%)')+
    scale_color_identity(name = "",
                         breaks = c("#00BFC4"),
                         labels = c("Estimate without study-level controls"),
                         guide = "legend") +
    scale_fill_identity(name = "",
                        breaks = c("#F8766D"),
                        labels = c(" Estimate with study-level controls"),
                        guide = "legend")
  
  temp_conf <- copy(dt)
  gg_confAge <- ggplot(temp_conf) +
    aesth_pub+
    geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
    geom_hline(aes(yintercept=mean(dt[confounders_age==1]$rr)), color='#F8766D', linewidth=1.5)+
    geom_hline(aes(yintercept=mean(dt[confounders_age==0]$rr)), color ='#00BFC4', linewidth=1.5)+
    geom_jitter(mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(confounders_age)), shape = 1, alpha = 0.5)+ #,color = factor(outlier)
    scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
    scale_x_continuous(breaks = seq(0,18,2), labels = seq(0,18,2), limits = c(0,18)) +
    scale_size_continuous(range = c(0.5,50),guide = 'none')+
    scale_color_manual(name = paste0('Age'), 
                       values = c('1' = '#F8766D', '0'= '#00BFC4'), labels = c('1' = 'Controlled', '0'= 'Uncontrolled'))+
    labs(y="Relative Risk of All-Cause Mortality",x="Education Completed (years)", 
         caption = paste0(Sys.Date()))
  
  
  gg_confSex <- ggplot(temp_conf) +
    aesth_pub+
    geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
    geom_hline(aes(yintercept=mean(dt[confounders_sex==1]$rr)), color='#F8766D', linewidth=1.5)+
    geom_hline(aes(yintercept=mean(dt[confounders_sex==0]$rr)), color ='#00BFC4', linewidth=1.5)+
    geom_jitter(mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(confounders_sex)), shape = 1, alpha = 0.5)+ #,color = factor(outlier)
    scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
    scale_x_continuous(breaks = seq(0,18,2), labels = seq(0,18,2), limits = c(0,18)) +
    scale_size_continuous(range = c(0.5,50),guide = 'none')+
    scale_color_manual(name = paste0('Sex'), 
                       values = c('1' = '#F8766D', '0'= '#00BFC4'), labels = c('1' = 'Controlled', '0'= 'Uncontrolled'))+
    labs(y="Relative Risk of All-Cause Mortality",x="Education Completed (years)", 
         caption = paste0(Sys.Date()))
  
  gg_confMS <- ggplot(temp_conf) +
    aesth_pub+
    geom_hline(yintercept = 1,linetype='longdash',alpha=.5)+
    geom_hline(aes(yintercept=mean(dt[confounders_marital_status==1]$rr)), color='#F8766D', linewidth=1.5)+
    geom_hline(aes(yintercept=mean(dt[confounders_marital_status==0]$rr)), color ='#00BFC4', linewidth=1.5)+
    geom_jitter(mapping = aes(x = cohort_exp_def_mid, y = rr,size = (1/se), color = factor(confounders_marital_status)), shape = 1, alpha = 0.5)+ #,color = factor(outlier)
    scale_y_continuous(breaks = seq(0,1.5,0.2), labels = seq(0,1.5,0.2), limits = c(0,1.5)) +
    scale_x_continuous(breaks = seq(0,18,2), labels = seq(0,18,2), limits = c(0,18)) +
    scale_size_continuous(range = c(0.5,50),guide = 'none')+
    scale_color_manual(name = paste0('Marital Status'), 
                       values = c('1' = '#F8766D', '0'= '#00BFC4'), labels = c('1' = 'Controlled', '0'= 'Uncontrolled'))+
    labs(y="Relative Risk of All-Cause Mortality",x="Education Completed (years)", 
         caption = paste0(Sys.Date()))
  ggtemp <- plot_grid(gg_confAge,gg_confMS,gg_confSex, nrow = 1)
  
  # confounder plots
  pdf(paste0(mod_viz_dir,"Pub_conf_effects_covs_",c.run.type,"_",Sys.Date(),".pdf"),width=16,height=12)
  plot_grid(gg_confeffects,ggtemp, nrow = 2, rel_heights = c(2/5, 3/5))
  dev.off()
  
  pdf(paste0(mod_viz_dir,"Pub_conf_effects_covs2_",c.run.type,"_",Sys.Date(),".pdf"),width=10,height=10)
  plot_grid(gg_confeffects,gg_confAge,gg_confMS,gg_confSex, nrow = 2)
  dev.off()
  
}

## Caterpillar plots # appendix ###################################################

#old version caterpillar
if(0){
  dt_plot <- copy(dt[super_region_name !=  "Multiple Regions"])
  dt_plot[,region_count := .N, by = 'region_name']
  dt_plot[,count := 1:.N, by = 'region_name']
  dt_plot[,new_region_name := paste0(region_name,' - ',floor(count/200)+1)]
  dt_plot[,region_count2 := .N, by = 'new_region_name']
  
  #order by region and obs
  dt_plot <- dt_plot[order(super_region_name,region_name,obs_slope)]
  dt_plot[, obs_slope_upper := (log(upper)/(cohort_exp_def_mid - cohort_unexp_def_mid))]
  dt_plot[, obs_slope_lower := (log(lower)/(cohort_exp_def_mid - cohort_unexp_def_mid))]
  dt_plot[is.infinite(obs_slope_lower), obs_slope_lower := -1]
  dt_plot[is.nan(obs_slope_lower), obs_slope_lower := -1]
  
  #create ID
  dt_plot[,seq := NULL]
  dt_plot[,seq := 1:nrow(dt_plot)]
  dt_plot[,seq := factor(seq, levels = c(dt_plot[, seq]))]
  dt_plot <- dt_plot[!is.na(obs_slope)] 
  dt_plot[,super_region_name := factor(super_region_name, levels =c( "Latin America and Caribbean","South Asia","Southeast Asia, East Asia, and Oceania" ,"Central Europe, Eastern Europe, and Central Asia","Sub-Saharan Africa", "High-income" ))]
  
  height_file <- data.table(new_region_name = unique(dt_plot[region_name!='']$new_region_name))
  height_file <- merge(height_file,unique(dt_plot[,.(new_region_name, region_count2)]), by = 'new_region_name')
  height_file[,height := ifelse(region_count2<12,3,
                                ifelse(region_count2 < 80, 5,
                                       ifelse(region_count2<300,12,12)))]
  
  weird <- scales::trans_new("signed_log",transform=function(x) sign(x)*(abs(x)^.5),inverse=function(x) sign(x)*(abs(x))^2)
  
  myplots <- vector('list', length(unique(dt_plot[region_name!='']$new_region_name)))
  for (c.region in unique(dt_plot[region_name!='']$new_region_name)) {
    message(c.region)
    myplots[[c.region]] <- local({
      c.region <- c.region
      gg <- ggplot(dt_plot[new_region_name==c.region & new_region_name != '' & !is.na(new_region_name)]) + 
        geom_errorbar(aes(x = as.factor(seq), ymin = obs_slope_lower, ymax = obs_slope_upper, 
                          color = as.factor(outlier)), linewidth = .25, width = 0, alpha = 0.8) + 
        geom_point(aes(x = as.factor(seq), y = obs_slope,color = as.factor(outlier)), size = .5,alpha = 0.8)+
        coord_flip(ylim = c(-1,1)) + geom_hline(yintercept = 0, linetype = "dotted")+
        facet_grid(str_wrap(super_region_name,30)~str_wrap(new_region_name,30), scale = 'free_y', space = 'free') +
        aesth_pub+ theme(axis.title = element_text(size=10),
                         axis.text = element_text(size = 6, face = NULL), 
                         strip.background = element_rect(fill="white"), 
                         strip.text = element_text(size=10),
                         legend.position = c(0.8,0.8),
                         legend.text = element_text(size = 10),
                         legend.title = element_text(size = 10),
                         axis.title.y=element_blank(),
                         panel.grid.major.y = element_blank())+
        scale_x_discrete(expand = expansion(add = 1)) + 
        scale_color_manual(name = 'Outliers', values = c('1'='#F8766D', '0' = '#00BFC4'), labels = c('1'='Outlier','0'='Inlier'))+
        ylab ('Normalized relative risks per year of education')+
        scale_y_continuous(trans = weird, limits = c(-5,5), breaks = c( -1, -.25, 0, .25, 1))
      
    })
  }
  
  #change hieight according to number of points
  for (c.superregion in c( "Latin America and Caribbean","South Asia","Sub-Saharan Africa","Southeast Asia, East Asia, and Oceania" ,"Central Europe, Eastern Europe, and Central Asia", "High-income" )){
    for(c.region in unique(dt_plot[super_region_name==c.superregion]$new_region_name)){
      pdf(paste0(mod_viz_dir,"CaterpillarV2_",c.region,'_',Sys.Date(),".pdf"), width = 8, height = height_file[new_region_name == c.region]$height)
      print(myplots[c.region])
      dev.off()
    }
  }
  #combine pdfs of different heights
  infiles <- list.files(paste0(mod_viz_dir), pattern='CaterpillarV2', full.names = T)
  pdf_combine(infiles,paste0(mod_viz_dir,"Caterpillar_Combined_",Sys.Date(),".pdf"))
  
  fwrite(dt_plot[,.(seq,field_citation_value)], paste0(mod_viz_dir,"Caterpillar_KEY_",Sys.Date(),".csv"))
  
}

#new caterpillar
if(subset_mod == FALSE){
  dt_plot <- copy(dt[super_region_name !=  "Multiple Regions"])
  
  dt_plot[authors%like%'Kravdal'&`pub year`==2018, authors := 'Kravdal, Grundy E, Keenan K']
  dt_plot[authors%like%'Goldman, N'&`pub year`==2020, authors := 'Beltran-Sanchez, H. and Goldman, N. and Pebley, A. R. and Morales, J. F.']
  dt_plot[authors%like%'Chadeau-Hyam'&`pub year`==2018, authors := 'Castagne R, Gar?¿s V, Karimi M, Chadeau-Hyam M, Vineis P, Delpierre C, et al']
  dt_plot[,region_count := .N, by = 'region_name']
  dt_plot[,count := 1:.N, by = 'region_name']
  dt_plot[,new_region_name := paste0(region_name,' - ',floor(count/200)+1)]
  dt_plot[,region_count2 := .N, by = 'new_region_name']
  
  #order by region and obs
  dt_plot <- dt_plot[order(super_region_name,region_name,field_citation_value)]
  dt_plot[, obs_slope_upper := (log(upper)/(cohort_exp_def_mid - cohort_unexp_def_mid))]
  dt_plot[, obs_slope_lower := (log(lower)/(cohort_exp_def_mid - cohort_unexp_def_mid))]
  dt_plot[is.infinite(obs_slope_lower), obs_slope_lower := -1]
  dt_plot[is.nan(obs_slope_lower), obs_slope_lower := -1]
  
  #create ID
  dt_plot[,seq := NULL]
  dt_plot[,seq := 1:nrow(dt_plot)]
  dt_plot[,seq := factor(seq, levels = c(dt_plot[, seq]))]
  dt_plot[,name := paste(trimws(str_sub(authors,end=16)),', ',`pub year`,', ',ihme_loc_id)]
  dt_plot <- dt_plot[!is.na(obs_slope)] 
  dt_plot[,super_region_name := factor(super_region_name, levels =c( "Latin America and Caribbean","South Asia","Southeast Asia, East Asia, and Oceania" ,"Central Europe, Eastern Europe, and Central Asia","Sub-Saharan Africa", "High-income" ))]
  
  height_file <- data.table(new_region_name = unique(dt_plot[region_name!='']$new_region_name))
  height_file <- merge(height_file,unique(dt_plot[,.(new_region_name, region_count2)]), by = 'new_region_name')
  height_file[,height := ifelse(region_count2<12,2.5,
                                ifelse(region_count2 < 80, 4,
                                       ifelse(region_count2<300,11,11)))]
  
  weird <- scales::trans_new("signed_log",transform=function(x) sign(x)*(abs(x)^.5),inverse=function(x) sign(x)*(abs(x))^2)
  
  myplots <- vector('list', length(unique(dt_plot[region_name!='']$new_region_name)))
  for (c.region in unique(dt_plot[region_name!='']$new_region_name)) {
    message(c.region)
    myplots[[c.region]] <- local({
      c.region <- c.region
      gg <- ggplot(dt_plot[new_region_name==c.region & new_region_name != '' & !is.na(new_region_name)]) + 
        geom_pointrange(aes(x = name, y = obs_slope,ymin = obs_slope_lower, ymax = obs_slope_upper, color = as.factor(outlier)), 
                         size = 0.2,linewidth = .25, alpha = 0.8,position = position_dodge2(width=1,reverse=T))+
        coord_flip(ylim = c(-1,1)) + geom_hline(yintercept = 0, linetype = "dotted")+
        facet_grid(str_wrap(super_region_name,30)~str_wrap(new_region_name,30), scale = 'free_y', space = 'free') +
        aesth_pub+ theme(axis.title = element_text(size=10),
                         axis.text = element_text(size = 6, face = NULL), 
                         strip.background = element_rect(fill="white"), 
                         strip.text = element_text(size=10),
                         legend.position = c(0.8,0.8),
                         legend.text = element_text(size = 10),
                         legend.title = element_text(size = 10),
                         axis.title.y=element_blank(),
                         panel.grid.major.y = element_blank()
                         )+
        scale_x_discrete(expand = expansion(add = 1)) + 
        scale_color_manual(name = 'Outliers', values = c('1'='#F8766D', '0' = '#00BFC4'), labels = c('1'='Outlier','0'='Inlier'))+
        ylab ('Normalized relative risks per year of education')+
        scale_y_continuous(trans = weird, limits = c(-5,5), breaks = c( -1, -.25, 0, .25, 1))
      
    })
  }
  
  #change height according to number of points
  for (c.superregion in c( "Latin America and Caribbean","South Asia","Sub-Saharan Africa","Southeast Asia, East Asia, and Oceania" ,"Central Europe, Eastern Europe, and Central Asia", "High-income" )){
    for(c.region in unique(dt_plot[super_region_name==c.superregion]$new_region_name)){
      pdf(paste0(mod_viz_dir,"CaterpillarV2_",c.region,'_',Sys.Date(),".pdf"), width = 8, height = height_file[new_region_name == c.region]$height)
      print(myplots[c.region])
      dev.off()
    }
  }
  #combine pdfs of different heights
  infiles <- list.files(paste0(mod_viz_dir), pattern='CaterpillarV2', full.names = T)
  pdf_combine(infiles,paste0(mod_viz_dir,"Caterpillar_Combined_",Sys.Date(),".pdf"))
  
  fwrite(dt_plot[,.(seq,field_citation_value)], paste0(mod_viz_dir,"Caterpillar_KEY_",Sys.Date(),".csv"))
  
}

## Combine Figures for Publication #################################################

# age preds and beeswarm
if(subset_mod == FALSE){
  pdf(paste0(mod_viz_dir,"Pub_Age&Beeswarm_",c.run.type,"_",Sys.Date(),".pdf"),width=14,height=12)
  plot_grid(ggage2 +labs(y="RR",x="Education completed (years)", caption = ''),
            ggbee1 + theme(legend.position = c(0.2, 0.8)), ncol = 1)
  dev.off()
  
  pdf(paste0(mod_viz_dir,"Pub_Global&Beeswarm_",c.run.type,"_",Sys.Date(),".pdf"),width=14,height=12)
  plot_grid(gglobal_02a_pub +labs(y="RR",x="Education completed (years)", caption = '') +geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==6],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
              geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==12],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF')+
              geom_label_repel(show.legend = F,min.segment.length = 200,direction='y',fill='white',size = 6,data=df_pred2[cohort_exp_def_lower==18],aes(y=RR,x=cohort_exp_def_lower+0.5, label=paste0(round(RR,2))),color = '#357EBDFF'),
            ggbee_supreg + theme(legend.position = c(0.1, 0.8)), ncol = 1)
  dev.off()
  
}


pdf(paste0(mod_viz_dir,"Pub_Funnel1_",c.run.type,"_",Sys.Date(),".pdf"),width=12,height=8)
fun1+theme(legend.position = c(0.9,0.8), legend.title = element_blank())
dev.off()

pdf(paste0(mod_viz_dir,"Pub_Funnel2_",c.run.type,"_",Sys.Date(),".pdf"),width=8,height=8)
metafor::funnel(x = dt$residual, sei = dt$se_ln,ylim = c(0,2),  #https://towardsdatascience.com/constructing-contour-enhanced-funnel-plots-for-meta-analysis-6434cc8e51d0
                refline=0, level=c(90, 95, 99),shade=c("white", "gray", "darkgray"),pch=21, col=ifelse(dt$outlier > 0, "red", "black"),
                legend = FALSE,
                ylab = 'Standard Error of Observation',
                xlab = 'Residual (normalized RR)',
                hlines = 'black')
legend('topright',bty='o',
       c("p < 0.10", "p < 0.05", "p < 0.01", 'Outlier','Inlier'),
       fill=c("white", "gray", "darkgray",'red','black'))

dev.off()
