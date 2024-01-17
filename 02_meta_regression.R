################################################################################
# Run MR BRT For Adult Mortality - Education Relationship
## Setup #####################################################################
rm(list=ls())

library(reticulate)
reticulate::use_python("/FILEPATH/mrtool_0.0.1/bin/python")
mr <- import("mrtool")
pacman::p_load(data.table,RColorBrewer,gtools,stringr,ggrepel,ggplot2,dplyr,metafor,tidyverse,dplyr,qpdf,gridExtra,scales,cowplot)


aesth <- theme_bw() + theme(axis.title = element_text(size=13,face='bold'),axis.text = element_text(size=13,face='bold'),plot.title = element_text(size=14,face='bold'),strip.background = element_rect(fill="white"),strip.text = element_text(size=10,face='bold'),legend.position = 'top')
aesth_pub <- theme_classic() +  theme(axis.title = element_text(size=18,face='bold'),axis.text = element_text(size=16,face='bold'),plot.title = element_text(size=18,face='bold'),strip.background = element_rect(fill="white"), strip.text = element_text(size=16,face='bold'), legend.position = 'top', legend.text = element_text(size = 16),legend.title = element_text(size = 16))

## Model Run Name
am.model_version <- paste0(gsub('-','_',Sys.Date()),'_model_name')

## Set up root directories
root <- "/FILEPATH/"
input_dir <- paste0(root, "/inputs")
output_dir <- paste0(root, "/outputs/",am.model_version,"/")
mod_viz_dir <- paste0(output_dir,'visuals/')
dir.create(paste0(mod_viz_dir), recursive = T, showWarnings = T)

## Load data #########################################################################################################################
dt_all <- fread(paste0('/FILEPATH/SR_extraction_cleaned.csv'))

#If running with different SDI quantiles [sensitivity analysis]
if(0){
  dt_all[,sdi_group := cut(sdi, c( 0.0, mid_sdi , 1.0),labels = c('SDI_below', 'SDI_above'), include.lowest = TRUE)]
  dt_all[,sdi_group := cut(sdi, c(0.00, seq(0.62, max(dt_all$sdi), (max(dt_all$sdi)-0.62)/5)), labels = c( "SDI Group 1" , "SDI Group 2","SDI Group 3","SDI Group 4", "SDI Group 5","SDI Group 6"))] #V1
  dt_all[,sdi_group := cut(sdi, c(0.00, seq(0.6, max(dt_all$sdi), (max(dt_all$sdi)-0.6)/4)), labels = c( "SDI Group 1" , "SDI Group 2","SDI Group 3","SDI Group 4", "SDI Group 5"))] #V2
  dt_all[,sdi_group := cut(sdi, c(0.00, seq(0.7, max(dt_all$sdi), (max(dt_all$sdi)-0.7)/5)), labels = c( "SDI Group 1" , "SDI Group 2","SDI Group 3","SDI Group 4", "SDI Group 5","SDI Group 6"))] #V3
}

#Model options in main text #####
models <- c('Global') #main/global model
subset_mod <- F
predict_for_groups <- TRUE #predicts for different age groups

# main text
# models <- c('Male','Female')
# subset_mod <- T
# subset_version <- 'Sex_v2' #indicate column in subset_config
# predict_for_groups <- FALSE

## sensitivity analyses &appendices results
# age sensitivity analysis (appendix)
# models <- c('Age_15_to_49', 'Age_50_to_59','Age_60_to_69','Age_70_plus')
# subset_version <- 'AgeGroup_v1' #indicate column in subset_config
# subset_mod <- T
# predict_for_groups <- FALSE

# SDI models (appendix)
# models <- c('SDI_1_2_Quintile', 'SDI_3_Quintile','SDI_4_Quintile','SDI_5_Quintile')
# subset_version <- 'SDI_v2' #indicate column in config
# subset_mod <- T
# predict_for_groups <- FALSE

# models <- c("SDI Group 1" , "SDI Group 2","SDI Group 3","SDI Group 4", "SDI Group 5","SDI Group 6")
# subset_version <- 'SDI_v5' #indicate column in convig
# subset_mod <- T
# predict_for_groups <- FALSE

# models <- c("SDI Group 1" , "SDI Group 2","SDI Group 3","SDI Group 4", "SDI Group 5")
# subset_version <- 'SDI_v6' #indicate column in convig
# subset_mod <- T
# predict_for_groups <- FALSE
#
# models <- c('SDI_below', 'SDI_above')
# subset_version <- 'SDI_v4' #indicate column in convig
# subset_mod <- T
# predict_for_groups <- FALSE 
#
# models <- c('HIC', 'LMIC')
# subset_version <- 'Income_v1' #indicate column in convig
# subset_mod <- T
# predict_for_groups <- FALSE

## Time/cohort sensitivity
# models <- c('1870-1929 cohort','1930-1949 cohort', '1950-1990 cohort')
# subset_version <- 'Cohort_v2' #indicate column in config
# subset_mod <- T
# predict_for_groups <- FALSE
# predict_for_COHORT <- TRUE

# models <- c('1893-1989 time period','1990 to 1999 time period', '2000-2020 time period')
# subset_version <- 'Period_v1' #indicate column in convig
# subset_mod <- T
# predict_for_groups <- FALSE
# predict_for_PERIOD <- TRUE

# additional settings
calc_outliers <- TRUE #do you want automatic 10% trimming of outliers? Yes
SDI_interactive <- FALSE #sensitivity analyses of SDI as an interactive covariate, appendix
predict_for_SDI_groups <- FALSE

##list of covariates to include in model
covariate_list_all = c("confounders_age","confounders_sex", "confounders_marital_status")

# STEP 2 Create Interactive terms, written as "covariate_int" #interactive terms are covariates, age intervals
for(c.cov in covariate_list_all){
  dt_all[, paste0(c.cov, "_int") := get(c.cov) * denom_int]
}
dt_all[,age_int:=age_interval_mdpt*denom_int]
#dt_all[,cohort_int:=cohort*denom_int] #if running cohort interactive model
#dt_all[,period_int:=period*denom_int] #if running period interactive model

# STEP 3 determine subset version
if(subset_mod==T){
  subsetconfig <- as.data.table(read.csv(paste0('/FILEPATH/subsetconfig.csv')))
  subset_vals <- subsetconfig[,..subset_version][[1]]
  subset_vals <- subset_vals[subset_vals!=""]
  subset_var <- subset_vals[1]
  subset_vals <- subset_vals[-1]
  config <- data.table(modelname=c(models), subset_vals)
  config
}


## Loop through each model type #############################################################################################################################################

for (c.run.type in models){
  if(subset_mod == T){
    # subset model data to relevant group, run model
      dt <- dt_all[get(subset_var) == config[modelname==c.run.type]$subset_vals]
      message(paste0(c.run.type, ' number of rows = ',nrow(dt)))
  } else {
    # full dataset
    dt <- copy(dt_all)
  }
  
  
  # select controls
  confounder_config <- fread(paste0(root,'/ref/confounder_config.csv'))
  # for certain model runs (e.g. sex specific) different confounders were selected
  if(paste0(c.run.type) %in% names(confounder_config)){
      confounder_temp <- confounder_config[,..c.run.type][[1]]
      conf.list <- confounder_temp[confounder_temp!=""]
      conf.list
  } else {
    conf.list <- c('confounders_age','confounders_sex','confounders_marital_status')
    conf.list
  }

  # Step 4 Determine covariate list to be used in model and save multiplicative form covariates in new vector, c.covariate_list, for model
  covariate_list_in <- c(conf.list,names(dt)[names(dt)%like%"dummy_SDI" & !(names(dt)%like%"_int")])
  c.covariate_list  <- c(paste0(covariate_list_in, "_int"))
  
  conf.list ## used to create portion of prediction template (with each covariate on or off, while all others are off)
  covariate_list_in ## used to create portion of prediction template including interactive terms
  c.covariate_list #used in model fitting and to produce linear covs (this includes only the interactive terms)
  
  ### Fit Model ###################################################################################################
  
  message('fit model')
  
  # load data & relevant columns
  dat1 <- mr$MRData()
  dat1$load_df(data = dt,  col_obs = "rr_ln", col_obs_se = "se_ln",
               col_study_id="field_citation_value",col_data_id = 'seq',
               col_covs = c(c.covariate_list,
                            #'cohort_int', #if any additional covariates are set up as interactive covs, add here
                            #'period_int', #if any additional covariates are set up as interactive covs, add here
                            'age_int',
                            "cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
  
  # model specifications for each covariates, the exposure definitions, and age_int
  linear_covs <- lapply(c(c.covariate_list,
                          #'cohort_int',#if any additional covariates are set up as interactive covs, add here
                          #'period_int',#if any additional covariates are set up as interactive covs, add here
                          'age_int'
                          ), mr$LinearCovModel)
  linear_covs[[length(linear_covs)+1]] <-
    mr$LinearCovModel(
      alt_cov = list("cohort_exp_def_lower", "cohort_exp_def_upper"),
      ref_cov = list("cohort_unexp_def_lower", "cohort_unexp_def_upper"),
      use_re=T, name = 'Education_Exposure'
    )
  
  # Fit model 
  mod1 <- mr$MRBRT(data = dat1, cov_models = linear_covs, inlier_pct = 0.9) 
  mod1$fit_model(inner_print_level = 3L, inner_max_iter = 1000L, outer_max_iter = 10L)

  ### Set up samples #############################################################################################
  n_samples <- 1000L
  samples <- mr$core$other_sampling$sample_simple_lme_beta(
    sample_size = n_samples,
    model = mod1)
   gamma_matrix <- do.call("rbind", lapply(1:n_samples, function(x) mod1$gamma_soln)) #takes gammas and makes a matrix if multiple gammas/spline
 
  message('predict')
  
  #### 1 predict with individual conf.list factors on #these are diagnostic predictions, not main article ########################################################################################
  pred.list <- list()
  for (c.var in conf.list){
    temp <- data.table(expand.grid(cohort_exp_def_lower = seq(0,18,1),
                                   cohort_unexp_def_lower = 0,
                                   cohort_unexp_def_upper = 0.1,
                                   age5=round(mean(dt$age_interval_mdpt)), 
                                   #cohort=round(mean(dt$cohort)), #if using cohort interactive term
                                   #period=round(mean(dt$period)), #if using cohort interactive term
                                   Model=c.run.type,
                                   temp=c(0,1),
                                   type=c.var))
    setnames(temp,"temp",c.var)
    for (c.var2 in conf.list[conf.list!=c.var]){
      temp[,temp2:=0]
      setnames(temp,"temp2",c.var2)}
    pred.list[[length(pred.list)+1]] <- temp
  }

  df_pred1 <- rbindlist(pred.list,use.names = T)
  df_pred1[,cohort_exp_def_upper := cohort_exp_def_lower]
  
  #create interaction terms
  df_pred1[, denom_int := ((cohort_exp_def_lower + cohort_exp_def_upper)/2) - ((cohort_unexp_def_lower + cohort_unexp_def_upper)/2)]
  df_pred1[, age_int := as.numeric(age5)*denom_int]
  #df_pred1[, cohort_int := as.numeric(cohort)*denom_int]
  #df_pred1[, period_int := as.numeric(period)*denom_int]

  for(c.cov in covariate_list_in){
    df_pred1[, paste0(c.cov, "_int") := get(c.cov) * denom_int]}
  
  #### make predictions, individual factors on
  dat_pred1 <- mr$MRData()
  dat_pred1$load_df(
    data = df_pred1,
    col_covs=c(c.covariate_list,
               #'cohort_int',
               #'period_int',
               'age_int',
               "cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
  
  
 draws1 <- mod1$create_draws(
      data = dat_pred1,
      beta_samples = samples,
      gamma_samples = gamma_matrix,
      random_study =  FALSE,
      sort_by_data_id = TRUE)

  df_pred1[,rr_pred :=  mod1$predict(data = dat_pred1,  predict_for_study = FALSE, sort_by_data_id = TRUE)] #incorporates the estimated random effects into point predictions
  df_pred1$rr_pred_lwr <- apply(draws1, 1, function(x) quantile(x, 0.025))
  df_pred1$rr_pred_upr <- apply(draws1, 1, function(x) quantile(x, 0.975))
  df_pred1[,RR:=exp(rr_pred)]
  df_pred1[,RR_lwr:=exp(rr_pred_lwr)]
  df_pred1[,RR_upr:=exp(rr_pred_upr)] 
  
  #get presence/absence var collapsed
  for (c.var in c(conf.list)){
    df_pred1[type==c.var&get(c.var)==1,presence:="present"]
    df_pred1[type==c.var&get(c.var)==0,presence:="absent"]
  }
  
  #### 2 predict all factors turned on, age = 60 # these are the main results/predictions ####################################################################
  df_pred2 <- data.table(expand.grid(cohort_exp_def_lower = seq(0,18,1),
                                     cohort_unexp_def_lower = 0,
                                     cohort_unexp_def_upper = 0.1,
                                     age5=round(mean(dt$age_interval_mdpt)),
                                     #cohort=round(mean(dt$cohort)), 
                                     #period=round(mean(dt$period)), 
                                     Model=c.run.type))
  df_pred2[,cohort_exp_def_upper := cohort_exp_def_lower]
  
  for (c.var in conf.list){
    df_pred2[,temp:=1]
    setnames(df_pred2,"temp",c.var)}
  
  # #create interaction terms (age w/ other covs)
  df_pred2[, denom_int := ((cohort_exp_def_lower + cohort_exp_def_upper)/2) - ((cohort_unexp_def_lower + cohort_unexp_def_upper)/2)]
  df_pred2[, age_int:=as.numeric(age5)*denom_int]
  #df_pred2[, cohort_int := as.numeric(cohort)*denom_int]
  #df_pred2[, period_int := as.numeric(period)*denom_int]
  
  for(c.cov in covariate_list_in){
    df_pred2[, paste0(c.cov, "_int") := get(c.cov) * denom_int]}
  
  #### make predictions, all factors on
  dat_pred2 <- mr$MRData()
  dat_pred2$load_df(
    data = df_pred2,
    col_covs=c(c.covariate_list,
               'age_int',
               #'cohort_int',#
               #'period_int',
               "cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
  
  draws2 <- mod1$create_draws(
      data = dat_pred2,
      beta_samples = samples,
      gamma_samples = gamma_matrix,
      random_study =  FALSE,
      sort_by_data_id = TRUE)
    
  #point estimate
  df_pred2[,rr_pred :=  mod1$predict(dat_pred2,predict_for_study = FALSE, sort_by_data_id = TRUE)]
  df_pred2$rr_pred_lwr <- apply(draws2, 1, function(x) quantile(x, 0.025))
  df_pred2$rr_pred_upr <- apply(draws2, 1, function(x) quantile(x, 0.975))
  df_pred2[,RR:=exp(rr_pred)]
  df_pred2[,RR_lwr:=exp(rr_pred_lwr)]
  df_pred2[,RR_upr:=exp(rr_pred_upr)]

  #### 3 predict for different SDI groups ########################################################################################
  if(predict_for_SDI_groups == TRUE) {
    pred.list <- list()
    for (c.var in  names(dt)[names(dt) %like% "dummy_SDI" & !(names(dt) %like% "_int")]){
      temp <- data.table(expand.grid(cohort_exp_def_lower = seq(0,18,1),
                                     cohort_unexp_def_lower = 0,
                                     cohort_unexp_def_upper = 0.1,
                                     age5=round(mean(dt$age_interval_mdpt)), 
                                     sdi_group=gsub('dummy_','',c.var),
                                     Model=c.run.type,
                                     temp=c(1,0)))
      setnames(temp,"temp",c.var)
      for (c.var2 in names(dt)[names(dt)%like%"dummy_SDI" & !(names(dt)%like%"_int")& (names(dt)!=c.var)]){
        temp[,temp2:=0]
        setnames(temp,"temp2",c.var2)}
      pred.list[[length(pred.list)+1]] <- temp
    }
    
    df_pred3 <- rbindlist(pred.list,use.names = T)
    df_pred3[,cohort_exp_def_upper := cohort_exp_def_lower]
    
    for (c.var in conf.list){
      df_pred3[,temp:=1]
      setnames(df_pred3,"temp",c.var)}
    
    # #create interaction terms (age and dummys w/ other covs)
    df_pred3[, denom_int := ((cohort_exp_def_lower + cohort_exp_def_upper)/2) - ((cohort_unexp_def_lower + cohort_unexp_def_upper)/2)]
    df_pred3[,age_int:=as.numeric(age5)*denom_int]                                                                                           
    for(c.cov in covariate_list_in){
      df_pred3[, paste0(c.cov, "_int") := get(c.cov) * denom_int]}
  
  
  #### make predictions, for each SDI group
  dat_pred3 <- mr$MRData()
  dat_pred3$load_df(
    data = df_pred3,
    col_covs=c(c.covariate_list,'age_int',"cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
  
    draws3 <- mod1$create_draws(
      data = dat_pred3,
      beta_samples = samples,
      gamma_samples = gamma_matrix,
      random_study =  FALSE,
      sort_by_data_id = TRUE)

  #point estimate
  df_pred3[,rr_pred :=  mod1$predict(dat_pred3,predict_for_study = FALSE, sort_by_data_id = TRUE)]
  df_pred3$rr_pred_lwr <- apply(draws2, 1, function(x) quantile(x, 0.025))
  df_pred3$rr_pred_upr <- apply(draws2, 1, function(x) quantile(x, 0.975))
  df_pred3[,RR:=exp(rr_pred)]
  df_pred3[,RR_lwr:=exp(rr_pred_lwr)]
  df_pred3[,RR_upr:=exp(rr_pred_upr)]
  
# pull out just different levels
    agg_df_pred3 <- data.table()
    for (c.SDI in names(dt)[names(dt)%like%"dummy_SDI" & !(names(dt)%like%"_int")]){
      temp <- df_pred3[sdi_group==gsub('dummy_','',c.SDI) & get(c.SDI)==1,.(Model,cohort_exp_def_lower,sdi_group,rr_pred,rr_pred_lwr,rr_pred_upr,RR,RR_lwr,RR_upr)]
      agg_df_pred3 <- rbind(agg_df_pred3,temp)
    }
    temp <- df_pred3[sdi_group==gsub('dummy_','',c.SDI) & get(c.SDI)==0,.(Model,cohort_exp_def_lower,sdi_group,rr_pred,rr_pred_lwr,rr_pred_upr,RR,RR_lwr,RR_upr)]
    temp[,sdi_group := 'SDI_above']
    agg_df_pred3 <- rbind(agg_df_pred3,temp)
    setnames(agg_df_pred3,'sdi_group','group')
    agg_df_pred3[,pred_type := 'SDI Groups']
}
  
  #### 4 predict for different AGE groups ########################################################################################
  if(predict_for_groups == TRUE){
  pred.list <- list()
  for (c.age in  seq(20,95,5)){
    temp <- data.table(expand.grid(cohort_exp_def_lower = seq(0,18,1),
                                   cohort_unexp_def_lower = 0,
                                   cohort_unexp_def_upper = 0.1,
                                   age5 = c.age,
                                   Model=c.run.type))
    pred.list[[length(pred.list)+1]] <- temp
  }
  df_pred4 <- rbindlist(pred.list,use.names = T)
  df_pred4[,cohort_exp_def_upper := cohort_exp_def_lower]
  
  for (c.var in conf.list){
    df_pred4[,temp:=1]
    setnames(df_pred4,"temp",c.var)}
  
  ## create interaction terms (age and dummys w/ other covs)
  df_pred4[, denom_int := ((cohort_exp_def_lower + cohort_exp_def_upper)/2) - ((cohort_unexp_def_lower + cohort_unexp_def_upper)/2)]
  df_pred4[,age_int := as.numeric(age5)*denom_int]

  for(c.cov in covariate_list_in){
    df_pred4[, paste0(c.cov, "_int") := get(c.cov) * denom_int]}
  
  #### make predictions, for each AGE group
  dat_pred4 <- mr$MRData()
  dat_pred4$load_df(
    data = df_pred4,
    col_covs=c(c.covariate_list,'age_int',"cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
  
  draws4 <- mod1$create_draws(
      data = dat_pred4,
      beta_samples = samples,
      gamma_samples = gamma_matrix,
      random_study =  FALSE,
      sort_by_data_id = TRUE)

  #point estimate
  df_pred4[,rr_pred :=  mod1$predict(dat_pred4,predict_for_study = FALSE, sort_by_data_id = TRUE)]
  df_pred4$rr_pred_lwr <- apply(draws4, 1, function(x) quantile(x, 0.025))
  df_pred4$rr_pred_upr <- apply(draws4, 1, function(x) quantile(x, 0.975))
  df_pred4[,RR:=exp(rr_pred)]
  df_pred4[,RR_lwr:=exp(rr_pred_lwr)]
  df_pred4[,RR_upr:=exp(rr_pred_upr)]
  
  ## Aggregate to three age groups
  df_pred4[age5 <50, age_group := '18 to 49']
  df_pred4[is.na(age_group) & age5 < 60, age_group := '50 to 59']
  df_pred4[is.na(age_group) & age5 < 70, age_group := '60 to 69']
  df_pred4[is.na(age_group), age_group := '> 70']
  
  agg_df_pred4 <- df_pred4[,.(rr_pred=mean(rr_pred,na.rm=T),
                              rr_pred_lwr=mean(rr_pred_lwr,na.rm=T),
                              rr_pred_upr=mean(rr_pred_upr,na.rm=T),
                              RR=mean(RR,na.rm=T),
                              RR_lwr=mean(RR_lwr,na.rm=T),
                              RR_upr=mean(RR_upr,na.rm=T)),
                           by = .(age_group,Model,cohort_exp_def_lower)]
  
  setnames(agg_df_pred4,'age_group','group')
  agg_df_pred4[,pred_type := 'Age Groups']
  
}
  
  #### 5 predict for different COHORT groups interactive term (10yr increments, sensitivity analysis) ########################################################################################
  if(predict_for_COHORT == TRUE){
    pred.list <- list()
    for (c.cohort in  seq(1870,1980,10)){
      temp <- data.table(expand.grid(cohort_exp_def_lower = seq(0,18,1),
                                     cohort_unexp_def_lower = 0,
                                     cohort_unexp_def_upper = 0.1,
                                     age5=round(mean(dt$age_interval_mdpt)),
                                     cohort=c.cohort,
                                     Model=c.run.type))
      pred.list[[length(pred.list)+1]] <- temp
    }
    df_pred5 <- rbindlist(pred.list,use.names = T)
    df_pred5[,cohort_exp_def_upper := cohort_exp_def_lower]
    
    for (c.var in conf.list){
      df_pred5[,temp:=1]
      setnames(df_pred5,"temp",c.var)}
    
    ## create interaction terms (age and dummys w/ other covs)
    df_pred5[, denom_int := ((cohort_exp_def_lower + cohort_exp_def_upper)/2) - ((cohort_unexp_def_lower + cohort_unexp_def_upper)/2)]
    df_pred5[,age_int := as.numeric(age5)*denom_int]
    df_pred5[,cohort_int := as.numeric(cohort)*denom_int]

    for(c.cov in covariate_list_in){
      df_pred5[, paste0(c.cov, "_int") := get(c.cov) * denom_int]}
    
    #### make predictions, for each AGE group
    dat_pred5 <- mr$MRData()
    dat_pred5$load_df(
      data = df_pred5,
      col_covs=c(c.covariate_list,'age_int','cohort_int', 
                 "cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
    
    draws5 <- mod1$create_draws(
        data = dat_pred5,
        beta_samples = samples,
        gamma_samples = gamma_matrix,
        random_study =  FALSE,
        sort_by_data_id = TRUE)

    #point estimate
    df_pred5[,rr_pred :=  mod1$predict(dat_pred5,predict_for_study = FALSE, sort_by_data_id = TRUE)]
    df_pred5$rr_pred_lwr <- apply(draws5, 1, function(x) quantile(x, 0.025))
    df_pred5$rr_pred_upr <- apply(draws5, 1, function(x) quantile(x, 0.975))
    df_pred5[,RR:=exp(rr_pred)]
    df_pred5[,RR_lwr:=exp(rr_pred_lwr)]
    df_pred5[,RR_upr:=exp(rr_pred_upr)]
    
    df_pred5[,pred_type := 'Cohorts']
    
  }
  
  
  #### 6 predict for different PERIOD groups interactive (10yr increments, sensitivity analysis) ########################################################################################
  if(predict_for_PERIOD == TRUE){
    pred.list <- list()
    for (c.period in seq(1900,2020,10)){
      temp <- data.table(expand.grid(cohort_exp_def_lower = seq(0,18,1),
                                     cohort_unexp_def_lower = 0,
                                     cohort_unexp_def_upper = 0.1,
                                     age5=round(mean(dt$age_interval_mdpt)),
                                     period=c.period,
                                     Model=c.run.type))
      pred.list[[length(pred.list)+1]] <- temp
    }
    df_pred6 <- rbindlist(pred.list,use.names = T)
    df_pred6[,cohort_exp_def_upper := cohort_exp_def_lower]
    
    for (c.var in conf.list){
      df_pred6[,temp:=1]
      setnames(df_pred6,"temp",c.var)}
    
    ## create interaction terms (age and dummys w/ other covs)
    df_pred6[, denom_int := ((cohort_exp_def_lower + cohort_exp_def_upper)/2) - ((cohort_unexp_def_lower + cohort_unexp_def_upper)/2)]
    df_pred6[,age_int := as.numeric(age5)*denom_int]
    df_pred6[,period_int := as.numeric(period)*denom_int]
    for(c.cov in covariate_list_in){
      df_pred6[, paste0(c.cov, "_int") := get(c.cov) * denom_int]}
    
    #### make predictions, for each AGE group
    dat_pred6 <- mr$MRData()
    dat_pred6$load_df(
      data = df_pred6,
      col_covs=c(c.covariate_list,'age_int', 'period_int',
                 "cohort_exp_def_lower","cohort_exp_def_upper","cohort_unexp_def_lower", "cohort_unexp_def_upper"))
    
    draws6 <- mod1$create_draws(
        data = dat_pred6,
        beta_samples = samples,
        gamma_samples = gamma_matrix,
        random_study =  FALSE,
        sort_by_data_id = TRUE)

    #point estimate
    df_pred6[,rr_pred :=  mod1$predict(dat_pred6,predict_for_study = FALSE, sort_by_data_id = TRUE)]
    df_pred6$rr_pred_lwr <- apply(draws6, 1, function(x) quantile(x, 0.025))
    df_pred6$rr_pred_upr <- apply(draws6, 1, function(x) quantile(x, 0.975))
    df_pred6[,RR:=exp(rr_pred)]
    df_pred6[,RR_lwr:=exp(rr_pred_lwr)]
    df_pred6[,RR_upr:=exp(rr_pred_upr)]
    
    df_pred6[,pred_type := 'Periods']
    
  }
  
  
  ## ADD TRIM WEIGHTS #####################################################################################################################
     df_mod1 <- cbind(mod1$data$to_df(), data.frame(seq = mod1$data$data_id), data.frame(trim_wt = mod1$w_soln)) #chech to_df
     df_mod1 <- as.data.table(df_mod1)
     setnames(df_mod1,'study_id','field_citation_value')
    
     #save weights to combine onto data later (by model)
     save_mod_weight <- df_mod1[,.(seq,field_citation_value,trim_wt)]
     setnames(save_mod_weight,'trim_wt',paste0(gsub(' ','_',c.run.type),'_trim_wt'))
     fwrite(save_mod_weight,paste0(output_dir,gsub(' ','_',c.run.type),'_trim_weights.csv'))
    
     dt <- merge(dt, df_mod1[,.(seq,trim_wt)], by = c('seq'), all.x = T)
     dt[,outlier := ifelse(trim_wt>0,0,1)]
     table(dt$outlier, useNA = 'ifany')

  ### Graph
  source(paste0(root,'/FILEPATH/graph_mrbrt.R'), echo = TRUE)
  
#  test for publication bias
  regtest(x = residual, sei = se_ln, predictor = "sei", data = dt)
 
  ### Save data sets and calculate summary vals ##################################################################################################
  
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
  print('save data sets')
  
  df_pred2[,group := c.run.type]
  
  fwrite(dt,paste0(output_dir,"Input_data_",c.run.type,"_",Sys.Date(),".csv"))
  
  dt$trim_wt <- NULL
  dt$outlier <- NULL

  fwrite(df_pred1[,c("Model","type","cohort_exp_def_lower","RR","RR_lwr","RR_upr","presence")], paste0(output_dir,"RR_Estimates_",c.run.type,"_",Sys.Date(),".csv"))
  fwrite(df_pred2[,c("Model",'group',"cohort_exp_def_lower","RR","RR_lwr","RR_upr")], paste0(output_dir,"RR_All_On_",c.run.type,"_",Sys.Date(),".csv"))
  fwrite(df_pred1_ci,paste0(output_dir,"RR_Ratio_Summary_",c.run.type,"_",Sys.Date(),".csv")) 
  
  if(predict_for_SDI_groups == TRUE){
    agg_df <- rbind(agg_df_pred3,agg_df_pred4)
    fwrite(df_pred4,paste0(output_dir,"RR_AGE_",c.run.type,"_",Sys.Date(),".csv"))
    fwrite(df_pred3,paste0(output_dir,"RR_SDI_",c.run.type,"_",Sys.Date(),".csv"))
    fwrite(agg_df,paste0(output_dir,"RR_AGE_SDI_AGG_",c.run.type,"_",Sys.Date(),".csv"))
  }
  
  if(predict_for_groups == TRUE){
    agg_df <- rbind(agg_df_pred4)
    fwrite(df_pred4,paste0(output_dir,"RR_AGE_",c.run.type,"_",Sys.Date(),".csv"))
    fwrite(agg_df,paste0(output_dir,"RR_AGE_AGG_",c.run.type,"_",Sys.Date(),".csv"))
    
    temp <- rbindlist(list(df_pred2,agg_df_pred4),fill = T)
    effects <- rbindlist(lapply(unique(temp$group), calc_yearly_effect))
    fwrite(effects, paste0(output_dir,c.run.type,"_yearly_effect_sizes_bymod_",Sys.Date(),".csv"))
    
  } else {
    temp <- rbindlist(list(df_pred2),fill = T)
    effects <- rbindlist(lapply(unique(temp$group), calc_yearly_effect))
    fwrite(effects, paste0(output_dir,c.run.type,"_yearly_effect_sizes_bymod_",Sys.Date(),".csv"))
    
  }
  
  ### Save Model Info ######################################################################
  py_save_object(object = mod1, filename = file.path(output_dir, paste0(c.run.type,"_mod1_",Sys.Date(),".pkl")), pickle = "dill")
  get_gamma_sd <- function(mod1){
    gamma <- mod1$gamma_soln
    gamma_fisher <- mod1$lt$get_gamma_fisher(gamma)
    return(1/sqrt(gamma_fisher[1,1]))
  }
  get_bounds <- function(beta, beta_sd) {
    temp <- data.table()
    for (i in 1:length(beta)){
      temp1 <- as.data.table(list(
        lower=beta[i]-(qnorm(0.975)*beta_sd[i]), 
        upper=beta[i]+(qnorm(0.975)*beta_sd[i]), 
        p=(1-pnorm(abs(beta[i]/beta_sd[i])))*2)
      )
      temp <- rbind(temp,temp1)
    }
    return(temp)
  }

  # beta and sd for all coeff
  stdevs_tmp <- apply(X = mr$core$other_sampling$sample_simple_lme_beta(sample_size = 1000L, model = mod1), MARGIN = 2, FUN = sd)
  all_posteriors <- as.data.table(list(values = mod1$beta_soln, SD = stdevs_tmp))
  # beta and sd for gamma coeff
  temp <- as.data.table(list(values = mod1$gamma_soln,SD = get_gamma_sd(mod1)))
  
  # calculate upper/lower bounds using beta/sd 
  names <- reshape2::melt(mod1$summary()[[1]]) 
  all_cov_uis <- cbind(names$variable, all_posteriors, get_bounds(beta = all_posteriors$values, beta_sd = all_posteriors$SD))
 
  gamma_uis <- cbind('Gamma', temp, get_bounds(beta = temp$values, beta_sd = temp$SD))
  all_cov_uis <- rbind(all_cov_uis,gamma_uis)
  
  fwrite(all_cov_uis,paste0(output_dir,"mod_cov_uis_",c.run.type,"_",Sys.Date(),".csv"))
}




