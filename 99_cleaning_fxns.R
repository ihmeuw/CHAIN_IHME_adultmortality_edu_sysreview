#################################################################################
### Cleaning Functions for cleaning Script ###
#################################################################################

###  Called from clean script. Contains functions to do simple cleaning tasks.

#################################################################################

create_bins <- function(c.data, cohort_exposed_def_var,cohort_unexp_def_var){
  data <- copy(c.data)

  data[, cohort_exposed_var_arch := get(cohort_exposed_def_var)] #save original
  #clean exposure
  data[is.na(get(cohort_exposed_def_var)), (cohort_exposed_def_var) := NA]
  data[get(cohort_exposed_def_var) %like% ",", (cohort_exposed_def_var) := NA]
  data[get(cohort_exposed_def_var) %like% "NA|N/A|na", (cohort_exposed_def_var) := NA]
  data[get(cohort_exposed_def_var) %like% "and|AND", (cohort_exposed_def_var) := NA]
  data[tolower(get(cohort_exposed_def_var)) %like% "no education", (cohort_exposed_def_var) := 0]
  data[, paste0(cohort_exposed_def_var) := gsub("-", "to", get(cohort_exposed_def_var), fixed = T)]
  data[!get(cohort_exposed_def_var) %like% ">|<|to|\u2264|\u2265", paste0(cohort_exposed_def_var) := paste0(gsub("[^0-9]", "",get(cohort_exposed_def_var)), "to", gsub("[^0-9]", "",get(cohort_exposed_def_var)))]
  data[, paste0(cohort_exposed_def_var) := gsub("=", "", get(cohort_exposed_def_var), fixed = T)]
  data[, paste0(cohort_exposed_def_var) := gsub(" ", "", get(cohort_exposed_def_var), fixed = T)]
  # split into exp low and high
  data[, cohort_exp_def_lower := tstrsplit(get(cohort_exposed_def_var), "to", keep = 1)]
  data[, cohort_exp_def_lower := gsub("[^0-9]", "", cohort_exp_def_lower)]
  data[, cohort_exp_def_upper := tstrsplit(get(cohort_exposed_def_var), "to", keep = 2)]
  data[, cohort_exp_def_upper := gsub("[^0-9]", "", cohort_exp_def_upper)]
  data[, cohort_exp_def_upper := as.double(cohort_exp_def_upper)]
  data[, cohort_exp_def_lower := as.double(cohort_exp_def_lower)]
 
  data[,.(cohort_exposed_def1,cohort_exp_def_lower,cohort_exp_def_upper)]
   # cap at 18
  data[get(cohort_exposed_def_var) %like% ">", cohort_exp_def_upper := 18]
  data[get(cohort_exposed_def_var) %like% ">" & !(get(cohort_exposed_def_var) %in% c('>18','>19','>20','>21')), cohort_exp_def_lower := cohort_exp_def_lower + 1]
  data[get(cohort_exposed_def_var) %like% ">" & (get(cohort_exposed_def_var) %in% c('>18','>19','>20','>21')), cohort_exp_def_lower := 18]
  
  data[,.(cohort_exposed_def1,cohort_exp_def_lower,cohort_exp_def_upper)]
  
  data[get(cohort_exposed_def_var) %like% "<",cohort_exp_def_upper := cohort_exp_def_lower - 1]
  data[get(cohort_exposed_def_var) %like% "<", cohort_exp_def_lower := 0]
  data[get(cohort_exposed_def_var) %like% "\u2265", cohort_exp_def_upper := 18]
  data[get(cohort_exposed_def_var) %like% "\u2264",cohort_exp_def_upper := cohort_exp_def_lower]
  data[get(cohort_exposed_def_var) %like% "\u2264", cohort_exp_def_lower := 0]
  data[!get(cohort_exposed_def_var) %like% ">|<|to|\u2264|\u2265", cohort_exp_def_lower := as.numeric(gsub("[^0-9]", "",get(cohort_exposed_def_var)))]#for single digit entries, copy value to both upper and lower
  data[!get(cohort_exposed_def_var) %like% ">|<|to|\u2264|\u2265", cohort_exp_def_upper := as.numeric(gsub("[^0-9]", "",get(cohort_exposed_def_var)))]#for single digit entries, copy value to both upper and lower
  data[, cohort_exp_def_upper := as.double(cohort_exp_def_upper)]
  data[, cohort_exp_def_lower := as.double(cohort_exp_def_lower)]
  data[, cohort_exp_def_mid := (cohort_exp_def_upper + cohort_exp_def_lower)/2]
  data[inrange(cohort_exp_def_upper, 18, 28), cohort_exp_def_upper := 18]
  data[!inrange(cohort_exp_def_lower, 0, 19), cohort_exp_def_lower := NA]
  data[!inrange(cohort_exp_def_upper, 0, 19), cohort_exp_def_upper := NA]
  data[, clean_cohort_exp_def := paste0(cohort_exp_def_lower, " to ", cohort_exp_def_upper)]
  data[get(cohort_exposed_def_var) %like% "NA", clean_cohort_exp_def := NA]
  data[, cohort_exp_def_upper := cohort_exp_def_upper + .99]

  #then unexposed
  data[, cohort_unexp_var_arch := get(cohort_unexp_def_var)]
  data[is.na(get(cohort_unexp_def_var)), (cohort_unexp_def_var) := NA]
  data[get(cohort_unexp_def_var) %like% ",", (cohort_unexp_def_var) := NA]
  data[get(cohort_unexp_def_var) %like% "NA|N/A|na", (cohort_unexp_def_var) := NA]
  data[get(cohort_unexp_def_var) %like% "and|AND", (cohort_unexp_def_var) := NA]
  data[str_to_lower(get(cohort_unexp_def_var)) %like% "no education", (cohort_unexp_def_var) := 0]

  data[, paste0(cohort_unexp_def_var) := gsub("-", "to", get(cohort_unexp_def_var), fixed = T)]
  data[!get(cohort_unexp_def_var) %like% ">|<|to|\u2264|\u2265", paste0(cohort_unexp_def_var) := paste0(gsub("[^0-9]", "",get(cohort_unexp_def_var)), "to", gsub("[^0-9]", "",get(cohort_unexp_def_var)))]
  data[, paste0(cohort_unexp_def_var) := gsub("=", "", get(cohort_unexp_def_var), fixed = T)]
  data[, paste0(cohort_unexp_def_var) := gsub(" ", "", get(cohort_unexp_def_var))]

  data[, cohort_unexp_def_lower := tstrsplit(get(cohort_unexp_def_var), "to", keep = 1)]
  data[, cohort_unexp_def_lower := gsub("[^0-9]", "", cohort_unexp_def_lower)]
  data[, cohort_unexp_def_upper := tstrsplit(get(cohort_unexp_def_var), "to", keep = 2)]
  data[, cohort_unexp_def_upper := gsub("[^0-9]", "", cohort_unexp_def_upper)]
  data[, cohort_unexp_def_upper := as.double(cohort_unexp_def_upper)]
  data[, cohort_unexp_def_lower := as.double(cohort_unexp_def_lower)]
  
  data[get(cohort_unexp_def_var) %like% ">", cohort_unexp_def_upper := 18]
  data[get(cohort_unexp_def_var) %like% ">" & !(get(cohort_unexp_def_var) %in% c('>18','>19','>20','>21')), cohort_unexp_def_lower := cohort_unexp_def_lower + 1]
  data[get(cohort_unexp_def_var) %like% ">" & (get(cohort_unexp_def_var) %in% c('>18','>19','>20','>21')), cohort_unexp_def_lower := 18]
  data[get(cohort_unexp_def_var) %like% "<",cohort_unexp_def_upper := cohort_unexp_def_lower - 1]
  data[get(cohort_unexp_def_var) %like% "<", cohort_unexp_def_lower := 0]
  data[get(cohort_unexp_def_var) %like% "\u2265", cohort_unexp_def_upper := 18]
  data[get(cohort_unexp_def_var) %like% "\u2264",cohort_unexp_def_upper := cohort_unexp_def_lower]
  data[get(cohort_unexp_def_var) %like% "\u2264", cohort_unexp_def_lower := 0]
  data[get(cohort_unexp_def_var) %like% "<",cohort_unexp_def_upper := cohort_unexp_def_lower]
  data[get(cohort_unexp_def_var) %like% "<", cohort_unexp_def_lower := 0]
  data[!get(cohort_unexp_def_var) %like% ">|<|to|\u2264|\u2265", cohort_unexp_def_lower := as.numeric(gsub("[^0-9]", "",get(cohort_unexp_def_var)))]#for single digit entries, copy value to both upper and lower
  data[!get(cohort_unexp_def_var) %like% ">|<|to|\u2264|\u2265", cohort_unexp_def_upper := as.numeric(gsub("[^0-9]", "",get(cohort_unexp_def_var)))]#for single digit entries, copy value to both upper and lower
  data[, cohort_unexp_def_upper := as.double(cohort_unexp_def_upper)]
  data[, cohort_unexp_def_lower := as.double(cohort_unexp_def_lower)]
  data[, cohort_unexp_def_mid := (cohort_unexp_def_upper + cohort_unexp_def_lower)/2]
  data[inrange(cohort_unexp_def_upper, 18, 28), cohort_unexp_def_upper := 18]
  data[!inrange(cohort_unexp_def_lower, 0, 19), cohort_unexp_def_lower := 18]
  data[!inrange(cohort_unexp_def_upper, 0, 19), cohort_unexp_def_upper := 18]
  data[, clean_cohort_unexp_def := paste0(cohort_unexp_def_lower, " to ", cohort_unexp_def_upper)]
  data[get(cohort_unexp_def_var) %like% "NA", clean_cohort_unexp_def := NA]
  data[, cohort_unexp_def_upper := cohort_unexp_def_upper + .99]

  #print error message if studies are missing values
  if(!(all(!is.na(data$cohort_unexp_def_lower))) | 
     !(all(!is.na(data$cohort_unexp_def_upper))) |
     !(all(!is.na(data$cohort_exp_def_lower))) |
     !(all(!is.na(data$cohort_exp_def_upper)))){
    warning("Warning: There are NA values of exposed or unexposed definitions")
    warning(
      paste0("Non-standard inputs include: ",
             paste(c(
               data[is.na(cohort_unexp_def_upper)|is.na(cohort_unexp_def_lower), unique(get(cohort_unexp_def_var))],
               data[is.na(cohort_exp_def_upper)|is.na(cohort_exp_def_lower), unique(get(cohort_exposed_def_var))]
             ), collapse = "; "
             )
      )
    )

  }
  return(data)
}


create_conf_ints <- function(input_data){
  #If missing upper/lower, but do have a CI_uncertainty_type_value:
  #1) Remove and tag the ones with no indication of significance (CI_uncertainty_type_value = 0)
  input_data[(is.na(upper) | is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 0, nonsig := 1]
  input_data[(is.na(upper) | is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 0, CI_uncertainty_type_value := NA]
  #table(input_data$nonsig, useNA = 'always')
  
  # 2) If only indication of significance from CI_uncertainty_type_value col (e.g 90, 95), Add p-values to nonCI_uncertainty_value for
  #nrow(input_data[(is.na(upper) | is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value %in% c(90,95,99,99.9)])
  message(paste0('Replace ', nrow(input_data[(is.na(upper)|is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value %in% c(90,95,99,99.9)]),
                 ' CI_values with respective relative p values, e.g. 95 --> p<0.05'))  
  input_data[(is.na(upper) | is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 90, p_value := 'p<0.1']
  input_data[(is.na(upper) | is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 95, p_value := 'p<0.05']
  input_data[(is.na(upper) | is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 99, p_value := 'p<0.01']
  input_data[(is.na(upper) | is.na(lower)) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 99.9, p_value := 'p<0.001']
  input_data[(is.na(upper) | is.na(lower)) &(nonCI_uncertainty_value %like% 'p'), p_value := nonCI_uncertainty_value]
  
  # remove the CI_uncertainty_type_value and nonCI_uncertainty_value from the ones that now have p-values
  input_data[!is.na(p_value), c('CI_uncertainty_type_value','nonCI_uncertainty_value')] <- NA
  input_data[!is.na(p_value), nonCI_uncertainty_type := 'SE from p-value']
  nrow(input_data[(is.na(upper) | is.na(lower)) & is.na(CI_uncertainty_type_value) & is.na(nonCI_uncertainty_value)& !is.na(p_value)])
  
  # 3) convert p-values to ln_SE 
  input_data[p_value %like% 'p>', c('nonCI_uncertainty_type','p_value')] <- NA
  nrow(input_data[!is.na(p_value)])
  
  # pull out p values that we can use (exact or p<)
  tempCI <- input_data[!is.na(p_value)]
  input_data <- input_data[is.na(p_value)]

  message(paste0('calculating se_ln for ',nrow(tempCI),' observations from p-values')) 
  #method: https://www.bmj.com/content/343/bmj.d2090
  tempCI[p_value %like% 'p=', p_value := str_sub(p_value,start=3)]
  tempCI[p_value %like% 'p<', p_value := str_sub(p_value,start=3)]
  table(tempCI$p_value)
  tempCI[,p_value := as.numeric(p_value)]
  tempCI[,se_ln := round(abs((log(rr))/(-0.862 + sqrt(0.743 - 2.404*log(p_value)))),4)]
  
  #visualize new ses
  gg1 <- ggplot(tempCI)+geom_hline(yintercept = 1)+
    geom_point(aes(x=cohort_exp_def_mid,y=rr, size = 1/(rr*se_ln)), alpha = 0.2)+
    ggtitle("SE's calculated from p-values")
  
  #4) calculate se_ln from those cases where there is upper and lower bounds and the CI uncertainty value
  input_data[,nonCI_uncertainty_value := as.numeric(nonCI_uncertainty_value)]
  #assume NAs with CIs are %95 CIs
  table(input_data[!is.na(upper)&!is.na(lower)&is.na(nonCI_uncertainty_value)]$CI_uncertainty_type_value, useNA = 'always')
  input_data[!is.na(upper)&!is.na(lower)&is.na(nonCI_uncertainty_value) &is.na(CI_uncertainty_type_value), CI_uncertainty_type_value := 95]
  
  # Change 0s to NAs and tag as nonsignificant
  input_data[!is.na(upper)&!is.na(lower)&is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 0, nonsig := 1]
  input_data[is.na(nonsig), nonsig := 0]
  input_data[!is.na(upper)&!is.na(lower)&is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 0, CI_uncertainty_type_value := NA]
  
  # tag as getting se from confidence interval
  table(input_data[!is.na(upper)&!is.na(lower)&is.na(nonCI_uncertainty_value)&!(is.na(CI_uncertainty_type_value))]$nonCI_uncertainty_type, useNA = 'always')
  input_data[!is.na(upper)&!is.na(lower)&is.na(nonCI_uncertainty_value)&!(is.na(CI_uncertainty_type_value)),nonCI_uncertainty_type := 'SE from confidence Interval']
  
  #calculate se_ln
  input_data[!is.na(upper) & !is.na(lower) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value  == 90,
             se_ln := (log(upper) - log(lower)) / (1.645 * 2)]
  input_data[!is.na(upper) & !is.na(lower) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value  == 95,
             se_ln :=  (log(upper) - log(lower)) / (1.96 * 2)]
  input_data[!is.na(upper) & !is.na(lower) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 99,
             se_ln := (log(upper) - log(lower)) / (2.58 * 2)]
  input_data[!is.na(upper) & !is.na(lower) & is.na(nonCI_uncertainty_value) & CI_uncertainty_type_value == 99.9,
             se_ln := (log(upper) - log(lower)) / (3.291 * 2)]
  
    #5 Combine ln_se from p-value and ln_se from SE
  input_data <- rbind(input_data, tempCI)
  input_data[,se := rr*se_ln]
  input_data[is.na(se_ln) & !(is.na(se)), se_ln := se/rr]
  summary(input_data$se_ln)
  summary(input_data$se)
  
  ## 6 fill in SE and se_ln from nonCI
  input_data[is.na(se) & nonCI_uncertainty_type %like% 'Reported' & !is.na(nonCI_uncertainty_value), se:= nonCI_uncertainty_value]
  input_data[is.na(se_ln) & !(is.na(se)), se_ln := se/rr]
  
  #6) for other observations missing information necessary to calculate CI, simulate them based on other examples
  training_data <- input_data[!is.na(se) & se > 0 & !is.na(cohort_sample_size_total) ] %>%
    .[,.(se,se_ln, cohort_sample_size_total)] %>% unique()
  
  # take 95th percentile of training data
  training_data <- training_data[inrange(cohort_sample_size_total,
                                         quantile(training_data$cohort_sample_size_total, .025),
                                         quantile(training_data$cohort_sample_size_total, .975))]
  training_data <- training_data[inrange(se,
                                         quantile(training_data$se, .025),
                                         quantile(training_data$se, .975))]
  
  #plot relationship b/w SS and SE
  gg2 <- ggplot(training_data) +
    geom_point(aes(x = log(as.numeric(cohort_sample_size_total)), y = log(se)))+
    ggtitle('Training Data for predicitng missing SE')
  
  #fit model
  model_SE_SS <- lm(log(se) ~ log(as.numeric(cohort_sample_size_total)),
                    data = training_data)
  
  # save predictions and plot
  predicted_df <- data.table(pred = predict(model_SE_SS, input_data), 
                             cohort_sample_size_total = input_data$cohort_sample_size_total)
  
  #fill in sample size for studies missing it using the 5%ile of all SS
  warning(paste0(nrow(input_data[is.na(cohort_sample_size_total)]),' cohort_sample_size_total vals are being assigned to 5th pctile'))
  input_data[is.na(cohort_sample_size_total), cohort_sample_size_total := quantile(input_data[,cohort_sample_size_total], .05, na.rm = T)]
  
  # create CI orig indicator and predict new SE
  input_data[,CI_orig := ifelse(is.na(se),0,1)]
  input_data[CI_orig == 0, nonCI_uncertainty_type := 'SE Predicted']
  
  warning(paste0(nrow(input_data[is.na(se_ln)]),' NonCI_uncertainty vals are being predicted'))
  
  input_data[is.na(se),se := exp(predict(model_SE_SS, newdata = .SD)), .SDcols = names(input_data)]
  
  quantile(input_data$se,seq(0,1, 0.05) )
  input_data[se<0.01, se := 0.01]# set to ~5th pctile
  input_data[,se_ln := se/rr]
  
  gg3 <- ggplot() + #pred is in log space, so dont exp to match log ....
    geom_jitter(data = input_data, aes(x = log(cohort_sample_size_total), y = log(se), 
                                       color=nonCI_uncertainty_type, 
                                       shape = factor(nonsig)), alpha = 0.6, size = 2) +
    geom_line(data = predicted_df, aes(x=log(cohort_sample_size_total), y=pred), color='red',)+
    ggtitle('Data and model fit predicting absent SE')
  
  
  gg4 <- ggplot() + #pred is in log space, so dont exp to match log ....
    geom_jitter(data = input_data[log(cohort_sample_size_total)>15], 
               aes(x = log(cohort_sample_size_total), y = log(se), color=factor(CI_orig),shape = factor(nonsig)) ) +
    geom_line(data = predicted_df[log(cohort_sample_size_total)>15], aes(x=log(cohort_sample_size_total), y=(pred)), color='red')+
    ggtitle('Data and model fit predicting absent SE')
  
  
  gg5 <- ggplot() + #pred is in log space, so dont exp to match log ....
    geom_jitter(data = input_data[log(cohort_sample_size_total)>15], 
               aes(x = log(cohort_sample_size_total), y = log(se), color=ihme_loc_id) ) +
    geom_line(data = predicted_df[log(cohort_sample_size_total)>15], aes(x=log(cohort_sample_size_total), y=(pred)), color='red')+
    ggtitle('Data and model fit predicting absent SE')
  
  
  #visualize new ses
  gg6 <- ggplot(input_data[rr<5])+geom_hline(yintercept = 1)+
    geom_jitter(aes(x=cohort_exp_def_mid,y=rr, size = 1/se, color = nonCI_uncertainty_type), alpha = 0.5)+
    ggtitle("All data + type of confidence measure")
  
  pdf(paste0(root, "/FILEPATH/se_diagnostics_",Sys.Date(),".pdf"), width = 12, height = 9)
  print(gg1)
  print(gg2)
  print(gg3)
  print(gg4)
  print(gg5)
  print(gg6)
  dev.off()
  
  input_data[,lower := exp(rr_ln - 1.96*se_ln)]
  input_data[,upper := exp(rr_ln + 1.96*se_ln)]

  return(input_data)
}


create_covariates <- function(working_data){ #cleans and graphs covariate distributions
  #typical covariates
  covs_summary <- working_data[,.SD,.SDcols = c(names(working_data)[names(working_data) %like% "confounders" & names(working_data) != "confounders_other"], "field_citation_value")] %>% unique()
  covs_string <- names(working_data)[names(working_data) %like% "confounders" & names(working_data) != "confounders_other"]
  covs_summary[, (covs_string) := lapply(.SD, as.numeric), .SDcols = covs_string]
  
  covs_counts <- data.table(reshape2::melt(covs_summary[,colSums(.SD, na.rm = T)/nrow(.SD), .SDcols = names(working_data)[names(working_data) %like% "confounders" & names(working_data) != "confounders_other"]]))
  covs_counts[, confounder := names(working_data)[names(working_data) %like% "confounders" & names(working_data) != "confounders_other"]]
  setnames(covs_counts, c("value"), ("prevalence"))
  covs_counts[, confounder := factor(confounder, levels = covs_counts[ order(prevalence)]$confounder)]
  
  #plot prevalence of covariates
  pdf(paste0(root, "/visuals/covariate_counts_orig.pdf"), onefile = T, width = 10, height = 8)
  gg1 <- ggplot(covs_counts) +
    geom_bar(aes(x = confounder, y = prevalence), stat= "identity") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent)+
    coord_flip()
  print(gg1)
  dev.off()
  
  
  working_data[, confounders_other_clean := tolower(gsub(" ","",gsub("[^a-zA-Z0-9 ., ]", "", confounders_other)))]
  
  #get list of other confounders
  other_covs <- unique(c(other_covs, unique(cov_map$new_covariate)))
  other_cov_dt <- data.table()
  for(c.cov in unique(other_covs)){
    temp <- data.table(confounder = c.cov,
                       count = nrow(unique(working_data[confounders_other_clean %like% c.cov, .(confounders_other_clean)])))
    other_cov_dt <- rbind(other_cov_dt, temp)
  }
  
  other_cov_dt[, confounder := factor(confounder, levels = other_cov_dt[ order(count)]$confounder)]
  other_cov_dt[, prevalence := count/nrow(unique(working_data[,.(field_citation_value)]))]
  
  pdf(paste0(root, "/visuals/covariate_counts_free_text.pdf"), onefile = T, width = 10, height = 8)
  gg1 <- ggplot(other_cov_dt[count > 15]) +
    geom_bar(aes(x = confounder, y = prevalence), stat= "identity") +
    theme_bw() +
    coord_flip() + xlab("Other Confounders") + ylab("Prevalence") +
    ggtitle("Prevalence of Other Study Covariates As a Proportion\nof Non-Standard covariates")
  
  print(gg1)
  dev.off()
  
  covs_summary <- working_data[,.SD,.SDcols = c(names(working_data)[names(working_data) %like% "confounders" & !(names(working_data) %in% c("confounders_other", "confounders_other_clean"))], "field_citation_value")] %>% unique()
  covs_string <- names(working_data)[names(working_data) %like% "confounders" & !(names(working_data) %in% c("confounders_other", "confounders_other_clean"))]
  covs_summary[, (covs_string) := lapply(.SD, as.numeric), .SDcols = covs_string]
  
  #counts
  covs_counts <- data.table(reshape2::melt(covs_summary[,colSums(.SD, na.rm = T)/nrow(.SD), .SDcols =names(working_data)[names(working_data) %like% "confounders" & !(names(working_data) %in% c("confounders_other", "confounders_other_clean"))]]))
  covs_counts[, confounder := names(working_data)[names(working_data) %like% "confounders" & !(names(working_data) %in% c("confounders_other", "confounders_other_clean"))]]
  setnames(covs_counts, c("value"), c("prevalence"))
  covs_counts[, confounder := factor(confounder, levels = covs_counts[ order(prevalence)]$confounder)]
  covs_counts[, confounder_pretty := gsub("confounders_", "", confounder)]
  covs_counts[, confounder_pretty := gsub("_", " ", confounder_pretty, fixed = T)]
  covs_counts[, confounder_pretty := gsub(".", " ", confounder_pretty, fixed = T)]
  
  camel <- function(x){ #function for camel case
    capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
    sapply(strsplit(x, " "), function(x) paste(capit(x), collapse=" "))
  }
  
  covs_counts[, confounder_pretty := camel(confounder_pretty)]
  covs_counts <- covs_counts[order(prevalence, decreasing = T)]
  covs_counts[confounder_pretty == "Otherparenteducation", confounder_pretty := "Other Parent's Education"]
  covs_counts[confounder %like% "bmi", confounder_pretty := "Body Mass Index"]
  covs_counts[confounder %like% "sex", confounder_pretty := "Sex"]
  covs_counts[confounder %like% "age", confounder_pretty := "Age"]
  covs_counts[confounder %like% "water", confounder_pretty := "Access to Safe Water"]
  covs_counts[confounder %like% "rural", confounder_pretty := "Rural/Urban Residence"]
  covs_counts[confounder %like% "birthinterv", confounder_pretty := "Birth interval"]
  covs_counts[confounder %like% "birthorder", confounder_pretty := "Birth Order"]
  
  covs_counts <- covs_counts[!confounder %like% "index"]
  covs_counts[, confounder_pretty := factor(confounder_pretty, levels = rev(covs_counts$confounder_pretty))]
  #plot prevalence of covariates
  pdf(paste0(root, "/visuals/covariate_counts_all.pdf"), onefile = T, width = 10, height = 8)
  gg1 <- ggplot(covs_counts[prevalence > 0.000]) +
    geom_bar(aes(x = confounder_pretty, y = prevalence), stat= "identity") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent)+
    coord_flip() + xlab("Confounders") + ylab("Prevalence") +
    ggtitle("Prevalence of Study Covariates As a Proportion of All Covariates-Study Combinations")
  print(gg1)
  dev.off()
  
  return(working_data)
}
