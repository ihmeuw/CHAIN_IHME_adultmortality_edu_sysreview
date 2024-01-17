################################################################################
# Cleaning of Adult Mortality Extractions
################################################################################
#### Setup #####################################################################
pacman::p_load(data.table, plyr, dplyr, ggplot2, stringr,tidyverse,RColorBrewer)

## Set up root directories
root <- "FILEPATH"
input_dir <- paste0(root, "/inputs")
output_dir <- paste0(root, "/outputs")

source(paste0("/FILEPATH/99_cleaning_fxns.R"))

locs <- fread('/FILEPATH/location_metadata.csv')

extraction <- fread(paste0(input_dir,"/raw_extractions.csv"))

#### Clean colnames, covs, locations, and subgroups ######################
names(extraction) <- gsub(" ","_",names(extraction))

working_data <- extraction[,.(field_citation_value, location_name, location_id, rep_geography,
                              year_start_study, year_end_study, age_start, age_end, percent_male, design,
                              duration_fup_measure,duration_fup_units,	value_of_duration_fup,
                              Sub_GENDER,	Sub_AGE,	Sub_AGE_start,	Sub_AGE_end, Sub_Cohort, Sub_RACE, Sub_OTHERS,
                              subgroup_analysis_free_text,
                              exp_type, effect_size_measure, effect_size,dose_response,dose_response_detail,
                              cohort_sample_size_exp, cohort_sample_size_unexp, cohort_sample_size_total,
                              cohort_exposed_def1, cohort_exposed_def2, cohort_unexp_def1, cohort_unexp_def2,cohort_unexp_level_rr,cohort_exp_level_dr,
                              lower, upper, CI_uncertainty_type_value, nonCI_uncertainty_value, nonCI_uncertainty_type,
                              cc_cases,cc_control,Absolute_sample_size_used, Absolute_number_of_death, Mortality_rate_unexposed,Mortality_rate_exposed_group)]

#add covariates
covs <- extraction[,.SD,.SDcols = names(extraction)[names(extraction) %like% "confounders"]]
other_covs <- covs$confounders_other %>%
  strsplit(split = ",") %>%
  unlist %>%
  tolower() %>%
  gsub("[^a-zA-Z0-9 ., ]", "", .) %>%
  gsub(" ", "", .) %>%
  unique() %>%
  .[!is.na(.)]

other_covs <- other_covs[other_covs != ""]
other_covs <- other_covs[other_covs != " "]
working_data <- as.data.table(cbind(working_data, covs))

# add other_covariates column (cleaned version)
working_data <- create_covariates(working_data)

#create ihme_loc_id indicator and merge on location hierarchy, fix character issues
working_data[location_name == "East Asia", location_name := "East Asia|R3"]
working_data[location_name %like% "Nord-Tr", location_name := "Nord-Trondelag|NOR_4925"]
working_data[, ihme_loc_id := tstrsplit(location_name, "|", keep = 2, fixed = T)]
working_data[, ihme_loc_id := tstrsplit(ihme_loc_id, "_", keep = 1, fixed = T)]
working_data$location_id <- NULL

working_data <- merge(working_data, locs[,.(ihme_loc_id,location_id, region_name, super_region_name)], by = "ihme_loc_id", all.x = T)
working_data <- working_data[!is.na(super_region_name)] 

## add state to subgroup_other for one case, used to downweight the ~500 effect measures from this study by state
fix_study <- 'Educational Disparities in Adult Mortality Across U.S. States: How Do They Differ, and Have They Changed Since the Mid-1980s?'
working_data[field_citation_value==fix_study, Sub_OTHERS := str_sub(subgroup_analysis_free_text, start=23)]
working_data[field_citation_value==fix_study & Sub_OTHERS=="" & subgroup_analysis_free_text %like% 'Women,', Sub_OTHERS := str_sub(subgroup_analysis_free_text, start=8)]
working_data[field_citation_value==fix_study & Sub_OTHERS=="" & subgroup_analysis_free_text %like% 'Men,', Sub_OTHERS := str_sub(subgroup_analysis_free_text, start=6)]

#### Standardize Exposure Intervals ###############################################################################
for(c.col in c('cohort_unexp_def1','cohort_exposed_def1')){
  working_data[get(c.col)=='n.d.' | get(c.col)=='n.c' | get(c.col)==''| get(c.col)=='N/A' | get(c.col)=='NA', paste0(c.col) := NA]
}

# Edit DR studies w/out exp intervals
dr_names <- unique(working_data[dose_response==1]$field_citation_value)
dr_studies <- working_data[field_citation_value %in% dr_names]
dr_fixed <- fread('/FILEPATH/dr_studies_check_final.csv')

# #remove original DR studies and add back fixes (added max and min education range)
working_data <- working_data[!(field_citation_value %in% dr_studies$field_citation_value)]
working_data <- rbind(working_data,dr_fixed,fill = T)

# remove any still missing ref/exp data
working_data <- working_data[!(is.na(cohort_exposed_def1) | is.na(cohort_unexp_def1))] 
working_data <- working_data[!(cohort_exposed_def1 == '' | cohort_unexp_def1 == '')] 

# create standardized exposure interval bins
source(paste0(root,"/code/99_cleaning_fxns.R"))
working_data <- create_bins(working_data, "cohort_exposed_def1", "cohort_unexp_def1")

#### Clean effect sizes & Create SE for those missing them #######################################################

#replace character N/A, n.d, ' ' for actual NA in all relevant vars
for(c.col in c('upper','lower','effect_size','CI_uncertainty_type_value','nonCI_uncertainty_value','nonCI_uncertainty_type','cohort_sample_size_total','cohort_sample_size_exp','cohort_sample_size_unexp','Absolute_sample_size_used','cc_cases','cc_control')){
  message(paste0(c.col, ' Number changed: ', nrow(working_data[get(c.col)=='n.d.' | get(c.col)==''| get(c.col)=='N/A'| get(c.col)=='NA'])))
  working_data[get(c.col)=='n.d.'|get(c.col)=='n.c'|get(c.col)==''|get(c.col)=='N/A'|get(c.col)=='NA'|get(c.col)=='NDA', paste0(c.col) := NA]
}

#coerce effect size, upper, lower, nonCI_uncertainty_value to numeric
working_data[,upper := as.numeric(gsub(',','.',upper))]
working_data[,lower := as.numeric(gsub(',','.',lower))]
working_data[,effect_size := as.numeric(gsub(',','.',effect_size))]
working_data[,nonCI_uncertainty_value := gsub(',','.',nonCI_uncertainty_value)]

# Clean sample size vars
working_data[, cohort_sample_size_total := as.numeric(gsub("[^0-9.-]", "", cohort_sample_size_total))] 
working_data[, Absolute_sample_size_used := as.numeric(gsub("[^0-9.-]", "", Absolute_sample_size_used))]  
working_data[, cohort_sample_size_exp := as.numeric(gsub("[^0-9.-]", "", cohort_sample_size_exp))]  
working_data[, cohort_sample_size_unexp := as.numeric(gsub("[^0-9.-]", "", cohort_sample_size_unexp))]
working_data[, cc_cases := as.numeric(gsub("[^0-9.-]", "", cc_cases))]  
working_data[, cc_control := as.numeric(gsub("[^0-9.-]", "", cc_control))]  
working_data[design %like% "ase" & is.na(cohort_sample_size_total), cohort_sample_size_total := cc_cases + cc_control] 
working_data[design %like% "ase" & is.na(cohort_sample_size_total) & (!is.na(cc_cases) & is.na(cc_control)), cohort_sample_size_total := cc_cases]
working_data[design %like% "ase" & is.na(cohort_sample_size_total) & (is.na(cc_cases) & is.na(cc_control)), cohort_sample_size_total := Absolute_sample_size_used]#5
working_data[is.na(cohort_sample_size_total), cohort_sample_size_total := cohort_sample_size_exp + cohort_sample_size_unexp]

# clean CI and non CI uncertainty 
working_data[(!is.na(upper) & !is.na(lower) & 
                !is.na(CI_uncertainty_type_value) & 
                !is.na(nonCI_uncertainty_value) & 
                is.na(nonCI_uncertainty_type)),
             nonCI_uncertainty_value := NA]

#### Calculate SE for observations missing it #####
working_data[,rr:= effect_size]
working_data[,rr_ln := log(rr)]
working_data[,upper_orig := upper]
working_data[,lower_orig := lower]
working_data[tolower(nonCI_uncertainty_type) %like% "standard|se", nonCI_uncertainty_type := 'Reported Standard Error']
source(paste0(root,"/code/99_cleaning_fxns.R"))
working_data <- create_conf_ints(working_data) 

#### Standardize exposure direction (higher ed is exposure, lower is reference) #####################################################
temp_invert <- working_data[cohort_exp_def_mid < cohort_unexp_def_mid] # if exposed is less than unexposed, subset, remove from working_data
temp_working_data <- working_data[cohort_exp_def_mid > cohort_unexp_def_mid] # keep where exposed is >/= unexposed
temp_working_data[,es_orig := 1]

# reciprocal of effect size, upper and lower CI
temp_invert <- temp_invert[, effect_size_invert := 1/rr]
temp_invert <- temp_invert[, upper_invert := 1/upper]
temp_invert <- temp_invert[, lower_invert := 1/lower]
temp_invert[, rr := NULL]
temp_invert[, upper := NULL]
temp_invert[, lower := NULL]
temp_invert[,es_orig := 0]

setnames(temp_invert, c("effect_size_invert", "upper_invert", "lower_invert"), c("rr", "lower", "upper"))
setnames(temp_invert, c("cohort_sample_size_exp", "cohort_sample_size_unexp", "cohort_exposed_def1", "cohort_exposed_def2", "cohort_unexp_def1", "cohort_unexp_def2", "cohort_exposed_var_arch", "cohort_exp_def_lower", "cohort_exp_def_upper", "cohort_exp_def_mid", "clean_cohort_exp_def", "cohort_unexp_var_arch", "cohort_unexp_def_lower", "cohort_unexp_def_upper", "cohort_unexp_def_mid", "clean_cohort_unexp_def"),
                      c("cohort_sample_size_unexp", "cohort_sample_size_exp", "cohort_unexp_def1", "cohort_unexp_def2", "cohort_exposed_def1", "cohort_exposed_def2", "cohort_unexp_var_arch", "cohort_unexp_def_lower", "cohort_unexp_def_upper", "cohort_unexp_def_mid", "clean_cohort_unexp_def", "cohort_exposed_var_arch", "cohort_exp_def_lower", "cohort_exp_def_upper", "cohort_exp_def_mid", "clean_cohort_exp_def"))

#recalculate SE from those that were just flipped
temp_invert[!is.na(upper) & !is.na(lower), se_ln := (log(upper) - log(lower)) / 3.92]
temp_invert[,se := se_ln*rr]
temp_invert[,rr_ln := log(rr)]

list <- list(temp_working_data, temp_invert)
working_data <- rbindlist(list, use.names = TRUE )

#### Identify any remaining extractions to quality check ###########################################################################################################

missing_exp_ranges <- working_data[!complete.cases(working_data[,.(
  cohort_exp_def_lower, cohort_unexp_def_lower,
  cohort_exp_def_upper, cohort_unexp_def_upper)]), unique(field_citation_value)]
overlapping_exposure_ref <- working_data[(cohort_unexp_def_lower >= cohort_exp_def_lower & cohort_unexp_def_lower <= cohort_exp_def_upper)|
                                           (cohort_unexp_def_upper >= cohort_exp_def_lower & cohort_unexp_def_upper <= cohort_exp_def_upper)|
                                           (cohort_exp_def_lower >= cohort_unexp_def_lower & cohort_exp_def_lower <= cohort_unexp_def_upper)|
                                           (cohort_exp_def_upper >= cohort_unexp_def_lower & cohort_exp_def_upper <= cohort_unexp_def_upper), unique(field_citation_value)]
CI_off <- working_data[!(((upper - effect_size )) >= 0), unique(field_citation_value)]
negative_effect_sizes <- working_data[effect_size < 0, unique(field_citation_value)]
check_large_effect_sizes <-working_data[effect_size > 2, unique(field_citation_value)]
non_RR_OR <- working_data[!tolower(effect_size_measure) %like% "risk|odds|ratio", unique(field_citation_value)]
no_effect_size <- working_data[effect_size %like% "n.d.", unique(field_citation_value)]
exp_ref_same <- working_data[cohort_exp_def_mid == cohort_unexp_def_mid, unique(field_citation_value)]
reversed_signs <- working_data[((cohort_exp_def_mid > cohort_unexp_def_mid & lower > 1)|
                                  (cohort_exp_def_mid < cohort_unexp_def_mid & upper < 1)), unique(field_citation_value)]
issue_year_study_missing <- working_data[year_start_study == "n.d." | year_end_study == "n.d."]
issue_age_missing <- working_data[age_start == "n.d."| age_end == "n.d."]
issue_location_id <- working_data[location_id %like% "#N/A|N/A"]
issue_effect_size_missing <- working_data[effect_size %like% "n.d." | is.na(effect_size)]
issue_exposure_def_missing <- working_data[cohort_exposed_def1 ==""]
year_study_missing <- issue_year_study_missing[,unique(field_citation_value)]
age_missing <- issue_age_missing [,unique(field_citation_value)]
location_id_missing <- issue_location_id[,unique(field_citation_value)]
effect_size_missing <- issue_effect_size_missing[,unique(field_citation_value)]
exposure_def_missing <- issue_exposure_def_missing[,unique(field_citation_value)]

prob_list <- c("missing_exp_ranges", "overlapping_exposure_ref", "CI_off",
               "negative_effect_sizes", "check_large_effect_sizes", "non_RR_OR",'no_effect_size',
               "exp_ref_same", "reversed_signs", "year_study_missing", "age_missing",
               "location_id_missing", "effect_size_missing", "exposure_def_missing")

problems <- data.table(field_citation_value = unique(working_data$field_citation_value))
problems <- problems[!duplicated(field_citation_value)]

for(c.prob in prob_list){
  print(c.prob)
  c.prob_eval <- get(c.prob)
  print(length(c.prob_eval))
  problems[field_citation_value %in% c.prob_eval, paste0(c.prob) := 1]
  problems[is.na(get(c.prob)),paste0(c.prob) := 0]
}

#add total number of problems
problems$num_probs <- rowSums(problems[,2:15])

#add name of extractor
problems <- merge(problems,extraction[,c("field_citation_value","extractor")],all.x=T)
problems <- problems[!duplicated(field_citation_value)]

fwrite(problems, paste0("/FILEPATH/qualitycheck_extractions_",Sys.Date(),".csv"))

#### Final clean of data set #########################################################
#subset complete cases
findata <- working_data[complete.cases(working_data[,.(rr,se, upper, lower,rr_ln, se_ln,cohort_exp_def_lower, cohort_unexp_def_lower, cohort_exp_def_upper, cohort_unexp_def_upper,cohort_sample_size_total)])]
names(findata) <- gsub(" |-|, ","_",names(findata))

for(c.col in c('rr','se', 'upper', 'lower','rr_ln', 'se_ln',
               'CI_uncertainty_type_value','nonCI_uncertainty_value','nonCI_uncertainty_type',
               'cohort_sample_size_total','cohort_sample_size_exp','cohort_sample_size_unexp',
               'Absolute_sample_size_used','cc_cases','cc_control', 'super_region_name','location_name')){
  message(paste0(c.col, ' Number changed: ', nrow(findata[get(c.col)=='n.d.' | get(c.col)==''| get(c.col)=='N/A'| get(c.col)=='NA'])))
  findata[get(c.col)=='n.d.'|get(c.col)==''|get(c.col)=='N/A'|get(c.col)=='NA'|get(c.col)=='NDA', paste0(c.col) := NA]
}

## CLEAN & FORMAT ##
#calculate n_effect per study & ncrease variance on effect sizes from same population 
findata[,n_effect := .N, by = c('field_citation_value','age_start', 'subgroup_analysis_free_text','Sub_GENDER','Sub_AGE','Sub_AGE_start','Sub_AGE_end','Sub_Cohort','Sub_RACE','Sub_OTHERS',
                                           'cohort_sample_size_total','Absolute_sample_size_used','clean_cohort_exp_def','clean_cohort_unexp_def')]
# Increase SE
findata[,adjusted_se := sqrt(se^2 /(1/n_effect))]

# replace se with adjusted_se
findata[,se := adjusted_se]
findata[,se_ln := se/rr]
findata$adjusted_se <- NULL

# transform Dose Response effect sizes in LOG SPACE with exp/reference information # and replace normal space vals as well for accurate graphing
findata[dose_response == 1, temp := (cohort_exp_def_mid-cohort_unexp_def_mid)/2]
findata[dose_response == 1, rr_ln := rr_ln*temp]
findata[dose_response == 1, se_ln := se_ln*temp]
findata$temp <- NULL
findata[dose_response == 1, rr :=  exp(rr_ln)]
findata[dose_response == 1, se :=  rr*se_ln]
findata[dose_response == 1, upper := exp(rr_ln  + (1.96*se_ln))]
findata[dose_response == 1, lower := exp(rr_ln  - (1.96*se_ln))]

# calculate final vals for everything
findata[,upper_ln:=log(upper)]
findata[,lower_ln:=log(lower)]

# Create other  model/visualization values
findata[, denom_int := (cohort_exp_def_mid - cohort_unexp_def_mid)] #difference in midpoints of exp & unexp intvls
findata[, obs_slope := rr_ln/denom_int]

findata[, age_start := round(as.numeric(age_start))] %>%
  .[, age_end := round(as.numeric(age_end))] %>%
  .[, year_end_study := round(as.numeric(year_end_study))] %>%
  .[, year_start_study := round(as.numeric(year_start_study))]

under_18 <- (findata[age_start < 18, .(field_citation_value,location_name,age_start)])
under_18 <- findata[field_citation_value %in% under_18$field_citation_value]
under_18[,diff := age_end-age_start]
under_18[,pct_u_18 := ((18-age_start)/diff)*100]
View(unique(under_18[,.(field_citation_value,age_start,age_end,diff,pct_u_18)]))

fwrite(under_18, paste0(root, "/outputs/cleaned_datasets/findata_under_age_18studies_",Sys.Date(),".csv"))
findata[age_start < 18, age_start := 18] #change for eight studies with small portions under 18 of total sample (e.g. was 17-49)

#Make age interval and age midpoint, age groups
findata[, age_range := age_end - age_start]
findata[, age_interval_mdpt := (age_end + age_start) /2]
findata[age_interval_mdpt <50, age_group := '15 to 49']
findata[is.na(age_group) & age_interval_mdpt < 60, age_group := '50 to 59']
findata[is.na(age_group) & age_interval_mdpt < 70, age_group := '60 to 69']
findata[is.na(age_group), age_group := '> 70']
findata[,age10_mid:=(floor(age_interval_mdpt/10)*10)]
findata[age10_mid <20,age10_mid := 20]
findata[,age10_mid_name := factor(paste0(age10_mid,'-',(age10_mid+9)))]
findata[age10_mid_name == '20-29',age10_mid_name := '18-29']
findata[,age10_mid := factor(age10_mid)]

# make year midpoint 
findata[, year_mdpt := round((year_end_study + year_start_study)/2)]
findata[, year_mdpt_decade := (floor(year_mdpt/10))*10]

# clean locations
findata[is.na(super_region_name) & (location_name == "Asia" | location_name == "South Asia"), super_region_name := "South Asia"]
findata[location_id == 4925, super_region_name:= 'High-income']
findata[location_id == 4925, ihme_loc_id:= 'NOR']
findata[is.na(super_region_name), super_region_name := "Multiple Regions"]
findata[,region_name:=NULL]
findata[location_name == 'United States|USA', location_id := 102]

findata <- findata[!(is.na(ihme_loc_id))]
findata <- findata[super_region_name!=""]

findata <- merge(findata,locs[,c("region_name","ihme_loc_id"),with=T],by='ihme_loc_id',all.x=T)

## add SDI 
sdi <- fread('/FILEPATH/SDI_estimates.csv')
setnames(sdi,c('mean_value', 'year_id'),c('sdi','year_mdpt'))

findata <- merge(sdi[,.(location_id, year_mdpt, sdi)], findata, by = c('location_id','year_mdpt'), all.y=T)
findata[is.na(sdi),.(location_name, location_id,year_mdpt)]
findata[is.na(sdi) & year_mdpt<1900 & location_name == 'France|FRA', sdi := 0.5407848]
findata[is.na(sdi) & year_mdpt<1900 & location_name == 'Italy|ITA', sdi := 0.5256069]
findata <- findata[!is.na(sdi)]

# fill in confounders with 0s if NA
covlist <- names(findata)[names(findata) %like% "confounders"]
for (c.conf in covlist){
  message(nrow(findata[is.na(get(c.conf))]))
  findata[is.na(get(c.conf)), paste0(c.conf) := 0]
}


## Add Gender using subtype_gender and percent_male
if(!('Sub_GENDER' %in% names(findata))){
  findata$Sub_GENDER <- NA
}
findata[Sub_GENDER=='n.d.'|Sub_GENDER==''|Sub_GENDER=='N/A'|Sub_GENDER=='NA'|Sub_GENDER=='NDA', Sub_GENDER := NA]
findata[,percent_male := gsub(',','.',findata$percent_male)]
findata[percent_male=='n.d.'|percent_male==''|percent_male=='N/A'|percent_male=='NA'|percent_male=='NDA', percent_male := NA]
findata[,percent_male := as.numeric(percent_male)]
findata[percent_male==1, Gender := 'M']
findata[Sub_GENDER=='M', Gender := 'M']
findata[percent_male==0, Gender := 'F']
findata[Sub_GENDER=='F', Gender := 'F']

# add cohort and time period
findata[,cohort10 := floor((year_mdpt-age_interval_mdpt)/10)*10]
findata[,cohort := round(year_mdpt-age_interval_mdpt)]
findata[,cohort_group := cut(cohort, c( 0,1930,1950,1990),labels = c('1870-1929 cohort','1930-1949 cohort', '1950-1990 cohort'), include.lowest = TRUE)]
findata[,period := year_mdpt]
findata[,period_group := cut(period, c( 0,1990,2000,2021),labels = c('1893-1989 time period','1990 to 1999 time period', '2000-2020 time period'), include.lowest = TRUE)]

#add  World Bank HIC/LIC groups
hist_incomelocs <- fread(paste0('/FILEPATH/WB_income_levels_OGHIST.csv'),header = TRUE) #https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
hist_incomelocs <- melt.data.table(hist_incomelocs,id.vars = c('ihme_loc_id', 'location_name'),variable.name = 'year_mdpt',value.name = 'income_level')
hist_incomelocs[,year_mdpt := as.numeric(as.character(year_mdpt))]
hist_incomelocs[income_level=='',income_level := NA]
hist_incomelocs[is.na(income_level)&ihme_loc_id=='RUS',income_level := 'LM']
hist_incomelocs[is.na(income_level)&ihme_loc_id=='CZE',income_level := 'UM']
findata <- merge(findata, hist_incomelocs[,.(ihme_loc_id,year_mdpt,income_level)], by = c('ihme_loc_id','year_mdpt'), all.x = T)
findata[income_level %in% c('World Bank Low Income','World Bank Lower Middle Income','World Bank Upper Middle Income'),income := 'LMIC']
findata[income_level == 'World Bank High Income',income := 'HIC']

# # make data id and study ID 
findata[, seq := 1:nrow(findata)] #create unique line id
citations <- unique(findata[,.(field_citation_value)])
citations[, study_num := 1:nrow(citations)] #create unique line id
findata <- merge(findata,citations, by = 'field_citation_value',all = T)

#### Final Save #########################################################

fwrite(findata, paste0("/FILEPATH/SR_extraction_cleaned_",Sys.Date(),".csv")) #model data

final_observations_key <- unique(findata[,.(seq,study_num,field_citation_value,dose_response)])
final_studies_key <- unique(findata[,.(study_num,field_citation_value,authors,`pub year`,dose_response)])
savedata <- findata[, !c('Note','REEXTRACTED_REFEXP','unclear_direction','nonsig', 'CI_orig','es_orig'), with=FALSE]
fwrite(savedata,paste0('/FILEPATH/Supplementary_appendix_2_',Sys.Date(),'.csv'))
fwrite(final_studies_key,paste0('/FILEPATH/Supplementary_appendix_3_',Sys.Date(),'.csv'))