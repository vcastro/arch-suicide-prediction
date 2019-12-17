#
# Install required packages
#
require("pROC")


working_dir = '/Users/XXX/WORK_DIR/'
training_path = paste(working_dir, 'training',sep='/'); if(!file.exists(training_path)) dir.create(training_path)
testing_path = paste(working_dir, 'testing',sep='/'); if(!file.exists(testing_path)) dir.create(testing_path)
results_dir = paste(working_dir, 'results',sep='/'); if(!file.exists(results_dir)) dir.create(results_dir)

# #################################################################
#
#               PART 1 – DESCRIPTIVE STATISTICS
#
# #################################################################

# ##################################################################
#                         TABLE 1
# ##################################################################
setwd(working_dir)
addCountAndPercentageForEachValue <- function(var, total){
  res = data.frame()
  for(l in levels(var)){
    count = sum(!is.na(var) & var == l); prcnt = round(100 * count / total, digits = 1)
    res = rbind(res, data.frame(VAR=l, VAL=paste(count,' (',prcnt,'%)', sep = '')))
  }
  return(res)
}

demographics = readRDS('demographics.RDs') # including case definition as a variable called 'case_any'

table1 = data.frame()
table1 = rbind(table1, addCountAndPercentageForEachValue(demographics$gender, nrow(demographics)))
table1 = rbind(table1, data.frame(VAR='age', VAL=paste(mean(demographics$age, na.rm=T), ' SD = ', sd(demographics$age, na.rm=T)) ))
table1 = rbind(table1, addCountAndPercentageForEachValue(demographics$race, nrow(demographics)))
#printCountAndPercentageForEachValue(demographics$marital_status, nrow(demographics))
table1 = rbind(table1, addCountAndPercentageForEachValue(demographics$case_any, nrow(demographics)))

saveRDS(table1,  paste(results_dir, 'Table1.RDs',sep='/'))

# #################################################################
#
#     PART 2 – MAKING SURE WE HAVE ALL THE REQUIRED DATASETS
#
# #################################################################

setwd(working_dir)
demographics = readRDS('demographics.RDs') # including case definition
dxproc = readRDS('dxproc.RDs')
meds = readRDS('meds.RDs')
lab.files = paste(working_dir, list.files(path = "labs", full.names = T), sep='/') # labs is very large dataset, we have a list of files that together make the entire labs dataset

# #################################################################
#
#       PART 3 – DIVIDE DATA TO TRAINING AND TESTING SETS
#
# #################################################################

# --------------------------------------------------
# PART 3A: Divide subjects into 10 different cohorts
# --------------------------------------------------
num_of_cohorts = 10
demographics$group_num = sample(1:num_of_cohorts, nrow(demographics), replace = T)

# --------------------------------------------------
# PART 3B: Split data to training & testing
# --------------------------------------------------
saveTrainingAndTestingSets <- function(df, training_ids, testing_ids, cases, file_name)
{
  df$case_any = df$patient_num %in% cases;
  saveRDS(df[df$patient_num %in% training_ids, ], paste(training_path, file_name,sep='/'))
  saveRDS(df[df$patient_num %in% testing_ids, ], paste(testing_path, file_name,sep='/'))
  print(paste(file_name,': cases=',sum(df$case_any), ', controls=',sum(!df$case_any),', training_size=',sum(df$patient_num %in% training_ids), 'testing_size=',sum(df$patient_num %in% testing_ids)))
}

training_ids = demographics$patient_num[demographics$group_num %in% c(1:5)]
testing_ids = demographics$patient_num[demographics$group_num %in% c(6:10)]
cases = demographics$patient_num[demographics$case_any == TRUE]
print(paste('training=',length(training_ids), ', testing=',length(testing_ids),', cases=',length(cases)))
print(paste('training cases=',sum(training_ids %in% cases), ', testing cases=',sum(testing_ids %in% cases)))
print(paste(demographics$patient_num[1:10], demographics$case_any[1:10]))
print(paste(dxproc$patient_num[1:10], dxproc$patient_num[1:10] %in% cases))

saveTrainingAndTestingSets(dxproc, training_ids, testing_ids, cases, 'dxproc.RDs')
rm(dxproc); gc()

saveTrainingAndTestingSets(meds, training_ids, testing_ids, cases, 'meds.RDs')
rm(meds);gc();

saveTrainingAndTestingSets(demographics, training_ids, testing_ids, cases, 'demographics.RDs')

# --------------------------------------------------
#
# PART 3C: Truncate data from index event onwards
#
# --------------------------------------------------

# Suicide Attempt defined as an ICD-9 code of E95*.*, 965.*, 967.*, 969.* (suicide narrow definition) or 
# 881.x (suicide broad definition)
identifyCaseDefinition <- function(dxproc)
{
  dxproc$code_case_narrow = dxproc$code_case_broad = FALSE
  dxproc$code_case_narrow[grep('(^E95)|(^965)|(^967)|(^969)', dxproc$concept_cd, ignore.case = T)] = TRUE
  dxproc$code_case_broad[grep('^881', dxproc$concept_cd)] = TRUE
  dxproc$code_case_any = (dxproc$code_case_broad | dxproc$code_case_narrow)
  return(dxproc)
}


#
# Get the date of the first suicidal event for each of the case-subjects
# input: dxproc dataset
# output: a data.frame with the index_date for each subject (based on case definition for meds & dxproc files)
#
getIndexEvent <- function(dxproc)
{
  # visits_with_cases
  
  suicidal_events = dxproc[dxproc$code_case_any, c('patient_num', 'concept_date')];
  suicidal_events = suicidal_events[order(suicidal_events$patient_num, suicidal_events$concept_date), ]
  suicidal_events = suicidal_events[!duplicated(suicidal_events$patient_num), ]
  colnames(suicidal_events) <- c('patient_num', 'index_date')

  return(suicidal_events)
}



#
# Remove all data occuring after the index event
# input: df: a data.frame to filter (with a column named 'concept_date'), ref_dates: the dates of the index event
# output: the input data.frame (df) without data occuring after index event
#
filterDataOutsideDates <- function(df, ref_dates)
{
  df$row_num = 1:nrow(df)
  cases = which(df$case_any); controls = which(!df$case_any);
  df_cases = merge(df[cases,], ref_dates, by='patient_num', all.x=T, all.y=F)

  excluded_cells = df_cases$row_num[which(is.na(df_cases$index_date) | (df_cases$concept_date >= df_cases$index_date))]
  print(paste('number of case-subjects excluded: ', sum(!duplicated(df$patient_num)) - sum(!duplicated(df$patient_num[-excluded_cells]))))

  return(excluded_cells)
}

#
# Go over training & testing sets and truncate dxproc, meds, and demographics data after index event.
#
for (path_name in c(training_path, testing_path))
{
  setwd(path_name)
  dxproc = readRDS('dxproc.RDs')
  meds = readRDS('meds.RDs')
  demographics = readRDS('demographics.RDs')

  dxproc = identifyCaseDefinition(dxproc)
  suicidal_events = getIndexEvent(dxproc)
  saveRDS(suicidal_events, 'suicidal_events.RDs');

  dx_idxs = filterDataOutsideDates(dxproc[,c('patient_num','concept_date','case_any')], suicidal_events)
  dxproc = dxproc[-dx_idxs, ]
  saveRDS(dxproc, 'dx_trunc.RDs');

  meds_idxs = filterDataOutsideDates(meds[,c('patient_num','concept_date','case_any')], suicidal_events)
  meds = meds[-meds_idxs, ]
  saveRDS(meds, 'meds_trunc.RDs')

  demo_trunc = demographics[demographics$patient_num %in% unique(c(dxproc$patient_num, meds$patient_num)), ]
  saveRDS(demo_trunc, 'demo_trunc.RDs')
}

#
# since labs is a big file we filter it after getting the list of subjects using meds/dxproc datasets
# input: lab_files: list of files with lab data, ids: patient_num to be included in the result
# output: the lab data for the given ids, ordered by date and without duplicate labs for the same visit
#
getSubsetOfLabs <- function(lab_files, ids)
{
  cols = c('patient_num', 'UniqueIdentifier', 'concept_date', 'suiciderelated_fact')
  df_list = list()
  for (file_name in lab_files){
    df = readRDS(file_name)[,cols]
    print(paste(file_name,':',paste(names(df),collapse = ', ')))
    print(df[1:2,])
    colnames(df) = c('patient_num', 'concept_cd', 'concept_date', 'suiciderelated_fact')
    df = df[which(df$patient_num %in% ids), ];
    df = df[order(df$patient_num, df$concept_date), ]
    df = df[!duplicated(paste(df$patient_num, df$concept_cd)),]
    df_list[[length(df_list)+1]] <- df
  }
  res = do.call("rbind", df_list)
  res = res[order(res$patient_num, res$concept_date), ]
  res = res[!duplicated(paste(res$patient_num, res$concept_cd)),]

  return(res);
}

#
# Go over training & testing sets and truncate lab-data after index event.
#
if (length(grep('subject_num', names(df)))>0){
  print("replacing 'subject_num' with 'patient_num' in labs.RDs")
  colnames(df)[grep('subject_num', colnames(df))] <- 'patient_num'
}

for (path_name in c(training_path, testing_path))
{
  setwd(path_name)
  demographics = readRDS('demo_trunc.RDs')
  suicidal_events = readRDS('suicidal_events.RDs');

  labs = getSubsetOfLabs(lab.files, demographics$patient_num)
  labs$case_any = (labs$patient_num %in% demographics$patient_num[demographics$case_any==TRUE])
  labs_idxs = filterDataOutsideDates(labs[,c('patient_num','concept_date','case_any')], suicidal_events)
  labs = labs[-labs_idxs, ]
  saveRDS(labs, 'labs_trunc.RDs');
}


# #################################################################
#
#       PART 4 – BUILDING THE PREDICTIVE MODEL
#
# #################################################################
getNaiveBayesTable <- function(df)
{
  # code-based scoring
  nbc_code = as.data.frame(t(as.data.frame.matrix(table(df$case_any,df$concept_cd))))
  colnames(nbc_code) = c('CONTROLS_CODE', 'CASES_CODE')
  nbc_code[is.na(nbc_code) | nbc_code==0] = 0.001 # to avoid devide by zero, we use 1/1000 as a place-holder. In the manuscript we only report conceepts with more than 100 subjects so this isn't relevantt there. 
  total_cases_codes = sum(df$case_any); total_control_codes = sum(!df$case_any);
  nbc_code$OR_CODE = (nbc_code$CASES_CODE / (total_cases_codes - nbc_code$CASES_CODE)) / (nbc_code$CONTROLS_CODE / (total_control_codes - nbc_code$CONTROLS_CODE))
  nbc_code$NBC_CODE = log(nbc_code$OR_CODE)

  # subject-based scoring (this is what we use in the manuscript)
  df = df[!duplicated(paste(df$patient_num,df$concept_cd)), ]
  nbc_subj = as.data.frame(t(as.data.frame.matrix(table(df$case_any,df$concept_cd))))
  colnames(nbc_subj) = c('CONTROLS_SUBJ', 'CASES_SUBJ')
  nbc_subj[is.na(nbc_subj) | nbc_subj==0] = 0.001
  cases_ids = unique(df$patient_num[df$case_any]); controls_ids = unique(df$patient_num[!df$case_any])
  total_cases = length(cases_ids); total_controls = length(controls_ids);
  nbc_subj$OR_SUBJ = (nbc_subj$CASES_SUBJ / (total_cases - nbc_subj$CASES_SUBJ)) / (nbc_subj$CONTROLS_SUBJ / (total_controls - nbc_subj$CONTROLS_SUBJ))
  nbc_subj$NBC_SUBJ = log(nbc_subj$OR_SUBJ)

  nbc = merge(nbc_code, nbc_subj,by=0,all=T)
  rownames(nbc) = nbc$Row.names

  return(nbc)
}


# ----------------------------------------------------------------
# PART 4.1 – CALCULATING INDIVIDUAL NBC SCORES FOR EACH CONCEPT
# ----------------------------------------------------------------
setwd(training_path)
dxproc = readRDS('dx_trunc.RDs')
meds = readRDS('meds_trunc.RDs')
labs = readRDS('labs_trunc.RDs')
demographics = readRDS('demo_trunc.RDs')

total_cases = sum(demographics$case_any==TRUE); total_controls = sum(demographics$case_any==FALSE)

labs_nbc = getNaiveBayesTable(labs); saveRDS(labs_nbc, 'labs_nbc.RDs')
dx_nbc = getNaiveBayesTable(dxproc); saveRDS(dx_nbc, 'dx_nbc.RDs')
meds_nbc = getNaiveBayesTable(meds); saveRDS(meds_nbc, 'meds_nbc.RDs')


# ##################################################################
#                         TABLE 2
# ##################################################################
getTopScores <- function(nbc.scores, n=20, min_count=50)
{
  nbc.scores$variable = sapply(rownames(nbc.scores), function(x) substr(x,1,15));
  nbc.scores = nbc.scores[order(nbc.scores$OR_SUBJ, decreasing=T),]
  nbc.scores$total_count = nbc.scores$CASES_SUBJ + nbc.scores$CONTROLS_SUBJ;
  nbc.scores = nbc.scores[nbc.scores$total_count > min_count & nbc.scores$variable != 'isCase', ]
  return(nbc.scores[1:n,c('variable','CASES_SUBJ','CONTROLS_SUBJ','OR_SUBJ')])
}


nbc.res = rbind(getTopScores(labs_nbc), getTopScores(dx_nbc), getTopScores(meds_nbc))
saveRDS(nbc.res,  paste(results_dir, 'Table2.RDs',sep='/'))

# ----------------------------------------------------------------
# PART 4.2 – CALCULATE CUMULATE RISK SCORE FOR EACH SUBJECT
# ----------------------------------------------------------------
# 4.2.1) read validation datasets
setwd(testing_path)
dxproc = readRDS('dx_trunc.RDs')
meds = readRDS('meds_trunc.RDs')
labs = readRDS('labs_trunc.RDs')
demographics = readRDS('demo_trunc.RDs')

# 4.2.2) add NBC scores
dxproc$NBC_SUBJ = dx_nbc[as.character(dxproc$concept_cd),'NBC_SUBJ']
meds$NBC_SUBJ = meds_nbc[as.character(meds$concept_cd),'NBC_SUBJ']
labs$NBC_SUBJ = labs_nbc[as.character(labs$concept_cd),'NBC_SUBJ']

# 4.2.3) merge meds with dxproc and sort by subject and by date
cols = c('patient_num', 'concept_date', 'concept_cd', 'NBC_SUBJ')
all_codes = rbind(dxproc[,cols], meds[,cols], labs[,cols])
all_codes = all_codes[order(all_codes$patient_num, all_codes$concept_date), ]

# 4.2.4) remove duplicate records of the same code - count only once
all_codes = all_codes[!duplicated(paste(all_codes$patient_num, all_codes$concept_cd)), ]

# 4.2.5) calculate the score over time for each subject and the maximal value achieved
cum_sum = tapply(all_codes$NBC_SUBJ, all_codes$patient_num, cumsum)
max_score_per_subject = sapply(cum_sum, max)

# ----------------------------------------------------------------
# PART 4.3 – EVALUATE MODEL’S OVERALL PERFORMANCE
# ----------------------------------------------------------------
require(pROC)
res = data.frame(patient_num=names(max_score_per_subject), max_score=max_score_per_subject)
res$case_any = res$patient_num %in% demographics$patient_num[demographics$case_any==TRUE]

roc.res = roc(res$case_any, res$max_score)
print(paste('AUC=',roc.res$auc))

# ##################################################################
#                         TABLE 3
# ##################################################################
getSummaryStatsTbl <- function(roc.res)
{
  stats_cols = c("accuracy", "sen", "spec", "ppv", "npv",'tp','tn','fp','fn')

  summary.stats = data.frame(rbind(
    coords(roc.res, x=0.99, input="specificity", ret=stats_cols),
    coords(roc.res, x=0.95, input="specificity", ret=stats_cols),
    coords(roc.res, x=0.90, input="specificity", ret=stats_cols),
    coords(roc.res, x=0.80, input="specificity", ret=stats_cols),
    coords(roc.res, x=0.90, input="sensitivity", ret=stats_cols)))
  summary.stats$AUC = roc.res$auc
  return(round(summary.stats, digits = 2))
}

saveRDS(getSummaryStatsTbl(roc.res),  paste(results_dir, 'Table3.RDs',sep='/'))
# plotROC(roc.res, "ROC")

# ----------------------------------------------------------------
# PART 4.4 – CALCULATE MODEL’S SCORE BY MONTH
# ----------------------------------------------------------------
setwd(working_dir)
require(data.table)
demographics = readRDS('demographics.RDs')
df = all_codes[,c('patient_num','concept_date')];
df = merge(df, df[!duplicated(df$patient_num),c('patient_num','concept_date')], by='patient_num',all=T)
df$rel_month = round(as.numeric(difftime(df$concept_date.x, df$concept_date.y, units = 'days'))/30.41667, digits = 0)
df$score = round(all_codes$NBC_SUBJ, digits = 1)
df$cum_sum = unlist(cum_sum)
df$case_any = (df$patient_num %in% demographics$patient_num[demographics$case_any])
df = df[rev(!duplicated(rev(paste(df$patient_num, df$rel_month)))), ]
saveRDS(df[,c('patient_num', 'rel_month', 'cum_sum', 'case_any')],paste(results_dir, 'Score by Month.RDs',sep='/'))

# ##################################################################
#                         TABLE 4
# cumulative risk scores over time for cases vs. controls (automatically
# generated while building the model).
# ##################################################################
max_month = 15*12 # 15 years
cases_scores = df[df$case_any & df$rel_month < max_month, ]
controls_scores = df[!df$case_any & df$rel_month < max_month, ]

median_cases = tapply(cases_scores$cum_sum, cases_scores$rel_month, median, na.rm=T)
sd_cases = tapply(cases_scores$cum_sum, cases_scores$rel_month, sd, na.rm=T)
median_controls = tapply(controls_scores$cum_sum, controls_scores$rel_month, median, na.rm=T)
sd_controls = tapply(controls_scores$cum_sum, controls_scores$rel_month, sd, na.rm=T)
table4 = data.frame(merge(cbind(median_cases, sd_cases), cbind(median_controls, sd_controls), by=0, all=T))
colnames(table4)[1] = 'month'
saveRDS(table4[order(as.numeric(table4$month)), ],paste(results_dir, 'Table4.RDs',sep='/'))
# ##################################################################
#                         TABLE 5
# cases cumulative risk score in the months preceding suicidal event
# (automatically generated while building the model).
# ##################################################################

# Saved before as part of the monthly cumulative score






