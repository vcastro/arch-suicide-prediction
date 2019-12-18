#
# Install required packages
#
require('data.table')


# ###################################################################################
# preperations before running this script:
# ----------------------------------------
# 1. Make sure that 'working_dir' is pointing to where all the datasets reside in
# 2. Create a new sub-folder in the working directory called 'labs' and point 'labs_dir' to that new folder
#
# ###################################################################################

DELIMITER = "\t"
TIME_FORMAT = switch('C', A='%m/%d/%Y', B='%m/%d/%Y %H:%M:%S.000000 %p', C='%Y%m%d')

require(data.table)
working_dir = '/Users/XXX/WORK_DIR/'
labs_dir = paste(working_dir, 'labs',sep='/') # Create this folder if it's missing
setwd(working_dir)

concept_dict_filename = 'FUSS_Dictionary.txt'
demographics_filename = 'FUSS_Demographics.txt'
dxproc_filename = 'FUSS_Dx_Procs.txt'
meds_filename = 'FUSS_Medications.txt'
labs_filename = 'FUSS_Labs.txt'

concept_dict_filename_RDs = 'concept_dict.RDs'
demographics_filename_RDs = 'demographics.RDs'
dxproc_filename_RDs = 'dxproc.RDs'
meds_filename_RDs = 'meds.RDs'
labs_filename_RDs = 'labs.RDs'

# psych_dict_path = 'fuss_deid_psychconcept_dict.txt'

getRDSfilename <- function(txt_filename){ return(sub('.txt','.RDs',txt_filename,fixed = T))}

# ---- Concepts Dict ----------------------------------------------------
concept_dict = fread(concept_dict_filename, header = T, sep = DELIMITER, stringsAsFactors=F, data.table=F) # na.strings=na_formats
colnames(concept_dict)[1] = 'concept_cd'
for (col in c('code_type', 'code_class', 'code_description')) concept_dict[,col] = factor(concept_dict[,col])
saveRDS(concept_dict, concept_dict_filename_RDs)

# ---- Demographics ----------------------------------------------------
demographics = fread(demographics_filename, header = T, sep = DELIMITER, stringsAsFactors=F, data.table=F) # na.strings=na_formats
colnames(demographics)[1] = 'patient_num'
for (col in c('gender','race','deceased')) demographics[,col] = factor(demographics[,col])
demographics$death_date = strptime(demographics$death_date, TIME_FORMAT,tz='EST')
demographics$case_any = (demographics$case_narrow | demographics$case_broad)
saveRDS(demographics, demographics_filename_RDs)

# ---- DXPROC ----------------------------------------------------
dxproc = fread(dxproc_filename, header = T, sep = DELIMITER, stringsAsFactors=F, data.table=F) # na.strings=na_formats
colnames(dxproc)[1] = 'patient_num'
print(paste('concept_date=', dxproc$concept_date[1],', Format used=',TIME_FORMAT,'. Result: ',strptime(dxproc$concept_date[1],TIME_FORMAT), sep = ''))
dxproc$concept_date = strptime(dxproc$concept_date, TIME_FORMAT, tz='EST')
saveRDS(dxproc, dxproc_filename_RDs)

# ---- Meds ----------------------------------------------------
meds = fread(meds_filename, header = T, sep = DELIMITER, stringsAsFactors = F, data.table = F)
colnames(meds)[1] = 'patient_num'
meds$concept_date = strptime(meds$concept_date, TIME_FORMAT,tz='EST')
saveRDS(meds, meds_filename_RDs)
rm(meds);

# Labs (U = uninterpretable, H=high, L=low, A=abnormal, N=negative, P=positive, R=refused)
# Since the Labs file is huge, I broke it up to 64 different files of 10M rows each.
# All these files reside in the dir "WORKING_DIR/labs/"
#


#
# TODO: Make sure that this is the correct order of columns at each site
#
setwd(working_dir)
dir.create('labs')
# -- read raw labs text file ---
# Headers of the lab file: patient_num concept_cd concept_date LabFlag suiciderelated_fact
labs = fread(labs_filename, header = T, sep = DELIMITER, stringsAsFactors=F, data.table=F) # na.strings=na_formats
colnames(labs) = c('patient_num','concept_cd', 'concept_date', 'LabFlag', 'suiciderelated_fact')
print(labs[1:2,])
labs$LabFlag[is.na(labs$LabFlag) | labs$LabFlag=='' | labs$labflag=='null'] = 'NoFlag'
labs$LabFlag = factor(labs$LabFlag)
labs$UniqueIdentifier = paste(labs$concept_cd, labs$LabFlag,sep='_')

# -- split text file into subsets of 10M rows each and save as .RDs files in the 'labs_dir'

n = nrow(labs)
cut_points = seq(0,n, min(10000000, n))
print(paste('cut points:',paste(cut_points, collapse = ', ')))
previous_cut_point = 0;
idx=1;
for (cut_point in cut_points[-1])
{
  print(paste('reading labs file, part ',idx,'/',length(cut_points)-1,sep=''))
  labs_cut = data.frame(labs[(previous_cut_point+1):cut_point,])
  labs_cut$concept_date = strptime(labs_cut$concept_date, TIME_FORMAT,tz='EST')
  saveRDS(labs_cut, paste("labs/labs",idx,'.RDs',sep=''))
  previous_cut_point = cut_point;
  idx = idx+1
}
rm(labs)

