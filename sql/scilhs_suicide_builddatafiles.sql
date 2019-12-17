/*
  Fuss - SCILHS - Build Suicide Prediction Data Files 

  Inclusion criteria:  All datamart patients 3+ ICD-9 codes >30 days apart.  The most recent ICD-9 code
					   must occur between the ages of 10 and 90.

  3 tables are built:
	- Suicide_DemFile: Demographics file
	- Suicide_FactFile: All facts (DX, PROC, LAB, MEDS
	- Suicide_ConceptDict: A dictionary of concepts in the fact file

  Dependencies
    - SCILHS metadata tables: pcornet_diag, pcornet_proc, pcornet_med
	- i2b2 tables: patient_dimension, visit_dimension, observation_fact
	- RXNORM_INGREDIENT_LIST (populated by build_rxnorm_ing_list stored procedure)

*/


-- Create codebook with list of all ICD9 codes AND suicide narrow and broad definitions
-- Narrow definition: ICD9 E95.* or 965.* or 967.* or 969.*

IF OBJECT_ID('tempdb..#suicide_codebook') IS NOT NULL DROP TABLE #suicide_codebook

select c_basecode, cast('All_ICD9' as varchar(100)) category
into #suicide_codebook
from dbo.pcornet_diag
where c_fullname like '\PCORI\DIAGNOSIS\09\%' and PCORI_BASECODE like 'ICD9:%' and PCORI_BASECODE <> '' and PCORI_BASECODE not like '%-%'
order by C_BASECODE

insert into #suicide_codebook
select c_basecode, 'Suicide_Narrow' category
from dbo.pcornet_diag
where c_fullname like '\PCORI\DIAGNOSIS\09\%' and PCORI_BASECODE like 'ICD9:%' and PCORI_BASECODE <> '' and PCORI_BASECODE not like '%-%'
	and (PCORI_BASECODE like 'ICD9:E95%' or PCORI_BASECODE like 'ICD9:965%' or PCORI_BASECODE like 'ICD9:967%' or PCORI_BASECODE like 'ICD9:969%')

insert into #suicide_codebook
select c_basecode, 'Suicide_Broad' category
from dbo.pcornet_diag
where c_fullname like '\PCORI\DIAGNOSIS\09\%' and PCORI_BASECODE like 'ICD9:%' and PCORI_BASECODE <> '' and PCORI_BASECODE not like '%-%'
		 and PCORI_basecode like 'ICD9:881%'


-- build list of RXNORM ingredients
if object_id('RXNORM_INGREDIENT_LIST') is not null drop table RXNORM_INGREDIENT_LIST
exec dbo.build_rxnorm_ing_list


-- tall file of all ICD9 facts for entire cohort
IF OBJECT_ID('tempdb..#all_icd9_facts') IS NOT NULL DROP TABLE #all_icd9_facts
select distinct f.patient_num, f.concept_cd, f.start_date, datediff(year, BIRTH_DATE, f.start_date) dx_age
into #all_icd9_facts
from	dbo.observation_fact f,
		dbo.patient_dimension p, 
		#suicide_codebook c
where	f.concept_cd = c.C_BASECODE and 
		c.category = 'All_ICD9' and
		p.patient_num = f.patient_num 
		

-- apply minimum ICD9 count and date span criteria and age filter
IF OBJECT_ID('tempdb..#Suicide_Population') IS NOT NULL DROP TABLE #Suicide_Population

select patient_num
into #Suicide_Population
from #all_icd9_facts
where dx_age between 10 and 89
group by patient_num
having	count(*) >= 3 and
		datediff(day, min(start_date), max(start_date)) >= 30 and
		max(dx_age) < 90 and
		min(dx_age) >= 10



-- create preliminary dictionary of all valid concepts to include in the fact file

IF OBJECT_ID('tempdb..#Suicide_Valid_Facts') IS NOT NULL DROP TABLE #Suicide_Valid_Facts

select distinct PCORI_BASECODE code, c_basecode, 'ICD-9-CM' code_type, 'Diagnosis' code_class, c_name
into #Suicide_Valid_Facts
from dbo.pcornet_diag
where C_FULLNAME like '\PCORI\DIAGNOSIS\09\%'
	and C_BASECODE is not null and PCORI_BASECODE is not null and PCORI_BASECODE <> '' and PCORI_BASECODE not like '%-%'
	and C_SYNONYM_CD = 'N'
union
select distinct PCORI_BASECODE, c_basecode, 'ICD-9-Proc', 'Procedure' , c_name
from dbo.pcornet_proc
where c_fullname like '\PCORI\PROCEDURE\09\%'
	and C_BASECODE is not null and PCORI_BASECODE is not null and PCORI_BASECODE <> '' and PCORI_BASECODE not like '%-%'
	and C_SYNONYM_CD = 'N'
union
select distinct c_basecode, c_basecode, 'CPT-4', 'Procedure' , c_name
from dbo.pcornet_proc
where c_fullname like '\PCORI\PROCEDURE\CH\%' 
	and C_BASECODE is not null and PCORI_BASECODE is not null and PCORI_BASECODE <> '' and PCORI_BASECODE not like '%-%'
	and C_SYNONYM_CD = 'N'
union
select distinct PCORI_BASECODE code, c_basecode, 'LOINC' code_type, 'Lab test' code_class, c_name
from dbo.pcornet_lab
where C_FULLNAME like '\PCORI\LAB_RESULT_CM\%' and C_BASECODE is not null and PCORI_BASECODE is not null and PCORI_BASECODE <> ''
and C_SYNONYM_CD = 'N'
union
select distinct b.rxn_ingredient, b2.c_basecode, 'RXNORM', 'Medication', a.c_name
from dbo.pcornet_med a, dbo.[RXNORM_INGREDIENT_LIST] b, dbo.pcornet_med b2
where a.C_BASECODE = b.rxn_ingredient and b2.C_FULLNAME like a.C_FULLNAME + '%'
and b2.C_SYNONYM_CD = 'N'

create clustered index ix_vf_cbasecode on #Suicide_Valid_Facts(c_basecode)
create nonclustered index ix_vf_code on #Suicide_Valid_Facts(code)


-- create the fact file
if object_id('tempdb..Suicide_FactFile') is not null drop table Suicide_FactFile
-- fact file
select  p.patient_num, c.code concept_cd, f.start_date concept_date, 
		case when code_class = 'Lab test' and f.VALUEFLAG_CD <> '@' then f.VALUEFLAG_CD else null end LabFlag,
		cast(null as bit) SuicideRelatedFact
into Suicide_FactFile
from #Suicide_Population p,
	 dbo.observation_fact f,
	 #Suicide_Valid_Facts c
where p.patient_num = f.patient_num and f.concept_cd = c.c_basecode 
group by p.PATIENT_NUM, c.code, c.code_class, f.START_DATE, f.VALUEFLAG_CD

create clustered index ix_factfile on Suicide_FactFile (concept_cd)
create nonclustered index ix_factfile_pt on Suicide_FactFile (patient_num)

-- a suicide-related fact is any fact occuring in the same encounter as the suicide code

IF OBJECT_ID('tempdb..#Suicide_RelatedEncounters') IS NOT NULL DROP TABLE #Suicide_RelatedEncounters
select e.patient_num, e.start_date, isnull(e.end_date, e.start_date) end_date
into #Suicide_RelatedEncounters
from dbo.observation_fact f, 
	 dbo.visit_dimension e,
	 #suicide_codebook cb,
	 #Suicide_Population p
where p.patient_num = f.patient_num and f.PATIENT_NUM = e.PATIENT_NUM and cb.C_BASECODE = f.CONCEPT_CD and cb.category in ('Suicide_Narrow', 'Suicide_Broad')
group by e.patient_num, e.start_date, e.end_date


update Suicide_FactFile
set SuicideRelatedFact = 1
from #Suicide_RelatedEncounters e
where e.PATIENT_NUM = Suicide_FactFile.patient_num 
	and Suicide_FactFile.concept_date between e.start_date and e.end_date

update Suicide_FactFile
set SuicideRelatedFact = 0
where SuicideRelatedFact is null
	

-- narrow case definition
IF OBJECT_ID('tempdb..#suicide_narrow_patients') IS NOT NULL DROP TABLE #suicide_narrow_patients

select distinct p.patient_num
into #suicide_narrow_patients
from #Suicide_Population p,
	 #Suicide_Codebook cb,
	 #all_icd9_facts f
where p.patient_num = f.patient_num and f.concept_cd = cb.c_basecode
		and cb.category = 'Suicide_Narrow'


-- broad case definition
IF OBJECT_ID('tempdb..#suicide_broad_patients') IS NOT NULL DROP TABLE #suicide_broad_patients

select distinct p.patient_num
into #suicide_broad_patients
from #Suicide_Population p,
	 #Suicide_Codebook cb,
	 #all_icd9_facts f
where p.patient_num = f.patient_num and f.concept_cd = cb.c_basecode
		and cb.category in ('Suicide_Narrow', 'Suicide_Broad')


-- demographics file
if object_id('Suicide_DemFile') is not null drop table Suicide_DemFile

select  s.patient_num, 
		sex_cd gender, 
		p.age_in_years_num age, 
		p.RACE_CD race,
		vital_status_cd deceased,
		p.Death_Date death_date,
		case when c.patient_num is null then 0 else 1 end case_narrow,
		case when d.patient_num is null then 0 else 1 end case_broad,
		case when c.patient_num is null and d.patient_num is null then 0 else 1 end case_any

into Suicide_DemFile
from #Suicide_Population s, dbo.patient_dimension p
left join #suicide_narrow_patients c on c.patient_num = p.patient_num
left join #suicide_broad_patients d on d.patient_num = p.patient_num
where s.patient_num = p.patient_num



-- concept dictionary file
--concept_key: concept_cd, code_type, code_description

if object_id('tempdb..#Suicide_ConceptDict') is not null drop table #Suicide_ConceptDict

select c.code concept_cd, c.code_type, c.code_class, c.c_name code_description
into Suicide_ConceptDict
from #Suicide_Valid_Facts c, Suicide_FactFile f
where c.code = f.concept_cd
group by c.code, c.code_type, c.code_class, c.c_name


-- output diagnosis and procedure facts
select  distinct f.patient_num,
        f.concept_cd,
        CONVERT(char(8), concept_date,112) concept_date
		,isnull(LabFlag, '') LabFlag
        ,cast(isnull(SuicideRelatedFact, 0) as TINYINT) suiciderelated_fact   
from Suicide_FactFile f, Suicide_ConceptDict d
where f.concept_cd = d.concept_cd and code_class in ('Diagnosis', 'Procedure')

-- output medication facts
select  distinct f.patient_num,
        f.concept_cd,
        CONVERT(char(8), concept_date,112) concept_date
		,isnull(LabFlag, '') LabFlag
        ,cast(isnull(SuicideRelatedFact, 0) as TINYINT) suiciderelated_fact   
from Suicide_FactFile f, Suicide_ConceptDict d
where f.concept_cd = d.concept_cd and code_class in ('Medication')


-- output lab test facts
select  distinct f.patient_num,
        f.concept_cd,
        CONVERT(char(8), concept_date,112) concept_date
		,isnull(LabFlag, '') LabFlag
        ,cast(isnull(SuicideRelatedFact, 0) as TINYINT) suiciderelated_fact   
from Suicide_FactFile f, Suicide_ConceptDict d
where f.concept_cd = d.concept_cd and code_class in ('Lab test')



-- output demographics
select patient_num, 
	   gender, 
	   age, 
	   race,
	   deceased,
	   CONVERT(char(8), death_date,112) death_date,
	   case_narrow,
	   case_broad
from Suicide_DemFile 


-- output concept dictionary
select concept_cd, code_type, code_class, code_description
from Suicide_ConceptDict
