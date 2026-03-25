# README
This repository contains programming code used in **Simard Duplain and Zhang (2026)** "**[Association Between Prenatal Opioid Exposure and Health, Education, and Foster Care Between Ages 0 and 18](https://academic.oup.com/pnasnexus/article/5/3/pgag024/8503063)**".

Contact: jonathan.zhang@duke.edu 

Below are the instructions to replicate the findings in the paper.

### Data
The data analyzed in this paper comes from British Columbia’s Data Innovation Program. Information about data access can be found on Population BC’s website at https://www.popdata.bc.ca/projects/data-innovation-program. For those who are interested, we will do our best to assist them in gaining access to the data. Data is confidential and not available in this repository.

### Files
#### Data construction scripts
`Create NAS Exposure Measures` produces four csv files (`perinatal_dx_newborn_mother.csv`, `substance_use_questionnaire_delivery.csv`, `newborn_dx_2months.csv`, `mother_drug_abuse_pregnancy.csv`) that are used to construct measures of in utero exposure using medical records, prescription records, and perinatal files. 

`Get All Mother Opioid Rx.R` pulls all opioid prescriptions used by mothers and creates `all_mothers_opioid_rx.gz`

`Create All Births.R` creates cohort of all births in BC and relevant covariates of newborns and their parents, output is `allbirths.csv`

`Create MCFD variables.R`, `0102_data_gen_health_care_expenditures_20250407.DO`, `0103_data_gen_psychotropic_drugs_20250421.DO`, `0105_data_gen_ia_20250410.DO`, `0106_data_gen_parents_together_20250407.DO`, `0201_data_gen_all_outcomes_20250414.DO` create auxillary variables relevant for child outcomes that feed into creating the panel outcomes.


XXX pulls mothers covariates which are used as controls for propensity score matching.

`Create Panel Outcomes.R` creates panel outcomes from ages 0-18 for all births in BC.

#### Analysis scripts
`Setup.R` loads relevant packages and sets the working directory.

XXX performs the main analysis of the paper and produces regression output.

XXX converts the regression output into exhibits.

XXX performs propensity score matching.

### Replication Instructions
1. Run `Setup.R`
2. Run `Get All Mother Opioid Rx.R`
3. Run `Create NAS Exposure Measures`
4. Run `Create All Births.R`
5. Run `Create MCFD variables.R`, `0102_data_gen_health_care_expenditures_20250407.DO`, `0103_data_gen_psychotropic_drugs_20250421.DO`, `0105_data_gen_ia_20250410.DO`, `0106_data_gen_parents_together_20250407.DO`, `0201_data_gen_all_outcomes_20250414.DO` which creates auxillary variables
6. 
