# README
This repository contains programming code used in **Simard Duplain and Zhang (2026)** "**[Association Between Prenatal Opioid Exposure and Health, Education, and Foster Care Between Ages 0 and 18](https://academic.oup.com/pnasnexus/article/5/3/pgag024/8503063)**".

Contact: jonathan.zhang@duke.edu 

Below are the instructions to replicate the findings in the paper.

### Data
The data analyzed in this paper comes from British Columbia’s Data Innovation Program. Information about data access can be found on Population BC’s website at https://www.popdata.bc.ca/projects/data-innovation-program. For those who are interested, we will do our best to assist them in gaining access to the data. Data is confidential and not available in this repository.

### Files
#### Data construction scripts
XXX produces four csv files that are used to construct measures of in utero exposure using medical records, prescription records, and perinatal files. 

XXX pulls all opioid prescriptions used by mothers.

XXX pulls all opioid prescriptions dispensed in BC.

XXX pulls mothers covariates which are used as controls for propensity score matching.

`Create All Births.R` creates cohort of all births in BC and relevant covariates of newborns and their parents.

`Create Panel.R` creates panel outcomes from ages 0-18 for all births in BC.

#### Analysis scripts
`Setup.R` loads relevant packages and sets the working directory.

XXX performs the main analysis of the paper and produces regression output.

XXX converts the regression output into exhibits.

XXX performs propensity score matching.

### Replication Instructions
1. Run `Setup.R` 
