### Create panel outcomes for all births
### A row for each Child-Age 
### Will not be a balanced panel

library(tidyverse)
library(data.table)
library(haven)
library(readr)
library(lfe)
library(zoo)
library(stargazer)

memory.limit(size=100000000)

perinatal <- fread("allbirths.csv")

###########################################################################
###############               HEALTH OUTCOMES               ###############
###########################################################################

# 1. Mortality
# 2. Any Hospitalization
# 3. Any ED visit
# 4. Total medical expenditures
# 5. Costs of prescription drugs
# 6. Any MH diagnosis
# 7. Indicator for MH ED/Hosp

birth_panel <- data.table(studyid2=rep(unique(perinatal$studyid2), each=18), RelativeYear=rep(1:18, length(unique(perinatal$studyid2))))
birth_panel <- merge(x=birth_panel, y=perinatal[,c("studyid1", "studyid2", "Month", "Year")], by.x="studyid2", by.y="studyid2", all.x=T)
setnames(birth_panel, old=c("Month", "Year"), new=c("BirthMonth", "BirthYear"))

birth_panel$Year <- birth_panel$BirthYear+birth_panel$RelativeYear
birth_panel$BirthDate <- as.Date(paste(birth_panel$BirthYear, "-", birth_panel$BirthMonth, "-01", sep=""), "%Y-%m-%d")

birth_panel <- birth_panel[Year<=2022]


### 1. Mortality
# Stillbirths are NOT counted as deaths in death records
deaths <- read_fwf("R:/DATA/core-snapshot/20230217/vital_events/deaths/deaths1986-2022.C.dat.gz", 
                   fwf_cols(DEATHDTCCYY=c(27,30),
                            DEATHDTCCmm=c(31,32),
                            STUDYID=c(600,609)))
deaths$DeathDate <- as.Date(paste(deaths$DEATHDTCCYY, "-", deaths$DEATHDTCCmm, "-01", sep=""), "%Y-%m-%d")
deaths <- data.table(deaths)

birth_panel <- merge(x=birth_panel, y=deaths[,c("STUDYID", "DeathDate")], by.x="studyid2", by.y="STUDYID", all.x=T)
birth_panel$Dead <- (birth_panel$DeathDate<=(birth_panel$BirthDate+365*as.numeric(as.character(birth_panel$RelativeYear)))); birth_panel$Dead[which(is.na(birth_panel$Dead))] <- 0; 

### 2. Any hospitalization
dad <- NULL
tmp <- data.table(read_fwf("R:/DATA/core-snapshot/20230217/hospital/dat/hospital2000-01.A.dat.gz", fwf_cols(admission_date=c(5,14), studyid=c(726,735))))
tmp$admission_date <- as.Date(tmp$admission_date, "%Y-%m-%d")
tmp <- tmp[studyid %in% perinatal$studyid2]
tmp <- unique(tmp)
unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
dad <- rbind(dad, tmp)
rm(tmp)
for(year in c("2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07")){
  tmp <- data.table(read_fwf(paste("R:/DATA/core-snapshot/20230217/hospital/dat/hospital", year, ".G.dat.gz", sep=""), fwf_cols(admission_date=c(1,10), studyid=c(2044,2053))))
  tmp$admission_date <- as.Date(tmp$admission_date, "%Y-%m-%d")
  tmp <- tmp[studyid %in% perinatal$studyid2]
  tmp <- unique(tmp)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  dad <- rbind(dad, tmp)
  rm(tmp)
}
for(year in c("2007-08", "2008-09")){
  tmp <- data.table(read_fwf(paste("R:/DATA/core-snapshot/20230217/hospital/dat/hospital", year, ".H.dat.gz", sep=""), fwf_cols(admission_date=c(1,10), studyid=c(2786,2785))))
  tmp$admission_date <- as.Date(tmp$admission_date, "%Y-%m-%d")
  tmp <- tmp[studyid %in% perinatal$studyid2]
  tmp <- unique(tmp)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  dad <- rbind(dad, tmp)
  rm(tmp)
}
for(year in c("2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22")){
  tmp <- data.table(read_fwf(paste("R:/DATA/core-snapshot/20230217/hospital/dat/hospital", year, ".O.dat.gz", sep=""), fwf_cols(admission_date=c(1,10), studyid=c(2786,2795))))
  tmp$admission_date <- as.Date(tmp$admission_date, "%Y-%m-%d")
  tmp <- tmp[studyid %in% perinatal$studyid2]
  tmp <- unique(tmp)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  dad <- rbind(dad, tmp)
  rm(tmp)
}
dad <- unique(dad)
dad <- merge(x=dad, y=perinatal[,c("studyid2", "Year", "Month")], by.x="studyid", by.y="studyid2", allow.cartesian=T)
dad$BirthDate <- as.Date(paste(dad$Year, "-", dad$Month, "-01", sep=""), "%Y-%m-%d")

# Supposedly the DAD does not include newborns, but this is clearly not true. Newborns all have an admission in their month...
dad <- dad[!format(admission_date, "%Y-%m")==format(BirthDate, "%Y-%m")]
dad$RelativeYear <- ceiling(as.numeric(dad$admission_date-dad$BirthDate)/365)
dad <- dad[, .(Hosp_Count=length(unique(admission_date))), by=c("studyid", "RelativeYear")]
dad$Hosp_Any <- dad$Hosp_Count>0

birth_panel <- merge(x=birth_panel, y=dad, by.x=c("studyid2", "RelativeYear"), by.y=c("studyid", "RelativeYear"), all.x=T)

# Clean hospital variables
birth_panel$Hosp_Any <- as.numeric(birth_panel$Hosp_Any)
setnafill(birth_panel, cols=c("Hosp_Any", "Hosp_Count"), fill=0)
birth_panel$Hosp_Count[which(birth_panel$Hosp_Count>6)] <- 6
birth_panel[Dead==1, c("Hosp_Any", "Hosp_Count")] <- NA

### 3. Any ED Visits
NACRS <-  read_fwf("R:/working/unzipped_raw_data/nacrs2011-2021.A.dat", 
                   fwf_cols(NACRS.EDDIAG1=c(251,256),
                            NACRS.EDDIAG2=c(257,262),
                            NACRS.EDDIAG3=c(263,268),
                            NACRS.REGDATETIME=c(369,398),
                            STUDYID=c(455,464)))
NACRS <- data.table(NACRS)
NACRS$NACRS.REGDATETIME <- as.Date(substring(NACRS$NACRS.REGDATETIME, 1, 10), "%Y-%m-%d")
NACRS <- merge(x=NACRS, y=perinatal[,c("studyid2", "Year", "Month")], by.x="STUDYID", by.y="studyid2", allow.cartesian=T)
NACRS$BirthDate <- as.Date(paste(NACRS$Year, "-", NACRS$Month, "-01", sep=""), "%Y-%m-%d")
NACRS$RelativeYear <- ceiling(as.numeric(NACRS$NACRS.REGDATETIME-NACRS$BirthDate)/365)
NACRS <- NACRS[, .(ED_Any=1, ED_Count=length(unique(NACRS.REGDATETIME))), by=c("STUDYID", "RelativeYear")]

birth_panel <- merge(x=birth_panel, y=NACRS, by.x=c("studyid2", "RelativeYear"), by.y=c("STUDYID", "RelativeYear"), all.x=T)

# Clean ED variables
setnafill(birth_panel, cols=c("ED_Any", "ED_Count"), fill=0)
birth_panel$ED_Count[which(birth_panel$ED_Count>6)] <- 6
birth_panel[Dead==1 | Year<2011, c("ED_Any", "ED_Count")] <- NA

### 4.3 Total medical expenditures (from Jeff, see Githubb Issue #2)
spending <- fread("medical_costs_children.csv")

birth_panel <- merge(x=birth_panel, y=spending[,c("studyid", "year", "expdamt", "hospital_costs")], by.x=c("studyid2", "Year"), by.y=c("studyid", "year"), all.x=T)

setnafill(birth_panel, cols=c("expdamt", "hospital_costs"), fill=0)
birth_panel[Year>2021 | Dead==1, c("expdamt", "hospital_costs")] <- NA
birth_panel$TotalSpending <- birth_panel$expdamt+birth_panel$hospital_costs

birth_panel$TotalSpending[which(birth_panel$Year==1998)] <- birth_panel$TotalSpending[which(birth_panel$Year==1998)]*132.4/93.4
birth_panel$TotalSpending[which(birth_panel$Year==1999)] <- birth_panel$TotalSpending[which(birth_panel$Year==1999)]*132.4/94.4
birth_panel$TotalSpending[which(birth_panel$Year==2000)] <- birth_panel$TotalSpending[which(birth_panel$Year==2000)]*132.4/96.1
birth_panel$TotalSpending[which(birth_panel$Year==2001)] <- birth_panel$TotalSpending[which(birth_panel$Year==2001)]*132.4/97.7
birth_panel$TotalSpending[which(birth_panel$Year==2002)] <- birth_panel$TotalSpending[which(birth_panel$Year==2002)]*132.4/100
birth_panel$TotalSpending[which(birth_panel$Year==2003)] <- birth_panel$TotalSpending[which(birth_panel$Year==2003)]*132.4/102.2
birth_panel$TotalSpending[which(birth_panel$Year==2004)] <- birth_panel$TotalSpending[which(birth_panel$Year==2004)]*132.4/104.2
birth_panel$TotalSpending[which(birth_panel$Year==2005)] <- birth_panel$TotalSpending[which(birth_panel$Year==2005)]*132.4/106.3
birth_panel$TotalSpending[which(birth_panel$Year==2006)] <- birth_panel$TotalSpending[which(birth_panel$Year==2006)]*132.4/108.1
birth_panel$TotalSpending[which(birth_panel$Year==2007)] <- birth_panel$TotalSpending[which(birth_panel$Year==2007)]*132.4/110
birth_panel$TotalSpending[which(birth_panel$Year==2008)] <- birth_panel$TotalSpending[which(birth_panel$Year==2008)]*132.4/112.3
birth_panel$TotalSpending[which(birth_panel$Year==2009)] <- birth_panel$TotalSpending[which(birth_panel$Year==2009)]*132.4/112.3
birth_panel$TotalSpending[which(birth_panel$Year==2010)] <- birth_panel$TotalSpending[which(birth_panel$Year==2010)]*132.4/113.8
birth_panel$TotalSpending[which(birth_panel$Year==2011)] <- birth_panel$TotalSpending[which(birth_panel$Year==2011)]*132.4/116.5
birth_panel$TotalSpending[which(birth_panel$Year==2012)] <- birth_panel$TotalSpending[which(birth_panel$Year==2012)]*132.4/117.8
birth_panel$TotalSpending[which(birth_panel$Year==2013)] <- birth_panel$TotalSpending[which(birth_panel$Year==2013)]*132.4/117.7
birth_panel$TotalSpending[which(birth_panel$Year==2014)] <- birth_panel$TotalSpending[which(birth_panel$Year==2014)]*132.4/118.9
birth_panel$TotalSpending[which(birth_panel$Year==2015)] <- birth_panel$TotalSpending[which(birth_panel$Year==2015)]*132.4/120.2
birth_panel$TotalSpending[which(birth_panel$Year==2016)] <- birth_panel$TotalSpending[which(birth_panel$Year==2016)]*132.4/122.4
birth_panel$TotalSpending[which(birth_panel$Year==2017)] <- birth_panel$TotalSpending[which(birth_panel$Year==2017)]*132.4/125.0
birth_panel$TotalSpending[which(birth_panel$Year==2018)] <- birth_panel$TotalSpending[which(birth_panel$Year==2018)]*132.4/128.4
birth_panel$TotalSpending[which(birth_panel$Year==2019)] <- birth_panel$TotalSpending[which(birth_panel$Year==2019)]*132.4/131.4

birth_panel$expamt[which(birth_panel$Year==1998)] <- birth_panel$expamt[which(birth_panel$Year==1998)]*132.4/93.4
birth_panel$expamt[which(birth_panel$Year==1999)] <- birth_panel$expamt[which(birth_panel$Year==1999)]*132.4/94.4
birth_panel$expamt[which(birth_panel$Year==2000)] <- birth_panel$expamt[which(birth_panel$Year==2000)]*132.4/96.1
birth_panel$expamt[which(birth_panel$Year==2001)] <- birth_panel$expamt[which(birth_panel$Year==2001)]*132.4/97.7
birth_panel$expamt[which(birth_panel$Year==2002)] <- birth_panel$expamt[which(birth_panel$Year==2002)]*132.4/100
birth_panel$expamt[which(birth_panel$Year==2003)] <- birth_panel$expamt[which(birth_panel$Year==2003)]*132.4/102.2
birth_panel$expamt[which(birth_panel$Year==2004)] <- birth_panel$expamt[which(birth_panel$Year==2004)]*132.4/104.2
birth_panel$expamt[which(birth_panel$Year==2005)] <- birth_panel$expamt[which(birth_panel$Year==2005)]*132.4/106.3
birth_panel$expamt[which(birth_panel$Year==2006)] <- birth_panel$expamt[which(birth_panel$Year==2006)]*132.4/108.1
birth_panel$expamt[which(birth_panel$Year==2007)] <- birth_panel$expamt[which(birth_panel$Year==2007)]*132.4/110
birth_panel$expamt[which(birth_panel$Year==2008)] <- birth_panel$expamt[which(birth_panel$Year==2008)]*132.4/112.3
birth_panel$expamt[which(birth_panel$Year==2009)] <- birth_panel$expamt[which(birth_panel$Year==2009)]*132.4/112.3
birth_panel$expamt[which(birth_panel$Year==2010)] <- birth_panel$expamt[which(birth_panel$Year==2010)]*132.4/113.8
birth_panel$expamt[which(birth_panel$Year==2011)] <- birth_panel$expamt[which(birth_panel$Year==2011)]*132.4/116.5
birth_panel$expamt[which(birth_panel$Year==2012)] <- birth_panel$expamt[which(birth_panel$Year==2012)]*132.4/117.8
birth_panel$expamt[which(birth_panel$Year==2013)] <- birth_panel$expamt[which(birth_panel$Year==2013)]*132.4/117.7
birth_panel$expamt[which(birth_panel$Year==2014)] <- birth_panel$expamt[which(birth_panel$Year==2014)]*132.4/118.9
birth_panel$expamt[which(birth_panel$Year==2015)] <- birth_panel$expamt[which(birth_panel$Year==2015)]*132.4/120.2
birth_panel$expamt[which(birth_panel$Year==2016)] <- birth_panel$expamt[which(birth_panel$Year==2016)]*132.4/122.4
birth_panel$expamt[which(birth_panel$Year==2017)] <- birth_panel$expamt[which(birth_panel$Year==2017)]*132.4/125.0
birth_panel$expamt[which(birth_panel$Year==2018)] <- birth_panel$expamt[which(birth_panel$Year==2018)]*132.4/128.4
birth_panel$expamt[which(birth_panel$Year==2019)] <- birth_panel$expamt[which(birth_panel$Year==2019)]*132.4/131.4

birth_panel$hospital_costs[which(birth_panel$Year==1998)] <- birth_panel$hospital_costs[which(birth_panel$Year==1998)]*132.4/93.4
birth_panel$hospital_costs[which(birth_panel$Year==1999)] <- birth_panel$hospital_costs[which(birth_panel$Year==1999)]*132.4/94.4
birth_panel$hospital_costs[which(birth_panel$Year==2000)] <- birth_panel$hospital_costs[which(birth_panel$Year==2000)]*132.4/96.1
birth_panel$hospital_costs[which(birth_panel$Year==2001)] <- birth_panel$hospital_costs[which(birth_panel$Year==2001)]*132.4/97.7
birth_panel$hospital_costs[which(birth_panel$Year==2002)] <- birth_panel$hospital_costs[which(birth_panel$Year==2002)]*132.4/100
birth_panel$hospital_costs[which(birth_panel$Year==2003)] <- birth_panel$hospital_costs[which(birth_panel$Year==2003)]*132.4/102.2
birth_panel$hospital_costs[which(birth_panel$Year==2004)] <- birth_panel$hospital_costs[which(birth_panel$Year==2004)]*132.4/104.2
birth_panel$hospital_costs[which(birth_panel$Year==2005)] <- birth_panel$hospital_costs[which(birth_panel$Year==2005)]*132.4/106.3
birth_panel$hospital_costs[which(birth_panel$Year==2006)] <- birth_panel$hospital_costs[which(birth_panel$Year==2006)]*132.4/108.1
birth_panel$hospital_costs[which(birth_panel$Year==2007)] <- birth_panel$hospital_costs[which(birth_panel$Year==2007)]*132.4/110
birth_panel$hospital_costs[which(birth_panel$Year==2008)] <- birth_panel$hospital_costs[which(birth_panel$Year==2008)]*132.4/112.3
birth_panel$hospital_costs[which(birth_panel$Year==2009)] <- birth_panel$hospital_costs[which(birth_panel$Year==2009)]*132.4/112.3
birth_panel$hospital_costs[which(birth_panel$Year==2010)] <- birth_panel$hospital_costs[which(birth_panel$Year==2010)]*132.4/113.8
birth_panel$hospital_costs[which(birth_panel$Year==2011)] <- birth_panel$hospital_costs[which(birth_panel$Year==2011)]*132.4/116.5
birth_panel$hospital_costs[which(birth_panel$Year==2012)] <- birth_panel$hospital_costs[which(birth_panel$Year==2012)]*132.4/117.8
birth_panel$hospital_costs[which(birth_panel$Year==2013)] <- birth_panel$hospital_costs[which(birth_panel$Year==2013)]*132.4/117.7
birth_panel$hospital_costs[which(birth_panel$Year==2014)] <- birth_panel$hospital_costs[which(birth_panel$Year==2014)]*132.4/118.9
birth_panel$hospital_costs[which(birth_panel$Year==2015)] <- birth_panel$hospital_costs[which(birth_panel$Year==2015)]*132.4/120.2
birth_panel$hospital_costs[which(birth_panel$Year==2016)] <- birth_panel$hospital_costs[which(birth_panel$Year==2016)]*132.4/122.4
birth_panel$hospital_costs[which(birth_panel$Year==2017)] <- birth_panel$hospital_costs[which(birth_panel$Year==2017)]*132.4/125.0
birth_panel$hospital_costs[which(birth_panel$Year==2018)] <- birth_panel$hospital_costs[which(birth_panel$Year==2018)]*132.4/128.4
birth_panel$hospital_costs[which(birth_panel$Year==2019)] <- birth_panel$hospital_costs[which(birth_panel$Year==2019)]*132.4/131.4


###########################################################################
##############              EDUCATION OUTCOMES               ##############
###########################################################################

# 1. Behind a grade
# 2. Special needs
# 3. FSA grades
# 4. Student Learning Survey
# 5. Provincial exams (grade 10, 11, 12)
# 6. Course grades for grade 10 onwards

enrollment <-  read_fwf("R:/DATA/core-snapshot/20230217/ido_med/dat/med1991-2020.stulvlcb_ft_schlstud.C.dat.gz", 
                        fwf_cols(STUDYID=c(1310,1319),
                                 SCHOOL_YEAR=c(73,83),                                     	
                                 AGE_IN_YEARS_DEC_31=c(908,929),
                                 GRADE_TYPE_THIS_ENROLMENT=c(1229,1252),                      	
                                 GRADE_THIS_ENROL=c(1253,1276),
                                 SPECIAL_NEED_THIS_COLL=c(762,803)))
enrollment <- enrollment[which(enrollment$SCHOOL_YEAR %in% c("2004/2005", "2005/2006", "2006/2007", "2007/2008", "2008/2009", "2009/2010", "2010/2011", "2011/2012", "2012/2013",
                                                             "2013/2014", "2014/2015", "2015/2016", "2016/2017", "2017/2018", "2018/2019", "2019/2020", "2020/2021")),]
enrollment <- merge(x=enrollment, y=perinatal[,c("studyid2", "Year")], by.x="STUDYID", by.y="studyid2")
enrollment <- data.table(enrollment)

## Compute if on-track or held-back grade:
# On track if: age-grade<=5 (BC starts based on calendar year)
# If home-schooled, then give missing for on-track
# If drop out, then impute as held-back
enrollment$Grade <- as.numeric(enrollment$GRADE_THIS_ENROL); enrollment$Grade[which(enrollment$GRADE_THIS_ENROL %in% c("KF", "KH"))] <- 0
enrollment$HomeSchooled <- (enrollment$GRADE_THIS_ENROL=="HOME SCHOOLED STUDENT" | enrollment$GRADE_TYPE_THIS_ENROLMENT=="HOME SCHOOLED")

# A student can have multiple special needs
enrollment$SpecialNeeds <- (!enrollment$SPECIAL_NEED_THIS_COLL %in% c("Non Special Need", "Gifted"))
enrollment$SpecialNeeds_Sensory <- (enrollment$SPECIAL_NEED_THIS_COLL %in% c("Visual Impairment", "Deaf Or Hard Of Hearing", "Deafblind"))
enrollment$SpecialNeeds_Physical <- (enrollment$SPECIAL_NEED_THIS_COLL %in% c("Phys Disability Or Chronic Health Impair", "Physically Dependent"))
enrollment$SpecialNeeds_BehavMental <- (enrollment$SPECIAL_NEED_THIS_COLL %in% c("Inten Behav Inter/Serious Mental Illness", "Moderate Behaviour Support/Mental Illnes"))
enrollment$SpecialNeeds_Intellectual <- (enrollment$SPECIAL_NEED_THIS_COLL %in% c("Mild Intellectual Disability", "Mod To Profound Intellectual Disability", "Autism Spectrum Disorder"))
enrollment$SpecialNeeds_Learning <- (enrollment$SPECIAL_NEED_THIS_COLL=="Learning Disability")

# Aggregate
enrollment <- enrollment[, .(Grade=max(Grade, na.rm=T), HomeSchooled=max(HomeSchooled, na.rm=T), SpecialNeeds=max(SpecialNeeds, na.rm=T),
                             SpecialNeeds_Sensory=max(SpecialNeeds_Sensory, na.rm=T), SpecialNeeds_Physical=max(SpecialNeeds_Physical, na.rm=T), SpecialNeeds_BehavMental=max(SpecialNeeds_BehavMental, na.rm=T),
                             SpecialNeeds_Intellectual=max(SpecialNeeds_Intellectual, na.rm=T), SpecialNeeds_Learning=max(SpecialNeeds_Learning, na.rm=T)), by=c("STUDYID", "AGE_IN_YEARS_DEC_31")]
enrollment$Grade[which(enrollment$Grade<0)] <- NA
enrollment$OnTrack <- (enrollment$AGE_IN_YEARS_DEC_31-enrollment$Grade)<=5
enrollment$OnTrack[which(enrollment$HomeSchooled==1)] <- NA
enrollment$BehindGrade <- 1-enrollment$OnTrack

birth_panel <- merge(x=birth_panel, y=enrollment, by.x=c("studyid2", "RelativeYear"), by.y=c("STUDYID", "AGE_IN_YEARS_DEC_31"), all.x=T)

# Enrollment data ends in 2020/2021
birth_panel[Year>2020 | Dead==1, c("BehindGrade", "HomeSchooled", "SpecialNeeds", "SpecialNeeds_Sensory", "SpecialNeeds_Physical", "SpecialNeeds_BehavMental", "SpecialNeeds_Intellectual", "SpecialNeeds_Learning")] <- NA


### FSA grades
fsa1991_2006 <- read_fwf("R:/DATA/core-snapshot/20230217/ido_med/dat/med1999-2006.stulvlcb_ft_fsasclsm0.C.dat.gz",
                         fwf_cols(STUDYID=c(1422,1431),
                                  FSA_SKILL_NAME=c(1126,1148),
                                  FSA_GRADE=c(1118,1121),
                                  SCORE_PCT=c(109,120),
                                  FSA_5_POINT_SCALE=c(746,804),
                                  MINCODE_DELIVERY=c(378,396)))

fsa2007_2016 <- read_fwf("R:/DATA/core-snapshot/20230217/ido_med/dat/med2007-2016.stulvlcb_ft_fsasclsm8.C.dat.gz",
                         fwf_cols(STUDYID=c(1400,1409),
                                  FSA_SKILL_NAME=c(1191,1213),
                                  FSA_GRADE=c(1164,1167),
                                  SCORE_PCT=c(181,192),
                                  FSA_3_POINT_SCALE=c(789,819),
                                  MINCODE_DELIVERY=c(414,432)))

fsa2017_2020 <- read_fwf("R:/DATA/core-snapshot/20230217/ido_med/dat/med2017-2020.stulvlcb_ft_fsasclsm17.D.dat.gz",
                         fwf_cols(STUDYID=c(2045,2054),
                                  FSA_SKILL_NAME=c(1816,1838),
                                  FSA_GRADE=c(1770,1792),
                                  SCORE_PCT=c(169,180),
                                  FSA_3_POINT_SCALE=c(1331,1358),
                                  MINCODE_DELIVERY=c(670,688)))
fsa <- rbind(fsa1991_2006[,c("STUDYID", "FSA_SKILL_NAME", "FSA_GRADE", "SCORE_PCT", "MINCODE_DELIVERY")], fsa2007_2016[,c("STUDYID", "FSA_SKILL_NAME", "FSA_GRADE", "SCORE_PCT", "MINCODE_DELIVERY")], fsa2017_2020[,c("STUDYID", "FSA_SKILL_NAME", "FSA_GRADE", "SCORE_PCT", "MINCODE_DELIVERY")])
rm(fsa1991_2006, fsa2007_2016, fsa2017_2020)
fsa <- data.table(fsa)

# Remove zero:
fsa <- fsa[SCORE_PCT>0]

# Standardize within a school-year-subject
fsa[, `:=` (Mean_withinschool=mean(SCORE_PCT), SD_withinschool=sqrt(var(SCORE_PCT))), by=c("MINCODE_DELIVERY", "FSA_GRADE", "FSA_SKILL_NAME")]
fsa$StandardizedScore_withinschool <- (fsa$SCORE_PCT-fsa$Mean_withinschool)/fsa$SD_withinschool
# Standardize within a year-skill
fsa[, `:=` (Mean=mean(SCORE_PCT), SD=sqrt(var(SCORE_PCT))), by=c("FSA_GRADE", "FSA_SKILL_NAME")]
fsa$StandardizedScore <- (fsa$SCORE_PCT-fsa$Mean)/fsa$SD

fsa$FSA_GRADE <- as.numeric(as.character(fsa$FSA_GRADE))
fsa$Pass <- (fsa$FSA_GRADE>=50)

fsa <- fsa[, .(FSA_Math=mean(StandardizedScore[which(FSA_SKILL_NAME=="Numeracy")], na.rm=T), FSA_Math_withinschool=mean(StandardizedScore_withinschool[which(FSA_SKILL_NAME=="Numeracy")], na.rm=T), FSA_MathPass=(max(SCORE_PCT[which(FSA_SKILL_NAME=="Numeracy")])>=50),
               FSA_Writing=mean(StandardizedScore[which(FSA_SKILL_NAME=="Writing")], na.rm=T), FSA_Writing_withinschool=mean(StandardizedScore_withinschool[which(FSA_SKILL_NAME=="Writing")], na.rm=T), FSA_WritingPass=(max(SCORE_PCT[which(FSA_SKILL_NAME=="Writing")])>=50),
               FSA_Reading=mean(StandardizedScore[which(FSA_SKILL_NAME=="Reading Comprehension")], na.rm=T), FSA_Reading_withinschool=mean(StandardizedScore_withinschool[which(FSA_SKILL_NAME=="Reading Comprehension")], na.rm=T), FSA_ReadingPass=(max(SCORE_PCT[which(FSA_SKILL_NAME=="Reading Comprehension")])>=50)),
           by=c("STUDYID", "FSA_GRADE")]
fsa$FSA_MathPass[which(is.na(fsa$FSA_Math))] <- NA; fsa$FSA_WritingPass[which(is.na(fsa$FSA_Writing))] <- NA; fsa$FSA_ReadingPass[which(is.na(fsa$FSA_Reading))] <- NA; 

# No one in our sample is old enough to take FSA 10 before they got rid of it
fsa$Age <- fsa$FSA_GRADE+5 #This is the age they should be, not the age they actually are
birth_panel <- merge(x=birth_panel, y=fsa, by.x=c("studyid2", "RelativeYear"), by.y=c("STUDYID", "Age"), all.x=T)
birth_panel <- birth_panel[,-c("FSA_GRADE")]

# Complete any FSA?
birth_panel$CompleteAnyFSA <- (!is.na(birth_panel$FSA_Math) | !is.na(birth_panel$FSA_Writing) | !is.na(birth_panel$FSA_Reading))

# FSA data ends after 2020
birth_panel[Year>2020 | Dead==1, c("FSA_Math", "FSA_Math_withinschool", "FSA_MathPass", "FSA_Writing", "FSA_Writing_withinschool", "FSA_WritingPass", "FSA_Reading","FSA_Reading_withinschool", "FSA_ReadingPass", "CompleteAnyFSA")] <- NA

### High school graduation
graduation <- read_fwf("R:/DATA/core-snapshot/20230217/ido_med/dat/med1991-2020.stulvlcb_ft_studcrd.C.dat.gz",
                       fwf_cols(STUDYID=c(1349,1358),
                                CREDENTIAL_NAME=c(1279,1310),
                                CREDENTIAL_GPA=c(13,24),
                                TRAX_SCHOOL_YEAR=c(37,47)))
graduation <- data.table(graduation)
graduation <- graduation[CREDENTIAL_NAME=="BC SECONDARY SCHOOL GRADUATION"]
graduation <- graduation[, .(HSDiploma=1, HSGPA=max(CREDENTIAL_GPA)), by="STUDYID"]
graduation$RelativeYear <- 18
birth_panel <- merge(x=birth_panel, y=graduation[,c("STUDYID", "RelativeYear", "HSDiploma", "HSGPA")], by.x=c("studyid2", "RelativeYear"), by.y=c("STUDYID", "RelativeYear"), all.x=T)
birth_panel$HSDiploma[which(birth_panel$RelativeYear==18 & birth_panel$BirthYear<=2003 & is.na(birth_panel$HSDiploma))] <- 0
birth_panel[Dead==1, c("HSDiploma")] <- NA

#############################################################################
###############               BENEFITS OUTCOMES               ###############
#############################################################################

# Annual benefits of the daughter or mother's family file
cases <-  rbind(data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_1989_2017/dat/idosdpr1989-2017.bceacases.B.dat.gz", 
                                    fwf_cols(ym=c(1,8), fileid=c(9,18), program=c(19,21), pay=c(43,52)))),
                data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2018_2019/dat/idosdpr2018-2019.bceacases.B.dat.gz", 
                                    fwf_cols(ym=c(1,8), fileid=c(9,18), program=c(19,21), pay=c(43,52)))),
                data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2020/dat/idosdpr2020.bceacases.B.dat.gz", 
                                    fwf_cols(ym=c(1,8), fileid=c(9,18), program=c(19,21), pay=c(43,52)))),
                data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2021/dat/idosdpr2021.bceacases.B.dat.gz", 
                                    fwf_cols(ym=c(1,8), fileid=c(9,18), program=c(19,21), pay=c(43,52)))))
cases <- cases[substring(ym, 1, 4)>=1998]

involvement <-  rbind(data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_1989_2017/dat/idosdpr1989-2017.bceainvolvement.A.dat.gz", 
                                          fwf_cols(ym=c(1,8), fileid=c(9,18), studyid=c(69,78)))),
                      data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2018_2019/dat/idosdpr2018-2019.bceainvolvement.A.dat.gz", 
                                          fwf_cols(ym=c(1,8), fileid=c(9,18), studyid=c(69,78)))),
                      data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2020/dat/idosdpr2020.bceainvolvement.A.dat.gz", 
                                          fwf_cols(ym=c(1,8), fileid=c(9,18), studyid=c(69,78)))),
                      data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2021/dat/idosdpr2021.bceainvolvement.A.dat.gz", 
                                          fwf_cols(ym=c(1,8), fileid=c(9,18), studyid=c(69,78)))))
involvement <- involvement[substring(ym, 1, 4)>=1998]

# Disability assistance is "H" according to Jeff (coded incorrectly)
benefits <- cases[, .(Benefits=sum(pay), Disability=max(program=="H")), by=c("fileid", "ym")]

# Get lists of fileid that have mother or newborn on it
involvement <- involvement[studyid %in% perinatal$studyid1 | studyid %in% perinatal$studyid2]
newborn <- involvement[studyid %in% perinatal$studyid2]
newborn$studyid2 <- newborn$studyid
mother <- merge(x=involvement[studyid %in% perinatal$studyid1], y=perinatal[,c("studyid1", "studyid2")], by.x="studyid", by.y="studyid1", allow.cartesian=TRUE)
involvement <- rbind(newborn[,c("ym", "fileid", "studyid2")], mother[,c("ym", "fileid", "studyid2")])
involvement <- unique(involvement)

# Effectively calculating the average monthly benefit of payments to the mother and child
# A newborn can be on multiple fileid, take sum
benefits <- merge(x=benefits, y=involvement, by.x=c("fileid", "ym"), by.y=c("fileid", "ym"))
benefits$Year <- as.numeric(substring(benefits$ym, 1, 4))
benefits <- benefits[, .(MonthlyBenefits=sum(Benefits)/12, DisabilityBenefits=max(Disability)), by=c("studyid2", "Year")]

# Using BC's CPI
benefits$MonthlyBenefits[which(benefits$Year==1998)] <- benefits$MonthlyBenefits[which(benefits$Year==1998)]*132.4/93.4
benefits$MonthlyBenefits[which(benefits$Year==1999)] <- benefits$MonthlyBenefits[which(benefits$Year==1999)]*132.4/94.4
benefits$MonthlyBenefits[which(benefits$Year==2000)] <- benefits$MonthlyBenefits[which(benefits$Year==2000)]*132.4/96.1
benefits$MonthlyBenefits[which(benefits$Year==2001)] <- benefits$MonthlyBenefits[which(benefits$Year==2001)]*132.4/97.7
benefits$MonthlyBenefits[which(benefits$Year==2002)] <- benefits$MonthlyBenefits[which(benefits$Year==2002)]*132.4/100
benefits$MonthlyBenefits[which(benefits$Year==2003)] <- benefits$MonthlyBenefits[which(benefits$Year==2003)]*132.4/102.2
benefits$MonthlyBenefits[which(benefits$Year==2004)] <- benefits$MonthlyBenefits[which(benefits$Year==2004)]*132.4/104.2
benefits$MonthlyBenefits[which(benefits$Year==2005)] <- benefits$MonthlyBenefits[which(benefits$Year==2005)]*132.4/106.3
benefits$MonthlyBenefits[which(benefits$Year==2006)] <- benefits$MonthlyBenefits[which(benefits$Year==2006)]*132.4/108.1
benefits$MonthlyBenefits[which(benefits$Year==2007)] <- benefits$MonthlyBenefits[which(benefits$Year==2007)]*132.4/110
benefits$MonthlyBenefits[which(benefits$Year==2008)] <- benefits$MonthlyBenefits[which(benefits$Year==2008)]*132.4/112.3
benefits$MonthlyBenefits[which(benefits$Year==2009)] <- benefits$MonthlyBenefits[which(benefits$Year==2009)]*132.4/112.3
benefits$MonthlyBenefits[which(benefits$Year==2010)] <- benefits$MonthlyBenefits[which(benefits$Year==2010)]*132.4/113.8
benefits$MonthlyBenefits[which(benefits$Year==2011)] <- benefits$MonthlyBenefits[which(benefits$Year==2011)]*132.4/116.5
benefits$MonthlyBenefits[which(benefits$Year==2012)] <- benefits$MonthlyBenefits[which(benefits$Year==2012)]*132.4/117.8
benefits$MonthlyBenefits[which(benefits$Year==2013)] <- benefits$MonthlyBenefits[which(benefits$Year==2013)]*132.4/117.7
benefits$MonthlyBenefits[which(benefits$Year==2014)] <- benefits$MonthlyBenefits[which(benefits$Year==2014)]*132.4/118.9
benefits$MonthlyBenefits[which(benefits$Year==2015)] <- benefits$MonthlyBenefits[which(benefits$Year==2015)]*132.4/120.2
benefits$MonthlyBenefits[which(benefits$Year==2016)] <- benefits$MonthlyBenefits[which(benefits$Year==2016)]*132.4/122.4
benefits$MonthlyBenefits[which(benefits$Year==2017)] <- benefits$MonthlyBenefits[which(benefits$Year==2017)]*132.4/125.0
benefits$MonthlyBenefits[which(benefits$Year==2018)] <- benefits$MonthlyBenefits[which(benefits$Year==2018)]*132.4/128.4
benefits$MonthlyBenefits[which(benefits$Year==2019)] <- benefits$MonthlyBenefits[which(benefits$Year==2019)]*132.4/131.4

# This one merges on year as opposed to relative year
birth_panel <- merge(x=birth_panel, y=benefits, by.x=c("studyid2", "Year"), by.y=c("studyid2", "Year"), all.x=T)

# Clean benefits
setnafill(birth_panel, cols=c("MonthlyBenefits", "DisabilityBenefits"), fill=0)
birth_panel[Year>2021 | Dead==1, c("MonthlyBenefits", "DisabilityBenefits")] <- NA

#########################################################################
###############               CHILD SERVICES               ##############
#########################################################################

childservices <- fread("ChildServices_Intake_Protection.csv")

# supervisionmcfd: if in that year, max of supervision by mcfd
# If supervisionmcfd=0, then all other variables should be NA

# levelcare: if they were in a specialized care (serious disabilities, behavioral issues)

# neglect and protection: in readme

# returnparent, permanent_transfer, adoption, temp_transfer
# these variables are the "weakest", and it's just the PLAN
# transfer: taken from home and go somewhere else

# other: mentoring relationships? 

# Ever part of any service provided by MCFD (including counseling services for children)
# Child under MFCD supervision
# Child under MFCD supervision for neglect/abuse
# Child under foster care

birth_panel <- merge(x=birth_panel, y=childservices[,c("STUDYID", "agecat", "supervisionmcfd", "supervision_neglect", "protection_ever", "fostercare", "levelcare")],
                     by.x=c("studyid2", "RelativeYear"), by.y=c("STUDYID", "agecat"), all.x=T)

setnafill(birth_panel, cols=c("supervisionmcfd", "supervision_neglect", "protection_ever", "fostercare", "levelcare"), fill=0)
birth_panel[Year>2021 | Dead==1, c("supervisionmcfd", "supervision_neglect", "protection_ever", "fostercare", "levelcare")] <- NA


#########################################################################
###############               LEFT PROVINCE               ###############
#########################################################################

# If not in MSP enrollment (rpblite) then assume left province
birth_panel$MSPEnrolled <- NA
for(year in 2001:2022){
  rpblite <- read_fwf(paste("R:/DATA/core-snapshot/20230217/rpblite/dat/calendar/rpblite", year, ".D.dat.gz", sep=""),
                      fwf_cols(studyid=c(78,87)))
  birth_panel$MSPEnrolled[which(birth_panel$Year==year)] <- (birth_panel$studyid2[which(birth_panel$Year==year)] %in% rpblite$studyid)
}



#########################################################################
#########               PRESCRIPTION UTILIZATION               ##########
#########################################################################


alldrugs <- NULL
for(year in c("1999_2001","2002_2004", "2005_2007", "2008_2009", "2010_2011", "2012_2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")){
  print(year)
  rx <-  read_fwf(paste("R:/DATA/core-snapshot/20230217/pharmanet/dat/pharmanet_claims_", year, ".A.dat.gz",  sep=""),
                  fwf_cols(PC.STUDYID=c(1,10), PC.HLTH_PROD_LABEL=c(380,389), PC.SRV_DATE=c(392,401), PC.DSPD_DAYS_SPLY=c(422,431), PC.BLD_PROF_FEE=c(482,491), PC.BLD_PROD_COST=c(452,461)))
  rx <- data.table(rx)
  rx <- rx[PC.STUDYID %in% perinatal$studyid2]
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  alldrugs <- rbind(alldrugs, rx)
  rm(rx)
}

alldrugs <- unique(alldrugs)
alldrugs$PC.SRV_DATE <- as.Date(alldrugs$PC.SRV_DATE, "%Y-%m-%d")
alldrugs <- merge(x=alldrugs, y=perinatal[,c("studyid2", "BirthDate")], by.x="PC.STUDYID", by.y="studyid2")
alldrugs$RelativeYear <- ceiling(as.numeric(alldrugs$PC.SRV_DATE-alldrugs$BirthDate)/365)
alldrugs$RelativeYear[which(alldrugs$RelativeYear<1)] <- 1

alldrugs <- alldrugs[,.(DaysSupply=sum(PC.DSPD_DAYS_SPLY), DrugSpending=sum(PC.BLD_PROF_FEE+PC.BLD_PROD_COST), N_UniqueDrugs=length(unique(PC.HLTH_PROD_LABEL))), by=c("PC.STUDYID", "RelativeYear")]

birth_panel <- merge(x=birth_panel, y=alldrugs, by.x=c("studyid2", "RelativeYear"), by.y=c("PC.STUDYID", "RelativeYear"), all.x=T)

birth_panel$DrugSpending[which(birth_panel$Year==1998)] <- birth_panel$DrugSpending[which(birth_panel$Year==1998)]*132.4/93.4
birth_panel$DrugSpending[which(birth_panel$Year==1999)] <- birth_panel$DrugSpending[which(birth_panel$Year==1999)]*132.4/94.4
birth_panel$DrugSpending[which(birth_panel$Year==2000)] <- birth_panel$DrugSpending[which(birth_panel$Year==2000)]*132.4/96.1
birth_panel$DrugSpending[which(birth_panel$Year==2001)] <- birth_panel$DrugSpending[which(birth_panel$Year==2001)]*132.4/97.7
birth_panel$DrugSpending[which(birth_panel$Year==2002)] <- birth_panel$DrugSpending[which(birth_panel$Year==2002)]*132.4/100
birth_panel$DrugSpending[which(birth_panel$Year==2003)] <- birth_panel$DrugSpending[which(birth_panel$Year==2003)]*132.4/102.2
birth_panel$DrugSpending[which(birth_panel$Year==2004)] <- birth_panel$DrugSpending[which(birth_panel$Year==2004)]*132.4/104.2
birth_panel$DrugSpending[which(birth_panel$Year==2005)] <- birth_panel$DrugSpending[which(birth_panel$Year==2005)]*132.4/106.3
birth_panel$DrugSpending[which(birth_panel$Year==2006)] <- birth_panel$DrugSpending[which(birth_panel$Year==2006)]*132.4/108.1
birth_panel$DrugSpending[which(birth_panel$Year==2007)] <- birth_panel$DrugSpending[which(birth_panel$Year==2007)]*132.4/110
birth_panel$DrugSpending[which(birth_panel$Year==2008)] <- birth_panel$DrugSpending[which(birth_panel$Year==2008)]*132.4/112.3
birth_panel$DrugSpending[which(birth_panel$Year==2009)] <- birth_panel$DrugSpending[which(birth_panel$Year==2009)]*132.4/112.3
birth_panel$DrugSpending[which(birth_panel$Year==2010)] <- birth_panel$DrugSpending[which(birth_panel$Year==2010)]*132.4/113.8
birth_panel$DrugSpending[which(birth_panel$Year==2011)] <- birth_panel$DrugSpending[which(birth_panel$Year==2011)]*132.4/116.5
birth_panel$DrugSpending[which(birth_panel$Year==2012)] <- birth_panel$DrugSpending[which(birth_panel$Year==2012)]*132.4/117.8
birth_panel$DrugSpending[which(birth_panel$Year==2013)] <- birth_panel$DrugSpending[which(birth_panel$Year==2013)]*132.4/117.7
birth_panel$DrugSpending[which(birth_panel$Year==2014)] <- birth_panel$DrugSpending[which(birth_panel$Year==2014)]*132.4/118.9
birth_panel$DrugSpending[which(birth_panel$Year==2015)] <- birth_panel$DrugSpending[which(birth_panel$Year==2015)]*132.4/120.2
birth_panel$DrugSpending[which(birth_panel$Year==2016)] <- birth_panel$DrugSpending[which(birth_panel$Year==2016)]*132.4/122.4
birth_panel$DrugSpending[which(birth_panel$Year==2017)] <- birth_panel$DrugSpending[which(birth_panel$Year==2017)]*132.4/125.0
birth_panel$DrugSpending[which(birth_panel$Year==2018)] <- birth_panel$DrugSpending[which(birth_panel$Year==2018)]*132.4/128.4
birth_panel$DrugSpending[which(birth_panel$Year==2019)] <- birth_panel$DrugSpending[which(birth_panel$Year==2019)]*132.4/131.4

setnafill(birth_panel, cols=c("DaysSupply", "DrugSpending", "N_UniqueDrugs"), fill=0)
birth_panel[Year>2021 | Dead==1, c("DaysSupply", "DrugSpending", "N_UniqueDrugs")] <- NA



fwrite(birth_panel, "panel_outcomes.gz")

