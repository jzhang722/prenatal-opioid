### This code constructs different prenatal opioid exposure measures and saves them in different csv files


#################################################################################################
#############                 1. NAS FROM PERINATAL DIAGNOSIS FILES                 #############
#################################################################################################

perinatal <- read_fwf("R:/working/unzipped_raw_data/perinatal2000-2020.babynewborn.D.dat", 
                      fwf_cols(b_res_ha=c(1,4),
                               b_res_ha_text=c(5,23),
                               b_res_hsda=c(24,27),
                               b_res_hsda_text=c(28,58),
                               b_res_lha=c(59,63),
                               b_res_lha_text=c(64,90),
                               baby_sequence=c(91,93),
                               multiple_birth_count=c(94,96),
                               b_institution_id=c(97,102),
                               b_institution_to=c(103,108),
                               nb_transfer_higher=c(109,111),
                               b_admission_dateyyyy=c(112,115),
                               b_admission_datemm=c(116,117),
                               b_admission_hour=c(118,135),
                               b_discharge_dateyyyy=c(136,139),
                               b_discharge_datemm=c(140,141),
                               b_discharge_hour=c(142,159),
                               b_fiscal_yr=c(160,170),
                               b_tot_los=c(171,182),
                               nicu_ii=c(183,187),
                               nicu_iii=c(188,192),
                               b_admission_weight=c(193,198),
                               discharge_weight=c(199,204),
                               gest_age_by_exam=c(205,208),
                               gest_age_from_document=c(209,212),
                               final_ga=c(213,216),
                               birth_length=c(217,220),
                               birth_head_circumference=c(221,224),
                               b_birth_type=c(225,237),
                               stillbirth=c(238,240),
                               vitamin_k=c(241,243),
                               eye_prophylaxis=c(244,246),
                               breast_feed_at_discharge=c(247,249),
                               newborn_feeding=c(250,253),
                               breast_feeding_initiation=c(254,257),
                               discharge_to=c(258,260),
                               temperature_first=c(261,272),
                               surfactant=c(273,275),
                               b_antibiotics=c(276,278),
                               apgar_1_minute=c(279,282),
                               apgar_5_minutes=c(283,286),
                               apgar_10_minutes=c(287,290),
                               meconium_thick=c(291,293),
                               meconium=c(294,296),
                               drugs=c(297,299),
                               suction_perineum=c(300,302),
                               suction_oropharynx=c(303,305),
                               suction_trachea=c(306,308),
                               suction_unspecified=c(309,311),
                               oxygen_flg=c(312,314),
                               age_start_oxygen=c(315,326),
                               age_stop_oxygen=c(327,338),
                               b_resus_dur_oxygen=c(339,350),
                               ippv_mask_flg=c(351,353),
                               age_start_ippv_mask=c(354,365),
                               age_stop_ippv_mask=c(366,377),
                               b_resus_dur_ippv_mask=c(378,389),
                               ippv_ett_flg=c(390,392),
                               age_start_ippv_ett=c(393,404),
                               age_stop_ippv_ett=c(405,416),
                               b_resus_dur_ippv_ett=c(417,428),
                               chest_compress_flg=c(429,431),
                               age_start_chest_compress=c(432,443),
                               age_stop_chest_compress=c(444,455),
                               b_resus_dur_chest_compress=c(456,467),
                               ventilator_days=c(468,472),
                               cpap_days=c(473,477),
                               oxygen_days=c(478,482),
                               tpn_days=c(483,487),
                               cord_arterial_gases_ph=c(488,499),
                               cord_arterial_gases_base=c(500,511),
                               baby_pos_blood_culture=c(512,514),
                               baby_blood_infect_agent_1=c(515,521),
                               baby_blood_infect_agent_2=c(522,528),
                               baby_pos_urine_culture=c(529,531),
                               baby_urine_infect_agent_1=c(532,538),
                               baby_urine_infect_agent_2=c(539,545),
                               baby_pos_other_culture=c(546,548),
                               baby_other_infect_agent_1=c(549,555),
                               baby_other_infect_agent_2=c(556,562),
                               b_main_patient_service=c(563,566),
                               b_postal_code3=c(567,569),
                               b_date_of_birthyyyy=c(570,573),
                               b_date_of_birthmm=c(574,575),
                               sex=c(576,576),
                               newbornbaby_motherid=c(577,586),
                               newborn_babyid=c(587,596),
                               linkbaby=c(597,606),
                               source=c(607,610),
                               version=c(611,612),
                               seqno=c(613,622),
                               studyid1=c(623,632),
                               studyid2=c(633,642),
                               linefeed=c(643,643)))
perinatal <- data.table(perinatal)
perinatal$BirthDate <- paste(perinatal$b_date_of_birthmm, perinatal$b_date_of_birthyyyy, sep="-")





### DRUG ABUSE: 
## ICD10
# Newborn experiencing withdrawal symptoms from maternal drugs of addition: P96.1 (typical NAS dx)
# Newborn affected by maternal use of drugs of addiction: P04.4
# Newborn affected by maternal use of maternal medication: P04.1

# Newborn experiencing withdrawal symptoms from therapeutic use of drugs in newborn: P96.2 (eg, baby is given morphine and then has withdrawal)

# ICD9
# Newborn affected by narcotics affecting fetus: like 760.7 but not 760.71 (alcohol)
# Drug withdrawal syndrome in newborn: 779.5



# Newborn diagnoses
nb <-  read_fwf("R:/DATA/core-snapshot/20230217/perinatal/dat/perinatal2000-2020.pdrdiagnoses_baby_nb.B.dat.gz", 
                fwf_cols(patient_id=c(1,10),
                         diagnosis_prefix=c(11,13),
                         diagnosis_cd=c(14,24),
                         diagnosis_type=c(25,27),
                         source=c(28,31),
                         version=c(32,33),
                         seqno=c(34,43),
                         studyid=c(44,53),
                         linefeed=c(54,54)))
nb <- data.table(nb)

bt <-  read_fwf("R:/DATA/core-snapshot/20230217/perinatal/dat/perinatal2000-2020.pdrdiagnoses_baby_bt.B.dat.gz", 
                fwf_cols(patient_id=c(1,10),
                         diagnosis_prefix=c(11,13),
                         diagnosis_cd=c(14,24),
                         diagnosis_type=c(25,27),
                         source=c(28,31),
                         version=c(32,33),
                         seqno=c(34,43),
                         studyid=c(44,53),
                         linefeed=c(54,54)))
bt <- data.table(bt)
newborn_dx <- rbind(nb, bt); rm(nb, bt)

newborn_dx <- newborn_dx[diagnosis_cd %in% c(#ICD10
  "P961", #Newborn experiencing withdrawal symptoms from maternal drugs of addiction
  "P044", #Newborn affected by maternal use of drugs of addiction
  "P041", #Newborn affected by other maternal medication (P0414 are opioids, but diagnosis code isn't that refined)
  # ICD9
  "7795", #Drug withdrawal syndrome in newborn
  "7607", #Noxious influences affecting fetus or newborn (unknown)
  "76070", #Noxious influences affecting fetus or newborn (unspecified)
  "76072", #Noxious influences affecting fetus or newborn (narcotics)
  "76073", #Noxious influences affecting fetus or newborn (hallucinogenic)
  "76075", #Noxious influences affecting fetus or newborn (cocaine)
  "76079"), #Noxious influences affecting fetus or newborn (other)
  c("studyid", "seqno", "diagnosis_cd")]

newborn_dx <- merge(x=newborn_dx, y=perinatal[,c("studyid1", "studyid2")], by.x="studyid", by.y="studyid2")
newborn_dx <- newborn_dx[,c("studyid1", "studyid", "diagnosis_cd")]
names(newborn_dx) <- c("studyid1", "studyid2", "diagnosis_cd")
newborn_dx <- unique(newborn_dx)



# Mothers delivery (nothing comes from postpartum)
maternal_dx_md <-  read_fwf("R:/DATA/core-snapshot/20230217/perinatal/dat/perinatal2000-2020.pdrdiagnoses_mother_md.B.dat.gz", 
                            fwf_cols(patient_id=c(1,10),
                                     diagnosis_prefix=c(11,13),
                                     diagnosis_cd=c(14,24),
                                     diagnosis_type=c(25,27),
                                     source=c(28,31),
                                     version=c(32,33),
                                     seqno=c(34,43),
                                     studyid=c(44,53),
                                     linefeed=c(54,54)))
maternal_dx_md <- data.table(maternal_dx_md)

maternal_dx_mp <-  read_fwf("R:/DATA/core-snapshot/20230217/perinatal/dat/perinatal2000-2020.pdrdiagnoses_mother_mp.B.dat.gz", 
                            fwf_cols(patient_id=c(1,10),
                                     diagnosis_prefix=c(11,13),
                                     diagnosis_cd=c(14,24),
                                     diagnosis_type=c(25,27),
                                     source=c(28,31),
                                     version=c(32,33),
                                     seqno=c(34,43),
                                     studyid=c(44,53),
                                     linefeed=c(54,54)))
maternal_dx_mp <- data.table(maternal_dx_mp)
maternal_dx <- rbind(maternal_dx_md, maternal_dx_mp); rm(maternal_dx_md, maternal_dx_mp)
maternal_dx <- unique(maternal_dx)

# Mother's opioid abuse and dependence, or other substances
maternal_dx <- maternal_dx[(diagnosis_cd %like% "^F111" | diagnosis_cd %like% "^F112" | diagnosis_cd %like% "^304" | diagnosis_cd %like% "^305" |
                              diagnosis_cd %like% "^F13" | diagnosis_cd %like% "^F14" | diagnosis_cd %like% "^F15") & !diagnosis_cd %like% "^3043", c("studyid", "diagnosis_cd", "seqno", "patient_id")]
maternal_dx <- merge(x=maternal_dx, y=perinatal[,c("studyid1", "studyid2", "newbornbaby_motherid")], by.x=c("studyid", "patient_id"), by.y=c("studyid1", "newbornbaby_motherid"))
maternal_dx <- maternal_dx[,c("studyid", "studyid2", "diagnosis_cd")]
names(maternal_dx) <- c("studyid1", "studyid2", "diagnosis_cd")
maternal_dx <- unique(maternal_dx)

delivery_dx <- rbind(newborn_dx, maternal_dx)
delivery_dx <- delivery_dx[, .(NAS=max(diagnosis_cd=="P961" | diagnosis_cd=="7795"),
                               Newborn_AffectedByDrugs=max(diagnosis_cd=="P044" | diagnosis_cd=="76072" | diagnosis_cd=="76073" | diagnosis_cd=="76075"),
                               Newborn_AffectedOtherMedication=max(diagnosis_cd=="P041" | diagnosis_cd=="7607" | diagnosis_cd=="76070" | diagnosis_cd=="76079"),
                               MaternalOpioidAbuse_Delivery=max(diagnosis_cd %like% "^F111" | diagnosis_cd %like% "^F112" | diagnosis_cd %like% "^3040" | diagnosis_cd %like% "^3047" | diagnosis_cd %like% "^3048" | diagnosis_cd %like% "^3055" ),
                               MaternalDrugAbuse_Delivery=max(diagnosis_cd %like% "^F111" | diagnosis_cd %like% "^F112" | diagnosis_cd %like% "^304" | diagnosis_cd %like% "^305" |
                                                                diagnosis_cd %like% "^F13" | diagnosis_cd %like% "^F14" | diagnosis_cd %like% "^F15")), by=c("studyid1", "studyid2")]

fwrite(delivery_dx, "perinatal_dx_newborn_mother.csv")



#################################################################################################
############             2. SUBSTANCE USE FLAGS IN DELIVERY QUESTIONNAIRE            ############
#################################################################################################

motherdelivery <-  read_fwf("R:/working/unzipped_raw_data/perinatal2000-2020.motherdelivery.D.dat", 
                            fwf_cols(STUDYID=c(979,988),
                                     DELIVERY_MOTHERID=c(943,952),
                                     R_DRUGS_FLG=c(565,567), # drug use during pregnancy identified as a risk
                                     R_SUBSTANCE_USE=c(568,570),
                                     R_HEROIN=c(571,573),
                                     R_COCAINE=c(574,576),
                                     R_METHADONE=c(577,579),
                                     R_RX=c(583,585),
                                     R_MARIJUANA=c(586,588),
                                     R_OTHER_DRUG=c(589,591),
                                     R_UNK_DRUG=c(592,594)))

motherdelivery <- data.table(motherdelivery)
motherdelivery <- motherdelivery[R_HEROIN==1 | R_SUBSTANCE_USE==1 | R_COCAINE==1 | R_METHADONE==1 | R_RX==1 | R_MARIJUANA==1 | R_OTHER_DRUG==1 | R_UNK_DRUG==1 | R_DRUGS_FLG=='Y']
motherdelivery <- merge(x=motherdelivery, y=perinatal[,c("studyid1", "studyid2", "newbornbaby_motherid")], by.x=c("STUDYID", "DELIVERY_MOTHERID"), by.y=c("studyid1", "newbornbaby_motherid"))
setnames(motherdelivery, old="STUDYID", new="studyid1")
motherdelivery <- motherdelivery[,.(R_HEROIN=max(R_HEROIN==1), R_SUBSTANCE_USE=max(R_SUBSTANCE_USE==1 | R_DRUGS_FLG=="Y"), R_DRUGS_FLG=max(R_DRUGS_FLG=="Y"), R_COCAINE=max(R_COCAINE==1), R_METHADONE=max(R_METHADONE==1),
                                    R_RX=max(R_RX==1), R_MARIJUANA=max(R_MARIJUANA==1), R_OTHER_DRUG=max(R_OTHER_DRUG==1)), by=c("studyid1", "studyid2")]

fwrite(motherdelivery, "substance_use_questionnaire_delivery.csv")



###################################################################################################
############            3. NAS FROM NEWBORN ED/HOSPITAL DX IN FIRST 2 MONTHS           ############
###################################################################################################

# Get all NAS-related hospitalizations in the DAD in the first 2 calendar months of the birth

perinatal$BirthDate <- as.Date(paste("01-", perinatal$BirthDate, sep=""), "%d-%m-%Y")
newborn_dad_dx <- NULL
for(year in 2000:2016){
  dad <- data.table(read_dta(paste("R:/working/jeffs_cleaned_files/dad", year, ".dta", sep="")))
  dad$admission_date <- as.Date(dad$admission_date, "%Y-%m-%d")
  dad <- dad[diag1 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079") | 
               diag2 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079") |
               diag3 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079")]
  dad$NAS <- dad$diag1 %in% c("P961", "7795") | dad$diag2 %in% c("P961", "7795") | dad$diag3 %in% c("P961", "7795") 
  dad$Newborn_AffectedByDrugs <- dad$diag1 %in% c("P044", "76072", "76073", "76075") | dad$diag2 %in% c("P044", "76072", "76073", "76075") | dad$diag3 %in% c("P044", "76072", "76073", "76075")
  dad$Newborn_AffectedOtherMedication <- dad$diag1 %in% c("P041", "7607", "76070", "76079") | dad$diag2 %in% c("P041", "7607", "76070", "76079") | dad$diag3 %in% c("P041", "7607", "76070", "76079") 
  dad <- merge(x=dad, y=perinatal[,c("studyid1", "studyid2", "BirthDate")], by.x="studyid", by.y="studyid2")
  dad <- dad[as.numeric(admission_date-BirthDate)<60] # within the same or next calendar month
  setnames(dad, old="studyid", new="studyid2")
  dad <- dad[, .(NAS=max(NAS), Newborn_AffectedByDrugs=max(Newborn_AffectedByDrugs), Newborn_AffectedOtherMedication=max(Newborn_AffectedOtherMedication)), by=c("studyid1", "studyid2")]
  newborn_dad_dx <- rbind(newborn_dad_dx, dad)
}
for(year in c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21")){
  dad <- data.table(read_fwf(paste("R:/DATA/core-snapshot/20230217/hospital/dat/hospital", year, ".O.dat.gz", sep=""),
                             fwf_cols(admission_date=c(1,10),
                                      diag1=c(233,239),
                                      diag2=c(240,246),
                                      diag3=c(247,253),
                                      studyid=c(2786,2795))))
  dad$admission_date <- as.Date(dad$admission_date, "%Y-%m-%d")
  dad <- dad[diag1 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079") | 
               diag2 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079") |
               diag3 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079")]
  dad$NAS <- dad$diag1 %in% c("P961", "7795") | dad$diag2 %in% c("P961", "7795") | dad$diag3 %in% c("P961", "7795") 
  dad$Newborn_AffectedByDrugs <- dad$diag1 %in% c("P044", "76072", "76073", "76075") | dad$diag2 %in% c("P044", "76072", "76073", "76075") | dad$diag3 %in% c("P044", "76072", "76073", "76075")
  dad$Newborn_AffectedOtherMedication <- dad$diag1 %in% c("P041", "7607", "76070", "76079") | dad$diag2 %in% c("P041", "7607", "76070", "76079") | dad$diag3 %in% c("P041", "7607", "76070", "76079") 
  dad <- merge(x=dad, y=perinatal[,c("studyid1", "studyid2", "BirthDate")], by.x="studyid", by.y="studyid2")
  dad <- dad[as.numeric(admission_date-BirthDate)<60] # within the same or next calendar month
  setnames(dad, old="studyid", new="studyid2")
  dad <- dad[, .(NAS=max(NAS), Newborn_AffectedByDrugs=max(Newborn_AffectedByDrugs), Newborn_AffectedOtherMedication=max(Newborn_AffectedOtherMedication)), by=c("studyid1", "studyid2")]
  newborn_dad_dx <- rbind(newborn_dad_dx, dad)
}
newborn_dad_dx <- newborn_dad_dx[, .(NAS=max(NAS), Newborn_AffectedByDrugs=max(Newborn_AffectedByDrugs), Newborn_AffectedOtherMedication=max(Newborn_AffectedOtherMedication)), by=c("studyid1", "studyid2")]
rm(dad)

# Get all NAS-related emergency visits in the NACRS in the first 2 calendar months of the birth
# NACRS always uses ICD10
NACRS <-  read_fwf("R:/working/unzipped_raw_data/nacrs2011-2021.A.dat", 
                   fwf_cols(NACRS.FILEYEAR=c(1,4),
                            NACRS.ACCESS_CD=c(5,5),
                            NACRS.AGE1=c(6,6),
                            NACRS.AGE10=c(7,8),
                            NACRS.AGEYRS=c(9,11),
                            NACRS.AMBCARETYPE=c(12,13),
                            NACRS.AMBULANC=c(14,14),
                            NACRS.ARRDATE=c(15,24),
                            NACRS.ARRTIME=c(25,64),
                            NACRS.ASSESSDATETIME=c(65,94),
                            NACRS.BATPD=c(95,96),
                            NACRS.CARE_LEVEL=c(97,97),
                            NACRS.CDU=c(98,98),
                            NACRS.CDUINDATE=c(99,108),
                            NACRS.CDUINTIME=c(109,148),
                            NACRS.CDUOUTDATE=c(149,158),
                            NACRS.CDUOUTTIME=c(159,198),
                            NACRS.COMPLAINT1=c(199,201),
                            NACRS.COMPLAINT2=c(202,204),
                            NACRS.COMPLAINT3=c(205,207),
                            NACRS.DISPDATETIME=c(208,237),
                            NACRS.DOB=c(238,244),
                            NACRS.DOBEST=c(245,245),
                            NACRS.DOC_SPEC=c(246,250),
                            NACRS.EDDIAG1=c(251,256),
                            NACRS.EDDIAG2=c(257,262),
                            NACRS.EDDIAG3=c(263,268),
                            NACRS.EDVISIT=c(269,269),
                            NACRS.ERTIME=c(270,275),
                            NACRS.GENDER=c(276,276),
                            NACRS.HCNPROV=c(277,278),
                            NACRS.HOSP=c(279,281),
                            NACRS.HOSPPROV=c(282,282),
                            NACRS.HOSPPROV_CHAR=c(283,284),
                            NACRS.INST_NUM=c(285,288),
                            NACRS.LEFTERDATETIME=c(289,318),
                            NACRS.LHA3=c(319,321),
                            NACRS.LOSHRS=c(322,326),
                            NACRS.POSTAL3CHAR=c(327,329),
                            NACRS.PROVNUM2=c(330,334),
                            NACRS.PROVNUM3=c(335,339),
                            NACRS.PROVNUM4=c(340,344),
                            NACRS.PROVNUM5=c(345,349),
                            NACRS.PROVNUM6=c(350,354),
                            NACRS.PROVNUM7=c(355,359),
                            NACRS.PROVNUM8=c(360,364),
                            NACRS.PROVSTCD=c(365,366),
                            NACRS.PROVTYP1=c(367,367),
                            NACRS.PROVTYP2=c(368,368),
                            NACRS.REGDATETIME=c(369,398),
                            NACRS.RESPPHYS=c(399,403),
                            NACRS.RFP=c(404,405),
                            NACRS.SUBLEVEL=c(406,407),
                            NACRS.TRIAGELEVEL=c(408,408),
                            NACRS.TRIAGEDATETIME=c(409,438),
                            NACRS.TTODHRS=c(439,446),
                            NACRS.VISDISP=c(447,448),
                            NACRS.WAITPIA=c(449,454),
                            STUDYID=c(455,464),
                            Linefeed=c(465,465)))
NACRS <- data.table(NACRS)
NACRS$NACRS.REGDATETIME <- as.Date(substring(NACRS$NACRS.REGDATETIME, 1, 10), "%Y-%m-%d")
NACRS <- NACRS[NACRS.EDDIAG1 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079") | 
                 NACRS.EDDIAG2 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079") |
                 NACRS.EDDIAG3 %in% c("P961", "7795", "P041", "P044", "76072", "76073", "76075", "7607", "76070", "76079")]

# Get all NAS-related MSP diagnoses
# MSP always uses ICD9
perinatal$BirthDate <- paste(perinatal$b_date_of_birthmm, perinatal$b_date_of_birthyyyy, sep="-")
perinatal$BirthDate <- as.Date(paste("01-", perinatal$BirthDate, sep=""), "%d-%m-%Y")
newborn_msp_dx <- NULL

for(year in c("1999_00", "2000_01", "2001_02", "2002_03", "2003_04", "2004_05", "2005_06", "2006_07", "2007_08", "2008_09", "2009_10",
              "2010_11", "2011_12", "2012_13", "2013_14", "2014_15", "2015_16", "2016_17", "2017_18")){
  print(year)
  msp <- data.table(read_sas(paste("R:/working/jeffs_cleaned_files/msp_", year, ".sas7bdat", sep="")))
  msp$servdate <- as.Date(msp$servdate, "%Y-%m-%d")
  msp <- msp[icd9_1 %in% c("P961", "7795", "P041", "P044", "76072", "7073", "76075", "7607", "76070", "76079") | 
               icd9_2 %in% c("P961", "7795", "P041", "P044", "76072", "7073", "76075", "7607", "76070", "76079") |
               icd9_3 %in% c("P961", "7795", "P041", "P044", "76072", "7073", "76075", "7607", "76070", "76079")]
  msp$NAS <- msp$icd9_1 %in% c("P961", "7795") | msp$icd9_2 %in% c("P961", "7795") | msp$icd9_3 %in% c("P961", "7795") 
  msp$Newborn_AffectedByDrugs <- msp$icd9_1 %in% c("P044", "76072", "76073", "76075") | msp$icd9_2 %in% c("P044", "76072", "76073", "76075") | msp$icd9_3 %in% c("P044", "76072", "76073", "76075")
  msp$Newborn_AffectedOtherMedication <- msp$icd9_1 %in% c("P041", "7607", "76070", "76079") | msp$icd9_2 %in% c("P041", "7607", "76070", "76079") | msp$icd9_3 %in% c("P041", "7607", "76070", "76079") 
  msp <- merge(x=msp, y=perinatal[,c("studyid1", "studyid2", "BirthDate")], by.x="studyid", by.y="studyid2")
  msp <- msp[as.numeric(servdate-BirthDate)<60] # within the same or next calendar month
  setnames(msp, old="studyid", new="studyid2")
  msp <- msp[, .(NAS=max(NAS), Newborn_AffectedByDrugs=max(Newborn_AffectedByDrugs), Newborn_AffectedOtherMedication=max(Newborn_AffectedOtherMedication)), by=c("studyid1", "studyid2")]
  newborn_msp_dx <- rbind(newborn_msp_dx, msp)
}
newborn_msp_dx <- newborn_msp_dx[, .(NAS=max(NAS), Newborn_AffectedByDrugs=max(Newborn_AffectedByDrugs), Newborn_AffectedOtherMedication=max(Newborn_AffectedOtherMedication)), by=c("studyid1", "studyid2")]


newborn_dx_2months <- rbind(newborn_msp_dx, newborn_dad_dx)
newborn_dx_2months <- newborn_dx_2months[, .(NAS=max(NAS), Newborn_AffectedByDrugs=max(Newborn_AffectedByDrugs), Newborn_AffectedOtherMedication=max(Newborn_AffectedOtherMedication)), by=c("studyid1", "studyid2")]

fwrite(newborn_dx_2months, "newborn_dx_2months.csv")


########################################################################################################
############             MOTHER DRUG ABUSE/DEPENDENCE/OVERDOSE DURING PREGNANCY             ############
########################################################################################################

# opioid abuse/dependence/overdose
#ICD10: %like% "F111" %like% "F112" 
#ICD10: "T400", "T401", "T402", "T403", "T404", "T406"
#ICD9: %like% "3040" %like% "3047"  %like% "3048"  %like% "3055"
#ICD9: like 9650, 9701, E850, E9350, E9351, E9352, E9401

# drug abuse/dependence/overdose
#ICD10:F11, F13, F14, F15, F16, T39, T40, T41, T42, T436
#ICD9: (%like% "304" | %like% "305") AND NOT 3043
#ICD9: 965, 967, 970, E850, E852, E855


perinatal$BirthDate <- paste(perinatal$b_date_of_birthmm, perinatal$b_date_of_birthyyyy, sep="-")
perinatal <- perinatal[,c("studyid1", "gest_age_from_document", "BirthDate")]


##### NACRS
NACRS <-  data.table(read_fwf("R:/DATA/core-snapshot/20230217/nacrs/dat/nacrs2011-2021.A.dat.gz", 
                              fwf_cols(STUDYID=c(455,464),
                                       NACRS.REGDATETIME=c(369,398),
                                       NACRS.EDDIAG1=c(251,256),
                                       NACRS.EDDIAG2=c(257,262),
                                       NACRS.EDDIAG3=c(263,268))))
NACRS <- data.table(NACRS)
NACRS$NACRS.REGDATETIME <- as.Date(substring(NACRS$NACRS.REGDATETIME, 1, 10), "%Y-%m-%d")
NACRS <- NACRS[NACRS.EDDIAG1 %like% "^F11" | NACRS.EDDIAG2 %like% "^F11" | NACRS.EDDIAG3 %like% "^F11" |
                 NACRS.EDDIAG1 %like% "^F13" | NACRS.EDDIAG2 %like% "^F13" | NACRS.EDDIAG3 %like% "^F13" |
                 NACRS.EDDIAG1 %like% "^F14" | NACRS.EDDIAG2 %like% "^F14" | NACRS.EDDIAG3 %like% "^F14" |
                 NACRS.EDDIAG1 %like% "^F15" | NACRS.EDDIAG2 %like% "^F15" | NACRS.EDDIAG3 %like% "^F15" |
                 NACRS.EDDIAG1 %like% "^F16" | NACRS.EDDIAG2 %like% "^F16" | NACRS.EDDIAG3 %like% "^F16" |
                 NACRS.EDDIAG1 %like% "^T39" | NACRS.EDDIAG2 %like% "^T39" | NACRS.EDDIAG3 %like% "^T39" |
                 NACRS.EDDIAG1 %like% "^T40" | NACRS.EDDIAG2 %like% "^T40" | NACRS.EDDIAG3 %like% "^T40" |
                 NACRS.EDDIAG1 %like% "^T41" | NACRS.EDDIAG2 %like% "^T41" | NACRS.EDDIAG3 %like% "^T41" |
                 NACRS.EDDIAG1 %like% "^T42" | NACRS.EDDIAG2 %like% "^T42" | NACRS.EDDIAG3 %like% "^T42" |
                 NACRS.EDDIAG1 %like% "^T436" | NACRS.EDDIAG2 %like% "^T436" | NACRS.EDDIAG3 %like% "^T436"]
NACRS <- merge(x=NACRS, y=perinatal, by.x="STUDYID", by.y="studyid1")
names(NACRS) <- c("studyid", "diagnosisdate", "dx1", "dx2", "dx3", "gest_age_from_document", "BirthDate")

# Be conservative and for missing gestation age, impute 37 
NACRS$gest_age_from_document <- as.numeric(NACRS$gest_age_from_document); NACRS$gest_age_from_document[which(is.na(NACRS$gest_age_from_document))] <- 37

# Drug abuse needs to be prior to birthdate (be conservative so assume birth date is 1st of the month)
NACRS$BirthDate_imputed <- as.Date(paste("01-", NACRS$BirthDate, sep=""), "%d-%m-%Y")
NACRS <- NACRS[diagnosisdate<BirthDate_imputed]

# Drug abuse needs to be after gestation, so be conservative and impute birth date is 31 of the month
NACRS$BirthDate_imputed <- as.Date(paste("31-", NACRS$BirthDate, sep=""), "%d-%m-%Y")
NACRS$GestationDate <- NACRS$BirthDate_imputed-NACRS$gest_age_from_document*7
NACRS <- NACRS[diagnosisdate>=GestationDate] 

NACRS <- NACRS[,c("studyid", "diagnosisdate", "dx1", "dx2", "dx3", "BirthDate")]
NACRS$OpioidOverdose <- (NACRS$dx1 %like% '^T400' |  NACRS$dx1 %like% '^T401' | NACRS$dx1 %like% '^T402' | NACRS$dx1 %like% '^T403' | NACRS$dx1 %like% '^T404' | NACRS$dx1 %like% '^T406' | 
                           NACRS$dx2 %like% '^T400' |  NACRS$dx2 %like% '^T401' | NACRS$dx2 %like% '^T402' | NACRS$dx2 %like% '^T403' | NACRS$dx2 %like% '^T404' | NACRS$dx2 %like% '^T406' |
                           NACRS$dx3 %like% '^T400' |  NACRS$dx3 %like% '^T401' | NACRS$dx3 %like% '^T402' | NACRS$dx3 %like% '^T403' | NACRS$dx3 %like% '^T404' | NACRS$dx3 %like% '^T406')
NACRS$Overdose <- (NACRS$dx1 %like% '^T39' | NACRS$dx1 %like% '^T40' | NACRS$dx1 %like% '^T41' | NACRS$dx1 %like% '^T42' | NACRS$dx1 %like% '^T436' |
                     NACRS$dx2 %like% '^T39' | NACRS$dx2 %like% '^T40' | NACRS$dx2 %like% '^T41' | NACRS$dx2 %like% '^T42' | NACRS$dx2 %like% '^T436' |
                     NACRS$dx3 %like% '^T39' | NACRS$dx3 %like% '^T40' | NACRS$dx3 %like% '^T41' | NACRS$dx3 %like% '^T42' | NACRS$dx3 %like% '^T436')
NACRS$OUD <- (NACRS$dx1 %like% '^F112' | NACRS$dx2 %like% '^F112' | NACRS$dx3 %like% '^F112')
NACRS$SUD <- (NACRS$dx1 %like% '^F132' | NACRS$dx1 %like% '^F142' | NACRS$dx1 %like% '^F152' | NACRS$dx1 %like% '^F162' |
                NACRS$dx2 %like% '^F132' | NACRS$dx2 %like% '^F142' | NACRS$dx2 %like% '^F152' | NACRS$dx2 %like% '^F162' |
                NACRS$dx3 %like% '^F132' | NACRS$dx3 %like% '^F142' | NACRS$dx3 %like% '^F152' | NACRS$dx3 %like% '^F162')
NACRS$Setting <- "NACRS"

##### DAD
dad <- NULL
for(year in 2000:2016){
  tmp <- data.table(read_dta(paste("R:/working/jeffs_cleaned_files/dad", year, ".dta", sep="")))
  tmp$admission_date <- as.Date(tmp$admission_date, "%Y-%m-%d")
  tmp <- tmp[,c("admission_date", "diag1", "diag2", "diag3", "studyid")]
  tmp <- tmp[diag1 %like% "^F11" | diag2 %like% "^F11" | diag3 %like% "^F11" |
               diag1 %like% "^F13" | diag2 %like% "^F13" | diag3 %like% "^F13" |
               diag1 %like% "^F14" | diag2 %like% "^F14" | diag3 %like% "^F14" |
               diag1 %like% "^F15" | diag2 %like% "^F15" | diag3 %like% "^F15" |
               diag1 %like% "^F16" | diag2 %like% "^F16" | diag3 %like% "^F16" |
               diag1 %like% "^T39" | diag2 %like% "^T39" | diag3 %like% "^T39" |
               diag1 %like% "^T40" | diag2 %like% "^T40" | diag3 %like% "^T40" |
               diag1 %like% "^T41" | diag2 %like% "^T41" | diag3 %like% "^T41" |
               diag1 %like% "^T42" | diag2 %like% "^T42" | diag3 %like% "^T42" |
               diag1 %like% "^T436" | diag2 %like% "^T436" | diag3 %like% "^T436"|
               diag1 %like% "^304" | diag2 %like% "^304" | diag3 %like% "^304" | 
               diag1 %like% "^305" | diag2 %like% "^305" | diag3 %like% "^305" | 
               diag1 %like% "^965" | diag2 %like% "^965" | diag3 %like% "^965" | 
               diag1 %like% "^967" | diag2 %like% "^967" | diag3 %like% "^967" | 
               diag1 %like% "^970" | diag2 %like% "^970" | diag3 %like% "^970" | 
               diag1 %like% "^E850" | diag2 %like% "^E850" | diag3 %like% "^E850" | 
               diag1 %like% "^E852" | diag2 %like% "^E852" | diag3 %like% "^E852" | 
               diag1 %like% "^E855" | diag2 %like% "^E855" | diag3 %like% "^E855" | 
               diag1 %like% "^E9350" | diag2 %like% "^E9350" | diag3 %like% "^E9350" | 
               diag1 %like% "^E9351" | diag2 %like% "^E9351" | diag3 %like% "^E9351" | 
               diag1 %like% "^E9352" | diag2 %like% "^E9352" | diag3 %like% "^E9352" | 
               diag1 %like% "^E9401" | diag2 %like% "^E9401" | diag3 %like% "^E9401"]
  tmp <- tmp[!diag1 %like% "^3043" & !diag2 %like% "^3043"& !diag3 %like% "^3043"]
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  dad <- rbind(dad, tmp)
  rm(tmp)
}
for(year in c("2016-17", "2017-18", "2018-19", "2019-20", "2020-21")){
  tmp <- data.table(read_fwf(paste("R:/DATA/core-snapshot/20230217/hospital/dat/hospital", year, ".O.dat.gz", sep=""),
                             fwf_cols(admission_date=c(1,10),
                                      diag1=c(233,239),
                                      diag2=c(240,246),
                                      diag3=c(247,253),
                                      studyid=c(2786,2795))))
  tmp$admission_date <- as.Date(tmp$admission_date, "%Y-%m-%d")
  tmp <- tmp[,c("admission_date", "diag1", "diag2", "diag3", "studyid")]
  tmp <- tmp[diag1 %like% "^F11" | diag2 %like% "^F11" | diag3 %like% "^F11" |
               diag1 %like% "^F13" | diag2 %like% "^F13" | diag3 %like% "^F13" |
               diag1 %like% "^F14" | diag2 %like% "^F14" | diag3 %like% "^F14" |
               diag1 %like% "^F15" | diag2 %like% "^F15" | diag3 %like% "^F15" |
               diag1 %like% "^F16" | diag2 %like% "^F16" | diag3 %like% "^F16" |
               diag1 %like% "^T39" | diag2 %like% "^T39" | diag3 %like% "^T39" |
               diag1 %like% "^T40" | diag2 %like% "^T40" | diag3 %like% "^T40" |
               diag1 %like% "^T41" | diag2 %like% "^T41" | diag3 %like% "^T41" |
               diag1 %like% "^T42" | diag2 %like% "^T42" | diag3 %like% "^T42" |
               diag1 %like% "^T436" | diag2 %like% "^T436" | diag3 %like% "^T436"|
               diag1 %like% "^304" | diag2 %like% "^304" | diag3 %like% "^304" | 
               diag1 %like% "^305" | diag2 %like% "^305" | diag3 %like% "^305" | 
               diag1 %like% "^965" | diag2 %like% "^965" | diag3 %like% "^965" | 
               diag1 %like% "^967" | diag2 %like% "^967" | diag3 %like% "^967" | 
               diag1 %like% "^970" | diag2 %like% "^970" | diag3 %like% "^970" | 
               diag1 %like% "^E850" | diag2 %like% "^E850" | diag3 %like% "^E850" | 
               diag1 %like% "^E852" | diag2 %like% "^E852" | diag3 %like% "^E852" | 
               diag1 %like% "^E855" | diag2 %like% "^E855" | diag3 %like% "^E855" | 
               diag1 %like% "^E9350" | diag2 %like% "^E9350" | diag3 %like% "^E9350" | 
               diag1 %like% "^E9351" | diag2 %like% "^E9351" | diag3 %like% "^E9351" | 
               diag1 %like% "^E9352" | diag2 %like% "^E9352" | diag3 %like% "^E9352" | 
               diag1 %like% "^E9401" | diag2 %like% "^E9401" | diag3 %like% "^E9401"]
  tmp <- tmp[!diag1 %like% "^3043" & !diag2 %like% "^3043"& !diag3 %like% "^3043"]
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  dad <- rbind(dad, tmp)
  rm(tmp)
}
dad <- merge(x=dad[,c("studyid", "admission_date", "diag1", "diag2", "diag3")], y=perinatal, by.x="studyid", by.y="studyid1")
names(dad) <- c("studyid", "diagnosisdate", "dx1", "dx2", "dx3", "gest_age_from_document", "BirthDate")

# Be conservative and for missing gestation age, impute 37 
dad$gest_age_from_document <- as.numeric(dad$gest_age_from_document); dad$gest_age_from_document[which(is.na(dad$gest_age_from_document))] <- 37

# Drug abuse needs to be prior to birthdate (be conservative so assume birth date is 1st of the month)
dad$BirthDate_imputed <- as.Date(paste("01-", dad$BirthDate, sep=""), "%d-%m-%Y")
dad <- dad[diagnosisdate<BirthDate_imputed]

# Drug abuse needs to be after gestation, so be conservative and impute birth date is 31 of the month
dad$BirthDate_imputed <- as.Date(paste("31-", dad$BirthDate, sep=""), "%d-%m-%Y")
dad$GestationDate <- dad$BirthDate_imputed-dad$gest_age_from_document*7
dad <- dad[diagnosisdate>=GestationDate] 

dad <- dad[,c("studyid", "diagnosisdate", "dx1", "dx2", "dx3", "BirthDate")]
dad$OpioidOverdose <- (dad$dx1 %like% '^T400' |  dad$dx1 %like% '^T401' | dad$dx1 %like% '^T402' | dad$dx1 %like% '^T403' | dad$dx1 %like% '^T404' | dad$dx1 %like% '^T406' | 
                         dad$dx2 %like% '^T400' |  dad$dx2 %like% '^T401' | dad$dx2 %like% '^T402' | dad$dx2 %like% '^T403' | dad$dx2 %like% '^T404' | dad$dx2 %like% '^T406' |
                         dad$dx3 %like% '^T400' |  dad$dx3 %like% '^T401' | dad$dx3 %like% '^T402' | dad$dx3 %like% '^T403' | dad$dx3 %like% '^T404' | dad$dx3 %like% '^T406'|
                         dad$dx1 %like% '^9650' | dad$dx1 %like% '^9671' | dad$dx1 %like% '^E850' | 
                         dad$dx2 %like% '^9650' | dad$dx2 %like% '^9671' | dad$dx2 %like% '^E850' | 
                         dad$dx3 %like% '^9650' | dad$dx3 %like% '^9671' |dad$dx3 %like% '^E850')
dad$Overdose <- (dad$dx1 %like% '^T39' | dad$dx1 %like% '^T40' | dad$dx1 %like% '^T41' | dad$dx1 %like% '^T42' | dad$dx1 %like% '^T436' |
                   dad$dx2 %like% '^T39' | dad$dx2 %like% '^T40' | dad$dx2 %like% '^T41' | dad$dx2 %like% '^T42' | dad$dx2 %like% '^T436' |
                   dad$dx3 %like% '^T39' | dad$dx3 %like% '^T40' | dad$dx3 %like% '^T41' | dad$dx3 %like% '^T42' | dad$dx3 %like% '^T436'|
                   dad$dx1 %like% '^965' | dad$dx1 %like% '^967' | dad$dx1 %like% '^970' | dad$dx1 %like% '^E850' | dad$dx1 %like% '^E852' | dad$dx1 %like% '^E855' | 
                   dad$dx2 %like% '^965' | dad$dx2 %like% '^967' | dad$dx2 %like% '^970' | dad$dx2 %like% '^E850' | dad$dx2 %like% '^E852' | dad$dx2 %like% '^E855' | 
                   dad$dx3 %like% '^965' | dad$dx3 %like% '^967' | dad$dx3 %like% '^970'| dad$dx3 %like% '^E850' | dad$dx3 %like% '^E852' | dad$dx3 %like% '^E855')
dad$OUD <- (dad$dx1 %like% '^F112' | dad$dx2 %like% '^F112' | dad$dx3 %like% '^F112' | dad$dx1 %like% '^3040' | dad$dx2 %like% '^3040' | dad$dx3 %like% '^3040' | dad$dx1 %like% '^3047' | dad$dx2 %like% '^3047' | dad$dx3 %like% '^3047')
dad$SUD <- (dad$dx1 %like% '^F132' | dad$dx1 %like% '^F142' | dad$dx1 %like% '^F152' | dad$dx1 %like% '^F162' |
              dad$dx2 %like% '^F132' | dad$dx2 %like% '^F142' | dad$dx2 %like% '^F152' | dad$dx2 %like% '^F162' |
              dad$dx3 %like% '^F132' | dad$dx3 %like% '^F142' | dad$dx3 %like% '^F152' | dad$dx3 %like% '^F162' |
              dad$dx1 %like% '^304' | dad$dx2 %like% '^304' | dad$dx3 %like% '^304')
dad$Setting <- "DAD"



##### MSP
msp <- NULL
for(year in c("1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", 
              "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20")){
  print(year)
  tmp <- data.table(read_fwf(paste("R:/DATA/core-snapshot/20230217/msp/dat/msp", year, ".C.dat.gz", sep=""),
                             fwf_cols(studyid=c(308,317),
                                      servdate=c(2,9), 
                                      icd9_1=c(111,115), 
                                      icd9_2=c(116,120), 
                                      icd9_3=c(121,125))))
  tmp <- tmp[icd9_1 %like% "^F11" | icd9_2 %like% "^F11" | icd9_3 %like% "^F11" |
               icd9_1 %like% "^F13" | icd9_2 %like% "^F13" | icd9_3 %like% "^F13" |
               icd9_1 %like% "^F14" | icd9_2 %like% "^F14" | icd9_3 %like% "^F14" |
               icd9_1 %like% "^F15" | icd9_2 %like% "^F15" | icd9_3 %like% "^F15" |
               icd9_1 %like% "^F16" | icd9_2 %like% "^F16" | icd9_3 %like% "^F16" |
               icd9_1 %like% "^T39" | icd9_2 %like% "^T39" | icd9_3 %like% "^T39" |
               icd9_1 %like% "^T40" | icd9_2 %like% "^T40" | icd9_3 %like% "^T40" |
               icd9_1 %like% "^T41" | icd9_2 %like% "^T41" | icd9_3 %like% "^T41" |
               icd9_1 %like% "^T42" | icd9_2 %like% "^T42" | icd9_3 %like% "^T42" |
               icd9_1 %like% "^T436" | icd9_2 %like% "^T436" | icd9_3 %like% "^T436"|
               icd9_1 %like% "^304" | icd9_2 %like% "^304" | icd9_3 %like% "^304" | 
               icd9_1 %like% "^305" | icd9_2 %like% "^305" | icd9_3 %like% "^305" | 
               icd9_1 %like% "^965" | icd9_2 %like% "^965" | icd9_3 %like% "^965" | 
               icd9_1 %like% "^967" | icd9_2 %like% "^967" | icd9_3 %like% "^967" | 
               icd9_1 %like% "^970" | icd9_2 %like% "^970" | icd9_3 %like% "^970" | 
               icd9_1 %like% "^E850" | icd9_2 %like% "^E850" | icd9_3 %like% "^E850" | 
               icd9_1 %like% "^E852" | icd9_2 %like% "^E852" | icd9_3 %like% "^E852" | 
               icd9_1 %like% "^E855" | icd9_2 %like% "^E855" | icd9_3 %like% "^E855" | 
               icd9_1 %like% "^E9350" | icd9_2 %like% "^E9350" | icd9_3 %like% "^E9350" | 
               icd9_1 %like% "^E9351" | icd9_2 %like% "^E9351" | icd9_3 %like% "^E9351" | 
               icd9_1 %like% "^E9352" | icd9_2 %like% "^E9352" | icd9_3 %like% "^E9352" | 
               icd9_1 %like% "^E9401" | icd9_2 %like% "^E9401" | icd9_3 %like% "^E9401"]
  tmp <- tmp[!icd9_1 %like% "^3043" & !icd9_2 %like% "^3043"& !icd9_3 %like% "^3043"]
  tmp$servdate <- as.Date(as.character(tmp$servdate), "%Y%m%d")
  msp <- rbind(msp, tmp)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  rm(tmp)
}
msp <- merge(x=msp, y=perinatal, by.x="studyid", by.y="studyid1")
names(msp) <- c("studyid", "diagnosisdate", "dx1", "dx2", "dx3", "gest_age_from_document", "BirthDate")

# Be conservative and for missing gestation age, impute 37 
msp$gest_age_from_document <- as.numeric(msp$gest_age_from_document); msp$gest_age_from_document[which(is.na(msp$gest_age_from_document))] <- 37

# Drug abuse needs to be prior to birthdate (be conservative so assume birth date is 1st of the month)
msp$BirthDate_imputed <- as.Date(paste("01-", msp$BirthDate, sep=""), "%d-%m-%Y")
msp <- msp[diagnosisdate<BirthDate_imputed]

# Drug abuse needs to be after gestation, so be conservative and impute birth date is 31 of the month
msp$BirthDate_imputed <- as.Date(paste("31-", msp$BirthDate, sep=""), "%d-%m-%Y")
msp$GestationDate <- msp$BirthDate_imputed-msp$gest_age_from_document*7
msp <- msp[diagnosisdate>=GestationDate] 

msp <- msp[,c("studyid", "diagnosisdate", "dx1", "dx2", "dx3", "BirthDate")]
msp$OpioidOverdose <- (msp$dx1 %like% '^T400' |  msp$dx1 %like% '^T401' | msp$dx1 %like% '^T402' | msp$dx1 %like% '^T403' | msp$dx1 %like% '^T404' | msp$dx1 %like% '^T406' | 
                         msp$dx2 %like% '^T400' | msp$dx2 %like% '^T401' | msp$dx2 %like% '^T402' | msp$dx2 %like% '^T403' | msp$dx2 %like% '^T404' | msp$dx2 %like% '^T406' |
                         msp$dx3 %like% '^T400' | msp$dx3 %like% '^T401' | msp$dx3 %like% '^T402' | msp$dx3 %like% '^T403' | msp$dx3 %like% '^T404' | msp$dx3 %like% '^T406'|
                         msp$dx1 %like% '^9650' | msp$dx1 %like% '^9671' | msp$dx1 %like% '^E850' | 
                         msp$dx2 %like% '^9650' | msp$dx2 %like% '^9671' | msp$dx2 %like% '^E850' | 
                         msp$dx3 %like% '^9650' | msp$dx3 %like% '^9671' |msp$dx3 %like% '^E850')
msp$Overdose <- (msp$dx1 %like% '^T39' | msp$dx1 %like% '^T40' | msp$dx1 %like% '^T41' | msp$dx1 %like% '^T42' | msp$dx1 %like% '^T436' |
                   msp$dx2 %like% '^T39' | msp$dx2 %like% '^T40' | msp$dx2 %like% '^T41' | msp$dx2 %like% '^T42' | msp$dx2 %like% '^T436' |
                   msp$dx3 %like% '^T39' | msp$dx3 %like% '^T40' | msp$dx3 %like% '^T41' | msp$dx3 %like% '^T42' | msp$dx3 %like% '^T436'|
                   msp$dx1 %like% '^965' | msp$dx1 %like% '^967' | msp$dx1 %like% '^970' | msp$dx1 %like% '^E850' | msp$dx1 %like% '^E852' | msp$dx1 %like% '^E855' | 
                   msp$dx2 %like% '^965' | msp$dx2 %like% '^967' | msp$dx2 %like% '^970' | msp$dx2 %like% '^E850' | msp$dx2 %like% '^E852' | msp$dx2 %like% '^E855' | 
                   msp$dx3 %like% '^965' | msp$dx3 %like% '^967' | msp$dx3 %like% '^970' | msp$dx3 %like% '^E850' | msp$dx3 %like% '^E852' | msp$dx3 %like% '^E855')
msp$OUD <- (msp$dx1 %like% '^F112' | msp$dx2 %like% '^F112' | msp$dx3 %like% '^F112' | msp$dx1 %like% '^3040' | msp$dx2 %like% '^3040' | msp$dx3 %like% '^3040' | msp$dx1 %like% '^3047' | msp$dx2 %like% '^3047' | msp$dx3 %like% '^3047')
msp$SUD <- (msp$dx1 %like% '^F132' | msp$dx1 %like% '^F142' | msp$dx1 %like% '^F152' | msp$dx1 %like% '^F162' |
              msp$dx2 %like% '^F132' | msp$dx2 %like% '^F142' | msp$dx2 %like% '^F152' | msp$dx2 %like% '^F162' |
              msp$dx3 %like% '^F132' | msp$dx3 %like% '^F142' | msp$dx3 %like% '^F152' | msp$dx3 %like% '^F162' |
              msp$dx1 %like% '^304' | msp$dx2 %like% '^304' | msp$dx3 %like% '^304')
msp$Setting <- "MSP"


mother_drug_abuse_pregnancy <- rbind(dad, msp, NACRS)


#ICD10: %like% "F111" %like% "F112" 
#ICD10: "T400", "T401", "T402", "T403", "T404", "T406"
#ICD9: %like% "3040" %like% "3047"  %like% "3048"  %like% "3055"
#ICD9: like 9650, 9701, E850, E9350, E9351, E9352, E9401

# Is the abuse/dependence/overdose opioid-related?
mother_drug_abuse_pregnancy$OpioidRelated <- (mother_drug_abuse_pregnancy$OUD==1 | mother_drug_abuse_pregnancy$OpioidOverdose==1 | 
                                                mother_drug_abuse_pregnancy$dx1 %like% "^F111" | mother_drug_abuse_pregnancy$dx1 %like% "^3048" |  mother_drug_abuse_pregnancy$dx1 %like% "^3055" |  mother_drug_abuse_pregnancy$dx1 %like% "^E9350" | mother_drug_abuse_pregnancy$dx1 %like% "^E9351" |
                                                mother_drug_abuse_pregnancy$dx1 %like% "^E9352" |  mother_drug_abuse_pregnancy$dx1 %like% "^E9401")

perinatal <- read_fwf("R:/working/unzipped_raw_data/perinatal2000-2020.babynewborn.D.dat", 
                      fwf_cols(b_date_of_birthyyyy=c(570,573), b_date_of_birthmm=c(574,575), studyid1=c(623,632), studyid2=c(633,642)))
perinatal <- data.table(perinatal)
perinatal$BirthDate <- paste(perinatal$b_date_of_birthmm, perinatal$b_date_of_birthyyyy, sep="-")
perinatal <- perinatal[,c("studyid1", "studyid2", "BirthDate")]

mother_drug_abuse_pregnancy <- merge(x=mother_drug_abuse_pregnancy, y=unique(perinatal), by.x=c("studyid", "BirthDate"), by.y=c("studyid1", "BirthDate"))
setnames(mother_drug_abuse_pregnancy, old="studyid", new="studyid1")
mother_drug_abuse_pregnancy <- unique(mother_drug_abuse_pregnancy)

fwrite(mother_drug_abuse_pregnancy, "mother_drug_abuse_pregnancy.csv")

