### This code create cohort of all births in BC


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

motherdelivery <-  read_fwf("R:/working/unzipped_raw_data/perinatal2000-2020.motherdelivery.D.dat", 
                            fwf_cols(STUDYID=c(979,988),
                                     DELIVERY_MOTHERID=c(943,952),
                                     M_NUM_BIRTHS=c(287,289),
                                     M_AGE_NO=c(290,301),
                                     PARITY=c(333,340),
                                     M_POSTAL_CODE3=c(934,936),
                                     M_LABOUR_TYPE=c(741,753),
                                     M_MODE_DEL=c(784,792)))

perinatal <- merge(x=perinatal, y=motherdelivery, by.x=c("studyid1", "newbornbaby_motherid"), by.y=c("STUDYID", "DELIVERY_MOTHERID"), all.x=T)


# Covariates: mother's age (M_AGE_NO), FSA (M_POSTAL_CODE3), PARITY, year and month FEs
perinatal$AgeBin <- "Unknown"
perinatal$AgeBin[which(perinatal$M_AGE_NO<20)] <- "<20"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=20 & perinatal$M_AGE_NO<=24)] <- "20-24"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=25 & perinatal$M_AGE_NO<=34)] <- "25-34"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=35)] <- "35+"

# Stilbirth
perinatal$StillBirth_new <- as.numeric(perinatal$stillbirth %in% c("A", "P", "U"))

# Birthweight, LBW, VLBW
perinatal$Birthweight <- as.numeric(perinatal$b_admission_weight)
perinatal$LBW <- perinatal$Birthweight<2500
perinatal$VLBW <- perinatal$Birthweight<1500

# Apgar score
perinatal$Apgar_1min <- as.numeric(perinatal$apgar_1_minute)
perinatal$Apgar_5min <- as.numeric(perinatal$apgar_5_minutes)
perinatal$LowApgar_1min <- as.numeric(perinatal$Apgar_1min<7)
perinatal$LowApgar_5min <- as.numeric(perinatal$Apgar_5min<7)

# Pre-term birth
perinatal$final_ga <- as.numeric(perinatal$final_ga)
perinatal$PreTermBirth <- as.numeric(perinatal$final_ga<37)

# Small/Large for gestational age
perinatal <- merge(x=perinatal, y=perinatal[, .(p10_BW_ga=quantile(Birthweight, 0.1, na.rm=T), p90_BW_ga=quantile(Birthweight,0.9, na.rm=T)), by="final_ga"], by.x="final_ga", by.y="final_ga")
perinatal$SGA <- (perinatal$Birthweight<perinatal$p10_BW_ga); perinatal$LGA <- (perinatal$Birthweight>perinatal$p90_BW_ga)

# Length of baby and head circumference
perinatal$birth_length <- as.numeric(perinatal$birth_length)
perinatal$birth_head_circumference <- as.numeric(perinatal$birth_head_circumference)

# Length of stay

# C-section, induction of labor
perinatal$csection <- as.numeric(perinatal$M_MODE_DEL=="cs")
perinatal$LaborInduced <- as.numeric(perinatal$M_LABOUR_TYPE=="induced")
perinatal <- data.table(perinatal)

setnafill(perinatal, cols=c("StillBirth_new", "PreTermBirth", "csection", "LaborInduced"), fill=0)

# NICU
perinatal$nicu_ii[which(perinatal$nicu_ii==".")] <- 0; perinatal$nicu_iii[which(perinatal$nicu_iii==".")] <- 0
perinatal$nicu_ii <- as.numeric(perinatal$nicu_ii); perinatal$nicu_iii <- as.numeric(perinatal$nicu_iii); 

# Discharge to adoption or foster home
perinatal$Discharge_AdoptionFoster <- as.numeric(perinatal$discharge_to %in% c("A", "F"))

# Breastfeeding
perinatal$BreastFeedingDischarge <- as.numeric(perinatal$breast_feed_at_discharge=="Y" | perinatal$newborn_feeding %in% c("BR"))
perinatal$BreastFeedingDischarge[which(perinatal$breast_feed_at_discharge=="N" | perinatal$newborn_feeding %in% c("FR", "BF"))] <- 0
perinatal$BreastfeedWithin1H <- as.numeric(perinatal$breast_feeding_initiation=="1"); perinatal$BreastfeedWithin1H[which(perinatal$breast_feeding_initiation==".")] <- NA

# Resuscitation 
perinatal$resuscitation <- as.numeric(!perinatal$age_start_chest_compress=="." | !perinatal$age_start_ippv_mask=="." | !perinatal$age_start_ippv_ett=="." | !perinatal$age_start_oxygen==".")

# Aggregate so no duplicate newborns
perinatal <- perinatal[, .(studyid1=studyid1[1], Sex=sex[1], M_AGE_NO=M_AGE_NO[1], AgeBin=AgeBin[1], M_POSTAL_CODE3=M_POSTAL_CODE3[1], PARITY=PARITY[1], b_date_of_birthmm=b_date_of_birthmm[1], b_date_of_birthyyyy=b_date_of_birthyyyy[1],
                           StillBirth_new=max(StillBirth_new, na.rm=T), Birthweight=mean(Birthweight, na.rm=T), LBW=max(LBW, na.rm=T), VLBW=max(VLBW, na.rm=T),
                           LOS=max(LOS, na.rm=T), Apgar_1min=max(Apgar_1min, na.rm=T), Apgar_5min=max(Apgar_5min, na.rm=T), LowApgar_1min=max(LowApgar_1min, na.rm=T), LowApgar_5min=max(LowApgar_5min, na.rm=T), final_ga=max(final_ga, na.rm=T),
                           PreTermBirth=max(PreTermBirth, na.rm=T), SGA=max(SGA, na.rm=T), LGA=max(LGA, na.rm=T), birth_length=max(birth_length, na.rm=T), birth_head_circumference=max(birth_head_circumference, na.rm=T),
                           csection=max(csection, na.rm=T), LaborInduced=max(LaborInduced, na.rm=T), nicu_ii=max(nicu_ii), nicu_iii=max(nicu_iii),
                           Discharge_AdoptionFoster=max(Discharge_AdoptionFoster), BreastFeedingDischarge=max(BreastFeedingDischarge, na.rm=T), BreastfeedWithin1H=max(BreastfeedWithin1H, na.rm=T),
                           oxygen_days=max(oxygen_days), tpn_days=max(tpn_days), cpap_days=max(as.numeric(cpap_days), na.rm=T), ventilator_days=max(ventilator_days), 
                           resuscitation=max(resuscitation)),
                       by=c("studyid2")]
perinatal[perinatal<0] <- NA
setnames(perinatal, old=c("b_date_of_birthyyyy", "b_date_of_birthmm"), new=c("Year", "Month"))

##### CONCEIVED DATE
# Do not use birth year-month since endogenous
# Get date of conception; when missing impute
perinatal$BirthDate <- as.Date(paste(perinatal$Year, "-", perinatal$Month, "-15", sep=""), "%Y-%m-%d")
perinatal$ConceivedDate <- perinatal$BirthDate-perinatal$final_ga*7
perinatal$ConceivedDate[which(is.na(perinatal$ConceivedDate))] <- perinatal$BirthDate[which(is.na(perinatal$ConceivedDate))]-40*7
perinatal$ConceivedYear <- format(perinatal$ConceivedDate, "%Y")
perinatal$ConceivedYearQuarter <- paste(format(perinatal$ConceivedDate, "%Y"), floor(as.numeric(format(perinatal$ConceivedDate, "%m"))/4)+1, sep="-")
perinatal$ConceivedYearMonth <- format(perinatal$ConceivedDate, "%Y-%m")
perinatal$ConceivedDate <- as.Date(paste(perinatal$ConceivedYearMonth, "-01", sep=""), "%Y-%m-%d")

##### Different definitions of NAS and exposed cohorts
newborn <- fread("perinatal_dx_newborn_mother.csv")
questionnaire <- fread("substance_use_questionnaire_delivery.csv")
twomonths <- fread("newborn_dx_2months.csv")
pregnancy <- fread("mother_drug_abuse_pregnancy.csv")

rx <- fread("all_mothers_opioid_rx.gz")
allopioids <- data.table(allopioids)
MOUD <- allopioids[HP.GEN_DRUG %like% 'METHADONE'|HP.GEN_DRUG %like% 'BUPRENOR'|HP.GEN_DRUG %like% 'SUBOXONE'|HP.GEN_DRUG %like% 'NALTREX'|HP.DRUG_BRAND_NM %like% 'METHADONE'|HP.DRUG_BRAND_NM %like% 'BUPRENOR'|HP.DRUG_BRAND_NM %like% 'SUBOXONE'|HP.DRUG_BRAND_NM %like% 'NALTREX']
MOUD <- MOUD[!HP.GEN_DRUG %like% 'NORMETHADONE' & !HP.DRUG_BRAND_NM %like% 'NORMETHADONE' & !HP.DRUG_BRAND_NM %like% 'PAIN']
allopioids <- allopioids[!DINPIN %in% unique(MOUD$DINPIN)]

## All MOUD during pregnancy
MOUD <- merge(x=MOUD, y=perinatal[,c("studyid1", "studyid2", "ConceivedDate", "BirthDate")], by.x="studyid", by.y="studyid1")
MOUD <- unique(MOUD$studyid2[which(MOUD$ServiceDate>=MOUD$ConceivedDate & MOUD$ServiceDate<MOUD$BirthDate)])

## All opioids during pregnancy
allopioids <- merge(x=allopioids, y=perinatal[,c("studyid1", "studyid2", "ConceivedDate", "BirthDate")], by.x="studyid", by.y="studyid1")
allopioids <- allopioids[ServiceDate>=ConceivedDate & ServiceDate<BirthDate]
opioiduser <- unique(allopioids$studyid2)
opioidrx <- allopioids[,.(DaysSupply=sum(DaysSupply), N_RX=length(DINPIN)), by="studyid2"]
opioidabuser <- unique(opioidrx$studyid2[which(opioidrx$N_RX>=3)])

A1 <- unique(newborn$studyid2[which(newborn$NAS==1)])
A2 <- unique(twomonths$studyid2[which(twomonths$NAS==1)])
B1 <- unique(newborn$studyid2[which(newborn$Newborn_AffectedByDrugs==1)])
B2 <- unique(twomonths$studyid2[which(twomonths$Newborn_AffectedByDrugs==1)])
C1 <- unique(newborn$studyid2[which(newborn$Newborn_AffectedOtherMedication==1)])
C2 <- unique(twomonths$studyid2[which(twomonths$Newborn_AffectedOtherMedication==1)])
D1 <- unique(newborn$studyid2[which(newborn$MaternalOpioidAbuse_Delivery==1)])
D3 <- unique(pregnancy$studyid2[which(pregnancy$OpioidRelated==1)]) #Everything here is at least abuse
E1 <- unique(newborn$studyid2[which(newborn$MaternalDrugAbuse_Delivery==1)])
E3 <- unique(pregnancy$studyid2) #Everything here is at least abuse
F3 <- opioiduser
F4 <- unique(questionnaire$studyid2[which(questionnaire$R_HEROIN==1)]) #R_DRUG_FLG: Drug use during pregnancy identified as a risk; R_RX: Substance use during pregnancy: prescription drugs
G4 <- unique(questionnaire$studyid2[which(questionnaire$R_DRUGS_FLG==1 | questionnaire$R_RX==1 | questionnaire$R_HEROIN==1 | questionnaire$R_COCAINE==1 | questionnaire$R_METHADONE==1)]) #R_DRUG_FLG: Drug use during pregnancy identified as a risk; R_RX: Substance use during pregnancy: prescription drugs


# NAS at delivery only
perinatal$NewbornNAS <- perinatal$studyid2 %in% c(A1)

# + NAS and NAS adjacent within 2 months
perinatal$NewbornNASAdjacent <- perinatal$studyid2 %in% c(A2,B1,B2) 

# + Mother drug abuse at delivery
perinatal$MotherDrugAbuseDelivery <- perinatal$studyid2 %in% c(D1,E1)

# + Mother drug abuse during pregnancy (incl. MOUD)
perinatal$MotherDrugAbusePregnancy <- perinatal$studyid2 %in% c(D3,E3,MOUD)

# + Mother admits drug use during delivery
perinatal$MotherAdmitDrugUseDelivery <- perinatal$studyid2 %in% c(F4,G4)

# + Mother abused opioids
perinatal$MotherAbusedOpioids <- perinatal$studyid2 %in% c(opioidabuser)

perinatal$Measure1 <- perinatal$NewbornNAS
perinatal$Measure2 <- pmax(perinatal$NewbornNAS, perinatal$NewbornNASAdjacent)
perinatal$Measure3 <- pmax(perinatal$NewbornNAS, perinatal$NewbornNASAdjacent, perinatal$MotherDrugAbuseDelivery)
perinatal$Measure4 <- pmax(perinatal$NewbornNAS, perinatal$NewbornNASAdjacent, perinatal$MotherDrugAbuseDelivery, perinatal$MotherDrugAbusePregnancy)
perinatal$Measure5 <- pmax(perinatal$NewbornNAS, perinatal$NewbornNASAdjacent, perinatal$MotherDrugAbuseDelivery, perinatal$MotherDrugAbusePregnancy, perinatal$MotherAdmitDrugUseDelivery)
perinatal$Measure6 <- pmax(perinatal$NewbornNAS, perinatal$NewbornNASAdjacent, perinatal$MotherDrugAbuseDelivery, perinatal$MotherDrugAbusePregnancy, perinatal$MotherAdmitDrugUseDelivery, perinatal$MotherAbusedOpioids)


births_vital <- NULL
for(year in 1999:2021){
  tmp <- data.table(read_fwf(paste("R:/Data/core-snapshot/20230217/vital_events/births/births", year, ".C.dat.gz", sep=""),
                             fwf_cols(Married=c(46,46),
                                      Sex_vital=c(47,47),
                                      N_Preg=c(52,53),
                                      M_Country=c(104,153),
                                      HospID=c(36,45),
                                      F_Age=c(273,275),
                                      N_Newborn_This_Birth=c(389,389),
                                      studyid=c(425,434),
                                      fatherid=c(445,454))))
  births_vital <- rbind(births_vital, tmp)
}

perinatal <- merge(x=perinatal, y=births_vital, by.x="studyid2", by.y="studyid", all.x=T)

##### Sex of child
perinatal$Sex[which(perinatal$Sex %in% c("U", "O") | is.na(perinatal$Sex))] <- perinatal$Sex_vital[which(perinatal$Sex %in% c("U", "O") | is.na(perinatal$Sex))]

##### MOTHER'S/FATHER'S AGE
perinatal$M_AGE <- floor(perinatal$M_AGE_NO)
perinatal$AgeBin <- "Unknown"
perinatal$AgeBin[which(perinatal$M_AGE_NO<20)] <- "<20"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=20 & perinatal$M_AGE_NO<25)] <- "20-24"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=25 & perinatal$M_AGE_NO<30)] <- "25-29"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=30 & perinatal$M_AGE_NO<35)] <- "30-34"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=35 & perinatal$M_AGE_NO<40)] <- "35-39"
perinatal$AgeBin[which(perinatal$M_AGE_NO>=40)] <- "40+"


perinatal$F_Age[which(perinatal$F_Age<13|perinatal$F_Age>80)] <- NA
perinatal$F_AgeBin <- "Unknown"
perinatal$F_AgeBin[which(perinatal$F_Age<20)] <- "<20"
perinatal$F_AgeBin[which(perinatal$F_Age>=20 & perinatal$F_Age<=24)] <- "20-24"
perinatal$F_AgeBin[which(perinatal$F_Age>=25 & perinatal$F_Age<=34)] <- "25-34"
perinatal$F_AgeBin[which(perinatal$F_Age>=35 & perinatal$F_Age<=44)] <- "35-44"
perinatal$F_AgeBin[which(perinatal$F_Age>=45)] <- "45+"


##### Delivering doctor (pediatrician, obgyn, GP, midwife)
doctors_baby <- read_fwf("R:/DATA/core-snapshot/20230217/perinatal/dat/perinatal2000-2020.pdrdoctors_baby_nb.B.dat.gz", 
                         fwf_cols(studyid2=c(37,46),
                                  DOCTOR_SERVICE=c(11,17),
                                  DOCTOR_TYPE=c(18,20)))

doctors_baby <- data.table(doctors_baby)
doctors_baby <- doctors_baby[DOCTOR_TYPE=="M", c("studyid2", "DOCTOR_SERVICE")] # M is main doctor
doctors_baby$DOCTOR_SERVICE <- as.numeric(doctors_baby$DOCTOR_SERVICE)
doctors_baby <- unique(doctors_baby, by="studyid2")

perinatal <- merge(x=perinatal, y=doctors_baby, by.x="studyid2", by.y="studyid2", all.x=T)
perinatal$DoctorType <- "Other"
perinatal$DoctorType[which(perinatal$DOCTOR_SERVICE %in% c(20,21,22,23,24,25,26,27,28,61,63,65))] <- "Paediatrician"
perinatal$DoctorType[which(perinatal$DOCTOR_SERVICE %in% c(1))] <- "GP"
perinatal$DoctorType[which(perinatal$DOCTOR_SERVICE %in% c(50,51,52,54,97))] <- "OBGyn"
perinatal$DoctorType[which(perinatal$DOCTOR_SERVICE %in% c(11004))] <- "Midwife"

##### Diagnoses of baby
baby_dx <- read_fwf("R:/DATA/core-snapshot/20230217/perinatal/dat/perinatal2000-2020.pdrdiagnoses_baby_nb.B.dat.gz", 
                    fwf_cols(studyid2=c(44,53),
                             DIAGNOSIS_CD=c(14,24),
                             DIAGNOSIS_TYPE=c(25,27)))
baby_dx <- data.table(baby_dx)
baby_dx <- baby_dx[!substring(DIAGNOSIS_CD,1,3)=="Z38" & !substring(DIAGNOSIS_CD,1,2)=="V3"]

perinatal$AbnormalNewbornDx <- (perinatal$studyid2 %in% baby_dx$studyid2) # Could be more specific in the future


##### GEOGRAPHIC VARIABLE
# Get geographic variable the year prior to birth
# DAIPPE splits each FSA into ten decides based on dissemination area
# QAIPPE splits into quintiles

perinatal$YearLag1 <- perinatal$Year-1

geographic <- NULL
for(year in 1999:2013){
  tmp <- data.table(read_fwf(paste("R:/Data/core-snapshot/20230217/dip_census_geodata/dat/calendar/dip_census_geodata", year, ".C.dat.gz", sep=""),
                             fwf_cols(studyid=c(1,10), PostalCode=c(16,24), FSA=c(12,14), CensusTract=c(71,94), DAIPPE=c(122,123), QAIPPE=c(118,118))))
  tmp$Year <- year
  tmp <- merge(x=tmp, y=perinatal[,c("studyid1", "YearLag1")], by.x=c("studyid", "Year"), by.y=c("studyid1", "YearLag1"))
  geographic <- rbind(geographic, tmp)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
}
for(year in 2014:2019){
  tmp <- data.table(read_fwf(paste("R:/Data/core-snapshot/20230217/dip_census_geodata/dat/calendar/dip_census_geodata", year, ".D.dat.gz", sep=""),
                             fwf_cols(studyid=c(1,10), PostalCode=c(16,24), FSA=c(12,14), CensusTract=c(65,88), DAIPPE=c(134,135), QAIPPE=c(130,130)))) ##QAB is before tax, QAA is after tax
  tmp$Year <- year
  tmp <- merge(x=tmp, y=perinatal[,c("studyid1", "YearLag1")], by.x=c("studyid", "Year"), by.y=c("studyid1", "YearLag1"))
  geographic <- rbind(geographic, tmp)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
}
geographic <- unique(geographic, by=c("studyid", "Year"))
perinatal <- merge(x=perinatal, y=geographic[,c("studyid", "Year", "PostalCode", "FSA", "CensusTract", "DAIPPE", "QAIPPE")], by.x=c("studyid1", "YearLag1"), by.y=c("studyid", "Year"), all.x=T)
perinatal$HospID[which(perinatal$HospID %in% c("0", "NA"))] <- NA

# When FSA last year is missing, use FSA at birth
perinatal$FSA[which(is.na(perinatal$FSA))] <- perinatal$M_POSTAL_CODE3[which(is.na(perinatal$FSA))]


##### PARITY
perinatal$Parity <- NA
perinatal$Parity[which(perinatal$N_Preg %in% c(0,1))] <- "1"
perinatal$Parity[which(perinatal$N_Preg==2)] <- "2"
perinatal$Parity[which(perinatal$N_Preg>=3)] <- "3+"


##### ABORIGINAL STATUS
# Per Gaelle's email on 4-4-2023: Bill recommends crosstabbing with groupid
# This two digit groupid is "09" but I don't think this approach works by itself (too many false negatives since it only has 150,000 or so)
enrollment <-  read_fwf("R:/DATA/core-snapshot/20230217/ido_med/dat/med1991-2020.stulvlcb_ft_schlstud.C.dat.gz", 
                        fwf_cols(STUDYID=c(1310,1319),
                                 ABORIGINAL_THIS_COLL_FLAG=c(510,525),                       	
                                 ABORIGINAL_EVER_FLAG=c(526,541),
                                 BAND_RESIDENCY_STATUS=c(1052)))
aboriginal_kids <- unique(enrollment$STUDYID[which(enrollment$ABORIGINAL_THIS_COLL_FLAG=="Aboriginal" | enrollment$ABORIGINAL_EVER_FLAG=="Aboriginal" | enrollment$BAND_RESIDENCY_STATUS=="B")])
rm(enrollment)

aboriginal_imputed <- NULL
for(year in 1992:2019){
  tmp <-  data.table(read_fwf(paste("R:/DATA/core-snapshot/20230217/rpblite/dat/calendar/rpblite", year, ".D.dat.gz", sep=""),
                              fwf_cols(STUDYID=c(78,87), GROUPID=c(30,36))))
  tmp <- data.table(tmp)
  aboriginal_imputed <- c(aboriginal_imputed, tmp$STUDYID[which(substring(tmp$GROUPID, 1, 2)=="09")])
}
aboriginal_imputed <- unique(aboriginal_imputed)

# Actually doesn't make a huge different using just enrollment or including the "09" groupid: Aboriginal parents go from 122K to 140K
perinatal$AboriginalParents <- (perinatal$studyid1 %in% aboriginal_imputed | perinatal$studyid1 %in% aboriginal_kids |
                                  perinatal$studyid2 %in% aboriginal_imputed | perinatal$studyid2 %in% aboriginal_kids |
                                  perinatal$fatherid %in% aboriginal_imputed | perinatal$fatherid %in% aboriginal_kids)


##### MOTHER BIRTH COUNTRY/BORN IN CANADA?
perinatal$MotherBornCanada <- (perinatal$M_Country=="CANADA")

perinatal$AboriginalCanadaImmigrant <- NA
perinatal$AboriginalCanadaImmigrant[which(perinatal$AboriginalParents==1)] <- "Aboriginal"
perinatal$AboriginalCanadaImmigrant[which(is.na(perinatal$AboriginalCanadaImmigrant) & perinatal$M_Country=="CANADA")] <- "Canada/NonAboriginal"
perinatal$AboriginalCanadaImmigrant[which(is.na(perinatal$AboriginalCanadaImmigrant) & !is.na(perinatal$M_Country))] <- "Immigrant"

# Basic cleaning
perinatal$M_Country[which(perinatal$M_Country %like% "CHINA" | perinatal$M_Country %like% "HONG KONG" | perinatal$M_Country %like% "TAIWAN" | perinatal$M_Country %like% "MACAU")] <- "CHINA/HK/TW"
perinatal$M_Country[which(perinatal$M_Country %like% "VIET")] <- "VIETNAM"
perinatal$M_Country[which(perinatal$M_Country %in% c("USA", "UNITED STATES"))] <- "USA"
perinatal$M_Country[which(perinatal$M_Country %in% c("ENGLAND", "UNITED KINGDOM", "NORTHERN IRELAND", "WALES", "SCOTLAND"))] <- "UK"
perinatal$M_Country[which(perinatal$M_Country %like% "KOREA")] <- "KOREA"
perinatal$M_Country[which(perinatal$M_Country %like% "GERMANY")] <- "GERMANY"

# Save top 10, other, and Canada
perinatal$TopImmigrantCountries <- NA
perinatal$TopImmigrantCountries[which(perinatal$M_Country %in% names(table(perinatal$M_Country)[order(table(perinatal$M_Country), decreasing=T)][2:11]))] <- perinatal$M_Country[which(perinatal$M_Country %in% names(table(perinatal$M_Country)[order(table(perinatal$M_Country), decreasing=T)][2:11]))]
perinatal$TopImmigrantCountries[which(!perinatal$M_Country %in% names(table(perinatal$M_Country)[order(table(perinatal$M_Country), decreasing=T)][2:11]))] <- "Other"
perinatal$TopImmigrantCountries[which(perinatal$M_Country=="CANADA")] <- "Canada"

# Categorize countries of immigration (INCOMPLETE, TOO TIME CONSUMING AND TEDIOUS FOR NOW)
perinatal$WorldRegionOfBirth <- NA
perinatal$WorldRegionOfBirth[which(perinatal$M_Country %in% c("CANADA", "USA"))] <- "CANADA/USA"
perinatal$WorldRegionOfBirth[which(perinatal$M_Country %in% c("INDIA", "PHILIPPINES", "VIETNAM", "HONG KONG", "TAIWAN", "JAPAN", "CHINA/HK/TW", "KOREA", "LAOS", "NEPAL", "PAKISTAN", "AFGHANISTAN", "MALAYSIA", "SRI LANKA", "SINGAPORE", "UZBEKISTAN", "MONGOLIA", "BURMA", "MYANMAR", "THAILAND", "INDONESIA", "BANGLADESH", "KAZAKHSTAN", "CAMBODIA", "AZERBAIJAN"))] <- "Asia"
perinatal$WorldRegionOfBirth[which(perinatal$M_Country %in% c("UK", "FRANCE", "GERMANY", "RUSSIA", "SPAIN", "POLAND", "HUNGARY", "CROATIA", "KOSOVO", "BOSNIA AND HERZEGOVINA", "CZECH REPUBLIC", "BELARUS", "ROMANIA", "UKRAINE", "IRELAND", "USSR", "LATVIA", "NETHERLANDS", "ICELAND", "NORWAY", "SLOVAKIA", "SWITZERLAND", "YUGOSLAVIA", "ITALY", "SWEDEN", "PORTUGAL", "CZECHOSLOVAKIA", "GREECE", "ALBANIA", "BELGIUM", "AUSTRIA", "FINLAND", "BOSNIA", "ESTONIA", "DENMARK", "SERBIA", "BULGARIA"))] <- "Europe"
perinatal$WorldRegionOfBirth[which(perinatal$M_Country %in% c("IRAN", "IRAQ", "SYRIA", "TURKEY", "SOUTH AFRICA", "SAUDI ARABIA", "NIGERIA", "ETHIOPIA", "KENYA", "IVORY COAST", "NOZAMBIQUE", "GUINEA", "ALGERIA", "NAMIBIA", "EGYPT", "LIBYA", "UNITED ARAB EMIRATES", "PALESTINE", "LEBANON", "SOMALIA", "ISRAEL", "SUDAN", "ERITREA", "AFRICA",  "ZAMBIA", "LIBERIA", "DEMOCRATIC REPUBLIC OF CONGO", "CONGO", "TUNISIA", "MOROCCO", "UGANDA", "GHANA", "KUWAIT", "SENEGAL", "SIERRA LEONE", "SOUTH SUDAN", "RWANDA", "QATAR", "TANZANIA", "ZIMBABWE", "JORDAN", "CAMEROON"))] <- "Africa and Middle East"
perinatal$WorldRegionOfBirth[which(perinatal$M_Country %in% c("MEXICO", "GUATEMALA", "PERU", "COLOMBIA", "CHILE", "ARGENTINA", "BRAZIL", "BAHAMAS", "EL SALVADOR", "PANAMA", "URUGUAY", "SWAZILAND", "HAITI", "JAMAICA", "PUERTO RICO", "NICARAGUA", "VENEZUELA", "PARAGUAY", "GUYANA", "HONDURAS", "ECUADOR", "TRINIDAD & TOBAGO", "BARBADOS"))] <- "Central and South America"
perinatal$WorldRegionOfBirth[which(perinatal$M_Country %in% c("FIJI", "AUSTRALIA", "NEW ZEALAND", "FIJI ISLAND", "PAPUA NEW GUINEA"))] <- "OCEANIA"



##### MOTHER MARITAL STATUS
perinatal$Married[which(is.na(perinatal$Married))] <- "U"


##### BENEFITS
cases <-  rbind(data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_1989_2017/dat/idosdpr1989-2017.bceacases.B.dat.gz", 
                                    fwf_cols(ym=c(1,8), fileid=c(9,18), program=c(19,21), pay=c(43,52)))),
                data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2018_2019/dat/idosdpr2018-2019.bceacases.B.dat.gz", 
                                    fwf_cols(ym=c(1,8), fileid=c(9,18), program=c(19,21), pay=c(43,52)))),
                data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2020/dat/idosdpr2020.bceacases.B.dat.gz", 
                                    fwf_cols(ym=c(1,8), fileid=c(9,18), program=c(19,21), pay=c(43,52)))))
cases <- cases[substring(ym, 1, 4)>=1998]

involvement <-  rbind(data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_1989_2017/dat/idosdpr1989-2017.bceainvolvement.A.dat.gz", 
                                          fwf_cols(ym=c(1,8), fileid=c(9,18), studyid=c(69,78)))),
                      data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2018_2019/dat/idosdpr2018-2019.bceainvolvement.A.dat.gz", 
                                          fwf_cols(ym=c(1,8), fileid=c(9,18), studyid=c(69,78)))),
                      data.table(read_fwf("R:/DATA/core-snapshot/20230217/ido_sdprV23_2020/dat/idosdpr2020.bceainvolvement.A.dat.gz", 
                                          fwf_cols(ym=c(1,8), fileid=c(9,18), studyid=c(69,78)))))
involvement <- involvement[substring(ym, 1, 4)>=1998]

involvement <- involvement[studyid %in% perinatal$studyid1]
benefits <- merge(x=cases, y=involvement, by.x=c("fileid", "ym"), by.y=c("fileid", "ym"))
benefits <- unique(benefits)
benefits <- benefits[, .(Benefits=sum(pay)), by=c("studyid", "ym")]
benefits$Year <- substring(benefits$ym, 1, 4)
benefits <- benefits[, .(BenefitsMo_lag1=mean(Benefits)), by=c("studyid", "Year")]

# Using BC's CPI
benefits$BenefitsMo_lag1[which(benefits$Year==1998)] <- benefits$BenefitsMo_lag1[which(benefits$Year==1998)]*132.4/93.4
benefits$BenefitsMo_lag1[which(benefits$Year==1999)] <- benefits$BenefitsMo_lag1[which(benefits$Year==1999)]*132.4/94.4
benefits$BenefitsMo_lag1[which(benefits$Year==2000)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2000)]*132.4/96.1
benefits$BenefitsMo_lag1[which(benefits$Year==2001)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2001)]*132.4/97.7
benefits$BenefitsMo_lag1[which(benefits$Year==2002)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2002)]*132.4/100
benefits$BenefitsMo_lag1[which(benefits$Year==2003)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2003)]*132.4/102.2
benefits$BenefitsMo_lag1[which(benefits$Year==2004)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2004)]*132.4/104.2
benefits$BenefitsMo_lag1[which(benefits$Year==2005)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2005)]*132.4/106.3
benefits$BenefitsMo_lag1[which(benefits$Year==2006)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2006)]*132.4/108.1
benefits$BenefitsMo_lag1[which(benefits$Year==2007)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2007)]*132.4/110
benefits$BenefitsMo_lag1[which(benefits$Year==2008)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2008)]*132.4/112.3
benefits$BenefitsMo_lag1[which(benefits$Year==2009)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2009)]*132.4/112.3
benefits$BenefitsMo_lag1[which(benefits$Year==2010)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2010)]*132.4/113.8
benefits$BenefitsMo_lag1[which(benefits$Year==2011)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2011)]*132.4/116.5
benefits$BenefitsMo_lag1[which(benefits$Year==2012)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2012)]*132.4/117.8
benefits$BenefitsMo_lag1[which(benefits$Year==2013)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2013)]*132.4/117.7
benefits$BenefitsMo_lag1[which(benefits$Year==2014)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2014)]*132.4/118.9
benefits$BenefitsMo_lag1[which(benefits$Year==2015)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2015)]*132.4/120.2
benefits$BenefitsMo_lag1[which(benefits$Year==2016)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2016)]*132.4/122.4
benefits$BenefitsMo_lag1[which(benefits$Year==2017)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2017)]*132.4/125.0
benefits$BenefitsMo_lag1[which(benefits$Year==2018)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2018)]*132.4/128.4
benefits$BenefitsMo_lag1[which(benefits$Year==2019)] <- benefits$BenefitsMo_lag1[which(benefits$Year==2019)]*132.4/131.4


perinatal <- merge(x=perinatal, y=benefits, by.x=c("studyid1", "ConceivedYear"), by.y=c("studyid", "Year"), all.x=T)
perinatal$BenefitsMo_lag1[which(is.na(perinatal$BenefitsMo_lag1))] <- 0


##### PROXIES FOR INCOME AND POVERTY (SES)
# Variables about MSP and insurance coverage
registry <- NULL
for(year in 1999:2020){
  tmp <- data.table(read_fwf(paste("R:/Data/core-snapshot/20230217/registry/dat/calendar/registry", year, ".E.dat.gz", sep=""),
                             fwf_cols(studyid=c(121,130),
                                      MSP_ELIG_DAYS=c(25,27),
                                      PremiumAssistance=c(69,69),
                                      SupplementalBenefits=c(76,76),
                                      Pharmacare1=c(77,77),
                                      Pharmacare2=c(78,78),
                                      Pharmacare3=c(79,79),
                                      Pharmacare4=c(80,80),
                                      Pharmacare5=c(81,81),
                                      Pharmacare6=c(82,82),
                                      LHA=c(89,91),
                                      Year=c(105,108))))
  tmp <- tmp[studyid %in% perinatal$studyid1]
  tmp$PharmacareAny <- (tmp$Pharmacare1=="Y"|tmp$Pharmacare2=="Y"|tmp$Pharmacare3=="Y"|tmp$Pharmacare4=="Y"|tmp$Pharmacare5=="Y"|tmp$Pharmacare6=="Y")
  tmp <- tmp[,c("studyid", "Year", "MSP_ELIG_DAYS", "PremiumAssistance", "SupplementalBenefits", "PharmacareAny")]
  registry <- rbind(registry, tmp)
}

perinatal <- merge(x=perinatal, y=registry, by.x=c("studyid1", "YearLag1"), by.y=c("studyid", "Year"), all.x=T)
fwrite(perinatal, "allbirths.csv")
