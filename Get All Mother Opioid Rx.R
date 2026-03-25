# This code gets ALL opioid (excl. methadone and buprenorphine) prescription records between 1999-2020 for mothers who give birth between 2000-2020
# NO relative time restrictions.
# Output: all_mothers_opioid_rx.csv

perinatal <- read_fwf("R:/working/unzipped_raw_data/perinatal2000-2020.babynewborn.D.dat", 
                      fwf_cols(gest_age_from_document=c(209,212), b_date_of_birthyyyy=c(570,573), b_date_of_birthmm=c(574,575), studyid1=c(623,632)))
perinatal <- data.table(perinatal)
perinatal$BirthDate <- paste(perinatal$b_date_of_birthmm, perinatal$b_date_of_birthyyyy, sep="-")
perinatal <- perinatal[,c("studyid1", "gest_age_from_document", "BirthDate")]

din <-  read_fwf("R:/working/unzipped_raw_data/pharmanet_health-products.A.dat", 
                 fwf_cols(HP.DIN_PIN=c(1,10),
                          HP.DRUG_BRAND_NM=c(11,74),
                          HP.GEN_DRUG=c(75,114),
                          HP.GCN_SEQ_NUM=c(115,134),
                          HP.GEN_DRUG_STRGTH_VAL=c(135,144),
                          HP.UNIT_OF_MSR=c(145,159),
                          HP.GEN_DSG_FORM_CD=c(160,161),
                          HP.GEN_DSG_FORM=c(162,181),
                          HP.AHFS_3_CD=c(182,191),
                          HP.TC5_CD=c(192,211),
                          Linefeed=c(212,212)))
din <- data.table(din)
opioid_din <- din[HP.AHFS_3_CD=="280808"|
                    
                    HP.GEN_DRUG %like% "HYDROCODONE"|HP.GEN_DRUG %like% "MORPHINE"|HP.GEN_DRUG %like% "OXYCODONE"|HP.GEN_DRUG %like% "TRAMADOL"|HP.GEN_DRUG %like% "FENTANYL"|HP.GEN_DRUG %like% "HEROIN"|HP.GEN_DRUG %like% "CODEINE"|HP.GEN_DRUG %like% "HYDROMORPHONE"|HP.GEN_DRUG %like% 'MERERIDINE'|
                    
                    HP.DRUG_BRAND_NM %like% "ABSTRAL"|HP.DRUG_BRAND_NM %like% "ACTIQ"|HP.DRUG_BRAND_NM %like% "AVINZA"|HP.DRUG_BRAND_NM %like% "DEMEROL"|HP.DRUG_BRAND_NM %like% "DILAUDID"|
                    HP.DRUG_BRAND_NM %like% "DOLOPHINE"|HP.DRUG_BRAND_NM %like% "DURAGESIC"|HP.DRUG_BRAND_NM %like% "FENTORA"|HP.DRUG_BRAND_NM %like% "HYSINGLA"|HP.DRUG_BRAND_NM %like% "MORPHABOND"|
                    HP.DRUG_BRAND_NM %like% "NUCYNTA"|HP.DRUG_BRAND_NM %like% "ONSOLIS"|HP.DRUG_BRAND_NM %like% "ORAMORPH"|HP.DRUG_BRAND_NM %like% "OXAYDO"|HP.DRUG_BRAND_NM %like% "ROXANOL"|
                    HP.DRUG_BRAND_NM %like% "SUBLIMAZE"|HP.DRUG_BRAND_NM %like% "XTAMPZA"|HP.DRUG_BRAND_NM %like% "ZOHYDRO"|HP.DRUG_BRAND_NM %like% "ANEXSIA"|HP.DRUG_BRAND_NM %like% "EMBEDA"|
                    HP.DRUG_BRAND_NM %like% "EXALGO"|HP.DRUG_BRAND_NM %like% "HYCET"|HP.DRUG_BRAND_NM %like% "HYDROMET"|HP.DRUG_BRAND_NM %like% "IBUDONE"|HP.DRUG_BRAND_NM %like% "KADIAN"|HP.DRUG_BRAND_NM %like% "LIQUICET"|
                    HP.DRUG_BRAND_NM %like% "LORCET"|HP.DRUG_BRAND_NM %like% "LORTAB"|HP.DRUG_BRAND_NM %like% "MAXIDONE"|HP.DRUG_BRAND_NM %like% "OPANA"|HP.DRUG_BRAND_NM %like% "OXYCONTIN"|HP.DRUG_BRAND_NM %like% "OXYCET"|
                    HP.DRUG_BRAND_NM %like% "PALLADONE"|HP.DRUG_BRAND_NM %like% "PERCOCET"|HP.DRUG_BRAND_NM %like% "PERCODAN"|HP.DRUG_BRAND_NM %like% "REPREXAIN"|HP.DRUG_BRAND_NM %like% "REZIRA"|HP.DRUG_BRAND_NM %like% "ROXICET"|
                    HP.DRUG_BRAND_NM %like% "TARGINIQ"|HP.DRUG_BRAND_NM %like% "TUSSICAPS"|HP.DRUG_BRAND_NM %like% "TUSSIONEX"|HP.DRUG_BRAND_NM %like% "TUZISTRA"|HP.DRUG_BRAND_NM %like% "VICODIN"|HP.DRUG_BRAND_NM %like% "VICOPROFEN"|
                    HP.DRUG_BRAND_NM %like% "VITUZ"|HP.DRUG_BRAND_NM %like% "XARTEMIS"|HP.DRUG_BRAND_NM %like% "ZOLVIT"|HP.DRUG_BRAND_NM %like% "ZUTRIPRO"|HP.DRUG_BRAND_NM %like% "ZYDONE"]
opioid_din$HP.DIN_PIN <- as.numeric(opioid_din$HP.DIN_PIN)


# Dispensing events are recoreds that are NOT tied to a claim
dispense <-  read_fwf("R:/working/unzipped_raw_data/pharmanet_dispensation_pre_2020.A.dat", 
                      fwf_cols(DE.STUDYID=c(1,10),
                               DE.FCTY_LABEL=c(125,149),
                               DE_FC_GEO.FCTY_HA_AREA_CD=c(150,159),	
                               DE_FC_GEO.FCTY_HSDA=c(160,199),
                               DE_FC_GEO.FCTY_LHA=c(200,239),
                               DE_FC_GEO.FCTY_PSTL_CD=c(240,242),
                               DE_PRAC.PRAC_IDNT=c(243,247),
                               DE_PRAC.PRAC_BLLG_NUM=c(248,252),
                               DE_PR_GEO.PRSCR_PRAC_HA_AREA_CD=c(253,262),	
                               DE_PR_GEO.PRSCR_PRAC_HSDA_CD=c(263,302),
                               DE_PR_GEO.PRSCR_PRAC_LHA=c(303,342),
                               DE_PR_GEO.PRSCR_PRAC_PSTL_CD=c(343,345),
                               DE_PRAC.PRAC_LIC_BODY_IDNT=c(346,355),
                               DE_PR_INFO.PRSCR_SPTY_FLG=c(356,356),
                               DE_PR_PRAC.PRSCR_PRAC_PROF=c(377,401),
                               DE_PR_INFO.RCNT_BLLG_SPTY_1_LABEL=c(402,411),
                               DE_PR_INFO.RCNT_BLLG_SPTY_2_LABEL=c(412,421),
                               DE.HLTH_PROD_LABEL=c(422,431),
                               DE.SRV_DATE=c(432,441), 
                               DE.DSPD_QTY=c(442,451),
                               DE.DSPD_DAYS_SPLY=c(452,461),
                               Linefeed=c(462,462)))
dispense <- data.table(dispense)
dispense <- dispense[substring(DE.SRV_DATE, 1,4)>=1999]
dispense <- dispense[DE.STUDYID %in% perinatal$studyid1]
dispense$DE.HLTH_PROD_LABEL <- as.numeric(dispense$DE.HLTH_PROD_LABEL)
dispense <- merge(x=dispense, y=opioid_din[,c("HP.DIN_PIN", "HP.DRUG_BRAND_NM", "HP.GEN_DRUG")], by.x="DE.HLTH_PROD_LABEL", by.y="HP.DIN_PIN")
dispense <- dispense[,c("DE.STUDYID", "DE.HLTH_PROD_LABEL", "DE_PR_GEO.PRSCR_PRAC_LHA", "DE.FCTY_LABEL", "DE_PR_GEO.PRSCR_PRAC_PSTL_CD", "DE_PRAC.PRAC_BLLG_NUM", "DE.SRV_DATE", "DE.DSPD_QTY", "DE.DSPD_DAYS_SPLY",  "HP.GEN_DRUG", "HP.DRUG_BRAND_NM")]
names(dispense) <- c("studyid1", "DINPIN", "Prescriber_LHA", "PharmacyID", "Prescriber_FSA", "PrescriberID", "ServiceDate", "Quantity", "DaysSupply", "HP.GEN_DRUG", "HP.DRUG_BRAND_NM")
fwrite(dispense, "all_mothers_opioid_rx.gz")

# Runs super slowly, even on the fast servers, may need to run in batches and save
claims <- NULL
for(year in c("1999_2001", "2002_2004", "2005_2007", "2008_2009", "2010_2011", "2012_2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")){
  print(year)
  # Significantly faster to read the zipped data directly
  tmp <-  read_fwf(paste("R:/DATA/core-snapshot/20230217/pharmanet/dat/pharmanet_claims_", year, ".A.dat.gz",  sep=""),
                   fwf_cols(PC.STUDYID=c(1,10),
                            PC.HLTH_PROD_LABEL=c(380,389),
                            PR_GEO.PRSCR_PRAC_LHA=c(261,300),
                            PR_GEO.PRSCR_PRAC_PSTL_CD=c(301,303),
                            PC.FCTY_LABEL=c(123,147),
                            PC.SRV_DATE=c(392,401),
                            PC.DSPD_QTY=c(402,411),
                            PC.DSPD_DAYS_SPLY=c(422,431),
                            PRAC.PRAC_BLLG_NUM=c(585,589)))
  tmp <- tmp[which(tmp$PC.STUDYID %in% perinatal$studyid1),]
  tmp$PC.HLTH_PROD_LABEL <- as.numeric(tmp$PC.HLTH_PROD_LABEL)
  tmp <- data.table(tmp) # Crashes loading as data.table earlier on the full data, so need to do after
  tmp <- merge(x=tmp, y=opioid_din[,c("HP.DIN_PIN", "HP.DRUG_BRAND_NM", "HP.GEN_DRUG")], by.x="PC.HLTH_PROD_LABEL", by.y="HP.DIN_PIN")
  tmp <- tmp[,c("PC.STUDYID", "PC.HLTH_PROD_LABEL", "PR_GEO.PRSCR_PRAC_LHA", "PC.FCTY_LABEL", "PR_GEO.PRSCR_PRAC_PSTL_CD", "PRAC.PRAC_BLLG_NUM", "PC.SRV_DATE", "PC.DSPD_QTY", "PC.DSPD_DAYS_SPLY",  "HP.GEN_DRUG", "HP.DRUG_BRAND_NM")]
  names(tmp) <- c("studyid1", "DINPIN", "Prescriber_LHA", "PharmacyID", "Prescriber_FSA", "PrescriberID", "ServiceDate", "Quantity", "DaysSupply", "HP.GEN_DRUG", "HP.DRUG_BRAND_NM")
  fwrite(tmp, paste("all_mothers_opioid_rx_", year, ".gz", sep=""))
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive=TRUE)
  rm(tmp)
}


##### Merge prescriptions together:
dispense <- fread("dispensed.gz")
claims1999_2001 <- fread("all_mothers_opioid_rx_1999_2001.gz")
claims2002_2004 <- fread("all_mothers_opioid_rx_2002_2004.gz")
claims2005_2007 <- fread("all_mothers_opioid_rx_2005_2007.gz")
claims2008_2009 <- fread("all_mothers_opioid_rx_2008_2009.gz")
claims2010_2011 <- fread("all_mothers_opioid_rx_2010_2011.gz")
claims2012_2013 <- fread("all_mothers_opioid_rx_2012_2013.gz")
claims2014 <- fread("all_mothers_opioid_rx_2014.gz")
claims2015 <- fread("all_mothers_opioid_rx_2015.gz")
claims2016 <- fread("all_mothers_opioid_rx_2016.gz")
claims2017 <- fread("all_mothers_opioid_rx_2017.gz")
claims2018 <- fread("all_mothers_opioid_rx_2018.gz")
claims2019 <- fread("all_mothers_opioid_rx_2019.gz")
claims2020 <- fread("all_mothers_opioid_rx_2020.gz")

rx <- rbind(dispense, claims1999_2001, claims2002_2004, claims2005_2007, claims2008_2009, claims2010_2011, claims2012_2013, claims2014, claims2015, claims2016, claims2017, claims2018, claims2019, claims2020)
fwrite(rx, "all_mothers_opioid_rx.gz")

