library(tidyverse)
library(data.table)
library(haven)
library(readr)
library(lfe)
library(stargazer)
`%notin%`=Negate(`%in%`)
######################################

# All Births 

dfbirths=fread("allbirths.csv")
# only take necessary variables
dfbirths=dfbirths[,c("BirthDate","studyid2")]
dfbirths= dfbirths %>% rename(STUDYID=studyid2)

# create birth year:
dfbirths$birthyear=substr(dfbirths$BirthDate,1,4); dfbirths$BirthDate=NULL


#each row a child under observation by month-year
#By month, a complete listing of children and youth under the supervision of MCFD by a court order or by agreement with the parents
df_actv=read_fwf(gzfile("R:/DATA/core-snapshot/20230217/ido_mcfd/dat/idomcfd1997-22.actv_stage7.D.dat.gz"),
                 fwf_cols(ICMPID=c(1,16),  #ICM unique identifier for case contacts (pids)
                          FILEID=c(17,28), # Combination of two letters followed by 8 digits to uniquely identify a case. Each child/youth client will have only one \"CS\" case but may appear on multiple \"FS\" cases - use dep_seq to identify unique contacts\n"
                          opensince=c(37,46), # Cases can open/close multiple times in the child/youth's life. This date was the last time the case was either originally open or reopened
                          lvgarrcd=c(47,50), # living arrangements before admission 
                          pgmservcd=c(54,56), # Program Service Area. Child Protection or CYSN (special needs stream)
                          basis=c(57,59), # Service Basis: Either Protection or Non-Protection
                          SERVREAS1=c(60,64), # Reason for service 
                          SERVREAS2=c(65,69), # Reason for service 
                          SERVREAS3=c(70,74), # Reason for service 
                          RESID=c(75,86),# File number ubiquely identifying a caregiver under contract with MCFD. All resid begin with \"RE\" and end
                          SEP_NUM=c(87,97), # A number uniquely identifying service provider
                          OGC_NUM=c(98,108),  # A number uniquely identifying service provider
                          PLA_SEQ=c(109,119), # Service providers can have multiple places of service. This is a sequential number related to a sep_num. The concatentaion of sep_num and pla_seq uniquely identifies a place of service\n"
                          DEP_SEQ=c(120,123), # A sequential number related to fileid starting at 00 going to number of related contacts listed on a file\n"
                          age=c(124,135), # age at cym
                          leggrp=c(136,148), # The last and largest groupings of sections of the CFCSA\n
                          legcat=c(149,175), # "The first of two roll-ups/groupings of CFCSA sections in legauth\n"
                          legauth=c(176,207),# "The section of the CFCSA providing MCFD with the authority to provide services to the child/youth client\n"
                          curplan=c(214,258), # Current permanency plan
                          plctype=c(259,285), # A grouping of services that determine the type to service provider (All Level 1 Foster Services get rolled up to Level 1 Type of placement)
                          servtype=c(286,350), # Text description of the service the ministry contracts out to a service provider
                          cym=c(351,358), # Year Month the child youth was on an open cs or fs case
                          cysn_cyic=c(383,385), # A child/youth in care clients that has Children and Youth with Special Needs indicators on their ICM record (service stream of CYSN, Disability Benefit set to In Progress or In Pay, In Care under a Special Needs Agreement, was placed with a CYSN service provider, or ever was listed on a CYSN family case and determined to be eligible for a ministry CYSN program (Autism, At Home Medical Benefits, or Developmental Disability)
                          stream=c(208,213),# Text expansion of pgmservcd
                          icmpid_cord=c(584,599), # See edition notes in the Dataset Summary. Replaced with coordinating ID and then project specific encryption. ICM unique identifier for case contacts (pids). Can be key player or any other case contact\n"
                          STUDYID=c(616,625)))


# This file is children who are under supervision as ordered by the court or arrangement with the parents - not the same thing as contacts

# Filter on: only people who are under child protection (not special needs ): pgmservcd variable 
#%>% filter(pgmservcd=="C") : do not filter based on program served because these people 

# Get the first time the case was opened, and last time they were ever under supervision
#df_actv=df_actv %>% group_by(STUDYID,FILEID) %>% mutate(min_opendate=min(opensince), max_closedate=max(cym))

# create code for if child was ever in "protection" during the case
df_actv$protection_ind=ifelse(df_actv$basis=="P",1,0)

# create code for service reasons:
# neg (neglect, physical harm)
df_actv$reason_neglect=0
df_actv$reason_neglect[df_actv$SERVREAS1 %in% c("ABN","abn","ABS","abs","DEP","dep","EMO","emo","EDV","edv","PHY","phy","SXL","sxl","NEG","neg","PNP","pnp","UNA","una","RTR","rtr") ]=1                                      
df_actv$reason_neglect[df_actv$SERVREAS2 %in% c("ABN","abn","ABS","abs","DEP","dep","EMO","emo","EDV","edv","PHY","phy","SXL","sxl","NEG","neg","PNP","pnp","UNA","una","RTR","rtr") ]=1                                      
df_actv$reason_neglect[df_actv$SERVREAS3 %in% c("ABN","abn","ABS","abs","DEP","dep","EMO","emo","EDV","edv","PHY","phy","SXL","sxl","NEG","neg","PNP","pnp","UNA","una","RTR","rtr") ]=1                                      

df_actv$reason_parentdeath=0
df_actv$reason_parentdeath[df_actv$SERVREAS1 %in% c("DEC","dec")]=1
df_actv$reason_parentdeath[df_actv$SERVREAS2 %in% c("DEC","dec")]=1
df_actv$reason_parentdeath[df_actv$SERVREAS3 %in% c("DEC","dec")]=1

# indicator for: child under supervision
df_actv$supervisionmcfd=1

# Permanent or independence 
# Trnasfer Custody 54.01: parents' rights have been terminated and they are under the Ministry's permanent guardianship 

#[1] "Not Coded"                      NA                               "Adoption"                       "Return to Parent"               "Return Parent/Concurrent Plan"  "Mentoring Relationship w adult"
#[7] "With Extended Family"           "Transfer Custody 54.1"          "Mentoring Relation. w adv/surr" "Independent Living"             "Substitute Care"                "Family Law Act"                
#[13] "Transfer Custody 54.01"         "Placed Within Aboriginal Comm." "Interprovincial Placement Agmt" "Plan For Independence"         

df_actv$permanent_transfer=ifelse(df_actv$curplan %in% c("Transfer Custody 54.01", "Transfer Custody 54.1"),1,0)
df_actv$returnparent=ifelse(df_actv$curplan %in% c("Return to Parent","Return Parent/Concurrent Plan"),1,0)
df_actv$temp_transfer=ifelse(df_actv$curplan %in% c("With Extended Family","Placed within Aboriginal Comm.","Substitute Care"),1,0)
df_actv$other=ifelse(df_actv$curplan %in% c("Not Coded",NA,"Mentoring Relation. w adv/surr","Interprovincial Placement Agmt","Independent Living","Plan For Independence","Mentoring Relationship w adult","Family Law Act"),1,0)

df_actv$adoption=ifelse(df_actv$curplan=="Adoption",1,0)
df_actv$permanent_transfer[is.na(df_actv$curplan)]=0
df_actv$returnparent[is.na(df_actv$curplan)]=0
df_actv$temp_transfer[is.na(df_actv$curplan)]=0
df_actv$adoption[is.na(df_actv$curplan)]=0
df_actv$other[is.na(df_actv$curplan)]=0

#View(df_actv[df_actv$curplan!="Not Coded" & !is.na(df_actv$curplan),c("STUDYID","agecat","curplan")])

# indicator for plctype values 
#[1] "Living Independently"      "Parents/Relatives"         "Other Resources"           "Level 2 Care"             
#[5] "Level 1 Care"              "Level 3 Care"              "Restricted Family Care"    "Contracted Resources"     
#[9] "Missing/Runaway"           "Aboriginal Resources"      "Not Coded"                 "Regular Family Care"      
#[13] "Support Service"           "Adoption Residency Period" "Out of Care"               "Miscoded placement type"  

# Level 1/2/3: caregiver provides service to children who can no longer remain in their own home and have certain behavioural needs

df_actv$levelcare=ifelse(df_actv$plctype %in% c("Level 1 Care","Level 2 Care","Level 3 Care"),1,0)

df_actv$fostercare=ifelse(df_actv$plctype %in% c("Level 1 Care","Level 2 Care","Level 3 Care", "Regular Family Care","Restricted Family Care"),1,0)

df_actv$adoptionplacement=ifelse(df_actv$plctype %in% c("Adoption Residency Period"),1,0)

# Test: count # of kids in foster care by year and see if it matches public records

df_by_year=df_actv[,c("cym","STUDYID","fostercare","levelcare")]
df_by_year$year=substr(df_by_year$cym,1,4)
# if ever in foster/level care during that year
df_by_year=df_by_year %>% group_by(STUDYID,year) %>% summarize(fostercare=max(fostercare),levelcare=max(levelcare))
foster_year=df_by_year%>% filter(fostercare==1) %>% group_by(year) %>% summarize(count=n())
level_year=df_by_year%>% filter(levelcare==1) %>% group_by(year) %>% summarize(count=n())
# foster_year matches with Census 2011 for BC



df_actv$agecat=cut(df_actv$age, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),
                   right=FALSE)

# drop adults

df_actv=df_actv[!is.na(df_actv$agecat),]
# ONE ROW PER PERSON: this takes the max of all the months: not the best, kind of messy
dfsummary_protect=df_actv %>% filter(!is.na(agecat)) %>% group_by(STUDYID,agecat) %>% summarize(levelcare=max(levelcare),
                                                                                                fostercare=max(fostercare),
                                                                                                supervision_neglect=max(reason_neglect),
                                                                                                protection_ever=max(protection_ind),
                                                                                                supervisionmcfd=max(supervisionmcfd),
                                                                                                returnparent=max(returnparent),
                                                                                                permanent_transfer=max(permanent_transfer),
                                                                                                adoption=max(adoption),
                                                                                                temp_transfer=max(temp_transfer),
                                                                                                other=max(other))


#not better: take the last month of a current age:
#dfsummary_protect=df_actv %>% group_by(STUDYID,agecat) %>% mutate(maxmonth=max(cym))

#dfsummary_protect=dfsummary_protect %>% filter(maxmonth==cym)

# keep those in the allbirths file
dfsummary_protect=dfsummary_protect[dfsummary_protect$STUDYID %in% dfbirths$STUDYID,]





##################################################################
# All_client_export dataset: lists all of the child youth contacts associated with MCFD services

df_client=read_fwf(gzfile("R:/DATA/core-snapshot/20230217/ido_mcfd/dat/idomcfd1997-22.all_client_export.C.dat.gz"),
                   fwf_cols(ICMPID=c(1,16),  #ICM unique identifier for case contacts (pids)
                            icmpid_cord=c(240,255),
                            STUDYID=c(272,281)))


##################################################################
#all_family_services_case
# Each row is a family contact for a studyid by year-month: 
# ICMPID identifies the family contact
# case_num identifies which case it's for
# rel_cd identifieds relationship between family contact and the child 
df_family=read_fwf(gzfile("R:/DATA/core-snapshot/20230217/ido_mcfd/dat/idomcfd1997-22.all_family_serv_cases.B.dat.gz"),
                   fwf_cols(ICMPID=c(1,16),  #ICM unique identifier for case contacts (pids)
                            case_num=c(25,39), #Electronic number that defines the case
                            cym=c(17,24),#Year Month the client had an open Child Services (In Care, Youth Agreement, and Out of Care from Dec 2014 ) case at month end\n"
                            rel_cd=c(40,71), # A three letter code describing the relationship between the person identified in the row to the person identified as the Key Player of the case (uniquely identified by fileid). This relationship code is only valid with in a group of people with the same fileid. The same person involved in multiple cases can have multiple relationship codes\n"
                            STUDYID=c(301,310)
                            
                   )
) 

length(unique(df_family$STUDYID))

# indicator for having a family services case:
df_family$famservice=1

# bring in age: 
df_family=merge(df_family,dfbirths,by="STUDYID") # this keeps about a third of the dataset: those that are in our time period of interest

df_family$age=as.numeric(substr(df_family$cym,1,4))-as.numeric(df_family$birthyear)


df_family$agecat=cut(df_family$age, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),
                     right=FALSE)



df_familysum=df_family %>% filter(!is.na(agecat)) %>% group_by(STUDYID,agecat) %>% summarize(famservice=max(famservice))
##################################
#CS_Clients_Parental
# indicators for if the child was on the parents income assistance file : indicates if during a year-month, if the child was on an MFCD open case AND on mother's or father's income assistance case


# CYSN_Clients
# describes the special needs services that the child is eligible for 

# Location_history: complete history of postal codes 
###########################################
# all case numbers 
df_intake=read_fwf(gzfile("R:/DATA/core-snapshot/20230217/ido_mcfd/dat/idomcfd1997-22.intake_export.D.dat.gz"),
                   fwf_cols(
                     ICMPID=c(1,16), # Replaced with coordinating id and then project specific encryption. ICM unique identifier for case contacts (pids). Can be key player or any other intake contact
                     ENTITYNUM=c(17,33), # Replaced with project specific encryption. ICM database number for the intake
                     MEMONUM=c(34,48), # Replaced with project specific encryption. ICM database number for the screening memo
                     ENTITY_TYPE=c(49,53), # The type of entity the intake ends in (Memo, Incident, or Service Request)
                     CALL_CD=c(54,83), # Social worker's determination of the service required
                     REQACTION=c(84,115), # The determiniation of the type of intake (Caregiver App-EFP, Family Law Application, Interprovincial Request, Protection Report, Protection Report - Resource, Request For CYSN FS, Request For Fam Supp Serv, Request For Youth Services)
                     STUDYID=c(2023,2032),
                     CALL_DATE=c(150,159),
                     MEMO_DATE=c(120,129), #Date the screening memo occurred
                     PROT_FINDING=c(160,191), #For Child Protection Incidents the final protection finding
                     CALLER=c(419,450), #  The type of caller that contacted MCFD (Police, School, Parent, Community Professional, Relative, etc.)
                     RESOLUTION=c(451,482), #The final decision of the social worker responsible for the intake
                     concern_abn=c(539,550), # cause for concern: child abandoned
                     concern_abs=c(551,562), # cause : child absent from home
                     concern_dep=c(563,574), # cause: deprived of health
                     concern_emo=c(575,586), # cause: emotional harm
                     concern_edv=c(587,598), # cause: emotional and physical violence
                     concern_exp=c(599,610), # cause: parent not able to manage
                     concern_neg=c(611,622), # cause: neglect
                     concern_dec=c(623,634), # cause: parent dead
                     concern_pnp=c(635,646), # cause: parent not protecting
                     concern_una=c(647,658), # cause: parent unable to protect
                     concern_phy=c(659,670), # cause: likley physical harm
                     concern_rtr=c(671,682), # cause : parent does not treat condition
                     concern_sxl=c(683,694), # cause: sexual abuse
                     valid_abn=c(695,706), #  valid: child abandoned
                     valid_abs=c(719,730), # valid : child absent from home
                     valid_dep=c(743,752), # valid: deprived of health
                     valid_emo=c(767,778), # valid: emotional harm
                     valid_edv=c(791,802), # valid: emotional and physical violence
                     valid_exp=c(863,874), # valid: parent not able to manage
                     valid_neg=c(815,826), # valid: neglect
                     valid_dec=c(839,850), # valid: parent dead
                     valid_pnp=c(887,898), # valid: parent not protecting
                     valid_una=c(911,922), # valid: parent unable to protect
                     valid_phy=c(995,1006), # valid: likley physical harm
                     valid_rtr=c(959,970), # valid : parent does not treat condition
                     valid_sxl=c(935,946), # valid: sexual abuse
                     q9=c(1199,1210),# "From Safety Assessment: 9. Parent/care-provider's current alcohol, drug, or substance abuse seriously impairs his/her ability to supervise, protect, or care for the child/youth\n"
                     n8_either_parent_alcohol_drugs=c(1355,1366), # Vulnerability Assessment Section Neglect Question 8 : Either Parent has past or current alcohol, drug or substance problem\n
                     
                     prim_par_ment_iss_abs_drug=c(1583,1594), # Primary Parent has issue with drugs identified in the abuse section
                     prim_par_ment_iss_abs_prior=c(1595,1606), # Primary Parent has issue with drugs or alcohol identified in the abuse section prior to 12 months of call date
                     prim_par_ment_iss_abs_within=c(1607,1618), # Primary Parent has issue with drugs or alcohol identified in the abuse section within 12 months of call date
                     
                     prim_par_ment_iss_drug=c(1631,1642), # Primary Parent has issue with drugs identified in the neglect section
                     prim_par_ment_iss_prior=c(1643,1654), # Primary Parent has issue with drugs or alcohol identified in the neglect section prior to 12 months of call date
                     prim_par_ment_iss_within=c(1655,1666), # Primary Parent has issue with drugs or alcohol identified in the neglect section within 12 months of call date
                     
                     
                     sec_par_ment_iss_abs_drug=c(1703,1714), # secondary Parent has issue with drugs identified in the abuse section
                     sec_par_ment_iss_abs_prior=c(1715,1726), # secary Parent has issue with drugs or alcohol identified in the abuse section prior to 12 months of call date
                     sec_par_ment_iss_abs_within=c(1727,1738), # secary Parent has issue with drugs or alcohol identified in the abuse section within 12 months of call date
                     
                     sec_par_ment_iss_drug=c(1751,1762), # secary Parent has issue with drugs identified in the neglect section
                     sec_par_ment_iss_prior=c(1763,1774), # secary Parent has issue with drugs or alcohol identified in the neglect section prior to 12 months of call date
                     sec_par_ment_iss_within=c(1775,1786) # secary Parent has issue with drugs or alcohol identified in the neglect section within 12 months of call date
                     
                   ))


# get those from the birth file
df_intake=merge(df_intake,dfbirths,by="STUDYID")
##########################################
# summarize  parent drugs/alcohol issues

df_intake=df_intake %>% rowwise() %>% mutate(alcohol_drug_primary=max(prim_par_ment_iss_abs_drug, prim_par_ment_iss_abs_prior, prim_par_ment_iss_abs_within,prim_par_ment_iss_drug, prim_par_ment_iss_prior, prim_par_ment_iss_within),
                                             alcohol_drug_secondary=max(sec_par_ment_iss_abs_drug,sec_par_ment_iss_abs_prior,sec_par_ment_iss_abs_within,sec_par_ment_iss_abs_within, sec_par_ment_iss_drug, sec_par_ment_iss_prior, sec_par_ment_iss_within))
#

df_intake$concern_neglect=ifelse((df_intake$concern_abn==1|df_intake$concern_abs==1|df_intake$concern_dep==1|df_intake$concern_edv==1|df_intake$concern_emo==1|df_intake$concern_exp==1|df_intake$concern_neg==1|df_intake$concern_phy==1|df_intake$concern_pnp==1|df_intake$concern_rtr==1|df_intake$concern_sxl==1|df_intake$concern_una==1),1,0)
df_intake$valid_neglect=ifelse((df_intake$valid_abn==1|df_intake$valid_abs==1|df_intake$valid_dep==1|df_intake$valid_edv==1|df_intake$valid_emo==1|df_intake$valid_exp==1|df_intake$valid_neg==1|df_intake$valid_phy==1|df_intake$valid_pnp==1|df_intake$valid_rtr==1|df_intake$valid_sxl==1|df_intake$valid_una==1),1,0)

df_intake$alcoholdrug=ifelse(df_intake$alcohol_drug_primary==1 | df_intake$alcohol_drug_secondary==1 | df_intake$n8_either_parent_alcohol_drugs==1,1,0)

df_clean=df_intake[ ,c("STUDYID", "REQACTION", "birthyear","CALL_CD","CALL_DATE","MEMO_DATE", "PROT_FINDING", "CALLER","RESOLUTION","concern_neglect","valid_neglect","alcoholdrug")]

# indicator for contact with child services
df_clean$contact_mcfd=1
#indicator for whether case was opened
df_clean$contact_caseopen=ifelse(df_clean$RESOLUTION %in% c("Service Provided","Referall Provided","Incident Opened","Associated to Open Case","Legacy System Case Opened","Related to Open Incident","Service Request Opened",
                                                            "Case Opened","Related to Open Case"),1,0)

# indicator for call code 
df_clean$protection_call=ifelse(df_clean$CALL_CD %in% c("Protection Report","Protection Report - Resource"),1,0)

# age
df_clean$age=as.numeric(substr(df_clean$CALL_DATE,1,4))-as.numeric(df_clean$birthyear)
df_clean$agecat=cut(df_clean$age, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),labels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18"),
                    right=FALSE)
# summarize 
dfsummary_intake=df_clean %>% group_by(STUDYID,agecat) %>% summarize(contactmfcd=max(contact_mcfd),
                                                                     #firstintakedate=min(CALL_DATE),
                                                                     intakecaseopen=max(contact_caseopen),
                                                                     intake_protection=max(protection_call),
                                                                     intake_concern_neglect=max(concern_neglect),
                                                                     intake_neglect_valid=max(valid_neglect),
                                                                     alcoholdrug=max(alcoholdrug))
########################### Merge dfsummary_intake and dfsummary_protect ###################

dffinal=dfsummary_protect
#dffinal=merge(dfsummary_protect,df_familysum,by=c("STUDYID","agecat"),all=TRUE)
#dffinal=merge(dffinal,df_familysum,by=c("STUDYID","agecat"),all.x=TRUE)

# fill in: 
# get all ages 
d=data.frame(agecat=seq(from=1,to=17,by=1))

agedf=do.call("rbind",replicate(length(unique(dffinal$STUDYID)),d, simplify=FALSE))

id=data.frame(STUDYID=unique(dffinal$STUDYID))

iddf=do.call("rbind",replicate(17,id,simplify=FALSE)) %>% arrange(STUDYID)

df_fill=cbind(agedf,iddf)

#df_fill=df_fill %>% arrange(STUDYID,agecat)

dffinal=merge(dffinal,df_fill,by=c("STUDYID","agecat"),all.y=TRUE)

# clean NAs:
dffinal$supervisionmcfd[is.na(dffinal$supervisionmcfd)]=0
#dffinal$famservice[is.na(dffinal$famservice)]=0
#dffinal$contactmfcd[is.na(dffinal$contactmfcd)]=0
col_order=c("STUDYID","agecat","supervisionmcfd","reason_neglect",
            "fostercare","levelcare","protection_ind","returnparent","permanent_transfer","adoption","temp_transfer","other")
dffinal=dffinal[,col_order]
View(df_actv[df_actv$curplan!="Not Coded" & !is.na(df_actv$curplan),c("STUDYID","agecat","curplan")])

studyid_ACTV=df_actv$STUDYID[df_actv$curplan!="Not Coded" & !is.na(df_actv$curplan)]

View(dffinal[dffinal$STUDYID %in% studyid_ACTV,])

#dffinal$famservice[dffinal$supervisionmcfd==1]=1

fwrite(dffinal,"ChildServices_Intake_Protection.csv")
