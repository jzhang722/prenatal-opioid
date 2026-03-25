#### February 6, 2023
# Descriptive analyses comparing exposed to not exposed newborns



perinatal <- fread("U:/Data/allbirths.csv")

# Merge in composite outcomes
composite <- fread("U:/Data/composite_outcomes.csv")
perinatal <- merge(x=perinatal, y=composite, by.x="studyid2", by.y="studyid2", all.x=T)

###############################################################
#####       Various trajectories by exposure measure      #####
###############################################################


birth_panel <- fread("U:/Data/panel_outcomes.gz")

birth_panel <- merge(x=birth_panel, y=perinatal[,c("studyid1", "studyid2", "AgeBin", "Married", "WorldRegionOfBirth", "GovernmentBenefits", "IncomeProxy_subsidy", "M_POSTAL_CODE3", "N_Preg", "Month", "NewbornNAS", "DiagnosedAbuse", "AdmitOrUse", "Control")], by.x="studyid2", by.y="studyid2")

# Left province if not in enrollment of school or MSP
birth_panel$LeftProvince <- (is.na(birth_panel$Grade) & birth_panel$MSPEnrolled==0)

# Unless they have a FSA or medical records
birth_panel$LeftProvince[which(is.na(birth_panel$LeftProvince))] <- 0
birth_panel$LeftProvince[which(birth_panel$TotalSpending>0 | !is.na(birth_panel$FSA_Math) | !is.na(birth_panel$FSA_Writing) | !is.na(birth_panel$FSA_Reading) | birth_panel$Hosp_Any>0 | birth_panel$ED_Any>0)] <- 0

##### HEALTH
# Regression function
panel_reg_controls <- function(y, dt){
  NAS <- felm(as.formula(paste(y, "~NewbornNAS+N_Preg|Married+WorldRegionOfBirth+GovernmentBenefits+IncomeProxy_subsidy+Year+BirthYear+BirthMonth+AgeBin+M_POSTAL_CODE3|0|M_POSTAL_CODE3")), data=dt)
  Abuse <- felm(as.formula(paste(y, "~DiagnosedAbuse+N_Preg|Married+WorldRegionOfBirth+GovernmentBenefits+IncomeProxy_subsidy+Year+BirthYear+BirthMonth+AgeBin+M_POSTAL_CODE3|0|M_POSTAL_CODE3")), data=dt)
  Use <- felm(as.formula(paste(y, "~AdmitOrUse+N_Preg|Married+WorldRegionOfBirth+GovernmentBenefits+IncomeProxy_subsidy+Year+BirthYear+BirthMonth+AgeBin+M_POSTAL_CODE3|0|M_POSTAL_CODE3")), data=dt)
  return(list(NAS=NAS, Abuse=Abuse, Use=Use))
}

plot_fn <- function(dt, ylimits, title=""){
  plot(dt$NAS_coef, type="b", col="firebrick3", pch=19, lwd=2, ylim=ylimits, cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main=title); abline(h=0)
  axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
  points(dt$Abuse_coef, col="orange", type="b", pch=17, lwd=2)
  points(dt$Use_coef, col="grey30", type="b", pch=15, lwd=2)
  polygon(x=c(1:18,18:1), y=c(dt$NAS_coef+1.96*dt$NAS_se, rev(dt$NAS_coef-1.96*dt$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
  polygon(x=c(1:18,18:1), y=c(dt$Abuse_coef+1.96*dt$Abuse_se, rev(dt$Abuse_coef-1.96*dt$Abuse_se)), col=alpha("orange", 0.2), border=NA)
  polygon(x=c(1:18,18:1), y=c(dt$Use_coef+1.96*dt$Use_se, rev(dt$Use_coef-1.96*dt$Use_se)), col=alpha("grey30", 0.2), border=NA)
  legend(x=1, y=-0.3, pch=c(19,17,15), lty=c(1,1,1), lwd=c(2,2,2), cex=1.3, col=c("firebrick3", "orange", "grey30"), c("NAS diagnosis", "Mother drug abuse or dependence", "Mother screen positive or multiple rx"))
}


health <- birth_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg(y="Health_Composite", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="Health_Composite", dt=.SD)$NAS)$coef[1,2], 
                                                   Abuse_coef=summary(panel_reg(y="Health_Composite", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="Health_Composite", dt=.SD)$Abuse)$coef[1,2],
                                                   Use_coef=summary(panel_reg(y="Health_Composite", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="Health_Composite", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
health_controls <- birth_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg_controls(y="Health_Composite", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="Health_Composite", dt=.SD)$NAS)$coef[1,2], 
                                                   Abuse_coef=summary(panel_reg_controls(y="Health_Composite", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="Health_Composite", dt=.SD)$Abuse)$coef[1,2],
                                                   Use_coef=summary(panel_reg_controls(y="Health_Composite", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="Health_Composite", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
health_controls <- health_controls[order(RelativeYear)]

pdf("U:/Results/February 2024/Trajectories_Health_controls.pdf", height=8.5, width=14)
par(mfrow=c(1,1), mar=c(4.5,3,2,2), oma=c(0,0,0,0))
plot_fn(dt=health_controls, ylimits=c(-0.4,0.08))
dev.off()

rm(health, health_controls)

# Detailed health outcomes
# Windsorize at 99.9%:
birth_panel$TotalSpending[which(birth_panel$TotalSpending>quantile(birth_panel$TotalSpending,0.999, na.rm=T))] <- quantile(birth_panel$TotalSpending,0.999, na.rm=T)
birth_panel$DrugSpending[which(birth_panel$DrugSpending>quantile(birth_panel$DrugSpending,0.999, na.rm=T))] <- quantile(birth_panel$DrugSpending,0.999, na.rm=T)

TotalSpending_controls <- birth_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg_controls(y="TotalSpending", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="TotalSpending", dt=.SD)$NAS)$coef[1,2], 
                                                          Abuse_coef=summary(panel_reg_controls(y="TotalSpending", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="TotalSpending", dt=.SD)$Abuse)$coef[1,2],
                                                          Use_coef=summary(panel_reg_controls(y="TotalSpending", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="TotalSpending", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
TotalSpending_controls <- TotalSpending_controls[order(RelativeYear)]

DrugSpending_controls <- birth_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg_controls(y="DrugSpending", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="DrugSpending", dt=.SD)$NAS)$coef[1,2], 
                                                                   Abuse_coef=summary(panel_reg_controls(y="DrugSpending", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="DrugSpending", dt=.SD)$Abuse)$coef[1,2],
                                                                   Use_coef=summary(panel_reg_controls(y="DrugSpending", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="DrugSpending", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
DrugSpending_controls <- DrugSpending_controls[order(RelativeYear)]

pdf("Figure 1.pdf", height=8.5, width=14)
par(mfrow=c(2,1), mar=c(5.5,3,4,4), oma=c(4.5,0,0,0))
plot_fn(dt=TotalSpending_controls, ylimits=c(-0.07,0.45), title="(a) Total Spending")
plot_fn(dt=DrugSpending_controls, ylimits=c(-0.07,0.6), title="(b) Total Drug Spending")
legend(x=c(-22,19.5), y=c(-0.26,-0.36), pch=c(19,17,15), lty=c(1,1,1), lwd=c(2,2,2), cex=1.6, col=c("firebrick3", "orange", "grey30"), c("NAS diagnosis", "Mother drug abuse or dependence", "Mother screen positive or multiple rx"), xpd=NA, horiz=TRUE,)
dev.off()


### Special Needs (not standardized since binary)
SpecialNeeds_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(NAS_coef=summary(panel_reg_controls(y="SpecialNeeds", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="SpecialNeeds", dt=.SD)$NAS)$coef[1,2], 
                                                                                    Abuse_coef=summary(panel_reg_controls(y="SpecialNeeds", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="SpecialNeeds", dt=.SD)$Abuse)$coef[1,2],
                                                                                    Use_coef=summary(panel_reg_controls(y="SpecialNeeds", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="SpecialNeeds", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
SpecialNeeds_controls <- SpecialNeeds_controls[order(RelativeYear)]

SpecialNeeds_Sensory_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(NAS_coef=summary(panel_reg_controls(y="SpecialNeeds_Sensory", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="SpecialNeeds_Sensory", dt=.SD)$NAS)$coef[1,2], 
                                                                                            Abuse_coef=summary(panel_reg_controls(y="SpecialNeeds_Sensory", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="SpecialNeeds_Sensory", dt=.SD)$Abuse)$coef[1,2],
                                                                                            Use_coef=summary(panel_reg_controls(y="SpecialNeeds_Sensory", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="SpecialNeeds_Sensory", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
SpecialNeeds_Sensory_controls <- SpecialNeeds_Sensory_controls[order(RelativeYear)]

SpecialNeeds_Physical_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(NAS_coef=summary(panel_reg_controls(y="SpecialNeeds_Physical", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="SpecialNeeds_Physical", dt=.SD)$NAS)$coef[1,2], 
                                                                                             Abuse_coef=summary(panel_reg_controls(y="SpecialNeeds_Physical", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="SpecialNeeds_Physical", dt=.SD)$Abuse)$coef[1,2],
                                                                                             Use_coef=summary(panel_reg_controls(y="SpecialNeeds_Physical", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="SpecialNeeds_Physical", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
SpecialNeeds_Physical_controls <- SpecialNeeds_Physical_controls[order(RelativeYear)]

SpecialNeeds_BehavMental_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(NAS_coef=summary(panel_reg_controls(y="SpecialNeeds_BehavMental", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="SpecialNeeds_BehavMental", dt=.SD)$NAS)$coef[1,2], 
                                                                                                Abuse_coef=summary(panel_reg_controls(y="SpecialNeeds_BehavMental", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="SpecialNeeds_BehavMental", dt=.SD)$Abuse)$coef[1,2],
                                                                                                Use_coef=summary(panel_reg_controls(y="SpecialNeeds_BehavMental", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="SpecialNeeds_BehavMental", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
SpecialNeeds_BehavMental_controls <- SpecialNeeds_BehavMental_controls[order(RelativeYear)]

SpecialNeeds_Intellectual_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(NAS_coef=summary(panel_reg_controls(y="SpecialNeeds_Intellectual", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="SpecialNeeds_Intellectual", dt=.SD)$NAS)$coef[1,2], 
                                                                                                 Abuse_coef=summary(panel_reg_controls(y="SpecialNeeds_Intellectual", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="SpecialNeeds_Intellectual", dt=.SD)$Abuse)$coef[1,2],
                                                                                                 Use_coef=summary(panel_reg_controls(y="SpecialNeeds_Intellectual", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="SpecialNeeds_Intellectual", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
SpecialNeeds_Intellectual_controls <- SpecialNeeds_Intellectual_controls[order(RelativeYear)]

SpecialNeeds_Learning_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(NAS_coef=summary(panel_reg_controls(y="SpecialNeeds_Learning", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="SpecialNeeds_Learning", dt=.SD)$NAS)$coef[1,2], 
                                                                                             Abuse_coef=summary(panel_reg_controls(y="SpecialNeeds_Learning", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="SpecialNeeds_Learning", dt=.SD)$Abuse)$coef[1,2],
                                                                                             Use_coef=summary(panel_reg_controls(y="SpecialNeeds_Learning", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="SpecialNeeds_Learning", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
SpecialNeeds_Learning_controls <- SpecialNeeds_Learning_controls[order(RelativeYear)]

pdf("Figure 2.pdf", height=8.5, width=14)
par(mfrow=c(2,3), mar=c(5.5,3,4,4), oma=c(4.5,0,0,0))
plot(y=SpecialNeeds_controls$NAS_coef, x=6:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.035, 0.35), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(a) Any Special Needs"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=SpecialNeeds_controls$Abuse_coef, x=6:18, col="orange", type="b", pch=17, lwd=2)
points(y=SpecialNeeds_controls$Use_coef, x=6:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_controls$NAS_coef+1.96*SpecialNeeds_controls$NAS_se, rev(SpecialNeeds_controls$NAS_coef-1.96*SpecialNeeds_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_controls$Abuse_coef+1.96*SpecialNeeds_controls$Abuse_se, rev(SpecialNeeds_controls$Abuse_coef-1.96*SpecialNeeds_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_controls$Use_coef+1.96*SpecialNeeds_controls$Use_se, rev(SpecialNeeds_controls$Use_coef-1.96*SpecialNeeds_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=SpecialNeeds_Physical_controls$NAS_coef, x=6:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.035, 0.35), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(b) Physical Dsblty/Chronic Impair.; Physical Dep"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=SpecialNeeds_Physical_controls$Abuse_coef, x=6:18, col="orange", type="b", pch=17, lwd=2)
points(y=SpecialNeeds_Physical_controls$Use_coef, x=6:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Physical_controls$NAS_coef+1.96*SpecialNeeds_Physical_controls$NAS_se, rev(SpecialNeeds_Physical_controls$NAS_coef-1.96*SpecialNeeds_Physical_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Physical_controls$Abuse_coef+1.96*SpecialNeeds_Physical_controls$Abuse_se, rev(SpecialNeeds_Physical_controls$Abuse_coef-1.96*SpecialNeeds_Physical_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Physical_controls$Use_coef+1.96*SpecialNeeds_Physical_controls$Use_se, rev(SpecialNeeds_Physical_controls$Use_coef-1.96*SpecialNeeds_Physical_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=SpecialNeeds_BehavMental_controls$NAS_coef, x=6:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.035, 0.35), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(c) Mental Illness; Require Behav. Support/Interv."); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=SpecialNeeds_BehavMental_controls$Abuse_coef, x=6:18, col="orange", type="b", pch=17, lwd=2)
points(y=SpecialNeeds_BehavMental_controls$Use_coef, x=6:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_BehavMental_controls$NAS_coef+1.96*SpecialNeeds_BehavMental_controls$NAS_se, rev(SpecialNeeds_BehavMental_controls$NAS_coef-1.96*SpecialNeeds_BehavMental_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_BehavMental_controls$Abuse_coef+1.96*SpecialNeeds_BehavMental_controls$Abuse_se, rev(SpecialNeeds_BehavMental_controls$Abuse_coef-1.96*SpecialNeeds_BehavMental_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_BehavMental_controls$Use_coef+1.96*SpecialNeeds_BehavMental_controls$Use_se, rev(SpecialNeeds_BehavMental_controls$Use_coef-1.96*SpecialNeeds_BehavMental_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=SpecialNeeds_Intellectual_controls$NAS_coef, x=6:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.035, 0.35), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(d) Intellectual Disability; ASD"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=SpecialNeeds_Intellectual_controls$Abuse_coef, x=6:18, col="orange", type="b", pch=17, lwd=2)
points(y=SpecialNeeds_Intellectual_controls$Use_coef, x=6:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Intellectual_controls$NAS_coef+1.96*SpecialNeeds_Intellectual_controls$NAS_se, rev(SpecialNeeds_Intellectual_controls$NAS_coef-1.96*SpecialNeeds_Intellectual_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Intellectual_controls$Abuse_coef+1.96*SpecialNeeds_Intellectual_controls$Abuse_se, rev(SpecialNeeds_Intellectual_controls$Abuse_coef-1.96*SpecialNeeds_Intellectual_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Intellectual_controls$Use_coef+1.96*SpecialNeeds_Intellectual_controls$Use_se, rev(SpecialNeeds_Intellectual_controls$Use_coef-1.96*SpecialNeeds_Intellectual_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=SpecialNeeds_Learning_controls$NAS_coef, x=6:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.035, 0.35), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(e) Learning Disability"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=SpecialNeeds_Learning_controls$Abuse_coef, x=6:18, col="orange", type="b", pch=17, lwd=2)
points(y=SpecialNeeds_Learning_controls$Use_coef, x=6:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Learning_controls$NAS_coef+1.96*SpecialNeeds_Learning_controls$NAS_se, rev(SpecialNeeds_Learning_controls$NAS_coef-1.96*SpecialNeeds_Learning_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Learning_controls$Abuse_coef+1.96*SpecialNeeds_Learning_controls$Abuse_se, rev(SpecialNeeds_Learning_controls$Abuse_coef-1.96*SpecialNeeds_Learning_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Learning_controls$Use_coef+1.96*SpecialNeeds_Learning_controls$Use_se, rev(SpecialNeeds_Learning_controls$Use_coef-1.96*SpecialNeeds_Learning_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=SpecialNeeds_Sensory_controls$NAS_coef, x=6:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.035, 0.35), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(f) Vision Impair; Hard of Hearing; Deaf/Blind"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=SpecialNeeds_Sensory_controls$Abuse_coef, x=6:18, col="orange", type="b", pch=17, lwd=2)
points(y=SpecialNeeds_Sensory_controls$Use_coef, x=6:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Sensory_controls$NAS_coef+1.96*SpecialNeeds_Sensory_controls$NAS_se, rev(SpecialNeeds_Sensory_controls$NAS_coef-1.96*SpecialNeeds_Sensory_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Sensory_controls$Abuse_coef+1.96*SpecialNeeds_Sensory_controls$Abuse_se, rev(SpecialNeeds_Sensory_controls$Abuse_coef-1.96*SpecialNeeds_Sensory_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds_Sensory_controls$Use_coef+1.96*SpecialNeeds_Sensory_controls$Use_se, rev(SpecialNeeds_Sensory_controls$Use_coef-1.96*SpecialNeeds_Sensory_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

legend(x=c(-38,13), y=c(-0.16,-0.23), pch=c(19,17,15), lty=c(1,1,1), lwd=c(2,2,2), cex=1.6, col=c("firebrick3", "orange", "grey30"), c("NAS diagnosis", "Mother drug abuse or dependence", "Mother screen positive or multiple rx"), xpd=NA, horiz=TRUE,)
dev.off()

##### HUMAN CAPITAL

birth_panel$FSA <- rowMeans(birth_panel[,c("FSA_Math", "FSA_Writing", "FSA_Reading")], na.rm=T)

birth_panel[LeftProvince==0 & Dead==0, BehindGrade_scale:=-scale(BehindGrade), by="RelativeYear"] # Negative because behind grade is bad
birth_panel[LeftProvince==0 & Dead==0, HSDiploma_scale:=scale(HSDiploma), by="RelativeYear"]
birth_panel[LeftProvince==0 & Dead==0, HSGPA_scale:=scale(HSGPA), by="RelativeYear"]
birth_panel[LeftProvince==0 & Dead==0, FSA_scale:=scale(FSA), by="RelativeYear"]
birth_panel[LeftProvince==0 & Dead==0, FSA_Math_scale:=scale(FSA_Math), by="RelativeYear"]
birth_panel[LeftProvince==0 & Dead==0, FSA_Writing_scale:=scale(FSA_Writing), by="RelativeYear"]
birth_panel[LeftProvince==0 & Dead==0, FSA_Reading_scale:=scale(FSA_Reading), by="RelativeYear"]


birth_panel$WriteAnyFSA <- !is.na(birth_panel$FSA)


WriteFSA_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(NAS_coef=summary(panel_reg_controls(y="WriteAnyFSA", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="WriteAnyFSA", dt=.SD)$NAS)$coef[1,2], 
                                                                                        Abuse_coef=summary(panel_reg_controls(y="WriteAnyFSA", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="WriteAnyFSA", dt=.SD)$Abuse)$coef[1,2],
                                                                                        Use_coef=summary(panel_reg_controls(y="WriteAnyFSA", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="WriteAnyFSA", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
WriteFSA_controls <- WriteFSA_controls[order(RelativeYear)]

BehindGrade_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6 & RelativeYear<18, .(NAS_coef=summary(panel_reg_controls(y="BehindGrade", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="BehindGrade", dt=.SD)$NAS)$coef[1,2], 
                                                                                                    Abuse_coef=summary(panel_reg_controls(y="BehindGrade", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="BehindGrade", dt=.SD)$Abuse)$coef[1,2],
                                                                                                    Use_coef=summary(panel_reg_controls(y="BehindGrade", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="BehindGrade", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
BehindGrade_controls <- BehindGrade_controls[order(RelativeYear)]

HSDiploma_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear==18, .(NAS_coef=summary(panel_reg_controls(y="HSDiploma", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="HSDiploma", dt=.SD)$NAS)$coef[1,2], 
                                                                                 Abuse_coef=summary(panel_reg_controls(y="HSDiploma", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="HSDiploma", dt=.SD)$Abuse)$coef[1,2],
                                                                                 Use_coef=summary(panel_reg_controls(y="HSDiploma", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="HSDiploma", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
HSDiploma_controls <- HSDiploma_controls[order(RelativeYear)]

HSGPA_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear==18, .(NAS_coef=summary(panel_reg_controls(y="HSGPA_scale", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="HSGPA_scale", dt=.SD)$NAS)$coef[1,2], 
                                                                             Abuse_coef=summary(panel_reg_controls(y="HSGPA_scale", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="HSGPA_scale", dt=.SD)$Abuse)$coef[1,2],
                                                                             Use_coef=summary(panel_reg_controls(y="HSGPA_scale", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="HSGPA_scale", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
HSGPA_controls <- HSGPA_controls[order(RelativeYear)]

FSA_Math_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(NAS_coef=summary(panel_reg_controls(y="FSA_Math_scale", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="FSA_Math_scale", dt=.SD)$NAS)$coef[1,2], 
                                                                                 Abuse_coef=summary(panel_reg_controls(y="FSA_Math_scale", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="FSA_Math_scale", dt=.SD)$Abuse)$coef[1,2],
                                                                                 Use_coef=summary(panel_reg_controls(y="FSA_Math_scale", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="FSA_Math_scale", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
FSA_Math_controls <- FSA_Math_controls[order(RelativeYear)]

FSA_Writing_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(NAS_coef=summary(panel_reg_controls(y="FSA_Writing_scale", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="FSA_Writing_scale", dt=.SD)$NAS)$coef[1,2], 
                                                                                    Abuse_coef=summary(panel_reg_controls(y="FSA_Writing_scale", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="FSA_Writing_scale", dt=.SD)$Abuse)$coef[1,2],
                                                                                    Use_coef=summary(panel_reg_controls(y="FSA_Writing_scale", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="FSA_Writing_scale", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
FSA_Writing_controls <- FSA_Writing_controls[order(RelativeYear)]

FSA_Reading_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(NAS_coef=summary(panel_reg_controls(y="FSA_Reading_scale", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="FSA_Reading_scale", dt=.SD)$NAS)$coef[1,2], 
                                                                                            Abuse_coef=summary(panel_reg_controls(y="FSA_Reading_scale", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="FSA_Reading_scale", dt=.SD)$Abuse)$coef[1,2],
                                                                                            Use_coef=summary(panel_reg_controls(y="FSA_Reading_scale", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="FSA_Reading_scale", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
FSA_Reading_controls <- FSA_Reading_controls[order(RelativeYear)]


pdf("Figure 3.pdf", height=8.5, width=14)
par(mfrow=c(2,3), mar=c(5.5,3,4,4), oma=c(4.5,0,0,0))
plot(y=FSA_Math_controls_controls$NAS_coef, x=c(9,12), type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.68, 0.05), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(a) Standardized Math Score"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=FSA_Math_controls$Abuse_coef, x=c(9,12), col="orange", type="b", pch=17, lwd=2)
points(y=FSA_Math_controls$Use_coef, x=c(9,12), col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(9,12,12,9), y=c(FSA_Math_controls$NAS_coef+1.96*FSA_Math_controls$NAS_se, rev(FSA_Math_controls$NAS_coef-1.96*FSA_Math_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(9,12,12,9), y=c(FSA_Math_controls$Abuse_coef+1.96*FSA_Math_controls$Abuse_se, rev(FSA_Math_controls$Abuse_coef-1.96*FSA_Math_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(9,12,12,9), y=c(FSA_Math_controls$Use_coef+1.96*FSA_Math_controls$Use_se, rev(FSA_Math_controls$Use_coef-1.96*FSA_Math_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=FSA_Reading_controls$NAS_coef, x=c(9,12), type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.68, 0.05), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(b) Standardized Reading Score"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=FSA_Reading_controls$Abuse_coef, x=c(9,12), col="orange", type="b", pch=17, lwd=2)
points(y=FSA_Reading_controls$Use_coef, x=c(9,12), col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(9,12,12,9), y=c(FSA_Reading_controls$NAS_coef+1.96*FSA_Reading_controls$NAS_se, rev(FSA_Reading_controls$NAS_coef-1.96*FSA_Reading_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(9,12,12,9), y=c(FSA_Reading_controls$Abuse_coef+1.96*FSA_Reading_controls$Abuse_se, rev(FSA_Reading_controls$Abuse_coef-1.96*FSA_Reading_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(9,12,12,9), y=c(FSA_Reading_controls$Use_coef+1.96*FSA_Reading_controls$Use_se, rev(FSA_Reading_controls$Use_coef-1.96*FSA_Reading_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=FSA_Writing_controls$NAS_coef, x=c(9,12), type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.68, 0.05), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(c) Standardized Writing Score"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=FSA_Writing_controls$Abuse_coef, x=c(9,12), col="orange", type="b", pch=17, lwd=2)
points(y=FSA_Writing_controls$Use_coef, x=c(9,12), col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(9,12,12,9), y=c(FSA_Writing_controls$NAS_coef+1.96*FSA_Writing_controls$NAS_se, rev(FSA_Writing_controls$NAS_coef-1.96*FSA_Writing_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(9,12,12,9), y=c(FSA_Writing_controls$Abuse_coef+1.96*FSA_Writing_controls$Abuse_se, rev(FSA_Writing_controls$Abuse_coef-1.96*FSA_Writing_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(9,12,12,9), y=c(FSA_Writing_controls$Use_coef+1.96*FSA_Writing_controls$Use_se, rev(FSA_Writing_controls$Use_coef-1.96*FSA_Writing_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=HSDiploma_controls$NAS_coef, x=17.6, cex=1.5, col="firebrick3", pch=19, lwd=2, ylim=c(-0.25, 0.04), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(e) Graduate High School"); abline(h=0)
axis(1, at=c(seq(1,18,by=2), 18), c(seq(1,18,by=2), 18), cex.axis=1.5)
points(y=HSDiploma_controls$Abuse_coef, x=18, col="orange", type="b", pch=17, lwd=2, cex=1.5)
points(y=HSDiploma_controls$Use_coef, x=18.4, col="grey30", type="b", pch=15, lwd=2, cex=1.5)
arrows(x0=17.6, y0=HSDiploma_controls$NAS_coef-1.96*HSDiploma_controls$NAS_se, y1=HSDiploma_controls$NAS_coef+1.96*HSDiploma_controls$NAS_se, length=0, col="firebrick3", lwd=2)
arrows(x0=18, y0=HSDiploma_controls$Abuse_coef-1.96*HSDiploma_controls$Abuse_se, y1=HSDiploma_controls$Abuse_coef+1.96*HSDiploma_controls$Abuse_se, length=0, col="orange", lwd=2)
arrows(x0=18.4, y0=HSDiploma_controls$Use_coef-1.96*HSDiploma_controls$Use_se, y1=HSDiploma_controls$Use_coef+1.96*HSDiploma_controls$Use_se, length=0, col="grey30", lwd=2)

plot(y=HSGPA_controls$NAS_coef, x=17.6, cex=1.5, col="firebrick3", pch=19, lwd=2, ylim=c(-0.8, 0.05), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(f) High School GPA"); abline(h=0)
axis(1, at=c(seq(1,18,by=2), 18), c(seq(1,18,by=2), 18), cex.axis=1.5)
points(y=HSGPA_controls$Abuse_coef, x=18, col="orange", type="b", pch=17, lwd=2, cex=1.5)
points(y=HSGPA_controls$Use_coef, x=18.4, col="grey30", type="b", pch=15, lwd=2, cex=1.5)
arrows(x0=17.6, y0=HSGPA_controls$NAS_coef-1.96*HSGPA_controls$NAS_se, y1=HSGPA_controls$NAS_coef+1.96*HSGPA_controls$NAS_se, length=0, col="firebrick3", lwd=2)
arrows(x0=18, y0=HSGPA_controls$Abuse_coef-1.96*HSGPA_controls$Abuse_se, y1=HSGPA_controls$Abuse_coef+1.96*HSGPA_controls$Abuse_se, length=0, col="orange", lwd=2)
arrows(x0=18.4, y0=HSGPA_controls$Use_coef-1.96*HSGPA_controls$Use_se, y1=HSGPA_controls$Use_coef+1.96*HSGPA_controls$Use_se, length=0, col="grey30", lwd=2)

legend(x=c(-38,13), y=c(-1.22,-1.07), pch=c(19,17,15), lty=c(1,1,1), lwd=c(2,2,2), cex=1.6, col=c("firebrick3", "orange", "grey30"), c("NAS diagnosis", "Mother drug abuse or dependence", "Mother screen positive or multiple rx"), xpd=NA, horiz=TRUE,)
dev.off()


##### WELL-BEING


MonthlyBenefits_controls <- birth_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg_controls(y="MonthlyBenefits", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="MonthlyBenefits", dt=.SD)$NAS)$coef[1,2], 
                                                                    Abuse_coef=summary(panel_reg_controls(y="MonthlyBenefits", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="MonthlyBenefits", dt=.SD)$Abuse)$coef[1,2],
                                                                    Use_coef=summary(panel_reg_controls(y="MonthlyBenefits", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="MonthlyBenefits", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
MonthlyBenefits_controls <- MonthlyBenefits_controls[order(RelativeYear)]

birth_panel$AnyWelfare <- as.numeric(birth_panel$MonthlyBenefits>0 & birth_panel$DisabilityBenefits==0)
AnyWelfare_controls <- birth_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg_controls(y="AnyWelfare", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="AnyWelfare", dt=.SD)$NAS)$coef[1,2], 
                                                                 Abuse_coef=summary(panel_reg_controls(y="AnyWelfare", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="AnyWelfare", dt=.SD)$Abuse)$coef[1,2],
                                                                 Use_coef=summary(panel_reg_controls(y="AnyWelfare", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="AnyWelfare", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
AnyWelfare_controls <- AnyWelfare_controls[order(RelativeYear)]

AnyDisability_controls <- birth_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg_controls(y="DisabilityBenefits", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="DisabilityBenefits", dt=.SD)$NAS)$coef[1,2], 
                                                                    Abuse_coef=summary(panel_reg_controls(y="DisabilityBenefits", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="DisabilityBenefits", dt=.SD)$Abuse)$coef[1,2],
                                                                    Use_coef=summary(panel_reg_controls(y="DisabilityBenefits", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="DisabilityBenefits", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
AnyDisability_controls <- AnyDisability_controls[order(RelativeYear)]

supervisionmcfd_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear<=17, .(NAS_coef=summary(panel_reg_controls(y="supervisionmcfd", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="supervisionmcfd", dt=.SD)$NAS)$coef[1,2], 
                                                                                       Abuse_coef=summary(panel_reg_controls(y="supervisionmcfd", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="supervisionmcfd", dt=.SD)$Abuse)$coef[1,2],
                                                                                       Use_coef=summary(panel_reg_controls(y="supervisionmcfd", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="supervisionmcfd", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
supervisionmcfd_controls <- supervisionmcfd_controls[order(RelativeYear)]

fostercare_controls <- birth_panel[LeftProvince==0 & Dead==0 & RelativeYear<=17, .(NAS_coef=summary(panel_reg_controls(y="fostercare", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="fostercare", dt=.SD)$NAS)$coef[1,2], 
                                                                                  Abuse_coef=summary(panel_reg_controls(y="fostercare", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="fostercare", dt=.SD)$Abuse)$coef[1,2],
                                                                                  Use_coef=summary(panel_reg_controls(y="fostercare", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="fostercare", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
fostercare_controls <- fostercare_controls[order(RelativeYear)]


pdf("Figure 4.pdf", height=8.5, width=14)
par(mfrow=c(2,3), mar=c(5.5,3,4,4), oma=c(4.5,0,0,0))

plot(y=MonthlyBenefits_controls$NAS_coef, x=1:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-100,1000), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(a) Monthly Government Transfers (_controls$)"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=MonthlyBenefits_controls$Abuse_coef, x=1:18, col="orange", type="b", pch=17, lwd=2)
points(y=MonthlyBenefits_controls$Use_coef, x=1:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(1:18,18:1), y=c(MonthlyBenefits_controls$NAS_coef+1.96*MonthlyBenefits_controls$NAS_se, rev(MonthlyBenefits_controls$NAS_coef-1.96*MonthlyBenefits_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(MonthlyBenefits_controls$Abuse_coef+1.96*MonthlyBenefits_controls$Abuse_se, rev(MonthlyBenefits_controls$Abuse_coef-1.96*MonthlyBenefits_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(MonthlyBenefits_controls$Use_coef+1.96*MonthlyBenefits_controls$Use_se, rev(MonthlyBenefits_controls$Use_coef-1.96*MonthlyBenefits_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=AnyWelfare_controls$NAS_coef, x=1:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.05,0.4), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(b) Any Income Assistance"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=AnyWelfare_controls$Abuse_coef, x=1:18, col="orange", type="b", pch=17, lwd=2)
points(y=AnyWelfare_controls$Use_coef, x=1:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(1:18,18:1), y=c(AnyWelfare_controls$NAS_coef+1.96*AnyWelfare_controls$NAS_se, rev(AnyWelfare_controls$NAS_coef-1.96*AnyWelfare_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(AnyWelfare_controls$Abuse_coef+1.96*AnyWelfare_controls$Abuse_se, rev(AnyWelfare_controls$Abuse_coef-1.96*AnyWelfare_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(AnyWelfare_controls$Use_coef+1.96*AnyWelfare_controls$Use_se, rev(AnyWelfare_controls$Use_coef-1.96*AnyWelfare_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=AnyDisability_controls$NAS_coef, x=1:18, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.05,0.4), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(c) Any Disability Assistance"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(y=AnyDisability_controls$Abuse_coef, x=1:18, col="orange", type="b", pch=17, lwd=2)
points(y=AnyDisability_controls$Use_coef, x=1:18, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(1:18,18:1), y=c(AnyDisability_controls$NAS_coef+1.96*AnyDisability_controls$NAS_se, rev(AnyDisability_controls$NAS_coef-1.96*AnyDisability_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(AnyDisability_controls$Abuse_coef+1.96*AnyDisability_controls$Abuse_se, rev(AnyDisability_controls$Abuse_coef-1.96*AnyDisability_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(AnyDisability_controls$Use_coef+1.96*AnyDisability_controls$Use_se, rev(AnyDisability_controls$Use_coef-1.96*AnyDisability_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=supervisionmcfd_controls$NAS_coef, x=1:17, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.05,0.55), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(d) Under Supervision of Child Protective Services"); abline(h=0)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5)
points(y=supervisionmcfd_controls$Abuse_coef, x=1:17, col="orange", type="b", pch=17, lwd=2)
points(y=supervisionmcfd_controls$Use_coef, x=1:17, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(1:17,17:1), y=c(supervisionmcfd_controls$NAS_coef+1.96*supervisionmcfd_controls$NAS_se, rev(supervisionmcfd_controls$NAS_coef-1.96*supervisionmcfd_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(1:17,17:1), y=c(supervisionmcfd_controls$Abuse_coef+1.96*supervisionmcfd_controls$Abuse_se, rev(supervisionmcfd_controls$Abuse_coef-1.96*supervisionmcfd_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(1:17,17:1), y=c(supervisionmcfd_controls$Use_coef+1.96*supervisionmcfd_controls$Use_se, rev(supervisionmcfd_controls$Use_coef-1.96*supervisionmcfd_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

plot(y=fostercare_controls$NAS_coef, x=1:17, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.05,0.55), xlim=c(1,18), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n", main="(e) Foster Care"); abline(h=0)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5)
points(y=fostercare_controls$Abuse_coef, x=1:17, col="orange", type="b", pch=17, lwd=2)
points(y=fostercare_controls$Use_coef, x=1:17, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(1:17,17:1), y=c(fostercare_controls$NAS_coef+1.96*fostercare_controls$NAS_se, rev(fostercare_controls$NAS_coef-1.96*fostercare_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(1:17,17:1), y=c(fostercare_controls$Abuse_coef+1.96*fostercare_controls$Abuse_se, rev(fostercare_controls$Abuse_coef-1.96*fostercare_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(1:17,17:1), y=c(fostercare_controls$Use_coef+1.96*fostercare_controls$Use_se, rev(fostercare_controls$Use_coef-1.96*fostercare_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)

legend(x=c(-16,36), y=c(-0.25,-0.34), pch=c(19,17,15), lty=c(1,1,1), lwd=c(2,2,2), cex=1.6, col=c("firebrick3", "orange", "grey30"), c("NAS diagnosis", "Mother drug abuse or dependence", "Mother screen positive or multiple rx"), xpd=NA, horiz=TRUE,)
dev.off()


##### MORTALITY
mortality_controls <- birth_panel[, .(NAS_coef=summary(panel_reg_controls(y="Dead", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg_controls(y="Dead", dt=.SD)$NAS)$coef[1,2], 
                                      Abuse_coef=summary(panel_reg_controls(y="Dead", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg_controls(y="Dead", dt=.SD)$Abuse)$coef[1,2],
                                      Use_coef=summary(panel_reg_controls(y="Dead", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg_controls(y="Dead", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"]
mortality_controls <- mortality_controls[order(RelativeYear)]

pdf("Figure 5.pdf", height=8.5, width=14)
par(mfrow=c(1,1), mar=c(4.5,3,2,2), oma=c(0,0,0,0))
plot(mortality_controls$NAS_coef, type="b", col="firebrick3", pch=19, lwd=2, ylim=c(-0.005, 0.018), cex.lab=1.5, cex.axis=1.5, ylab="", xlab="Child Age", cex.main=1.7, xaxt="n"); abline(h=0)
axis(1, at=seq(1,18,by=2), seq(1,18,by=2), cex.axis=1.5)
points(mortality_controls$Abuse_coef, col="orange", type="b", pch=17, lwd=2)
points(mortality_controls$Use_coef, col="grey30", type="b", pch=15, lwd=2)
polygon(x=c(1:18,18:1), y=c(mortality_controls$NAS_coef+1.96*mortality_controls$NAS_se, rev(mortality_controls$NAS_coef-1.96*mortality_controls$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(mortality_controls$Abuse_coef+1.96*mortality_controls$Abuse_se, rev(mortality_controls$Abuse_coef-1.96*mortality_controls$Abuse_se)), col=alpha("orange", 0.2), border=NA)
polygon(x=c(1:18,18:1), y=c(mortality_controls$Use_coef+1.96*mortality_controls$Use_se, rev(mortality_controls$Use_coef-1.96*mortality_controls$Use_se)), col=alpha("grey30", 0.2), border=NA)
legend(x=1, y=0.018, pch=c(19,17,15), lty=c(1,1,1), lwd=c(2,2,2), cex=1.3, col=c("firebrick3", "orange", "grey30"), c("NAS diagnosis", "Mother drug abuse or dependence", "Mother screen positive or multiple rx"))
dev.off()


##### GET MEANS OF CONTROL GROUPS
# to be consistent with PSM group: 
birth_panel <- birth_panel[!is.na(NewbornNAS)& !is.na(N_Preg) &!is.na(Year) & !is.na(BirthYear) & !is.na(BirthMonth) & !is.na(AgeBin)& !is.na(M_POSTAL_CODE3)]

baselinemeans <- rbind(
round(colMeans(birth_panel[RelativeYear<=5 & Control==1, c("TotalSpending", "DrugSpending", "ED_Count", "Hosp_Count", "FSA_Math", "FSA_Reading", "FSA_Writing", "HSDiploma", "HSGPA", "SpecialNeeds", "SpecialNeeds_Physical", "SpecialNeeds_BehavMental", "SpecialNeeds_Intellectual",
                                                           "SpecialNeeds_Learning", "SpecialNeeds_Sensory", "Dead", "MonthlyBenefits", "AnyWelfare", "DisabilityBenefits", "supervisionmcfd", "fostercare")], na.rm=T), 3),
round(colMeans(birth_panel[RelativeYear>=6 & RelativeYear<=12 & Control==1, c("TotalSpending", "DrugSpending", "ED_Count", "Hosp_Count", "FSA_Math", "FSA_Reading", "FSA_Writing", "HSDiploma", "HSGPA", "SpecialNeeds", "SpecialNeeds_Physical", "SpecialNeeds_BehavMental", "SpecialNeeds_Intellectual",
                                                           "SpecialNeeds_Learning", "SpecialNeeds_Sensory", "Dead", "MonthlyBenefits", "AnyWelfare", "DisabilityBenefits", "supervisionmcfd", "fostercare")], na.rm=T), 3),
round(colMeans(birth_panel[RelativeYear>=13 & Control==1, c("TotalSpending", "DrugSpending", "ED_Count", "Hosp_Count", "FSA_Math", "FSA_Reading", "FSA_Writing", "HSDiploma", "HSGPA", "SpecialNeeds", "SpecialNeeds_Physical", "SpecialNeeds_BehavMental", "SpecialNeeds_Intellectual",
                                                           "SpecialNeeds_Learning", "SpecialNeeds_Sensory", "Dead", "MonthlyBenefits", "AnyWelfare", "DisabilityBenefits", "supervisionmcfd", "fostercare")], na.rm=T), 3)
)
baselinemeans <- data.frame(baselinemeans)
baselinemeans$WriteAnyFSA[2] <- round(mean(birth_panel$WriteAnyFSA[which(birth_panel$RelativeYear %in% c("9", "12") & birth_panel$Control==1)]),3)

fwrite(baselinemeans, "baselinemeans.csv")

##### FIGURE 6: PROPENSITY SCORE MATCHING


perinatal <- fread("allbirths.csv")
perinatal$Control <- (perinatal$NewbornNAS==0 & perinatal$NewbornNASAdjacent==0 & perinatal$MotherDrugAbuseDelivery==0 & perinatal$MotherDrugAbusePregnancy==0 & perinatal$MotherAdmitDrugUseDelivery==0 & perinatal$MotherAbusedOpioids==0)
perinatal$DiagnosedAbuse <- (perinatal$MotherDrugAbuseDelivery==1 | perinatal$MotherDrugAbusePregnancy==1)
perinatal$AdmitOrUse <- (perinatal$MotherAdmitDrugUseDelivery==1 | perinatal$MotherAbusedOpioids==1)

perinatal$Married <- as.factor(perinatal$Married); perinatal$Married <- relevel(perinatal$Married, "Y")
perinatal$AgeBin_new <- perinatal$AgeBin; perinatal$AgeBin_new[which(perinatal$AgeBin %in% c("20-24", "25-29"))] <- "20-29"; perinatal$AgeBin_new[which(perinatal$AgeBin %in% c("30-34"))]<- "30-34"; perinatal$AgeBin_new[which(perinatal$AgeBin %in% c("35-39", "40+"))] <- "35+"; perinatal$AgeBin_new <- as.factor(perinatal$AgeBin_new); perinatal$AgeBin_new <- relevel(perinatal$AgeBin_new, "35+")
perinatal$WorldRegionOfBirth <- as.factor(perinatal$WorldRegionOfBirth); perinatal$WorldRegionOfBirth <- relevel(perinatal$WorldRegionOfBirth, "CANADA/USA")
perinatal$PremiumAssistance_Any <- as.numeric(!perinatal$PremiumAssistance=="C")

perinatal$GovernmentBenefits <- (perinatal$PharmacareAny==1 | perinatal$BenefitsMo_lag1>0 | perinatal$SupplementalBenefits=="Y")
perinatal$GovernmentBenefits[which(is.na(perinatal$GovernmentBenefits))] <- 0

# 2018-19 subsidy cutoffs
perinatal$IncomeProxy_subsidy <- NA
perinatal$IncomeProxy_subsidy[which(perinatal$PremiumAssistance %in% c("H", "A", "D", "X"))] <- "0-24,000"
perinatal$IncomeProxy_subsidy[which(perinatal$PremiumAssistance %in% c("I", "B", "J", "K", "F"))] <- "24,001-30,000"
perinatal$IncomeProxy_subsidy[which(perinatal$PremiumAssistance %in% c("L", "G", "M", "N", "E"))] <- "30,001-42,000"
perinatal$IncomeProxy_subsidy[which(perinatal$PremiumAssistance %in% c("C") | is.na(perinatal$PremiumAssistance) | is.na(perinatal$IncomeProxy_subsidy))] <- "> 42,000" # if no premium assistance, impute as higher income
perinatal$IncomeProxy_subsidy <- as.factor(perinatal$IncomeProxy_subsidy)
perinatal$IncomeProxy_subsidy <- relevel(perinatal$IncomeProxy_subsidy, "> 42,000")

# Years:
perinatal$YearPeriod <- "2000-2009"; perinatal$YearPeriod[which(perinatal$Year>=2010 & perinatal$Year<=2013)] <- "2010-2013"; perinatal$YearPeriod[which(perinatal$Year>=2014)] <- "2014-2021"
perinatal$YearPeriod <- factor(perinatal$YearPeriod, levels=c("2000-2009", "2010-2013", "2014-2021"))



birth_panel <- fread("U:/Data/panel_outcomes.gz")
birth_panel <- merge(x=birth_panel, y=perinatal[,c("studyid1", "studyid2", "AgeBin", "Married", "WorldRegionOfBirth", "GovernmentBenefits", "IncomeProxy_subsidy", "M_POSTAL_CODE3", "N_Preg", "Month", "NewbornNAS", "DiagnosedAbuse", "AdmitOrUse", "Control")], by.x="studyid2", by.y="studyid2")

# Left province if not in enrollment of school or MSP
birth_panel$LeftProvince <- (is.na(birth_panel$Grade) & birth_panel$MSPEnrolled==0)

# Unless they have a FSA or medical records
birth_panel$LeftProvince[which(is.na(birth_panel$LeftProvince))] <- 0
birth_panel$LeftProvince[which(birth_panel$TotalSpending>0 | !is.na(birth_panel$FSA_Math) | !is.na(birth_panel$FSA_Writing) | !is.na(birth_panel$FSA_Reading) | birth_panel$Hosp_Any>0 | birth_panel$ED_Any>0)] <- 0

# Merge in prior year outcomes of parents:
prioroutcomes <- fread("siblings_additional_outcomes.csv")
perinatal$YearPriorBorn <- perinatal$Year-1
perinatal <- merge(x=perinatal, y=prioroutcomes[parent==1, c("studyid", "year", "exp_total_nodrug", "exp_mental_nodrug", "exp_physical_nodrug", "days_supply", "ia", "charged")],
                   by.x=c("studyid1", "YearPriorBorn"), by.y=c("studyid", "year"), all.x=T)
perinatal <- merge(x=perinatal, y=prioroutcomes[child==1, c("studyid", "year", "parents_together")], by.x=c("studyid2", "YearPriorBorn"), by.y=c("studyid", "year"), all.x=T)
setnafill(perinatal, cols=c("exp_total_nodrug", "exp_mental_nodrug", "exp_physical_nodrug", "days_supply", "ia", "charged", "parents_together"), fill=0)



panel_reg <- function(y, dt){
  NAS <- felm(as.formula(paste(y, "~NewbornNAS|M_POSTAL_CODE3|0|M_POSTAL_CODE3")), weights=dt$weights, data=dt)
  Abuse <- felm(as.formula(paste(y, "~DiagnosedAbuse|M_POSTAL_CODE3|0|M_POSTAL_CODE3")), weights=dt$weights, data=dt)
  Use <- felm(as.formula(paste(y, "~AdmitOrUse|M_POSTAL_CODE3|0|M_POSTAL_CODE3")), weights=dt$weights, data=dt)
  return(list(NAS=NAS, Abuse=Abuse, Use=Use))
}

# Create PSM matches
perinatal <- perinatal[!is.na(N_Preg)]
nas_obj <- matchit(NewbornNAS~YearPeriod+AgeBin_new+Married+WorldRegionOfBirth+GovernmentBenefits+IncomeProxy_subsidy+exp_total_nodrug+exp_mental_nodrug+exp_physical_nodrug+days_supply+charged+parents_together,
                   data=perinatal[NewbornNAS==1|Control==1], method="nearest", distance="glm", ratio=1, replace=FALSE)
abuse_obj <- matchit(DiagnosedAbuse~YearPeriod+AgeBin_new+Married+WorldRegionOfBirth+GovernmentBenefits+IncomeProxy_subsidy+exp_total_nodrug+exp_mental_nodrug+exp_physical_nodrug+days_supply+charged+parents_together,
                     data=perinatal[DiagnosedAbuse==1|Control==1], method="nearest", distance="glm", ratio=1, replace=FALSE)
use_obj <- matchit(AdmitOrUse~YearPeriod+AgeBin_new+Married+WorldRegionOfBirth+GovernmentBenefits+IncomeProxy_subsidy+exp_total_nodrug+exp_mental_nodrug+exp_physical_nodrug+days_supply+charged+parents_together,
                   data=perinatal[AdmitOrUse==1|Control==1], method="nearest", distance="glm", ratio=1, replace=FALSE)
matched_data_nas <- match.data(nas_obj)
matched_data_abuse <- match.data(abuse_obj)
matched_data_use <- match.data(use_obj)


# Merge in outcomes
nas_panel <- merge(x=birth_panel, y=matched_data_nas[,c("distance", "weights", "subclass", "studyid2")], by.x="studyid2", by.y="studyid2")
abuse_panel <- merge(x=birth_panel, y=matched_data_abuse[,c("distance", "weights", "subclass", "studyid2")], by.x="studyid2", by.y="studyid2")
use_panel <- merge(x=birth_panel, y=matched_data_use[,c("distance", "weights", "subclass", "studyid2")], by.x="studyid2", by.y="studyid2")


# Regressions
TotalSpending <-merge(merge(nas_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg(y="TotalSpending", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="TotalSpending", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                            abuse_panel[LeftProvince==0 & Dead==0, .(Abuse_coef=summary(panel_reg(y="TotalSpending", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="TotalSpending", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                      use_panel[LeftProvince==0 & Dead==0, .(Use_coef=summary(panel_reg(y="TotalSpending", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="TotalSpending", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

DrugSpending <-merge(merge(nas_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg(y="DrugSpending", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="DrugSpending", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                           abuse_panel[LeftProvince==0 & Dead==0, .(Abuse_coef=summary(panel_reg(y="DrugSpending", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="DrugSpending", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                     use_panel[LeftProvince==0 & Dead==0, .(Use_coef=summary(panel_reg(y="DrugSpending", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="DrugSpending", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

SpecialNeeds <-merge(merge(nas_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(NAS_coef=summary(panel_reg(y="SpecialNeeds", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="SpecialNeeds", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                           abuse_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(Abuse_coef=summary(panel_reg(y="SpecialNeeds", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="SpecialNeeds", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                     use_panel[LeftProvince==0 & Dead==0 & RelativeYear>=6, .(Use_coef=summary(panel_reg(y="SpecialNeeds", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="SpecialNeeds", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

FSA_Math <-merge(merge(nas_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(NAS_coef=summary(panel_reg(y="FSA_Math", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="FSA_Math", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                       abuse_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(Abuse_coef=summary(panel_reg(y="FSA_Math", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="FSA_Math", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                 use_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(Use_coef=summary(panel_reg(y="FSA_Math", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="FSA_Math", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

FSA_Reading <-merge(merge(nas_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(NAS_coef=summary(panel_reg(y="FSA_Reading", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="FSA_Reading", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                          abuse_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(Abuse_coef=summary(panel_reg(y="FSA_Reading", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="FSA_Reading", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                    use_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(Use_coef=summary(panel_reg(y="FSA_Reading", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="FSA_Reading", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

FSA_Writing <-merge(merge(nas_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(NAS_coef=summary(panel_reg(y="FSA_Writing", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="FSA_Writing", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                          abuse_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(Abuse_coef=summary(panel_reg(y="FSA_Writing", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="FSA_Writing", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                    use_panel[LeftProvince==0 & Dead==0 & RelativeYear %in% c(9,12), .(Use_coef=summary(panel_reg(y="FSA_Writing", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="FSA_Writing", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

MonthlyBenefits <-merge(merge(nas_panel[LeftProvince==0 & Dead==0, .(NAS_coef=summary(panel_reg(y="MonthlyBenefits", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="MonthlyBenefits", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                              abuse_panel[LeftProvince==0 & Dead==0, .(Abuse_coef=summary(panel_reg(y="MonthlyBenefits", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="MonthlyBenefits", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                        use_panel[LeftProvince==0 & Dead==0, .(Use_coef=summary(panel_reg(y="MonthlyBenefits", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="MonthlyBenefits", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

fostercare <-merge(merge(nas_panel[LeftProvince==0 & Dead==0 & RelativeYear<18, .(NAS_coef=summary(panel_reg(y="fostercare", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="fostercare", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                         abuse_panel[LeftProvince==0 & Dead==0 & RelativeYear<18, .(Abuse_coef=summary(panel_reg(y="fostercare", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="fostercare", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
                   use_panel[LeftProvince==0 & Dead==0 & RelativeYear<18, .(Use_coef=summary(panel_reg(y="fostercare", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="fostercare", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])

Dead <-merge(merge(nas_panel[LeftProvince==0, .(NAS_coef=summary(panel_reg(y="Dead", dt=.SD)$NAS)$coef[1,1], NAS_se=summary(panel_reg(y="Dead", dt=.SD)$NAS)$coef[1,2]), by="RelativeYear"],
                   abuse_panel[LeftProvince==0, .(Abuse_coef=summary(panel_reg(y="Dead", dt=.SD)$Abuse)$coef[1,1], Abuse_se=summary(panel_reg(y="Dead", dt=.SD)$Abuse)$coef[1,2]), by="RelativeYear"]),
             use_panel[LeftProvince==0, .(Use_coef=summary(panel_reg(y="Dead", dt=.SD)$Use)$coef[1,1], Use_se=summary(panel_reg(y="Dead", dt=.SD)$Use)$coef[1,2]), by="RelativeYear"])


pdf("Figure 6.pdf", height=16, width=16)
par(mfrow=c(3,3))
plot(TotalSpending[,c("RelativeYear", "NAS_coef")], type="o", lwd=2, ylim=c(-1100,2000), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(a) Total Spending", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(1:18,18:1), y=c(TotalSpending$NAS_coef+1.96*TotalSpending$NAS_se, rev(TotalSpending$NAS_coef-1.96*TotalSpending$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(TotalSpending[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(1:18,18:1), y=c(TotalSpending$Abuse_coef+1.96*TotalSpending$Abuse_se, rev(TotalSpending$Abuse_coef-1.96*TotalSpending$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(TotalSpending[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(1:18,18:1), y=c(TotalSpending$Use_coef+1.96*TotalSpending$Use_se, rev(TotalSpending$Use_coef-1.96*TotalSpending$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(DrugSpending[,c("RelativeYear", "NAS_coef")], type="o", lwd=2, ylim=c(-100,800), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(b) Drug Spending", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(1:18,18:1), y=c(DrugSpending$NAS_coef+1.96*DrugSpending$NAS_se, rev(DrugSpending$NAS_coef-1.96*DrugSpending$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(DrugSpending[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(1:18,18:1), y=c(DrugSpending$Abuse_coef+1.96*DrugSpending$Abuse_se, rev(DrugSpending$Abuse_coef-1.96*DrugSpending$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(DrugSpending[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(1:18,18:1), y=c(DrugSpending$Use_coef+1.96*DrugSpending$Use_se, rev(DrugSpending$Use_coef-1.96*DrugSpending$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(SpecialNeeds[,c("RelativeYear", "NAS_coef")], xlim=c(1,18), type="o", lwd=2, ylim=c(-0.05,0.5), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(c) Special Needs", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds$NAS_coef+1.96*SpecialNeeds$NAS_se, rev(SpecialNeeds$NAS_coef-1.96*SpecialNeeds$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(SpecialNeeds[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds$Abuse_coef+1.96*SpecialNeeds$Abuse_se, rev(SpecialNeeds$Abuse_coef-1.96*SpecialNeeds$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(SpecialNeeds[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(6:18,18:6), y=c(SpecialNeeds$Use_coef+1.96*SpecialNeeds$Use_se, rev(SpecialNeeds$Use_coef-1.96*SpecialNeeds$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(FSA_Math[,c("RelativeYear", "NAS_coef")], xlim=c(1,18), type="o", lwd=2, ylim=c(-0.4,0.05), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(d) Standardized Math", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(9,12,12,9), y=c(FSA_Math$NAS_coef+1.96*FSA_Math$NAS_se, rev(FSA_Math$NAS_coef-1.96*FSA_Math$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(FSA_Math[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(9,12,12,9), y=c(FSA_Math$Abuse_coef+1.96*FSA_Math$Abuse_se, rev(FSA_Math$Abuse_coef-1.96*FSA_Math$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(FSA_Math[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(9,12,12,9), y=c(FSA_Math$Use_coef+1.96*FSA_Math$Use_se, rev(FSA_Math$Use_coef-1.96*FSA_Math$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(FSA_Reading[,c("RelativeYear", "NAS_coef")], xlim=c(1,18), type="o", lwd=2, ylim=c(-0.4,0.05), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(e) Standardized Reading", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(9,12,12,9), y=c(FSA_Reading$NAS_coef+1.96*FSA_Reading$NAS_se, rev(FSA_Reading$NAS_coef-1.96*FSA_Reading$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(FSA_Reading[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(9,12,12,9), y=c(FSA_Reading$Abuse_coef+1.96*FSA_Reading$Abuse_se, rev(FSA_Reading$Abuse_coef-1.96*FSA_Reading$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(FSA_Reading[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(9,12,12,9), y=c(FSA_Reading$Use_coef+1.96*FSA_Reading$Use_se, rev(FSA_Reading$Use_coef-1.96*FSA_Reading$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(FSA_Writing[,c("RelativeYear", "NAS_coef")], xlim=c(1,18), type="o", lwd=2, ylim=c(-0.4,0.05), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(f) Standardized Writing", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(9,12,12,9), y=c(FSA_Writing$NAS_coef+1.96*FSA_Writing$NAS_se, rev(FSA_Writing$NAS_coef-1.96*FSA_Writing$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(FSA_Writing[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(9,12,12,9), y=c(FSA_Writing$Abuse_coef+1.96*FSA_Writing$Abuse_se, rev(FSA_Writing$Abuse_coef-1.96*FSA_Writing$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(FSA_Writing[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(9,12,12,9), y=c(FSA_Writing$Use_coef+1.96*FSA_Writing$Use_se, rev(FSA_Writing$Use_coef-1.96*FSA_Writing$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(MonthlyBenefits[,c("RelativeYear", "NAS_coef")], type="o", lwd=2, ylim=c(-50,600), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(g) Monthly Government Transfers ($)", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(1:18,18:1), y=c(MonthlyBenefits$NAS_coef+1.96*MonthlyBenefits$NAS_se, rev(MonthlyBenefits$NAS_coef-1.96*MonthlyBenefits$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(MonthlyBenefits[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(1:18,18:1), y=c(MonthlyBenefits$Abuse_coef+1.96*MonthlyBenefits$Abuse_se, rev(MonthlyBenefits$Abuse_coef-1.96*MonthlyBenefits$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(MonthlyBenefits[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(1:18,18:1), y=c(MonthlyBenefits$Use_coef+1.96*MonthlyBenefits$Use_se, rev(MonthlyBenefits$Use_coef-1.96*MonthlyBenefits$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(fostercare[,c("RelativeYear", "NAS_coef")], type="o", lwd=2, ylim=c(-0.05,0.5), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(h) Foster Care", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(1:17,17:1), y=c(fostercare$NAS_coef+1.96*fostercare$NAS_se, rev(fostercare$NAS_coef-1.96*fostercare$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(fostercare[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(1:17,17:1), y=c(fostercare$Abuse_coef+1.96*fostercare$Abuse_se, rev(fostercare$Abuse_coef-1.96*fostercare$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(fostercare[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(1:17,17:1), y=c(fostercare$Use_coef+1.96*fostercare$Use_se, rev(fostercare$Use_coef-1.96*fostercare$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)

plot(Dead[,c("RelativeYear", "NAS_coef")], type="o", lwd=2, ylim=c(-0.01,0.019), col="firebrick3", pch=19, xaxt="n", xlab="Child Age", ylab="", main="(i) Cumulative Mortality", cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
axis(1, at=seq(1,17,by=2), seq(1,17,by=2), cex.axis=1.5, cex=1.5)
polygon(x=c(1:18,18:1), y=c(Dead$NAS_coef+1.96*Dead$NAS_se, rev(Dead$NAS_coef-1.96*Dead$NAS_se)), col=alpha("firebrick3", 0.2), border=NA)
points(Dead[,c("RelativeYear", "Abuse_coef")], type="o", lwd=2, col="orange", pch=17)
polygon(x=c(1:18,18:1), y=c(Dead$Abuse_coef+1.96*Dead$Abuse_se, rev(Dead$Abuse_coef-1.96*Dead$Abuse_se)), col=alpha("orange", 0.2), border=NA)
points(Dead[,c("RelativeYear", "Use_coef")], type="o", lwd=2, col="grey30", pch=16)
polygon(x=c(1:18,18:1), y=c(Dead$Use_coef+1.96*Dead$Use_se, rev(Dead$Use_coef-1.96*Dead$Use_se)), col=alpha("grey30", 0.2), border=NA)
abline(h=0)
dev.off()

# Plot balance (supplemental material)
plot(summary(use_obj), abs=FALSE)

vars <- c("Propensity Score Distance", "Birthyear: 2000-2009", "Birthyear: 2010-2013", "Birthyear: 2014-2021", "Mother's Age: 35+", "Mother's Age: <20", "Mother's Age: 20-29", "Mother's Age: 30-34", 
          "Marital Status: Married", "Marital Status: Not Married", "Marital Status: Unknown" )

plot(x=summary(use_obj)$sum.all[,3])
points(y=29:1, x=summary(use_obj)$sum.matched[,3])


fwrite(data.frame(summary(use_obj)$sum.all[,3]), "Balance_use_unmatched.csv")
fwrite(summary(use_obj)$sum.matched[,3], "Balance_use_matched.csv")


