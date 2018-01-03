####PCM/Rasch analysis

#-------------------------------------------------------------------------------  
###1. PCM analysis
set.seed(1052017)
model <- PCM(data_PCM[,vars_metric])                                      
save(model, file=paste(path_rasch,"/PCM_model.RData", sep=""))


#-------------------------------------------------------------------------------  
###2. Item Difficulty thresholds
Thr_PCM <- thresholds(model)
write.csv(Thr_PCM[3], paste(path_rasch,"/Location_Threshold.csv",sep=""))


Thresholds_Table_Recoded <- cbind(Thr_PCM$threshpar, Thr_PCM$se.thresh)
colnames(Thresholds_Table_Recoded) <- c("Threshold", "SE Threshold")
write.csv(Thresholds_Table_Recoded, file=paste(path_rasch,"PCM_thresholds_CI_Recoded.csv", sep=""))


#-------------------------------------------------------------------------------
###3. ICC plot & person-item map

###save ICC curvees

pdf(paste(path_rasch, "/ICC_curves.pdf", sep=""))
plotICC(model, ask=FALSE)
dev.off()

##save the person-item map
pdf(file=paste(path_rasch, "PImap.pdf", sep=""), width=7, height=9)
plotPImap(model, sorted = TRUE)
dev.off()
#

#-------------------------------------------------------------------------------  
#4. person parameters
set.seed(952017)
person_parameters <- person.parameter(model)

#------------------------------------------------------------------------------- 
#7. Standardized Residuals
Residuals_PCM_Recoded <- residuals(person_parameters)
write.csv(Residuals_PCM_Recoded,file=paste(path_rasch, "Residuals_PCM.csv", sep=""))

#------------------------------------------------------------------------------- 
#8. Person Abilities
person_par <-  person_parameters$theta.table 
names(person_par) <- c("person_pars", "NAgroup", "Interpolated")
table(person_par$Interpolated, is.na(person_par$person_pars))                                                               

data_persons <-  cbind(data_PCM[,c(vars_id,vars_metric)],person_par)
summary(data_persons$person_pars)

for(i in 1:nrow(data_persons)){
  if(is.na(data_persons[i,"person_pars"])){
    data_persons[i,"person_pars"]<-min(data_persons[,"person_pars"], na.rm=TRUE)
  }
}

write.csv(data_persons, paste(path_rasch, "DatawAbilities.csv", sep=""),row.names = FALSE)

###more persons abilities 
Person_Abilities <- cbind(person_parameters$thetapar$NAgroup1,person_parameters$se.theta$NAgroup1)
colnames(Person_Abilities) <- c("Abil", "SE_Abil")
write.csv(Person_Abilities, file=paste(path_rasch,"PersonPara.csv", sep=""))
###


