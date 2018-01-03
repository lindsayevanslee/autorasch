##### MODEL/ITEM FIT ------

#------------------------------------------------------------------------------- 
#5. Person separation index
PSIreport <- as.numeric(SepRel(person_parameters)[1])


#-------------------------------------------------------------------------------  
#6. item.fit
Itemfit <- eRm::itemfit(person_parameters)
table_Itemfit <- as.data.frame(cbind(Itemfit$i.fit, Itemfit$i.df, Itemfit$i.outfitMSQ,   Itemfit$i.infitMSQ,  Itemfit$i.outfitZ,  Itemfit$i.infitZ  ) )
names(table_Itemfit) <- c("i.fit", "i.df", "i.outfitMSQ",   "i.infitMSQ",  "i.outfitZ",  "i.infitZ" )

##additional cut-off for the fit based on Smith (see litterature)
Sample_Size <- nrow(data_orig)
Cut_Infit <- 1+(2/sqrt(Sample_Size))
Cut_Outfit <- 1+(6/sqrt(Sample_Size))

Additional_Row <- as.data.frame(rbind(table_Itemfit,rep(NA, ncol(table_Itemfit)), rep(NA,ncol(table_Itemfit))))
Additional_Row <- apply(Additional_Row,2,as.numeric)

Additional_Row[nrow(Additional_Row)-1,3] <- Cut_Outfit
Additional_Row[nrow(Additional_Row)-1,4] <- Cut_Infit
Additional_Row[nrow(Additional_Row),1] <- PSIreport
rownames(Additional_Row) <- c(vars_metric,"Smith's Critical Cut-Offs", "PSI") 


write.csv(Additional_Row, file=paste(path_rasch,"Item_MSQs.csv", sep=""))
write.xlsx(table_Itemfit, file=paste(path_rasch, "item_fit.xlsx", sep=""))


#------------------------------------------------------------------------------- 
###11. Targetting

#Item Difficulties

Mean_Difficulty <- mean(Thresholds_Table_Recoded[,1], na.rm=TRUE)
SD_Difficulty <- sd(Thresholds_Table_Recoded[,1], na.rm=TRUE)
Mean_Resi_Dif <- mean(Thresholds_Table_Recoded[,2], na.rm=TRUE)
SD_Resi_Dif <- sd(Thresholds_Table_Recoded[,2], na.rm=TRUE)
Target_Difficulty <- cbind(Mean_Difficulty, SD_Difficulty, Mean_Resi_Dif, SD_Resi_Dif)

#Person Abilities  
Mean_Ability <- mean(Person_Abilities[,1])
SD_Ability <- sd(Person_Abilities[,1])
Mean_Resi_Abil <-  mean(Person_Abilities[,2])
SD_Resi_Abil <-  sd(Person_Abilities[,2])
Target_Ability <- cbind(Mean_Ability, SD_Ability, Mean_Resi_Abil, SD_Resi_Abil)


Targetting <- cbind(rbind(Target_Difficulty, Target_Ability), PSIreport)
rownames(Targetting) <- c("Difficulty", "Ability")
colnames(Targetting) <- c("Mean", "SD", "Mean Residuals", "SD Residuals", "PSI")

write.csv(Targetting, file=paste(path_rasch,"Targetting.csv", sep=""))
