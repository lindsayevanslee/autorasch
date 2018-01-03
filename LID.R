######### LID ------

#------------------------------------------------------------------------------- 
###9. Local Dependency

#Correlation Plot for Local Dependence
LID <- cor(Residuals_PCM_Recoded, use="pairwise.complete", method="pearson")
write.csv(LID, file=paste(path_rasch,"Residual_Correlations.csv",sep=""))

LIDpca <- LID

#print LID above threshold
LIDcutoff <- 0.1

LID[is.na(LID)]=0
LID[(LID < LIDcutoff) | lower.tri(LID)]=0

write.csv(LID, paste0(path_rasch, paste(c("LID_above_",LIDcutoff,".csv"),collapse="")),row.names=TRUE)

###calls a file which makes a visualisation of the LID
source(paste0(path_syntax, "DependencyGraph.r"))


#------------------------------------------------------------------------------- 
###10. Principal component analyis: PCA

PCA <- prcomp(LIDpca,center=TRUE, retx=TRUE)
Eigen_Value <- eigen(LIDpca)$values
Percentage_Eigen_Value <- eigen(LIDpca)$value/sum(eigen(LIDpca)$value)*100 
Cumulative_Percentage_Eigen_Value <- cumsum(Percentage_Eigen_Value)
Eigen_Value_Table <- cbind(Eigen_Value,Percentage_Eigen_Value,Cumulative_Percentage_Eigen_Value)
write.csv(Eigen_Value_Table, file=paste(path_rasch, "Original_Data_Eigenvalues.csv", sep="") )
write.csv(PCA$rotation, file=paste(path_rasch,"Original_Data_PCA.csv", sep="") )

pdf(paste(path_rasch,"Original_Data_Screeplot.pdf", sep=""))
barplot(Eigen_Value, main="metric")
screeplot(PCA, type="lines", main="metric")
dev.off()

#----------------------
### Test LID 


LIDind <- which(LID>0 & LID<1, arr.ind = TRUE)
unique(c(rownames(LID)[LIDind[,1]],colnames(LID)[LIDind[,2]]))

max.values <- data.frame(var=vars_metric,
                         max.val=n.resp.opt-1,
                         stringsAsFactors = FALSE)

if (isTRUE(all.equal(components(finalgraph)$no, 0))) {
  return()
} else {
  #find all components
  comp.index <- 1:components(finalgraph)$no
  
  #turn each component into a testlet -> later implement other strategies
  for (i in comp.index) {
    #capture variables for new testlet
    new.testlet <-
      names(components(finalgraph)$membership)[components(finalgraph)$membership ==
                                                 i]
    
    #create testlet
    data_PCM[, paste(new.testlet, collapse = "_")] <-
      rowSums(data_PCM[, new.testlet], na.rm = TRUE)
    
    #edit list of variables
    vars_metric <- vars_metric[-which(vars_metric %in% new.testlet)]
    vars_metric <- c(vars_metric, paste(new.testlet, collapse = "_"))
    
    #edit data.frame of maximum possible values
    max.values <-
      max.values[-which(max.values$var %in% new.testlet), ]
    max.values <- rbind(max.values,
                        c(
                          var = paste(new.testlet, collapse = "_"),
                          max.val = (n.resp.opt - 1) * length(new.testlet)
                        ))
  }
}



















