rm(list=ls())
getwd() # where am I?

crimeData_pre <- as.data.frame(read.csv("CrimeData08.csv"))
crimeData_pre <- crimeData_pre[, -26]
crime_Y <- as.numeric(crimeData_pre[,99])
crime_X <- as.data.frame(crimeData_pre[,1:98])

#summary(crime_X)
#dataset<-as.data.frame(Response = crime_Y, Reg = crime_X)
model <- lm(crime_Y ~ . , data = crime_X) # data set

table <- summary(model)
table2 <- table$coefficients
cat("Number of significant regressors SIGlevel 10%:   ",length(which(table2[,4]<=0.1)) )
cat("Number of significant regressors SIGlevel 5%:    ",length(which(table2[,4]<=0.05)) )

cat("Retain regressors significant for alpha = 5%. Remove all other regressors.")
index_Sig<-as.numeric(which(table2[,4]<0.05))
index_Sig
# If the intercept is significant, then '1' is an element in index_Sig.
# Index_Sig helps in removing non-significant variables. Therefore, remove "1" from index_Sig,
# as it doesn't give any meaningful information.
if(index_Sig[1] == 1)
{
  index_Sig <- index_Sig[-1]
  index_Sig <-index_Sig - 1 
}

index_Sig


# Construct filtered dataset after retaining only significant regressors.
crime_Xnew <- crime_X[,index_Sig]
crime_Ynew <- crime_Y

#Save the filtered data on disk
save_table <- cbind(crime_Ynew, crime_Xnew)
write.csv(save_table, "filtered_CrimeData.csv", row.names = FALSE)

#Further analysis with the filtered data
modelFit <- lm(crime_Ynew ~ . , data = crime_Xnew)
summary(modelFit)

