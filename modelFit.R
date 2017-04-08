getwd() # where am I?

crimeData_pre <- as.data.frame(read.csv("CrimeData08.csv"))
crimeData_pre <- crimeData_pre[, -26]
crime_Y <- as.numeric(crimeData_pre[,99])
crime_X <- as.data.frame(crimeData_pre[,1:98])

#summary(crime_X)
dataset<-cbind(Response = crime_Y, Reg = crime_X)
model <- lm(crime_Y ~ . , data = crime_X) # data set

table <- summary(model)
table2 <- table$coefficients
index_Sig<-as.numeric(which(table2[,4]<0.05))

# This is the filtered data such that #regressors = 40
crime_Xnew <- crime_X[,index_Sig-1]
crime_Ynew <- crime_Y

#Save the filtered data on disk
save_table <- cbind(crime_Y, crime_Xnew)
write.csv(save_table, "filtered_CrimeData.csv", row.names = FALSE)

#Further analysis with the filtered data
modelFit <- lm(crime_Y ~ . , data = crime_Xnew)

