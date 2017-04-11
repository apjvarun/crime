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
}
index_Sig <-index_Sig - 1 

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

# Now, some plots related to the model fit. Residual plots, Q-Q plots, etc.
# Execute the next statement and press enter to see plots back to back.
plot(modelFit)


## Residual Analysis



plot(model$fitted.values,modelFit$residuals)

## **********************************************************************************************
## Multicollinearity Analysis

### VIF: Variable selection
library(usdm)
#step - 1
df = crime_Xnew

flag = 1
while(flag) 
{
  vif_table <-vif(df)
  
  if(max(vif_table[,2])>10)
  {
    flag = 1
    df<-df[,- which.max(vif_table[,2])]
  }
  else
    flag = 0
}
vif_table

crime_Xnew_vif <- df
model_vif<- lm(crime_Ynew ~ .,data=crime_Xnew_vif )
#summary(model_vif)

"Rsquare adjusted"
rbind(c("actual model", "reduced model", "after VIF"), c(summary(model)$adj.r.squared, summary(modelFit)$adj.r.squared, summary(model_vif)$adj.r.squared) )
"Rsquare"
rbind(c("actual model", "reduced model", "after VIF"), c(summary(model)$r.squared, summary(modelFit)$r.squared, summary(model_vif)$r.squared) )


############################### Variance Decomposition method for variable selection*****************************
X_prod = t(as.matrix(crime_Xnew))%*%as.matrix(crime_Xnew)
evals = eigen(X_prod)$values
evecs = eigen(X_prod)$vectors

VDmatrix <- matrix(, nrow = length(evals), ncol = length(evals))
for( j in 1:length(evals)) #regressors
{
  sumCol_VD <-0
  for (pappu in 1:length(evals))
  {
    sumCol_VD<- sumCol_VD + (1/evals[pappu])*evecs[pappu,j]^2
  }
  for (k in 1:length(evals)) #eigenvalues
  {
    VDmatrix[k,j] <- (1/evals[k])*evecs[k,j]^2/sumCol_VD
  }
} #Verify that colSums of VDmatrix is 1.

colSums(VDmatrix)
rowSums(VDmatrix)

#construct condition index
root_evals <- sqrt(evals)
condition_index<- max(root_evals)/root_evals
# Analysis of variance decomposition matrix
rounded_VDmatrix <- round(as.data.frame(VDmatrix),2)
#Conclusions: (x20,x18); (x24,x25)


###******************************************************************************************
## Variable Selection : forward, backward, stepwise

null=lm(crime_Ynew~1, data = crime_Xnew)
null
full=lm(crime_Ynew~ ., data = crime_Xnew)
full
#this is only for AIC based forward selection
step(null, scope=list(lower=null, upper=full), direction="forward")

# rms library for backward selection
library(rms)
xnam <- paste("x", 1:25, sep="")
fmla <- as.formula(paste("crime_Ynew ~ ", paste(colnames(crime_Xnew), collapse=" + ")))

#Automated F-test-based backward selection using rms::fastbw()
ols.full<- ols(fmla, data = crime_Xnew)
fastbw(ols.full, rule = "p", sls = 0.1)




#Manual F-test-based forward selection ****test

lm.full <- lm(crime_Ynew ~ ., data = crime_Xnew)
lm.null <- lm(crime_Ynew ~ 1, data = crime_Xnew)
lm.base<-lm.null

reglist<- colnames(crime_Xnew)

flag_fwsel<- 1
while(flag_fwsel)
{

  tempfmla <- as.formula(paste("crimeY_new ~ ", paste(reglist, collapse=" + ")))
  temp<-add1(lm.base, tempfmla , test = "F")
  cat("display F-scores")
  temp$`F value`
  
  if(max(temp$`F value`[2:length(temp$`F value`)]) > 10) #F0 can be chosen accordingly.
  {
    fmla_updt<-as.formula(paste("~  . + ", rownames(temp)[which.max(temp$`F value`)]))
    
    cat("maximum F value found",max(temp$`F value`[2:length(temp$`F value`)]))
    
    fmla_updt #print
    #update for the next step
    lm.base <- update(lm.base,  fmla_updt)
    #update remaining variables
    reglist <- reglist[-which(reglist== rownames(temp)[which.max(temp$`F value`)] )]
  }
  else
    flag_fwsel <- 0
}
#* I tried implementing forward selection using add1(), but some error is there. Will have to debug!




## Miscellaneous

