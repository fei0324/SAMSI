library(refund)
library(ggplot2)
library(caret)
library(lattice)
library(MASS)


data(DTI) #Read it in
DTI_CLEAN = subset(DTI,DTI$visit==1) #First Visit only
DTI_a = DTI_CLEAN[,-3] # Removing Visit Time
DTI_b = DTI_a[,-8] #Removing RCST

#Preprocessed and Cleaned Data Set
DTI.1 = DTI_b



##CCA Metrics

#Mean
cca.iMean = array(NA)
for(i in 1:142){
  cca.ivect=c(DTI.1$cca[i,])
  cca.iMean[i]=mean(cca.ivect, na.rm = TRUE)
}

ccaMean = cca.iMean

#Median
cca.iMedian = array(NA)
for(i in 1:142){
  cca.ivect=c(DTI.1$cca[i,])
  cca.iMedian[i]=median(cca.ivect, na.rm = TRUE)
}

ccaMed = cca.iMedian

#Minimum
cca.iMin = array(NA)
for(i in 1:142){
  cca.ivect=c(DTI.1$cca[i,])
  cca.iMin[i]=min(cca.ivect, na.rm = TRUE)
}

ccaMin = cca.iMin

#Final Data Set:  Including CCA Metrics

DTI.1 = cbind(DTI.1, ccaMean, ccaMed, ccaMin)

#Final Data Set for Exploratory Analysis Missing Value Inputation
#DTI.1

#Data Set for model fitting (MS Patients only)
DTI.2 = subset(DTI.1, as.numeric(DTI.1$case) == 1)
#DTI.2


set.seed(2145) #To Make reproducible
#Take a SRS Based on Outcome

trainIndex = createDataPartition(DTI.2$pasat, p=.7, list= FALSE, times=1)

DTITrain = DTI.2[trainIndex,]
DTITest = DTI.2[-trainIndex,]

#Training data: DTITrain

# Proportion Test
table(DTI.1$case, DTI.1$sex)
prop.test(x = c(34, 66), n = c(46, 96), correct = FALSE)
# Since p=0.5281, we cannot reject null hypothesis.
# Conclusion: Proportions of MS patients do not differ by gender.

# Poisson Model
poissonModel = glm(DTITrain$pasat ~ ccaMean + Nscans + sex, family="poisson", data=DTITrain)
summary(poissonModel)
# Estimate Regression Equation
# log(miu) = 3.06 + 1.37ccaMean + 0.01Nscans + 0.07sex

# sum of the squared Pearson residuals.
# Does a Poisson regression model adequately fit the data?
res = residuals(poissonModel1,"pearson")
# Goodness of Fit Statistics
fitStats = sum(res^2)
fitStats

# Under the null hypothesis that the poisson model fits the data, the Pearson chi-square (and deviance) statistics have a
# chi-squared 97 distribution. The expected value of this distribution is equal to the degrees of freedom.
# Informally, you can see there is a problem with the fit because the goodness-of-fit statistic is much bigger than 97.
# Here we will calculate the p-value for this test.

# p-value
1-pchisq(fitStats,67) #67 is the degrees of freedom of the Residual deviance
# Since p-value = 2.33e-15, we reject null hypothesis.
# Conclusion: The poisson model does not fit the data.

# Quasipoison (Poisson regression accounting for overdispersion)
quasipoissonModel = glm(DTITrain$pasat ~ ccaMean + Nscans + sex, data=DTITrain, family=quasipoisson)
summary(quasipoissonModel)

# Test if ccaMean is significantly related to pasat score (F test)
# Null hypothesis: the parameters that we are going to drop are 0
summary(quasipoissonModel)
ccaPoissonModel = glm(DTITrain$pasat ~ ccaMean, data=DTITrain, family=quasipoisson)
summary(ccaPoissonModel)

deviancecca = 223.91 #Residual deviance of ccaPoissonModel
devianceFull = 218.54 #Residual deviance of the full model
paraDropped = 2 #parameter dropped
overDispersion = 3.004 #Over dispersion parameter from the full model

# Test statistics
F.stat = ((devianceNscans-devianceFull)/paraDropped)/overDispersion
F.stat

# Under the null hypothesis that the two parameters dropped from the model are equal to zero, this
# statistic has a F2,97 distribution.

# p-value
1-pf(F.stat,2,67)
# p-value is close to 1, do not reject null hypothesis.
# Conclusion: the parameters dropped (Nscans and sex) are not significant to the patients' pasat scores.

poissonModel1 = glm(DTITrain$pasat ~ ccaMean + sex, family="poisson", data=DTITrain)
poissonModel2 = glm(DTITrain$pasat ~ ccaMed + sex, family="poisson", data=DTITrain)
poissonModel3 = glm(DTITrain$pasat ~ ccaMin + sex, family="poisson", data=DTITrain)
1-pchisq(fitStats,68)
quasiPoisson1 = glm(DTITrain$pasat ~ ccaMean + sex, family="quasipoisson", data=DTITrain)
quasiPoisson2 = glm(DTITrain$pasat ~ ccaMed + sex, family="quasipoisson", data=DTITrain)
quasiPoisson3 = glm(DTITrain$pasat ~ ccaMin + sex, family="quasipoisson", data=DTITrain)
                    