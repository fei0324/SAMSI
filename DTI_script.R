library(refund)
DTI_CLEAN = subset(DTI, DTI$visit==1)
DTI.1 = DTI_CLEAN[,-3]
summary(DTI.1$sex)

# Separate the data by gender
femaleData = subset(DTI.1, DTI.1$sex=="female")
maleData = subset(DTI.1, DTI.1$sex=="male")
View(DTI.1)

# Frequency of male and female grouped by cases
Cases = factor(DTI.1$case)
caseByGender = ggplot(DTI.1, aes(x=DTI.1$sex, fill=Cases))
caseByGender + geom_bar() + xlab("Gender") + ylab("Frequency")

# Boxplot of Pasat score grouped by gender
DTI.1$sex = factor(DTI.1$sex, c("female","male"))
Gender = DTI.1$sex
pasatVal = ggplot(DTI.1, aes(x=sex, y=pasat)) + geom_boxplot(aes(colour=Gender)) + xlab("Gender") + ylab("Pasat")
pasatVal + geom_jitter(width = 0.1, aes(color=sex))

# Proportion Test
table(DTI.1$case, DTI.1$sex)
prop.test(x = c(34, 66), n = c(46, 96), correct = FALSE)
# Since p=0.5281, we cannot reject null hypothesis.

# T-test determine if pasat score differs by gender
wilcox.test(femaleData$pasat, maleData$pasat)
# Result in a p-value of 0.8279, cannot reject null hypothesis

# Find the means for all columns of cca
meancca = c()
for (i in 1:93) {
  meancca[[i]] = mean(DTI.1$cca[,i], na.rm = TRUE)
}
meancca
# Find the medians for all columns of cca
mediancca = c(DTI.1[[382,]])
for (j in 1:93) {
  mediancca[[j]] = median(DTI.1$cca[,j], na.rm = TRUE)
}
mediancca

# Control Group
controlGroup = subset(DTI.1, case == 0)

# Patient Group
patientGroup = subset(DTI.1, case == 1)

# Mean for the control group
meancca0 = c()
for (i in 1:42) {
  meancca0[[i]] = mean(controlGroup$cca[,i], na.rm = TRUE)
}
meancca0

# Mean for the patient group
meancca1 = c()
for (i in 1:51) {
  meancca1[[i]] = mean(patientGroup$cca[,i], na.rm = TRUE)
}
meancca1

# Plot the means
dev.new(); par(mfrow=c(2,1))
x = c(1:42)
y = c(1:51)
plot(x, meancca0, xlab="Individual No.", ylab="Mean of Control Group", main="Mean Plot of Control Group")
plot(y, meancca1, xlab="Patient No.", ylab="Mean of Patient Group", main="Mean Plot of Patient Group")

for (i in 1:42) {
  DTI.1[i,6] = 60
}
View(DTI.1)

library(ggplot2)

x = c(1:142)
model1 = lm(DTITrain$pasat~ccaMean + Nscans)
summary(model1)
plot(model1)

m1 = glm(DTITrain$pasat ~ ccaMean + Nscans, family="poisson", data=DTITrain)
summary(m1)
plot(m1)

View(DTITrain)

m2 = glm(DTITrain$pasat ~ ccaMean + sex, family="poisson", data=DTITrain)
summary(m2)
plot(m2)

# Poisson Model
poissonModel = glm(DTITrain$pasat ~ ccaMean + Nscans + sex, family="poisson", data=DTITrain)
summary(m3)
# Estimate Regression Equation
# log(m) = 2.78 + 2.22ccaMean - 0.02Nscans + 0.0013sex

# sum of the squared Pearson residuals.
# Does a Poisson regression model adequately fit the data?
res = residuals(poissonModel,"pearson")
# Goodness of Fit Statistics
fitStats = sum(res^2)
fitStats

# Under the null hypothesis that the poisson model fits the data, the Pearson chi-square (and deviance) statistics have a
# chi-squared 97 distribution. The expected value of this distribution is equal to the degrees of freedom.
# Informally, you can see there is a problem with the fit because the goodness-of-fit statistic is much bigger than 97.
# Here we will calculate the p-value for this test.

# p-value
1-pchisq(fitStats,97)
# Since p-value = 2.22e-16, we reject null hypothesis.
# Conclusion: The poisson model does not fit the data.

# Quasipoison (Poisson regression accounting for overdispersion)
quasipoissonModel = glm(DTITrain$pasat ~ ccaMean + Nscans + sex, data=DTITrain, family=quasipoisson)
summary(quasipoissonModel)

# Test if ccaMean is significantly related to pasat score (F test)
# Null hypothesis: the parameters that we are going to drop are 0
summary(quasipoissonModel)
ccaPoissonModel = glm(DTITrain$pasat ~ ccaMean, data=DTITrain, family=quasipoisson)
summary(ccaPoissonModel)

deviancecca = 294.29
devianceFull = 291.62
paraDropped = 2
overDispersion = 2.64

AllModel = glm(DTITrain$pasat ~ ccaMean + ccaMin + ccaMed, data=DTITrain, family=quasipoisson)
summary(AllModel)

cor(data.frame(ccaMean,ccaMin,ccaMed))













































# Under the null hypothesis that the two parameters dropped from the model are equal to zero, this
# statistic has a F2,97 distribution.

# p-value
1-pf(F.stat,2,97)
# p-value is close to 1, do not reject null hypothesis.
# Conclusion: the parameters dropped are not significant.
