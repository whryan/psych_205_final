library(nlme)

library(lattice)

# All the studetns
data(MathAchieve)
# Look at first 10 rows
MathAchieve[1:10,]

# Now organized by school
data(MathAchSchool)
MathAchSchool[1:10,]

# Recalculate the mean of SES for each school
attach(MathAchieve)
mses <- tapply(SES, School, mean) 
mses[as.character(MathAchSchool$School[1:10])]
detach(MathAchieve)

# Now we are making a new data frame that has all the data.
Bryk <- as.data.frame(MathAchieve[, c("School", "SES", "MathAch")])
names(Bryk) <- c("school", "ses", "mathach")
sample20 <- sort(sample(nrow(Bryk), 20)) # 20 randomly sampled students
Bryk[sample20,] 

# Now we add the data that is the same for all schools
Bryk$meanses <- mses[as.character(Bryk$school)]
Bryk$cses <- Bryk$ses - Bryk$meanses
sector <- MathAchSchool$Sector
names(sector) <- row.names(MathAchSchool)
Bryk$sector <- sector[as.character(Bryk$school)]
Bryk[sample20,]

# Now we are going to look at the data

cat <- sample(unique(Bryk$school[Bryk$sector=='Catholic']), 20)
Cat.20 <- groupedData(mathach ~ ses | school, data=Bryk[is.element(Bryk$school, cat),])
pub <- sample(unique(Bryk$school[Bryk$sector=='Public']), 20)
Pub.20 <- groupedData(mathach ~ ses | school, data=Bryk[is.element(Bryk$school, pub),])


trellis.device(color=F)
xyplot(mathach ~ ses | school, data=Cat.20, main="Catholic",
	panel=function(x, y)	{
	panel.xyplot(x, y)
	panel.loess(x, y, span=1)
	panel.lmline(x, y, lty=2)
	}
)

trellis.device(color=F)
xyplot(mathach ~ ses | school, data=Pub.20, main="Public",
	panel=function(x, y)	{
		panel.xyplot(x, y)
		panel.loess(x, y, span=1)
		panel.lmline(x, y, lty=2)
	}
)

# lm list generates a list of linear models - in this case for each school
cat.list <- lmList(mathach ~ ses | school, subset = sector=='Catholic', data=Bryk)

pub.list <- lmList(mathach ~ ses | school, subset = sector=='Public', data=Bryk)

# Plots the intervals for the coefficients

trellis.device(color=F)
plot(intervals(cat.list), main='Catholic')

trellis.device(color=F)
plot(intervals(pub.list), main='Public')

# Make a box plot of coefficients.
# First store the coefficients in a new array
cat.coef <- coef(cat.list)
cat.coef[1:10,]
pub.coef <- coef(pub.list)
pub.coef[1:10,]
# Second, make the box plot
# dev.new()
old <- par(mfrow=c(1,2))
boxplot(cat.coef[,1], pub.coef[,1], main='Intercepts',
	names=c('Catholic', 'Public'))
boxplot(cat.coef[,2], pub.coef[,2], main='Slopes',
	names=c('Catholic', 'Public'))
par(old)


# Now that we understand the data we can fit a hierarchical linear model
bryk.lme.1 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ cses | school, data=Bryk)
summary(bryk.lme.1)


# Did the inclusion of the random effects worked? Let's compare to models without a different intercept for each school and without a different slope.

# First the model without the different slopes
bryk.lme.2 <- update(bryk.lme.1, random = ~ 1 | school) 
anova(bryk.lme.1, bryk.lme.2)

# Next the model without the different intercepts
bryk.lme.3 <- update(bryk.lme.1, random = ~ cses - 1 | school) 
anova(bryk.lme.1, bryk.lme.3)

# When comparing fixed effect you must set the method of Maximum Likelihood "ML" 
# instead of Restricted Maximum Likelihood ("RML")
bryk.lme.1 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ cses | school, data=Bryk, method="ML")
summary(bryk.lme.1)
bryk.lme.4 <- lme(mathach ~ meanses*cses, random = ~ cses | school, data=Bryk, method="ML")
anova(bryk.lme.1, bryk.lme.4)
library(car)
Anova(bryk.lme.1)

# Exercise1.  Compare to the lm model.

bryk.lm <- lm(mathach ~ meanses*cses + sector*cses, data=Bryk)
summary(bryk.lm)
anova(bryk.lme.1, bryk.lm)

# I see that in this case the value of the coefficients and their signficance 
# are very similar. However the mixed-effect models is better for predicting
# the data as shown by the likelihood ratio test.


#
# We are going to do a within subjects ANOVA using the mixed linear model
#

# First get the data.  Change the following line to fit your folder structure
setwd('/Users/frederictheunissen/Documents/Classes/Psych 102/Fall 14/')
load('ALLSUBJSphase2.Rdata')

# The data frame: data.ex5 has results on reaction times in a musical stroop test
head(data.ex5)


# In this test subjects were asked to identify the note shown on a musical partition 
# while a note was played to them simultaneously. 
# Congruence: The played note was either the same note as shown in the partition 
# (Congruent or C) or a different note (Incongruent or I)
# SubjType : Subjects were musicians that had either relative pitch (RP) or absolute pitch (AP)
# BinRefTone : In some trials a reference tone that was played and displayed right before the test (0: no ref tone, 1: with ref tone)
# It was hypothesized that perfect pitch subjects only would be affected (slower response times) by the Incongruent condition without ref tone
# and that both perfect pitch and relative pitch subjects would be affected when the reference tone was present.
# We are therefore interested in the triple interaction between Congruence, Reference Tone and Subject Type
# We are going to take a look at the data per subject
library(lattice)
xyplot(TTime~Congruence:BinRefTone | Subj, data.ex5, groups=SubjType)

# Exercise 2. Test the mixed linear model with random intercept for reaction time and compare the coefficients and the results to the one obtained without including the subject variability.

library(nlme)
miren.lme.1 <- lme( TTime ~ Congruence*BinRefTone*SubjType, random = ~ 1 | Subj, data=data.ex5)
summary(miren.lme.1)

miren.lm.1 <- lm( TTime ~ Congruence*BinRefTone*SubjType, data=data.ex5)
summary(miren.lm.1)

# In this case the within subject analysis allowed us to avoid a false positive!  The linear model finds an effect between subjects that have AP vs RP but the mixed model tells us that that variability can be explained by within subject random effects.

# Exercise 3. Repeat the analysis with the log of the reaction time.
data.ex5$logRT <- log(data.ex5$TTime)

miren.lme.2 <- lme( logRT ~ Congruence*BinRefTone*SubjType, random = ~ 1 | Subj, data=data.ex5)
summary(miren.lme.2)

miren.lm.2 <- lm( logRT ~ Congruence*BinRefTone*SubjType, data=data.ex5)
summary(miren.lm.2)

# The log increased the t-values of the signficant values.










