#Reminder: LME 4 is better ultimately
#the two things to get are lmerTest and lme4

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
#incorrect in the base data
attach(MathAchieve)
mses <- tapply(SES, School, mean) 
mses[as.character(MathAchSchool$School[1:10])] #giving this vector names - cool
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
names(sector) <- row.names(MathAchSchool) #this will maybe be convenient at some point
#taking the names
#we are given each of them a name
Bryk$sector <- sector[as.character(Bryk$school)] #turning the numerical values into strings
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
#Observe that there are different slopes and different intercepts for different schools
#You will get a better fit if you use random/mixed effects models than if you don't
#also see hwo impt it is to replicate - although you had a lot of students, it seems like school 
#has a huge effect on how this works


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
# put tilde to stick everything else elsewhere. Remember that this#will change both tihe intercept and the slot
#random =  ~cses is the same as random = ~ 1 + cses
summary(bryk.lme.1)
#AIC and BIC are very similar here 
#AIC = 2*-logL + # of parameters in model * 2
#10 parameters in the model, and they are off from 2*-log L by 20
#so that's how we figured out the weighting
#think of negative log likelihood as the error
# discussion: https://stats.stackexchange.com/questions/577/is-there-any-reason-to-prefer-the-aic-or-bic-over-the-other
#BIC - 2(-logL + k*logn) n 
#- total number of rows, not number of levels in grouping variable

#This corresponds to the plots we made above - the variance etc it is describing are shown in those graphs. The part below shows the spread of the intercept and slopes across the schools
#this is four of the fit parameters
#Random effects:
#  Formula: ~cses | school
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev    Corr  
#(Intercept) 1.5426166 (Intr)
#cses        0.3182103 0.391 
#Residual    6.0597952 - standard deviation of residual.
                        #Gives an idea of how good your model is
#compare the residual to the range which your variable can take on
#if it' really big, then you might have some issues


#these are the last 6 parameters
#Fixed effects: mathach ~ meanses * cses + sector * cses 
#Value Std.Error   DF  t-value p-value
#(Intercept)         12.127931 0.1992921 7022 60.85505   0e+00
#meanses              5.332875 0.3691687  157 14.44563   0e+00
#cses                 2.945041 0.1556008 7022 18.92690   0e+00
#sectorCatholic       1.226579 0.3062736  157  4.00485   1e-04
#meanses:cses         1.039229 0.2988976 7022  3.47687   5e-04
#cses:sectorCatholic -1.642674 0.2397803 7022 -6.85074   0e+00

# If in catholic school, intercept increases, slope decreases
# so differences in SES appear to matter less when you are in a catholic school
# schools with higher mean ses see that there is more of an effect of cses


#correlation table on your variables woul dgive you the correlation table result
#this helps you check for colinearity
#Correlation: 
#  (Intr) meanss cses   sctrCt mnss:c
#meanses              0.256                            
#cses                 0.075  0.019                     
#sectorCatholic      -0.699 -0.356 -0.053              
#meanses:cses         0.019  0.074  0.293 -0.026       
#cses:sectorCatholic -0.052 -0.027 -0.696  0.077 -0.351

#takes residuals by school, z scores, and then shows you what the range of
#them are. Any obvious deviations from normality woul dappear here
#median should be close to 0
#q1 and q3 should be close to 0.69
#min and max shouldn't be something insanely huge
#Standardized Within-Group Residuals:
#  Min         Q1        Med         Q3        Max 
#-3.1592610 -0.7231893  0.0170471  0.7544512  2.9582207

#Fixed effect - if studying it and have all possible cases, then it should
# be a fixed effect? 
# but you never actually have all of the possible cases
#stack exchange answer: https://stats.stackexchange.com/questions/4700/what-is-the-difference-between-fixed-effect-random-effect-and-mixed-effect-mode
#davids stats class answer: http://statweb.stanford.edu/~jtaylo/courses/stats203/notes/fixed+random.pdf
#gelman answer: http://andrewgelman.com/2005/01/25/why_i_dont_use/



# Did the inclusion of the random effects worked? Let's compare to models without a different intercept for each school and without a different slope.

# First the model without the different slopes
bryk.lme.2 <- update(bryk.lme.1, random = ~ 1 | school) 
anova(bryk.lme.1, bryk.lme.2)

# Next the model without the different intercepts
bryk.lme.3 <- update(bryk.lme.1, random = ~ cses - 1 | school) 
anova(bryk.lme.1, bryk.lme.3)
#if nothing is significant, you expect this to be al the same
#about identical sizes of differences between them
#simple summary - p < .05 means there is a significant difference, if not, there isn't



# When comparing fixed effect you must set the method of Maximum Likelihood "ML" 
# instead of Restricted Maximum Likelihood ("RML")
#RML is the standard method for comparison when you are doing mixed effects 
bryk.lme.1 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ cses | school, data=Bryk, method="ML")
summary(bryk.lme.1)
bryk.lme.4 <- lme(mathach ~ meanses*cses, random = ~ cses | school, data=Bryk, method="ML")
anova(bryk.lme.1, bryk.lme.4)
library(car)
Anova(bryk.lme.1)

#in general, lme4 is similar, except you put the ( 1 | variable) etc specification into the 
#formula directly

# Exercise1.  Compare to the lm model.
# Exercise 1A - Original version from last week
bryk.lm.1 <- lm(mathach ~ meanses*cses + sector*cses,  data=Bryk)
summary(bryk.lm.1)
summary(bryk.lme.1)
anova(bryk.lme.1, bryk.lm.1)
#the coefficients are similar between models
#however, based on an anova, the lm model is overall worse than the MLM - use MLM over the fixed effect model
#note - here we are using a different lme model than below - here I am using the second one 
#which was specified instead of the first, which is what we used below in the second section


#Exercise 1B - New version from 4/13 section
bryk.lme.1 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ cses | school, data=Bryk)

bryk.lm.1 <- lm(mathach ~ meanses*cses + sector*cses, data=Bryk)
summary(bryk.lm.1)
summary(bryk.lme.1)
#note that degrees of freedom are different between the two, slightly, because they are
#treated as having different levels
#this doesn't matter much here, but in many other situations it might effect things more

#Random effects:
#  Formula: ~cses | school
#Structure: General positive-definite, Log-Cholesky parametrization
#StdDev    Corr  
#(Intercept) 1.5219305 (Intr)
#cses        0.2556119 0.483 
#Residual    6.0598006 
#We might be interested in this, however -- gives us an idea of how you quantify more
# the effects of difft fixed effects

#compare coefficients
lm_co = coef(bryk.lm.1)
lme_co = fixef(bryk.lme.1)
data.frame(lm_co, lme_co)
#quite similar
anova(bryk.lme.1, bryk.lm.1)
#Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#bryk.lme.1     1 10 46516.43 46585.23 -23248.21                        
#bryk.lm.1      2  7 46727.12 46775.27 -23356.56 1 vs 2 216.6866  <.0001
#p value is smaller for the lm() model
#NOTE: It really IS NOT clear that this is necessarily working correctly, since order matters
#here, which doesn't really make a lot of sense
#but it does look like the random effects helped, so this was explaining a significant portio of variance

#RLR sim is a useful package if you have only one random effect
require(RLRsim)
#but you can't use it here because you have multiple random effects
exactLRT(m=bryk.lme.1, m0=bryk.lm.1)
#this would be the normal way to identify significance of that single effect, but you can't 
#do it in this case because we have two random effects in our model

#Note- some people think that you shouldn't include if it isn't significant.Idea is that if the
#intercept isn't significant, then you aren't really going to get a level two sinificant
# ie with school, it would be telling you that there isn't really school level variation
#But it isn't clear if this is actually backed up by literature, and that control should still
#be there because it is still accurately reflecting the data?
# we are sort of unclear on what exactly this means as a group- but it seems like maybe we
# should be using it?
#IT seems like this would be something worth looking into, but it seems like usually as a rule
#people do this with nested data because it is nested data


#
# We are going to do a within subjects ANOVA using the mixed linear model
#

# First get the data.  Change the following line to fit your folder structure
#learing things out
rm(list=ls())
setwd('C:/Users/William/Documents/Berkeley_Post-Bac/01 - Courses/Spring 2018 Classes/Psych 205/homework/tutorials/')
load('ALLSUBJSphase2.Rdata')

# The data frame: data.ex5 has results on reaction times in a musical stroop test
head(data.ex5)


#You can think about this as similar to the stroop task - they are shown a sound and then are played one, and you want to see if that changes
#their perception of the heard tone
#there are perfect and non perfect pitch people

# In this test subjects were asked to identify the note shown on a musical partition 
# while a note was played to them simultaneously. 
# Congruence: The played note was either the same note as shown in the partition 
# (Congruent or C) or a different note (Incongruent or I)
# SubjType : Subjects were musicians that had either relative pitch (RP) or absolute pitch (AP)
# BinRefTone : In some trials a reference tone that was played and displayed right before the test (0: no ref tone, 1: with ref tone)
# It was hypothesized that perfect pitch subjects only would be affected (slower response times) by the Incongruent condition without ref tone
# and that both perfect pitch and relative pitch subjects would be affected when the reference tone was present.

#so relative pitch makes you immune to being screwed up by the wrong note visually. With perfect pitch you are screwed up by the note visually. With the 
#reference tone both are screwed up

# We are therefore interested in the triple interaction between Congruence, Reference Tone and Subject Type
# We are going to take a look at the data per subject

#I just need to fix a mistake I made at the end of today's #c#lass. The music troop test experiment hypothesises a three #way interaction, and I made the mistake of looking at the p #value for the coefficient on the three way interaction term as #testing this hypothesis. But that was wrong. The correct test #of the three-way interaction is a model comparison between 1) #a model including the main effects and two way interactions 2) #the full model. The anova() function will allow this. See if #you can do this at home.

#ie compare DV ~ X*Y + Y*Z + Z*X to DV ~ X*Y*Z


library(lattice)
xyplot(TTime~Congruence:BinRefTone | Subj, data.ex5, groups=SubjType)
boxplot(TTime ~ Congruence*SubjType*BinRefTone, data=data.ex5, notch=TRUE, ylim=c(1,20000))
#best solution might still be just doing it manually where you calculate all the stats yourself if you want smth else
#gives us some hope for our theory
#C v I is congruent incongruent
#0 or 1 is ref tone
#r p is relative pitch a p is perfect pitch

# Exercise 2. Test the mixed linear model with random intercept for reaction time and compare the coefficients and the results to the one obtained without including the subject variability.

library(nlme)

#Exercise 2A - from prior to 4/13 section
new_mlm =  lme(TTime  ~Congruence*BinRefTone*SubjType, random= ~1|Subj, data=data.ex5)
summary(new_mlm)
anova(new_mlm)
new_lm =  lm(TTime  ~Congruence*BinRefTone*SubjType, data=data.ex5)
summary(new_lm)
anova(new_lm)

#Exercise 2B - from this section, 4/13
music_mod1 = lme(TTime ~ Congruence*BinRefTone*SubjType, random = ~1|Subj,data=data.ex5)
music_mod0 = lm(TTime ~ Congruence*BinRefTone*SubjType, data=data.ex5)
anova(music_mod1, music_mod0)
exactLRT(m=music_mod1, m0 = music_mod0)
#it looks liek the random intercept did help the model - ie a significant amount of error can be attributed to differences between subjects

#let's compare everything but 3 ways vs 3 ways

music_mod1 = lme(TTime ~ Congruence*BinRefTone*SubjType, random = ~1|Subj,data=data.ex5, method="ML")
music_mod2 = lme(TTime ~ Congruence*BinRefTone + Congruence*SubjType + BinRefTone*SubjType, random = ~1|Subj, data=data.ex5, method="ML")
anova(music_mod1, music_mod2)
#this is not significant... seems odd

require(lme4)
mm1 = lmer(TTime ~ Congruence*BinRefTone*SubjType + (1|Subj), data=data.ex5)
mm2 = lmer(TTime ~ Congruence*BinRefTone + Congruence*SubjType + BinRefTone*SubjType + (1|Subj), data=data.ex5)
anova(mm1, mm2)
#redone with lme4 it still doesn't show significance

# Exercise 3. Repeat the analysis with the log of the reaction time.

#3A - done with the method prior to the 4/13 section
data.ex5$logRT <- log(data.ex5$TTime)
e3_mlm =  lme(logRT  ~Congruence*BinRefTone*SubjType, random= ~1|Subj, data=data.ex5)
summary(e3_mlm)
anova(e3_mlm)
e3_lm =  lm(logRT  ~Congruence*BinRefTone*SubjType, data=data.ex5)
summary(e3_lm)
anova(e3_lm)
#you find that the 3 way interaction is significant with a fixed effect model but not with an mlm - so, without accounting for participant variability you can get a false positive using just the fixed effects model. Once you have accounted for participant variability appropriately, the effect goes away. 

#3B - done with the method after the 4/13 section

#Using lme4
require(lme4)
ex3_mm1 = lmer(logRT ~ Congruence*BinRefTone*SubjType + (1|Subj), data=data.ex5)
ex3_mm2 = lmer(logRT ~ Congruence*BinRefTone + Congruence*SubjType + BinRefTone*SubjType + (1|Subj), data=data.ex5)
(anova(ex3_mm1, ex3_mm2))


music_mod1 = lme(logRT ~ Congruence*BinRefTone*SubjType, random = ~1|Subj,data=data.ex5)
music_mod0 = lm(logRT ~ Congruence*BinRefTone*SubjType, data=data.ex5)
anova(music_mod1, music_mod0)
exactLRT(m=music_mod1, m0 = music_mod0)
#now it looks significant again between lm and lme

music_mod1 = lme(logRT ~ Congruence*BinRefTone*SubjType, random = ~1|Subj,data=data.ex5, method="ML")
music_mod2 = lme(logRT ~ Congruence*BinRefTone + Congruence*SubjType + BinRefTone*SubjType, random = ~1|Subj, data=data.ex5, method="ML")
anova(music_mod1, music_mod2)
#doesn't look much better even with things logged! - tho it is MORE significant


###########################################################################################
#
# SECOND VERSION OF THE FILE GOES HERE
#
# Mixing between the two files was confusing me, so I just rm(list=ls()) here to clear
# everything and then go through the new stuff. But the exercises are only above
#
###########################################################################################

#clear everything. below this point is just for reference
rm(list=ls())


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
bryk.lme.1 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ (1 + cses) | school, data=Bryk)
summary(bryk.lme.1)


# Did the inclusion of the random effects worked? Let's compare to models without a different intercept for each school and without a different slope.

# First the model without the different slopes
bryk.lme.2 <- update(bryk.lme.1, random = ~ 1 | school) 
anova(bryk.lme.1, bryk.lme.2)

# Next the model without the different intercepts
bryk.lme.3 <- update(bryk.lme.1, random = ~ cses - 1 | school) 
anova(bryk.lme.1, bryk.lme.3)

# When comparing fixed effect you must set the method of Maximum Likelihood "ML" 
# instead of Restricted Maximum Likelihood ("REML")
bryk.lme.1 <- lme(mathach ~ meanses*cses + sector*cses, random = ~ cses | school, data=Bryk, method="ML")
summary(bryk.lme.1)
bryk.lme.4 <- lme(mathach ~ meanses*cses, random = ~ cses | school, data=Bryk, method="ML")
anova(bryk.lme.1, bryk.lme.4)
# There is a difference between Public and Catholic Schools
library(car)
Anova(bryk.lme.1)

# Using lme4
library(lme4)
bryk.lmer.1 <- lmer(mathach ~ meanses*cses + sector*cses + (cses | school), data=Bryk)
summary(bryk.lmer.1)
bryk.lmer.4 <- lmer(mathach ~ meanses*cses + (cses | school), data=Bryk)
anova(bryk.lmer.1, bryk.lmer.4)

# Using cross-validation
my.Bryk <-na.omit(Bryk[sample(nrow(Bryk)),])


# Setting folds but using only one - note this is crude - you might want to
# preserve the fraction in each school.
n.folds <- 10
folds <- cut(seq(1,nrow(my.Bryk)),breaks=n.folds,labels=FALSE)
testIndexes <- which(folds==1,arr.ind=TRUE)
testData <- my.Bryk[testIndexes, ]
trainData <- my.Bryk[-testIndexes, ]
my.bryk.lmer.1 <- lmer(mathach ~ meanses*cses + sector*cses + (cses | school), data=trainData)
ma.pred.1 <- predict(my.bryk.lmer.1, newdata = testData)
(ma.mse.1 <- mean((ma.pred.1-testData$mathach)**2))

my.bryk.lmer.4 <- lmer(mathach ~ meanses*cses + (cses | school), data=trainData)
ma.pred.4 <- predict(my.bryk.lmer.4, newdata = testData)
(ma.mse.4 <- mean((ma.pred.4-testData$mathach)**2))

# Slightly better.

# Exercise1.  Compare to the lm model.




#
# We are going to do a within subjects ANOVA using the mixed linear model
#

# First get the data.  Change the following line to fit your folder structure

setwd('/Users/frederictheunissen/Documents/Classes/Psych 205/Spring 18/')
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

# Exercise 3. Repeat the analysis with the log of the reaction time.
data.ex5$logRT <- log(data.ex5$TTime)















