#############################################################
##  In this tutorial, we are going to learn to assess the 
## goodnes of fits of models and to compare models
## with modern cross-validation approaches.

# We are now going to practice doing the statistics for linear models using the F-statistic.
library(car)

# Remember the job prestige data has a mixture of numeric predictors (income, education, women) and factors (group)

# Always take a peak at the data and look at the sample size
head(Prestige)
nrow(Prestige)

# Always visualize your data
scatterplotMatrix(~ prestige + log2(income) + education + women | type, data=Prestige, by.group=TRUE, id.n=0,
                  smooth=FALSE, col=gray(c(0,0.5,0.7)))

# Some of the rows have missing data.  Since we are going to compare models using
# our own functions, we are going to eliminate rows with NA.
is.na(Prestige)

Prestige.clean <- Prestige[rowSums(is.na(Prestige)) == 0,]
# Our favorite model is an ANOCOVA where type as a factor and education and log(income) as covariates  
prestige.mod.full <- lm(prestige ~ education*type + log2(income)*type,
                      data=Prestige.clean)
(prestige.mod.full.sum <- summary(prestige.mod.full))

prestige.mod.ed <- lm(prestige ~ education, data=Prestige.clean)
(prestige.mod.ed.sum <- summary(prestige.mod.ed))

aov.results <- anova(prestige.mod.ed, prestige.mod.full)

# The partial R2
(R2.ed.vs.full <- 1 - (aov.results$RSS[2]/aov.results$Res.Df[2])/(aov.results$RSS[1]/aov.results$Res.Df[1]))

######################  Model Validation with Resampling #########
# Cross-validation with bootstrap estimates of R2.
# We are now going to perform a 10-fold cross validation on the prestige
# data frame to get our own value of R2.

# First we randomly shuffle the data and omit na rows.
n.folds <- 10

#break up the first ten rows etc just in case there is something going on here
my.Prestige <-na.omit(Prestige[sample(nrow(Prestige)),])


# Next, we create 10 equally size folds
folds <- cut(seq(1,nrow(my.Prestige)),breaks=n.folds,labels=FALSE)



########################################################
#   Exercise 1.  Complete the following loop to get n.folds value
# of cross validated R2 (one for each fold) for the full model and the model with Education only compared to
# the zeroth order model and the full model compared to the education model.  
# Compare the mean of those cv-R2 to the adjusted R2 you got in the summary
# or to the adjusted R2 you used to calculate the error decrease
# going from model 1 to model 2.


# Make space for your arrays
R2.cv.full <- array(data=0, dim = n.folds)
R2.cv.ed <- array(data=0, dim = n.folds)
R2.cv.ed.vs.full <- array(data=0, dim=n.folds)

#Perform n.folds fold cross validation
for(i in 1:n.folds){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE) #gives all indices where folds = i
    testData <- my.Prestige[testIndexes, ]
    trainData <- my.Prestige[-testIndexes, ]
    
    #Your code here
    # Fit the two models 
    
    #full model
    prestige.mod.full <- lm(prestige ~ education*type + log2(income)*type,data=trainData)
    #education only model
    prestige.mod.ed <- lm(prestige ~ education, data=trainData)
    #null model
    prestige.mean.train = mean(trainData$prestige)
    ######################  Model Validation with Resampling #########

    # Get predictions
    prestige_hat_full = predict.lm(prestige.mod.full, testData)
    prestige_hat_ed = predict.lm(prestige.mod.ed, testData)

    # Calculate cv R2.
    ss2 = sum((testData$prestige - prestige_hat_full)^2)
    ss1 = sum((testData$prestige - prestige_hat_ed)^2) #ss1 will be from the less complex model, in this case ed
    ss0 = sum((testData$prestige - prestige.mean.train)^2)
    
    R2.cv.full[i] = 1-ss2/ss0
    R2.cv.ed[i] = 1 - ss1/ss0
    R2.cv.ed.vs.full[i] = 1 - ss2/ss1
}

# More summary code here
#mean(R2.cv.ed) #.73
#(prestige.mod.ed.sum) #.75
sprintf('Ed Model: R2 cv = %.2f +- %.3f vs R2 adj= %.2f', mean(R2.cv.ed), sd(R2.cv.ed), prestige.mod.ed.sum$adj.r.squared)

#mean(R2.cv.ed.vs.full) #.36
#R2.ed.vs.full #.44
sprintf('Compared Models: R2 cv = %.2f +- %.3f vs R2 adj= %.2f', mean(R2.cv.ed.vs.full), sd(R2.cv.ed.vs.full), R2.ed.vs.full)

#mean(R2.cv.full) #.84
#prestige.mod.full.sum #.86
sprintf('Full Model: R2 cv = %.2f +- %.3f vs R2 adj= %.2f', mean(R2.cv.full), sd(R2.cv.full), prestige.mod.full.sum$adj.r.squared)

#this lets you get the mean of a statistic, as well as the standard error of that statistic (ie sd in this case)

#IF R2 cv is different from 0, then you know. Use your standard error - 2*standard error (ie standard deviation) is 5%. So take the R2 cv and subtract sd(R2cv)*2 (remember sd(R2cv)=SE)

#Rule of thumb -- need a minimum of 10 df on your testing df -- ie if you are training one with 5 you need 50 

#This is another way to visualize these statistics -- it lets you see the 95% confidence interval and mean of your cross validated R2 estimates easily
#if the tail hit 0 then that would be bad!
boxplot(cbind(R2.cv.full, R2.cv.ed, R2.cv.ed.vs.full))


#  END of Exercise 1.

###################################
# Permutation test example from Text book. 
###################################
# Our second example is the permutation test from the book.
# In this case we are interrested in determining whether a model for salary prediction fitted on men works for women as well.
library(car)
set.seed(12345) # to reproduce results in the text

# Let's look at the data format
some(Salaries)
nrow(Salaries)

# let's relevel the factor rank
Salaries$rank <- relevel(Salaries$rank, ref="AsstProf")

# Making a table to see how many subjects in each category
# xtabs make a table and ftable "flattens" the table
ftable(x1 <- xtabs(~ discipline + rank + sex, data=Salaries))
round(100*ftable(prop.table(x1, margin=c(1, 2))), 1) # % m and f

# We are plotting the data
library(lattice)
xyplot(salary ~ yrs.since.phd | discipline:rank, group=sex,
  data=Salaries, type=c("g", "p", "r"), auto.key=TRUE)

bwplot(salary ~ discipline:sex | rank, data=Salaries,
    scales=list(rot=90), layout=c(3, 1))

# Generate a model for Males only
fselector <- Salaries$sex == "Female" # TRUE for females
salmod <- lm(salary ~ rank*discipline + yrs.since.phd, data=Salaries,
    subset=!fselector) # regression for males
    
# predictions for females:
femalePreds <- predict(salmod, newdata=Salaries[fselector, ])
(meanDiff <- mean(Salaries$salary[fselector] - femalePreds))

# Let's do a bootstrap to see how often we could get this result
set.seed(8141976) # for reproducibility
fnumber <- sum(fselector) # number of females
n <- length(fselector) # number of observations
B <- 2 # number of replications
simDiff <- numeric(B) # initialize vector with B entries
for (j in 1:B){
    sel <- sample(1:n, fnumber) # random sample of nominated 'females'
    m2 <- update(salmod, subset=-sel) # refit regression model
    simDiff[j] <- mean(Salaries$salary[sel]
        - predict(m2, newdata=Salaries[sel, ])) # compute mean diff.
    }
    
# Calculate the p-value    
(frac <- round(sum(meanDiff > simDiff)/(1 + B), 3))

# Plot the histogram
hist(simDiff,
   main=paste("Histogram of Simulated Mean Differences\np-value =",
       frac),
   xlab="Dollars")
abline(v=meanDiff, lty="dashed")



#######################################
# Our last example examines overfitting.
#######################################

# We are now going to do an example with data from the Theunissen lab.
# In the data, we quantify the "timbre" of a musical instrument with the timbre
# column, and various acoustic features of the sound in the columns labeled
# sound.1, sound.2, etc. 
library(car)

# First, change this path to the path where the file mds1PCA40.txt lives:
setwd('C:/Users/William/Documents/Berkeley_Post-Bac/01 - Courses/Spring 2018 Classes/Psych 205/homework')

# Then read the file and print out the number of rows
Timbre <- read.table('mds1PCA40.txt')
(n.inst <- nrow(Timbre))

# Check out a summary of the data. It's all numerical! 
summary(Timbre)

######################################
# EXERCISE 2
# Use Scatter plot matrix to visualize the data.  Use only the first 3 sound features, specifically
# sound.1, sound.2, and sound.3.
######################################
#for everything
#two methods of getting it for sound 1-3
scatterplot.matrix(Timbre[,2:4])
scatterplotMatrix(~ sound.1 + sound.2 + sound.3, data=Timbre, by.group=TRUE, id.n=0, smooth=FALSE)


### End of 2


# We are now going to predict timbre and keep track of R-square values as a function of the number of parameters.


#Commented out so it doesn't screw up my code
#Rvals <- numeric(40)
#Rvals.adj  <- numeric(40)
#timbre.mod <- lm(timbre ~ sound.1, data=Timbre)
#(timbre.sum <- summary(timbre.mod))
#Rvals[1] <- timbre.sum$r.squared
#sserror <- sum(timbre.mod$residual^2)
#mean.timbre <- mean(timbre.mod$model$timbre)
#sstotal <- sum((timbre.mod$model$timbre - mean.timbre)^2)
#dferror <- timbre.mod$df.residual  # n - k -1
#dftotal  <- length(timbre.mod$fitted.values)-1 # n - 1
#Rvals.adj[1]  <- 1- ((sserror/dferror)/(sstotal/dftotal))

#i <- 2
#timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", i))
#timbre.sum <- summary(timbre.mod)
#Rvals[i] <- timbre.sum$r.squared
#sserror <- sum(timbre.mod$residual^2)
#dferror <- timbre.mod$df.residual  # n - k -1
#Rvals.adj[i]  <- 1- ((sserror/dferror)/(sstotal/dftotal))


################################################
# EXERCISE 3
# Compute and plot the R2 (obtained from the model) and the adjusted R square (as a red line) as a function of the number of sound parameters
# used to model timbre perception.  To do this, you can use the code above inside of a
# for loop, which iterates 40 times because there are 40 sound predictors.
################################################

#Method 1
parameter_ct = 40

Rvals <- numeric(parameter_ct)
Rvals.adj  <- numeric(parameter_ct)
timbre.mod <- lm(timbre ~ sound.1, data=Timbre)
(timbre.sum <- summary(timbre.mod))
Rvals[1] <- timbre.sum$r.squared
sserror <- sum(timbre.mod$residual^2)
mean.timbre <- mean(timbre.mod$model$timbre)
sstotal <- sum((timbre.mod$model$timbre - mean.timbre)^2)
dferror <- timbre.mod$df.residual  # n - k -1
dftotal  <- length(timbre.mod$fitted.values)-1 # n - 1
Rvals.adj[1]  <- 1- ((sserror/dferror)/(sstotal/dftotal))

for(i in 2:40){
  timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", i))
  timbre.sum <- summary(timbre.mod)
  Rvals[i] <- timbre.sum$r.squared
  sserror <- sum(timbre.mod$residual^2)
  dferror <- timbre.mod$df.residual  # n - k -1
  Rvals.adj[i]  <- 1- ((sserror/dferror)/(sstotal/dftotal))
}



#Method 2
#IMO this is a better method
#Rvals <- numeric(parameter_ct)
#Rvals.adj  <- numeric(parameter_ct)
#for(i in 1:parameter_ct){
  #timbre.mod <- lm(timbre ~ [1:i], data=Timbre)
  #(timbre.sum <- summary(timbre.mod))
  #Rvals[i] <- timbre.sum$r.squared
  #sserror <- sum(timbre.mod$residual^2)
  #mean.timbre <- mean(timbre.mod$model$timbre)
  #sstotal <- sum((timbre.mod$model$timbre - mean.timbre)^2)
  #dferror <- timbre.mod$df.residual  # n - k -1
  #dftotal  <- length(timbre.mod$fitted.values)-1 # n - 1
  #Rvals.adj[i]  <- 1- ((sserror/dferror)/(sstotal/dftotal))
#}

#basic plot
plot(Rvals)
lines(Rvals.adj, col="red")

#nicer plot
plot.new()
plot(Rvals, type="l", col="blue", xlab="Number of predictors", ylab="r squared", main="Comparison of R2 (blue) and R2adj (red)")
lines(Rvals.adj, col="red")
legend(1, 1, legend=c("R2", "R2 adj"),
       col=c("blue", "red"), lty=1, cex=0.8)
################################################
# EXERCISE 4 
# Calculate by "hand" the R-square obtained for a model with 3
# sound parameters. Hint: You have done this on your previous tutorial. Use
# the $residuals of the model timbre ~ sound.1 + sound.2 + sound.3.
###############################################
sound1to3 <- lm(timbre ~ sound.1 + sound.2 + sound.3, data=Timbre)

summary(sound1to3)
#Multiple R-squared:  0.7506,	Adjusted R-squared:  0.7309 

meanmod = lm(timbre ~1, data=Timbre)
basemod = lm(timbre ~ sound.1 + sound.2 + sound.3, data=Timbre)
SSEmeanmod = sum(meanmod$residuals^2)
SSEbasemod = sum(basemod$residuals^2)
SSEmeanmod-SSEbasemod

R2_model = 1 - SSEbasemod/SSEmeanmod

R2_adj_model = 1 - (SSEbasemod/(42-3-1))/(SSEmeanmod/(42-1))

###############################################
# EXERCISE 5
# Using the cross-validation example above and a model with 3 sound parameters:
#  1) Randomly divide the sound instruments into a "fitting" and "validation" data set. (Use only one fold for simplicity)
#  2) Fit a linear model on the "fitting" data set.
#  3) Predict the timbre of the data points in the "validation" data set using the linear
#     model you fit on the "fitting" data set, report the R2. Hint: use the predict(..) function
#     with data = the fitting data set.
#  4) Repeat 1-3 for models that include an increasing number of sound features. The exercise
#     started with a model trained on 3 features, but combine the code you wrote to do 1-3 with
#     code from exercise 3 to loop through models that increase in the number of sounds they
#     utilize as predictors. The goal is to determine the "generalization" performance of each
#     model. Generalization performance is defined here as the R2 of model predictions on the "validation"
#     data set, when that model has been trained on the "fitting" data set.
#
# Suggestion: use 10 data ponts for a validation data set but note that you will then have n-10 rows
# in your data set for fitting model parameters.
###############################################


#
# Exercise 5, with what I think are improvements
#

#clear things just in case
rm(list=ls())
library(car)

# First, change this path to the path where the file mds1PCA40.txt lives:
setwd('C:/Users/William/Documents/Berkeley_Post-Bac/01 - Courses/Spring 2018 Classes/Psych 205/homework')

# Then read the file and print out the number of rows
Timbre <- read.table('mds1PCA40.txt')
(n.inst <- nrow(Timbre))

# Check out a summary of the data. It's all numerical! 
summary(Timbre)

#5.1-3, My version
test_size = 10
cv_split = c(rep(0,nrow(Timbre)-test_size),rep(1,test_size))

Timbre$cv = sample(cv_split)
test = Timbre[Timbre$cv==1,]
train = Timbre[Timbre$cv==0,]

trainmod <- lm(timbre ~ sound.1 + sound.2 + sound.3, data=train)
meanmod = lm(timbre ~1, data=train)
timbre_mean = mean(train$timbre)

testpreds = predict.lm(trainmod, test)
meanpreds = predict.lm(meanmod, test)

ss1 = sum((test$timbre - testpreds)^2)
ss0 = sum((test$timbre- meanpreds)^2)

R2cv = 1 - ss1/ss0


#5.1-3, class version
#testindex = sample(1:42, replace=F)
#test = Timbre[testindex,]
#train = Timbre[-testindex,]
#mod = lm(timbre~sound.1 + sound.2 + sound.3, data=train)
#preds = predict(mod, newdata=test)
#nullmod = mean(mod$model$timbre)
#SSE= sum((test$timbre - preds)^2)
#SST = sum((test$timbre - nullmod)^2)
#R2cv = 1 - SSE/SST


#5.4 - I wanted to try something different from the section video. in particular I wanted to 
# (1) try making it work with any column names, (2)avoid using update so I could make it 
# parallel later on and (3) practice using more functions in my code

#Note - for models past sound.31, you can't really get a reasonable fit 
#because your number of 
#parameters is greater than your number of predictors

#function to find a cross validated R^2
find_R2cv = function(train_subset, test){
  trainmod <- lm(timbre ~ ., data=train_subset)
  meanmod = lm(timbre ~1, data=train_subset) #this is identical to taking the mean
  testpreds = predict.lm(trainmod, test)
  meanpreds = predict.lm(meanmod, test)
  
  ss1 = sum((test$timbre - testpreds)^2)
  ss0 = sum((test$timbre- meanpreds)^2)
  
  R2cv = 1 - ss1/ss0
  return(R2cv)
}

#function to find all cross validated R^2 for a given # of params
find_all_R2cvs = function(Timbre){
  R2cvs = integer(40)
  test_size = 10
  cv_split = c(rep(0,nrow(Timbre)-test_size),rep(1,test_size)) # I think this is cleaner than using which()
  rands = sample(cv_split)
  test = Timbre[rands==1,]
  train = Timbre[rands==0,]
  for(i in 1:40){ #I don't think that the sampling should be inside the for loop. My reasoning
    #is that the cross validated R^2 with more parameters is better compared on the same data
    #so we are seeing just the effects of adding parameters and not adding additional
    #noise by resampling. Later on I will run these tests many times and average across them
    #to get a better estimate
    #Also note that this could only go to 30, since I only have 42-10 = 32 rows for fitting my dataset on. There isn't much point to training models beyond this
    #subset
    keeps = 1:(i+1) #i think this is cleaner than the method used above -- it works with
    #more types of column names
    train_subset = subset(train, select=keeps)
    R2cvs[i] = find_R2cv(train_subset, test)
  }
  return(R2cvs)
}

#this gives the answer to exercise 5-4
#shows that at first it is terrible, gets good with just a few parameters,
#and then gets worse and worse as it overfits more and more
#at a certain point it is worse than a null model
answer = find_all_R2cvs(Timbre)

#this gives a more accurate answer by repeating this process a number of times
#You can set reps much higher for fun
reps = 100
res1 = matrix(vector(),40,reps) #create an empty results matrix
for(j in 1:reps){
  res1[,j] = find_all_R2cvs(Timbre) #run this over and over, put data in results matrix
}

averages = rowMeans(res1) #get the average across rows

#basic plot
plot(averages, type='l', col='blue', main='R^2cv by # of parameters', ylab='R^2 cross validated', xlab="number of parameters used", ylim=c(0,1))
legend(1, 1, legend=c("R2 CV"),
       col=c("blue"), lty=1, cex=0.8)

# re-run some stuff for a nicer plot

#Method 1
parameter_ct = 40

Rvals <- numeric(parameter_ct)
Rvals.adj  <- numeric(parameter_ct)
timbre.mod <- lm(timbre ~ sound.1, data=Timbre)
(timbre.sum <- summary(timbre.mod))
Rvals[1] <- timbre.sum$r.squared
sserror <- sum(timbre.mod$residual^2)
mean.timbre <- mean(timbre.mod$model$timbre)
sstotal <- sum((timbre.mod$model$timbre - mean.timbre)^2)
dferror <- timbre.mod$df.residual  # n - k -1
dftotal  <- length(timbre.mod$fitted.values)-1 # n - 1
Rvals.adj[1]  <- 1- ((sserror/dferror)/(sstotal/dftotal))

for(i in 2:40){
  timbre.mod <- update(timbre.mod, sprintf(". ~ . + sound.%d", i))
  timbre.sum <- summary(timbre.mod)
  Rvals[i] <- timbre.sum$r.squared
  sserror <- sum(timbre.mod$residual^2)
  dferror <- timbre.mod$df.residual  # n - k -1
  Rvals.adj[i]  <- 1- ((sserror/dferror)/(sstotal/dftotal))
}




#Even better plot for 5-4
plot.new()
plot(Rvals, type="l", col="blue", xlab="Number of predictors", ylab="r squared", main="Comparison of R2 (blue), R2adj (red), and R2cv (green)")
lines(Rvals.adj, col="red")
lines(averages, col="green")
legend(1, 1, legend=c("R2", "R2 adj", "R2 CV"),
       col=c("blue", "red", "green"), lty=1, cex=0.8)




