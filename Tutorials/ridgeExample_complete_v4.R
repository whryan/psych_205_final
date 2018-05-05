#to fix most graphing problems either

#par(mar=c(1,1,1,1)) to make your margins small
#dev.off() after running a plot to reset your graphics device

library (ridge)
library(caret)
library(MASS)
#ridge model
data(GenCont)
mod.ridge <- linearRidge(Phenotypes ~ ., data = as.data.frame(GenCont))
summary(mod.ridge)

gencont = as.data.frame(GenCont)

#lm model
mod.lm <- lm(Phenotypes ~ ., data = as.data.frame(GenCont))
summary(mod.lm)

#find R2 for ridge
beta.ridge = coef(mod.ridge)
resid.ridge = train$lpsa - beta.ridge[1] - as.matrix(gencont[,2:13])%*%beta.ridge[2:13]

d = svd(as.matrix(gencont[2]))


# Perform a ridge regression on a Data Set from your lab.  Chose a data set
# where you have a high number of predictors whith high covariance.  Compare
# the results to a full model.  In particular, perform the fit using a 
# fitting data set and test it on a training data set.  Calculate
# the R2 value for both models.

#
# Ridge regresson 1
#

#Data used is a replication/extension I ran of Rigdon et al 2009 on MTurk, seeing if minimal social cues of eyes effect giving in the dictator game

data <- read_csv("~/Berkeley_Post-Bac/02 - Research/Haas Research/Other Research/Random Replications/Rigdon et al 2009/clean_data/minimal_social_cues_clean.csv")
str(data)

#I'm going to predict amount kept for themselves based:
# Demographics (uncorrelated)
# Their usage of MTurk and their involvement with communities around it (correlated)
# Attention checks/understanding checks (very very correlated)

#subset
keeps = c("kept", "age", "gender",  "race_ethnicity", "amt_usage", "amt_community_involvement", "believe_anonymous", "believe_other_participant", "if_keep_100", "if_keep_50", "pass_attn_checks")
df = data[,keeps]

#convert to factors
factors = c("gender",  "race_ethnicity", "amt_usage", "amt_community_involvement", "believe_anonymous", "believe_other_participant", "pass_attn_checks", "if_keep_100", "if_keep_50")
df[,factors] = lapply(df[,factors],as.factor)

#convert factors to dummy variables so that it works in matrix multiplication
dummy_df = predict(dummyVars("~.", data=df), newdata=df)
df = as.data.frame(dummy_df)

#test/train split
idx <- sample(1:213,120,replace=F)
train <- df[idx,]
test <- df[-idx,]

# fit ridge model
ridgemod <- linearRidge(kept~.,data=train)
ridgecoefs <- coef(ridgemod)

#get predictions and SSE
mM <- model.matrix(kept~.,data=test)
Yhats <- ridgecoefs%*%t(mM)
resids <-  test$kept - Yhats
SSE <- sum(resids^2)

# fit linear model
lmmod <- lm(kept~.,data=train)
#get predictions and SSE
Yhats <- predict(lmmod,test)
residslm <- test$kept - Yhats
SSElm <- sum(residslm^2)

deviations <- test$kept - mean(train$kept)
SST <- sum(deviations^2)

R2r <- 1-SSE/SST




#
# Ridge Regression v2
#

#I wanted to run something more reproducible, so I thought I'd run it on some simulated data and see if it is appropriately low
fake_df = data.frame(dv = rpois(1000,1),iv = sample(rep(c(1,0),500)),mod1 = rnorm(1000, 3, 5),mod2 = rnorm(1000, 3, 5),mod3 = rnorm(1000, 3, 5),mod4 = rnorm(1000, 3, 5),mod5 = rnorm(1000, 3, 5),mod6 = rnorm(1000, 3, 5),mod7 = rnorm(1000, 3, 5),mod8 = rnorm(1000, 3, 5),mod9 = rnorm(1000, 3, 5),mod10 = rnorm(1000, 3, 5),mod11 = rnorm(1000, 3, 5))


idx <- sample(1:1000,900,replace=F)
train <- fake_df[idx,]
test <- fake_df[-idx,]

# fit ridge model
ridgemod <- linearRidge(dv~.,data=train)
ridgecoefs <- coef(ridgemod)
mM <- model.matrix(dv~.,data=test)
Yhats <- ridgecoefs%*%t(mM)
resids <-  test$dv - Yhats
SSE <- sum(resids^2)

# fit linear model
lmmod <- lm(dv~.,data=train)
Yhats <- predict(lmmod,test)
residslm <- test$dv - Yhats
SSElm <- sum(residslm^2)

deviations <- test$dv - mean(train$dv)
SST <- sum(deviations^2)

R2r <- 1-SSE/SST
R2lm <- 1-SSElm/SST

#> R2r
#[1] -0.0008508337
#> R2lm
#[1] -0.010371

#Yup, looks appropriately low