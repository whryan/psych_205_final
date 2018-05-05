# In this tutorial we are going to examine "classical" statistical 
# significance tests for glm().
# We are going to analyze the significance in the first example in the book.

# Get rid of stars, set seed and load library
options(show.signif.stars=FALSE)
set.seed(100) # to reproduce results in the text
library(car)

# The example uses data on married womens work force participation

# Let's lookd at the data. lfp is the binary variable. k5 is the number of children < 5 and k618 childre 6 to 18. wc wife college, lwg log of wife wage

some(Mroz)  # sample 10 rows

# What is the sample size?
nrow(Mroz)

# We are now fitting the generalized linear model.  The logit link is the default link function
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
   family=binomial(link=logit), data=Mroz)
summary(mroz.mod)


# We are now going to compare two models - with and without the participation of children
mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
anova(mroz.mod.2, mroz.mod, test="Chisq")

# The Anova command in car performs a type II test
Anova(mroz.mod)



# Exercise 1. Calculate the Residual deviance for the full mroz model (similar to R2 error). Note that the log likelyhood of the saturated model is equal to zero.  In my solution, I used: the datayes variable, a for loop to add the contribution of each entry and a if statement to look at yes vs no. 

# I will use LS: log likelihood for saturated model
#            LM: log likelihood for model
#            L0: log likelihood for zeroth order model

# Saturated Log likelihood is zero
LS <- 0

n <- length(mroz.mod$model$lfp)
datayes <- mroz.mod$model$lfp == 'yes'
LM <- 0
for (i in 1:n) {
	if (datayes[i]) {
		LM <- LM + log(mroz.mod$fitted.values[i])
	}
	else {
		LM <- LM + log(1-mroz.mod$fitted.values[i])	}	
}
# More directly: LM  <- sum(log(mroz.mod$fitted.values[datayes]))+sum(log(1-mroz.mod$fitted.values[!datayes]))

(mroz.deviance <- 2*(LS-LM))

# Exercise 2. Now calculate the Null deviance.  The null deviance is the deviance obtained from the mean response. (similar to SS total). Compare your values to those obtained in the summary.

pavg <- sum(datayes)/n

L0 <- 0
for (i in 1:n) {
	if (datayes[i]) {
		L0 <- L0 + log(pavg)
	}
	else {
		L0 <- L0 + log(1-pavg)	}	
}

(mroz.deviance.null <- 2*(LS-L0))

# Exercise 3. Now calculate the change in deviance of a full model, with and without age.  Obtain a p value from the chisquare  distribution. Perform the Type II test to check your result (you can do this both with the anova and the Anova function - do it both ways so that you understand it well).

# First I get a new model
mroz.mod.3 <- update(mroz.mod, . ~ . - age)

# Then I calculate the deviance for this model
LM <- 0
for (i in 1:n) {
	if (datayes[i]) {
		LM <- LM + log(mroz.mod.3$fitted.values[i])
	}
	else {
		LM <- LM + log(1-mroz.mod.3$fitted.values[i])	}	
}
(mroz.3.deviance <- 2*(LS-LM))

# Now the difference in deviances:
(diff.deviance <- mroz.3.deviance - mroz.deviance)
(pval <- pchisq(diff.deviance, df=1, lower.tail=FALSE))

anova(mroz.mod.3, mroz.mod, test="Chisq")
Anova(mroz.mod)

# Exercise 4.  Comparing Deviance to sum of square Errors

# linear models can also be called using glm.  The two following models are equivalent, but give different summaries:

prestige.mod.lm <- lm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.lm)

prestige.mod.glm <- glm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.glm)

# Compare the deviance in the glm model to the sum of square errors from the lm model?  Is the residual Deviance given by R unitless (normalized) or does it have units?

(sserror <- sum(prestige.mod.lm$residual^2))

# This ss errors is equal to the residual deviance in the model.  
# R uses a definition of deviance that has units - sometimes called scaled deviance 

# This deviance is only equal to the difference in log likelihoods for distributions where the dispersion parameter is one. 
# To get a unit-less deviance (defined as the difference in log likelihoods), you would have to divide by the dispersion parameter - here the estimate of the error (the variance)
# For that purpose you need an F test.

closeness  <- factor(rep(c('one.sided','close'), c(3,3)), levels=c('one.sided','close'))
preference  <- factor(rep(c('weak', 'medium', 'strong'), 2), levels=c('weak', 'medium', 'strong'))
voted  <- c(91, 121, 64, 214, 284, 201)
did.not.vote  <- c(39, 49, 24, 87, 76, 25)
logit.turnout  <- log(voted/did.not.vote)
Campbell  <- data.frame(closeness, preference, voted, did.not.vote, logit=logit.turnout)

oldpar  <- par(mar=c(5.1, 4.1, 4.1, 4.1))
with(Campbell,
  interaction.plot(preference, closeness, logit,
    type='b', pch=c(1,16), cex=2,
    ylab='log(Voted/Did not Vote)'))
probabilityAxis(side="right", at=seq(0.7, 0.875, by=0.025),
  axis.title="Proportion(Voted)")
par(oldpar)

campbell.mod  <- glm(cbind(voted, did.not.vote) ~ closeness*preference, family=binomial, data=Campbell)
summary(campbell.mod)

campbell.mod2  <- glm(cbind(voted, did.not.vote) ~ closeness + preference, family=binomial, data=Campbell)
summary(campbell.mod2)

anova(campbell.mod2, campbell.mod, test='Chisq')
