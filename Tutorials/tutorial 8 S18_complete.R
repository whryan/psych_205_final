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
#residual deviance = 2*[0-LogM]

#LogM
#sum(log(yhat_u)) if yes
#sum(log(1-y_hat_i)) if no
#add up and that is logM

datayes = mroz.mod.2$model$lfp=="yes"
sumyes = sum(log(mroz.mod$fitted.values[datayes]))
sumno = sum(log(1-mroz.mod$fitted.values[!datayes]))

logm = sumyes + sumno

res_dev = 2*(0-logm)



# Exercise 2. Now calculate the Null deviance.  The null deviance is the deviance obtained from the mean response. (similar to SS total). Compare your values to those obtained in the summary.
m_resp = mean(datayes)
sumyes_null = sum(log(m_resp)*datayes)
sumno_null = sum(log(1-m_resp)*(!datayes))
nulldev = 2 * (0-(sumyes_null+sumno_null))


# Exercise 3. Now calculate the change in deviance of a full model, with and without age.  Obtain a p value from the chisquare  distribution. Perform the Type II test to check your result (you can do this both with the anova and the Anova function - do it both ways so that you understand it well).
mroz.mod.3 <- update(mroz.mod, . ~ . - age)
summary(mroz.mod.3)

resdev2 = mroz.mod.3$deviance

G = resdev2-resdev
pchisq(G,df=1,lower.tail = FALSE)

anova(mroz.mod.3, mroz.mod)
Anova(mroz.mod)

# Exercise 4.  Comparing Deviance to sum of square Errors

# linear models can also be called using glm.  The two following models are equivalent, but give different summaries:

prestige.mod.lm <- lm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.lm)

prestige.mod.glm <- glm(prestige ~ education + log2(income), data=Prestige)
summary(prestige.mod.glm)

# Compare the deviance in the glm model to the sum of square errors from the lm model?  
sum(prestige.mod.lm$residuals^2)
#it is identical -see summary output

#Is the residual Deviance given by R unitless (normalized) or does it have units?
Prestige$prestige_z= scale(Prestige$prestige)
prestige.mod.glm <- glm(prestige_z ~ education + log2(income), data=Prestige)
summary(prestige.mod.glm)

#deviance has changed, so it is related to the scale of your variable -- see the difference between this and the non z scored answer.

#########  This is the other way to specify a logistic regression  ########


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
