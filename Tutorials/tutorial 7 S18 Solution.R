
# This is a short tutorial to introduce us to logistic regression
# Corresponding textbook reading: Sections 5.1 - 5.4 of Companion book.

####################
# Exercise 1.
# Try to make figure 5.1 in R.  Notice that figure 5.1 are inverse link functions
# or mean kernel functions.  
# Hint: you will have to use the help menu to find functions to calculate the probit,
# make sure you go to Tools->Install Packages and install the “VGAM” library. Once
# you have that library installed, you can use the “probit”, and “cloglog” functions to
# make the plot of 5.1.
####################

# Another hint: figure 5.1 actually displays the *inverse* link functions. So make sure
# you pass inverse=TRUE to the probit and cloglog functions. Also, the inverse of
# the logit function is the sigmoid:
sig = function(x) 1. / (1. + exp(-x))

x <- seq(-5,5,by=0.1)
ylogit <- 1/(1+exp(-x))

plot(x,ylogit, type='l', xlab=expression(eta(x)), ylab=expression(mu(x)))

library(VGAM)
yprobit <- probit(x, inverse=TRUE)
lines(x,yprobit,lty='dashed')

yclog <- cloglog(x, inverse=TRUE)
lines(x, yclog, lty='dotdash')

legend('topleft',c('logit','probit','cloglog'),lty=c(1,2,4))

####################
# END OF EXERCISE 1
####################


# We are now going to do the first example in the book.
# Get rid of stars, set seed and load library
options(show.signif.stars=FALSE)
set.seed(100) # to reproduce results in the text
library(car)

# The example uses data on married women’s workforce participation

# Let's look at the data. lfp is the binary variable. k5 is the number of
# children < 5 and k618 children 6 to 18. wc wife college, lwg log of wife wage

some(Mroz)  # sample 10 rows

# What is the sample size?

# We are now fitting the generalized linear model.  The logit link is the default link
# function
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
                family=binomial(link=logit), data=Mroz)
summary(mroz.mod)

# We are calculating the exponents of the coefficients
round(exp(cbind(Estimate=coef(mroz.mod), confint(mroz.mod))), 2)


##########################
# Exercise 2. 
# What is the effect of having one additional child who is less than 6 years old?
##########################

# For more information about the odds ratio, check out
# the lecture material and these two links:
# http://www.blackwellpublishing.com/specialarticles/jcn_10_268.pdf
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2938757/

# it reduces the odds of working by 77%!

################
# END OF EXERCISE 2
################


# We are now going to compare two models - with and without the participation of children
mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
anova(mroz.mod.2, mroz.mod, test="Chisq")

# The Anova command in car performs a type II test
Anova(mroz.mod)

# As in the case of lm() we can also look at the predictions.
head(predict(mroz.mod)) # first 6 values
head(predict(mroz.mod, type="response"))


#########################
# Exercise 3. 
# By setting a threshold at 50% calculate the number of correct predictions.
#########################

# Hints:
# Use the predict(...) function to make a prediction on the dataset. The output of
# the predict(...) command is an array of probabilities. Element i of the output array 
# is the probability of working given the weighted combination of input regressors.

# Use logical arrays or a for loop to count the number of correct and incorrect
# guesses. The number of correct predictions is the number of times the predicted
# probability is greater than the threshold of 0.5 and the actual value of lfp is 1,
# plus the number of times the predicted probability is less than 0.50 and the actual
# value of lfp in the data is 0.

modelyes <- predict(mroz.mod, type='response') > 0.5
datayes <- mroz.mod$model$lfp == 'yes'

ncorrect <- sum(modelyes & datayes) + sum(!datayes & !modelyes);

(pcorrect <- ncorrect/length(mroz.mod$model$lfp))

# We are predicting 70% of answers....

# Now we make a nice plot to explain logistic regression
sel = sample(length(mroz.mod$linear.predictors), 200);
scatterplot(mroz.mod$linear.predictors[sel],jitter(mroz.mod$fitted.values[sel],factor=1000),group=datayes[sel],smooth=FALSE, reg.line=FALSE, cex=0.5)

p <- sum(datayes)/length(mroz.mod$model$lfp)
points(mroz.mod$linear.predictors[sel], rep(p,200), cex=0.5)


