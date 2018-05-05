
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

install.packages("VGAM")
require(VGAM)

y = seq(0,1,0.01)
odds = y/(1-y)
logodds = log(odds)
plot(y~logodds,type='l')
probits = probit(y)
clogloggs = cloglog(y)

lines(y~probits, col="red")
lines(y~clogloggs, col="blue")


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
nrow(Mroz)
# We are now fitting the generalized linear model.  The logit link is the default link
# function
mroz.mod <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc,
                family=binomial(link=logit), data=Mroz)
summary(mroz.mod)


# We are calculating the exponents of the coefficients
round(exp(cbind(Estimate=coef(mroz.mod), confint(mroz.mod))), 2)
#coef(mrox.mod) - gives coefs
#confint givs confidence intervals for those coefficients
#put them inside cbind() to bing the estimates and confidence intervals together
#when we make it an exponent then we get the odds ratio

#Exmaple:
#k5              0.23  0.16   0.34
#if it was one, it would mean a unit increase in the var wouldnt change the odds at all
#these are ratios of the odds with an additional
#unit over the odds without that additional unit
#therefore- with one, they are jsut the same
#an odds ratio lower than 1 means that odds
#wtith an additional unit are lower than without that unit
#so in this case, if you add a kid, your odds go down by about 70%
#the reason to report it this way is because 
#it will be the same for everyone in your sample, no matter how
#high or low their odds were to begin wih
#but that doesn't mean that their change in probability was the same

#so what's interesting is that at different points on the scale, 
#you'll get very different results for prooabbiltiy increases
# but you will have the same odds ratio change, relative to what those specific probabilities are

#so there are useful insofar as they apply to everyone, but not useful
#since it is hard to understand what they mean, and they mean something different
#for different people in terms of the actual outcomes they will experience

##########################
# Exercise 2. 
# What is the effect of having one additional child who is less than 6 years old?
##########################

# For more information about the odds ratio, check out
# the lecture material and these two links:
# http://www.blackwellpublishing.com/specialarticles/jcn_10_268.pdf
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2938757/

#for any given person, our model predicts tthat an additional child below 6yo is going to decrease your
#odds of workforce participation by about 70%

#but the same chage could mean different things for different people depening where they are on the other covariates

#one option - average treatment effect
#first, get predictions from the model
y_hats = predict(mroz.mod, Mroz, type="response")
#now do the predictions where they all have another kid

Mroz1 = Mroz
Mroz1$k5 = Mroz1$k5 + 1

y_hats2 = predict(mroz.mod, Mroz1, type="response")

diffprobs = y_hats - y_hats2

mean(diffprobs)
#.29
# at mean values of all covariates, we predict that this will make them 30% less likely
# to be in the labor force
#this can be an easier to interpret way to look at this result
# this is commonly how we do this in eg polisci papers
#gives you an easier way to understand things

################
# END OF EXERCISE 2
################


# We are now going to compare two models - with and without the participation of children
mroz.mod.2 <- update(mroz.mod, . ~ . - k5 - k618)
anova(mroz.mod.2, mroz.mod, test="Chisq")

#kids below 5 seem key as it turns out!

# The Anova command in car performs a type II test
Anova(mroz.mod)

# As in the case of lm() we can also look at the predictions.
logodds = head(predict(mroz.mod)) # first 6 values
head(predict(mroz.mod, type="response"))
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


#my initial method
y_hats = predict(mroz.mod, type="response")

in_workforce = ifelse(Mroz$lfp=="yes" & y_hats>.5,1,0)
not_in_workforce = ifelse(Mroz$lfp=="no" & y_hats<=.5,1,0)

(sum(in_workforce) + sum(not_in_workforce))/nrow(Mroz)

#lecture method
correct = ifelse((Mroz$lfp=="yes" & y_hats>.5) | (Mroz$lfp=="no" & y_hats<=.5),1,0 )
sum(correct)/nrow(Mroz)
#same result!

#order of operations means you don't need the parens

#69% correct

###### End of Exercise 3

datayes = correct

# Now we make a nice plot to explain logistic regression
sel = sample(length(mroz.mod$linear.predictors), 200);
scatterplot(mroz.mod$linear.predictors[sel],jitter(mroz.mod$fitted.values[sel],factor=1000),group=datayes[sel],smooth=FALSE, reg.line=FALSE, cex=0.5)

p <- sum(datayes)/length(mroz.mod$model$lfp)
points(mroz.mod$linear.predictors[sel], rep(p,200), cex=0.5)


