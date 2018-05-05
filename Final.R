# Psych 205. Final Exam. Spring 2017.           Name:  Ulysses
# Theunissen

 set.seed(7777)        # Used to ensure replicability

# Make sure you have these libraries
library(car) 
library(nlme)
library(lme4)
library(MASS)

################################################################
# Problem 1. Personality Problems?
################################################################
# Load the data set
# Change the statement below to go to the working directory where  you used to save the data file
person <- read.csv("C:/Users/User/Dropbox/Drive Sync/Berkeley Post Bac/Spring 2018 Classes/Psych 205 - Data Analysis/Final Exam Code/ft_personality.csv")

# View the data
View(Person)

# This is a classic dataset in personality psychology.  It has:
#   bfi1 - bfi44: forty-four questions measuring five personality traits
#   BFIE, BFIA, BFIC, BFIN, BFIO: the average 'scale scores' for the five traits 
#     BFIE - extraversion
#     BFIA - agreeableness
#     BFIC - conscientiousness
#     BFIN - neuroticism
#     BFIO - openness
#     sexF: gender (M = male; F = female)
#     ethF: ethnicity (AS = asian; W = white; L = latino; M = middle east; AA = african american; O = other)
#     incF: family household income (different levels)
#     ed.dadF: father's highest level of education (different levels)
#     ed.momF: mother's highest level of education (different levels)
 
# Question 1.1 
# Visualize the data.  Generate 2 separate box plots (boxplot()) and 2 separated 
# interaction plots (interaction.plot()) to show 
# how average scale scores for extraversion and openness vary as a function 
# of the interaction between ethnicity and gender. 
# Comment on what you observe.



# Question 1.2 
# For each personality trait, fit a model and perform the appropriate test to determine whether gender, ethnicity and their interaction affect a participant’s scale score on the trait. 
# Note: For this question you should fit 5 separate models rather than grouping the DVs into a single variable.
# Make sure to include the following as comments in your R code:
#   a. Specify the name of the statistical test as you would report in a paper.
#   b. Summarize the results in a couple of sentences.


# Question 1.3.  
# Are there correlations between the scale scores for the five traits in the personality test? In theory, these should be small. 
# Calculate the correlation table for all of the scale measures (traits), display it, and comment on the results.
# Note: You will have to deal with missing datavalues!.  Use the help for the "cor" function to determine good options.
# Write a couple of concluding sentences, describing the results and stating what might be an alternative to the 5 tests.


 
 



################################################################
# Problem 2.  Exercise, Anarexia and Age
################################################################
# Visualize a longitidunal study that examined whether anorexic patients
# are not only eating less but exercising more.
 
View(Blackmore)  # Warning on old versions of car this data set is Blackmoor - change if it does not work for you
# The Blackmore dataframe has the the number of hours per week of exercise (exercise)
# for different subjects (subject), taken repeatidly at different ages (age).
# The subjects are divided into 'control' and 'patient' (group). 
# Patient are the eating-disordered patients.

# Question 2.1.  
# Use a scatter plot to display exercise as a function of age and using different symbols for control and patient.


 

# Question 2.2. 
# Specify the right model and perform the correct test to determine whether
# exercise is different between the two groups (control vs patient) after taking
# into account the effect of age. 
# In a short sentence state your conclusion


 
 
# Question 2.3.  
# On the same plot from 2.1, draw the two curves obtained from your
# model. 



################################################################
# Problem 3
################################################################
# Fertility in transition.
# view the data set.
View(swiss)
# This data contains fertility rates (Fertility) in 47 counties in Switzerland in 1888 at
# a time of transition (to a more developped and industrialized economy country).  The variable Agriculture contains the 
# percentage % of Swiss males (no female data was collected for this dataset :-( ) involved in agriculture, and Examination and Education are two measures of
# the education level.  Catholic is the % of the population
# that is Catholic (as opposed to Protestant). Infant.Mortality is Infant Mortality. (type ?swiss for more information)

# Question 3.1.  
# Use a scatter plot matrix to visualize how Fertility is 
# affected by all the other variables.


 
 
# Question 3.2
# You may have noticed that counties are divided into mostly
# Catholic and mostly Protestant. To deal with this bi-modal distribution
# make a new categorical variable "Religion" that is 'C' when
# Catholic is greater than 50% and 'P' otherwise.  Generate a new scatter plot
# matrix that separates the data between Catholic and Protestants (different slopes for different folks!)


 
 
# Question 3.3  
# Is the fertility rate different in Catholic vs. Protestant counties?
# Perform the modeling and statistical analysis to answer this question WITHOUT
# taking into account any of the other variables. 
# Notice from the distribution in the diagonal of your scatter plot matrix
# that the probability of Fertility gvien C or P is well approximated by a
# normal distribution
# What is the difference in Fertility rate between the Catholic vs. Protestant 
# counties? You should calculate this difference directly from the data. 
# Summarize your results in a single sentence.


 
 
 

# Question 3.4
# Perform a permutation analysis to calculate obtain a p-value for the average 
# difference in Fertility between Protestant and Catholic counties using 1000 permutated subsamples of the original data. 
# Compare the p-value calculated for your permutation test to the p-value you obtained in 3.3


 
 
# Question 3.5
# Test again for the effect of Religion on Fertility but this time
# take after taking into account all the other regressors as well. 
# From the summary  of yourthe model, what is the new difference in Fertility between the two groups?.
# Explain the difference with the analysis performed in 3.4


 
 
 
 
# Question 3.6
# Perform a second permutation analysis to test the average difference in Fertility 
# between Catholic and Protestants counties where you now take into account all the
# other regressors to test the difference between Catholic and Protestants counties.
# Hint:.  Generate a model for one group to obtain a prediction for the other group and test
# whether that difference in prediction could have been obtained with random groups of similar sizes.


 
 
 
 
 
# Question 3.7
# Compare your full model from 3.5 to a model that excludes the two
# regressors that capture the educational level: Education and Examination.
# Perform a significance test for the model comparison in R, then replicate
# this result by  and calculatinge the F value and p value "by hand" (i.e.,, 
# meaning from the correct sum of square errors and degrees of freedom)


#################################################################.


# Problem 4.  Zebra finch call types.
#################################################################
# Load the data - 

finch <- read.csv("C:/Users/User/Dropbox/Drive Sync/Berkeley Post Bac/Spring 2018 Classes/Psych 205 - Data Analysis/Final Exam Code/zebraFinchCalls.csv")# The first column of this data frame has the call type:
#   DC : Distance Call
#   LT : Long Tonal Call - the disctance call produced by Juveniles
#   Be : Begging Call
# The 21 other columns are acoustical parameters extracted from the calls

# Question 4.1.  
# Perform an analysis to determine whether the three types of calls 
# can be distinguished based on their acoustical parameters.  How many dimensions are significant? 




# Question 4.2. 
# Make a standard scatter plot showing yourthe results from 4.1
# Hint - use the standard plot for the model that you used and for nice colors 
# give the optional color argument as : col = as.integer(ZFCalls$CallType)


 
 

# Question 4.3  
# Determine the percentage of average classification
# using 10 cross-validation sets where 20% of the data is saved
# each time for validation. Use a flat prior (ie. equal probability) (4pts)



#################################################################

# Extra Credit question.  
#################################################################
# Perform a principal component analysis on the 44 questions used to assess the personality traits in Problem 2
# and determine the percent of the variance explained by the first
# 10 PC. You will have to deal with missing values!






