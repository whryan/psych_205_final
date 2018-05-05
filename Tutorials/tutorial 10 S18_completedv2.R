# This tutorial illustrates PCA, ICA and LDA

# In this example there are four light recorders and two light sources
recorders <- data.frame("X"=c(0,0,1,1), "Y" = c(0,1,1,0), row.names=c("A", "B","C","D"))
locs <- data.frame("X"=c(.3,.5),"Y"=c(.8,.2))
intensities <- data.frame("sine"=sin(0:99*(pi/10))+1.2,
"cosine"= .7*cos(0:99*(pi/15))+.9)

# We can plot this data frame as two time series
plot.ts(intensities)

# Now, we are going to generate "recorded" data
dists <- matrix(nrow=dim(locs)[1], ncol=dim(recorders)[1],
dimnames <- list(NULL, row.names(recorders)))
for (i in 1:dim(dists)[2]) {
	dists[,i]=sqrt((locs$X-recorders$X[i])^2 + (locs$Y-recorders$Y[i])^2)
}
recorded.data <- data.frame(as.matrix(intensities)%*%
matrix(data=exp(-2*as.numeric(dists)), nrow=dim(locs)[1], ncol=dim(recorders)[1]))

recorded.data <- as.data.frame(apply(recorded.data,2,FUN=function(x)
	{sdx=sd(x, na.rm=TRUE);
	noise=rnorm(length(x),0,sdx/10);
	return(x+noise)}
	))
	
# Let's take a look at it.

plot(recorded.data)
round(cor(recorded.data),2)

# We can also plot is as a time-series
plot.ts(recorded.data)

# Lines 33 to 57 for 205
#Calculate PCA by hand...
Xoriginal<-t(as.matrix(recorded.data))
# Center the data so that the mean of each row is 0
rm<-rowMeans(Xoriginal)
X<-Xoriginal-matrix(rep(rm, dim(Xoriginal)[2]), nrow=dim(Xoriginal)[1])
# Calculate P, whose rows are the eigenvectors of XXT

# A is the covariance matrix but note that it is not normalized by n-1
A<-X %*% t(X)

# Also you can get it directly
(Adirect = cov(recorded.data))
(A/(dim(X)[2]-1))

E<-eigen(A,TRUE)

# P is the transpose of the eigenvectors.
P<-t(E$vectors)
# Find the new data and standard deviations of the principal components
# The principal components are the rows of P.
# Standard deviations are the square root of variances, which can be read off
# the diagonal of the covariance matrix of PX.
newdata <- P %*% X

# Here we are recovering the eigenvalues
sdev <- sqrt(diag((1/(dim(X)[2]-1)* P %*% A %*% t(P))))

# This is also equal to
(sqrt(E$values)/sqrt(dim(X)[2]-1))
# We can also calculate the principal componnents using R functions
pr=prcomp(recorded.data)
pr

# This plots the variance explained by each pc
plot(pr)  

# Let's plot the normalized standard deviation explained by each PC
barplot(pr$sdev/pr$sdev[1])

# You can also set a tolerance value to select PCA that explain a certain fraction of the variance.
pr2=prcomp(recorded.data, tol=.1)
plot.ts(pr2$x)  # x are the coefficients of the pca
plot.ts(intensities)    #how do they compare to the int?
plot.ts(recorded.data)  #how do they compare to the recorded data?

# We can also obtain reconstructions of the data using only some of the pca

# Here is a reconstruction using all PCs
od=pr$x %*% t(pr$rotation)
#now we are going back tot he original space, either with all of the PCs or just with the first two (below)
# Here is the reconstruction using the first two PCs
od2=pr2$x %*% t(pr2$rotation)

# Let's plot the reconstructions on different graphs
plot.ts(od)
plot.ts(od2)
#you can see that it is verrrry slightly less accurate, but not much less accurate

#WE HAD MADE THE ASSUMPTION THAT thi was multivariate normal, but this was inaccurate
#

# Let's try ICA
library(fastICA)
a <- fastICA(recorded.data, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE) #set it to only two variables based on the PCA analysis

plot.ts(a$S)
plot.ts(intensities)
#does a much better job extracting the unerlying features becuase lacks the asumptions

# We are now also going to look at linear discriminant analysis or MANOVA 
# The R function is called lda() and it is found in the MASS package
library(MASS)
library(car)

# the iris data has information on the morphology of three types of irises.. Can you tell the species apart from this morphology?

Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                    Sp = rep(c("s","c","v"), rep(50,3)))
#taking half of this data only
train <- sample(1:150, 75)
#checking if things are ablanced well between species in the random sample
table(Iris$Sp[train])

# Let's perform a discriminant function analysis
#what is the species based on everything else?
#making a uniform prior, and creating it with the training dataset alone
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
#tells you that there are two new axes - they maximize the 
#betwen cov matrix/ within cov matrix
#maximize separation between groups, and can see how much seperation
#is explained by each of the dimensions
#also note that LDA only shows you the ones which are significant - 
#there are likely more (4) which didn't meet
# that threshold

#remeber - this means two axes on which the groups are seperated, not the same
#as having just two groups
z
#based on coefficients:
#LDA1, eg is .69*sepal leng + 1.6 * sepal w, + -1.8*pelal l + -2.8* petal w

((z$svd)**2)
#this is the variance explained by lda1 and lda 2 respectively, more or less?
#he said maybe
#square of that gives you the eigen values, but don't need to go into too many details

# we can plot the results
scatterplot(Sepal.L. ~ Sepal.W. | Sp, data = Iris)
#can see that groups seperate well, but there is lots of correlation. So LDA 1 might be a negtive slope line through 
#all 3

plot(z) #shows where all of the things you are plotting
#fall on each of these linear discriminants
#you can see here quite clearly that most of the 
#seperation is due to ld1 - while ld2 might be slithgly
#changing things, it doesn't change it very much
plot(z, dimen=1, type='both')
#shows where the groups fall along LDA 1, demonstrates how good the seperation is

# We can do a manova
#does the same thing, but the syntax is flipped - ie put the four in first, then Sp
z.man  <- manova( cbind(Sepal.L., Sepal.W., Petal.L., Petal.W.) ~ Sp, data = Iris)
s.z.man  <- summary(z.man, test = 'Wilks')
#this tells you that it is significant, and you can indeed seperate the groups
#does not tell the number of groups as far as we can tell, tho it must tell us that
#we do not know how to get at it
#in general, just use LDA - it is the more modern way to do things

# And get predictions for the data that we did not use
preds = predict(z, Iris[-train, ])$class
actual = Iris$Sp[-train]

#2/73 incorrect
table(preds==actual)/sum(table(preds==actual))
#this is wayyyy above chance
#same as prop table, not sure why i did it like that 

#THIS IS PROBABLY SOMETHING WE WILL DO ON OUR FINAL - WHAT IS PERCENT CORRECT ON THE CROSS VALIDATION 

#correct detection - model says yes, and it is yes (true positive)
#false positive = false positive

############
#
# EXERCISE HERE!
#
# Below here is the work that I did during/directly after section
#this is correct, I just left the second part for reference
############
data('biopsy')
biopsy_clean = biopsy
#Set up biopsy data
biopsy = biopsy[,-which(names(biopsy)=="ID")] #remove ID variable
set.seed(1)
#Get rid of NAs
biopsy = na.omit(biopsy)
head(biopsy)
#set up train with 80%
percent_train = .8
train = sample(1:nrow(biopsy),floor(nrow(biopsy)*percent_train), replace=FALSE)

z = lda(class~., biopsy[train,]) #default prior is just the observed probability in the data
z # just one discriminator

plot(z)

#not important, but here it is
#manova(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9)~class, data=biopsy[train,])

#Now we make predictions for the test set
preds = predict(z, biopsy[-train,])$class
actual = biopsy[-train,]$class

#Here is one way to do it
#3% of the time (3 cases) our prediction didn't match the actual
prop.table(table(preds == actual))
#there were no false positives, or 0%
prop.table(table(preds=="malignant" & actual=="benign"))
#there were three false negatives, or 3% of all predictions
prop.table(table(preds=="benign" & actual=="malignant"))

#Here is the way I like better, from section
res = rep(NA,length(preds))
res[preds=="benign" & actual=="benign"] = "TN"
res[preds=="benign" & actual=="malignant"] = "FN"
res[preds=="malignant" & actual=="malignant"] = "TP"
res[preds=="malignant" & actual=="benign"] = "FP"

table(res)
prop.table(table(res))
#2.1% false neg, 69% true neg, 28% true positive plus decimals

#sensitivity TP/TP+FN
sum(res=="TP")/(sum(res=="TP") + sum(res=="FN"))
#92%

#sepcificity TN/TN+FP
sum(res=="TN")/(sum(res=="TN") + sum(res=="FP"))
#again, 1

#Do an ROC Curve
require(pROC)
lda_pred = predict(z, biopsy[-train, ], type="prob")
biopsy$binary_response <- as.numeric(biopsy$class) - 1
lda_test_roc <- roc(binary_response ~ lda_pred$x, data = biopsy[-train, ], ci = TRUE)
# there turns out to be an extremely uninteresting
# ROC curve for this function
plot(lda_test_roc)


#

#
#
#
#
#
#
#
#
#
#
#

#
#
#
#
#
#
#
#

#
#
#
#
#

#
#
#
#
#
#
#
#
#
#
#

############
# Below here is the work that I did before section
#*~*~*~Ignore all this, this is just the difft method I used~**~*~*~*
############

# Exercise 1
# Look at the biopsy data which has morphological data on tumors.  THere are 9 measurements
# V1 clump thickness.
# V2 uniformity of cell size.
# V3 uniformity of cell shape.
# V4 marginal adhesion.
# V5 single epithelial cell size.
# V6 bare nuclei (16 values are missing).
# V7 bland chromatin.
# V8 normal nucleoli.
# V9 mitoses

head(biopsy)
#rename the class variable
colnames(biopsy)[11] = "type"

################################################
#1A  Perform a MANOVA (same as lda) on this data.
#1B Use 80% of the data for training and 20% for testing.
################################################
#for the ROC curve later on
require(pROC)
#nrow(biopsy)*.8
#699-560
#consistent with this tutorial
train = sample(1:699, 560)

#LDA, as above
z2 <- lda(type ~ V1+V2+V3+V4+V5+V6+V7+V8+V9, biopsy, prior = c(1,1)/2, subset = train, na.action=na.omit) #flat prior
((z2$svd)**2)
plot(z2)
plot(z2, dimen=1, type='both')

(z2)

#MANOVA, as above
z.man2  <- manova( cbind(V1, V2, V3, V4, V5, V6, V7, V8, V9) ~ type, data = biopsy)
s.z.man2  <- summary(z.man2, test = 'Wilks')
s.z.man2


################################################
#1C How many discriminant functions do you find? 
################################################
#According to the above, just one. 
(z2)
#Coefficients of linear discriminants:
#  LD1
#V1  0.29919042
#V2 -0.09667894
#V3  0.16569551
#V4  0.02626229
#V5  0.04988401
#V6  0.29355705
#V7  0.14153760
#V8  0.10249069
#V9  0.21542708

prop = z2$svd^2/sum(z2$svd^2)
prop
#sort of meaningless with just one discriminant function

################################################
#1D How well can you predict the class for the testing data? 
# (give a percent of correct detection and of false positives)
################################################

z2_hat = predict(z2, biopsy[-train, ])$class

z2_real = biopsy$type[-train]


prop.table(table(z2_hat))
#benign malignant 
#0.7293233 0.2706767 
prop.table(table(z2_real))
#benign malignant 
#0.705036  0.294964 

#93% of the time we correctly predicted the type
correct_detection = (z2_hat==z2_real)
sum(correct_detection[!is.na(correct_detection)])/length(correct_detection)

#0% of the time are there false positives
false_positive = (z2_hat=="malignant" & z2_real=="benign")
sum(false_positive[!is.na(false_positive)])/length(false_positive)

#However, there were 6 missing variables in the predict
#function, which could account for this error

#####################
# #Make an ROC curve just to see what happens
######################

lda_pred = predict(z2, biopsy[-train, ], type="prob")

biopsy$binary_response <- as.numeric(biopsy$type) - 1
lda_test_roc <- roc(binary_response ~ lda_pred$x, data = biopsy[-train, ], ci = TRUE)
# there turns out to be an extremely uninteresting
# ROC curve for this function
plot(lda_test_roc)

