

##############################################
# Final 205 Project Analysis Code
#
# William Ryan
# Turned in 05/07/2018
#
##############################################



##############################################
# Import packages
##############################################

require(caret)
require(ROCR)

##############################################
# Import data
##############################################

test <- read.csv("C:/Users/User/Dropbox/Drive Sync/Berkeley Post Bac/Anne Collin's Lab/MTurk Work/Data/rlwmpst_analysis/tests.csv")
train <- read.csv("C:/Users/User/Dropbox/Drive Sync/Berkeley Post Bac/Anne Collin's Lab/MTurk Work/Data/rlwmpst_analysis/trains.csv")


##############################################
# Basic checks that experiment was run correctly
##############################################

#Perform basic data checks
test_ids = unique(test$id)
train_ids = unique(train$id)

id_test = (length(train_ids) == length(test_ids))
train_len_test = (nrow(train)/length(train_ids)) == 780
test_len_test = (nrow(test)/length(test_ids)) == 216
print(paste0("All subjects present in test and train: ", id_test))
print(paste0("Correct train length: ", train_len_test))
print(paste0("Correct test length: ", test_len_test))

##############################################
# Data cleaning
##############################################

#Get rid of participants who were not engaged
table(train$id)
end_early_ids = c(5, 6, 10, 15, 18, 28, 29, 33, 38, 41)
#remove them
train = train[! train$id %in% end_early_ids,]
test = test[! test$id %in% end_early_ids,]

#check that the remaining participants performed above chance
corr_by_id = aggregate(correct ~ id, train, sum)
total_by_id = aggregate(stim ~ id, train, length)
per_correct = corr_by_id$correct/total_by_id$stim


#Add columns for analysis
train$truth_cond = ifelse(train$per_truth>.9, "High Reliability", "Low Reliability")
train$set_size = ifelse(train$set_size==6, "High Set Size", "Low Set Size")
train$overall_condition = paste(train$truth_cond, train$set_size, sep="; ")

##############################################
# Data analysis
##############################################


#Overall learning curves
correct_sum = aggregate(correct ~ stim_presentation, train, sum)
total_ct = aggregate(stim ~ stim_presentation, train, length)
names(correct_sum)
names(total_ct)
correct_sum$percentage = correct_sum$correct/total_ct$stim

plot(correct_sum$percentage, main="Percentage correct by stimulus presentation", xlab="Stimulus presentation", ylab="Percentage correct", type="l", lty=1)


#Learning curves by each set size and reliability condition combination
lc_by_cond = aggregate(correct ~ stim_presentation + overall_condition, train, sum)
lc_sum_by_cond = aggregate(stim ~ stim_presentation + overall_condition, train, length)
lc_by_cond$per_correct = lc_by_cond$correct/lc_sum_by_cond$stim

#learning curves by condition
plot(lc_by_cond$per_correct[lc_by_cond$overall_condition=="High Reliability; Low Set Size"], type="l", lty=1, col="purple", main="Learning phase percentage correct by condition", xlab="Stimulus presentation", ylab="Percentage correct")
lines(lc_by_cond$per_correct[lc_by_cond$overall_condition=="High Reliability; High Set Size"], col="blue")
lines(lc_by_cond$per_correct[lc_by_cond$overall_condition=="Low Reliability; High Set Size"], col="red")
lines(lc_by_cond$per_correct[lc_by_cond$overall_condition=="Low Reliability; Low Set Size"], col="orange")
legend("bottomright", legend=c("High Reliability; Low Set Size","High Reliability; High Set Size","Low Reliability; Low Set Size","Low Reliability; High Set Size"), col=c("purple", "blue", "orange", "red"), pch=15, pt.cex=2, cex=.8, inset=c(.1,.1))


#plot mean correct by overall condition
perf_by_cond = aggregate(correct ~ id + overall_condition, train, sum)
perf_ct_by_cond = aggregate(stim ~ id + overall_condition, train, length)
perf_by_cond$per_correct = perf_by_cond$correct/perf_ct_by_cond$stim

boxplot(per_correct~overall_condition, data=perf_by_cond, main="Learning phase percentage correct by condition across subjects", xlab="Condition", ylab="Percentage correct", col=c("blue", "purple", "red", "orange"), notch=FALSE)
legend("bottomright", legend=c("High Reliability; Low Set Size","High Reliability; High Set Size","Low Reliability; Low Set Size","Low Reliability; High Set Size"), col=c("purple", "blue", "orange", "red"), pch=15, pt.cex=2, cex=.8, inset=c(.02,.02))


#plot mean correct by reliability condition
perf_by_r = aggregate(correct ~ id + truth_cond, train, sum)
perf_ct_by_r = aggregate(stim ~ id + truth_cond, train, length)
perf_by_r$per_correct = perf_by_r$correct/perf_ct_by_r$stim

boxplot(per_correct~truth_cond, data=perf_by_r, main="Learning phase percentage correct by reliability condition across subjects", xlab="Reliability of feedback condition", ylab="Percentage correct", col=c("red", "orange"), notch=FALSE)
legend("bottomright", legend=c("High Reliability", "Low Reliability"), col=c("red", "orange"), pch=15, pt.cex=2, cex=.8, inset=c(.02,.02))

#plot test phase performance by set size condition
perf_by_ns = aggregate(correct ~ id + set_size, train, sum)
perf_ct_by_ns = aggregate(stim ~ id + set_size, train, length)
perf_by_ns$per_correct = perf_by_ns$correct/perf_ct_by_ns$stim

boxplot(per_correct~set_size, data=perf_by_ns, main="Learning phase percentage correct by set size condition across subjects", xlab="Set size condition", ylab="Percentage correct", col=c("blue", "purple"), notch=FALSE)
legend("bottomright", legend=c("High set size","Low Set size"), col=c("blue", "purple"), pch=15, pt.cex=2, cex=.8, inset=c(.02,.02))

##############################################
# Logistic regression
##############################################

#Model fitting
full_model = glm(correct ~ block + stim_presentation + set_size*truth_cond, family=binomial(link='logit'), data=train)
summary(full_model)

null_model = glm(correct ~ 1, family=binomial(link='logit'), data=train)
summary(null_model)


#check that it holds up with fixed effects
fe_full_model = glm(correct ~ block + stim_presentation + as.factor(id) +set_size*truth_cond, family=binomial(link='logit'), data=train)
summary(fe_full_model)
#confirms results even with fixed effects for each participant

#Get confidence intervals on coefficients
confint(full_model)

conf_ints = confint(full_model)


#Get table of odds-ratios
exp(cbind(OR = coef(full_model), confint(full_model)))

odds_ratios = exp(cbind(OR = coef(full_model), confint(full_model)))

odds_ratios = odds_ratios[2:6]
barplot(odds_ratios, main="Odds ratios by predictor", xlab="Predictor", ylab="Odds ratio")
axis(1, at=1:5, labels=c("block", "stim presentation", "low set size", "low_reliability", "interaction"))
lines(y=c(1,1), x=c(0,6), col="red")

#Classic model comparison
anova(full_model, null_model, test="Chisq")

#Wald tests
require(survey)

regTermTest(full_model, "block")
regTermTest(full_model, "stim_presentation")
regTermTest(full_model, "set_size")
regTermTest(full_model, "truth_cond")
regTermTest(full_model, "set_size:truth_cond")

#
# Statistical analysis by resampling
#

#Cross validate
#Divide into two samples
in_sample = sample(rep(c(1,0),nrow(train)/2))
training_data = train[in_sample==1,]
testing_data = train[in_sample==0,]

#create model
in_sample_full = glm(correct ~ block + stim_presentation + set_size*truth_cond, family=binomial(link='logit'), data=training_data)
in_sample_null = glm(correct ~ 1, family=binomial(link='logit'), data=training_data)

pred = predict(in_sample_full, newdata=testing_data)


#Get ROC for full model
prob <- predict(in_sample_full, newdata=testing_data, type="response")
pred <- prediction(prob, testing_data$correct)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC Curve for Full vs. Null Model on Test Data")
lines(x=c(0,1), y=c(0,1), col = "red")
legend("bottomright", legend=c("Full Model", "Null Model"), col=c("black", "red"), lty=1, pt.cex=2, cex=.8, inset=c(.02,.02))

#find AUC for full model
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Get ROC for null model
probnull <- predict(in_sample_null, newdata=testing_data, type="response")
prednull <- prediction(probnull, testing_data$correct)
perfnull <- performance(prednull, measure = "tpr", x.measure = "fpr")
plot(perfnull, main="ROC Curve for Null Model on Test Data")

#find AUC for null model
auc <- performance(prednull, measure = "auc")
auc <- auc@y.values[[1]]
auc


#K-fold cross validation
require(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

log_model_fit = train(as.factor(correct) ~ stim_presentation + block + set_size*truth_cond, data=training_data, method='glm', family='binomial', trControl=ctrl, tuneLength=5)

pred = predict(log_model_fit, newdata=testing_data)
confusionMatrix(data=pred, as.factor(testing_data$correct))



##############################################
# Linear regression
##############################################

#Reformat data to work for a linear regression
perf_by_conds = aggregate(correct ~ id + set_size + truth_cond, train, sum)
perf_ct_by_conds = aggregate(stim ~ id + set_size + truth_cond, train, length)
perf_by_conds$per_correct = perf_by_conds$correct/perf_ct_by_conds$stim

lm_data = perf_by_conds


#Model fitting

#full model
linear_full = lm(per_correct ~ set_size + truth_cond, data=lm_data)
summary(linear_full)

#set size t test
linear_s = lm(per_correct ~ set_size, data=lm_data)
summary(linear_s)

#reliability condition t test
linear_r = lm(per_correct ~ truth_cond, data=lm_data)
summary(linear_r)

#null model
linear_null = lm(per_correct ~ 1, data=lm_data)
summary(linear_null)

#Classic statistical analysis

#Type II ANOVA (most efficient)
Anova(linear_full)
#this is equivalent to these two
anova(linear_full, linear_s)
anova(linear_full, linear_r)

anova(linear_full, linear_null)



#
# Classical stats tests
#
plot(linear_full)

#
# Check linear regression using (1) random effects model and (2) paired t tests
#

#Fit fixed effect model as a check
fixed_and_random <- lme(per_correct ~ set_size + truth_cond, random= ~ 1|id, data=lm_data)
summary(fixed_and_random)
#See same pattern of results

fixed_effects_only <- lm(per_correct ~ set_size + truth_cond + as.factor(id), data=lm_data)
summary(fixed_effects_only)
#See same pattern of results, set size and truth condition remain significant even accounting
# for between individual variability - if this was just individual variability, this should
#have accounted for any differences, but it did not


#
# Resamplinig
#


#Keeping it simple  - going to compare cross validated R^2

#split into train and test
train_idx = sample(nrow(lm_data), 140, replace=FALSE)

#fit models with new data

train_lm = lm_data[train_idx,]
test_lm = lm_data[-train_idx,]

lm_full_cv = lm(per_correct ~ set_size + truth_cond, data=train_lm)
summary(lm_full_cv)
lm_s_cv = lm(per_correct ~ set_size, data=train_lm)
summary(lm_s_cv)

lm_r_cv = lm(per_correct ~truth_cond, data=train_lm)
summary(lm_r_cv)

lm_null_cv = lm(per_correct ~ 1, data=train_lm)
summary(lm_null_cv)

yhat_lmfull = predict(lm_full_cv, data=test_lm)
yhat_lmr = predict(lm_s_cv, data=test_lm)
yhat_lms = predict(lm_r_cv, data=test_lm)
yhat_lmnull = predict(lm_null_cv, data=test_lm)

ssfull = sum((yhat_lmfull - test_lm$per_correct)^2)
ssr = sum((yhat_lmr - test_lm$per_correct)^2)
sss = sum((yhat_lms - test_lm$per_correct)^2)
ssnull = sum((yhat_lmnull - test_lm$per_correct)^2)

R2cv_lm_full = abs((1-ssfull/ssnull))
R2cv_lm_s = abs((1-sss/ssnull))
R2cv_lm_r = abs((1-ssr/ssnull))




