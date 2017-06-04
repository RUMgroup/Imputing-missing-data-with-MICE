library(mice)
library(VIM)
library(lattice)


data("nhanes2")
str(nhanes2)
View(nhanes2)
####################################################################################
# Research question: are age, bmi and hypertension associated with cholesterol levels?
# we will use a multiple linear regression model to examine this
# using data from the NHANES survey

####################################################################################
# Explore the missing data
####################################################################################

md.pattern(nhanes2)
# 0 means missing data and 1 means complete - e.g. 10 rows with missing chl and 13 with complete data

aggr(nhanes2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
     labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# same information, but graphically

marginplot(nhanes2[, c("chl", "bmi")], col = mdc(1:2), cex = 1.0,
           cex.lab = 1.0, cex.numbers = 0.5, pch = 19)
# under MCAR distributions of red and blue should be identical

####################################################################################
# Imputation process
####################################################################################

# The missing values for each variable are imputed (filled-in) by sampling from
# a multivariate distribution of pre-selected variables (usually as many variables as possible).

# The sampling for each imputed value is conditional on the other values in that observation.
# For example, if height was missing but we knew the patient's gender,
# the imputed value would be typically higher if the patient was male than if the patient was female.
# (assuming that in our dataset men were typically taller than women)

# When there are >1 missing variables to impute, we use 'chained equations'. The chain moves from one variable to the next,
# imputing values in each variables conditional on all available (imputed or already non-missing) values.
# We iterate through a few times before settling on an imputed value to make sure the value is not biased by the starting values,
# which will have initially have been based on only non-missing and non-imputed values

# This is best described as an 'educated random guess'.
# We make this guess multiple times by repeating the process in many datasets. This gives an idea of how much variability there is in the guess.
# This variability contributes to the overall certainty we have in our eventual analysis model


### Created imputed datasets #################

imp <- mice(nhanes2, maxit = 20, m=5, seed = 262642)
# imp is a 'mids' object. stands for Multiple Imputation Data Set.
# It contains m multiply imputed datasets
# 'maxit' is the number of iterations to go through before selecting an imputed value
# 'm' is the total number of imputed datasets
# there are other, unused arguments that allow you to select the imputation method, which variables should be used to impute each missing variable, etc etc


View(nhanes2)
View(complete(imp, action=1))
View(complete(imp, action=2))


### Diagnostic checks #################

summary(imp)
plot(imp)
# want to check that there's no detectable trends, and that there is some random sampling variation

densityplot(imp, scales = list(x = list(relation = "free")), layout = c(2, 1))
# want to check that the red lines are roughly the same as the blue line

xyplot(imp, chl ~ bmi + hyp, pch=18, cex=1, scales="free")
# want to check that the distribution of red dots is similar to the blue dots


stripplot(imp, pch = 20, cex = 1.2)
# same



### Run Analysis in each imputed dataset and pool results #################

fit <- with(imp, lm(chl ~ age + bmi + hyp))
# fit is a 'mira' object. stands for Multiple Imputation Repeated Analyses
# It contains the linear models (lm) fitted on each imputed dataset


# pool all models together using Rubin's rules #############

# Rubin's rules average the parameter estimates across models,
# take into account the additional imputation uncertainty when estimating parameter standard errors

summary(pool(fit))
# linear regression model interpreted in the usual way


summary(getfit(fit, i = 1))





