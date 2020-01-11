#################################################
#              Lectures 21                      #
#  Penalized Methods: Ridge and Lasso Regresion #
#         Last updated: 12.05.2019              #
#################################################


rm(list = ls())

install.packages(c('dplyr','faraway','glmnet'))
       

library('dplyr')
library('faraway')
library('MASS')                           # for lm.ridge()
library('glmnet')                         # for glmnet()
library('dplyr')
library('reshape2')
library('ggplot2')


# Read R dataset 'state.x77 that contains information on 50 states from 1970s collected by US Census Bureau. 
# The goal is to predict 'life expectancy' using a combination of remaining variables.

dat_state<- data.frame(state.x77)

# Data attributes
names(dat_state)
dim(dat_state)
head(dat_state)

# Start with the full model
mult.fit <- lm(Life.Exp ~ ., data=dat_state)
summary(mult.fit)

# Notice several coeff of small magnitudes.

#####################################################
#                 Ridge Regression                  #
#####################################################

# Can use lm.ridge() or glmnet()

ridge1 <- lm.ridge(Life.Exp ~., data=dat_state)
ridge1

# Compare the LS and Ridge coefficients.
# No difference because the default value for lambda (tunning parameter) is set to 0 by default.

coef(ridge1)
coef(mult.fit)


# Try a grid of values for lambda: from 10^-2 to 10^5

grid <- 10^seq(5,-2, length=100)


# Matrix of 100X8 containing coefficients for all 100 values of lambda
ridge2 <- lm.ridge(Life.Exp ~., data=dat_state, lambda=grid)
dim(coef(ridge2))

# What are the coeffcients for 10^5?
coef(ridge2)[1,]
coef(mult.fit)

# What are the coeffcients for 10^-2?
coef(ridge2)[100,]
coef(mult.fit)

######################################################
#            Use function glmnet()                   #
######################################################

# For this function, you have to declare your Y and Xs

Y <- dat_state[,4]

X <- as.matrix(dat_state[,-4])


# Penalty term: alpha; Ridge alpha=0 (default); Lasso alpha=1 (default)

ridge3<-glmnet(X, Y, alpha=0, lambda=grid)
dim(coef(ridge3))


# Look at lambda and the coeff estimates on position 50
ridge3$lambda[50]          # Lambda=34.30469

coef(ridge3)[,50]

# L2 norm for Ridge
# sqrt(sum(coef(ridge3)[-1,50]^2))

###########################################################
#          Choice of lambda: Cross Validation (CV)        #
###########################################################

# Choose the training sample: 50:50 split.
# 25 observations for training and 25 for testing.

set.seed(1)

train<-sample(1:nrow(X),nrow(X)/2)

test<-(-train)

Y.test<-Y[test]

# Use build-in CV function; performs a 10-fold validation by default
# glmnet() generates it's own lambda sequence

set.seed(2)
cv.out.ridge<-cv.glmnet(X[train,],Y[train], alpha=0)
plot(cv.out.ridge)

# cv.glmnet() object contains the mean cross-validation error (cvm),
# lambda min that gives the minimum cvm, etc.

cv.out.ridge        # Gives best/min lambda=0.392 and the correspondng MSE=1.030

best.lambda.ridge<-cv.out.ridge$lambda.min
best.lambda.ridge   # 0.392

# Next we fit a ridge regression model on the training set, and evaluate its MSE on the test set. 
ridge_mod<-glmnet(X[train,],Y[train], alpha=0, lambda=grid)
plot(ridge_mod)

# We use predict() function to get predictions for a test set, by replacing type="coefficients" with the newx argument.

ridge.pred <- predict(ridge_mod, s=best.lambda.ridge, newx=X[test,])

#MSE
MSE_ridge <- mean((ridge.pred-Y[test])^2) # MSE from test data = 1.035, close to the training.
MSE_ridge


# Ridge regression using all observations and 'best' lambda
ridge3<-glmnet(X, Y, alpha=0, lambda=best.lambda.ridge)

# Compare LS and Ridge coefficients
res_ridge_ls<- cbind(coef(mult.fit),coef(ridge3))
colnames(res_ridge_ls) <- c("LS", "Ridge")

res_ridge_ls

#############################################################################
#                           Lasso regression                                #
#############################################################################

# Find the best.lambda
cv.out.lasso<-cv.glmnet(X[train,],Y[train], alpha=1)

cv.out.lasso        # Gives best/min lambda=0.0654 and the correspondng MSE=0.5984

best.lambda.lasso<-cv.out.lasso$lambda.min  

# Fit a Lasso model with all observations with the best lambda lasso
lasso2<- glmnet(X, Y, alpha =1, lambda=best.lambda.lasso)
coef(lasso2)

# Fit a Lasso model with all observations with the best lambda ridge
lasso3<- glmnet(X, Y, alpha =1, lambda=best.lambda.ridge)
coef(lasso3)

# Fraction of deviance explained
# Similar interpretation to R-squared: % variance explained by non-zero variables variables
lasso2$dev.ratio

# Compare LS, Ridge and Lasso regression coefficients (use lambda ridgeas - higher - gives more skrinkage)
res_ls_ridge_lasso<- cbind(coef(mult.fit),coef(ridge3),coef(lasso3))
colnames(res_ls_ridge_lasso) <- c("LS", "Ridge","Lasso")
res_ls_ridge_lasso

##############################################################################
#                             Additional code                                #
##############################################################################
# Using the entire data, fit Lasso regressions using the lambda grid used above.
# Save the estimated 'standardized' coefficients for all 7 predictors and plot them as functions of lambda or log(lambda).
# Use different colors for each variable and add a legend. 


# Using the entire data, fit Lasso regressions using the lambda grid.
lasso4 <- glmnet(X,Y, alpha=1, lambda=grid)

# Save the estimated 'standardized' coefficients for all 7 predictors without the intercept that is not of interest.
coef_lasso4 <- coef(lasso4)[-1,]
# Transpose the matrix
coef_lasso4_mat <- t(as.matrix(coef_lasso4))
# Rename and sort the matrix by asceding  grid
rownames(coef_lasso4_mat) <- grid
coef_lasso4_mat_sort <- coef_lasso4_mat[order(grid),]

# Plot using different colors
matplot(coef_lasso4_mat_sort,type="l",lty=1,xlim=c(0,50),
        xlab="lambda",ylab="coefficient",col=1:7) 
### add legend
legend('bottomright', inset=.05, legend=colnames(coef_lasso4_mat_sort), 
       pch=20, cex=0.8, col=1:7)

# Because of the different magnitudes, some of the predictors are not visible.
# You can separate them in different plots or play with the y-limits.



