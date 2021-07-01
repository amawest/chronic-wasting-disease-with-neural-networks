# =============================================================================
# Amanda West
# CWD Analysis | Downsampling & Cross Validation
# =============================================================================
# setup
library(dplyr)
#library(stargazer)
#library(caret)
#library(pROC)
setwd('/Users/amawest/Desktop')
rm(list=ls())
deer <- read.csv('DMA1_withlatlong.csv')  # with original dates
X    <- read.csv('X.csv')                 # without original dates
X    <- subset(X, select = -c(X)) # thanks, Hadley! 
X$Status <- deer$Status
# =============================================================================
# Cast column to correct object type (cat. or int)
str(X)
# recast some cols to categorical (check w/ above)
X$Sample.Date <- factor(X$Sample.Date) 
X$Sex <- factor(X$Sex)
X$Active...Hunter.Killed <- factor(X$Active...Hunter.Killed)  
X$Active...Other <- factor(X$Active...Other)  
X$Active...Road.Killed <- factor(X$Active...Road.Killed)  
X$Targeted...Other <- factor(X$Targeted...Other)  
X$Targeted.Clinical.suspect <- factor(X$Targeted.Clinical.suspect) 
X$Status <- factor(X$Status)  
# =============================================================================
# Reduce sample size to twice positive cases
set.seed(19)
Just_Neg <- X[X$Status == 0,]
rows <- sample(nrow(Just_Neg))
Just_Neg <- Just_Neg[rows[1:85], ]
Just_Pos <- X[X$Status == 1,]
Combined <- rbind(Just_Pos, Just_Neg)
rows <- sample(nrow(Combined))
Combined <- Combined[rows, ]
# =============================================================================
# Split Test & Train
sample <- sample(nrow(Combined),floor(nrow(Combined)*0.8))
train <- Combined[sample,]
test <- Combined[-sample,]
# =============================================================================
train_control <- trainControl(method = "cv", number = 5)
logit <- train(Status ~ .,
               data = train,
               trControl = train_control,
               method = "glm",
               family=binomial(),
               na.action=na.exclude)
predictions <- predict(logit, newdata = test)
str(predictions)
confusionMatrix(data = predictions, test$Status)
# =============================================================================

# =============================================================================
# Amanda West
# CWD Analysis | Full Dataset (WORST)
# =============================================================================
# ideas:
# - just same number negative as we have positive cases
# - add in cross validation
# - experiment with different test sizes
# - add in bootstrapping
# ---- https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train
# - classify lat & long as proper coordinates
# - xgboost? 
# - time series?
# - categorize year as ordinal
# - bootstrapping

# already tried:
# - use mile marker in lieu of lat & long --> TRIED, NOT AS ACCURATE
# =============================================================================
# setup
library(dplyr)
library(stargazer)
library(caret)
library(pROC)
setwd('/Users/amawest/Desktop')
rm(list=ls())
deer <- read.csv('DMA1_withlatlong.csv')  # with original dates
X    <- read.csv('X.csv')                 # without original dates
X    <- subset(X, select = -c(X)) # thanks, Hadley! 
X$Status <- deer$Status
# =============================================================================
# Cast column to correct object type (cat. or int)
str(X)
# recast some cols to categorical (check w/ above)
X$Sample.Date <- factor(X$Sample.Date) 
X$Sex <- factor(X$Sex)
X$Active...Hunter.Killed <- factor(X$Active...Hunter.Killed)  
X$Active...Other <- factor(X$Active...Other)  
X$Active...Road.Killed <- factor(X$Active...Road.Killed)  
X$Targeted...Other <- factor(X$Targeted...Other)  
X$Targeted.Clinical.suspect <- factor(X$Targeted.Clinical.suspect) 
X$Status <- factor(X$Status)  
# =============================================================================
# Split Test & Train
set.seed(19)
sample <- sample(nrow(X),floor(nrow(X)*0.8))
train <- X[sample,]
test <- X[-sample,]
# =============================================================================
# Run logistic regression
logit <- glm(Status ~ ., data = train, family = "binomial")
test$pred <- predict(logit, test, type="response")
test$predictions <- factor(ifelse(test$pred > 0.008, 1, 0))
confusionMatrix(test$predictions, 
                test$Status)
# =============================================================================
# Sources
# - http://www.science.smith.edu/~jcrouser/SDS293/labs/lab7-r.html
# - https://rpubs.com/dvorakt/255527
# - https://rpubs.com/dtime/672367
# - https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# =============================================================================

# =============================================================================
# Amanda West
# CWD Analysis | Downsampling (BEST)
# =============================================================================
# setup
library(dplyr)
library(stargazer)
library(caret)
library(pROC)
setwd('/Users/amawest/Desktop')
rm(list=ls())
deer <- read.csv('DMA1_withlatlong.csv')  # with original dates
X    <- read.csv('X.csv')                 # without original dates
X    <- subset(X, select = -c(X)) # thanks, Hadley! 
X$Status <- deer$Status
# =============================================================================
# Cast column to correct object type (cat. or int)
str(X)
# recast some cols to categorical (check w/ above)
X$Sample.Date <- factor(X$Sample.Date) 
X$Sex <- factor(X$Sex)
X$Active...Hunter.Killed <- factor(X$Active...Hunter.Killed)  
X$Active...Other <- factor(X$Active...Other)  
X$Active...Road.Killed <- factor(X$Active...Road.Killed)  
X$Targeted...Other <- factor(X$Targeted...Other)  
X$Targeted.Clinical.suspect <- factor(X$Targeted.Clinical.suspect) 
X$Status <- factor(X$Status)  
# =============================================================================
# Reduce sample size to twice number positive cases present
set.seed(19)
Just_Neg <- X[X$Status == 0,]
rows <- sample(nrow(Just_Neg))
Just_Neg <- Just_Neg[rows[1:85], ]
Just_Pos <- X[X$Status == 1,]
Combined <- rbind(Just_Pos, Just_Neg)
# =============================================================================
# Split Test & Train
sample <- sample(nrow(Combined),floor(nrow(Combined)*0.8))
train <- Combined[sample,]
test <- Combined[-sample,]
# =============================================================================
# Run logistic regression
logit <- glm(Status ~ ., data = train, family = "binomial")
test$pred <- predict(logit, test, type="response")
test$predictions <- factor(ifelse(test$pred > 0.5, 1, 0))
confusionMatrix(test$predictions, 
                test$Status)
# =============================================================================
# Sources
# - http://www.science.smith.edu/~jcrouser/SDS293/labs/lab7-r.html
# - https://rpubs.com/dvorakt/255527
# - https://rpubs.com/dtime/672367
# dslkfjsdlkjfsdlkj
# =============================================================================
