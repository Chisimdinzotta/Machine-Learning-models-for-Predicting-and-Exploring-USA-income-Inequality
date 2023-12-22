
#REGRESSION

#Loading the data
setwd(dirname(file.choose()))

# Load the IPUMS data dictionary and microdata
library(ipumsr)
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)
df <- as.data.frame(data)
write.csv(df, "ipums_data.csv", row.names = FALSE)
income_data <- read.csv("ipums_data.csv", stringsAsFactors = FALSE)

#Loading libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(Amelia)
library(rcompanion)
library(nortest)

#---------------DATA EXPLORATION---------------------------------

head(income_data)
str(income_data)
summary(income_data)
summary(income_data$INCWAGE)
#Scatter plot matrix for all variables in R
pairs(income_data, main = "Scatter Plot Matrix for All Variables")
plot(income_data)

#Check for missing data
apply(income_data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
#missingness map
missmap(income_data, col = c("black", "grey"), legend = FALSE)

#Box plot with outliers
boxplot(income_data, outline=TRUE, main="Box Plot with Outliers")
#Extract outliers
outliers <- boxplot(income_data, plot=FALSE)$out
#Print out the outliers
cat("Outliers:", outliers, "\n")
#Scatter plot for outliers
plot(rep(1, length(outliers)), outliers, pch=16, col="red",
     xlab="", ylab="Outliers", main="Outliers Scatter Plot")

#..............Exploration of the dependent variable of INCWAGE..............

#Boxplot of INCWAGE
boxplot(income_data$INCWAGE, main = "Boxplot of the dependent variables", xlab = "INCWAGE", ylab = "Range")
#Plot of INCWAGE
plotNormalHistogram(income_data$INCWAGE, main = "Histogram", xlab = "mpg")
#Histogram of INCWAGE
ggplot(income_data, aes(x = INCWAGE)) +
  geom_histogram(binwidth = 5000, color = "black", fill = "lightblue") +
  labs(title = "Income Wage Distribution", x = "Income Wage", y = "Frequency") +
  theme_minimal()
hist(income_data$INCWAGE)

#.............Exploration of the independent variables.......................

#Boxplot 
boxplot(income_data[c(-16)])
#Histogram of priority variables
#EMPLOYMENT STATUS
hist(income_data$EMPSTAT, main = "Employment Status", xlab = "EMPSTAT", ylab = "Population")
# Create legend
legend.labels <- c("0: N/A", "1: Employed", "2: Unemployed", "3: Not in labor force")
# Add the legend to the histogram
legend("center", legend = legend.labels)

#SEX
hist(income_data$SEX, main = "Male/Female", xlab = "Sex", ylab = "Population")
# Create legend
legend.labels <- c("1: Male", "2: Female")
# Add the legend to the histogram
legend("topright", legend = legend.labels)
#Checking how many people are black
sum(income_data$SEX == 1)
#Checking how many people are white
sum(income_data$SEX == 2)

#RACE - Asian
hist(income_data$RACASIAN, main = "Race - Asian", xlab = "RACASIAN", ylab = "Population")
# Create legend
legend.labels <- c("1: No", "2: Yes")
# Add the legend to the histogram
legend("topright", legend = legend.labels)

#RACE - Black
hist(income_data$RACBLK, main = "Race - Black", xlab = "RACBLK", ylab = "Population")
# Create legend
legend.labels <- c("1: No", "2: Yes")
# Add the legend to the histogram
legend("topright", legend = legend.labels)

#RACE - White
hist(income_data$RACWHT, main = "Race - White", xlab = "RACWHT", ylab = "Population")
# Create legend
legend.labels <- c("1: No", "2: Yes")
# Add the legend to the histogram
legend("center", legend = legend.labels)

#Scatterplot for further visualisation
plot(income_data)
#......................DATA WRANGLING........................
#Remove catergories N/A(0), Unemployed (2), and Not in labor force (3) from EMPSTAT
#and only leave the rows in the employed category (1)
income_data<- subset(income_data, !(EMPSTAT %in% c(0, 2, 3)))

#Replace RACASIAN values to 1 and turn those not RACASIAN to 0
income_data$RACASIAN <- ifelse(income_data$RACASIAN == 2, 1, ifelse(income_data$RACASIAN == 1, 0, income_data$RACASIAN))
#Leave RACBLK values as 2 and turn those not RACBLK to 0
income_data$RACBLK<- ifelse(income_data$RACBLK == 2, 2, ifelse(income_data$RACBLK == 1, 0, income_data$RACBLK))
#Replace RACWHT values to 3 and turn those not RACWHT to 0
income_data$RACWHT<- ifelse(income_data$RACWHT == 2, 3, ifelse(income_data$RACWHT == 1, 0, income_data$RACWHT))

#Remove races that are neither black, white or asian
income_data <- income_data[!(income_data$RACWHT == 0 & income_data$RACBLK == 0 & income_data$RACASIAN == 0), ]

#Create a new column named "RACE" by combining the RACWHT, RACBLK, and RACASIAN columns
income_data$RACE <- ifelse(income_data$RACWHT > 0, 1, 
                           ifelse(income_data$RACBLK > 0, 2,
                                  ifelse(income_data$RACASIAN > 0, 3, 0)))
#Checking how many people are asian
sum(income_data$RACE == 1)
#Checking how many people are black
sum(income_data$RACE == 2)
#Checking how many people are white
sum(income_data$RACE == 3)

#Remove irrelevant (repetitive variables) and the RACASIAN, RACBLK and RACWHT 
#indivually since they are now joined into one colume
income_data <- subset(income_data, select = -c(RACASIAN, RACBLK, RACWHT, EDUCD,
                                               DEGFIELDD,EMPSTAT, 
                                               EMPSTATD))

#Removing rows with 0 income, leaving 1296354 observations
income_data <- income_data[!(income_data$INCWAGE == 0), ]

#Changing categorical variables to factors
income_data$SEX = factor(income_data$SEX)
income_data$MARST = factor(income_data$MARST)
income_data$EDUC = factor(income_data$EDUC)
income_data$DEGFIELD = factor(income_data$DEGFIELD)
income_data$OCC = factor(income_data$OCC)
income_data$IND = factor(income_data$IND)
income_data$RACE = factor(income_data$RACE)

#Check the youngest age - 16
youngest_age <- min(income_data$AGE)
cat("The youngest age in the data frame is:", youngest_age)
#Check the oldest age - 95
oldest_age <- max(income_data$AGE)
cat("The oldest age in the data frame is:", oldest_age)

#Filter out any rows where the age is outside of the age range for the labor force (16 to 64)
income_data <- income_data[income_data$AGE >= 16 & income_data$AGE <= 64, ]

#Filter out rows that UHRSWORK with values less than 10 hours or more than 40 hours 
#Leavig 863343 observations
income_data <- income_data[income_data$UHRSWORK >= 10 & income_data$UHRSWORK <= 40, ]
str(income_data)

#................................NORMALISATION.................................

#Check the standard deviation of the numerical variable to see the spread of the data
#Find out how spread out they are from the mean or the average distance
nums <- sapply(income_data, is.numeric)
num_variable <- names(nums[nums==TRUE])
print(num_variable)
#check the range
apply(income_data[,num_variable],2,range)
#Check standard deviation
apply(income_data[,num_variable],2,sd)

#Check if dependent variable is normalised
#Statistical tests
lillie.test(income_data$INCWAG)
#K-S test
ks.test(income_data$INCWAGE, "pnorm", mean(income_data$INCWAGE), sd(income_data$INCWAGE))
#Graphically
hist(income_data$INCWAG, main = "Histogram", xlab = "INCWAG")
plotNormalHistogram(income_data$INCWAG, main = "Histogram", xlab = "INCWAG")
qqnorm(income_data$INCWAG, xlab = "INCWAG")
qqline(income_data$INCWAG, col = 2)

#Min-max scaling:
#income_data$INCWAG_norm <- (income_data$INCWAG - min(income_data$INCWAG)) / (max(income_data$INCWAG)                                                                 # - min(income_data$INCWAG))
#Z-score normalisation:
#income_data$INCWAG_norm <- (income_data$INCWAG - mean(income_data$INCWAG)) / sd(income_data$INCWAG)
#Mean-variance normalisation
#income_data$INCWAG_norm <- (income_data$INCWAG - mean(income_data$INCWAG)) / (2 * sd(income_data$INCWAG))

#Apply log transformation to INCWAG
#Convert census_data_7$INCWAG to int
as.integer(income_data$INCWA)
income_data$log_INCWAG <- log(income_data$INCWAG)
#log transformation led to the best normalisation of INCWAG 
#and  better model out of other normalisation methods tried

#Test normalisation after log normalisation
lillie.test(income_data$log_INCWAG)
#Histogram of log_INCWAG
hist(income_data$log_INCWAG, main = "Histogram", xlab = "INCWAG")
plotNormalHistogram(income_data$log_INCWAG, main = "Histogram", xlab = "INCWAG")

#QQ plot of log_INCWAG
qqnorm(income_data$log_INCWAG, xlab = "Income")
qqline(income_data$log_INCWAG, col = 2)

#Remove the unnormalised dependent variable
income_data <- subset(income_data, select = -c(INCWAGE))
#Create a new data frame with just the numerical variable 
#and the normalised dependent variable
# Create a new data frame with selected variables
usa_income_int <- data.frame(AGE = income_data$AGE, 
                             UHRSWORK = income_data$UHRSWORK, 
                             log_INCWAG = income_data$log_INCWAG)

# Print the new data frame
head(usa_income_int)
boxplot(usa_income_int)

#Normalisation of the numerical independent variables (Age and UHRSWORK)
# Define a function for min-max normalisation
min_max_normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply min-max normalisation to AGE and UHRSWORK variables
usa_income_int$AGE_norm <- min_max_normalise(usa_income_int$AGE)
usa_income_int$UHRSWORK_norm <- min_max_normalise(usa_income_int$UHRSWORK)
head(usa_income_int)

#Add the normalised and standardised variables into a dataframe
usa_income_transformed <- data.frame(AGE = usa_income_int$AGE_norm, 
                                     UHRSWORK = usa_income_int$UHRSWORK_norm, 
                                     log_INCWAG = income_data$log_INCWAG)
head(usa_income_transformed)
boxplot(usa_income_transformed)

#Correlation between numerical and independent variable
cor.test(usa_income_transformed$log_INCWAG, usa_income_transformed$AGE, method = "spearman")
cor.test(usa_income_transformed$log_INCWAG, usa_income_transformed$UHRSWORK, method = "spearman")

#Joining transformed variables and the rest of the variables
str(income_data)
#add the transformed variables and the categorical variables together
usa_income <- cbind(income_data, usa_income_transformed)
str(usa_income)

#..................CORRELATION..............................................
#Dependent and independent variables
table <-table(income_data$INCWAG , income_data$SEX)
table <-table(income_data$INCWAG , income_data$AGE)
table <-table(income_data$INCWAG , income_data$MARST)
table <-table(income_data$INCWAG , income_data$EDUC)
table <-table(income_data$INCWAG , income_data$DEGFIELD)
table <-table(income_data$INCWAG , income_data$OCC)
table <-table(income_data$INCWAG , income_data$IND)
table <-table(income_data$INCWAG , income_data$UHRSWORK)
table <-table(income_data$INCWAG , income_data$RACE)

chisq.test(table)
str(income_data)
#Correlation matrix
set.seed(12345)
warnings()
library(polycor)
incomeData.cor <- hetcor(income_data, use = "pairwise.complete.obs")
# print correlations
round(incomeData.cor$correlations, 2)
library(corrplot)
corrplot(incomeData.cor$correlations, is.corr = TRUE, type = "upper", tl.col = "black", tl.srt = 45)

#Dependent and independent numerical variables
cor.test(income_data$INCWAG, income_data$UHRSWORK, method = "spearman")
cor.test(income_data$INCWAG, income_data$AGE, method = "spearman")

#--------------------------EVALUATION FORMULA---------------------------------

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

#-------------------------------SPLITTING------------------------------------
library(dplyr)
# set the seed for reproducibility
set.seed(123)
# randomly select 10% of rows from income_data_transformeder
sampled.rf <- usa_income %>% 
  sample_frac(0.1)
# view the first few r§§ows of the new dataframe
str(usa_income )

set.seed(12345)
incomeData <- usa_income[order(runif(863343)), ]
incomeData.tr <- incomeData[1:690674, ]     # 80%
incomeData.te <- incomeData[690675:863343, ]   # 20%
# check the distribution of target variable
boxplot(incomeData$INCWAG, incomeData.tr$INCWAG, incomeData.te$INCWAG,
        names = c("INCWAG all", "INCWAG train", "INCWAG test"))


#-----------------------------LINEAR REGRESSION---------------------------------------
#Linear Regression
#Race
model1 <- lm(incomeData.tr$log_INCWAG ~ incomeData.tr$RACE, data = incomeData.tr)
summary(model1)
vif(model1)
sqrt(vif(model1)) > 2

#Sex
model2 <- lm(incomeData.tr$log_INCWAG ~ incomeData.tr$SEX, data = incomeData.tr)
summary(model2)
vif(model2)
sqrt(vif(model2)) > 2

#-----------------------------MULTIPLE REGRESSION----------------------------
#Model with all variables
model3<- lm(incomeData.tr$log_INCWAG ~ ., data = incomeData.tr)
summary(model3)

vif(model3)
sqrt(vif(model3)) > 2
# relative importance of variables
library(relaimpo)
calc.relimp(model3, type = c("lmg"), rela = TRUE)

#Variables from feature selection after correlation analysis  
model4 <- lm(incomeData.tr$log_INCWAG ~ incomeData.tr$AGE + incomeData.tr$UHRSWORK + incomeData.tr$MARST + 
               incomeData.tr$EDUC + incomeData.tr$OCC, data = incomeData.tr)
summary(model4)
vif(model4)
sqrt(vif(model4)) > 2

#Fewer variables after feature selection
model5 <- lm(incomeData.tr$log_INCWAG ~ incomeData.tr$AGE +incomeData.tr$SEX +
               incomeData.tr$UHRSWORK + incomeData.tr$MARST + 
               incomeData.tr$EDUC, data = incomeData.tr)
summary(model5)
vif(model5)
sqrt(vif(model5)) > 2

# use a stepwise approach to search for a best model
library(RcmdrMisc)
#Forward stepwise selection
model6 <- stepwise(model3, direction = "forward/backward")

#Model selected after stepwise
model_step <- lm(incomeData.tr$log_INCWAG ~ UHRSWORK + SEX + RACE + AGE + EDUC + MARST +OCC,
                 data = incomeData.tr)
model_step
summary(model_step)
sqrt(vif(model_step)) > 2

# relative importance of variables
library(relaimpo)
calc.relimp(model_step, type = c("lmg"), rela = TRUE)
#Calculate the relative importance of each predictor using the LMG method
rel_imp <- calc.relimp(model_step, type = c("lmg"), rela = TRUE)

# Sort the results in descending order
rel_imp_sorted <- sort(rel_imp, decreasing = TRUE)
rel_imp <- calc.relimp(model_step, type = "lmg", rela = TRUE)
rel_imp_sorted <- sort(rel_imp@lmg, decreasing = TRUE)
# Print the results
rel_imp_sorted
#visualise
barplot(rel_imp_sorted, horiz = FALSE, main = "Relative Importance of Predictors")


hist(model_step$residuals)
rug(model_step$residuals)
plot(model_step$residuals ~ model_step$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model_step$residuals, "pnorm", mean(model_step$residuals), sd(model_step$residuals))

# test if model_step and model3 are significantly different using F test
anova(model_step, model3, test = "F")

# remove the dependent variable 
newdata.new <- incomeData.te[, -10]
str(newdata.new)

#prediction using the test data
pred <- predict.lm(model_step, newdata.new, se.fit=TRUE)

# Plot actual versus predicted values
boxplot(incomeData.te$log_INCWAG, pred$fit,
        xlab="Actual          vs         Predicted", col = "grey")
plot(incomeData.te$log_INCWAG, pred$fit, main="Scatterplot",
     xlab="INCWAG actual", ylab="INCWAG predict")

#  Calculate RMSE 
error <- incomeData.te$log_INCWAG -  pred$fit
rmse(error)
# Calculate MAE
mae(error)

# R-squared
resid.mod <- lm(incomeData.te$log_INCWAG ~  pred$fit)
summary(resid.mod)


#...................Testing research hypothesis/questions.........................

str(income_data1)
library(dplyr)

income_data1 %>%
  group_by(SEX) %>%
  summarise(avg_income = mean(INCWAGE))

income_data1 %>%
  group_by(RACE) %>%
  summarise(avg_income = mean(INCWAGE))


# calculate the mean income by gender
mean_income_gender <- aggregate(INCWAGE ~ SEX, data = income_data1, FUN = mean)
mean_income_gender

# create a bar plot
ggplot(mean_income_gender, aes(x = SEX, y = INCWAGE, fill = SEX)) + 
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Mean Income") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Male", "Female"))

# calculate the mean income by race
mean_income_race <- aggregate(INCWAGE ~ RACE, data = income_data1, FUN = mean)
mean_income_race

# create a bar plot
ggplot(mean_income_race, aes(x = RACE, y = INCWAGE, fill = RACE)) + 
  geom_bar(stat = "identity") +
  labs(x = "Race", y = "Mean Income") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#800080"), labels = c("Asian", "Black", "White"))


# remove all variables from the environment
rm(list=ls())


#DECISION TREE

#Loading the data
setwd(dirname(file.choose()))
library(ipumsr)
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)
df <- as.data.frame(data)
write.csv(df, "ipums_data.csv", row.names = FALSE)
income_data <- read.csv("ipums_data.csv", stringsAsFactors = FALSE)

library(tidyr)
library(dplyr)
str(income_data)


#---------------Data Wrangling.............................................
#Remove catergories N/A(0), Unemployed (2), and Not in labor force (3) from EMPSTAT
#and only leave the rows in the employed category (1)
income_data<- subset(income_data, !(EMPSTAT %in% c(0, 2, 3)))
#Checking how many people are white
sum(income_data$RACWHT == 2)
#Encode RACASIAN to 1
#Encode RACBLK to 2
#Encode RACWHT to 3
income_data$RACASIAN <- ifelse(income_data$RACASIAN == 2, 1, ifelse(income_data$RACASIAN == 1, 0, income_data$RACASIAN))
income_data$RACBLK<- ifelse(income_data$RACBLK == 2, 2, ifelse(income_data$RACBLK == 1, 0, income_data$RACBLK))
income_data$RACWHT<- ifelse(income_data$RACWHT == 2, 3, ifelse(income_data$RACWHT == 1, 0, income_data$RACWHT))
#Remove races that are neither black, white or asian
income_data <- income_data[!(income_data$RACWHT == 0 & income_data$RACBLK == 0 & income_data$RACASIAN == 0), ]

#Create a new column named "race" by combining the RACWHT, RACBLK, and RACASIAN columns
income_data$RACE <- ifelse(income_data$RACWHT > 0, 1, 
                           ifelse(income_data$RACBLK > 0, 2,
                                  ifelse(income_data$RACASIAN > 0, 3, 0)))
sum(income_data$RACE == 1) #white
sum(income_data$RACE == 2) #black
sum(income_data$RACE == 3) #asian

#Remove the indivual category, RACASIAN, RACBLK and RACWHT since they are now joined 
#Remove irrelevant category as they repeat existinng variables
income_data <- subset(income_data, select = -c(RACASIAN, RACBLK, RACWHT, EDUCD,
                                               DEGFIELDD,EMPSTAT, 
                                               EMPSTATD))
# Find the youngest age - 16
youngest_age <- min(income_data$AGE)
cat("The youngest age in the data frame is:", youngest_age)
# Find the oldest age - 95
oldest_age <- max(income_data$AGE)
cat("The oldest age in the data frame is:", oldest_age)

#Remove rows where age is not in the age bracket of the labour force 16 to 64 years old.
income_data <- income_data[income_data$AGE >= 16 & income_data$AGE <= 64, ]

str(income_data)
#Remove rows that work less than 10 hours or more than 40 hours (863343)
income_data <- income_data[income_data$UHRSWORK >= 10 & income_data$UHRSWORK <= 40, ]

#Remove rows where people earn 0
income_data <- income_data[!(income_data$INCWAGE == 0), ]
summary(income_data$INCWAGE)
str( income_data)

#Turn dependent variable INCWAGE into two: greater than 40000 or less than 40000
#40000 was chosen because it is the 40000
income_data$INCWAGE <- ifelse(income_data$INCWAGE < 40000, 0, 1)
#Check the distribution of each category with a graph
sum(income_data$INCWAGE == 0) #428804
sum(income_data$INCWAGE == 1) #434539

#Turn dependent variable to factors
income_data$INCWAGE = factor(income_data$INCWAGE)
#Visulase the distributioon 
ggplot(income_data, aes(x = INCWAGE, fill = INCWAGE)) + 
  geom_bar() +
  scale_fill_manual(values = c("blue", "green"), name = "INCWAGE Category") +
  labs(title = "Distribution of Income Category")

str(income_data)

#............Normalisation...........................
#min_max_normalise <- function(x) {
#return((x - min(x)) / (max(x) - min(x)))
#}

# Apply min-max normalization to AGE and UHRSWORK variables
#It made no difference to the model result
#income_data$AGE <- min_max_normalise(income_data$AGE)
#income_data$UHRSWORK <- min_max_normalise(income_data$UHRSWORK)


#.............................DECISION TREE MODELS.....................................

#Splitting the data
set.seed(12345)
income1 <- income_data[order(runif(863343)), ]
train <- income1[1:690674, ]     # 80%
test <- income1[690675:863343, ]   # 20%
#-----Section 05-------------------------------------------

#Building a simple decision tree
library(C50)
dt_model <- C5.0(train[-9], train$INCWAGE)
#c50 code called exit with value 1
dt_model
summary(dt_model)

# evaluating model performance
# create a factor vector of predictions on test data
pred1 <- predict(dt_model, test)
summary(pred1)
# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(test$INCWAGE, pred1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual INCWAGE', 'Predicted INCWAGE'))

#More diagnostics
library(caret)
confusionMatrix(pred1, test$INCWAGE, positive = "1")

# pruning the tree to simplify and avoid over-fitting
set.seed(123)
dt_model_prune <- C5.0(train[-9], train$INCWAGE,
                       control = C5.0Control(minCases = 9))
dt_model_prune
summary(dt_model_prune)
#Make prediction
dt_model_prune_pred <- predict(dt_model_prune, test)
CrossTable(test$INCWAGE, dt_model_prune_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual INCWAGE', 'Predicted INCWAGE'))

confusionMatrix(dt_model_prune_pred, test$INCWAGE, positive = "1")

#Boosting decision tree with 10 trials
set.seed(1234)
dt_model_boost10 <- C5.0(train[-9], train$INCWAGE, control = C5.0Control(minCases = 9), trials = 10)
dt_model_boost10

summary(dt_model_boost10)
boost_pred <- predict(dt_model_boost10, test)
library(gmodels)
CrossTable(test$INCWAGE, boost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual INCWAGE', 'Predicted INCWAGE'))
confusionMatrix(boost_pred, test$INCWAGE, positive = "1")
conf_mat <-confusionMatrix(boost_pred, test$INCWAGE, positive = "1")
conf_mat

library (ROCR)
library (C50)
# obtain ROC and AUC
prob <- as.data.frame(predict(dt_model_boost10, test, type = "prob"))
res <- as.data.frame(cbind(boost_pred, prob))
head(res)
pred <- prediction(predictions = res$"1", labels = dt_model_boost10$INCWAGE)
# ROC
perf1 <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf1, lwd = 2)
abline(a = 0, b = 1, lty = 2)
#AUC
perf2 <- performance(pred,  measure ="auc")
perf2@y.values

library(caret)
# Get variable importance
var_imp <- varImp(dt_model_boost10, scale = FALSE)
# Plot variable importance
var_imp <- varImp(dt_model_boost10, scale = FALSE)
var_imp

# create a dataframe with actual and predicted values
dt_results <- data.frame(actual = test$INCWAGE, predicted = predict(dt_model, test))

# calculate accuracy
accuracy <- sum(dt_results$actual == dt_results$predicted)/nrow(dt_results)

# plot accuracy
ggplot(dt_results, aes(x = actual, fill = predicted)) + 
  geom_bar() +
  labs(title = paste("Decision Tree Accuracy: ", round(accuracy * 100, 2), "%"))

#..................Answer research question with model………………………………..
# Create contingency table of predicted income level by race
#white is 1 Black is 2 Asian is 3

table(boost_pred, test$RACE)
# Create contingency table of predicted income level by sex
table(boost_pred, test$SEX)
#Male is 1 Female is 2

#Values for both sex earning above and below $50k from the contingency table
male.income.above <- 48272
male.income.below <- 32758

female.income.above <- 41754
female.income.below <- 49885

# Visualise predicted income level by Sex
# Define the data
sex<- c("Male", "Female")
income_above_50k <- c(48272, 41754)
income_below_50k <- c(32758, 49885)

# Create a data frame
ff <- data.frame(sex, income_above_50k, income_below_50k)
#Barplot
colors <- c("#4daf4a", "#377eb8")
barplot(
  height = t(ff[,2:3]),
  names.arg = ff$sex,
  col = colors,
  main = "Predicted income level by gender",
  legend.text = c("Above $40k", "Below $40k"),
  args.legend = list(y = "center", x = "top"),
  beside = TRUE
)

#Values for race earning above and below $50k from the contingency table
white.income.above <- 73713
white.income.below <- 66141

black.income.above <- 7246
black.income.below <- 10993

asian.income.above <- 9067
asian.income.below <- 5509

# Visualise predicted income level by race
# Define the data
race <- c("White", "Black", "Asian")
income_above_50k <- c(73713, 7246, 9067)
income_below_50k <- c(66141, 10993, 5509)

# Create a data frame
df <- data.frame(race, income_above_50k, income_below_50k)
#Barplot
colors <- c("#4daf4a", "#377eb8")
barplot(
  height = t(df[,2:3]),
  names.arg = df$race,
  col = colors,
  main = "Predicted income level by race",
  legend.text = c("Above $40k", "Below $40k"),
  args.legend = list(x = "topright"),
  beside = TRUE
)



# remove all variables from the environment
rm(list=ls())


#..............................RANDOM FOREST MODELS……………………………

#Loading the data
setwd(dirname(file.choose()))
library(ipumsr)
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)
df <- as.data.frame(data)
write.csv(df, "ipums_data.csv", row.names = FALSE)
income_data <- read.csv("ipums_data.csv", stringsAsFactors = FALSE)

library(tidyr)
library(dplyr)
str(income_data)


#---------------Data Wrangling.............................................

#Remove catergories N/A(0), Unemployed (2), and Not in labor force (3) from EMPSTAT
#and only leave the rows in the employed category (1)
income_data<- subset(income_data, !(EMPSTAT %in% c(0, 2, 3)))
#Checking how many people are white
sum(income_data$RACWHT == 2)
#Encode RACASIAN to 1
#Encode RACBLK to 2
#Encode RACWHT to 3
income_data$RACASIAN <- ifelse(income_data$RACASIAN == 2, 1, ifelse(income_data$RACASIAN == 1, 0, income_data$RACASIAN))
income_data$RACBLK<- ifelse(income_data$RACBLK == 2, 2, ifelse(income_data$RACBLK == 1, 0, income_data$RACBLK))
income_data$RACWHT<- ifelse(income_data$RACWHT == 2, 3, ifelse(income_data$RACWHT == 1, 0, income_data$RACWHT))
#Remove races that are neither black, white or asian
income_data <- income_data[!(income_data$RACWHT == 0 & income_data$RACBLK == 0 & income_data$RACASIAN == 0), ]

#Create a new column named "race" by combining the RACWHT, RACBLK, and RACASIAN columns
income_data$RACE <- ifelse(income_data$RACWHT > 0, 1, 
                           ifelse(income_data$RACBLK > 0, 2,
                                  ifelse(income_data$RACASIAN > 0, 3, 0)))
sum(income_data$RACE == 1) #white
sum(income_data$RACE == 2) #black
sum(income_data$RACE == 3) #asian

#Remove the indivual category, RACASIAN, RACBLK and RACWHT since they are now joined 
#Remove irrelevant category as they repeat existinng variables
income_data <- subset(income_data, select = -c(RACASIAN, RACBLK, RACWHT, EDUCD,
                                               DEGFIELDD,EMPSTAT, 
                                               EMPSTATD))
# Find the youngest age - 16
youngest_age <- min(income_data$AGE)
cat("The youngest age in the data frame is:", youngest_age)
# Find the oldest age - 95
oldest_age <- max(income_data$AGE)
cat("The oldest age in the data frame is:", oldest_age)

#Remove rows where age is not in the age bracket of the labour force 16 to 64 years old.
income_data <- income_data[income_data$AGE >= 16 & income_data$AGE <= 64, ]

str(income_data)
#Remove rows that work less than 10 hours or more than 40 hours (863343)
income_data <- income_data[income_data$UHRSWORK >= 10 & income_data$UHRSWORK <= 40, ]

#Remove rows where people earn 0
income_data <- income_data[!(income_data$INCWAGE == 0), ]
summary(income_data$INCWAGE)
str( income_data)

#Turn dependent variable INCWAGE into two: greater than 40000 or less than 40000
#40000 was chosen because it is the 40000
income_data$INCWAGE <- ifelse(income_data$INCWAGE < 40000, 0, 1)
#Check the distribution of each category with a graph
sum(income_data$INCWAGE == 0) #428804
sum(income_data$INCWAGE == 1) #434539

#Turn dependent variable to factors
income_data$INCWAGE = factor(income_data$INCWAGE)
#Visulase the distributioon 
ggplot(income_data, aes(x = INCWAGE, fill = INCWAGE)) + 
  geom_bar() +
  scale_fill_manual(values = c("blue", "green"), name = "INCWAGE Category") +
  labs(title = "Distribution of Income Category")

str(income_data)

#........................RANDOM FOREST......................

#Sampling
library(dplyr)
# set the seed for reproducibility
set.seed(123)
# randomly select 10% of rows from income_data
sampled.rf <- income_data %>% 
  sample_frac(0.1)
# view the first few rows of the new dataframe
str(sampled.rf )
sampled.rf$INCWAGE = factor(sampled.rf$INCWAGE)
set.seed(12345)
income.gb1 <- sampled.rf[order(runif(86334)), ]
rf.tr <- income.gb1[1:69067, ]     # 80%
rf.te <- income.gb1[69068:86334, ]   # 20%

str(rf.te)

# random forest with default settings
library(randomForest)
set.seed(12345)
rf <- randomForest(rf.tr$INCWAGE ~ ., data = rf.tr)
# summary of model
rf

# variable importance plot
varImpPlot(rf, main = "rf - variable importance")

#Calculate variable importance measures
var_imp <- importance(rf)
# Sort the variable importance measures in descending order
var_imp_sorted <- var_imp[order(var_imp[, 1], decreasing = TRUE), , drop = FALSE]
var_imp_sorted
# Print the top 5 most important variables
head(var_imp_sorted, 5)

# apply the model to make predictions
p <- predict(rf, rf.te)

# evaluate
library(gmodels)
library(caret)
CrossTable(rf.te$INCWAGE, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual INCWAGE', 'predicted INCWAGE'))
confusionMatrix(p, rf.te$INCWAGE, positive = "1")


library (ROCR)
# obtain ROC and AUC
prob <- as.data.frame(predict(rf, rf.te, type = "prob"))
res <- as.data.frame(cbind(p, prob))
head(res)
pred <- prediction(predictions = res$"1", labels = rf.te$INCWAGE)
# ROC
perf1 <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf1, lwd = 2)
abline(a = 0, b = 1, lty = 2)
#AUC
perf2 <- performance(pred,  measure ="auc")
perf2@y.values

# auto-tune a random forest

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid_rf <- expand.grid(.mtry = c(4, 8, 16, 32))
grid_rf

set.seed(12345)
rf <- train(rf.tr$INCWAGE ~ ., data = rf.tr, method = "rf",
            metric = "Kappa", trControl = ctrl,
            tuneGrid = grid_rf)
#summary of model
rf

# apply the model to make predictions
p <- predict(rf, rf.te)

# evaluate
CrossTable(rf.te$INCWAGE, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual INCWAGE', 'predicted INCWAGE'))
confusionMatrix(p, rf.te$INCWAGE, positive = "1")

# obtain ROC and AUC
prob <- as.data.frame(predict(rf, rf.te, type = "prob"))
res <- as.data.frame(cbind(p, prob))
head(res)
pred <- prediction(predictions = res$"1", labels = rf.te$INCWAGE)
# ROC
perf1 <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf1, lwd = 2)
abline(a = 0, b = 1, lty = 2)
#AUC
perf2 <- performance(pred,  measure ="auc")
perf2@y.values


# weight the costs using the cutoff parameter for voting

set.seed(12345)
rf <- randomForest(INCWAGE ~ ., data = rf.te, nodesize = 4, cutoff = c(.9,.1))
# summary of model
rf

# apply the model to make predictions
p <- predict(rf, rf.te)

# variable importance plot
varImpPlot(rf, main = "rf - variable importance")

# evaluate
CrossTable(rf.te$INCWAGE, p,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual INCWAGE', 'predicted INCWAGE'))
confusionMatrix(p, rf.te$INCWAGE, positive = "1")

# obtain ROC and AUC
prob <- as.data.frame(predict(rf, rf.te, type = "prob"))
res <- as.data.frame(cbind(p, prob))
head(res)
pred <- prediction(predictions = res$"1", labels = rf.te$INCWAGE)
# ROC
perf1 <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf1, lwd = 2)
abline(a = 0, b = 1, lty = 2)
#AUC
perf2 <- performance(pred,  measure ="auc")
perf2@y.values

# remove all variables from the environment
rm(list=ls())


#..................................XGBOOST.................................

#Loading the data
setwd("/Users/chisimdi/Downloads")
setwd(dirname(file.choose()))
library(ipumsr)
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)
df <- as.data.frame(data)
write.csv(df, "ipums_data.csv", row.names = FALSE)
income_data <- read.csv("ipums_data.csv", stringsAsFactors = FALSE)


library(ggplot2)
library(tidyr)
library(dplyr)
#---------------Data Wrangling 
#Remove catergories N/A(0), Unemployed (2), and Not in labor force (3) from EMPSTAT
#and only leave the rows in the employed category (1)
income_data<- subset(income_data, !(EMPSTAT %in% c(0, 2, 3)))
#Checking how many people are white
sum(income_data$RACWHT == 2)
#Replace RACASIAN values to 1
income_data$RACASIAN <- ifelse(income_data$RACASIAN == 2, 1, ifelse(income_data$RACASIAN == 1, 0, income_data$RACASIAN))
income_data$RACBLK<- ifelse(income_data$RACBLK == 2, 2, ifelse(income_data$RACBLK == 1, 0, income_data$RACBLK))
income_data$RACWHT<- ifelse(income_data$RACWHT == 2, 3, ifelse(income_data$RACWHT == 1, 0, income_data$RACWHT))
#Remove races that are neither black, white or asian
income_data <- income_data[!(income_data$RACWHT == 0 & income_data$RACBLK == 0 & income_data$RACASIAN == 0), ]
#Join them all into one column

#Create a new column named "race" by combining the RACWHT, RACBLK, and RACASIAN columns
income_data$RACE <- ifelse(income_data$RACWHT > 0, 1, 
                           ifelse(income_data$RACBLK > 0, 2,
                                  ifelse(income_data$RACASIAN > 0, 3, 0)))

# Remove irrelevant (repetitive variables) and the races indivually since they are now joined
income_data <- subset(income_data, select = -c(RACASIAN, RACBLK, RACWHT, EDUCD,
                                               DEGFIELDD,EMPSTAT, 
                                               EMPSTATD))

income_data <- income_data[!(income_data$INCWAGE == 0), ]

# Find the youngest age - 16
youngest_age <- min(income_data$AGE)
cat("The youngest age in the data frame is:", youngest_age)
# Find the oldest age - 95
oldest_age <- max(income_data$AGE)
cat("The oldest age in the data frame is:", oldest_age)

#Remove rows where age is not in the age bracket of the labour force 16 to 64 years old.
income_data <- income_data[income_data$AGE >= 16 & income_data$AGE <= 64, ]
income_data3 <- income_data


#Remove rows that work less than 10 hours or more than 40 hours (863343)
income_data_a<- income_data3
income_data3 <- income_data3[income_data3$UHRSWORK >= 10 & income_data3$UHRSWORK <= 40, ]
income_data_b<- income_data3

str(income_data_b)

##turn dependent variale into greater than 5000 or less than
income_data_b$INCWAGE <- ifelse(income_data_b$INCWAGE < 40000, 0, 1)
sum(income_data_b$INCWAGE == 0)
sum(income_data_b$INCWAGE == 1)

income_data_b$INCWAGE = factor(income_data_b$INCWAGE)


#------------------------------------------------ XGBoost

# randomise and make train and test data sets
set.seed(12345)
income.gb1 <- income_data_b[order(runif(863343)), ]
incomeData.tr <- income.gb1[1:690674, ]     # 80%
incomeData.te <- income.gb1[690675:863343, ]   # 20%


library(xgboost)
library(gmodels)
library(caret)

# Fit the XGBoost model
xgb_model <- xgboost(data = as.matrix(incomeData.tr[, -10]), # excluding the dependent variable
                     label = incomeData.tr$INCWAGE, 
                     nrounds = 100, 
                     max_depth = 6, 
                     eta = 0.3, 
                     objective = "binary:logistic", 
                     eval_metric = "error", 
                     verbose = 0) 

#Run model again with different hyperparameters
xgb_model
summary(xgb_model)
# Make predictions on validation data
pred <- predict(xgb_model, as.matrix(incomeData.te [, -10]))
print(length(pred))
prediction <- as.numeric(pred > 0.5)
print(head(prediction))
err <- mean(as.numeric(pred > 0.5) != incomeData.te$INCWAGE)
print(paste("test-error=", err))
importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

CrossTable(incomeData.te$INCWAGE, prediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
confusionMatrix(as.factor(prediction), as.factor(incomeData.te$INCWAGE), positive = "1")
confMat <- confusionMatrix(as.factor(prediction), as.factor(incomeData.te$INCWAGE), positive = "1")
#Calculate precision
precision <- confMat$byClass["Precision"] #Precision: 0.8066553 
cat("Precision:", precision, "\n")
# Calculate recall #Recall is 0.8370606 
recall <- confMat$byClass["Recall"]
cat("Recall:", recall, "\n")
# Calculate f1_score. f1_score: 0.8215768
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("f1_score:", f1_score, "\n")

library(pROC)
# Make predictions on validation data
pred <- predict(xgb_model, as.matrix(incomeData.te[, -10]))
# Generate ROC curve
ROC <- roc(response = incomeData.te$INCWAGE, predictor = pred)
plot(ROC, print.auc = TRUE, main = "ROC Curve")
# Print AUC score
auc <- auc(ROC)
cat("AUC score:", auc, "\n")
library(ROCR)
#ROC curve
ROCRpred <- prediction(pred, incomeData.te$INCWAGE)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)

xgb.importance(model = xgb_model)
print (importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


#----------------Support Vector Machine (SVM)------------------

#Loading the data
setwd("/Users/chisimdi/Downloads")
setwd(dirname(file.choose()))
library(ipumsr)
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)
df <- as.data.frame(data)
write.csv(df, "ipums_data.csv", row.names = FALSE)
income_data <- read.csv("ipums_data.csv", stringsAsFactors = FALSE)


library(ggplot2)
library(tidyr)
library(dplyr)
#---------------Data Wrangling 
#Remove catergories N/A(0), Unemployed (2), and Not in labor force (3) from EMPSTAT
#and only leave the rows in the employed category (1)
income_data<- subset(income_data, !(EMPSTAT %in% c(0, 2, 3)))
#Checking how many people are white
sum(income_data$RACWHT == 2)
#Replace RACASIAN values to 1
income_data$RACASIAN <- ifelse(income_data$RACASIAN == 2, 1, ifelse(income_data$RACASIAN == 1, 0, income_data$RACASIAN))
income_data$RACBLK<- ifelse(income_data$RACBLK == 2, 2, ifelse(income_data$RACBLK == 1, 0, income_data$RACBLK))
income_data$RACWHT<- ifelse(income_data$RACWHT == 2, 3, ifelse(income_data$RACWHT == 1, 0, income_data$RACWHT))
#Remove races that are neither black, white or asian
income_data <- income_data[!(income_data$RACWHT == 0 & income_data$RACBLK == 0 & income_data$RACASIAN == 0), ]
#Join them all into one column

#Create a new column named "race" by combining the RACWHT, RACBLK, and RACASIAN columns
income_data$RACE <- ifelse(income_data$RACWHT > 0, 1, 
                           ifelse(income_data$RACBLK > 0, 2,
                                  ifelse(income_data$RACASIAN > 0, 3, 0)))

# Remove irrelevant (repetitive variables) and the races indivually since they are now joined
income_data <- subset(income_data, select = -c(RACASIAN, RACBLK, RACWHT, EDUCD,
                                               DEGFIELDD,EMPSTAT, 
                                               EMPSTATD))

income_data <- income_data[!(income_data$INCWAGE == 0), ]

# Find the youngest age - 16
youngest_age <- min(income_data$AGE)
cat("The youngest age in the data frame is:", youngest_age)
# Find the oldest age - 95
oldest_age <- max(income_data$AGE)
cat("The oldest age in the data frame is:", oldest_age)

#Remove rows where age is not in the age bracket of the labour force 16 to 64 years old.
income_data <- income_data[income_data$AGE >= 16 & income_data$AGE <= 64, ]
income_data3 <- income_data


#Remove rows that work less than 10 hours or more than 40 hours (863343)
income_data_a<- income_data3
income_data3 <- income_data3[income_data3$UHRSWORK >= 10 & income_data3$UHRSWORK <= 40, ]
income_data_b<- income_data3

str(income_data_b)

##turn dependent variale into greater than 5000 or less than
income_data_b$INCWAGE <- ifelse(income_data_b$INCWAGE < 40000, 0, 1)
sum(income_data_b$INCWAGE == 0)
sum(income_data_b$INCWAGE == 1)

#......................Sampling..........................

library(dplyr)
# set the seed for reproducibility
set.seed(123)
# randomly select 10% of rows from income_data_transformeder
sampled.rf <- income_data_b %>% 
  sample_frac(0.1)
# view the first few rows of the new dataframe
str(sampled.rf )
sampled.rf$INCWAGE = factor(sampled.rf$INCWAGE)
set.seed(12345)
income.gb1 <- sampled.rf[order(runif(86334)), ]
incomeData.tr <- income.gb1[1:69067, ]     # 80%
incomeData.te <- income.gb1[69068:86334, ]   # 20%


# run support vector machine algorithms
library(e1071)

# Train the SVM model
svm_model <- svm(incomeData.tr$INCWAGE ~ ., data = incomeData.tr, kernel = "linear", cost = 10, scale = TRUE)
svm_model <- svm(incomeData.tr$INCWAGE ~ ., data = as.matrix(incomeData.tr), kernel = "linear", cost = 10, scale = TRUE)

svm_model
summary(svm_model)
# Make predictions on the test set
predictions <- predict(svm_model, incomeData.te)
plot(svm_model)

# evaluate
# obtain model results
incomeData.res <- predict(svm_model, incomeData.te[-10])  # independent variables only
# evaluate the accuracy of the model
library(caret)
conf_mat <- confusionMatrix(incomeData.res, incomeData.te$INCWAGE)
conf_mat
accuracy <- conf_mat$overall["Accuracy"]
accuracy


library(gmodels)

CrossTable(incomeData.te$INCWAGE, predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual INCWAGE', 'Predicted INCWAGE'))
confusionMatrix(boost_pred, test$INCWAGE, positive = "1")
conf_mat <-confusionMatrix(predictions, incomeData.te$INCWAGE, positive = "1")
conf_mat

# Extract the recall, precision, and F1 score
recall <- conf_mat$byClass["Recall"]
precision <- conf_mat$byClass["Precision"]
f1_score <- conf_mat$byClass["F1"]

# Print the results
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 score:", f1_score, "\n")


# Create ROC curve
library(pROC)
#Error in roc.default(incomeData.te$INCWAGE, predictions) : 
#Predictor must be numeric or ordered.
roc_data <- roc(incomeData.te$INCWAGE, predictions)
auc_val <- auc(roc_data)
# Plot ROC curve and display AUC
plot(roc_data, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", paste("AUC = ", round(auc_val, 3)), bty = "n")

# Extract the recall, precision, and F1 score
recall <- conf_mat$byClass["Recall"]
precision <- conf_mat$byClass["Precision"]
f1_score <- conf_mat$byClass["F1"]

# Print the results
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 score:", f1_score, "\n")

# create a dataframe with actual and predicted values
dt_results <- data.frame(actual = incomeData.te$INCWAGE, predicted = predict(svm_model, incomeData.te))
# calculate accuracy
accuracy <- sum(dt_results$actual == dt_results$predicted)/nrow(dt_results)
# visualise accuracy
ggplot(dt_results, aes(x = actual, fill = predicted)) + 
  geom_bar() +
  labs(title = paste("SVM Model: ", round(accuracy * 100, 2), "%"))


# Get variable importance
var_imp <- varImp(svm_model, scale = FALSE)
var_imp

# Predict INCWAGE for men
men_data <- incomeData.te[incomeData.te$SEX == 1,] # select only men data

men_pred <- predict(svm_model, newdata = men_data, type = "raw")
mean(men_data$INCWAGE[men_pred == 1]) # mean INCWAGE for men who are predicted to have higher INCWAGE

# Predict INCWAGE for women
women_data <- test[test$SEX == 2,] # select only women data
women_pred <- predict(dt_model_boost10, newdata = women_data, type = "raw")
mean(women_data$INCWAGE[women_pred == 1]) # mean INCWAGE for women who are predicted to have higher INCWAGE

str(incomeData.tr)

# remove all variables from the environment
rm(list=ls())

