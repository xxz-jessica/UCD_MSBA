##############################################
########### Solved Example 1 #################
##############################################

SolvedEx1 <- read.csv("SolvedExample1.csv")

#### Correlation Coefficient and Scatterplot ####

plot(SolvedEx1$Floor, SolvedEx1$Price, main = "Price versus Floor", xlab = "Floor", ylab = "Price")
abline(lm(SolvedEx1$Price ~ SolvedEx1$Floor))

cor(SolvedEx1$Floor, SolvedEx1$Price)

### If you want to find whether there is a correlation between Price and Floor,
### use a two-tailed test.
### H0: rho = 0
### H1: rho =/= 0

cor.test(SolvedEx1$Floor, SolvedEx1$Price, alternative = "two.sided", method = "pearson")

### If you want to find whether there is a positive correlation between Price and Floor,
### use a right-tailed test.
### H0: rho <= 0
### H1: rho > 0

cor.test(SolvedEx1$Floor, SolvedEx1$Price, alternative = "greater", method = "pearson")

#### Regression Model ####

Price <- SolvedEx1$Price
Floor <- SolvedEx1$Floor

lm.FL <- lm(Price ~ Floor)

## Display the ANOVA table containing the SS information
anova(lm.FL)

## Display the Coefficients information
summary(lm.FL)

#### Prediction and Confidence Intervals ####
predict(lm.FL, data.frame(Floor = 20), interval = "prediction", level = 0.95)

predict(lm.FL, data.frame(Floor = 15), interval = "confidence", level = 0.99)

##############################################
########### Solved Example 2 #################
##############################################

SolvedEx2 <- read.csv("SolvedExample2.csv")

#### Correlation Coefficient and Scatterplot ####

plot(SolvedEx2$Price, SolvedEx2$Sales, main = "Price versus Sales", xlab = "Price", ylab = "Sales")
abline(lm(SolvedEx2$Sales ~ SolvedEx2$Price))
scatter.smooth(x=SolvedEx2$Price, y=SolvedEx2$Sales, main="Sales ~ Price")  # scatterplot

cor(SolvedEx2$Price, SolvedEx2$Sales)

### If you want to find whether there is a correlation between Price and Sales,
### use a two-tailed test.
### H0: rho = 0 -- no correlation in the population
### H1: rho =/= 0 -- there is a significant correlation in the population

cor.test(SolvedEx2$Price, SolvedEx2$Sales, alternative = "two.sided", method = "pearson")

### If you want to find whether there is a negative correlation between Price and Sales,
### use a left-tailed test.
### H0: rho >= 0
### H1: rho < 0

cor.test(SolvedEx2$Price, SolvedEx2$Sales, alternative = "less", method = "pearson")

#### If you try to look at the Pearson correlation coefficient, then you will see that
#### the correlation coefficient shows a very strong linear relationship and the 
#### P-value indicates that the correlation is statistically significant! But this is misleading.
#### This is exactly why you should plot the scatterplot AND look at the coefficient of correlation 
#### to determine whether or not there is a linear relationship.
#### The scatterplot shows signs of nonlinear relationship but the correlation coefficient is high.
### Tukey's Bulging Rule -- it is a suggestive rule to identify the TYPE of transformation to be used.
### These transformations can be used for correcting linearity problems, constant variance problem, independence problem,
### or normality problems.
#### We will transform the response variable. LOG10(Sales)
Log_Sales <- log10(SolvedEx2$Sales)

#### Now plot the scatterplot between Log_Sales and Price
#### to see whether the relationship becomes linear or not.
plot(SolvedEx2$Price, Log_Sales, main = "Price versus Log_Sales", xlab = "Price", ylab = "Log_Sales")
abline(lm(Log_Sales ~ SolvedEx2$Price))

cor(SolvedEx2$Price, Log_Sales) # Nearly perfect negative correlation.

### If you want to find whether there is a correlation between Price and Log_Sales,
### use a two-tailed test.
### H0: rho = 0
### H1: rho =/= 0

cor.test(SolvedEx2$Price, Log_Sales, alternative = "two.sided", method = "pearson")

### If you want to find whether there is a negative correlation between Price and Log_Sales,
### use a left-tailed test.
### H0: rho >= 0
### H1: rho < 0

cor.test(SolvedEx2$Price, Log_Sales, alternative = "less", method = "pearson")

#### Regression Model ####
Price <- SolvedEx2$Price

lm.DrinkSales <- lm(Log_Sales ~ Price)

anova(lm.DrinkSales)

summary(lm.DrinkSales)

#### Log(Sales) = 2.684 - 0.8737*Price

#### If we decide to sell a two-liter bottle for $1.25, then the predicted 
#### Log Sales is 2.685 - 0.8738*1.25 = 1.593.
#### So, Log(Sales) = 1.593
#### Sales = Antilog(1.593)
#### Sales = 101.593 = 39.174
#### Sales = 39 bottles

#### If we increase the price to $2.00, then the predicted 
#### Log Sales is 2.685 - 0.8738*2.00 = 0.9374.
#### So, Log(Sales) = 0.9374
#### Sales = Antilog(0.9374)
#### Sales = 100.9374 = 8.658
#### Sales = 9 bottles

##############################################
########### Solved Example 3 #################
##############################################

SolvedEx3 <- read.csv("SolvedExample3.csv")

#### Correlation Coefficient and Scatterplot ####

plot(SolvedEx3$RmMinusRf, SolvedEx3$RMinusRf, main = "Stock Returns versus Market Returns", xlab = "Rm Minus Rf", ylab = "R Minus Rf")
abline(lm(SolvedEx3$RMinusRf ~ SolvedEx3$RmMinusRf))

cor(SolvedEx3$RmMinusRf, SolvedEx3$RMinusRf)

### If you want to find whether there is a correlation between Risk-adjusted Asset Returns and Risk-Adjusted Market Returns,
### use a two-tailed test.
### H0: rho = 0
### H1: rho =/= 0

cor.test(SolvedEx3$RmMinusRf, SolvedEx3$RMinusRf, alternative = "two.sided", method = "pearson")

#### Regression Model ####

R_Minus_Rf <- SolvedEx3$RMinusRf
Rm_Minus_Rf <- SolvedEx3$RmMinusRf

lm.CAPM <- lm(R_Minus_Rf ~ Rm_Minus_Rf)

anova(lm.CAPM)

summary(lm.CAPM)

#### The estimate for the beta coefficient is 0.5844 and its standard error is 0.0803. 
#### In order to determine whether the beta coefficient is significantly less than *one*, 
#### we formulate the hypotheses as H0: beta >= 1 and H1: beta < 1.
#### This is a non-standard case, so we cannot infer based on the T-Values and P-Values from R.

#### First, we need to find the t_critical value. We will use qt function in R.
#### Note the degrees of freedom (df) is n - k - 1 where k = number of predictors, which in this case is 1.

t_critical <- qt(0.05, 60-1-1, lower.tail = TRUE)
t_critical

#### Next, we need to find the t_test_statistic value.
#### t = (b - beta)/std. error

t_test_stat <- (0.58438 - 1)/0.08032
t_test_stat

#### Decision.
#### Since the t_test_stat is less than the t_critical, for a left-tailed test we reject H0: beta >= 1.
#### We infer at 5% significance level that beta < 1 and is statistically significant.
#### Return on J&J stock is less risky than the returns on the market.

#### To test whether abnormal returns exist, we test whether the intercept, alpha is significantly different from 0.
#### This is a standard case, where the hypothesized value is different from ZERO. So we can use the p-value reported 
#### by R.
#### Hypotheses are H0: alpha = 0 meaning no abnormal returns, H1: alpha =/= 0 meaning there are abnormal returns.
#### The p-value for the intercept is 0.513 which is > any reasonable significance level.
#### So, at the 5% significance level, we can infer that alpha = 0 and J&J has no abnormal returns.

###########################
#### SOLVED EXAMPLE 4 #####
###########################

### Develop a model on theoretical basis
### Identify the dependent (response) and independent (predictor/explanatory) variables.

### Weeks of SP = f(Age, Years, AnnualPay)
### Weeks_SP = beta0 + beta1*Age + beta2*Years + beta3*AnnualPay + error

### Read the data
Pay <- read.csv("SolvedExample4.csv")

### Observe whether there is a linear relationship between each explanatory (independent)
### variable and the response (dependent) variable. Look for bending patterns.
### Bending patterns between explanatory variables and the response variable would suggest transformations.
### Plot a scatterplot matrix
plot(Pay) 
### OR
pairs(Pay[1:4], panel = panel.smooth)

### Correlation matrix
### Using correlation matrix, we can assess the strength and direction of correlation.
### The package Hmisc includes rcorr() function that displays r and the p-values.
cor(Pay)
### OR
### Use the rcorr function in the Hmisc package. This displays the r and p-values
Hmisc::rcorr(as.matrix(Pay))

### Observations from the Corrlation Matrix:
### Strong Positive correlation between WeeksSP and Age, WeeksSP and Years
### Weak Positive correlation between WeeksSP and Pay
### p-values matrix shows that the correlation between WeeksSP and Age is statistically significant
### WeeksSP and Years is statistically significant.
### WeeksSP and Pay is not statistically significant.

### Develop the multiple regression model.
Pay_Model <- lm(WeeksSP ~ Age+Years+Pay, data = Pay)
anova(Pay_Model)
summary(Pay_Model)

### Testing the overall model validity
### H0: beta1 = beta2 = beta3 = 0 (There is no linear relationship between the dependent variable and the independent variables.)
### H1: At least one beta(i) is not equal to zero. (There is a linear relationship between the dependent variable and at least one of the independent variables.)

### p-value for the F-test: 3.758e-12. The overall model is valid.

### This test measures the collective effect of all explanatory variables on the response variable.
### Since the p-value is approx. 0, we infer that the Age, Years, and Pay are jointly significant
### in explaining the variation in Weeks of Severance Pay.

### R-sq = 0.702. 68.25% of variation in Weeks SP is explained by variation in
### Age, Years, and Pay. The remaining 31.75% of variation is unexplained. 

### Adj. R-sq = 0.6825. 

### Interpreting the coefficients.
### Intercept. The intercept is 6.06. This is the average Weeks of SP when Age, Years, and Pay are zero.
### This is meaningless.

### Age. The relationship between Weeks SP and Age is described by b1 = -0.0078. 
### For each additional year of age, weeks of SP decreases on average by 0.0078 weeks
### assuming other independent variables in the model are held constant.

### Years. The relationship between Weeks SP and No. of Years with the Company is described by b2 = 0.603
### For each additional year with the company, the no. of weeks of severance pay increases by 0.603
### assuming other independent variables in the model are held constant.

### Pay. The relationship between Weeks SP and Annual Pay is described by b3 = -0.0702.
### For every one thousand dollars increase in annual pay, the no. of weeks of severance package decreases
### by 0.07 weeks assuming other independent variables in the model are held constant.

### Testing the coefficients (t-test for individual coefficients).
### Null Hypothesis: beta(i) = 0
### Alternative hyppothesis: beta(i) =/= 0
### t = (b(i) - beta(i))/se(b(i))

### Test of Coefficient of Age
### p-value = 0.9069. Not statistically significant.
### Not enough evidence to conclude that Age is linearly related to the No. of Weeks of SP.

### Test of Coefficient of Years
### p-value approx. 0. Highly statistically significant.
### Overwhelming evidence that No. of Years with the Company is linearly related to the No. of Weeks of SP.

### Test of Coefficient of Annual Pay
### p-value = 0.1864. Not statistically significant.
### Not enough evidence to conclude that Annual Pay is linearly related to the No. of Weeks of SP.

### Only No. of Years with the company is linearly related to the severance pay.

### Prediction Interval for Bill Smith. Age = 32 years, No. of years with the company = 10, Pay = $32,000
predict(Pay_Model, data.frame(Age = 36, Years = 10, Pay = 32), interval = "prediction", level = 0.95)
### Predict with 95% confidence 9.57 weeks of pay (point estimate)
### LCL = 5.63 weeks of pay 
### UCL = 13.50 weeks of pay.
### The offer of 5 weeks severance pay falls below the prediction interval and thus Bill is correct.

###########################
#### SOLVED EXAMPLE 5 #####
###########################

CarWash <- read.csv("SolvedExample5.csv")

### FULL MODEL: Contains ALL the independent variables.
### In this case, we have three - Price Discount, Radio Exp, and Newspaper Exp.
fullmodel <- lm(Sales_1000s ~ ., data = CarWash)

### REDUCED MODEL: Removes the independent variables to be tested.
### In this case, we have two independent variables to be tested 
### Radio Exp, and Newspaper Exp.
reducedmodel <- lm(Sales_1000s ~ PriceDiscount_Percent, data = CarWash)

summary(fullmodel)
summary(reducedmodel)

### The lm function does not provide us the SSEs for the full and reduced models.
### We run the anova function in R to get these.
anova(reducedmodel, fullmodel) ### Reduced model before full model

### OR you could display the SSE for the two models separately and calculate the F-statistic manually.

anova(reducedmodel)
anova(fullmodel)

### Conclusion and Interpretation
### The p-value from the ANOVA output leads to the rejection of H0
### H0: The reduced model and the full model do not differ significantly, so choose the reduced model.
### The reduced model and the full model do not differ significantly, so choose the reduced model.
### H1: The full model is significantly better.
### We conclude that the full model is better, so we infer at any reasonable significance level
### that the advertising expenditures on radio and newspapers have 
### a statistically significant influence on sales and should be included in the analysis.

### Partial R-squared
### SSE(reduced) = 2182.6
### SSE (full) = 1208.1
partial_r_sq <- (2182.6 - 1208.1)/2182.6
## The Radio expenditures and newspaper expenditures variables explain 44.64% of the variation in Sales that cannot be
## explained by PriceDiscount alone.
## OR
## The additional independent variables - radio and newspaper expenditures in the complete model explain 44.64% of 
## the unexplained variation in the reduced model.
