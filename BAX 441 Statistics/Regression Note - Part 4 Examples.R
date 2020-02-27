######################
### Solved Example 1
######################
WageDataset <- read.csv("SolvedExample1.csv")

Wage <- WageDataset$Wage
Educ <- WageDataset$Educ
Exper <- WageDataset$Exper

### Step 1: First, run the original Model
### Wage = beta1 + beta2EDUC + beta3EXPER + error
model_1 <- lm(Wage ~ Educ + Exper)
summary(model_1)

### Step 2: Obtain the residuals, square them, and take their logs
resids <- residuals(model_1)
resids_sq <- resids^2
ln_resids_sq <- log(resids_sq)

### Step 3: Run the regression against the X variables LN(EDUC)
model_2 <- lm(ln_resids_sq ~ log(Educ))
summary(model_2)

### The model is ln(Resids_sq) = -2.208 + 1.4673LN(EDUC)
### The p-value for Beta2 is 0.000338 < 0.05 (significant)
### Evidence of heteroscedasticity

### Step 3 (Optional): Run the regression against Predicted Values of Y
predicted_wages <- predict(model_2)
model_2a <- lm(ln_resids_sq ~ predicted_wages)
summary(model_2a)
### The p-value for Beta2 is again 0.000338 < 0.05 (significant)
### Evidence of heteroscedasticity

######################
### Solved Example 2
######################
WageDataset <- read.csv("SolvedExample1.csv")

Wage <- WageDataset$Wage
Educ <- WageDataset$Educ
Exper <- WageDataset$Exper

### Step 1: First, run the original Model
### Wage = beta1 + beta2EDUC + beta3EXPER + error
model_1 <- lm(Wage ~ Educ + Exper)
summary(model_1)

### Step 2: Obtain the residuals and store absolute values of the residuals
resids <- residuals(model_1)
abs_resids <- abs(resids)

### Step 3: Run Glejser test
### Test 1: abs(ei) = beta1 + beta2*Educ
### Test 2: abs(ei) = beta1 + beta2*sqrt(Educ)
### Test 3: abs(ei) = beta1 + beta2*(1/Educ)
### Test 4: abs(ei) = beta1 + beta2*(1/sqrt(Educ))

### Test 1: abs(ei) = beta1 + beta2*Educ
model_test_1 <- lm(abs_resids ~ Educ)
summary(model_test_1)
### Beta2 is statistically significant --> signs of heteroscedasticity

### Test 2: abs(ei) = beta1 + beta2*sqrt(Educ)
model_test_2 <- lm(abs_resids ~ sqrt(Educ))
summary(model_test_2)
### Beta2 is statistically significant --> signs of heteroscedasticity

### Test 3: abs(ei) = beta1 + beta2*(1/Educ)
recip_Educ <- 1/Educ
model_test_3 <- lm(abs_resids ~ recip_Educ)
summary(model_test_3)
### Beta2 is statistically significant --> signs of heteroscedasticity

### Test 4: abs(ei) = beta1 + beta2*(1/sqrt(Educ))
recip_sqrt_Educ <- 1/sqrt(Educ)
model_test_4 <- lm(abs_resids ~ recip_sqrt_Educ)
summary(model_test_4)
### Beta2 is statistically significant --> signs of heteroscedasticity

######################
### Solved Example 3
######################
MutualFunds <- read.csv("SolvedExample3.csv")

Return <- MutualFunds$AverageAnnualReturn
SD <- MutualFunds$StdDevRet

model_3 <- lm(Return ~ SD)
summary(model_3)

resids <- residuals(model_3)
abs_resids <- abs(resids)

cor.test(x = SD, y = abs_resids, alternative = "greater",method = "spearman", exact = FALSE)

### p-value = 0.173 > alpha, fail to reject H0. No signs of heteroscedasticity.

######################
### Solved Example 4
######################

WageDataset <- read.csv("SolvedExample1.csv")

Wage <- WageDataset$Wage
Educ <- WageDataset$Educ
Exper <- WageDataset$Exper

### Step 1: First, run the original Model
### Wage = beta1 + beta2EDUC + beta3EXPER + error
model_1 <- lm(Wage ~ Educ + Exper)
summary(model_1)
resids_sq <- (residuals(model_1))^2

### Step 2: Run the auxiliary regression
model_11 <- lm(resids_sq ~ Educ + Exper + I(Educ^2) + I(Exper^2) + I(Educ*Exper))
summary(model_11)

### Step 3: calculate the chi-squared value
chi_sq_val <- nrow(WageDataset)*summary(model_11)$r.squared

### Step 4: Find the critical chi-square value
qchisq(0.95, 5) # df = 5 because we have 5 regressors in the auxiliary regression 

### The chi-square critical value = 11.0705
### The chi-square test statistic = 11.2310
### Decision: Reject H0. Evidence of heteroscedasticity.

### Alternatively, find the p-value
pchisq(chi_sq_val, 5, lower.tail = FALSE)

### The p-value = 0.04699 < 0.05. Reject H0. Evidence of heteroscedasticity.

### Alternate command for running the White heteroscedasticity test...
## lmtest::bptest(model_1, ~ Educ*Exper +I(Educ^2) + I(Exper^2))

######################
### Solved Example 5
######################

WageDataset <- read.csv("SolvedExample1.csv")

Wage <- WageDataset$Wage
Educ <- WageDataset$Educ
Exper <- WageDataset$Exper

### Step 1: First, run the original Model
### Wage = beta1 + beta2EDUC + beta3EXPER + error
model_5 <- lm(Wage ~ Educ + Exper)
summary(model_5)
resids_sq <- (residuals(model_5))^2

### Obtain the predicted values of Wages and square them
pred_sq <- predict(model_5)^2

### Step 2: Run the auxiliary regression
### Resid_sq = alpha1 + alpha2*PredictedY_sq + vi
model_51 <- lm(resids_sq ~ pred_sq)
summary(model_51)

### p-value of alpha2 = 0.00513 < significance level. Reject H0
### There is sign of heteroscedasticity.

######################
### Solved Example 6
######################

WageDataset <- read.csv("SolvedExample1.csv")

Wage <- WageDataset$Wage
Educ <- WageDataset$Educ
Exper <- WageDataset$Exper

sqrt_educ <- sqrt(Educ)

wage_trans <- Wage/sqrt_educ
recip_sqrt_educ <- 1/sqrt_educ
exp_trans <- Exper/sqrt_educ

### Step 1: First, run the original Model
### Wage = beta1 + beta2EDUC + beta3EXPER + error
model_6 <- lm(wage_trans ~ 0 + recip_sqrt_educ + sqrt_educ + exp_trans)
summary(model_6)

resids_sq <- (residuals(model_6))^2

### Obtain the predicted values and square them
pred_sq <- predict(model_6)^2

### Step 2: Run the auxiliary regression
### Resid_sq = alpha1 + alpha2*PredictedY_sq + vi
model_61 <- lm(resids_sq ~ pred_sq)
summary(model_61)

### p-value of alpha2 = 0.206 > significance level. Fail to Reject H0
### There is no sign of heteroscedasticity in the transformed residuals.

### Assessing heteroscedasticity using White's test
model_w <- lm(resids_sq ~ recip_sqrt_educ + sqrt_educ + exp_trans)
summary(model_w)

### Chi-sq test statistic
(chi_sq_value <- nrow(WageDataset)*summary(model_w)$r.squared)

### Chi-sq critical value
qchisq(0.95, 3) # df = 5 because we have 3 regressors in the auxiliary regression 

######################
### Solved Example 7
######################

WageDataset <- read.csv("SolvedExample1.csv")

Wage <- WageDataset$Wage
Educ <- WageDataset$Educ
Exper <- WageDataset$Exper

Y_trans <- Wage/Educ
recip_educ <- 1/Educ
exper_trans <- Exper/Educ

model_7 <- lm(Y_trans ~ recip_educ + 1 + exper_trans)
summary(model_7)

### Check if there is evidence of heteroscedasticity
resids_sq <- (residuals(model_7))^2

### Obtain the predicted values and square them
pred_sq <- predict(model_7)^2

### Step 2: Run the auxiliary regression
### Resid_sq = alpha1 + alpha2*PredictedY_sq + vi
model_71 <- lm(resids_sq ~ pred_sq)
summary(model_71)

### p-value of alpha2 = 0.0954 > 0.05. Fail to Reject H0
### There is no sign of heteroscedasticity in the transformed residuals.

######################
### Solved Example 8
######################

WageDataset <- read.csv("SolvedExample1.csv")

Wage <- WageDataset$Wage
Educ <- WageDataset$Educ
Exper <- WageDataset$Exper

log_Wage <- log(Wage)
log_Educ <- log(Educ)
log_Exper <- log(Exper)

model_8 <- lm(log_Wage ~ log_Educ + log_Exper)
summary(model_8)

### Check if there is evidence of heteroscedasticity
resids_sq <- (residuals(model_8))^2

### Obtain the predicted values and square them
pred_sq <- predict(model_8)^2

### Step 2: Run the auxiliary regression
### Resid_sq = alpha1 + alpha2*PredictedY_sq + vi
model_81 <- lm(resids_sq ~ pred_sq)
summary(model_81)

### p-value of alpha2 = 0.90032 > 0.05. Fail to Reject H0
### There is no sign of heteroscedasticity in the transformed residuals.

######################
### Solved Example 9
######################

SolvedExample9 <- read.csv("SolvedExample9.csv")

Productivity <- SolvedExample9$Productivity
RealWages <- SolvedExample9$Compensation

model_9 <- lm(RealWages ~ Productivity)
summary(model_9)

### Check for autocorrelation

### Graphical - Residuals versus Time
resid_9 <- residuals(model_9)
timeperiods <- 0:47
plot(x=timeperiods, type="b", y=resid_9, pch=19, xlab = "Time", ylab = "Residuals", main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals don't seem to be randomly distributed.
### Shows signs of autocorrelation

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid_9, lags = 1, do.lines = FALSE, diag = FALSE, main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows evidence of positive autocorrelation.

### Durbin-Watson d Statistic
### Calculating using principles
ts_resid_9 <- ts(resid_9)
ts_resid_9_lag_1 <- lag(ts_resid_9, -1)
D <- ts_resid_9 - ts_resid_9_lag_1
D_sq <- D^2
e_sq <- resid_9^2
dw <- sum(D_sq)/sum(e_sq)
dw

### Using the command
lmtest::dwtest(model_9)

### The p-value is very small, indicating there is an evidence of
### positive first-order autocorrelation.

### Detect using Runs Test
randtests::runs.test(resid_9, alternative = "two.sided")

### p-value is very small indicating evidence of nonrandomness

### Remedy

### The First Difference Method
RealWages_First_Diff <- diff(RealWages, lag = 1, differences = 1)
Productivity_First_Diff <- diff(Productivity, lag = 1, differences = 1)

### Run the regression-through-the-origin
model_9_1 <- lm(RealWages_First_Diff ~ 0 + Productivity_First_Diff)
summary(model_9_1)

### Verify if this technique remedied autocorrelation

### Graphical - Residuals versus Time
resid_model_9_1 <- residuals(model_9_1)
timeperiods <- 0:(NROW(resid_model_9_1)-1)
plot(x=timeperiods, type="b", y=resid_model_9_1, pch=19, xlab = "Time", ylab = "Residuals", main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals don't seem to be randomly distributed.
### Shows no signs of autocorrelation.

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid_model_9_1, lags = 1, do.lines = FALSE, diag = FALSE, main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows less evidence of positive autocorrelation.

### Find DW Statistic
lmtest::dwtest(model_9_1, alternative = "greater")

### The p-value is 0.03692, indicating there is an evidence of
### positive first-order autocorrelation at 0.05 significance level.

### Detect using Runs Test
randtests::runs.test(resid_model_9_1, alternative = "two.sided")

### p-value is very small indicating evidence of nonrandomness

### Estimating rho from OLS residuals
lagged_resids <- ts_resid_9[1:length(ts_resid_9)-1]
resids_start_2nd_row <- ts_resid_9[-1]

model_9_1_est_rho <- lm(resids_start_2nd_row ~ 0 + lagged_resids)
summary(model_9_1_est_rho)

### Use rho-hat = 0.8915

### Creating the differencing terms
rho_hat <- 0.8915

RealWages_adj <- RealWages*rho_hat
Productivity_adj <- Productivity*rho_hat
Y_t_star <- RealWages[-1] - RealWages_adj[1:length(RealWages_adj)-1]
X_t_star <- Productivity[-1] - Productivity_adj[1:length(Productivity_adj)-1]

model_9_2 <- lm(Y_t_star ~ X_t_star)
summary(model_9_2)

### Verify if this technique remedied autocorrelation

### Graphical - Residuals versus Time
resid_model_9_2 <- residuals(model_9_2)
timeperiods <- 0:(NROW(resid_model_9_2)-1)
plot(x=timeperiods, type="b", y=resid_model_9_2, pch=19, xlab = "Time", ylab = "Residuals", main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals seems to be randomly distributed.
### Shows no signs of autocorrelation.

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid_model_9_2, lags = 1, do.lines = FALSE, diag = FALSE, main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows less evidence of positive autocorrelation.

### Find DW Statistic
lmtest::dwtest(model_9_2, alternative = "greater")

### The p-value is 0.05819, indicating the remdying of
### positive first-order autocorrelation at 0.05 significance level.

### Detect using Runs Test
randtests::runs.test(resid_model_9_2, alternative = "two.sided")

### p-value is very small indicating evidence of nonrandomness

### Prais-Winsten Transformation
Y_1_star_PW <- sqrt(1-rho_hat^2)*head(RealWages[1])
X_1_star_PW <- sqrt(1-rho_hat^2)*head(Productivity[1])

### Create a data frame containing adjusted values
df <- data.frame(c1 = Y_t_star, c2 = X_t_star)
df <- rbind(c(Y_1_star_PW, X_1_star_PW), df)
colnames(df) <- c("RealWages_Adj", "Productivity_Adj")

### Now consider this new dataframe of adjusted values
model_9_3 <- lm(df$RealWages_Adj ~ df$Productivity_Adj)
summary(model_9_3)

### Verify if this technique remedied autocorrelation

### Graphical - Residuals versus Time
resid_model_9_3 <- residuals(model_9_3)
timeperiods <- 0:(NROW(resid_model_9_3)-1)
plot(x=timeperiods, type="b", y=resid_model_9_3, pch=19, xlab = "Time", ylab = "Residuals", main = "Time-Sequence Plot")
abline(h=0)

### The plot shows that the residuals seems to be randomly distributed.
### Shows no signs of autocorrelation. May be - hard to see.

### Graphical - Residuals(t) versus Residuals(t-1)
lag.plot(resid_model_9_3, lags = 1, do.lines = FALSE, diag = FALSE, main = "Residuals versus Lag 1 Residuals")
abline(h=0, v=0)

### The plot shows some evidence of positive autocorrelation.

### Find DW Statistic
lmtest::dwtest(model_9_3, alternative = "greater")

### The p-value is 0, indicating the presence of
### positive first-order autocorrelation at 0.05 significance level.

### Detect using Runs Test
randtests::runs.test(resid_model_9_3, alternative = "two.sided")

### p-value is small indicating some evidence of nonrandomness