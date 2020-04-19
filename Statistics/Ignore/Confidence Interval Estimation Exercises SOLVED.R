######################################
#### Exercise 1 - Trash Bag Case #####
######################################

TrashBag <- read.csv("TrashBag.csv")

## To find the confidence interval estimate for mean, we need
## 1. The point Estimate (Sample mean)
## 2. The standard error of estimate
## 3. The z critical value corresponding to the Confidence Level (CL)

### Sample mean
x_bar <- mean(TrashBag$Strength)

### Sample Size
n <- nrow(TrashBag)

### Population Std. Dev.
sigma <- 1.65

### z-critical for 90%, 95%, and 99% CLs

### For 90% confidence... 
z90 <- qnorm(0.95, mean = 0, sd = 1, lower.tail = TRUE)

### For 95% confidence... 
z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

### For 99% confidence... 
z99 <- qnorm(0.995, mean = 0, sd = 1, lower.tail = TRUE)

### 90% Confidence Interval
l90 <- x_bar - (z90*sigma/sqrt(n))
r90 <- x_bar + (z90*sigma/sqrt(n))

l90_rounded <- round(l90, digits = 3)
r90_rounded <- round(r90, digits = 3)

### 95% Confidence Interval
l95 <- x_bar - (z95*sigma/sqrt(n))
r95 <- x_bar + (z95*sigma/sqrt(n))

l95_rounded <- round(l95, digits = 3)
r95_rounded <- round(r95, digits = 3)

### 99% Confidence Interval
l99 <- x_bar - (z99*sigma/sqrt(n))
r99 <- x_bar + (z99*sigma/sqrt(n))

l99_rounded <- round(l99, digits = 3)
r99_rounded <- round(r99, digits = 3)

### Display 90% CI
sprintf("90 percent Confidence Interval: (%s, %s)", l90_rounded, r90_rounded)
sprintf("INTERPRETATION: We might be 90 percent confident that the interval between %s lbs and %s lbs contains the true population mean strength of all bags.", l90_rounded, r90_rounded)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)
sprintf("INTERPRETATION: We might be 95 percent confident that the interval between %s lbs and %s lbs contains the true population mean strength of all bags.", l95_rounded, r95_rounded)

### Display 99% CI
sprintf("99 percent Confidence Interval: (%s, %s)", l99_rounded, r99_rounded)
sprintf("INTERPRETATION: We might be 99 percent confident that the interval between %s lbs and %s lbs contains the true population mean strength of all bags.", l99_rounded, r99_rounded)

### So what does this all mean for decision-making? PART e
### We are 90% confident that the mean strength is between 50.146 lbs and 51.004 lbs.
### We are 95% confident that the mean strength is between 50.064 lbs and 51.086 lbs.
### We are 99% confident that the mean strength is between 49.903 lbs and 51.247 lbs.

### The lower limit of the CI (LCL) indicates what the population mean could be (in the worst case) - because the lower the LCL, the lower the strength - the worse. 
### The upper limit of the CI (UCL) indicates what the population mean could be (in the best case) - because the greater the UCL, the higher the strength - the better.

### In this case, in the worst case, both 90% and 95% LCLs are greater than 50 lbs. So, we can be 90% and 95% confident that the mean breaking strength exceeds 50 lbs and the bags are stronger.
### Whereas in the worst case, the 99% LCL is less than 50 lbs. So, the population mean breaking strength might be as low as 49.903 lbs. So, we cannot be 99% confident in our claim that the new bags are stronger.

######################################
#### Exercise 2 - Drug Cost ##########
######################################

DrugCost <- read.csv("DrugCost.csv")

## To find the confidence interval estimate for mean, we need
## 1. The point Estimate (Sample mean)
## 2. The standard error of estimate
## 3. The z critical value corresponding to the Confidence Level (CL)

### Sample mean
x_bar <- mean(DrugCost$Costs)

### Sample Size
n <- nrow(DrugCost)

### Sample Std. Dev.
s <- sd(DrugCost$Costs)

### t-critical for 95% CL

### For 95% confidence... 
t95 <- qt(0.975, df = n - 1, lower.tail = TRUE)

### 95% Confidence Interval
l95 <- x_bar - (t95*s/sqrt(n))
r95 <- x_bar + (t95*s/sqrt(n))

l95_rounded <- round(l95, digits = 3)
r95_rounded <- round(r95, digits = 3)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)

##### Interpretation
### We are 95% condfident that the interval between $43 and $44.85 contains the 
### true population mean cost of generic drugs.
sprintf("INTERPRETATION: We are 95 percent confident that the interval between $ %.2f lbs and $ %.2f  contains the true population mean cost of all generic drugs.", l95_rounded, r95_rounded)

#######################################
#### Exercise 3 - Toothpaste ##########
#######################################

ToothpastePref <- read.csv("Toothpaste.csv")

### Get the total number of records
n <- nrow(ToothpastePref)

### Count the number of people who prefer Crest
num_Crest <- sum(ToothpastePref == 'Crest')

### Sample Proportion: Proportion of people preferring Crest
p_hat <- num_Crest/n

### Sample Proportion of people NOT preferring Crest
1 - p_hat

### Checking the condition for large sample size...
n*p_hat >= 5
n*(1 - p_hat) >= 5

### For 95% confidence... 
z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

### 95% Confidence Interval
l95 <- p_hat - (z95 * sqrt(p_hat * (1 - p_hat)/n))
r95 <- p_hat + (z95 * sqrt(p_hat * (1 - p_hat)/n))

l95_rounded <- round(l95, digits = 3)
r95_rounded <- round(r95, digits = 3)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)  ### Display in decimal form
sprintf("95 percent Confidence Interval: (%.1f %%, %.1f %%)", l95_rounded*100, r95_rounded*100)  ### Display in % form

#### Interpretation
sprintf("INTERPRETATION: We are 95 percent confident that the interval between %.1f %% and %.1f %% contains the true population proportion of all toothpaste consumers who prefer Crest.", l95_rounded*100, r95_rounded*100)
#### We are 95% confident that the interval between 23.6% and 36.4% contains
#### the true proportion of toothpaste customers who prefer Crest.

############################################
#### Exercise 4 - Part Diameters ###########
############################################

############################################
#### Objective: To find a confidence interval for the standard deviation of part diameters, 
#### and to see how variability affects the proportion of unusable parts produced.
############################################

PartDiameters <- read.csv("PartDiameters.csv")

#### Since the manager is concered with the mean and variability of diameters,
#### it is useful to obtain 95% confidence intervals for both.

### Sample mean
x_bar <- mean(PartDiameters$Diameter)

### Sample Size
n <- nrow(PartDiameters)

### Sample Std. Dev.
s <- sd(PartDiameters$Diameter)

### t-critical for 95% CL

### For 95% confidence... 
t95 <- qt(0.975, df = n - 1, lower.tail = TRUE)

### 95% Confidence Interval
l95 <- x_bar - (t95*s/sqrt(n))
r95 <- x_bar + (t95*s/sqrt(n))

l95_rounded <- round(l95, digits = 3)
r95_rounded <- round(r95, digits = 3)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)

### This confidence interval extends from 9.986 cm to 10.005 cm. 
### Therefore, there is probably not too much cause for concern about the mean. 
### The supervisor can be fairly confident that the mean diameter of all parts 
### is close to 10 cm. 

### Now let us find the 95% confidence interval for standard deviation

### For 95% confidence level, find the chi-sq (lower) and chi-sq (upper)
lchi_sq <- qchisq(0.025, df = n - 1, lower.tail = TRUE)
uchi_sq <- qchisq(0.025, df = n - 1, lower.tail = FALSE)

### Find the 95% CI Limits
l95 <- sqrt((n - 1)*(s^2)/uchi_sq)
r95 <- sqrt((n - 1)*(s^2)/lchi_sq)

l95_rounded <- round(l95, digits = 4)
r95_rounded <- round(r95, digits = 4)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)

### Proportion of parts that are unusable
target_mean <- 10
std_dev <- 0.043
max_deviation_from_usability <- 0.065

upper_bound_usability <- target_mean + max_deviation_from_usability
lower_bound_usability <- target_mean - max_deviation_from_usability

upper_bound_usability
lower_bound_usability

prop_not_usable <- pnorm(lower_bound_usability, target_mean, std_dev, lower.tail = TRUE) + 
  pnorm(upper_bound_usability, target_mean, std_dev, lower.tail = FALSE)

sprintf("Proportion not usable: %.1f %%", prop_not_usable*100)  ### Display in % form

##### Sensitivity Analysis
Mean <- c(x_bar - (t95*s/sqrt(n)), x_bar, x_bar + (t95*s/sqrt(n)))
Std_Dev <- c(sqrt((n - 1)*(s^2)/uchi_sq), s, sqrt((n - 1)*(s^2)/lchi_sq))

names(Mean) = c(x_bar - (t95*s/sqrt(n)), x_bar, x_bar + (t95*s/sqrt(n)))
names(Std_Dev) = c(sqrt((n - 1)*(s^2)/uchi_sq), s, sqrt((n - 1)*(s^2)/lchi_sq))

MyFunction <- function(Mean, Std_Dev) {
  round((pnorm(lower_bound_usability, Mean, Std_Dev, lower.tail = TRUE) + 
           pnorm(upper_bound_usability, Mean, Std_Dev, lower.tail = FALSE)), digits = 3) 
}

outer(Mean, Std_Dev, MyFunction)

##### Lesson
### Each value in the body of the data table is the resulting proportion of unusable parts.
### Obviously, a mean close to the target and a small standard deviation are best, but even 
### this best-case scenario results in 2.5% unusable parts.
### However, a mean off target and a large standard deviation can lead to as many as 14.9% 
### unusable parts. In any case, the message for the supervisor is clear-he must work to reduce
### the underlying variability in the process. This variability is hurting him much more than 
### an off-target mean. 

###############################################
#### Exercise 5 - Healthcare Policy ###########
###############################################

p_hat <- 0.60
q_hat <- (1-0.60)  ### Same as 1 - p-hat
Margin_of_error <- 0.04

### For 95% confidence... 
z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

### Desired Sample Size...Always rounded up.
n <- ceiling(p_hat * q_hat * (z95/Margin_of_error)^2)

n

### Interpretation
### At least 577 people should be surveyed.

###############################################
#### Exercise 6 - Streaming Sample ###########
###############################################

s <- 3
Margin_of_error <- 0.25

### For 95% confidence... 
### z95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)
t95 <- qt(0.975, df = 9, lower.tail = TRUE)

### Desired Sample Size...Always rounded up.
n <- ceiling((t95*s/Margin_of_error)^2)

n

### Interpretation
### At least 737 people should be surveyed.

###############################################
#### Exercise 7 - IRS Refunds #################
###############################################

IRSRefunds <- read.csv("IRSRefunds.csv")

## Total Population Size
N <- 1000000 # 1 million

## Sample size
n <- nrow(IRSRefunds)

## Sample mean
x_bar <- mean(IRSRefunds$Refund)

## Sample Std Dev
sd_sample <- sd(IRSRefunds$Refund)

## Point estimate of Total (T-hat)
T_hat <- N*x_bar

## Std Error of Total (SE of T_HAT)
SE_T_HAT <- N*sd_sample/sqrt(n)

### For 95% confidence... 
t95 <- qt(0.975, df = n - 1, lower.tail = TRUE)

### 95% Confidence Interval
l95 <- T_hat - (t95*SE_T_HAT)
r95 <- T_hat + (t95*SE_T_HAT)

l95_rounded <- format(round(l95), big.mark = ",", scientific = F)
r95_rounded <- format(round(r95), big.mark = ",", scientific = F)

### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)
### Display 95% CI
sprintf("95 percent Confidence Interval: (%s, %s)", l95_rounded, r95_rounded)
### INTERPRETATION
sprintf("INTERPRETATION: IRS can be 95 percent confident that the interval between $ %s and $ %s contains the true total amount that will be paid out to these 1,000,000 taxpayers.", l95_rounded, r95_rounded)

###############################################
#### Exercise 8 - ISlim #######################
###############################################

ISlim <- read.csv("ISlim.csv")

boxplot(ISlim$Supplier1, ISlim$Supplier2, names = c("Supplier 1", "Supplier 2"), horizontal = TRUE, main = "Boxplot for Two Suppliers", xlab = "Hours")

## These show that (1) the distributions of times until failure are skewed to the right for each supplier, 
## (2) the mean for supplier 1 is somewhat greater than the mean for supplier 2, and 
## (3) there are several outliers. 
## There seems to be little doubt that supplier 1's motors will last longer on average than supplier 2's - or is there? 
## A confidence interval for the mean difference allows us to see whether the differences apparent in the 
## box plots can be generalized to all motors from the two suppliers. 

x <- t.test(ISlim$Supplier1, ISlim$Supplier2, var.equal = FALSE, paired = FALSE, conf.level = 0.95)

lcl <- x$conf.int[1]
ucl <- x$conf.int[2]

### Display 95% CI
sprintf("95 percent Confidence Interval: (%.2f, %.2f)", lcl, ucl)

### Interpretation:
### Supplier 1's motors last on average from 47 hours fewer than to 233 hours greater than those from Supplier 2.

### What should ISlim do?
## Should ISlim continue with supplier 1? 
## This depends on the trade-off between the cost of the motors and warranty costs (and any other relevant costs). 
## Because the warranty probably depends on whether a motor lasts a certain amount of time, 
## warranty costs probably depend on a proportion (the proportion that fail before 500 hours, say) 
## rather than a mean. 

###############################################
#### Exercise 9 - Coupon Effectiveness ########
###############################################

## Read the data into R
CouponEffectivess <- read.csv("CouponEffectiveness.csv")

## How many of them received coupon?
n_received_coupon <- sum(CouponEffectivess$Received_coupon == "Yes")

## How many of them have no received coupon?
n_not_received_coupon <- sum(CouponEffectivess$Received_coupon == "No")

## How many of those who received the coupon purchased?
n_received_coupon_purchase <- sum(CouponEffectivess$Received_coupon == "Yes" & CouponEffectivess$Purchased == "Yes")

## How many of those who did not receive the coupon yet purchased?
n_not_received_coupon_but_purchase <- sum(CouponEffectivess$Received_coupon == "No" & CouponEffectivess$Purchased == "Yes")

## Sample proportion 1 = Num of people received coupon and purchased/num of people received coupon
p_1_hat <- n_received_coupon_purchase/n_received_coupon

## Sample proportion 2 = Num of people did not receive coupon but purchased/num of people not receive coupon
p_2_hat <- n_not_received_coupon_but_purchase/n_not_received_coupon

prop.test(c(n_received_coupon_purchase, n_not_received_coupon_but_purchase), c(n_received_coupon, n_not_received_coupon), alternative = "two.sided", conf.level = 0.95)

####################################################################
#### Exercise 9 - Coupon Effectiveness ALTERNATIVE SOLUTION ########
####################################################################

z_critical_95 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE)

margin_of_error <- z_critical_95*sqrt((p_1_hat*(1-p_1_hat)/n_received_coupon) + (p_2_hat*(1-p_2_hat)/n_not_received_coupon))

lcl <- (p_1_hat - p_2_hat) - margin_of_error
ucl <- (p_1_hat - p_2_hat) + margin_of_error

### Display 95% CI
sprintf("95 percent Confidence Interval: (%.2f, %.2f)", lcl, ucl)

### INTERPRETATION
### We see that 36.67% of customers who received a coupon purchased something, as opposed to only 23.33% of those who didn't receive a coupon. 
### The difference, 36.67% - 23.33% = 13.33% (or 0.1333), is the quantity of interest. 
### Specifically, the sample difference is 13.33%, and the objective is to find a confidence interval for this difference for the entire population.
### They show that the confidence interval for the difference between proportions extends from 0.031 to 0.236 (all positive).
### So there is good reason to conclude that the proportion who purchase is larger for those who receive a coupon. 