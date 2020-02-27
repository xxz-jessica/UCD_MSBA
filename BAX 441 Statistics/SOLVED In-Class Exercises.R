##### Exercise 2 ######

## Note: You have to write this on the submission document using Equation editor and not in R like I have written.
## I wrote as a part of the R code here so that it serves as a convenient reference for you.  
## H0: mu <= $85
## H1: mu > $85

Exercise2 <- read.csv("Exercise2.csv")

t.test(Exercise2$Orders, alternative = "greater", mu = 85)

## Original claim: egrocer would be profitable (> $85) - does not contain equality and we reject H0 at 5% significance level.
## INTERPRETATION AND FINAL CONCLUSION: At 5% significance level, there is sufficient evidence to support the claim that 
## the egrocer will be profitable in this city.

## sd(Exercise2$Orders)

## qt(0.05, 84, lower.tail = FALSE)

##### Exercise 3 ######

## Note: You have to write this on the submission document using Equation editor and not in R like I have written.
## I wrote as a part of the R code here so that it serves as a convenient reference for you.  
## H0: p >= 0.40
## H1: p < 0.40

Exercise3 <- read.csv("Exercise3.csv")

num_Yes <- sum(Exercise3$Exercise == 'Yes')
num_Yes_1 <- length(which(Exercise3$Exercise == 'Yes'))

prop.test(num_Yes, nrow(Exercise3), alternative = "less", p = 0.4)

## Original claim: initiate the program if p < 0.40. Does not contain equality and we fail to reject H0 at 1% significance level.
## INTERPRETATION AND FINAL CONCLUSION: At 1% significance level, there is not sufficient evidence to support the claim that less than
## 40% of the company's personnel take time to exercise prior to eating lunch.

## Original claim: initiate the program if p < 0.40. Does not contain equality and we reject H0 at 10% significance level.
## INTERPRETATION AND FINAL CONCLUSION: At 10% significance level, there is sufficient evidence to support the claim that less than
## 40% of the company's personnel take time to exercise prior to eating lunch.

##### Exercise 4 ######

## Note: You have to write this on the submission document using Equation editor and not in R like I have written.
## I wrote as a part of the R code here so that it serves as a convenient reference for you.  
## H0: sigma_sq <= 18
## H1: sigma_sq > 18

Exercise4 <- read.csv("Exercise4.csv")

varTest(Exercise4$Speeds, alternative = "greater", sigma.squared = 18)

## Original claim: The number of accidents will be unacceptbly high. Does not contain equality and we reject H0 at 10% significance level.
## INTERPRETATION AND FINAL CONCLUSION: At 10% significance level, there is sufficient evidence to support the claim that the number of accidents will be 
## unacceptably high.
