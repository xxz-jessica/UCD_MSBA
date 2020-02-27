## Exercise 4

Ex4 <- read.csv("Exercise 4 - Cereal Sales.csv")

Ex4_Stacked <- stack(Ex4)

names(Ex4_Stacked) <- c("Sales", "Shelf")

Sales <- Ex4_Stacked$Sales
Shelf <- Ex4_Stacked$Shelf

ANOVA_Ex1 <- aov(Sales ~ Shelf)
summary(ANOVA_Ex1)

model_ex1 <- lm(Sales ~ Shelf)
resids_ex1 <- residuals(model_ex1)
preds_ex1 <- predict(model_ex1)

nortest::ad.test(resids_ex1)

hist(resids_ex1)

qqnorm(resids_ex1)
qqline(resids_ex1)

car::leveneTest(Sales ~ Shelf)

plot(fitted(model_ex1), residuals(model_ex1))

TukeyHSD(ANOVA_Ex1)

plot(TukeyHSD(ANOVA_Ex1), las = 1)


## Welch's test when needed.
oneway.test(Sales ~ Shelf, var.equal = FALSE)

## Exercise 5
Ex5 <- read.csv("Exercise 5 - Soap Sales.csv")

PackageType <- factor(Ex5$PackageType) ## Pay attention to this...
Sales <- Ex5$Sales

## Optional
boxplot(Sales ~ PackageType)

install.packages("Rcmdr")
library(Rcmdr)
numSummary(Sales, group = PackageType)

tapply(Sales, PackageType, sd) ## Only look at Std Dev

model_ex5 <- lm(Sales ~ PackageType)
resids_ex5 <- residuals(model_ex5)
preds_ex5 <- predict(model_ex5)

nortest::ad.test(resids_ex5)

hist(resids_ex5)

qqnorm(resids_ex5)
qqline(resids_ex5)

car::leveneTest(Sales ~ PackageType)

fligner.test(Sales ~ PackageType)

plot(fitted(model_ex5), residuals(model_ex5))

ANOVA_Ex5 <- aov(Sales ~ PackageType)
summary(ANOVA_Ex5)

## Ex 6

Ex6 <- read.csv("Exercise 6 - Salespeople Performance.csv")

QuarterlySales <- Ex6$QuarterlySales
CompScheme <- factor(Ex6$CompensationScheme)

## ANOVA_Ex6 <- aov(QuarterlySales ~ CompScheme)
## summary(ANOVA_Ex6)

model_ex6 <- lm(QuarterlySales ~ CompScheme)
resids_ex6 <- residuals(model_ex6)
preds_ex6 <- predict(model_ex6)

nortest::ad.test(resids_ex6)

hist(resids_ex6)

qqnorm(resids_ex6)
qqline(resids_ex6)

car::leveneTest(QuarterlySales ~ CompScheme)

plot(preds_ex6, resids_ex6)

## Optional
boxplot(QuarterlySales ~ CompScheme)

fligner.test(QuarterlySales ~ CompScheme)

## Welch's test
oneway.test(QuarterlySales ~ CompScheme, var.equal = FALSE)

install.packages("userfriendlyscience")
library(userfriendlyscience)

posthocTGH(QuarterlySales, CompScheme, method = "games-howell")

## Ex 7

Ex7 <- read.csv("Exercise 7 - Rebco Payments.csv")

CustomerSize <- factor(Ex7$CustomerSize)
Days <- Ex7$Days
Amount <- Ex7$Amount

## Focus first on Days Until Payment

boxplot(Days ~ CustomerSize)

ANOVA7 <- aov(Days ~ CustomerSize)
summary(ANOVA7)

model_ex7 <- lm(Days ~ CustomerSize)
resids_ex7 <- residuals(model_ex7)
preds_ex7 <- predict(model_ex7)

nortest::ad.test(resids_ex7)

hist(resids_ex7)

qqnorm(resids_ex7)
qqline(resids_ex7)

car::leveneTest(Days ~ CustomerSize)

plot(fitted(model_ex7), residuals(model_ex7))

## Next Focus on Payment Amounts

model_ex7t <- lm(Amount ~ CustomerSize)
resids_ex7t <- residuals(model_ex7t)
preds_ex7t <- predict(model_ex7t)

nortest::ad.test(resids_ex7t)

hist(resids_ex7t)

qqnorm(resids_ex7t)
qqline(resids_ex7t)

plot(fitted(model_ex7t), residuals(model_ex7t))

boxplot(Amount ~ CustomerSize)

tapply(Amount, CustomerSize, sd) ## Only look at Std Dev

car::leveneTest(Amount ~ CustomerSize)

### Equal var assumption is violated. So take logs of Dependent var.

TransformedAmount <- log(Amount)

boxplot(TransformedAmount ~ CustomerSize)

## Run the ANOVA for the transformed Model...
ANOVA7Trans <- aov(TransformedAmount ~ CustomerSize)
summary(ANOVA7Trans)

model_ex7_Trans <- lm(TransformedAmount ~ CustomerSize)
resids_ex7_Trans <- residuals(model_ex7_Trans)
preds_ex7_Trans <- predict(model_ex7_Trans)

hist(resids_ex7_Trans)

car::leveneTest(TransformedAmount ~ CustomerSize)

plot(fitted(model_ex7_Trans), residuals(model_ex7_Trans))

TukeyHSD(ANOVA7Trans, las = 1)

plot(TukeyHSD(ANOVA7Trans), las = 1)

## Welch's test for Amount ~ Customer size.
oneway.test(Amount ~ CustomerSize, var.equal = FALSE)

library(userfriendlyscience)

posthocTGH(Amount, CustomerSize, method = "games-howell")

## Ex 8
Ex8 <- read.csv("Exercise8.csv")

Price <- Ex8$Price
Brands <- factor(Ex8$Brands)
Area <- factor(Ex8$MetroArea)

ANOVA8 <- aov(Price ~ Brands + Area)
summary(ANOVA8)

TukeyHSD(ANOVA8, which = 'Brands', ordered = TRUE)

plot(TukeyHSD(ANOVA8, which = 'Brands', ordered = TRUE), las = 1)

TukeyHSD(ANOVA8, which = 'Area', ordered = TRUE) ## IF needed ## If needed

plot(TukeyHSD(ANOVA8, which = 'Area', ordered = TRUE), las = 1)

## Ex 9

Ex9 <- read.csv("Exercise 9 - Coke Sales.csv")

Sales <- Ex9$Sales
Beverage <- factor(Ex9$Beverage)
Building_Block <- factor(Ex9$Building_Block)

ANOVA9 <- aov(Sales ~ Beverage + Building_Block)
summary(ANOVA9)

model_ex9 <- lm(Sales ~ Beverage + Building_Block)
resids_ex9 <- residuals(model_ex9)
preds_ex9 <- predict(model_ex9)

residuals_Ex9 <- resid(ANOVA9)

plot(fitted(model_ex9), residuals(model_ex9))

nortest::ad.test(residuals_Ex9)

qqnorm(residuals_Ex9)
qqline(residuals_Ex9)

shapiro.test(residuals_Ex9)

TukeyHSD(ANOVA9, which = 'Beverage', ordered = TRUE)

plot(TukeyHSD(ANOVA9, which = 'Beverage', ordered = TRUE), las = 1)

TukeyHSD(ANOVA9, which = 'Building_Block', ordered = TRUE) ## IF needed ## If needed

plot(TukeyHSD(ANOVA9, which = 'Building_Block', ordered = TRUE), las = 1)

## Another option - agricolae package
library(agricolae)

tukey.test.res <- HSD.test(ANOVA9, trt = 'Beverage')

tukey.test.res

## Ex 10

Ex10 <- read.csv("Exercise 10.csv")

Gender <- Ex10$Gender
Sector <- Ex10$Sector
Wage <- Ex10$Wage

Model_10 <- aov(Wage ~ Gender + Sector + Gender*Sector)
summary(Model_10)

interaction.plot(Gender, Sector, Wage)

interaction.plot(Sector, Gender, Wage)

## Ex 11

Ex11 <- read.csv("Exercise 11.csv")

Participation <- Ex11$Participation
Times <- factor(Ex11$Times)
Config <- factor(Ex11$Config)

Model_11 <- aov(Participation ~ Times + Config + Times*Config)
summary(Model_11)

interaction.plot(Times, Config, Participation)

interaction.plot(Config, Times, Participation)

TukeyHSD(Model_11, ordered = TRUE) ## IF needed ## If needed

## Ex 12

Ex12 <- read.csv("Exercise 12.csv")

Class <- factor(Ex12$Class)
Type <- factor(Ex12$Type)
MPG <- Ex12$MPG

Model_12 <- aov(MPG ~ Class + Type + Class*Type)
summary(Model_12)

interaction.plot(Class, Type, MPG)

interaction.plot(Type, Class, MPG)
