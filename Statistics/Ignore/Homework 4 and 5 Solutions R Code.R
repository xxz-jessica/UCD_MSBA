### Question 2
Question2 <- read.csv("Question2.csv")

PASWR2::z.test(Question2$Minutes, sigma.x = 8, alternative = "less", mu = 32)

### Type II Error
pnorm(30.75, mean = 30, sd = 8/sqrt(110), lower.tail = FALSE)
### Value may differ slightly from my solutions since I used formula instead
### of R.

### Question 4
Question4 <- read.csv("Question4.csv")

Age <- Question4$Age
Cereal <- factor(Question4$Cereal)
Income <- Question4$Income
Education <- Question4$Education

### For Age
ANOVA_Age <- aov(Age ~ Cereal)
summary(ANOVA_Age)

### Diagnostics

### Plot histograms of age for each cereal product
hist(subset(Age, Cereal == 1))
hist(subset(Age, Cereal == 2))
hist(subset(Age, Cereal == 3))
hist(subset(Age, Cereal == 4))

### Collect the Residuals
model_age <- lm(Age ~ Cereal)
resids_age <- residuals(model_age)
preds_age <- predict(model_age)

### Run AD test
nortest::ad.test(resids_age) ## Residuals are normal

### Histogram of residuals
hist(resids_age)

### Normal Probability Plot
qqnorm(resids_age)
qqline(resids_age)

### Constant Variance Assessment
car::leveneTest(Age ~ Cereal)

plot(fitted(model_age), residuals(model_age))

## Welch's ANOVA
oneway.test(Age ~ Cereal, var.equal = FALSE)

### Post Hoc for Age
userfriendlyscience::posthocTGH(Age, Cereal, method = "games-howell")

### For Income
ANOVA_Income <- aov(Income ~ Cereal)
summary(ANOVA_Income)

### Diagnostics

### Plot histograms of age for each cereal product
hist(subset(Income, Cereal == 1))
hist(subset(Income, Cereal == 2))
hist(subset(Income, Cereal == 3))
hist(subset(Income, Cereal == 4))

### Collect the Residuals
model_income <- lm(Income ~ Cereal)
resids_income <- residuals(model_income)
preds_income <- predict(model_income)

### Run AD test
nortest::ad.test(resids_income) ## Residuals are normal

### Histogram of residuals
hist(resids_income)

### Normal Probability Plot
qqnorm(resids_income)
qqline(resids_income)

### Constant Variance Assessment
car::leveneTest(Income ~ Cereal)

plot(fitted(model_income), residuals(model_income))

### All conditions satisfied. Run One-Way ANOVA.
ANOVA_Income <- aov(Income ~ Cereal)
summary(ANOVA_Income)

### Tukey's test
TukeyHSD(ANOVA_Income, las = 1)
plot(TukeyHSD(ANOVA_Income, las = 1))

### For Education
ANOVA_Education <- aov(Education ~ Cereal)
summary(ANOVA_Education)

### Diagnostics

### Plot histograms of age for each cereal product
hist(subset(Education, Cereal == 1))
hist(subset(Education, Cereal == 2))
hist(subset(Education, Cereal == 3))
hist(subset(Education, Cereal == 4))

### Collect the Residuals
model_education <- lm(Education ~ Cereal)
resids_education <- residuals(model_education)
preds_education <- predict(model_education)

### Run AD test
nortest::ad.test(resids_education) ## Residuals are normal

### Histogram of residuals
hist(resids_education)

### Normal Probability Plot
qqnorm(resids_education)
qqline(resids_education)

### Constant Variance Assessment
car::leveneTest(Education ~ Cereal)

plot(fitted(model_education), residuals(model_education))

### All conditions satisfied. Run One-Way ANOVA.
ANOVA_Education <- aov(Education ~ Cereal)
summary(ANOVA_Education)

### Question 6
Question6 <- read.csv("Question6Stacked.csv")

Sales <- Question6$Sales
Graduate <- Question6$Graduate
Block <- factor(Question6$Block)

ANOVA6 <- aov(Sales ~ Graduate + Block)
summary(ANOVA6)

model_q6 <- lm(Sales ~ Graduate + Block)
resids_q6 <- residuals(model_q6)
preds_q6 <- predict(model_q6)

residuals_q6 <- resid(ANOVA6)

plot(fitted(model_q6), residuals(model_q6))

nortest::ad.test(residuals_q6)

qqnorm(residuals_q6)
qqline(residuals_q6)

shapiro.test(residuals_q6)

TukeyHSD(ANOVA6, which = 'Graduate', ordered = TRUE)
plot(TukeyHSD(ANOVA6, which = 'Graduate', ordered = TRUE), las = 1)

## Another option - agricolae package
library(agricolae)

tukey.test.q6 <- HSD.test(ANOVA6, trt = 'Block')
tukey.test.q6

### Question 7
Question7 <- read.csv("Question7Stacked.csv")

Miles <- Question7$Miles
Age <- factor(Question7$Age)
Gender <- factor(Question7$Gender)

model_q7 <- aov(Miles ~ Age + Gender + Age*Gender)
summary(model_q7)

interaction.plot(Gender, Age, Miles)

interaction.plot(Age, Gender, Miles)

### Question 8
Obs <- c(0, 20, 83, 52)
Exp <- c(0.105, 0.219, 0.533, 0.143)

chisq.test(x = Obs, p = Exp)

### Question 9
Question9 <- read.csv("Question9PrepData.csv") ## Contains only two columns

chisq.test(Question9$MarketDirection, Question9$DayOfWeek)

### Question 10
Question10 <- read.csv("Question10Stacked.csv")

Asset_SomeCollege <- subset(Question10, Question10$EDCL == 3)
Asset_CollegeDegree <- subset(Question10, Question10$EDCL == 4)

### Check for normality
hist(Asset_SomeCollege$ASSET)
hist(Asset_CollegeDegree$ASSET)

### Normality requirement failed... Run Wilcoxon Rank Sum Test

## Save the values
Asset <- as.integer(Question10$ASSET)
EducationLevel <- as.factor(Question10$EDCL)

## Run the test
wilcox.test(Asset ~ EducationLevel, alt = "less", paired = FALSE, exact = FALSE, conf.level = 0.95)

### Question 11
Q11 <- read.csv("Question11Stacked.csv")

### Plotting histograms to assess the condition of normality
Main <- subset(Q11, Location =="Main")
hist(Main$Times)

Satellite1 <- subset(Q11, Location =="Satellite 1")
hist(Satellite1$Times)

Satellite2 <- subset(Q11, Location =="Satellite 2")
hist(Satellite2$Times)

Satellite3 <- subset(Q11, Location =="Satellite 3")
hist(Satellite3$Times)

### Run the Kruskal-Wallis Test
kruskal.test(Q11$Times ~ factor(Q11$Location))

## Not needed.
boxplot(Q11$Times ~ Q11$Location)

## Post Hoc Test -- Paiwise Wilcoxon test for multiple comparison
pairwise.wilcox.test(Q11$Times, Q11$Location, p.adjust = "bonferroni")

## Post Hoc Test -- Dunn test for multiple comparison
library(PMCMR)

posthoc.kruskal.dunn.test(x=Q11$Times, g=Q11$Location, p.adjust.method = "bonferroni")

## Post hoc Test -- Nemenyi test
posthoc.kruskal.nemenyi.test(x=Q11$Times, g=Q11$Location, p.adjust.method = "bonferroni")

### Question 12
Q12 <- read.csv("Question12Stacked.csv")

PersonBlock <- factor(Q12$PersonBlock)

## Run the Friedman test
stats::friedman.test(Q12$Ratings, Q12$Recipe, PersonBlock)

### Correction on # 12 on my handwritten Solutions.
### Need to look at p-value Adjusted for ties.
### Decision to Reject H0.

### Question 13
Question13 <- read.csv("Question13.csv")
cor.test(x = Question13$Growth, y = Question13$Value, alternative = "greater",method = "spearman", exact = FALSE)

### Question 14
Question14 <- read.csv("Question14.csv")
Question14_Cleaned <- na.omit(Question14)

cor.test(x = Question14_Cleaned$HRS1, y = Question14_Cleaned$JOBLOSE, alternative = "greater",method = "spearman", exact = FALSE)
### The handwritten solution is wrong. The result from this code is correct.

### Question 16
Question16 <- read.csv("Question16.csv")

## Run the test
wilcox.test(Question16$BN, Question16$Amazon, alt = "two.sided", paired = TRUE, conf.level = 0.95)

### Question 17
Obs <- c(10, 18, 48, 16, 8)
Exp <- c(0.0668, 0.2417, 0.3829, 0.2417, 0.0669)

chisq.test(x = Obs, p = Exp) ## Default df = 4 (incorrect)

### The actual degrees of freedom should be 2.
pchisq(8.7056, 2, lower.tail = FALSE)


