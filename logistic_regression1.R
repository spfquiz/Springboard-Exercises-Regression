## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:
setwd("~/Data/exercises/logistic_regression")

NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))

# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

library(effects)
plot(allEffects(hyp.out))

### ****  Note that this shows evidence of outliers or incorrect data which we would have probably ****
### ****  spotted with a summary on the data table in the first place.  For example it's inconceivable ****
### ****  that someone sleeps more than 24 hours a day.  Also a BMI of 99.99 looks like a default.  ****

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
##   2. Predict the probability of working for each level of marital
##      status.

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.

library(ggplot2)
library(dplyr)
library(Hmisc)

##    Start by checking the structure and summary of the data
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels
str(NH11)
summary(NH11)

##    Simple cluster graphs of the 3 elements
ggplot(data = NH11, aes(x = age_p, group = as.factor(everwrk))) +
  geom_histogram(binwidth = 1)

##  **** Clearly the age 85 is particularly frequent - there's a good chance that it means that 85 is used for 
##  **** all values >= 85 or that data collection was biased towards 85 year old patients.  
##  **** As this is a model it would probably be best just to run from 18 to 84.

NH11_clean <- subset(NH11, age_p < 85)


##    everwk has a very large number of entries other than "Yes" or "No".  
##    If we excluded these entries would this create bias in the dataset?  ie make this an unrepresentative
##    sample?  (This would be extremely important to test in a serious study, but for this exercise I'm
##    only going to create a visual comparison).

everwrk_na <- NH11_clean
everwrk_na$everwrk <- as.factor(!is.na(everwrk_na$everwrk))
levels(everwrk_na$everwrk) <- c(levels(everwrk_na$everwrk), "Ever worked answered Y or N", "Ever worked not answered")
everwrk_na$everwrk[everwrk_na$everwrk == "TRUE"] <- "Ever worked answered Y or N"
everwrk_na$everwrk[everwrk_na$everwrk == "FALSE"] <- "Ever worked not answered"

ggplot(data = everwrk_na, aes(x= everwrk_na$age_p)) + 
  geom_histogram(binwidth = 5, size = 0.9, aes(y = stat(width * density))) +
  facet_wrap(everwrk_na$everwrk) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age", y = "Percentage")

##  *** These distributions show some key differences - it's seems much more likely for a student or
##  *** retiree aged patient to have data on working than those in the "normal" working age brackets

ggplot(everwrk_na, aes(x= everwrk_na$r_maritl,  group=everwrk_na$everwrk)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..), stat="count"))+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percentage", fill="Marital Status", x = "Marital Status") +
  facet_grid(everwrk_na$everwrk) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##  *** Much more likely to ask someone widowed if they've ever worked

ggplot(everwrk_na, aes(x= everwrk_na$sex,  group=everwrk_na$everwrk)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..), stat="count"))+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percentage", fill="Sex", x = "Sex") +
  facet_grid(everwrk_na$everwrk) +
  scale_y_continuous(labels = scales::percent)

##  *** Appears to be some disparity between Male & Female proportions
  
ggplot(everwrk_na, aes(x= everwrk_na$mracrpi2,  group=everwrk_na$everwrk)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..), stat="count"))+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percentage", fill="Ethnic Gp", x = "Ethnic Gp") +
  facet_grid(everwrk_na$everwrk) +
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##  *** Seems broadly in line

##  **** So in glancing at a small selection of fields there are a couple of suggestions that recording
##  **** data for "Ever Worked" may depend on the values of some other data fields and that therefore
##  **** there may be some bias in the subset of data where "Ever worked?" has been answerered with "Yes" or
##  **** or "No".  However as this isn't the objective of the exercise, I'll leave it there.

# collapse all missing values to NA
NH11_clean$everwrk <- factor(NH11_clean$everwrk, levels=c("2 No", "1 Yes"))

# run our regression model
everwrk.out <- glm(everwrk~age_p+r_maritl,
               data=NH11_clean, family="binomial")
coef(summary(everwrk.out))

library(effects)
plot(allEffects(everwrk.out)) 

##  *** However now the question asks for probability per Marital Status only.  Rather than choose a particular
##  *** age which would potentially skew the answer (for example never married tend to be younger and 
##  *** widowed tend to be older), I'm recasting the model to just take marital status.

everwrk.out1 <- glm(everwrk~r_maritl,
                   data=NH11_clean, family="binomial")
coef(summary(everwrk.out1))

# Create a dataset with predictors set at desired levels
predDat1 <- with(NH11_clean,
                expand.grid(r_maritl = c("1 Married - spouse in household", "2 Married - spouse not in household",
                                      "4 Widowed", "5 Divorced", "6 Separated", "7 Never married",
                                      "8 Living with partner", "9 Unknown marital status")))
# predict ever worked at those levels
cbind(predDat1, predict(everwrk.out1, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat1))

##  **** So the probabilities range from 0.751 for never married up to 0.947 for Divorced
