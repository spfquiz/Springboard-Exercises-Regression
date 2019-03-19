#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
setwd("~/Data/exercises/linear_regression")


##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

# summary of metro and energy columns, all rows
sts.met.nrg <- subset(states.data, select = c("metro", "energy"))
summary(sts.met.nrg)

##  There are NA values here which will inhibit some calculations

sts.met.nrg <- sts.met.nrg[-c(9),]   # REMOVE DofColombia which has NA data

# correlation between metro and energy
cor(sts.met.nrg)

## The correlation shows a negative relationship between % Metropolitan and Energy usage per capita

# scatter plot of metro vs energy
plot(sts.met.nrg)

##  There are some extreme outliers here:
subset(states.data, energy > 500)

## As maybe expected the highest energy use is for Alaska probably due to the extreme climate.  The others are, 
## however, less predictable.  There is a very good argument for excluding Alaska from the model as an exception if
## metro is the main variable.

states.clean <- states.data[-c(2,9),]   # REMOVE Alaska as an outlier and DofColombia for NA

stat.nrg.model <- lm(energy ~ metro, data = states.clean)  # without Alaska
summary(stat.nrg.model)

stat.nrg.model1 <- lm(energy ~ metro, data = states.data)  # with Alaska
summary(stat.nrg.model1)

##  This isn't a great fit and can probably be improved - interestingly the model works slightly better
## with the Alaska outlier included rather than excluded

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## What has the highest correlation with Energy?
states.cor <- states.data[,3:21]  # create dataframe with just numeric values
states.cor <- states.cor[-c(9),]  # take out DofColombia which has a large number of NA values
cor(states.cor)

## The most significant correlation values to Energy are toxic, house and area.  However green is showing NA so
## could still be a significant variable and could be worth including in the initial model

stat.nrg.model2 <- lm(energy ~ area + toxic + house + green, data = states.data)  
summary(stat.nrg.model2)

## This is a significant improvement on R-squared.  Interestingly green has the best t-value so was well worth
## including.  However house doesn't appear to be significant so can be excluded.

stat.nrg.model3 <- lm(energy ~ area + toxic + green, data = states.data)  
summary(stat.nrg.model3)

## This gives a marginal drop on R-squared, but slightly improves the adjusted R-squared.  Area is still fairly
## insignificant so we'll try without it.

stat.nrg.model4 <- lm(energy ~ toxic + green, data = states.data)  
summary(stat.nrg.model4)

##  This model drop both normal and adjusted R-squared by about 0.015.  What's the nature of the relationship between
## Area and Energy?

sts.area.nrg <- subset(states.data, select = c("area", "energy"))
summary(sts.area.nrg)
plot(sts.area.nrg)

## Alaska is an extreme point - to what extent is the relationship influenced by this?

sts.area.nrg <- sts.area.nrg[-c(9),]  # exclude NA from DofColombia
cor(sts.area.nrg)

sts.area.nrg <- sts.area.nrg[-c(2),]  # exclude Alaska datapoint
cor(sts.area.nrg)

##  So the correlation between Area and Energy is heavily dependent on the Alaska datapoint.  THerefore for my 
## preferred model I will stick with model4

## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

stat.nrg.model4 <- lm(energy ~ toxic + green, data = states.data)  
summary(stat.nrg.model4)  # Base Model

stat.nrg.model5 <- lm(energy ~ toxic * green, data = states.data)  
summary(stat.nrg.model5)  # Interaction Model

##  There's an improvement in the model when adding an interaction between toxic and green


##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

states.data$region <- factor(states.data$region)
#Add region to the model
sat.region1 <- lm(energy ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region1)) # show regression coefficients table
anova(sat.region1) # show ANOVA table

## Although it appears that there's a particularly big change between West and North East regions, the ANOVA table
## doesn't give any major significance to Region

