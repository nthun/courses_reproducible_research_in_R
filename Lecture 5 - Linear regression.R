# Lecture 5 - Regression
# Load these packages
library(tidyverse)
library(broom)

# Read the cocktails dataset
cocktails <- read_tsv("https://raw.github.com/nthun/cocktail-balance/master/cocktail_data.tsv")
cocktails <- read_tsv("http://bit.ly/2zbj7kA") # Same stuff, but shortened url

# To be able to use catagorical variables in statistical models, we may need to convert the categories to dummy variables. This means that the variable name will be the category name, and if this category is true for the observation, the value of the variable will be 1, or otherwise 0.
# Task: create dummy coded variables in the cocktail dataset  from the type variable
cocktails %>% distinct(type)
dummy_cocktails <-
    cocktails %>% 
    mutate(value = 1) %>% # This defines what will be the spread across variables
    spread(type, value, fill = 0) #%>% # Add fill for binary coding, otherwise it will be NA
# select(name, blended:stirred) # Just to show the important variables, otherwise, don't use this


# Create simple linear regression of cocktail acidity on alcohol content
# This only returns intercept and slope
lm(abv ~ acid, data = cocktails)

# Store the model in a variable to be able to get details, predictions, etc.
acid_lm <- lm(abv ~ acid, data = cocktails)
summary(acid_lm)

# This also works without storing the results. However when you use pipes, mind that in lm(), data is not the first parameter
cocktails %>% 
    lm(abv ~ acid, data = .) %>% 
    summary()

# Plot the linear regression
cocktails %>% 
    ggplot() +
    aes(y = abv, x = acid) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) 

# The functions from broom package give you the informaion in neat data frames. 
# tidy() is for the coefficients()


# I recommend using the broom functions instead of summary()
# tidy() returns the coefficients summary in a neat data frame format
tidy(acid_lm)
# glance() is for the model fit measures
glance(acid_lm)
# augment() adds predictions, residuals and residual diagnostic to each data point
augment(acid_lm, cocktails)

# To get the standardized coefficients (scale free), you need to standardize the output and predictor variables. Use the scale() function on all variables in the model
acid_lm_std <- lm(scale(abv) ~ scale(acid), data = cocktails)
summary(acid_lm_std)
# You can check that the slope of acid now matches the correlation between abv and acid
cor(cocktails$abv, cocktails$acid)

## Predicting values based on the model
# Create data with new values to predict
newdata <- tibble(acid = c(0.2, 0.3, 0.4))
# predict returns a vector of predictions
predict(acid_lm, newdata)
# modelr::add_predictions() returns a data frame. This one is the preferable.
modelr::add_predictions(newdata, acid_lm)

## Model fit measures
# See model fitting "game" at http://www.dangoldstein.com/regression.html

# Plot the residuals (error term from the model prediction)
# Ignore the warnings, thet are known developer bugs
# This plot shows the unexplained variance of the model (summary of the red lines)
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .) %>% 
    ggplot() +
    aes(y = abv, x = acid) +
    geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
    geom_segment(aes(xend = acid, yend = .fitted), linetype = "dashed", color = "red", size = 1.2) +
    geom_point(size = 3)

# All variability in the outcome variable (variance)
# This plots shows the total variance of the outcome variable (summary of the blue lines)
cocktails %>% 
    mutate(mean_abv = mean(abv)) %>% 
    ggplot() +
    aes(y = abv, x = acid) +
    geom_hline(aes(yintercept = mean_abv), size = 1.5) +
    geom_segment(aes(xend = acid, yend = mean_abv), linetype = "dashed", color = "blue", size = 1.2) +
    geom_point(size = 3)

# Improvement of the fit by using the model, compared to only using the mean
# This plots shows the total variance of the outcome variable (summary of the blue lines)
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .) %>% 
    mutate(mean_abv = mean(abv)) %>% 
    ggplot() +
    aes(y = abv, x = acid) +
    geom_hline(aes(yintercept = mean_abv), size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "yellow") +
    geom_segment(aes(xend = acid, yend = mean_abv, y = .fitted), linetype = "dashed", color = "purple", size = 1.2) +
    geom_point(size = 2)

## Diagnostics
# The residuals should not have an underlying pattern: they should have a normal distribution
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .) %>% 
    ggplot() +
    aes(.resid) +
    geom_histogram(bins = 10)

# We can also do a normality test on the residuals
# The Shapiro-Wilks test shows that the residuals are normally distributed
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .) %>% 
    pull(.resid) %>% 
    shapiro.test(.)

# To explore the residuals we are actually better off to use the ggfortify package to make us diagnostic plots, using the autoplot() function
if (!require(ggfortify)) install.packages("ggfortify")
library(ggfortify)
# This will plot 6 different diagnostic plots that are all useful to tell if the predicion is reliable
# See explanation on the slides
autoplot(acid_lm, which = 1:6, label.size = 3)
# See how to interpret diagnostic plots in slides
# Let's store the diagnostic values in a variable
acid_lm_diag <- augment(acid_lm, cocktails)

# We can single out observations with the clice() function
cocktails %>% 
    slice(c(9, 44, 45))

# We can rerun the lm without cases that have zero acid
acid_lm_clean <-
    cocktails %>% 
    filter(acid != 0) %>% 
    lm(abv ~ acid, data = .)
summary(acid_lm_clean)

# Check diagnostics again
autoplot(acid_lm_clean, which = 1:6)
# We can see that the residuals are still not perfect, which makes the reliability of the model shaky
# Dealing with diagnostics is often an iterative process, where problematic values are investigated recursive
cocktails %>% 
    augment(lm(abv ~ acid, data = .), .)

## MULTIPLE REGRESSION
# Syntax for interactions
# Add multiple predictors: <outcome variable> ~ <predictor 1> + <predictor 2>
# You can choose to get a more verbose output using the summary() function.

lm1 <- lm(abv ~ acid + sugar, data = cocktails)
summary(lm1)
tidy(lm1)
# Add multiple predictors AND their interactions: <outcome variable> ~ <predictor 1> * <predictor 2>
lm2 <- lm(abv ~ acid * sugar, data = cocktails)
summary(lm2)
tidy(lm2)
# Add ONLY the interaction of predictors: <outcome variable> ~ <predictor 1> : <predictor 2>
lm3 <- lm(abv ~ acid : sugar, data = cocktails)
summary(lm3)
tidy(lm3)

# Get the confidence intervals for parameters
confint(lm1, level = 0.95)
# You can combine these
# R can also deal with categorical variables, as they are automatically dummied, and the first level is taken as baseline
lm(abv ~ acid : sugar + type, data = cocktails) %>% summary()
# To change the baseline, convert it to random, and use the levels to set the baseline to carbonated
# Use the forcats::fct_relevel() function
lm(abv ~ acid : sugar + fct_relevel(type, "carbonated"), data = cocktails) %>% summary()

## Model selection
# You can compare models if you use the same data, and the same approach to get the regression line
# There are 3 widely-used metrics, all provided in broom::glance()
# All of them have the similar underlying principle? 
# You should go for the smallest logLik, AIC, and BIC with the same df

glance(lm1)
glance(lm2)
glance(lm3)

# You can also compare the logLik models using the anova() function. It returns an F value, which is significant if difference.
anova(lm1, lm3)
# This tells us that the more complicated model is not significantly better, so we should not use it

# You can have more then 2 models, and the comparison refers to the _PREVIOUS_ model (so not the baseline). Pair-wise comparisons are thus preferable
anova(lm1, lm2, lm3)

# Based on the comparisons, there is no significant diffference. So we should choose the simplest model, that has the smallest df! It is model number 3!

# To report the results of regression, you have to use a table, according to APA6. To create such a table, the easiest is to use the stargazer package, that collects all information from the models, and creates a nice table.
install.packages("stargazer")
library(stargazer)

# To get the table in the console, use the type = "text" argument.
stargazer(lm1, lm2, title = "Results", align = TRUE, type = "text")

# You can also have the table in different formats, e.g. html. If you do this, you can save the object and view the results using your web browser. We will later learn a way to include those tables to your manuscripts.

results_table_html <-
    stargazer(lm1,
              lm2,
              lm3,
              title = "Model comparison",
              dep.var.labels = "Alcohol content",
              align = TRUE,
              ci = TRUE,
              df = TRUE,
              # keep.summary.stat = c("aic","bic","ll","f","rsq","adj.rsq","n","res.dev","chi2"),
              type = "html")

# You can save the results using the write_lines() function
write_lines(results_table_html, "results_table.html")

