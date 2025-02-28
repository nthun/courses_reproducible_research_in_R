# Lecture 7 - Covariation, correlaion, joining
# Load these packages
library(tidyverse)
library(broom)
library(forcats)
library(stringr)

# Read the cocktail dataset from my github repo
cocktails <- read_tsv("https://raw.github.com/nthun/cocktail-balance/master/cocktail_data.tsv")
cocktails <- read_tsv("http://bit.ly/2zbj7kA") # Same stuff, but shortened url

# Let's examine how alcohol content(abv), acid content(acid) are associated in the data.
# As a bonus, color the data points by type, and also write the names

# Task solution
cocktails %>% 
    ggplot() +
    aes(x = acid, y = abv, label = name) +
    geom_point(aes(color = type), size = 3) +
    geom_smooth(method = "lm")

# Generate data for plots
set.seed(1) # Sets up the random seed number
rand_num <- rep(1:5*10, 20) # Creates 20 repetitions of the numbers 10 to 50 with 10 increments
# Use random distributions for showing different correlation directions and magnitudes
corr_df <- 
    data_frame(x = rnorm(length(rand_num), rand_num, 5), 
               positive = rnorm(length(rand_num), rand_num, 10), 
               group = rand_num,
               negative = -positive + 50,
               no_correlation = rnorm(length(rand_num), 25, 25),
               weak_correlation = x + rnorm(length(x), 20, 40),
               moderate_correlation = x + rnorm(length(x), 12, 25),
               strong_correlation = positive) %>% 
    gather(key, value, -x, -group) %>% 
    mutate(key = fct_relevel(key, "positive","negative","no_correlation","weak_correlation","moderate_correlation","strong_correlation"))

# Plot correlations of different direction
corr_df %>% 
    filter(key %in% c("positive","negative","no_correlation")) %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~key)

# Plot correlations of different magnitude
corr_df %>% 
    filter(str_detect(key, "_correlation")) %>% 
    ggplot() + 
    aes(x = x, y = value) +
    geom_point(alpha = .4) +
    geom_smooth(method = "lm", color = "red", size = 1.5) +
    facet_wrap(~key, scales = "free_y")

# It is possible that a different variable is responsible for the correlation, 
corr_df %>% 
    filter(key == "positive") %>% 
    ggplot() + 
        aes(x = x, y = value) +
        geom_point(alpha = .4) +
        facet_wrap(~group, ncol = 5) +
        geom_smooth(method = "lm", color = "red", size = 1.5)

# Plotting the meaning of covariance
if (!require(ggrepel)) install.packages("ggrepel")
library(ggrepel) # This is used to plot non-overlapping text labels
cocktails %>% 
    select(index, name, abv, sugar) %>% 
    head(10) %>% 
    gather(property, value, abv:sugar) %>% 
    group_by(property) %>% 
    mutate(mean_value = mean(value)) %>% 
    ggplot() +
        aes(x = index, y = value, label = name) +
        geom_point(size = 3) +
        geom_hline(aes(yintercept = mean_value), size = 1.5, alpha = .5) +
        geom_segment(aes(xend = index, yend = mean_value), linetype = "dashed", color = "red", size = 1.2) +
        geom_text_repel() +
        facet_wrap(~property, nrow = 2, scales = "free_y")

## Correlation in R

# The simplest way is the built in cor() function
cor(cocktails$abv, cocktails$sugar)

# To get a correlation matrix, simly do it on a df
# You can set the method as pearson, spearman, or kendall correlation
cocktails %>% 
    select(abv:sugar) %>% 
    cor(method = "spearman")

# But to get p values, you need to run cor.test(). This can only be done in pairs of variables.
# This returns a more verbose output
cor.test(cocktails$abv, cocktails$sugar)

# Calculate the correlations in the tidyverse way, you have to use summarise. But you have to reshape the data first
cocktails %>% 
    select(name:sugar) %>% 
    gather(property, value, -name, -abv) %>% 
    group_by(property) %>% 
    summarise(r = cor(x = abv, y = value) %>% round(2),
              p = cor.test(x = abv, y = value, method = "pearson")$p.value)

# To get significance of the values in the matrix, we should use the psych paclage. corr.test() returns correlations, sample size, and p-values. It can also correct the significance for multipe comparisions, and calculate confidence intervals. 
if (!require(psych)) install.packages("psych")
library(psych)
cocktails %>% 
    select(abv:sugar) %>% 
    corr.test()

# Moreover, it can also correct the significance for family-wise error, or return CI-s
cocktails %>% 
    select(abv:sugar) %>% 
    corr.test(adjust = "bonferroni") %>% 
    print(short = FALSE)

# Check normality
shapiro.test(cocktails$abv)

# psych::cor.ci returns bootstrapped confidence intervals
cocktails %>% 
    select(abv:sugar) %>% 
    cor.ci(method = "pearson", n.iter = 1000) # Specify number of iterations

# Visualize correlation matrices. Best to use the GGally package.
# By default, ggpairs() shows scatter plots (all variables by all variables), density plots, and the actual correlation values. You can also add several features, check: https://ggobi.github.io/ggally/
if (!require(GGally)) install.packages("GGally")
library(GGally)

cocktails %>% 
    select(abv:sugar) %>% 
    ggpairs(lower = list(continuous = "smooth", color = "red"))

## Normality assumption
# Checking normality     
x <- data_frame(x = rnorm(10000, 0, 1)) # generate 10,000 random numbers
ggplot(x) + geom_histogram(aes(x))
ggplot(x) + geom_density(aes(x), fill = "grey40")
ggplot(x) + geom_qq(aes(sample = x))

# Plot about different distributions
distrib <- 
    tibble( normal = rnorm(4000, 100, 15),
            poisson = rpois(4000, 8),
            uniform = runif(4000, 0, 100),
            exponential = rexp(4000, 2),
            log_normal = rlnorm(4000, 0, 1),
            chi_square = rchisq(4000, 10, ncp = 0)) %>% 
    gather(distrib, value)

# Histogram
distrib %>% 
    ggplot() +
    aes(value) +
    geom_histogram(bins = 15) +
    facet_wrap(~distrib, scales = "free")

# Density
distrib %>% 
    ggplot() +
    aes(value) +
    geom_density(fill = "grey50") +
    facet_wrap(~distrib, scales = "free")

# qq plot
distrib %>% 
    ggplot() +
    aes(sample = value) +
    geom_qq_line(linetype = "dashed", color = "red") +
    geom_qq() +
    facet_wrap(~distrib, scales = "free")

# Transform the data
cocktails_trans <- 
    cocktails %>% 
    mutate(abv = abv %>% log())

# Check normality plots
ggplot(cocktails_trans) + geom_histogram(aes(x = abv), bins = 20)
ggplot(cocktails_trans) + geom_density(aes(x = abv), fill = "grey40")
ggplot(cocktails_trans) + geom_qq(aes(sample = abv))

cocktails %>% 
    pull(abv) %>% 
    shapiro.test()

# Do it on the tidyverse way
# Use do if you don't want to cram your results into one predefined variable. This way, you will get a nested dataframe
# Moreover, broom::tidy helps you to uniformize outputs, and put them in a data frame
# You can also create nested data frames, in which your cells will contain data frames
# You can unnest them using the unnest() function
cocktails %>% 
    do(sw = shapiro.test(.$abv) %>% tidy()) %>% 
    unnest(sw)

# This methods shows its real power when you do it on several variables.
# To do that, first you have to put your data into long format using gather()
cocktails %>% 
    gather(key, value, abv:sugar) %>% 
    group_by(key) %>% 
    do(sw = shapiro.test(.$value) %>% tidy()) %>% 
    unnest(sw)

# Compare correlations using psych::paired.r()
paired.r(-.47, -.67, n = 55)

# If you provide n2, you can specify the sample size for the variables independently
paired.r(-.47, -.67, n = 55, n2 = 550)

# Task
# Log transform the abv variable
# Check if it is now normally distributed
# (HINT: you can pull out one variable from your dataset using  df %>% pull(<variable name>)
#     Correlate the transformed variable to acid in an appropriate way
#     Write a sentence to summarise the results
#     Is the resulting correlation significantly different from abv and sugar correlation (-.47)?
        
cocktails2 <- 
cocktails %>% 
    mutate(abv_log = log(abv))

cocktails2 %>% 
    pull(abv_log) %>% 
    shapiro.test()

cor(cocktails2$acid, cocktails2$abv_log)
paired.r(-.47, -0.6596942, n = 55)

cor.test(cocktails2$acid, cocktails2$abv_log)

### Joining datasets
# download and load the nycflights13 package
install.packages("nycflights13")
library(nycflights13)
# it contains 5 dataframes
airlines
airports
flights
planes
weather

flights %>% 
    select(flight, carrier)

flights %>% 
    left_join(airlines, by = "carrier") %>% 
    select(flight, name)

# flights connects to planes via a single variable, tailnum.
# flights connects to airlines through the carrier variable.
# flights connects to airports in two ways: via the origin and dest variables.
# flights connects to weather via origin (the location), and year, month, day and hour (the time).

# Make a variables that shows only important data
flights2 <- flights %>% 
    select(year:day, hour, origin, dest, tailnum, carrier)

planes2 <- 
    planes %>% rename(plane_year = year)

# An inner join matches pairs of observations whenever their keys are equal, and discards all other observations
# This code keeps only planes that flew in July
flights2 %>%
    filter(month == 7) %>% 
    inner_join(planes2, by = "tailnum")

# A full join keeps all observations in x and y
# This code adds all the planes in the dataset, even those that did not fly in July
flights2 %>%
    filter(month == 7) %>% 
    full_join(planes2, by = "tailnum")

# Left join keeps all observations in x, and adds columns from y AND observations that match the key from y.
flights2 %>%
    left_join(airlines, by = "carrier")

# Right join keeps all observations in y, and adds columns from x AND observations that match the key from x.
flights2 %>%
    right_join(airlines, by = "carrier")

# Task:
# Which planes with more than 200 seats flew when there was a wind with a speed larger than 40 mph?
# Get the tail number of these planes
# SOLUTION: you have to join 3 datasets (weather, flights, planes)
weather %>% 
    filter(wind_speed > 40) %>% 
    inner_join(flights2) %>% 
    left_join(planes2 %>% filter(seats > 200), by = "tailnum") %>% 
    distinct(tailnum) %>% 
    drop_na() %>% 
    pull()

# What is the name of the airlines with the most planes in this set of data?
weather %>% 
    filter(wind_speed > 40) %>% 
    inner_join(flights2) %>% 
    left_join(planes2 %>% filter(seats > 200), by = "tailnum") %>% 
    inner_join(airlines, by = "carrier") %>% count(name, sort = TRUE)



