# Lecture 5 - Select helpers, control flow, and EDA
library(tidyverse)

# Let's use the titanic dataset
install.packages("titanic")
library(titanic)

# Check the codebook to identify variables
?titanic_train

# Solution for Titanic question 1
titanic_train %>% 
    filter(Age == 50 & Sex == "female" & Pclass == 1, SibSp + Parch == 0)

### Selecting variables from a dataset
# Check names
titanic_train %>% names()
# Transform to tibble to be able to see a more compact dataset on the console
titanic_train <- titanic_train %>% as_tibble()

# Select variables by name
titanic_train %>% select(PassengerId, Sex, Fare) 

# Select by position (not a good practice)
titanic_train %>% select(1, 4, 6)
titanic_train %>% select(1:6)

# Using a select helper function (most common examples)
titanic_train %>% select(starts_with("p", ignore.case = TRUE)) 
titanic_train %>% select(ends_with("e")) 
# Using `-` to remove variables. E.g. remove variables that don't have the "ar" string
titanic_train %>% select(-contains("ar")) 

# If you want to select a variable that is not in the dataset, you will get an error
titanic_train %>% select(Ticket, Gender, Embarked) 

# But with a helper function, you can safely look for a variable, it will not result an error (only a warning)
titanic_train %>% select(one_of("Ticket", "Gender", "Embarked"))

# Using select to rename variables
titanic_train %>% select(id = 1, name = 4, gender = 5)
# But this will also remove all other variables, so we need to add all other variables with helper function
titanic_train %>% select(id = 1, name = 4, gender = 5, everything())

# You can also use rename that keeps all variables by default, but can't use select helpers
titanic_train %>% rename(gender = Sex)

# It is also possible to batch rename with a select helper:
titanic_train %>% select(var_ = starts_with("P"), everything()) 

### Recoding variables
# Using case_when
titanic_train %>% 
    mutate(age_group = case_when(Age <= 14 ~ "0-14",
                                 Age >= 15 & Age <= 21 ~ "15-21",
                                 Age >= 22 & Age <= 35 ~ "22-35",
                                 Age >= 36 & Age <= 50 ~ "36-50",
                                 Age >= 50 & Age <= 63 ~ "50-63",
                                 Age >= 64 ~ "63+",
                                 TRUE ~ NA_character_)) %>% 
    select(Age, age_group) # Check if results are ok

### Variants of mutate can change several variables in the same time. They can be used with or without grouping
titanic_train %>% 
    mutate_all(.funs = funs(as.character(.)))

# mutate_at() applies a function to all variables
titanic_train %>% 
    mutate_at(.funs = funs(as.factor(.)), .vars = vars(Sex, Embarked))

# mutate_if() applies a function to all variables that fulfill a the predicament

titanic_train %>% 
    mutate_if(.predicate = is.character, .funs = funs(stringr::str_to_lower(.)))

# Let's see how this works when you make a calculation.
# Note that without defining groups, the variables will be overwritten with summary stats, like in the next example. 

titanic_train %>% 
    mutate_if(.predicate = is.integer, .funs = funs(mean(.))) 

# If you define groups, the stats will refer to the groups
# You can also give a name to the new variable in fun(), that will become a postfix.
# Note that the predicate don't have the () after the function name

titanic_train %>% 
    group_by(Sex) %>% 
    mutate_if(.predicate = is.integer, .funs = funs(mean = mean(.))) 

# Summarise functions  
# Can be used when you have too many variables and you don't want to list them all. You will get the results in "wide format". You can define groups too, so it is easy to make data summaries

# summarise_all() creates a separate summary for all variables. Moreover, you can execute several functions at the same time, that will be (without further naming) postfixes. But you can also name them similarly to the example for mutate_if().
titanic_train %>% 
    select(Sex, PassengerId:Pclass) %>% 
    group_by(Sex) %>%     
    summarise_all(.funs = funs(mean(.), sd(.), n()))

# summarise_at()
# Note that the function parameters (such as na.rm = T) can be passed to the functions from outside of the funs(), if you want all functions to take it as a paramater. Of course, you can define the parameter for all functions separately e.g. funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE))
titanic_train %>% 
    group_by(Sex, Embarked) %>% 
    summarise_at(.funs = funs(mean(.), sd(.)), na.rm = TRUE, .vars = vars(Age, Fare))

# summarise_if summarises variables that fulfill a predicament. 
titanic_train %>% 
    group_by(Embarked) %>%
    summarise_if(.predicate = is.integer, .funs = funs(mean_t2 = mean(., na.rm = T)*2))

### PRACTICE ON TITANIC DATA
# Solution for Titanic question 2
titanic_train %>% 
    group_by(Pclass, Sex, Survived) %>% 
    ggplot() +
    aes(x = Pclass, fill = Sex) +
    geom_bar(position = "dodge") +
    facet_wrap(~Survived)

# Solution for Titanic question 3
titanic_df <-
    titanic_train %>% 
    as_tibble() %>% 
    filter(Parch == 0 & SibSp == 0) %>% 
    select(PassengerId, Sex, Age, Pclass, Fare, Survived) %>% 
    drop_na() %>% 
    group_by(Pclass) %>% 
    mutate(med_price = median(Fare)) %>% 
    ungroup()


### Control structures
# Conditional statement if()
# You can use this to execute functions only if certain 

x <- 5

if(x <= 4) {print("smaller or equal than 4")}
if(x>4){
    print("larger than 4")
    x/2
    }

# This can be useful for e.g. to create a directory only when it does not exist
if(dir.exists("test_dir") == FALSE) dir.create("test_dir")

## Iteration
# for() is used to iterate over indexes, for e.g. when you want to create and write several plots 
for (i in 1:10) print(i)

# You have to setup the variable that will contain the results if you want to store values
plots <- list()

# Then you run the iteration, where you can refer to the index variable
# Let's make separate plots
for (i in unique(titanic_train$Embarked)){
plots[[i]] <-
    titanic_train %>% 
    filter(Embarked == i) %>%
    ggplot() +
    aes(x = Age, y = SibSp + Parch) +
    geom_point()
}

### LISTS
# We have a list object, with 4 different plots
# The names of the list elements are the same as the unique values of the Embark variable in titanic_train
plots %>% names()

# We got 4 separate plots, that you can see by typing the element name.
plots[1]
plots[2]
plots[3]
plots[[4]]

# We can also refer to them by name, if the list is named (mind the quote)
plots[["Q"]]

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

# Indexing a list can be a bit difficult first. There are 3 ways of subsetting a list
# [ returns a sub-list. The result will always be a list
a[1:2] %>% str()

# [[ returns a single component of a list. It removes the level of hierarchy
a[[1]] %>% str()
a[[4]] %>% str() # This remains a list, because this was a list in a list

# You can use $ as a shorthand for extracting named elements
a$a
# is the same as
a[["a"]]

# Let's write these plots to 4 separate files to the test_dir subdirectory

for (i in seq_along(plots)){
    ggsave(plot = plots[[i]], filename = paste0("test_dir/plot_",i,".jpg"))
}

### Exploratory data analysis
# To examine the distribution of a categorical variable, use a bar chart
?diamonds

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut))

# See the number of observations grouped by the cut variable
diamonds %>% 
    count(cut)

# To examine the distribution of a continuous variable, use a histogram
ggplot(data = diamonds) +
    geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# Typical values
summary(diamonds$carat)
ggplot(diamonds) +
    geom_density(aes(x = carat), fill = "red")

# Unusual values
ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# To make it easy to see the unusual values, we need to zoom to small values of the y-axis with coord_cartesian()
ggplot(diamonds) + 
    geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
    coord_cartesian(ylim = c(0, 50))

# Let's save the unusual values that are smaller than 3 and larger than 20
unusual <- 
    diamonds %>% 
    filter(y < 3 | y > 20) %>% 
    select(price, x, y, z) %>%
    arrange(y)
# Let's check the unusual values
unusual

# Instead of excluding the variables entirely, let's just make the outlier values NA
# This way, you keep the other values of the observation, that may not be invalid. Also, you don't have to make note somewhere else that you actually had to remove cases. This information remains in your data.
diamonds2 <- diamonds %>% 
    mutate(y = if_else(y < 3 | y > 20, NA_real_, y))

# Missing values will be always shown as a warning
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
    geom_point()

### COVARIATION
# It is possible to see differing distributions in a continuous vaiable for separate categorical levels
# To check  frequency distribution of price by cut, we can use frequency polygons (similar to histograms)
ggplot(data = diamonds) + 
    aes(x = price) +
    geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# Or you can choose to use density plots, those show relative frequencies (but the purpose is similar)
ggplot(data = diamonds) + 
    aes(x = price, fill = cut, color = cut) +
    geom_density(alpha = .3)

# However, this does not look that great, so let's check out a visualization that shows distributions in a way that is is also readable. For this, we need the ggridges package
install.packages("ggridges")
library(ggridges)
ggplot(diamonds) +
    aes(x = price, y = cut) +
    geom_density_ridges()

# You can also use a violin plot (or bean plot) for the same thing
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
    geom_violin()

# Checking the covariation of a continuous and a categorical variable
# To check the typical values along with a distribution summary in a plot, use boxplot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
    geom_boxplot()

# We can also reorder values based on the median (central line in boxplot)
ggplot(data = diamonds, mapping = aes(x = reorder(cut, price, FUN = median), y = price)) +
    geom_boxplot()

# Two categorical variables
# Comparing how color and cut covary, we can simply calculate the number of cases
diamonds %>% 
    count(color, cut)

# But we are better off if we also visualize the results 
# For e.g. we can use the count plot. This shows the number of cases by size of the dots
ggplot(data = diamonds) +
    geom_count(mapping = aes(x = cut, y = color))

# We can also make a heatmap, where we visualize the frequency of cases as color density
# In this example, darker colors mean less and brither colors mean more cases
diamonds %>% 
    count(color, cut) %>%  
    ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

# Two continuous variables
# The most obvious visualization is the simple scatter plot
ggplot(data = diamonds) +
    geom_point(mapping = aes(x = carat, y = price))

# Or by binning the data points that are close to each other
ggplot(diamonds) +
    geom_bin2d(mapping = aes(x = carat, y = price))

# You can also use hexagon bins instead of rectangles
ggplot(diamonds) +
    geom_hex(mapping = aes(x = carat, y = price))



