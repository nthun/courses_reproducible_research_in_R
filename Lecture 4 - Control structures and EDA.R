# Lecture 5 - Select helpers, and exercises
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
titanic_train %>% names
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


# Variants of mutate can change several variables in the same time. They can be used with or without grouping
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

# summarise_if summarises variables that fullfill a predicament. 
titanic_train %>% 
    group_by(Embarked) %>%
    summarise_if(.predicate = is.integer, .funs = funs(mean_t2 = mean(., na.rm = T)*2))

### PRACTICE ON TITANIC DATA
# Solution for Titanic question 2
titanic_train %>% 
    group_by(Pclass, Sex, Survived) %>% 
    ggplot() +
    aes(x = Pclass %>% as.factor(), fill = Sex) +
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



