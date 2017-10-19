### Lecture 3 and 4
# Install the tidyverse package. All the packages that we will use today are included in it
install.packages("tidyverse")


# Data importing
# The easies way is to use data import from the "Files" window


# IMPORTANT!! IN ORDER TO USE THIS CODE, YOU ALSO HAVE TO DOWNLOAD THE DATAFILES FROM THE COURSE'S PAGE, THAT YOU CAN FIND IN THE DATASETS LIBRARY! https://osf.io/xcvn7/

# R has its own data format (.RData), that stores variables from R. You can load an R object using the load() function.
# Usually, it is enough to provide the file name (with the relative or absolute path), but import functions often has optional parameters
library(tidyverse)

load("datasets/movies.RData")

# You can save your variables using the save() function. It requires two parameters: the variable to save, and the filename to save to.

x <- c("apple","orange","pinaple")
save(x, file = "datasets/fruits.RData")

# For reading common file formats, we will use the readr package (from tidyverse)

library(readr)

# A very common file format in the data analysis community is .csv. It means comma separated values, which is a quite literal description. It is a simple text file, but commas distinguish variables and values from each other. You can import a csv file using

read_csv("datasets/movies.csv")

# Note that ss we did not assign the result of the function to a variable, it is not stored. So let's try

m1 <- read_csv("datasets/movies.csv")

# To save a dataset to csv format, you can just use the write_csv() function. It requires two parameters: the data frame to save, and the filename to save to.

write_csv(m1, "datasets/movies.csv")

# tsv is almost the same as csv, but tabs are used instead of commas
cocktails <- read_tsv("datasets/cocktail_data.tsv")

# Writing to tsv... yes, you have guessed it: write_tsv()
# Reading text files is also easy. read_lines() will read the file line by line, and create a vector 
# Here, check out the complete works of Shakespeare:

shakespeare <- read_lines(file = "datasets/shakespeare_all_works.txt")

# Remember that you can read files directly from the internet, if you use the url() function:
# We can also skip the first 244 lines as it only contains licence info
shakespeare <- read_lines(url("https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"), skip = 244)

# To read Excel files, we will use the readxl package
library(readxl)

# For excel files, you also have to specify the worksheet that you want to access. You can do it by sheet name or position. So the next 2 commands return the same result
read_excel(path = "datasets/cocktails.xlsx", sheet = 1)
read_excel(path = "datasets/cocktails.xlsx", sheet = "Sheet 1")

# You can also read SPSS (and SAS, etc.) formats by using the haven package
library(haven)

read_spss("datasets/movies.sav")


### THE PIPE OPERATOR
# Let's try the pipe operator
# The pipe is in several packages, for e.g. the magrittr package
library(magrittr)

# Take the following vector
x <- c(55:120, 984, 552, 17, 650)

# Creating a pipeline of commands. Of course, the sorting does not change the result
x %>%
    sort() %>%
    subtract(5) %>%
    divide_by(3) %>%
    sd()


# [1] 46.02999
# Let's load the dplyr package, which is for data transformation in data frames
library(dplyr)

# Let's use the ToothGrowth data once again, and practice with the pipe opeartor
# Filter the rows that 
ToothGrowth %>%
    filter(supp == "OJ")

# Let's creare a new variable, which which returns tooth length in cm, not mm. Use mutate()
ToothGrowth %>%
    mutate(len_cm = len / 10)

# Let's see the average tooth length in centimeters, use summarise()
# This takes several data and summarizes it accoring to our wishes
# The result will not contain the original data
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    summarise(mean_len_cm = mean(len_cm))

# Let's also calculate the nuber of cases, using the function n()
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    summarise(mean_len_cm = mean(len_cm),
              cases = n())

# It makes the most sense if we also create groups but BEFORE summarise, using group_by()
tooth_results <-
    ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    group_by(supp) %>%
    summarise(mean_len_cm = mean(len_cm),
              cases = n())

# You ca also use the grouping with mutate. Then it adds the group means and number of cases to the original data
# This way, the result will also contain the original data AND the summary statistics with redundancy
ToothGrowth %>%
    mutate(len_cm = len / 10) %>%
    group_by(supp) %>%
    mutate(mean_len_cm = mean(len_cm),
           cases = n())

# We can also arrange the results based on a variable
tooth_results %>% 
    arrange(mean_len_cm)


# Practice on the gapminder data. First install it, than load the data 
install.packages("gapminder")
gapminder <- gapminder::gapminder

# Gapminder contains the data of several countries at tifferent times.
# It is in tidy format. Check the codebook
?gapminder

# Task 1 solution
solution_1 <-
    gapminder %>% 
    filter(year %in% c(1952, 1957, 1962)) %>%
    group_by(continent) %>% 
    summarise(life_exp_med = median(lifeExp)) %>% 
    arrange(-life_exp_med)

# Task 1 data viz   
library(ggplot2)
solution_1 %>% 
    ggplot() +
        aes(x = continent, y = life_exp_med) +
        geom_col()

# Task 2 solution
solution_2 <-
    gapminder %>% 
    filter(country %in% c("Hungary","Slovak Republic","Austria")) %>% 
    group_by(country) %>% 
    mutate(mean_pop = mean(pop),
           cent_pop = pop - mean_pop)

solution_2 %>% 
    ggplot() +
        aes(x = year, y = cent_pop, group = country, color = country) +
        geom_line(size = 1.5) +
        geom_hline(yintercept = 0) +
        scale_y_continuous()
    
### Data types
# R has so called atomic data types
# Numeric data is either integer (round numbers > 0), or numbers with decimals
    
# SEE SLIDES

### Cleaning datasets
library(tidyr)
library(tibble)
# We will use the who data from the tidyr package
# Check the codebook
data(who)
?who

# gather arranges data to long format
# you have to give a name that will store

who_long <- 
    who %>% 
    gather(key = variable, value = value, new_sp_m014:newrel_f65)

# You can see a lot of missing values (NA) that you can easily remove
who_long <- 
    who_long %>% 
    drop_na(value)

# According to the codebook, there are several things encoded in these variables, that is not tidy
# For example, ˙new_` in the vairable name does not contain information, so let's remove it
# To make operations on strings, let's use the stringr package, also from the tidyverse

library(stringr)

who_long %>% 
    mutate(variable = str_replace(variable, "new_",""))

# This way, the variable contains 3 different information: test result, gender, and age
# Let's separate the test result first

who_long %>% 
    mutate(variable = str_replace(variable, "new_","")) %>% 
    separate(col = variable, into = c("test_result","gender_age"), sep = "_")

# We still need to separate the gender from the age
who_tidy <-
    who_long %>% 
    mutate(variable = str_replace(variable, "new_","")) %>% 
    separate(col = variable, into = c("test_result","gender_age"), sep = "_") %>% 
    mutate(gender = gender_age %>% substring(1,1),
           age = gender_age %>% substring(2))

# Now we can verify what age groups we have
who_tidy %>% 
    distinct(age)

# We can also transform the data to wide format, for e.g. the age groups. 
who_tidy %>% 
    spread(age, value)
