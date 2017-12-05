# Lecture 6: Writing functions, using purrr, style guide
library(tidyverse)
# Loops vs. vectorised functions

# Let's make a data frame
df <- tibble(x = seq(from = 1, to = 10000, by = 10),
             y = seq(from = 1, to = 2000, by = 2),
             z = 1:1000)

# Let's calculte the standard deviation of the columns 
sd(df$x)
sd(df$y)
sd(df$z)

# The rule of thumb is that you create an iteration if you have to do the same thing more than twice
# So, let'a do the same thing using a loop
# 1. Make an empty variable that can contain the results later
output <- NULL 
# Now create a loop, and define the sequence that it has to iterate through using the index variable (i)

output <- c()
for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
}
output

# We can do the same in one line, using the purrr::map() function

map(df, ~sd(.x))
# First, we define the object to iterate through. We can refer to the elements of this object as .x
# Than we define the function to use on the elements of the object after a ~
# map() always returns a list.

# However, you can define the output format, for e.g. map_dbl() returns a vector of real numbers
map_dbl(df, ~sd(.x))

# map_lgl() makes a logical vector.
# map_int() makes an integer vector.
# map_chr() makes a character vector.
# These are strict functions, they don't allow different data formats into the vector, so this will fail:
map_int(df, ~ sd(.x))

# Another example: read all files in the working directory to a list
# First, do this with a for loop
files <- list()
for (i in list.files()){
    files[[i]] <- read_file(i)
}
files

# Let's do the same with map()
files <- map(list.files(), ~read_file(.x))

# Let's run some linear regression models on data subsets
mtcars %>%
    group_by(cyl) %>%
    nest() %>% # Put the data groups in separate nested dataframes withint the dataframe
    mutate(r_squared = map(data,
            ~ lm(mpg ~ disp, data = .x) %>%
            broom::glance() %>% # Tidy up model performance statistics
            pull(r.squared) # Get r squared values separately for each nested data frame
    )) 

# Shortcuts:
# For nested lists, you can also provide an element name and that element will be pulled  from all sublists
abc <- list(c(a = letters[1], b = letters[10], b = letters[20]))
map_chr(abc, "b")

# For nested lists or vectors, you can also provide an index number and that element will be pulled
x <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
map_dbl(x, 2)

# map2()
# Generate 3 x 10 random numbers with different mean and sd
map2(list(1,2,3), list(.1,.2,.3), ~rnorm(10, .x, .y))

# walk2() to save multiple plots
# First create the plots and file names
cyl_plots <- 
    mtcars %>% 
    group_by(cyl) %>% 
    nest() %>% 
    mutate(plot = map(data, ~ggplot(.x) +
                    aes(x = disp, y = mpg) +
                    geom_point()),
           file_name = paste0("plot_cyl_", cyl,".jpg")
           )

# Check one plot
cyl_plots %>% 
    slice(1) %>% # Get the first row
    pull(plot)

# Save plots using walk, by using the plot and file names
walk2(cyl_plots$plot, cyl_plots$file_name, ~ggsave(.x, file = .y))

# pmap()
# Generate random numbers with different number of numbers, mean, and sd
ls <- list(c(10, 20, 30), c(1,2,3), c(.1,.2,.3))
pmap(ls, ~ rnorm(..1, ..2, ..3))

# Dealing with failure
# safely() modifies the original function to provide an error and a result
safe_sqrt <- safely(sqrt)
# This produces the same result as sqrt, and there is no error
safe_sqrt(100)

# This produces an error an no result, because the input is a character
safe_sqrt("a")

# Using with map() makes it really useful
out <- map(list(1, 10, "a"), ~safe_sqrt(.x))

# To get the results and errors together, use transpose()
out %>% transpose()

# possibly() is similar to safely() because it always succeeds but instead of returning the error, it returns a default value that you can set using the otherwise parameter
poss_sqrt <- possibly(sqrt, otherwise = NA_real_)

# So this return NA instead of failing when getting "a"
map_dbl(list(1, 10, "a",100), ~poss_sqrt(.x))

# quietly() is similar to safely() but instead of errors, it returns a list of result, printed output, warnings, and messages
map(list(1, -1), quietly(log))

map(list("foo", "bar"), quietly(print))


mean_rm <- function(x, ...){
    mean(x, ...)
}
x <- c(1:10, NA_integer_)
mean_rm(x, na.rm = T)    
    
abc <- letters[1:20]
str_concat <- function(..., sep = "_"){
    paste(..., collapse = sep)
}
str_concat(abc)


