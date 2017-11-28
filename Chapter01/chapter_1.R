
regression_results <- lm(formula = Sepal.Length ~ Species, data = iris )
mode(x = regression_results)

first_vector                 <- c("a","b","c")
second_vector                <- c(1,2,3) 
vector_list                  <- list(first_vector, second_vector)
vector_list[[2]]
a_data_frame                 <- data.frame(first_attribute = c("alpha","beta","gamma"), second_attribute = c(14,20,11))
a_data_frame$second_attribute
a_data_frame$third_attribute <- c(TRUE,FALSE,FALSE)

summing_two <- function(the_number){
  the_number + 2}

my_func <- function(x){
  function_result <- x / 2 
  return(function_result)}

my_func(4)
function_result
install.packages("profvis")
profvis::profvis({lm(formula = Sepal.Length ~ Species, data = iris )})
