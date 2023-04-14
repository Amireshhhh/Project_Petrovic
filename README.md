# Project_Petrovic

1- Null values --> 1- remove all 2- remove some!?



# Function
# load the bnlearn package for ci.test function
library(bnlearn)

# define a function that takes a dataframe of returns, a type of test and a fixed variable
# it returns a dataframe with p-values and correlations for every pair of stocks given the fixed variable
ci_test_pairs <- function(df, test, fixed) {
  # get the names of the columns that are not the fixed variable
  stocks <- names(df)[names(df) != fixed]
  # create an empty dataframe to store the results
  results <- data.frame()
  # loop over all pairs of stocks
  for (i in 1:(length(stocks) - 1)) {
    for (j in (i + 1):length(stocks)) {
      # get the names of the pair
      x <- stocks[i]
      y <- stocks[j]
      # perform the conditional independence test given the fixed variable
      test_result <- ci.test(x, y, fixed, data = df, test = test)
      # extract the p-value and the correlation from the test result
      p_value <- test_result$p.value
      correlation <- cor(df[[x]], df[[y]])
      # append a row to the results dataframe with the pair names, p-value and correlation
      results <- rbind(results, data.frame(x = x, y = y, p_value = p_value, correlation = correlation))
    }
  }
  # return the results dataframe
  return(results)
}
# apply the function with linear correlation test and DXY as fixed variable
results <- ci_test_pairs(returns_data, test = "cor", fixed = "DXY")

# print the results
print(results)


Guys i was able to create the function. Let me know Amir if this is what you wanted?
