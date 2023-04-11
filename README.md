# Project_Petrovic

1- Null values --> 1- remove all 2- remove some!?



****Function****
conditional_independence_test <- function(AAPL, XLU, FTSE, XLV, XLF, XLP, XLK, QQQ, XLE, HSI) {
  
  fixed_var <- returns_data$DXY
  AAPL <- returns_data$AAPL
  XLU <- returns_data$XLU
  FTSE <- returns_data$FTSE
  XLV <- returns_data$XLV
  XLF <- returns_data$XLF
  XLP <- returns_data$XLP
  XLK <- returns_data$XLK
  QQQ <- returns_data$QQQ
  XLE <- returns_data$XLE
  HSI <- returns_data$HSI
  # create a list of all possible pairwise combinations of variables
  variable_pairs <- combn(c(AAPL, XLU, FTSE, XLV, XLF, XLP, XLK, QQQ, XLE, HSI), 2)
  
  # loop through each variable pair and compute the conditional independence test
  for (i in 1:ncol(variable_pairs)) {
    var1 <- variable_pairs[1,i]
    var2 <- variable_pairs[2,i]
    
    # create a data frame with the two variables and the fixed variable
    data <- data.frame(var1, var2, fixed_var)
    
    # perform the conditional independence test using the ci.test function
    result <- ci.test(var1 ~ var2 | fixed_var, data = returns_data)
    
    # print the result, including the variable names and p-value
    cat(paste0("Conditional independence test between ", var1, " and ", var2, " (given ", fixed_var, "):\n"))
    cat(paste0("p-value = ", result$p.value, "\n\n"))
  }
}

I do not know whats wrong but the function is not working for me I tried. I hope someone else can figure out whats wrong.
