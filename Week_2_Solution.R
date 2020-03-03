##########################################################################################
#
#   Program Name: Week_2_Tutorial.R
#   Purpose: Data analysis in R
#   Author: Lars Quaedvlieg
#   Description: A solution to the tutorial exercises for data analysis in R
#   Usage: R training tutorials
#
##########################################################################################
#
#   Version 0.1      Date 02-01-2020        Description
#
##########################################################################################

### PART I ###

# Import the dataset iris
data(iris)
summary(iris)

# 1. Write a function that calculates the sum of the squared differences between Petal.Length and Petal.Width

#' NaiveFunction
#'
#' @param df a dataframe containing at least the variables Petal.Length and Petal.Width
#'
#' @return the sum of squared differences between Petal.Length and Petal.Width
#' @export
#'
#' @examples
NaiveFunction <- function(df){
  
  # Calculate the sum of the squared differences
  ssd <- sum((df$Petal.Length - df$Petal.Width)^2)
  
  return(ssd)
}

#' BetterFunction
#'
#' @param df a dataframe
#' @param length_name the name of a column in the dataframe
#' @param width_name the name of a column in the dataframe
#'
#' @return the sum of the squared differences between the columns length_name and width_name
#' @export
#'
#' @examples
BetterFunction <- function(df, length_name, width_name){
  
  # Calculate the sum of the squared differences
  ssd <- sum((df[,length_name] - df[,width_name])^2)
  
  return(ssd)
}

#' BestFunction
#' 
#' Best function in the sense that this is the most general function
#'
#' @param a a vector of numeric values
#' @param b a vector of numeric values
#'
#' @return the sum of the squared differences between the numeric vectors a and b
#' @export
#'
#' @examples
BestFunction <- function(a, b){
  # Calculate the sum of the squared differences
  ssd <- sum((a- b)^2)
  
  return(ssd)
}

NaiveFunction(iris)
BetterFunction(iris, "Petal.Length", "Petal.Width")
BestFunction(iris$Petal.Length, iris$Petal.Width)

# 2. Use the same function to calculate the sum of the squared differences between Sepal.Length and Sepal.Width

# The NaiveFunction cannot be used, as we have hardcoded Petal.Length and Petal.Width inside the function. 
# There is no way to specify another column but to rewrite the function

# The BetterFunction and BestFunction can be used
BetterFunction(iris, "Sepal.Length", "Sepal.Width")
BestFunction(iris$Sepal.Length, iris$Sepal.Width)

# 3. Write a function that can transform Petal.Width into a boolean vector: TRUE if Petal.Width > 0.2, FALSE otherwise

#' BooleanSplitter
#'
#' @param column numeric vector 
#' @param break_value value to split the values from `column` at
#'
#' @return boolean vector. Values greater than `break_value` are set to `TRUE` and `FALSE` otherwise
#' @export
BooleanSplitter <- function(column, break_value){
  return(column > break_value)
}

BooleanSplitter(iris$Petal.Width, break_value = 0.2)

# 4. Use the same function to return a transform Petal.Length into a boolean vector: TRUE if Petal.Length > 1.5, FALSE otherwise
BooleanSplitter(iris$Petal.Width, break_value = 1.5)

# 5. Try to give the column Species as input to your function. What will happen? Make sure the function returns a meaningful message when this happens
BooleanSplitter(iris$Species, break_value = 0)

# Before the return statement, check if the input is indeed numeric
BooleanSplitter <- function(column, break_value){
  
  if(!is.numeric(column)){
    stop("column is not numeric. Expecting numeric value")
  }
  
  return(column > break_value)
}

### PART II ###

# Import dataset
house_data_train <- read.csv("data/house_price_train.csv")

# Build a model that can predict Sale Price based on the other features (evaluates on Mean Squared Error).
# Work in teams of 2 and use git to store your code. You can split the tasks into data cleaning, preprocessing,
# exploratory analysis and building the model. You can use different branches for each task.
# Where possible/necessary, functionalize your code.

# Please create one script (normally, one would split the tasks over multiple scripts, but for the purpose of
# learning git, please use one script only(there is a higher chance to have a merge conflict)), name
# it Week_2_Tutorial_teamname.R and track the script via Git

# You can use pull requests to review each other's code

# Several options you could consider for your model:
# - lm
# - glm
# - randomForest
# - glmnet

# A possible solution is presented below. Note that this is only one solution, among many
library(glmnet)

# Show a summary of the dataset
summary(house_data_train)

# Check for each column if there are NA's in the column. If so, replace by an appropriate method

# We provide two functions that will help in executing this task

#' GetMode
#'
#' @param x a vector with values,
#' @param na.rm should NA's be removed before calculating the mode? Defaults to TRUE
#'
#' @return
#' @export
#'
#' @examples
GetMode <- function(v, na.rm=TRUE){
  # Get the unique values
  uniqv <- unique(v)
  
  # If applicable, remove NA's from the vector
  if(na.rm){
    uniqv <- uniqv[!is.na(uniqv)]
  }
  
  # Tabulate the values and return the maximum
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

#' replaceNA
#'
#' # Function to deal with NA's
#' @param x a vector to be checked for NA and potentially replaces
#' @param method method to replace NA. 'mean', 'median' and 'mode' are implement. Defaults to 'mean'
#'
#' @return
#' @export
#'
#' @examples
replaceNA <- function(x, method='mean'){
  if(any(is.na(x))){
    if(method == 'mean'){
      if(is.numeric(x)){
        replace_value <- mean(x, na.rm=TRUE)
      }
      else{
        stop('mean is only applicable to numeric columns')
      }
    }
    else if(method == 'median'){
      if(is.numeric(x)){
        replace_value <- median(x, na.rm=TRUE)
      }
      else{
        stop('median is only applicable to numeric columns')
      }
    }
    else if(method == 'mode'){
      replace_value <- GetMode(x, na.rm=TRUE)
    }
    else{
      stop('unknown method. Expecting one of ["mean", "median", "mode"]')
    }
    
    return(ifelse(is.na(x), replace_value, x))
  }
  else{
    # There is nothing to replace, simply return x
    return(x)
  }
}

# For numeric vectors, we replace NA's by the mean. For others, we use the mode
for(x in colnames(house_data_train)){
  if(is.numeric(house_data_train[,x])){
    house_data_train[,x] <- replaceNA(house_data_train[,x], method='mean')
  }
  else{
    house_data_train[,x] <- replaceNA(house_data_train[,x], method='mode')
  }
}

# Extract all numeric values and LotShape, Condition1 and Street (this selection is completely random)
house_data_train <- house_data_train[,c(names(which(sapply(colnames(house_data_train),
                                                           function(x) is.numeric(house_data_train[,x])))),
                                       "LotShape", "Condition1", "Street")]

# Create a model matrix
house_data_train_matrix <- model.matrix(~. + 0, data=house_data_train)

# Split the data into train and validation set
train_id <- sample.int(nrow(house_data_train_matrix), floor(0.8*nrow(house_data_train_matrix)))
X_train <- house_data_train_matrix[train_id, which(colnames(house_data_train_matrix) != "SalePrice")]
X_validate <- house_data_train_matrix[-train_id, which(colnames(house_data_train_matrix) != "SalePrice")]
y_train <- house_data_train_matrix[train_id, "SalePrice"]
y_validate <- house_data_train_matrix[-train_id, "SalePrice"]

# Use glmnet to build a predictor
cv_lasso_model <- cv.glmnet(X_train, y_train, nfolds = 5)

# Plot the Cross validation
plot(cv_lasso_model)

# Use the minimum lambda to build the model
lasso <- glmnet(X_train, y_train, lambda = cv_lasso_model$lambda.min)

# Calculate the MSE on the test set
y_fit <- predict(lasso, newx = X_validate)
print(paste0("MSE is equal to: ", mean((y_fit - y_validate)^2)))
