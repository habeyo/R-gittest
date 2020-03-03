##########################################################################################
#
#   Program Name: Week_2_Tutorial.R
#   Purpose: Data analysis in R
#   Author: ...
#   Description: Tutorial exercises for data analysis in R
#   Usage: R training tutorials
#
##########################################################################################
#
#   Version 0.1      Date 27-12-2019        Description
#
##########################################################################################

### PART I ###

# Import the dataset iris
data(iris)
summary(iris)

# 1. Write a function that can calculate the sum of the squared differences between Petal.Length and Petal.Width

# 2. Use the exact same function to calculate the sum of the squared differences between Sepal.Length and Sepal.Width

# Of course both can be done in one line easily, but please use a self-written function for learning purposes

# 3. Write a function that can transform Petal.Width into a boolean vector: TRUE if Petal.Width > 0.2, FALSE otherwise

# 4. Use the same function to return a transform Petal.Length into a boolean vector: TRUE if Petal.Length > 1.5, FALSE otherwise

# 5. Try to give the column Species as input to your function. What will happen? Make sure the function returns a meaningful message when this happens

### PART II ###

# Import dataset
options(stringsAsFactors=FALSE)
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
