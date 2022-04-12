library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)

set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's


housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

## Create a new recipe()
housing_recipe <- recipe(medv ~ ., data = housing) %>% # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox 
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b, lstat,dis,nox, degree=6) # create square terms of some continuous variables


housing_prep <- housing_recipe %>% prep(housing_train , retain=T)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv) 
housing_test_x <- housing_test_prepped %>% select(-medv) 
housing_train_y <- housing_train_prepped %>% select( medv) 
housing_test_y <- housing_test_prepped %>% select( medv)

# What is the dimension of your training data (housing_train)?
dim(housing_train_x)

# How many more X variables do you have than in the original housing data?
dim(housing_train_x)[2] - dim(housing)[2] -1 ## exclude the y

## LASSO 
################

# now do lasso where we set the penalty
tune_spec <- linear_reg(penalty=0.5,mixture=1) %>%       # Specify a model
  set_engine("glmnet") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 10-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 10)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%
#add_recipe(housing_recipe)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print


top_rmse %>% print(n = 1)

## RIDGE 
################

# now do lasso where we set the penalty
tune_spec <- linear_reg(penalty=0.5,mixture=0) %>%       # Specify a model
  set_engine("glmnet") %>%   # Specify an engine: lm, glmnet, stan, keras, spark
  set_mode("regression") # Declare a mode: regression or classification

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 10-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 10)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%
#add_recipe(housing_recipe)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print


top_rmse %>% print(n = 1)



