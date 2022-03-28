library(tidyverse)
library(modelsummary)
library(mice)
library(flextable)

# Using R or Python, load the file wages.csv
wage_data <- read_csv("wages.csv")

# 5. Drop observations where either hgc or tenure are missing

wage_data <- wage_data %>% filter( !(is.na(hgc) | is.na(tenure)) )

# 6. Use modelsummary to produce a summary table of this data frame.
datasummary_skim(wage_data)


# At what rate are log wages missing? Do you think the logwage variable is most likely
# to be MCAR, MAR, or MNAR?

# I think it's MAR as we only lost few observations and we can use the existing covariates to
# impute them

# 7. Perform the following imputation methods for missing logwage observations. 

## complete cases
wage_data0 <- wage_data[!is.na(wage_data$logwage),]
  
## mean imputed data
wage_data1 <- wage_data
wage_data1$logwage[is.na(wage_data1$logwage)] <- wage_data1$logwage %>% mean(na.rm=T)


## regressions
complete_reg     = lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wage_data0) ##  complete cases 
mean_imputed_reg    = lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wage_data1) ##  mean imputed 

## impute missing log wages as their predicted values from the complete cases r
logwage_hat <- predict(complete_reg,newdata = wage_data) %>% as.tibble() ## getting the predicted values

wage_data2 <- wage_data ## new table and missing index
na_index <- which(wage_data2$logwage %>% is.na())

wage_data2$logwage[na_index] <- logwage_hat$value[na_index] # replacement

fit_imputed_reg    = lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wage_data2) ##  fit imputed 

## mice imputation
# source: https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/
wage_data3 <- wage_data
init = mice(wage_data3, maxit=0) 
meth = init$method
predM = init$predictorMatrix


## skip imputation 
meth[c("hgc", "college", "tenure", "age","married")]=""

# pecify the methods for imputing the missing values.
meth[c("logwage")]="norm" 

## imputate logwage
set.seed(103)
imputed = mice(wage_data3, method=meth, predictorMatrix=predM, m=1)

## save the imputed df
mice_imputed <- complete(imputed)
sapply(mice_imputed, function(x) sum(is.na(x))) ## checking

mice_imputed_reg    = lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = mice_imputed) ##  mice imputed 

models <- list(
  "complete"   =   complete_reg,
  "Mean" = mean_imputed_reg,
  "Predicted"   = fit_imputed_reg,
  "Mice" =    mice_imputed_reg
)

modelsummary(models, output = "table.tex")
