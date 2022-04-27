library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)
library(miscTools)
library(maxLik)
library(sampleSelection)
library(modelsummary)
library(magrittr)
setwd("~/Desktop/DScourseS22/ProblemSets/PS12")
set.seed(123456)

df <- read_csv("wages12.csv")

# exper refers to how long (in years) each woman has worked at an employer
# hgc refers to how many years of schooling each woman has completed. 
# union is a binary variable indicating whether or not the woman is currently holding a union job
# kids is an indicator for whither the woman has at least one children living at home.

# 5. Format the college, married, and union variables as factors.
# df %>% colnames()

df$college <- df$college %>% as.factor()
df$married <- df$married %>% as.factor()
df$union <- df$union %>% as.factor()


## summary stat

df %>% datasummary_skim()
datasummary(df,formula = college + married + union ~ N)
## 31 percent of the logwage data is missing. I think it's MAR

## complete cases
wage_data0 <- df[!is.na(df$logwage),]

## mean imputed data
wage_data1 <- df
wage_data1$logwage[is.na(wage_data1$logwage)] <- wage_data1$logwage[is.na(wage_data1$logwage)] %>% mean()


## regressions
complete_reg     = lm(logwage ~ hgc + college + kids +exper+ I(exper^2) + union + married, data = wage_data0) ##  complete cases 
mean_imputed_reg    = lm(logwage ~ hgc + college + kids +exper+ I(exper^2) + union + married, data = wage_data1) ##  mean imputed 

# heckman 
heck_data <- df
heck_data$valid <- ifelse(heck_data$logwage %>% is.na(), 0,1)
heck_data$logwage[heck_data$valid==0] <- 0

heck_reg <- selection ( selection = valid ~ hgc + union + college + exper + married + kids ,
                        outcome = logwage ~ hgc + union + college + exper + I ( exper ^2)  , data = heck_data, method = "2step")

heck_reg$lm %>% modelsummary()
heck_reg %>% summary()

models <- list(
  "complete"   =   complete_reg,
  "Mean" = mean_imputed_reg,
  "Heckman"   = heck_reg$lm
)

var_names <- c(
  'hgc' = 'N years of schooling' ,
  'union1' = 'Holding a union dummy' ,
  'college' = 'College educ. dummy',
  'exper' = 'N experience years',
  'I(exper^2)' = 'N experience years squared',
  'married1'='Married dummy',
  "kids" = 'Kids dummy',
  '(Intercept)' = 'Constant',
  #---- heck 2nd stage-----
  'XOhgc' = 'N years of schooling' ,
  'XOunion1' = 'Holding a union dummy' ,
  'XOcollege' = 'College educ. dummy',
  'XOexper' = 'N experience years',
  'XOI(exper^2)' = 'N experience years squared',
  'XOmarried1'='Married dummy',
  'XO(Intercept)' = 'Constant',
  'imrData$IMR1' = 'Inverse mills ratio'
)

modelsummary(models, 
             coef_map = var_names,
             stars = c('*' = .1, '**' = .05,'***' = .01)
             #output = "reg_table.tex"
             )

## heckman 
# Heckman selection model yielded the correct coefficient. 

# 8. Using the same data, estimate a probit model of preferences for working in a union job.
union_reg <- glm(union ~ hgc + college + kids +exper + married + kids, data = df,family=binomial(link='logit')) ##  complete cases

modelsummary(list("Union"=union_reg),
             coef_map = c(
               'hgc' = 'N years of schooling' ,
               'college' = 'College educ. dummy',
               'exper' = 'N experience years',
               'married1'='Married dummy',
               "kids" = 'Kids dummy',
               '(Intercept)' = 'Constant') )

df %<>% mutate(predLogit = predict(union_reg, newdata = df, type = "response"))
df %>% `$`(predLogit) %>% summary %>% print

# counterfactual policy
union_reg$coefficients["married1"] <- 0*union_reg$coefficients["married1"]
union_reg$coefficients["kids"] <- 0*union_reg$coefficients["kids"]
df %<>% mutate(predLogitCfl = predict(union_reg, newdata = df, type = "response"))
df %>% `$`(predLogitCfl) %>% summary %>% print
