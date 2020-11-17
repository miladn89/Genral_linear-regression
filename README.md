# Linear-regression


```{r}
require("compositions")
autoload("bpy.colors", "sp")
require("lmtest")

x = read.csv2(, header = TRUE)
colnames(x)

```


```{r}
update.packages("rlang")


library("lm")
read.csv2("Milad325_7")
model1 <- lm(formula =Total_agg_ST ~ 1 +	
               
               Age +	Parents_checking +	Parent_concern_need + Sibling +	Car_status + Financial_aids +	Grant	+ Scholarship	+ Loan + Major_of_study +	Time_Academic +	Time_Extracurricular +	Time_work +	Time_social +	Time_family +	 Co_op +	Internship +	GPA +	Ethinicity_1	+  Gender_1 +Father_education_1	+ Mother_education_1	+ Houshold_status_1+ Houshold_living_status_1,
                data    = Milad325_7)

summary(model1)

coef(model1)
  AIC(model1)
```
```{r}
install.packages("caret")


library(caret)
# Simple linear regression model (lm means linear model)
model <- train(mpg ~ wt,
               data = mtcars,
               method = "lm")

# Multiple linear regression model
model <- train(mpg ~ .,
               data = mtcars,
               method = "lm")

# Ridge regression model
model <- train(mpg ~ .,
               data = mtcars,
               method = "ridge") # Try using "lasso"



## 10-fold CV
# possible values: boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV"
fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "lasso",  # now we're using the lasso method
               trControl = fitControl)  

model.cv   

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "lasso",
               trControl = fitControl,
               preProcess = c('scale', 'center')) # default: no pre-processing

?train    # if you need more information about the train function
model.cv

# Here I generate a dataframe with a column named lambda with 100 values that goes from 10^10 to 10^-2
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "ridge",
               trControl = fitControl,
               preProcess = c('scale', 'center'),
               tuneGrid = lambdaGrid,   # Test all the lambda values in the lambdaGrid dataframe
               na.action = na.omit)   # Ignore NA values

model.cv



fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           search = "random")  # hyper-parameters random search 

model.cv <- train(mpg ~ .,
               data = mtcars,
               method = "ridge",
               trControl = fitControl,
               preProcess = c('scale', 'center'),
               na.action = na.omit)

model.cv


ggplot(varImp(model.cv))


predictions <- predict(model.cv, mtcars)

predictions
```
