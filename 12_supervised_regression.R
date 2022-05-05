##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Supervised Learning
##########################################################################

library(tidyverse)
library(hrbrthemes)
library(broom)
theme_set(theme_ipsum_rc())

survey <- read_csv('data/Stat100_Survey2_Fall2015.csv')

survey %>% 
  select(gender, drinks, partyHr, religion, workHr) -> survey

# Gender => When you were born, what was your biological sex?
# Party Hours per week  => About how many hours do you party per week on the average?
# Drinks per week => About how many alcoholic drinks do you consume per week on the average?
# (Religion) Which of the following groups best describes your religious identification?
# 0=Christian; 1=Jewish; 2=Muslim; 3=Hindu; 4=Buddhist; 5=Religious but not one of the above; 6=Agnostic; 7=Atheist
# (Work Hours) About how many hours a week do you work at a paying job?


# linear model predicting student’s party hours/week from the average number of drinks/week
fit <- lm(partyHr ~ drinks, data=survey)


# the intercept is 2.3152 and the slope of “drinks” is 0.5462. 
# The linear model is
# party hours = 2.3152 + 0.5462 * drinks
fit$coefficients

summary(fit)

plot(partyHr ~ drinks, data=survey, pch=19, xlab="Drinks/week", ylab="Party hours/week")
abline(fit, col="red")

# Exercise: Repeat the same plot using ggplot






# Remember from '11_stats_regression_correlation.Rmd'
r <- cor(survey$drinks,survey$partyHr)

sd_y <- sd(survey$partyHr)
sd_x <- sd(survey$drinks)

slope_drinks <- r * (sd_y / sd_x)
intercept <- mean(survey$partyHr) - slope_drinks * mean(survey$drinks)
model_coefficients <- c(intercept,slope_drinks)

# We know that R2 in simple regression is the same as the square of the correlation coefficient.
r^2

# Predictions
# y= b + mx
predict_partyHrfromDrinks <- function(drinks, beta) {
  beta[1] + beta[2]*drinks
}

predict_partyHrfromDrinks(survey$drinks[1:10], model_coefficients)

predict_partyHrfromDrinks(survey$drinks[1:10], fit$coefficients)

# Easier
fit$fitted.values[1:10]

plot(survey$partyHr, fit$fitted.values)

# Predict from new data
predict(fit, newdata=tibble(drinks=c(3,4,8,2,16)))


# Multiple lienar regression stepwise
lm_total <- lm(partyHr~., data = survey)
summary(lm_total)

# The AIC metric is used for checking model fit improvement. 
stats::step(lm_total, direction = "backward")

# Fitting the improved model
lm_step_backward <- lm(formula = partyHr ~ gender + drinks, data = survey)
summary(lm_step_backward)


# Categorical variables
# Now we want to know if there is any difference between male and female students. 
# We can include gender in the model by the command
# We want to change it to a factor variable, with “male” set to the first level:
survey %>% 
  mutate(gender = factor(gender, levels=c("male","female"))) -> survey


fit1 <- lm(partyHr ~ drinks + gender, data=survey)
summary(fit1)

# When lm() encounters a factor variable with two levels, it creates a new variable based on the second level. 
# In the example, the second level is female, and ‘genderfemale’ is created

# In this model, male and female have the same slope (0.55513) for ‘drinks’ but different intercepts. 
# For male, the intercept is 1.73761. For female, the intercept is 1.73761 + 0.77765 = 2.51526.


# Note that since we set “male” to be the first level (also called the base level or reference level), 
# the intercept corresponds to the intercept for “male”. We can change the base level by the relevel() command:
gender2 <- relevel(survey$gender,"female")

fit2 <- lm(survey$partyHr ~ survey$drinks + gender2)
summary(fit2)
# We see that the slope for ‘drinks’ remains the same as before. 
# The value of the intercept is 2.51525, which is the intercept for female we calculated above. 
# The intercept for male is 2.51525 + (-0.77765) = 1.7376, consistent with the previous result.



# Interaction

# In R, the interaction term is represented by a colon ‘:’. 
# To fit a model for ‘partyHr’ versus ‘drinks’, ‘gender’ and interaction between ‘drink’ and ‘gender’, 
# we can use the command

fit3 <- lm(partyHr ~ drinks + gender + drinks:gender, data=survey)
summary(fit3)

# or
fit3 <- lm(partyHr ~ drinks*gender, data=survey)
summary(fit3)


# party hours/week=2.3682+0.492(drinks/week)−0.322(female)+0.1519(drinks/week)⋅(female)

# the regression equation above can be split into separate equations for male and female: 
# party hours/week=2.3682+0.492(drinks/week) for male
# party hours/week=2.0462+0.6439(drinks/week) for female being = 0.6439 = 0.4920 + 0.1519 

# Exercise: 
# Fit a partyHr ~ drinks model for male gender and another one for female gender. 
# Check if the results are the same that above
# Plot the two regression lines













# Tidy modelling ----------------------------------------------------------
# library(broom)

survey %>%
  nest(data = -gender) %>% 
  mutate(
    fit = map(data, ~ lm(partyHr ~ drinks, data = .x)),  
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)

regressions <- survey %>%
  nest(data = -gender) %>% 
  mutate(
    fit = map(data, ~ lm(partyHr ~ drinks, data = .x)),
    tidied = map(fit, tidy, conf.int = TRUE),
    glanced = map(fit, glance),
    augmented = map(fit, augment, interval = "prediction")
  )

regressions %>% 
  unnest(tidied)

# Coefficient plot
# All coefficients whose confidence interval (marked by the horizontal lines) does not touch the zero line 
# are significant.

regressions %>% 
  unnest(tidied) %>% 
  ggplot(aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 4) +
  geom_errorbarh() +
  facet_wrap(vars(gender))


regressions %>%
  unnest(glanced)


regressions %>% 
  unnest(augmented)

# ggplot2 example where we also construct 95% prediction interval
regressions %>% 
  unnest(augmented) %>% 
  ggplot(aes(drinks, partyHr, color=gender)) + 
  geom_point() +
  geom_line(aes(y = .fitted)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill=gender), col = NA, alpha = 0.3)




# Caret - Classification And REgression Training --------------------------

train_df <- survey

library(caret)

# https://en.wikipedia.org/wiki/Cross-validation_(statistics)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 2)

set.seed(32)


# Linear models
lm_fit <- train(partyHr ~.,
                data = train_df,
                method = "lm",
                trControl = ctrl)
lm_fit

# Non-linear models

# Random forests
rF_fit <- train(partyHr ~., 
                data = train_df, 
                method = "rf", 
                metric = "RMSE", 
                trControl = ctrl,
                importance = TRUE)

rF_fit

# Multivariate adaptive regression splines (MARS) - Nonparametric regression
mars_fit <- train(partyHr ~., 
                  data = train_df, 
                  method = "earth", 
                  metric = "RMSE",
                  tuneLength = 10,
                  trControl = ctrl)

mars_fit

# Support vector machines
svm_fit <- train(partyHr~.,
                 data = train_df,
                 method = "svmRadial",
                 preProc = c("center", "scale"),
                 tuneLength = 10,
                 trControl = ctrl)
svm_fit


# Compare all models

results <- resamples(list(RandomForest = rF_fit,
                          MARS = mars_fit,
                          SVM = svm_fit,
                          OLS = lm_fit))
summary(results)


bwplot(results, metric = "RMSE", main = "Comparative Performance\nRoot Mean Squared Error")

bwplot(results, metric = "Rsquared", main = "Comparative Performance\nR Squared")

bwplot(results, metric = "MAE", main = "Comparative Performance\nMean Absolute Error")

# Optional: hypothesis tests to confirm that the performance differences observed above are statistically significant
modelDiff <- diff(results)
summary(modelDiff)
# P-values are not close to 0 for almost all pairwise comparisons confirming not statistical differences between the models.


# Predictions with best model
mars_fit_pred <- predict(mars_fit, newdata = train_df)
head(mars_fit_pred)

# MSE - mean squared error
pred_MSE <- mean((mars_fit_pred - train_df$partyHr)^2)
pred_MSE

# RMSE
sqrt(pred_MSE)


# Here is the code to calculate RMSE explicitly (for understanding) in our training dataset

rmse_demo <- tibble(partyHr = train_df$partyHr, 
                    predicted = mars_fit_pred, 
                    diff = train_df$partyHr - predicted, 
                    diff_sqr = diff^2)
rmse_demo

sqrt(sum(rmse_demo$diff_sqr)/nrow(rmse_demo))

RMSE(mars_fit_pred, train_df$partyHr)


# Exercise
# Repeat the previous exercise splitting the survey data set into train_df and test_df
# using the function initial_split in the rsample package
# Evaluate the models in test_df

library(rsample)

























# Tidy Models Introduction ------------------------------------------------

library(tidymodels)

lm_model <- linear_reg() %>% 
  set_engine('lm') %>%
  set_mode('regression')

# View object properties
lm_model

lm_fit <- lm_model %>% 
  fit(partyHr ~ ., data = train_df)

# View lm_fit properties
lm_fit


summary(lm_fit$fit)
tidy(lm_fit)
glance(lm_fit)
augment(lm_fit, new_data = test_df)

# Confidence interval: reflects the uncertainty around the mean predictions.
predict(lm_fit,new_data = test_df, type = "conf_int")


# Add predictions to the test set
test_df <- predict(lm_fit, new_data = test_df) %>% 
  bind_cols(test_df) 


# Add confidence intervals to the test set
test_df <- predict(lm_fit, new_data = test_df, type = "conf_int") %>% 
  bind_cols(test_df) 



# RMSE on test set
rmse(test_df, 
     truth = partyHr,
     estimate = .pred)

# R2 on test set
rsq(test_df,
    truth = partyHr,
    estimate = .pred)

# R2 Plot: R2 is simply the squared correlation between the true and predicted values, 
# which are plotted as paired in the plot

test_df %>% 
  ggplot(aes(x = .pred, y = partyHr)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - Survey Test Set',
       x = 'Predicted Party Hours',
       y = 'Actual Party Hours')



# Tidy Models with big data and Spark -------------------------------------

library(sparklyr)
sc <- spark_connect("local")

spark_survey <- copy_to(sc, survey)

linear_reg() %>%
  set_engine("spark") %>%
  translate()

lm_spark_model <- linear_reg() %>% 
  set_engine('spark') %>% 
  fit(partyHr ~ ., data = spark_survey)

lm_spark_model

tidy(lm_spark_model)

glance(lm_spark_model)

spark_disconnect(sc)  
