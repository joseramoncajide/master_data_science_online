##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Supervised Learning for classification
##########################################################################

library(tidyverse)
library(tidymodels)
library(rpart.plot)
library(hrbrthemes)
theme_set(theme_ipsum_rc())

# install.packages("remotes")
# remotes::install_github("grantmcdermott/parttree")
library("parttree") 


# Logistic regression -----------------------------------------------------

#  The logistic function is a sigmoid function, which takes any real input t, 
# and outputs a value between zero and one.
logistic <- function(x){exp(x)/(1 + exp(x))}

ggplot(data.frame(x = c(-6, 6)), aes(x)) +
  stat_function(fun = logistic)

# Two class problem
iris_df <- iris %>% 
  filter(Species != "setosa") %>% 
  droplevels()

table(iris_df$Species)

iris_df %>% 
  mutate(prob = ifelse(Species == "virginica", 1, 0)) %>% 
  ggplot(aes(x = Petal.Length, y = prob)) + 
  geom_point() + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(
    title = "Logistic Regression Model", 
    x = "Petal Lengh",
    y = "Probability of being a virginica"
  )


# Simply logistic regression
iris_simple_lr <- glm(Species ~ Petal.Length, 
               data = iris_df,
               family = binomial(link='logit'))

beta0.lr.1 <- coef(iris_simple_lr)[1]
beta1.lr.1 <- coef(iris_simple_lr)[2]
beta0.lr.1
beta1.lr.1

# We can estimate the probability that a iris whose Petal.Length is 5 as:
exp(beta0.lr.1 + beta1.lr.1*5)/(1 + exp(beta0.lr.1 + beta1.lr.1*5))


# Multiple logistic regression
iris_lr <- glm(Species ~ Petal.Length + Petal.Width, 
               data = iris_df,
               family = binomial(link='logit'))
iris_lr

lr.int <- -coef(iris_lr)[1]/coef(iris_lr)[3]
lr.slope <- -coef(iris_lr)[2]/coef(iris_lr)[3]

iris_df %>% 
  ggplot(aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(color = Species)) + 
  geom_abline(intercept = lr.int, slope = lr.slope) +
  labs(title = 'Logistic Regression decission boundary')

predicted_probs <- predict(iris_lr, iris_df, type = "response")

iris_predictions <- rep("versicolor", length(predicted_probs))

iris_predictions[predicted_probs > 0.5] <- "virginica"

iris_predictions

table(iris_predictions, as.character(iris_df$Species))


# Multiclass
iris %>% 
  ggplot(aes(x=Petal.Length,y=Sepal.Length))+
  geom_point(aes(color=Species))

library(nnet)
iris_multinom_model <- multinom(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data = iris)
summary(iris_multinom_model)

iris %>% 
  mutate(predicted = predict(iris_multinom_model, iris)) -> iris_res_df

table(iris_res_df$Species,iris_res_df$predicted)


# Trees -------------------------------------------------------------------

# Recursive Partitioning 
iris_tree <- rpart(Species ~ Petal.Length + Petal.Width, data=iris)
rpart.plot(iris_tree)

iris %>%
  as_tibble() %>% 
  ggplot(aes(x=Petal.Length, y=Petal.Width)) +
  geom_jitter(aes(col=Species), alpha=0.7) +
  geom_parttree(data = iris_tree, aes(fill=Species), alpha = 0.1) +
  theme_minimal()



# Tidy models: Classification ---------------------------------------------

titanic <- read_csv('data/titanic/train.csv') %>% 
  mutate(Survived = factor(Survived, labels = c("no", "yes")))

set.seed(1973)
split <- initial_split(titanic, strata = "Survived")
train <- training(split)
test <- testing(split)

log_model <- 
  logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification") %>%
  fit(Survived ~ Pclass + Age, data = train)

## Build our tree using parsnip (but with rpart as the model engine)
tree_model <- 
  decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification") %>%
  fit(Survived ~ Pclass + Age, data = train)

log_model %>% 
  augment(new_data = test) %>%
  accuracy(truth = Survived, estimate = .pred_class)

tree_model %>% 
  augment(new_data = test) %>%
  accuracy(truth = Survived, estimate = .pred_class)

log_model %>%
  augment(test, type.predict = "response") %>%
  conf_mat(Survived, .pred_class) %>% 
  autoplot(type = "heatmap")

tree_model %>%
  augment(test, type.predict = "response") %>%
  conf_mat(Survived, .pred_class) %>% 
  autoplot(type = "heatmap")

log_model %>%
  augment(test, type.predict = "response") %>%
  roc_curve(truth = Survived, estimate = .pred_no) %>%
  autoplot()

tree_model %>%
  augment(test, type.predict = "response") %>%
  roc_curve(truth = Survived, estimate = .pred_no) %>%
  autoplot()

# Tree Viz
tree_model %>%
  extract_fit_engine() %>%
  rpart.plot()

## Plot the data and model partitions
train %>%
  ggplot(aes(x=Pclass, y=Age)) +
  geom_jitter(aes(col=Survived), alpha=0.7) +
  geom_parttree(data = tree_model, aes(fill=Survived), alpha = 0.1) +
  theme_minimal()


# Hyperparameter tunning --------------------------------------------------
# model parameters are the variables that your chosen machine learning 
# technique uses to adjust to your data.
# Example:
# The complexity parameter (cp) is used to control the size of the decision tree and to select the optimal tree size

tree_model %>%
  extract_fit_engine() %>% 
  printcp()

tree_model %>%
  extract_fit_engine() %>% 
  plotcp()

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_args(cost_complexity = tune()) %>% 
  set_mode("classification")

tree_wf <- workflow() %>%
  add_model(tree_spec) %>%
  add_formula(Survived ~ Pclass + Age)

set.seed(1234)
train_fold <- vfold_cv(train, strata = Survived)

param_grid <- grid_regular(cost_complexity(range = c(-3, -1)), levels = 10)

tune_res <- tune_grid(
  tree_wf, 
  resamples = train_fold, 
  grid = param_grid, 
  metrics = metric_set(accuracy)
)

autoplot(tune_res)

best_complexity <- select_best(tune_res)

tree_final <- finalize_workflow(tree_wf, best_complexity)

tree_final_fit <- fit(tree_final, data = train)
tree_final_fit

predict(tree_final_fit, test[1,])

tree_final_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

tree_final_fit %>% 
  augment(new_data = test) %>%
  accuracy(truth = Survived, estimate = .pred_class)

# Model deployment: api ---------------------------------------------------

# Now we are going to learn how to deploy a model
saveRDS(tree_final_fit, "./tmp/titanic_model.rds")

# Go to:
# 'model_api.R'


# Your turn ---------------------------------------------------------------

# Add another feature to the model: Sex
# If the model improves, make the necessary changes in the api to deploy it again.



# Preprocessing data ------------------------------------------------------

prep <- recipe(Survived ~., data = train) %>%
  step_rm(PassengerId, Name, Ticket, Cabin) %>% 
  step_impute_mean(Fare) %>%
  step_impute_median(Age, SibSp, Parch) %>%
  step_impute_mode(Sex, Embarked) %>%
  step_impute_knn(Pclass) %>%
  step_dummy(all_nominal(), -Survived) %>% 
  step_normalize(all_predictors()) %>% 
  prep() 

juiced <- juice(prep) 

baked <- bake(prep, new_data = test)

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_wf <-
  workflow() %>% 
  add_model(tree_spec) %>% 
  add_recipe(prep)

tree_fit <-
  tree_wf %>% 
  fit(data = train)

tree_fit %>% 
  augment(new_data = test) %>%
  accuracy(truth = Survived, estimate = .pred_class)

tree_fit %>% 
  pluck("fit")


# Exercise: Fit a Random forests model and evaluate its results

# Define model


# Define workflow


# Fit model


# Evaluate


# Predict test data


