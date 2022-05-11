##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Supervised Learning: Workflows
##########################################################################
library(tidyverse)
library(tidymodels)
# use linear regression to predict the selling price of homes using the home_sales data

home_sales <- read_csv('data/home_sales.csv')

# Step 1. Split Our Data --------------------------------------------------

set.seed(1973)

# Create a split object
homes_split <- initial_split(home_sales, prop = 0.75, 
                             strata = selling_price)

# Build training data set
train_df <- homes_split %>% 
  training()

# Build testing data set
test_df <- homes_split %>% 
  testing()


# Step 2. Feature Engineering ---------------------------------------------

homes_recipe <- recipe(selling_price ~ ., data = train_df) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), - all_outcomes())

homes_recipe %>% 
  prep() %>% 
  bake(new_data = head(train_df,2))



# Step 3. Specify a Model -------------------------------------------------

lm_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')



# Step 4. Create a Workflow -----------------------------------------------

homes_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(homes_recipe)


# Step 5. Execute the Workflow --------------------------------------------

homes_fit <- homes_workflow %>% 
  last_fit(split = homes_split)


# Step 6: performance metrics on test data --------------------------------

homes_fit %>% 
  collect_metrics()


results <- homes_fit %>% 
  collect_predictions()

table(results$id)


# Predicted Vs Observed or R2 Plot
results %>% 
  ggplot(aes(x = .pred, y = selling_price)) +
  geom_point(color = 'grey50', alpha=.2) +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  geom_smooth(aes(.pred, selling_price), method = "lm", se = FALSE, color = "darkgreen")
labs(title = 'Linear Regression ResultsTest Set',
     x = 'Predicted',
     y = 'Observed')


# Step 3: Fitting to Training Data ----------------------------------------

lm_fit <- lm_model %>% 
  fit(selling_price ~ ., data = train_df)

lm_fit %>% 
  pluck("fit")

lm_fit %>% 
  pluck("fit") %>%
  summary()


# Step 4: extract model data ----------------------------------------------

# Use tidy(), glance()

# Extract model coefficientes 
tidy(lm_fit) %>% 
  filter(p.value < .05)

glance(lm_fit)

# Confidence interval: reflects the uncertainty around the mean predictions.
predict(lm_fit,new_data = test_df, type = "conf_int")


bind_cols(
  predict(lm_fit, new_data = test_df),
  test_df
) 

# or

test_df <- augment(lm_fit, new_data = test_df) 


# Step 5: Model evaluation ------------------------------------------------


# RMSE on test set
rmse(test_df, 
     truth = selling_price,
     estimate = .pred)

# R2 on test set
rsq(test_df,
    truth = selling_price,
    estimate = .pred)

# Predicted Vs Observed or R2 Plot
test_df %>% 
  ggplot(aes(x = .pred, y = selling_price)) +
  geom_point(color = 'grey50', alpha=.2) +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  geom_smooth(aes(.pred, selling_price), method = "lm", se = FALSE, color = "darkgreen")
  labs(title = 'Linear Regression ResultsTest Set',
       x = 'Predicted',
       y = 'Observed')


  
  
rf_model <- 
    # specify that the model is a random forest
    rand_forest() %>%
    set_args(mtry = 3, trees = 500, min_n=2) %>% 
    # select the engine/package that underlies the model
    set_engine("ranger", importance = "impurity", num.threads = 2 ) %>%
    # choose either the continuous regression or binary classification mode
    set_mode("regression")


homes_rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(homes_recipe)


homes_rf_fit <- homes_rf_workflow %>% 
  last_fit(split = homes_split)


homes_rf_fit %>% 
  collect_metrics()


homes_rf_fit %>% 
  collect_predictions()

# Houses thar are being sold at a selling_price lower than expected
augment(homes_rf_fit) %>% 
  arrange(.resid) %>% 
  top_n(-5)
  
