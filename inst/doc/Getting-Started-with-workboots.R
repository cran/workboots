## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

ggplot2::theme_set(
  ggplot2::theme_minimal() +
    ggplot2::theme(plot.title.position = "plot",
                   plot.background = ggplot2::element_rect(fill = "white", color = "white"))
)

## -----------------------------------------------------------------------------
library(tidymodels)

# setup data 
data("car_prices")
car_prices 

## -----------------------------------------------------------------------------
# apply global transfomations
car_prices <-
  car_prices %>%
  mutate(Price = log10(Price),
         Cylinder = as.character(Cylinder),
         Doors = as.character(Doors))

# split into testing and training
set.seed(999)
car_split <- initial_split(car_prices)
car_train <- training(car_split)
car_test <- testing(car_split)

## -----------------------------------------------------------------------------
set.seed(888)
car_val_split <- initial_split(car_train)
car_val_train <- training(car_val_split)
car_val_test <- testing(car_val_split)

## -----------------------------------------------------------------------------
car_val_rec <-
  recipe(Price ~ ., data = car_val_train) %>%
  step_BoxCox(Mileage) %>%
  step_dummy(all_nominal())

# fit and predict on our validation set
set.seed(777)
car_val_preds <- 
  workflow() %>%
  add_recipe(car_val_rec) %>%
  add_model(boost_tree("regression", engine = "xgboost")) %>%
  fit(car_val_train) %>%
  predict(car_val_test) %>%
  bind_cols(car_val_test)

car_val_preds %>%
  rmse(truth = Price, estimate = .pred)

## -----------------------------------------------------------------------------
car_val_preds %>%
  ggplot(aes(x = Price, y = .pred)) +
  geom_point(size = 2, alpha = 0.25) +
  geom_abline(linetype = "dashed")

## ---- eval=FALSE--------------------------------------------------------------
#  # re-setup recipe with training dataset
#  car_rec <-
#    recipe(Price ~ ., data = car_train) %>%
#    step_BoxCox(Mileage) %>%
#    step_dummy(all_nominal())
#  
#  # setup model spec
#  car_spec <-
#    boost_tree(
#      mode = "regression",
#      engine = "xgboost",
#      mtry = tune(),
#      trees = tune()
#    )
#  
#  # combine into workflow
#  car_wf <-
#    workflow() %>%
#    add_recipe(car_rec) %>%
#    add_model(car_spec)
#  
#  # setup cross-validation folds
#  set.seed(666)
#  car_folds <- vfold_cv(car_train)
#  
#  # tune model
#  set.seed(555)
#  car_tune <-
#    tune_grid(
#      car_wf,
#      car_folds,
#      grid = 5
#    )

## ---- echo=FALSE--------------------------------------------------------------
# re-setup recipe with training dataset
car_rec <- 
  recipe(Price ~ ., data = car_train) %>%
  step_BoxCox(Mileage) %>%
  step_dummy(all_nominal())

# setup model spec 
car_spec <-
  boost_tree(
    mode = "regression",
    engine = "xgboost",
    mtry = tune(),
    trees = tune()
  )

# combine into workflow
car_wf <-
  workflow() %>%
  add_recipe(car_rec) %>%
  add_model(car_spec)

car_tune <- readr::read_rds("https://github.com/markjrieke/workboots_support/raw/main/data/car_tune.rds")

## -----------------------------------------------------------------------------
car_tune %>%
  show_best("rmse")

## -----------------------------------------------------------------------------
car_wf_final <-
  car_wf %>%
  finalize_workflow(car_tune %>% select_best("rmse"))

car_wf_final

## ---- eval=FALSE--------------------------------------------------------------
#  library(workboots)
#  
#  set.seed(444)
#  car_preds <-
#    car_wf_final %>%
#    predict_boots(
#      n = 2000,
#      training_data = car_train,
#      new_data = car_test
#    )

## ---- echo=FALSE--------------------------------------------------------------
library(workboots)

car_preds <- readr::read_rds("https://github.com/markjrieke/workboots_support/raw/main/data/car_preds.rds")

## -----------------------------------------------------------------------------
car_preds %>%
  summarise_predictions()

## -----------------------------------------------------------------------------
car_preds %>%
  summarise_predictions() %>%
  bind_cols(car_test) %>%
  ggplot(aes(x = Price, 
             y = .pred,
             ymin = .pred_lower,
             ymax = .pred_upper)) +
  geom_point(size = 2,
             alpha = 0.25) +
  geom_errorbar(alpha = 0.25,
                width = 0.0125) +
  geom_abline(linetype = "dashed",
              color = "gray")

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(333)
#  car_importance <-
#    car_wf_final %>%
#    vi_boots(
#      n = 2000,
#      trainng_data = car_train
#    )

## ---- echo=FALSE--------------------------------------------------------------
car_importance <- readr::read_rds("https://github.com/markjrieke/workboots_support/raw/main/data/car_importance.rds")

## -----------------------------------------------------------------------------
car_importance %>%
  summarise_importance() %>%
  mutate(variable = forcats::fct_reorder(variable, .importance)) %>%
  ggplot(aes(x = variable,
             y = .importance,
             ymin = .importance_lower,
             ymax = .importance_upper)) +
  geom_point(size = 2) +
  geom_errorbar() +
  coord_flip()

